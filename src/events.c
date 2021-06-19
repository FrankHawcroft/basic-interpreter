/****** events.c ******/

/*
	$VER: events.c 0.16 (4.13.2013)

	AmigaBASIC-style event handling.
*/

#include <signal.h>
#include <string.h>
#include <ctype.h>
#include "interpreter.h"
#include "process.h"
#include "builtin.h"
#include "platform.h"
#include "sign.h"
#include "cqueue.h"
#include "heap.h"
#include "audio.h"

enum EventType {
	EVT_TIMER,   /* A time interval elapsed. */
	EVT_POINTER, /* Mouse or other pointing device was used. */
	EVT_INKEY,   /* Key pressed (in a GUI rather than a console). */
	EVT_MENU,    /* Menu selected. */
	EVT_BREAK,   /* Ctrl-C or equivalent signal. */
	EVT_ERROR    /* A trappable error occurred. */
};

#define FIRST_EVENT_TYPE EVT_TIMER
#define LAST_EVENT_TYPE EVT_ERROR
#define NUM_EVENT_TYPES (LAST_EVENT_TYPE + 1)

struct Event {
	/* Miscellaneous value giving more information about the event.
		For ERROR: the error number.
		For POINTER: kind of action, and location. */
	long lngInfo;
	
	/* For ERROR: name of closest label appearing before where error occurred at the same call nest level. 
		For INKEY: keypress information. 
		For MENU: menu selection. */
	QString	strInfo;

	/* Statement at which event occurred, if meaningful. */
	const char *position;
};

enum HandlerState {
	DISABLED, /* Events not recorded. */
	SUSPENDED, /* Events are queued, but no trapping takes place. */
	ENABLED /* Event trapping takes place. */
};

struct Trap {
	enum HandlerState status;

	/* Relative priority of events of this type. */
	int priority;
		
	/* Used to schedule events which have the same priority, on	a round-robin basis. */
	unsigned long sequencing;
	
	/* "ERROR" etc. Used to find the relevant trap by name.
	If the trap is internal (can't be accessed from a program) then the name is null. */
	QString name;
	
	/* Called to poll the system for new events. */
	void (*poll)(struct Trap *);
	
	/* Called when event occurs but trap is not ENABLED, or a higher-priority handler was executed. */
	void (*track)(enum HandlerState, struct CircularQueue *, struct Event *);
	
	/* Called when the status of the event trap changes. */
	void (*transition)(struct Trap *, enum HandlerState);
	
	/* Used to resolve which event should be handled, based on priority and sequencing. */
	void (*prioritise)(struct Process *proc, struct Trap *current, struct Trap **best, unsigned long *maxSequencing);
	
	/* The programatically-defined event handler. */
	union {
		const struct Statement *subprogram;
		struct {
			const char *label;
			const char *context; /* Enclosing subprogram body - NULL if none. */
		} simpleLocation;
	} handler;

	/* True if the handler is a sub (ON ... CALL ...) rather than a label (ON ... GOTO ...). */
	bool subprogramHandler;
	
	/* Call nest level just before handler is invoked, or suspended as a side-effect of a higher-priority trap occurring;
	recorded so that the handler can be re-enabled on subprogram exit or RESUME. */
	short suspendedAt;

	/* The time at which the timer last 'ticked'; this is recorded regardless of whether the handler is actually called
	or not, provided a timer event has been established and not	disabled. Currently only populated for TIMER events. */
	struct PfSystemTimeStamp lastTimerTick;
	
	/* The interval in seconds at which TIMER events should occur, as specified by an ON TIMER(<n>) ... statement. */
	float timerGranularity;
};

static void InitEvent(struct Event *evt)
{
	evt->lngInfo = 0;
	QsInitNull(&evt->strInfo);
	evt->position = NULL;	
}

static void DisposeEvent(void *evt)
{
	QsDispose(&((struct Event *)evt)->strInfo);
}

static bool EventAvailable(const struct Process *proc, enum EventType et)
{
	return !CQIsEmpty(&proc->q[et]);
}

/* The polling function for events which originate internally (e.g. ERROR) or
are platform-specific (e.g. UI events), and handled elsewhere. */
static void NullPoller(struct Trap *t)
{
}

/* Timer event traps can be recorded inside a timer event handler! */
static void PollTimer(struct Trap *t)
{
	if(t->status >= SUSPENDED && t->timerGranularity > 0) {
		struct PfSystemTimeStamp currentTime;

		PfGetSystemTimeStamp(&currentTime);

		if(PfTimeHasElapsed(&t->lastTimerTick, &currentTime, t->timerGranularity)) {
			struct Event e;
			
			t->lastTimerTick = currentTime;
			
			InitEvent(&e);
			EnqueueOnCQ(&Proc()->q[t - Proc()->trap], &e, TRUE);
		}
	}
}

extern bool KeyWasPressed(void);
extern bool PointerWasOperated(void);
extern bool MenuWasSelected(void);
	/* TODO should have a registration system where polling functions are added
		outside the events module itself */

static void PollInKey(struct Trap *t)
{
	if(t->status >= SUSPENDED && KeyWasPressed()) {
		struct Event e;
		InitEvent(&e);
		EnqueueOnCQ(&Proc()->q[t - Proc()->trap], &e, TRUE); /* TODO store keypress info ... */
	}
}

static void PollPointer(struct Trap *t)
{
	if(t->status >= SUSPENDED && PointerWasOperated()) {
		struct Event e;
		InitEvent(&e);
		EnqueueOnCQ(&Proc()->q[t - Proc()->trap], &e, TRUE); /* TODO store pointer info ... */
	}
}

static void PollMenu(struct Trap *t)
{
	if(t->status >= SUSPENDED && MenuWasSelected()) {
		struct Event e;
		InitEvent(&e);
		EnqueueOnCQ(&Proc()->q[t - Proc()->trap], &e, TRUE); /* TODO store menu info ... */
	}
}

#if PF_POLL_FOR_SIGNALS

static bool BreakHasOccurred()
{
	return PfTestAndClearBreakSignal();
}

#else

static void __cdecl RecordBreakSignal(int dummy)
{
	Proc()->breakFlag = TRUE;
}

/* This is not thread-safe, but then, the program state isn't shared ... */
static bool BreakHasOccurred(void)
{
	struct Process *proc = Proc();
	bool flagValue = proc->breakFlag;
	proc->breakFlag = FALSE;
	return flagValue;
}

#endif /* !PF_POLL_FOR_SIGNALS */

/* Polling is done before checking the trap status, so that the record of a break having
occurred is cleared. However, to speed up the per-statement event polling, event checkers
are only called for events that aren't DISABLED. This means that the break record must also
be flushed in the break status change handler. */
static void PollBreak(struct Trap *t)
{
	if(BreakHasOccurred() && t->status >= SUSPENDED) {
		struct Event e;
		InitEvent(&e);
		EnqueueOnCQ(&Proc()->q[t - Proc()->trap], &e, TRUE);
	}
}

/* Used to 'handle' events which don't need to be tracked if not active. */
static void RetentiveTracker(enum HandlerState status, struct CircularQueue *q, struct Event *evt)
{
	assert(status != DISABLED);
	EnqueueOnCQ(q, evt, FALSE);
}

static void BreakTracker(enum HandlerState status, struct CircularQueue *q, struct Event *evt)
{
	if(status == ENABLED) {
		Proc()->pendingError = EXECUTIONABORTED;
		Proc()->mode = MODE_INTERACTIVE;
	}
	else
		RetentiveTracker(status, q, evt);
}

static void ErrorTracker(enum HandlerState status, struct CircularQueue *q, struct Event *evt)
{
	Proc()->currentStatementStart = Proc()->currentPosition = evt->position;
		/* Ensures that the line at which the error occurred is displayed. */
	Proc()->pendingError = (Error)evt->lngInfo;
	Proc()->mode = MODE_INTERACTIVE;
}

/* If the status is changed to DISABLED, events are purged. */
static void DefaultTransition(struct Trap *t, enum HandlerState newStatus)
{
	t->status = newStatus;
	if(newStatus == DISABLED) {
		enum EventType et = t - Proc()->trap;
		InitEvent(&Proc()->activeEvent[et]);
		ClearCQ(&Proc()->q[et]);
	}
}

static void TransitionTimer(struct Trap *t, enum HandlerState newStatus)
{
	if(t->status == DISABLED && newStatus == ENABLED)
		PfGetSystemTimeStamp(&t->lastTimerTick); /* Start timing. */
	DefaultTransition(t, newStatus);
}

static void TransitionBreak(struct Trap *t, enum HandlerState newStatus)
{
	/* On systems that have a persistent break signal status, ensure it's flushed. */
	if(t->status == DISABLED || newStatus == DISABLED)
		BreakHasOccurred();
	DefaultTransition(t, newStatus);
}

/* Prioritise events. Generally this can be done simply based on which kinds of
event the program actually wants to handle, and the fixed priorities associated
with different kinds of events (Trap.priority).

The 'sequencing' value is used to order events of equal priority which occured
'at the same time' from the point of view of the interpreter - i.e. happened during
execution of the same statement and are both detected in its event checking epilogue.

Sequencing dependent events:

There are some equal-priority events which have a logical ordering, because 
they depend on each other. These events are generally in pairs. For example,
on platforms suporting GUI interaction, there are MOUSEDOWN and MOUSEUP events.
If the mouse button is currently down, and both MOUSEDOWN and MOUSEUP events are 
detected 'at the same time', trap the MOUSEUP ahead of the MOUSEDOWN.
Conversely, if the mouse button is not already down, favour a MOUSEDOWN.

Sequencing independent events (the default approach):

Equal-priority events are scheduled on a round-robin basis. The event with the
lowest sequencing number is chosen, and then its sequencing number is set to one greater
than the current maximum, sending it to the back of the queue for next time. */
static void PrioritiseWithDefaultStrategy(struct Process *proc,
	struct Trap *current, struct Trap **best, unsigned long *maxSequencing)
{
	assert(current != NULL && best != NULL && maxSequencing != NULL);
	
	/* If there's no event available, or the handler isn't enabled, ignore it. */
	if(current->status != ENABLED || !EventAvailable(proc, current - proc->trap))
		return;
	
	/* If the handler isn't populated, or is out of scope, ignore it. */
	if((current->subprogramHandler && current->handler.subprogram == NULL)
	|| (!current->subprogramHandler
		  && (current->handler.simpleLocation.label == NULL
		    || (current->handler.simpleLocation.context != StartOfCurrentSubprogram()))))
		return;
	
	if(*best == NULL || current->priority > (*best)->priority) {
		*best = current;
		*maxSequencing = 0;
	}
	else if(current->priority == (*best)->priority) {			
		if(current->sequencing > *maxSequencing)
			*maxSequencing = current->sequencing;
		if(current->sequencing < (*best)->sequencing)
			*best = current;
	}
}

/* Resets the mutable parts to an inactive state. */
static void ResetTrap(struct Trap *trap)
{
	trap->sequencing = 0;
	trap->subprogramHandler = FALSE;
	trap->handler.simpleLocation.label = NULL;
	trap->handler.simpleLocation.context = NULL;
	PfInitSystemTimeStamp(&trap->lastTimerTick);
	trap->timerGranularity = 0;
	trap->suspendedAt = SCOPE_NONEXISTENT;
}

static void InitTrap(struct Trap *trap, const char *name, enum HandlerState status, int priority)
{
	trap->status = status;
	trap->priority = priority;
	QsInitStatic(&trap->name, name, strlen(name));
	trap->poll = NullPoller;
	trap->track = RetentiveTracker;
	trap->transition = DefaultTransition;
	trap->prioritise = PrioritiseWithDefaultStrategy;
	ResetTrap(trap);
}

void InitEventTraps(void)	
{
	enum EventType et;

	Proc()->trap = New(sizeof(struct Trap) * NUM_EVENT_TYPES);
	Proc()->q = New(sizeof(struct CircularQueue) * NUM_EVENT_TYPES);
	Proc()->activeEvent = New(sizeof(struct Event) * NUM_EVENT_TYPES);
	
	InitTrap(&Proc()->trap[EVT_TIMER], "TIMER", DISABLED, 10);
	InitTrap(&Proc()->trap[EVT_POINTER], "MOUSE", DISABLED, 15);
	InitTrap(&Proc()->trap[EVT_INKEY], "INKEY", DISABLED, 20);
	InitTrap(&Proc()->trap[EVT_MENU], "MENU", DISABLED, 25);
	InitTrap(&Proc()->trap[EVT_BREAK], "BREAK", ENABLED, 30);
	InitTrap(&Proc()->trap[EVT_ERROR], "ERROR", ENABLED, 35);
			
	Proc()->trap[EVT_TIMER].poll = PollTimer;
	Proc()->trap[EVT_TIMER].transition = TransitionTimer;
	
	Proc()->trap[EVT_POINTER].poll = PollPointer;
	
	Proc()->trap[EVT_INKEY].poll = PollInKey;
	
	Proc()->trap[EVT_MENU].poll = PollMenu;
	
	Proc()->trap[EVT_BREAK].poll = PollBreak;
	Proc()->trap[EVT_BREAK].track = BreakTracker;
	Proc()->trap[EVT_BREAK].transition = TransitionBreak;
	
	Proc()->trap[EVT_ERROR].track = ErrorTracker;
	
	CreateCQ(&Proc()->q[EVT_TIMER], sizeof(struct Event), 10, DisposeEvent);
	CreateCQ(&Proc()->q[EVT_POINTER], sizeof(struct Event), 10, DisposeEvent);
	CreateCQ(&Proc()->q[EVT_INKEY], sizeof(struct Event), 20, DisposeEvent);
	CreateCQ(&Proc()->q[EVT_MENU], sizeof(struct Event), 4, DisposeEvent);
	CreateCQ(&Proc()->q[EVT_BREAK], sizeof(struct Event), 1, DisposeEvent);
	CreateCQ(&Proc()->q[EVT_ERROR], sizeof(struct Event), 1, DisposeEvent);
	
	for(et = FIRST_EVENT_TYPE; et <= LAST_EVENT_TYPE; et++)
		InitEvent(&Proc()->activeEvent[et]);
	
	/* Install the break signal handler (on systems where signals work),
	or just ensure the break flag is cleared (on systems where polling is required). */

#if !PF_POLL_FOR_SIGNALS
	if(signal(SIGINT, &RecordBreakSignal) == SIG_ERR) {
		Proc()->mode = MODE_HALTED_FOR_EXIT;
		Proc()->pendingError = INTERNALEVENT;
	}
#endif

	BreakHasOccurred();
}

void DisposeEventTraps(void)
{
	enum EventType et;
	for(et = FIRST_EVENT_TYPE; et <= LAST_EVENT_TYPE; et++) {
		InitEvent(&Proc()->activeEvent[et]);
		DisposeCQ(&Proc()->q[et]);
	}
	
	Dispose(Proc()->activeEvent);
	Dispose(Proc()->q);
	Dispose(Proc()->trap);
}

/* Decide which (if any) programmatic event handler will be invoked. */
static struct Trap *ChooseTrap(struct Process *proc)
{
	struct Trap *best = NULL;
	unsigned long maxSequencing = 0;
	enum EventType et;

	for(et = FIRST_EVENT_TYPE; et <= LAST_EVENT_TYPE; et++) {
		struct Trap *t = &proc->trap[et];
		(*t->prioritise)(proc, t, &best, &maxSequencing);
	}

	if(best != NULL)
		best->sequencing = maxSequencing + 1;

	return best;
}

/* Invokes a single event handler on a given event, if it hasn't been
handled.
	If the 'mooted' handler under consideration is the nominated 'userHandler'
this handler will be invoked. Otherwise, the tracking function will be called.
	Returns TRUE if the event has been handled by this call. */
static bool HandleEvent(struct Trap *mooted, struct Trap *userHandler)
{
	struct Event evt;
	enum EventType type = mooted - &Proc()->trap[0];
	
	if(!DequeueFromCQ(&Proc()->q[type], &evt))
		return FALSE;

	if(mooted != userHandler) {
		(*mooted->track)(mooted->status, &Proc()->q[type], &evt);
		return FALSE;
	}
	else {
		struct Trap *t;

		/* Suspend this trap, and those of lower priority which are currently enabled: */

		for(t = &Proc()->trap[FIRST_EVENT_TYPE]; t <= &Proc()->trap[LAST_EVENT_TYPE]; t++)
			if(t->status == ENABLED && userHandler->priority >= t->priority) {
				t->status = SUSPENDED;
				t->suspendedAt = Proc()->callNestLevel;
			}

		Proc()->activeEvent[type] = evt;
		
		/* Call the subprogram or jump to the label. */
		
		if(userHandler->subprogramHandler)
			CallSubprogram(userHandler->handler.subprogram, NULL, 0, TRUE);
		else
			Proc()->currentPosition = userHandler->handler.simpleLocation.label;
		
		return TRUE;
	}
}

extern void PollUIEvents(struct Process *);

/* Called as the last step in execution of each BASIC statement.
	Because of the polling ('nonzero-overhead') nature of BASIC event checking, this
function is designed to execute as quickly as possible if there are no events to be
handled. */
bool CheckForEvents(struct Process *proc)
{
	enum EventType et;
	bool anyEvents = FALSE, handledEvent = FALSE;

	PollUIEvents(proc);
	CheckAudio(proc);
	
	/* Retrieve events, recording any which are sufficiently recent and interesting: */

	for(et = FIRST_EVENT_TYPE; et <= LAST_EVENT_TYPE; et++) {
		struct Trap *t = &proc->trap[et];	
		if(t->status != DISABLED) {
			(*t->poll)(t);
			anyEvents |= EventAvailable(proc, et);
		}
	}

	/* Handle, or not: */
	
	if(anyEvents) {
		struct Trap *userTrap = ChooseTrap(proc);
		
		for(et = FIRST_EVENT_TYPE; et <= LAST_EVENT_TYPE; et++)
			handledEvent |= HandleEvent(&proc->trap[et], userTrap);
	}
	
	return handledEvent;
}

void ReenableEventTraps(struct Process *proc, short newCallNestLevel)
{
	struct Trap *t;

	assert(newCallNestLevel >= SCOPE_MAIN);
	
	for(t = &proc->trap[FIRST_EVENT_TYPE]; t <= &proc->trap[LAST_EVENT_TYPE]; t++)
		if(newCallNestLevel <= t->suspendedAt) {
			t->suspendedAt = SCOPE_NONEXISTENT;
			if(t->status == SUSPENDED)
				t->status = ENABLED;
		}
}

void CauseError(Error code)
{
	struct Event e;
	
	assert(code != SUCCESS);
	
	if(Proc()->mode == MODE_INTERACTIVE)
		/* Event handling doesn't happen in interactive mode. */
		Proc()->pendingError = code;
	else {
		assert(Proc()->currentStatementStart != NULL);
		
		InitEvent(&e);
		e.lngInfo = (long)code;
		e.position = Proc()->currentStatementStart;
		EnqueueOnCQ(&Proc()->q[EVT_ERROR], &e, FALSE);
	}
}

bool ValidEventName(const QString *name)
{
	enum EventType et;
	for(et = FIRST_EVENT_TYPE; et <= LAST_EVENT_TYPE && !QsEqNoCase(name, &Proc()->trap[et].name); et++)
		;
	return et <= LAST_EVENT_TYPE;
}

static enum EventType EventTypeFromName(const QString *name)
{
	enum EventType et;
	for(et = FIRST_EVENT_TYPE; et <= LAST_EVENT_TYPE && !QsEqNoCase(name, &Proc()->trap[et].name); et++)
		;
	assert(et <= LAST_EVENT_TYPE);
	return et;
}

void On_(const QString *toks, unsigned nToks)
{
	enum EventType eventClass;
	struct Trap *trap;
	Error error;
	bool subprogram;

	/* Check form of arguments: */

	if(nToks < 4) {
		CauseError(BADARGCOUNT);
		return;
	}
	
	subprogram = QsEqNoCase(&toks[nToks - 3], &g_CallKeyword);
	
	if(!subprogram && !QsEqNoCase(&toks[nToks - 3], &g_GoToKeyword)) {
		CauseError(BADSYNTAX);
		return;
	}
	
	/* Find the appropriate event trap and populate it: */

	if(!ValidEventName(&toks[0])) {
		CauseError(ER_UNKNOWN_EVENT_TYPE);
		return;
	}
	
	eventClass = EventTypeFromName(&toks[0]);
	trap = &Proc()->trap[eventClass];
	
	if(eventClass == EVT_TIMER) {
		/* ON TIMER(<n>) {CALL|GOTO} <name>

		At present, <n> can be a numerical (but not hex or octal) constant, 
		or a variable or constant name. */
		/* TODO should support an expression for <n> */

		if(QsGetFirst(&toks[1]) != '(' || QsGetFirst(&toks[3]) != ')') {
			CauseError(BADSYNTAX);
			return;
		}

		if(IsNumeric(&toks[2])) {
			Scalar n;

			/* Because ParseNumber assumes it's being used for input read by the program
				rather than the program text itself, translate its error code - */
			if(ParseNumber(&toks[2], &n) != SUCCESS) {
				CauseError(BADCONSTANT);
				return;
			}
			
			trap->timerGranularity = (float)GetDouble(&n);
		}
		else {
			const BObject *defn = LookUpCheckingType(&toks[2], Proc()->callNestLevel);
			
			if(defn == NULL || !IsVariable(defn) || IsArray(defn)) { /* vvv */
				CauseError(LookUpIgnoringType(&toks[2], Proc()->callNestLevel) != NULL ? BADARGTYPE : UNDEFINEDVARORFUNC);
				return;
			}
			
			trap->timerGranularity = (float)GetDouble(VarData(defn));
		}
		
		/*fprintf(stderr, "TIMER set: %f\n", trap->timerGranularity);*/
		
		PfGetSystemTimeStamp(&trap->lastTimerTick); /* Begin timing now. */	
	}

	trap->subprogramHandler = subprogram;
	if(subprogram) {
		if((error = GetStatement(&toks[nToks - 2], &trap->handler.subprogram)) == SUCCESS) {
			if(!IsSubprogram(trap->handler.subprogram) || trap->handler.subprogram->formalCount != 0)
				error = BADHANDLER;
		}
	}
	else {
		trap->handler.simpleLocation.label = FindReferencedLabel(&toks[nToks - 2], &error);
		trap->handler.simpleLocation.context = StartOfCurrentSubprogram();
	}
	
	if(error != SUCCESS) {
		ResetTrap(trap);
		CauseError(error);
		return;
	}
	
	/* Switch on event tracking for this handler: */

	if(trap->status != ENABLED)
		(*trap->transition)(trap, SUSPENDED); /* Note that it's not set to enabled. */
	trap->suspendedAt = SCOPE_NONEXISTENT;
		/* This is necessary to ensure a spurious return from an event handler
			isn't attempted. */
}

static void TransitionTrap(const QString *toks, unsigned nToks, enum HandlerState newStatus)
{
	if(nToks != 2)
		CauseError(BADSYNTAX);
	else if(!ValidEventName(&toks[0]))
		CauseError(ER_UNKNOWN_EVENT_TYPE);
	else {
		enum EventType et = EventTypeFromName(&toks[0]);
		struct Trap *t = &Proc()->trap[et];
		(*t->transition)(t, newStatus);
	}
}

void Disable_(const QString *toks, unsigned nToks)
{
	TransitionTrap(toks, nToks, DISABLED);
}

void Suspend_(const QString *toks, unsigned nToks)
{
	TransitionTrap(toks, nToks, SUSPENDED);
}

void Enable_(const QString *toks, unsigned nToks)
{
	TransitionTrap(toks, nToks, ENABLED);
}

void Forget_(const QString *toks, unsigned nToks)
{
	struct Trap *trap;
	const QString *name = &toks[0];
	enum HandlerState status;
	enum EventType code;

	if(nToks != 2) {
		CauseError(BADSYNTAX);
		return;
	}
	
	if(!ValidEventName(name)) {
		CauseError(ER_UNKNOWN_EVENT_TYPE);
		return;
	}

	code = EventTypeFromName(name);
	trap = &Proc()->trap[code];

	/* Ensure not currently executing in a handler for this event. */

	if(trap->suspendedAt != SCOPE_NONEXISTENT) {
		CauseError(FORGETINHANDLER);
		return;
	}

	/* Save the trap's status so it can be restored. Then disable it to flush all pending events. */

	status = trap->status;
	(*trap->transition)(trap, DISABLED);

	/* Clear the event handler. */

	ResetTrap(trap);
	
	if(code == EVT_BREAK)
		BreakHasOccurred(); /* Clear break flag for platforms where it persists. */
	
	InitEvent(&Proc()->activeEvent[code]);
	
	/* Restore the trap's tracking status. */
	
	(*trap->transition)(trap, status);
}

void Sleep_(BObject *arg, unsigned count)
{
	do
		PfSleep(10 * 1000); /* 10 milliseconds */
	while(!CheckForEvents(Proc()));
}

void Wait_(BObject *arg, unsigned count)
{
	double remaining = GetDouble(&arg[0].value.scalar); /* Avoid VBCC conversion bug. */

	if(remaining < 0) {
		CauseError(OUTSIDEDOMAIN);
		return;
	}

	while(remaining > 0) {
		const double oneMillion = 1000000.0;
		unsigned slice = (unsigned)((remaining > 1.0 ? 1.0 : remaining) * oneMillion); /* in micros */
		PfSleep(slice);
		remaining -= slice / oneMillion;
	}
}

void Error_(BObject *arg, unsigned count)
{
	CauseError(MakeTrappableError((unsigned)arg[0].value.scalar.value.number.s % 256, ER_M_USER));
}

void Err_(Scalar *result, const BObject *arg, unsigned count)
{
	short trapCallNestLevel = Proc()->trap[EVT_ERROR].suspendedAt;
	long errNum = trapCallNestLevel >= SCOPE_MAIN && trapCallNestLevel <= Proc()->callNestLevel
		? ErrorCode((Error)Proc()->activeEvent[EVT_ERROR].lngInfo) : 0;
	SetFromLong(result, errNum, T_INT);
}

void Erl_(Scalar *result, const BObject *arg, unsigned count)
{
	ErLab_(result, arg, count);
	if(ChangeType(result, T_INT) != SUCCESS)
		InitScalar(result, T_INT, FALSE);
}

void ErLab_(Scalar *result, const BObject *arg, unsigned count)
{
	struct Event *e = &Proc()->activeEvent[EVT_ERROR];
	short trapCallNestLevel = Proc()->trap[EVT_ERROR].suspendedAt;
	
	InitScalarAsString(result);
	
	if(trapCallNestLevel >= SCOPE_MAIN && trapCallNestLevel <= Proc()->callNestLevel) {
		if(QsIsNull(&e->strInfo))
			FindLabelPreceding(&e->strInfo, e->position);
		QsCopy(&result->value.string, &e->strInfo);
	}
}

void Break_(BObject *arg, unsigned count)
{
	struct Event e;
	InitEvent(&e);
	e.position = Proc()->currentStatementStart;
	EnqueueOnCQ(&Proc()->q[EVT_BREAK], &e, TRUE);
}

/* The highest-priority trap which has been suspended. */
static struct Trap *MostRecentTrap(void)
{
	struct Trap *t, *mostRecent = NULL;

	for(t = &Proc()->trap[FIRST_EVENT_TYPE]; t <= &Proc()->trap[LAST_EVENT_TYPE]; t++)
		if(t->suspendedAt > SCOPE_NONEXISTENT
		&& (mostRecent == NULL 
		  || t->priority > mostRecent->priority
		  || (t->priority == mostRecent->priority
		      && t->sequencing > mostRecent->sequencing)))
			mostRecent = t;
	
	return mostRecent;
}

void Resume_(const QString *toks, unsigned nToks)
{
	const QString *labelName = &toks[0];
	const char *label = NULL;
	struct Trap *activeTrap;
	short originalNestLevel = Proc()->callNestLevel, trapNestLevel;
	
	/* Check argument syntax: */

	if(nToks != 2) {
		CauseError(BADARGCOUNT);
		return;
	}

	/* Check that an event has in fact been trapped: */

	activeTrap = MostRecentTrap();
	
	if(activeTrap == NULL) {
		CauseError(RESUMEOUTSIDEHANDLER);
		return;
	}

	trapNestLevel = activeTrap->suspendedAt;
	
	/* Unwind subprogram calls.
		Any EXITSUB calls must be done after initially checking whether a trap has
	occurred, because EXITSUB calls ReenableEventTraps, which can change the status
	of traps and events. */

	if(activeTrap->subprogramHandler) {
		assert(originalNestLevel > SCOPE_MAIN);
		
		do {
			ExitSub_(NULL, 0);
			label = FindReferencedLabel(labelName, NULL);
		}
		while(label == NULL && Proc()->callNestLevel > SCOPE_MAIN);
	}
	else {
		label = FindReferencedLabel(labelName, NULL);
		while(label == NULL && Proc()->callNestLevel > SCOPE_MAIN) {
			ExitSub_(NULL, 0);
			label = FindReferencedLabel(labelName, NULL);
		}
	}

	/* Sanity check that:
	
	1. The label was found in some environment.
	2. The label is at a lower or equal nest level than the point of the trap. 
		Indicates a bug with event handling; hence an assertion. */

	if(label == NULL) {
		CauseError(UNDEFINEDLABEL);
		return;
	}
	
	assert(Proc()->callNestLevel <= originalNestLevel);
	
	if(trapNestLevel < Proc()->callNestLevel) {
		CauseError(RESUMEINTOHANDLER);
		return;
	}
	
	/* Slip out of any GOSUBs or block control structures remaining on the stack: */

	DiscardCurrentControlFlow();

	/* Explicitly mark the trap as not executing. This is redundant
		for subprogram traps but required for GOTO ones: */
	
	activeTrap->suspendedAt = SCOPE_NONEXISTENT;
	
	/* Now jump to the label: */

	Proc()->currentPosition = label;
}
