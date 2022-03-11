/****** GUI.c ******/

/* 
	$VER: gui.c 0.16A (12.15.2016)
	
	Support for windowing, menus, bitmap graphics, and other GUI things.
*/

#include <string.h>
#include "interpreter.h"
#include "platform.h"
#include "builtin.h"
#include "heap.h"
#include "process.h"

typedef struct BasicPoint_struct { 
	short x, y; 
} BasicPoint;

typedef struct BasicRectangle_struct {
	BasicPoint topLeft, bottomRight;
} BasicRectangle;

/* Special pen values used when a 'penOrDefault' parameter is passed - */
#define PEN_BG -2 /* use current 'background' pen colour, or background colour of window/view/screen */
#define PEN_FG -1 /* use current 'foreground' pen colour, or default foreground colour of window/view/screen */

#include "gui_amiga.h"
#include "gui_generic.h"

struct AnimatedObject {
	PfWindowHandle window;
	BasicPoint position;
	short vx, vy; /* Velocity in pixels/s. */
	short ax, ay; /* Acceleration in pixels/s/s. */
	bool visible, moving;
	struct PfSystemTimeStamp lastMovedAt;
	BasicRectangle bounds;
	BasicRectangle clipRegion;
	short priority;
	unsigned short collisionMask;
	PfAnimatedObjectHandle image;
};

#define MAX_COLLISIONS 16 /* In AmigaBASIC, this seems to be a self-imposed limit, rather than from the system. */

struct Collision {
	unsigned char windowId;
	unsigned char object1Id;
	signed char object2OrBorderId; /* -1 = Top border, -2 = Left border, -3 = Bottom border and -4 = Right border */
};

struct UserInterface {
	PfScreenHandle screen[MAX_SCREENS];
	PfWindowHandle window[MAX_WINDOWS];
	unsigned short openWindowCount;
	PfWindowHandle outputWindow;
	struct AnimatedObject *animObj;
	unsigned short animObjCount; /* Not all necessarily displayed or populated! */
	unsigned short maxAnimObj; /* Size of the animObj array. */
	/* Event logging - */
	PfKeypress keyData;
	PfPointer pointer;
	PfMenuSelection menuChoice;
	struct Collision collision[MAX_COLLISIONS];
	bool keyWasPressed, pointerOperated, menuSelected, objectsCollided;
};

static struct UserInterface *Gui(void)
{
	struct Process *proc = Proc();
	return proc != NULL ? proc->gui : NULL;
}

static void InitUI(void)
{
	if(Gui() == NULL) {
		struct UserInterface *gui = Proc()->gui = New(sizeof(*gui));
		int id;

		memset(gui, NUL, sizeof(*gui));

		for(id = 0; id < MAX_SCREENS; id++)
			gui->screen[id] = NULL_SCREEN_HANDLE;
		for(id = 0; id < MAX_WINDOWS; id++)
			gui->window[id] = NULL_WINDOW_HANDLE;
		
		gui->openWindowCount = 0;
		gui->outputWindow = NULL_WINDOW_HANDLE;
		
		gui->animObj = NULL;
		gui->animObjCount = gui->maxAnimObj = 0;
		
		gui->keyWasPressed = gui->pointerOperated = gui->menuSelected = gui->objectsCollided = FALSE;
	}
}

static void InitGfx(void)
{
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	InitUI();
#endif
}

static PfWindowHandle OutputWindow(void)
{
	struct UserInterface *ui = Gui();
	return ui != NULL ? ui->outputWindow : NULL_WINDOW_HANDLE;
}

static bool WindowExists(const PfWindowHandle w)
{
	int i;
	for(i = 0; i < MAX_WINDOWS && Gui()->window[i] != w; i++)
		;
	return i < MAX_WINDOWS;
}

static void RemoveAnimatedObject(int id);

static void RemoveWindow(int id)
{
	if(Gui()->window[id] != NULL_WINDOW_HANDLE) {
		int i;
		
		if(OutputWindow() == Gui()->window[id])
			Gui()->outputWindow = NULL_WINDOW_HANDLE;
		
		for(i = 0; i < Gui()->animObjCount; i++)
			if(Gui()->animObj[i].window == Gui()->window[id])
				RemoveAnimatedObject(i);
		
		RemoveWindowNative(Gui()->window[id], Gui()->openWindowCount == 1);
		
		Gui()->window[id] = NULL_WINDOW_HANDLE;
		--Gui()->openWindowCount;
	}
}

static void RemoveScreen(int id)
{
	if(Gui()->screen[id] != NULL_SCREEN_HANDLE) {
		ReleaseScreenNative(Gui()->screen[id]);
		Gui()->screen[id] = NULL_SCREEN_HANDLE;
	}
}

void CleanUpUI(void)
{
	if(Gui() != NULL) {
		int id;

		for(id = 0; id < Gui()->animObjCount; id++)
			RemoveAnimatedObject(id);
		
		for(id = 0; id < MAX_WINDOWS; id++)
			RemoveWindow(id);
		
		for(id = 0; id < MAX_SCREENS; id++)
			RemoveScreen(id);
			
		Dispose(Proc()->gui);
		Proc()->gui = NULL;
	}
}

void PollUIEvents(struct Process *proc)
{
	if(proc->gui != NULL) {
		PfWindowEvent nextEvent;
		
		while(GetNextWindowEventNative(&nextEvent)) {
			proc->gui->keyWasPressed |= TranslateKeyPressNative(&nextEvent, &proc->gui->keyData);
			proc->gui->pointerOperated |= TranslatePointerActionNative(&nextEvent, &proc->gui->pointer);
			proc->gui->menuSelected |= TranslateMenuActionNative(&nextEvent, &proc->gui->menuChoice);
		}
	}
}

void Screen_(BObject *arg, unsigned count)
{
	int id = arg[0].value.scalar.value.number.s;
	const QString *title = &arg[1].value.scalar.value.string;
	int width = arg[2].value.scalar.value.number.s;
	int height = arg[3].value.scalar.value.number.s;
	int depth = arg[4].value.scalar.value.number.s;
	unsigned long mode = (unsigned long)arg[5].value.scalar.value.number.l;
	
	if(!FeatureAvailable(Screen_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	InitUI();
	
	if(id < 0 || id >= MAX_SCREENS || Gui()->screen[id] != NULL_SCREEN_HANDLE) {
		CauseError(ER_BAD_SCREEN_ID);
		return;
	}
	
	if((Gui()->screen[id] = GetScreenNative(title, width, height, depth, mode)) == NULL_SCREEN_HANDLE)
		CauseError(ER_SCREEN);
}

void ScreenClose_(BObject *arg, unsigned count)
{
	int id = arg[0].value.scalar.value.number.s;

	if(!FeatureAvailable(Screen_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL || id < 0 || id >= MAX_SCREENS || Gui()->screen[id] == NULL_SCREEN_HANDLE) {
		CauseError(ER_BAD_SCREEN_ID);
		return;
	}

#if MULTIPLE_VIRTUAL_SCREENS_SUPPORTED
	{
		int wid;
		bool ok = TRUE;
		for(wid = 0; wid < MAX_WINDOWS && ok == TRUE; wid++)
			ok &= Gui()->window[wid] == NULL_WINDOW_HANDLE || !WindowUsesScreenNative(Gui()->window[wid], Gui()->screen[id]);
		if(!ok) {
			CauseError(ER_SCREEN_HAS_WINDOW);
			return;
		}
	}
#endif

	RemoveScreen(id);
}

void Window_(BObject *arg, unsigned count)
{
	int id = arg[0].value.scalar.value.number.s;
	const QString *title = &arg[1].value.scalar.value.string;
	short x1 = arg[2].value.scalar.value.number.s;
	short y1 = arg[3].value.scalar.value.number.s;
	short x2 = arg[4].value.scalar.value.number.s;
	short y2 = arg[5].value.scalar.value.number.s;
	int screenID = arg[6].value.scalar.value.number.s;
	const QString *screenTitle = &arg[7].value.scalar.value.string;
	unsigned long flags = arg[8].value.scalar.value.number.l;
	PfScreenHandle wScreen; 
	BasicRectangle extent;

	if(!FeatureAvailable(Window_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	InitUI();
	
	if(id < 0 || id >= MAX_WINDOWS || Gui()->window[id] != NULL_WINDOW_HANDLE) {
		CauseError(ER_BAD_WINDOW_ID);
		return;
	}
	
	wScreen = 0 <= screenID && screenID < MAX_SCREENS ? Gui()->screen[screenID] : NULL_SCREEN_HANDLE;
	if(screenID != -1 && wScreen == NULL_SCREEN_HANDLE) {
		CauseError(ER_BAD_SCREEN_ID);
		return;
	}
	
	extent.topLeft.x = x1;
	extent.topLeft.y = y1;
	extent.bottomRight.x = x2;
	extent.bottomRight.y = y2;
	
	if(extent.bottomRight.x <= extent.topLeft.x || extent.bottomRight.y <= extent.topLeft.y) {
		CauseError(ER_BAD_SIZE);
		return;
	}
	
	if((Gui()->window[id] = OpenWindowNative(id, title, &extent, wScreen, screenTitle, flags)) != NULL_WINDOW_HANDLE) {
		++Gui()->openWindowCount;
		Gui()->outputWindow = Gui()->window[id];
	}
	else
		CauseError(ER_WINDOW);
}

void WindowOutput_(BObject *arg, unsigned count)
{
	int id = arg[0].value.scalar.value.number.s;

	if(!FeatureAvailable(Window_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	InitUI();
	
	if(id < 0 || id >= MAX_WINDOWS || Gui()->window[id] == NULL_WINDOW_HANDLE || Gui()->openWindowCount == 0) {
		CauseError(ER_BAD_WINDOW_ID);
		return;
	}
	
	Gui()->outputWindow = Gui()->window[id];
}

void WindowClose_(BObject *arg, unsigned count)
{
	int id = arg[0].value.scalar.value.number.s;

	if(!FeatureAvailable(Window_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL || id < 0 || id >= MAX_WINDOWS || Gui()->window[id] == NULL_WINDOW_HANDLE || Gui()->openWindowCount == 0) {
		CauseError(ER_BAD_WINDOW_ID);
		return;
	}

	RemoveWindow(id);
}

void WindowInfo_(Scalar *result, const BObject *arg, unsigned count)
{
	int id = arg[0].value.scalar.value.number.s;
	int infoKey = arg[1].value.scalar.value.number.s;
	struct UserInterface *gui = Gui();
	
	if(!FeatureAvailable(Window_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(gui == NULL || id < 0 || id >= MAX_WINDOWS || gui->window[id] == NULL_WINDOW_HANDLE || gui->openWindowCount == 0) {
		SetError(result, ER_BAD_WINDOW_ID);
		return;
	}
	
	SetFromLong(result, GetWindowInfoNative(gui->window[id], (unsigned)infoKey), T_LONG);
}
	
void Palette_(BObject *arg, unsigned count)
{	
	int penID = arg[0].value.scalar.value.number.s;
	float red = arg[1].value.scalar.value.number.f;
	float green = arg[2].value.scalar.value.number.f;
	float blue = arg[3].value.scalar.value.number.f;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(Colour_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif

	SetPaletteColourNative(win, penID, red, green, blue);
}

void Colour_(BObject *arg, unsigned count)
{
	short fgID = arg[0].value.scalar.value.number.s;
	short bgID = arg[1].value.scalar.value.number.s;
	PfWindowHandle win = OutputWindow();

	if(!FeatureAvailable(Colour_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	if(fgID != -1)
		SetWindowPenNative(win, TRUE, fgID);
	
	if(bgID != -1)
		SetWindowPenNative(win, FALSE, bgID);
}

void Point_(Scalar *result, const BObject *arg, unsigned count)
{
	BasicPoint p;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(PSet_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	p.x = arg[0].value.scalar.value.number.s;
	p.y = arg[1].value.scalar.value.number.s;
	
	SetFromLong(result, GetPixelPaletteColourNative(win, &p), T_INT);
}
	
void PSet_(BObject *arg, unsigned count)
{
	BasicPoint p;
	short penID = arg[2].value.scalar.value.number.s;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(PSet_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	p.x = arg[0].value.scalar.value.number.s;
	p.y = arg[1].value.scalar.value.number.s;
	
	SetPixelNative(win, &p, penID);
}
	
void PReset_(BObject *arg, unsigned count)
{
	BasicPoint p;
	PfWindowHandle win = OutputWindow();

	if(!FeatureAvailable(PSet_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	p.x = arg[0].value.scalar.value.number.s;
	p.y = arg[1].value.scalar.value.number.s;
	
	SetPixelNative(win, &p, PEN_BG);
}

static void OperateOnRectangularRegion(BObject *arg, void (*act)(PfWindowHandle, unsigned char *, const BasicPoint *))
{
	short x1 = arg[0].value.scalar.value.number.s;
	short y1 = arg[1].value.scalar.value.number.s;
	short x2 = arg[2].value.scalar.value.number.s;
	short y2 = arg[3].value.scalar.value.number.s;
	BObject *a = &arg[4];
	BasicRectangle extent;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(ScreenGet_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	extent.topLeft.x = x1;
	extent.topLeft.y = y1;
	extent.bottomRight.x = x2;
	extent.bottomRight.y = y2;
	
	if(extent.bottomRight.x < extent.topLeft.x || extent.bottomRight.y < extent.topLeft.y) {
		CauseError(ER_BAD_SIZE);
		return;
	}
	
	/* Check array size is OK by attempting to subscript it: */
	
	{
		long size = (y2 - y1 + 1) * (x2 - x1 + 1);
		BObject subscript;
		struct Variable indexer;
		
		InitObject(&subscript, LITERAL);
		subscript.value.scalar = *g_ZeroLongInt;
		subscript.value.scalar.value.number.l = size; /* TODO scale by size of array items */
		
		InitVariable(&indexer, NonPointer(VarData(a)->type), TRUE);
		
		if(IndexArray(&indexer, VarPtr(a), &subscript, 1) != SUCCESS) {
			CauseError(ER_BAD_SIZE);
			return;
		}
	}
	
	{
		BasicPoint p;
		Scalar aptr;
		unsigned char *pptr; /* pen is assumed to fit in a byte */
		
		SetPointerToElement(&aptr, VarData(a), 0);
		pptr = GetPointer(&aptr);
		
		/* TODO too slow! */
		for(p.y = y1; p.y <= y2; p.y++)
			for(p.x = x1; p.x <= x2; p.x++)
				act(win, pptr++, &p); 
	}
}

static void ReadPt(PfWindowHandle win, unsigned char *pptr, const BasicPoint *p) 
{
	*pptr = GetPixelPaletteColourNative(win, p);
}

static void WritePt(PfWindowHandle win, unsigned char *pptr, const BasicPoint *p)
{
	SetPixelNative(win, p, *pptr);
}

void ScreenGet_(BObject *arg, unsigned count)
{
	OperateOnRectangularRegion(arg, &ReadPt);
}

void ScreenPut_(BObject *arg, unsigned count)
{
	OperateOnRectangularRegion(arg, &WritePt);
}

void Scroll_(BObject *arg, unsigned count)
{
	BasicRectangle region;
	PfWindowHandle win = OutputWindow();
	short dx, dy;
	
	if(!FeatureAvailable(Scroll_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	region.topLeft.x = arg[0].value.scalar.value.number.s;
	region.topLeft.y = arg[1].value.scalar.value.number.s;
	region.bottomRight.x = arg[2].value.scalar.value.number.s;
	region.bottomRight.y = arg[3].value.scalar.value.number.s;
	dx = arg[4].value.scalar.value.number.s;
	dy = arg[5].value.scalar.value.number.s;
	
	MoveRegionNative(win, &region, dx, dy);
}

/* TODO colour, boxes and optionally filling them, STEP, patterns, etc. */
void Line_(BObject *arg, unsigned count)
{
	BasicPoint p1, p2;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(Line_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	p1.x = arg[0].value.scalar.value.number.s;
	p1.y = arg[1].value.scalar.value.number.s;
	p2.x = arg[2].value.scalar.value.number.s;
	p2.y = arg[3].value.scalar.value.number.s;
	
	DrawLineNative(win, &p1, &p2);
}

static void AddPointToArea(short x, short y)
{
	BasicPoint p;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(Area_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	p.x = x;
	p.y = y;
	
	if(!AddVertexToPolygonNative(win, &p))
		CauseError(ER_AREA);
}

void Area_(BObject *arg, unsigned count)
{
	AddPointToArea(arg[0].value.scalar.value.number.s, arg[1].value.scalar.value.number.s);
}

void AreaStep_(BObject *arg, unsigned count)
{
	BasicPoint pen;
	PfWindowHandle win = OutputWindow();
	
	if(win != NULL_WINDOW_HANDLE)
		GetCurrentPenPositionNative(win, &pen);
	else
		pen.x = pen.y = 0;
	
	AddPointToArea(pen.x + arg[0].value.scalar.value.number.s, pen.y + arg[1].value.scalar.value.number.s);
}

/* TODO mode parameter: 0 = fill with current pattern; 1 = invert */
void AreaFill_(BObject *arg, unsigned count)
{
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(Area_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	if(!FillPolygonNative(win))
		CauseError(ER_AREA);
}

void Paint_(BObject *arg, unsigned count)
{
	BasicPoint p;
	short penID = arg[2].value.scalar.value.number.s;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(Paint_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	p.x = arg[0].value.scalar.value.number.s;
	p.y = arg[1].value.scalar.value.number.s;
	
	if(!FloodFillNative(win, &p, penID))
		CauseError(NOMEMORY); /* assume this rather than some more obscure gfx problem */
}

void Pattern_(BObject *arg, unsigned count)
{
	short linePattern = arg[0].value.scalar.value.number.s;
	short *areaPattern = VarData(&arg[1])->value.pointer.sp;
	int areaWords = VarPtr(&arg[1])->dim.few[0] * (VarPtr(&arg[1])->dim.few[1] <= 0 ? 1 : VarPtr(&arg[1])->dim.few[1]);
	PfWindowHandle win = OutputWindow();

	if(!FeatureAvailable(Pattern_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	SetWindowLinePatternNative(win, linePattern);
	SetWindowAreaPatternNative(win, areaPattern, areaWords);
}

void Circle_(BObject *arg, unsigned count)
{
	BasicRectangle bounds;
	BasicPoint centre;
	short radius;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(Circle_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
#if !DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
#else
	InitGfx();
#endif
	
	/* TODO aspect ratio; start and end angles */
	centre.x = arg[0].value.scalar.value.number.s;
	centre.y = arg[1].value.scalar.value.number.s;
	radius = arg[2].value.scalar.value.number.s;
	
	bounds.topLeft.x = centre.x - radius;
	bounds.topLeft.y = centre.y - radius;
	bounds.bottomRight.x = centre.x + radius;
	bounds.bottomRight.y = centre.y + radius;
	
	DrawEllipseNative(win, &bounds);
}

void CircleStep_(BObject *arg, unsigned count)
{
	BasicPoint pen;
	PfWindowHandle win = OutputWindow();
	
	if(win != NULL_WINDOW_HANDLE)
		GetCurrentPenPositionNative(win, &pen);
	else
		pen.x = pen.y = 0;
		
	arg[0].value.scalar.value.number.s += pen.x;
	arg[1].value.scalar.value.number.s += pen.y;
	Circle_(arg, count);
}

void Beep_(BObject *arg, unsigned count)
{
	if(!FeatureAvailable(Beep_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	AlertUserNative(OutputWindow());
}

void WClS_(BObject *arg, unsigned count)
{
	if(OutputWindow() == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	EraseWindowInteriorNative(OutputWindow());
}

void WPrint_(BObject *arg, unsigned count)
{
	const int windowPrintTabSize = 4;
	unsigned n;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(WPrint_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	for(n = 0; n != count; n++) {
		/* Assume parameters preformatted as strings! */
		const QString *s = (const QString *)GetPointer(&arg[n].value.scalar);
		assert(arg[n].category == LITERAL && GetSimpleType(&arg[n]) == T_STRING);
		RenderTextNative(win, QsGetData(s), QsGetLength(s), windowPrintTabSize); /* TODO tabsize from gui struct? */
	}
}

void WPTab_(Scalar *result, const BObject *arg, unsigned count)
{
	short x = arg[0].value.scalar.value.number.s;
	BasicPoint p;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(WPrint_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(win == NULL_WINDOW_HANDLE) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	GetCurrentPenPositionNative(win, &p);
	p.x = x;
	SetPenPositionNative(win, &p);
	
	/* Return an empty string to avoid messing with the position. */
	InitScalar(result, T_STRING, FALSE);
}

void WLocate_(BObject *arg, unsigned count)
{
	short y = arg[0].value.scalar.value.number.s; /* Unusual order compared to most graphics statements! */
	short x = arg[1].value.scalar.value.number.s;
	short width, height;
	BasicPoint pos;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(WPrint_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	if(win == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}

	GetFontCharacterDimensionsNative(win, &width, &height);
	pos.x = (x - 1) * width;
	pos.y = (y - 1) * height;
	SetPenPositionNative(win, &pos);
}

void WPos_(Scalar *result, const BObject *arg, unsigned count)
{
	short width, height;
	BasicPoint pen;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(WPrint_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
		
	if(win == NULL_WINDOW_HANDLE) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}

	GetCurrentPenPositionNative(win, &pen);
	GetFontCharacterDimensionsNative(win, &width, &height);

	SetFromLong(result, 1 + pen.x / width, T_INT);
	
	/*rp = w->RPort;
	SetTVLongWithType(result, 
	  (long)(rp->cp_x - w->BorderLeft) / AverageFontCharWidth(rp),
	  INTEGER);*/
}

void WCsrLin_(Scalar *result, const BObject *arg, unsigned count)
{
	short width, height;
	BasicPoint pen;
	PfWindowHandle win = OutputWindow();
	
	if(!FeatureAvailable(WPrint_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
		
	if(win == NULL_WINDOW_HANDLE) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}

	GetCurrentPenPositionNative(win, &pen);
	GetFontCharacterDimensionsNative(win, &width, &height);

	SetFromLong(result, 1 + pen.y / height, T_INT);
	
	/*rp = w->RPort;
	SetTVLongWithType(result, 
	  (long)(rp->cp_y - w->BorderTop) / FontLineHeight(rp),
	  INTEGER);*/
}

/*** WindowTabs_ ***/

/*
WINDOWTABS <n>

Sets tab stops for WPRINT every <n> columns.  This does not affect the tab
spacing for PRINT statements; that is a characteristic of the terminal or
console device. */

/*void WindowTabs_(nItems, args)
	short nItems;
	struct SValue *args;
	{
	int n = args[0].value.plainConst.value.number.s;

	if(n < 0)
		EFail(OUTSIDEDOMAIN);
	windowPrintTabSize = n;
	}
*/

static void CopyMenuNameToBuffer(const QString *name, char **text)
{
	if(QsGetLength(name) > MAX_MENU_NAME_LENGTH - 1)
		CauseError(ER_BAD_MENU_NAME);
	else {
		*text = New(MAX_MENU_NAME_LENGTH);
		QsCopyToBuffer(name, *text, MAX_MENU_NAME_LENGTH);
	}
}

/* Tokenises menu name and allocates memory to store one or more of menu, item,
   subitem name.  If a name is not present, the pointer will be set to NULL.
   Returns as for LSTokenise(), i.e. the number of tokens (3 == subitem, 
   2 == item, etc.)
   Does some (by no means infallible) error checking on the name format. */
static int SplitMenuName(const QString *name, char **menu, char **item, char **subItem)
{
	static const QString separator = {"|", 1};
	QString splitName[3];
	int depth = QsTokenise(name, &splitName[0], 3, &separator);
	
	*menu = *item = *subItem = NULL;
	
	if(depth == -1 || (depth >= 1 && QsGetLast(&splitName[depth - 1]) == '|'))
		/* Name has too many components or ends in '|' */
		CauseError(ER_BAD_MENU_NAME);
	
	if(depth >= 1)
		CopyMenuNameToBuffer(&splitName[0], menu);
	if(depth >= 2) 
		CopyMenuNameToBuffer(&splitName[1], item);
	if(depth == 3)
		CopyMenuNameToBuffer(&splitName[2], subItem);
		
	return depth;
}

/*** Menu_ ***/

/*
MENU [<name$>], [<hotkey@>], [<flags>]

Creates or modifies items in the current output window's menu bar.
	<name> should be a string of the form

	<menu>[|<item>[|<subitem>]]

If the <item> and <subitem> parts are omitted, then the name describes a top-
level menu.  If the <subitem> part only is omitted, it describes an item in a 
menu.  If all three parts are included, it describes a subitem attached to an
existing menu item.  The <name> string for each menu, item, and subitem in a
particular window should be unique, because names are used to identify menu
selections.  Case differences are not significant when distinguishing between
menu names, although the case of letters will be preserved in the menu display.
	<hotkey> is an optional one-character shortcut key for the menu item.
Hotkeys cannot be changed once a menu has been defined, and should not be
assigned to top-level menus.  The default value is CHR(0), meaning that no 
hotkey is defined.
	<flags> is a long integer mask used to control the state of the menu
item.  Different flags are selected by BITORing values together.  The default
value is 0.  The meanings of flags are described in the following section.
Note that some flags can only be applied to new menu items, while others may be
used to modify existing items.

Flags
=====

Value	Meaning
-----	-----------------------------------------------------------------------
1	The menu or item is disabled, i.e. it is made no longer selectable and
	  appears 'ghosted'.  
2	The menu or item is enabled.  If the item is already enabled, this flag
	  has no effect.  By default, newly-added menu items are enabled.
4	This flag can only be specified when creating a menu item or subitem.
	  The item or subitem will be a Boolean toggle with a checkmark symbol.
	  By default, the menu is 'ticked'.
	  (Issue: top-level menus; allow addition of checkbox later?).
8	Sets the 'tick' on a checkmarked menu item.
16	Removes the 'tick' from a checkmarked menu item.
64	This flag causes the menu [sub][item] to be removed from the menu bar/
	  menu/item.  The menu/item/subitem must already exist.
	  NOTE: name can be the empty string with flag 64 set, which removes
	  the entire menu bar.
	  Only the last menu/item/subitem or the whole menu bar can be removed.

Menu events
===========

The application may receive two kinds of events from menus: MENU and MENUHELP
events.  A MENUHELP event occurs when the user makes a menu selection while
pressing the Help key.  It is recommended that you use MENUHELP events to
display descriptions of menu items to the user.  However, you may want to treat
MENUHELP events as MENU events, in which case you can use one handler for both 
MENU and MENUHELP events, and check the menu's state when an event occurs.  The
method is demonstrated in this fragment:

-------------------------------------------------------------------------------
sub handlemenu
	menuname$ = menupicked
	if abs(menustate(menuname)) <> 1 then
		REM The menu/item/subitem was disabled (ghosted).
		REM You will receive MENUHELP events even if a menu/item/
		REM subitem is disabled.

		exitsub
	endif

	select menuname
	case ....
		....
	endselect
endsub

on menuhelp call handlemenu
on menu call handlemenu
-------------------------------------------------------------------------------

See Also: ON MENU, ON MENUHELP, MENUSTATE, MENUPICKED
Versions: This statement was added in v0.09.
Future enhancements:
	Sharing of menu bar/menus/items between windows.
	Font selection.
	Handle multiple selection.
	MENUVERIFY: do I need it?
	Separators (~~~~~ lines in gadtools).
	Use ResetMenuStrip() for small changes.
Bugs: 	MENUHELP events never happen.
*/

void Menu_(BObject *arg, unsigned count)
{
	const QString *fullName = &arg[0].value.scalar.value.string;
	char hotKey = arg[1].value.scalar.value.character;
	long flags = arg[2].value.scalar.value.number.l;
	int depth;		/* 0 = none, 1 = menu, 2 = item, 3 = subitem */
	char *mName, *iName, *sName;
	
	if(!FeatureAvailable(Menu_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	/* Can only attach menu to current output window: */

	if(OutputWindow() == NULL_WINDOW_HANDLE) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}

	/* Check for contradictory flags: */

	if((flags & 3) == 3		/* Disable and enable. */
	|| (flags & 24) == 24) {	/* Set and clear checkmark. */
		CauseError(OUTSIDEDOMAIN);
		return;
	}

	depth = SplitMenuName(fullName, &mName, &iName, &sName);

	/* Empty menu name is only allowed if deleting whole menu bar. */
	
	if(depth == 0 && !(flags & 64)) {
		CauseError(ER_NO_MENU);
		return;
	}

	AlterMenuNative(OutputWindow(), mName, iName, sName, hotKey, flags);
}

/*** MenuState_ ***/

/*
n% = MENUSTATE(<name$>)

This function yields the current state of the given existing menu, menu item 
or subitem. 
	The absolute value of the integer returned indicates whether the menu/
item/subitem is enabled (available for selection) or not (ghosted).  If
ABS(MENUSTATE(id)) = 1, the m/i/si is enabled.  If ABS(MENUSTATE(id)) = 2,
the m/i/si is ghosted.
	The sign of the integer returned is only significant if the i/si has
a checkmark (is a Boolean toggle).  A negative return value indicates that the
i/si is currently 'ON' (checked), while a positive value means it is 'OFF'. 
	A nonrecoverable error occurs if the menu/item/subitem doesn't exist in
the current output window's menu bar. */

void MenuState_(Scalar *result, const BObject *arg, unsigned count)
{
	const QString *fullName = &arg[0].value.scalar.value.string;
	char *menuName, *itemName, *subitemName;
	int depth;

	if(!FeatureAvailable(Menu_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(OutputWindow() == NULL_WINDOW_HANDLE) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}

	depth = SplitMenuName(fullName, &menuName, &itemName, &subitemName);
	
	if(depth == 0) {
		SetError(result, ER_NO_MENU);
		return;
	}
	
	SetFromLong(result, GetMenuItemStateNative(OutputWindow(), menuName, itemName, subitemName), T_INT);
}

bool MenuWasSelected(void)
{
	struct UserInterface *gui = Gui();
	if(gui != NULL && gui->menuSelected) {
		gui->menuSelected = FALSE;
		return TRUE;
	}
	else
		return FALSE;
}

/*** MenuPicked_ ***/

/*
name$ = MENUPICKED

This function returns the name of the last selected menu item, or "" if
none has been selected since the last call.  It is used both for HELP selection
and normal selection. 
*/

void MenuPicked_(Scalar *result, const BObject *arg, unsigned count)
{
	if(!FeatureAvailable(Menu_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL) {
		SetError(result, ER_NO_MENU);
		return;
	}
	
	InitScalarAsString(result);
	GetMenuSelectionNative(&result->value.string, &Gui()->menuChoice);
}

bool KeyWasPressed(void)
{
	struct UserInterface *gui = Gui();
	if(gui != NULL && gui->keyWasPressed) {
		gui->keyWasPressed = FALSE;
		return TRUE;
	}
	else
		return FALSE;
}

void InKey_(Scalar *result, const BObject *arg, unsigned count)
{
	if(!FunctionAvailable(InKey_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	InitScalarAsString(result);
	GetKeyPressDataNative(&result->value.string, &Gui()->keyData);
}

bool PointerWasOperated(void)
{
	struct UserInterface *gui = Gui();
	if(gui != NULL && gui->pointerOperated) {
		gui->pointerOperated = FALSE;
		return TRUE;
	}
	else
		return FALSE;
}

/*** Mouse_ ***/

/*
m% = MOUSE(<n>)

Returns information on the cursor of the mouse or other pointing device.  The 
parameter <n> is used to choose the value required and should be one of the
following integers:

0	TRUE (1) iff the mouse has been clicked since the last call to MOUSE.
	Otherwise FALSE (0).
1	TRUE iff a double-click has occurred.
2, 3	The X (n = 2) or Y (n = 3) coordinate of the mouse, relative to the
	input focus window the last time the button changed state.  If there is
	no input focus, these functions return -1.
4	The current button state at the instant the function is called.  TRUE
	iff the button is down.
5, 6	The X (n = 5) or Y (n = 6) coordinate of the mouse, relative to the
	current input focus, at the instant the function is called.

When checking for clicks and double-clicks, follow the Amiga convention: check
for a click on mouse up, and a double-click on mouse down.  Otherwise, you 
might get funny results from MOUSE(0) and MOUSE(1).

Example
=======

TO DO: example

Bugs: there is no support for direct handling of menu (right) button events.
	Double-click detection is unreliable.
Version: Added in v0.09.
See also: ON MOUSEMOVE, ON MOUSEDOWN, ON MOUSEUP, ON MENU
*/

void Mouse_(Scalar *result, const BObject *arg, unsigned count)
{
	short wanted = arg[0].value.scalar.value.number.s;

	if(!FunctionAvailable(Mouse_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	/*InitScalar(result, T_INT, FALSE);*/
	SetFromLong(result, GetPointerStateNative(&Gui()->pointer, wanted), T_INT);
}

static void AllocateAnimatedObjects(void)
{
	if(Gui()->animObj == NULL) {
		Gui()->maxAnimObj = MAX_OBJECTS;
		Gui()->animObj = New(sizeof(struct AnimatedObject) * Gui()->maxAnimObj);
		memset(Gui()->animObj, 0, sizeof(struct AnimatedObject) * Gui()->maxAnimObj);
		Gui()->animObjCount = 0;
	}
}

static void AlterObject(int what, const BObject *objectId, const BObject *val, unsigned nVal)
{
	short id = objectId->value.scalar.value.number.s;
	struct AnimatedObject *obj;
	unsigned n;
	
	if(!FunctionAvailable(Collision_)) {
		CauseError(NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL) {
		CauseError(ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	if(id >= MAX_OBJECTS) {
		CauseError(OUTSIDEDOMAIN);
		return;
	}
	
	AllocateAnimatedObjects();
	
	obj = &Gui()->animObj[id];
	
	if(what == 1) /* OBJECT.AX */
		obj->ax = val[0].value.scalar.value.number.s;
	else if(what == 2) /* OBJECT.AY */
		obj->ay = val[0].value.scalar.value.number.s;
	else if(what == 3) { /* OBJECT.CLIP */
		obj->clipRegion.topLeft.x = val[0].value.scalar.value.number.s;
		obj->clipRegion.topLeft.y = val[1].value.scalar.value.number.s;
		obj->clipRegion.bottomRight.x = val[2].value.scalar.value.number.s;
		obj->clipRegion.bottomRight.y = val[3].value.scalar.value.number.s;
	}
	else if(what == 4) { /* OBJECT.CLOSE */
		RemoveAnimatedObject(id);
		for(n = 0; n < nVal; n++)
			RemoveAnimatedObject(val[n].value.scalar.value.number.s);
	}
	else if(what == 5) { /* OBJECT.HIT */ /* TODO two masks */
		obj->collisionMask = val[0].value.scalar.value.number.s;
	}
	else if(what == 6) { /* OBJECT.OFF */
		obj->visible = obj->moving = FALSE;
		for(n = 0; n < nVal; n++)
			Gui()->animObj[val[n].value.scalar.value.number.s].visible
				= Gui()->animObj[val[n].value.scalar.value.number.s].moving = FALSE;
	}
	else if(what == 7) { /* OBJECT.ON */
		obj->visible = obj->moving = TRUE;
		for(n = 0; n < nVal; n++)
			Gui()->animObj[val[n].value.scalar.value.number.s].visible
				= Gui()->animObj[val[n].value.scalar.value.number.s].moving = TRUE;
	}
	else if(what == 8) /* OBJECT.PLANES */ /* TODO */
		obj->ax = obj->ax;
	else if(what == 9) /* OBJECT.PRIORITY */
		obj->priority = val[0].value.scalar.value.number.s;
	else if(what == 10) /* OBJECT.SHAPE */ /* TODO */
		obj->ax = obj->ax;
	else if(what == 11) { /* OBJECT.START */
		obj->moving = TRUE;
		for(n = 0; n < nVal; n++)
			Gui()->animObj[val[n].value.scalar.value.number.s].moving = TRUE;
	}
	else if(what == 12) { /* OBJECT.STOP */
		obj->moving = FALSE;
		for(n = 0; n < nVal; n++)
			Gui()->animObj[val[n].value.scalar.value.number.s].moving = FALSE;
	}
	else if(what == 13) /* OBJECT.VX */
		obj->vx = val[0].value.scalar.value.number.s;
	else if(what == 14) /* OBJECT.VY */
		obj->vy = val[0].value.scalar.value.number.s;
	else if(what == 15) /* OBJECT.X */
		obj->position.x = val[0].value.scalar.value.number.s;
	else if(what == 16) /* OBJECT.Y */
		obj->position.y = val[0].value.scalar.value.number.s;
		
	if(id >= Gui()->animObjCount)
		Gui()->animObjCount = id;
}

void Collision_(Scalar *result, const BObject *arg, unsigned count)
{
	short wanted = arg[0].value.scalar.value.number.s, i;

	if(!FunctionAvailable(Collision_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	for(i = 0; i < MAX_COLLISIONS; i++) {
		if(wanted == -1 && Gui()->collision[i].object1Id >= 1)
			SetFromLong(result, Gui()->collision[i].windowId, T_INT);
		else if(wanted == 0 && Gui()->collision[i].object1Id >= 1)
			SetFromLong(result, Gui()->collision[i].object1Id, T_INT);
		else if(Gui()->collision[i].object1Id == wanted) {
			SetFromLong(result, Gui()->collision[i].object2OrBorderId, T_INT);
			Gui()->collision[i].object1Id = -1; /* Only discard collision if checking a specific object. */
		}
	}
}

static void GetObjectProperty(Scalar *result, const BObject *objectId, int wanted)
{
	short id = objectId->value.scalar.value.number.s, val;
	
	if(!FunctionAvailable(Collision_)) {
		SetError(result, NOTIMPLEMENTED);
		return;
	}
	
	if(Gui() == NULL) {
		SetError(result, ER_NO_OUTPUT_WINDOW);
		return;
	}
	
	AllocateAnimatedObjects();
	
	if(id >= Gui()->animObjCount)
		val = -1;
	else if(wanted == 1)
		val = Gui()->animObj[id].position.x;
	else if(wanted == 2)
		val = Gui()->animObj[id].position.y;
	else if(wanted == 3)
		val = Gui()->animObj[id].vx;
	else if(wanted == 4)
		val = Gui()->animObj[id].vy;
	
	SetFromLong(result, val, T_INT);
}

void ObjectX_(Scalar *result, const BObject *arg, unsigned count) { GetObjectProperty(result, arg, 1); }
void ObjectY_(Scalar *result, const BObject *arg, unsigned count) { GetObjectProperty(result, arg, 2); }
void ObjectVX_(Scalar *result, const BObject *arg, unsigned count) { GetObjectProperty(result, arg, 3); }
void ObjectVY_(Scalar *result, const BObject *arg, unsigned count) { GetObjectProperty(result, arg, 4); }

/* Do all the animation display updates, and return true if any collisions. */
bool AnimatedObjectCollided(void)
{
	return FALSE; /* TODO */
}

static void RemoveAnimatedObject(int id)
{
	FreeAnimatedObjectNative(Gui()->animObj[id].image);
	memset(&Gui()->animObj[id], 0, sizeof(Gui()->animObj[id]));
	Gui()->animObj[id].window = NULL_WINDOW_HANDLE;
	if(id + 1 == Gui()->animObjCount)
		--Gui()->animObjCount;
}
