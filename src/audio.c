/****** audio.c ******/

/*
	$VER: audio.c 0.16 (8.11.2019)

	Implementation of statements for sound effects and music in BASIC.
*/

#include "interpreter.h"
#include "platform.h"
#include "builtin.h"
#include "heap.h"
#include "process.h"

#ifndef AMIGA

void InitAudio(void) { }
void CleanUpAudio(void) { }
void CheckAudio(struct Process *proc) { }
PfEventNotificationHandle GetAudioEventNotificationHandle(void) { return 0; }

void Sound_(BObject *arg, unsigned count) { CauseError(NOTIMPLEMENTED); }
void SoundResume_(BObject *arg, unsigned count) { CauseError(NOTIMPLEMENTED); }
void SoundWait_(BObject *arg, unsigned count) { CauseError(NOTIMPLEMENTED); }
void Wave_(BObject *arg, unsigned count) { CauseError(NOTIMPLEMENTED); }
void WaveSin_(BObject *arg, unsigned count) { CauseError(NOTIMPLEMENTED); }

#else /* AMIGA */

#include <clib/alib_protos.h>
#include <clib/exec_protos.h>
#include <exec/memory.h>
#include <graphics/gfxbase.h>
#include <devices/audio.h>

#include <math.h>
#include <string.h>

extern struct Library *GfxBase;

static LONG m_ClockConstant = 0;

struct Waveform {
	BYTE *wave;
	ULONG length;
};

struct AmigaChannel {
	struct MsgPort *port;
	/* TODO is there a more efficient way than having one port per channel? */
	struct IOAudio *request;
	struct Waveform playing;
	struct Waveform pending;
	bool active;
	bool wait;
};

#define NUM_AMIGA_CHANNELS 4

struct Audio {
	struct AmigaChannel chan[NUM_AMIGA_CHANNELS];
};

void InitAudio(void)
{
	if(Proc()->audio == NULL) {
		Proc()->audio = New(sizeof(struct Audio));
		memset(Proc()->audio, NUL, sizeof(struct Audio));
	}
}

static void BeginAudioIO(struct IOAudio *rq)
{
	rq->ioa_Request.io_Flags |= IOF_QUICK;
	BeginIO((struct IORequest *)rq);
}

void CleanUpAudio(void)
{
	struct AmigaChannel *ch;

	if(Proc()->audio == NULL)
		return;

	/* Flush any outstanding sounds and close each channel we still own. */
	for(ch = &Proc()->audio->chan[0]; ch < &Proc()->audio->chan[NUM_AMIGA_CHANNELS]; ch++) {
		if(ch->request != NULL) {
			if(ch->active) {
				ch->request->ioa_Request.io_Command = CMD_FLUSH;
				BeginAudioIO(ch->request);
				
				ch->request->ioa_Request.io_Command = ADCMD_FREE;
				BeginAudioIO(ch->request);
			}

			if(ch->port != NULL) {
				while(GetMsg(ch->port) != NULL) {
					/* TODO reply? */
				}
			}

			CloseDevice((struct IORequest *)ch->request);
			DeleteExtIO((struct IORequest *)ch->request);

			if(ch->port != NULL)
				DeletePort(ch->port);

			if(ch->playing.wave != NULL)
				FreeMem(ch->playing.wave, ch->playing.length);

			if(ch->pending.wave != NULL)
				FreeMem(ch->pending.wave, ch->pending.length);
		}
	}
	
	Dispose(Proc()->audio);
	Proc()->audio = NULL;
}

void CheckAudio(struct Process *proc)
{
	struct AmigaChannel *ch;

	if(proc->audio == NULL)
		return;

	for(ch = &proc->audio->chan[0]; ch < &proc->audio->chan[NUM_AMIGA_CHANNELS]; ch++) {
		if(ch->request != NULL && ch->port != NULL) {
			if(ch->active) {
				bool finished = FALSE;

				while(GetMsg(ch->port) != NULL)
					finished = TRUE;

				if(finished) {
					ch->request->ioa_Request.io_Command = ADCMD_FREE;
					BeginAudioIO(ch->request);	
					ch->active = FALSE;
				}
			}
			
			if(ch->wait && !ch->active) {
				ch->request->ioa_Request.io_Command = CMD_STOP;
				BeginAudioIO(ch->request);
				ch->wait = FALSE;
			}
		}
	}
}

PfEventNotificationHandle GetAudioEventNotificationHandle(void)
{
	struct Process *proc = Proc();
	if(proc->audio == NULL)
		return 0;
	else {
		ULONG mask = 0;
		struct AmigaChannel *ch;
		for(ch = &proc->audio->chan[0]; ch < &proc->audio->chan[NUM_AMIGA_CHANNELS]; ch++)
			mask |= (ch->request != NULL && ch->port != NULL ? ch->port->mp_SigBit : 0);
		return mask;
	}
}

/* Attempt to obtain one of the audio channels for use. This function will
create a message port, AudioIO structure and default waveform (SIN with 256 
sampling points) if none exists for the channel. It will open the device and
attempt to allocate the channel requested. */
static Error AcquireChannel(int channel, int priority)
{
	struct AmigaChannel *ch = &Proc()->audio->chan[channel];
	struct IOAudio *rq;
	BYTE result;
	UBYTE channelMask = 1 << channel;
	int otherCh;

	assert(channel >= 0 && channel <= NUM_AMIGA_CHANNELS);

	/* Need a message port for audio.device use. */
	if(ch->port == NULL && (ch->port = CreatePort(0, 0)) == NULL)
		return ER_AUDIO_OPEN_FAILED;

	if(ch->request == NULL) {
		/* In case this is the very first time we're using audio in
		this execution of the program, set the clock constant depending
		on whether we're NTSC or PAL. */
		
		if(GfxBase == NULL && (GfxBase = OpenLibrary("graphics.library", 0L)) == NULL)
			/* Should be opened by PfStart, or automatically for VBCC - but in case it isn't.
				Should be closed by PfFinish. */
			return ER_AUDIO_OPEN_FAILED;

		m_ClockConstant = (((struct GfxBase *)GfxBase)->DisplayFlags & PAL) ? 3546895 : 3579545;

		/* Build the I/O request structure, and open the device together with attempting
		to allocate the channel. */

		rq = (struct IOAudio *)CreateExtIO(ch->port, sizeof(struct IOAudio));
		if(rq == NULL)
			return ER_AUDIO_OPEN_FAILED;
		
		ch->request = rq;
		
		rq->ioa_Request.io_Message.mn_Node.ln_Pri = priority;
		rq->ioa_Request.io_Command = ADCMD_ALLOCATE;
		rq->ioa_Request.io_Flags = ADIOF_NOWAIT | IOF_QUICK;

		/* Use the same allocation key for every channel. */

		rq->ioa_AllocKey = 0;

		for(otherCh = 0; otherCh < NUM_AMIGA_CHANNELS; otherCh++)
			if(Proc()->audio->chan[otherCh].request != NULL && otherCh != channel)
				rq->ioa_AllocKey = Proc()->audio->chan[otherCh].request->ioa_AllocKey;

		rq->ioa_Data = &channelMask;
		rq->ioa_Length = 1;

		result = OpenDevice(AUDIONAME, 0L, (struct IORequest *)rq, 0L);

		if(result != 0) {
			DeleteExtIO((struct IORequest *)rq);
			ch->request = NULL;
			return result == ADIOERR_ALLOCFAILED ? ER_AUDIO_CHANNEL_DENIED : ER_AUDIO_OPEN_FAILED;
		}
	}
	else {
		/* I/O request already exists. Just allocate channel. */

		/* The below check is commented out 'cos it should be handled
		by the device. */
		/* if(ch->playing)
			return ER_AD_CHANNEL_DENIED; */

		rq = ch->request;
		rq->ioa_Request.io_Message.mn_Node.ln_Pri = priority;
		rq->ioa_Request.io_Command = ADCMD_ALLOCATE;
		rq->ioa_Request.io_Flags = ADIOF_NOWAIT;
		rq->ioa_Data = &channelMask;
		rq->ioa_Length = 1;

		BeginAudioIO(rq);
		if(rq->ioa_Request.io_Error != 0)
			return ER_AUDIO_CHANNEL_DENIED;
	}

	/*fprintf(stderr, "opened audio.device\n");*/

	/* If we got this far, we have a channel. Make sure there's some
	sample data. The default waveform is one cycle of the sine wave 
	sampled at 256 points. */

	if(ch->pending.wave != NULL) {
		if(ch->playing.wave != NULL)
			FreeMem(ch->playing.wave, ch->playing.length);
		ch->playing.wave = ch->pending.wave;
		ch->playing.length = ch->pending.length;
		ch->pending.wave = NULL;
		ch->pending.length = 0;
	}
	else if(ch->playing.wave == NULL) {
		const float pi = 3.141593;
		long i, resolution = 256;
		float x, increment;
		BYTE *waveform = (BYTE *)AllocMem(resolution, MEMF_CHIP);

		if(waveform == NULL)
			return ER_AUDIO_NO_SAMPLE_MEMORY;
		
		increment = 2.0 * pi / resolution;
		x = 0.0;
		for(i = 0; i < resolution; i++) {
			float y = sin(x) * 127.0;

			assert(y >= -128.0 && y <= 127.0);
			
			if(y < -128.0) y = -128.0;
			if(y > 127.0) y = 127.0;

			/*fprintf(stderr, "%d ", (BYTE)y);*/
			*waveform++ = (BYTE)y;
			x += increment;
		}
		
		ch->playing.wave = waveform;
		ch->playing.length = (ULONG)resolution;
		/*fprintf(stderr, "\n");*/
		
		/*fprintf(stderr, "created waveform\n");*/
	}

	return SUCCESS;
}

void Sound_(BObject *arg, unsigned count)
{
	struct AmigaChannel *ch;
	Error result;
	int channel, period, cycles;
	float frequency = arg[0].value.scalar.value.number.f;
	float duration = arg[1].value.scalar.value.number.f;
	int volume = arg[2].value.scalar.value.number.s;
	int voice = arg[3].value.scalar.value.number.s;
	int priority = arg[4].value.scalar.value.number.s;

	if(frequency < 20.0 || frequency > 15000.0
	|| duration < 0.0 || duration > 77.0
	|| volume < 0 || volume > 255
	|| voice < -1 || voice >= NUM_AMIGA_CHANNELS
	|| priority < -128 || priority > 127) {
		CauseError(ER_AUDIO_BAD_PARAMETER);
		return;
	}
	
	InitAudio();
	
	/* Set up an audio channel. */

	if(voice == -1) {
		/* Choose a channel automatically. Firstly grab any channel
		which hasn't been used yet, or isn't in use. Then try grabbing
		a channel based on our priority. */

		for(channel = 0; channel < NUM_AMIGA_CHANNELS && voice == -1; channel++) {
			if(Proc()->audio->chan[channel].request == NULL || !Proc()->audio->chan[channel].active) {
				result = AcquireChannel(channel, priority);
				if(result == SUCCESS)
					voice = channel;
			}
		}
		for(channel = 0; channel < NUM_AMIGA_CHANNELS && voice == -1; channel++) {
			result = AcquireChannel(channel, priority);
			if(result == SUCCESS)
				voice = channel;
		}
	}
	else
		/* We want a specific channel. */
		result = AcquireChannel(voice, priority);

	/* Failure to obtain an audio channel is a recoverable error. */

	if(result != SUCCESS) {
		CauseError(result);
		return;
	}

	/* Set up the request to play the sample. */

	ch = &Proc()->audio->chan[voice];

	assert(ch->playing.length % 2 == 0);
	assert(ch->playing.length <= 131072);

	/*fprintf(stderr, "params: len = %lu freq = %f dur = %f\n", ch->playing.length, frequency, duration);*/
	
	{
		double sampleIncrement, seconds, samplePoints;
		
		seconds = duration / 18.2;
		sampleIncrement = 1.0 / m_ClockConstant;
				
		/* period is for _one_ sample, I think. period must be in units of 
		1/m_ClockConstant s. */

		samplePoints = seconds * frequency * ch->playing.length;
		period = (seconds / samplePoints) / sampleIncrement;
		
		period = 30000; /* m_ClockConstant / ((LONG)ch->playing.length * frequency); */
		/*fprintf(stderr, "period = %d\n", period);*/
		if(period < 124) period = 124;		/* Sanity checks for h/w */
		if(period > 65536) period = 65536;

		/* cycles is number of times to repeat the sample data. Work this out
		from the desired frequency and duration. */

		cycles = 10000; /*frequency * seconds;*//*frequency * duration / 18.2; */
		/*fprintf(stderr, "cycles = %d\n", cycles);*/
		if(cycles < 1) cycles = 1;		/* Sanity checks for h/w */
		if(cycles > 65535) cycles = 65535;
	}

	/* memset(ch->request, '\0', sizeof(struct IOAudio)); */

	/*ch->request->ioa_Request.io_Message.mn_ReplyPort = ch->port;*/
	/* ch->request->ioa_Request.io_Device = m_Device; */
	/* ch->request->ioa_Request.io_Unit = AllocRequest.ioa_Request.io_Unit; */
	ch->request->ioa_Request.io_Command = CMD_WRITE;
	ch->request->ioa_Request.io_Flags = ADIOF_PERVOL;
	/* ch->request->ioa_AllocKey = AllocRequest.ioa_AllocKey; */
	ch->request->ioa_Data = (UBYTE *)ch->playing.wave;
	ch->request->ioa_Length = ch->playing.length;
	ch->request->ioa_Period = (UWORD)period;
	ch->request->ioa_Volume = volume / 4;		/* Scale to [0, 63] */
	ch->request->ioa_Cycles = (UWORD)cycles;

	/* Send the request to the device. */
	/* TO DO: check for stolen channel, AUDIO_NOALLOCATION. */

	BeginAudioIO(ch->request);
	
	/*fprintf(stderr, "began playing sound\n");*/

	ch->active = TRUE;
}

void Wave_(BObject *arg, unsigned count)
{
	int voice = arg[0].value.scalar.value.number.s;
	BYTE *sample, *sp;
	struct AmigaChannel *ch;
	ArraySubscript sampleSize = VarPtr(&arg[1])->dim.few[0], x; /* TODO effect of OPTION BASE */ /* vvv */

	if(voice < 0 || voice >= NUM_AMIGA_CHANNELS 
	|| VarPtr(&arg[1])->dim.few[1] != -1 /* TODO support multi-dim arrays */
	|| sampleSize > (1 << 16) || sampleSize % 2 != 0) {
		CauseError(ER_AUDIO_BAD_PARAMETER);
		return;
	}

	InitAudio();

	ch = &Proc()->audio->chan[voice];

	if((sample = (BYTE *)AllocMem(sampleSize, MEMF_CHIP)) == NULL) {
		CauseError(ER_AUDIO_NO_SAMPLE_MEMORY);
		return;
	}

	sp = sample;

	for(x = 0; x < sampleSize; x++) {
		Scalar elt;
		long y;
		
		SetPointerToElement(&elt, VarData(&arg[1]), x);
		y = GetLong(&elt);
		if(y > 127 || y < -128) {
			FreeMem(sample, sampleSize);
			CauseError(ER_AUDIO_BAD_PARAMETER);
			return;
		}
		*sp++ = y;
	}

	if(ch->pending.wave != NULL)
		FreeMem(ch->pending.wave, ch->pending.length);

	ch->pending.wave = sample;
	ch->pending.length = sampleSize;
}

void WaveSin_(BObject *arg, unsigned count)
{
	/* TODO */
}

void SoundWait_(BObject *arg, unsigned count)
{
	int i;

	InitAudio();
	
	for(i = 0; i < NUM_AMIGA_CHANNELS; i++)
		Proc()->audio->chan[i].wait = TRUE;

	/* See now AdCheck().  After it polls for completed writes, it checks
	for channels with the wait flag set, and issues a CMD_STOP to them. 
	Here's an example sequence of statements:
		SOUND f,d,vol,0		the sound begins playing
		SOUNDWAIT		all chs marked 'wait'
					other chs stopped
		SOUND f,d,vol,0		hmmm ... atm will cause an error		
		SOUND f,d,vol,1
		SOUND f,d,vol,2
		SOUND f,d,vol,3
		SOUNDRESUME
	*/
}

void SoundResume_(BObject *arg, unsigned count)
{
	int i, chMask = 0;

	InitAudio();

	for(i = 0; i < NUM_AMIGA_CHANNELS; i++)
		if(Proc()->audio->chan[i].active && !Proc()->audio->chan[i].wait)
			chMask |= 1 << i;

	for(i = 0; i < NUM_AMIGA_CHANNELS && chMask != 0; i++) {
		struct IOAudio *rq = Proc()->audio->chan[i].request;

		if(rq != NULL) {
			rq->ioa_Request.io_Command = CMD_START;
			rq->ioa_Request.io_Unit = (struct IOUnit *)chMask;
			BeginAudioIO(rq);
			return;
		}
	}
}

#endif /* AMIGA */
