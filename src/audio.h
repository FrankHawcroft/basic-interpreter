/****** audio.h ******/

/*
	$VER: audio.h 0.16 (8.11.2019)
	
	Support for music and sound effects.
*/

#ifndef BAS_AUDIO_H_INCLUDED
#define BAS_AUDIO_H_INCLUDED

#include "platform.h"

struct Process;

extern void InitAudio(void);
extern void CleanUpAudio(void);
extern void CheckAudio(struct Process *);
extern PfEventNotificationHandle GetAudioEventNotificationHandle(void);

#endif /* ndef BAS_AUDIO_H_INCLUDED */
