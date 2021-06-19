/****** platform_generic_private.h ******/

/*
	$VER: platform_generic_private.h 0.16A (5.1.2014)
	
	This header is included by platform_generic.c following all the headers for supported platforms.
	It simply selects all the 'generic' functions.
*/

#ifndef PF_RECOGNISED

#define PF_USE_GENERIC_INITIALISATION 1
#define PF_USE_GENERIC_MEMORY_ALLOCATION 1
#define PF_USE_GENERIC_PATH_CONVENTIONS 1
#define PF_USE_GENERIC_DIRECTORY_SCANNING 1
#define PF_USE_GENERIC_FILESYSTEM_INFO_ACCESS 1
#define PF_USE_GENERIC_EVENT_TIMING_FUNCTIONS 1
#define PF_USE_GENERIC_EXECUTION_TIMING_FUNCTIONS 1
#define PF_USE_GENERIC_TASKING_FUNCTIONS 1
#define PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS 1

#endif /* PF_RECOGNISED */
