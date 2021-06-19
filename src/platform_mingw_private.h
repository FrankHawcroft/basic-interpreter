/****** platform_mingw_private.h ******/

/*
	$VER: platform_mingw_private.h 0.16A (5.1.2014)
	
	Select whether 'generic' functions are used for any platform-specific aspects.
	Applies when compiling for the Minimal GNU System for Windows.
*/

#ifdef __MINGW32__

#define PF_RECOGNISED

#define PF_USE_GENERIC_INITIALISATION 1
#define PF_USE_GENERIC_MEMORY_ALLOCATION 0
#define PF_USE_GENERIC_PATH_CONVENTIONS 0
#define PF_USE_GENERIC_DIRECTORY_SCANNING 1
#define PF_USE_GENERIC_FILESYSTEM_INFO_ACCESS 1
#define PF_USE_GENERIC_EVENT_TIMING_FUNCTIONS 1
#define PF_USE_GENERIC_EXECUTION_TIMING_FUNCTIONS 0
#define PF_USE_GENERIC_TASKING_FUNCTIONS 1
#define PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS 0

#endif /* __MINGW32__ */
