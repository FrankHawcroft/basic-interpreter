/****** platform_amiga_private.h ******/

/*
	$VER: platform_amiga_private.h 0.16A (5.1.2014)
	
	Select whether 'generic' functions are used for any platform-specific aspects.
	Applies when compiling for the Amiga.
*/

#ifdef AMIGA

#define PF_RECOGNISED

#define PF_USE_GENERIC_INITIALISATION 0
#define PF_USE_GENERIC_MEMORY_ALLOCATION 0
#define PF_USE_GENERIC_PATH_CONVENTIONS 0

#ifdef __GNUC__
/* Avoid a strange bug where FileInfoBlock.fib_FileName omits the first 2 chars of the name.
	This may be due to an alignment inconsistency between the compiler and the library. */ 
#define PF_USE_GENERIC_DIRECTORY_SCANNING 1
#else
#define PF_USE_GENERIC_DIRECTORY_SCANNING 0	
#endif

#define PF_USE_GENERIC_FILESYSTEM_INFO_ACCESS 0

#ifdef __GNUC__
#define PF_USE_GENERIC_EVENT_TIMING_FUNCTIONS 0
#else
#define PF_USE_GENERIC_EVENT_TIMING_FUNCTIONS 1
#endif

#define PF_USE_GENERIC_EXECUTION_TIMING_FUNCTIONS 0
#define PF_USE_GENERIC_TASKING_FUNCTIONS 0 /* Can set to 1 if interpreter won't be made resident. */
#define PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS 0

#endif /* AMIGA */
