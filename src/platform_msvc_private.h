/****** platform_msvc_private.h ******/

/*
	$VER: platform_msvc_private.h 0.16A (7.22.2014)
	
	Select whether 'generic' functions are used for any platform-specific aspects.
	Applies when compiling with Microsoft Visual C++ - this is assumed to be for a Windows executable output.
*/

#ifdef _MSC_VER

#define PF_RECOGNISED

#define PF_USE_GENERIC_INITIALISATION 1
#define PF_USE_GENERIC_MEMORY_ALLOCATION 0
#define PF_USE_GENERIC_PATH_CONVENTIONS 0
#define PF_USE_GENERIC_DIRECTORY_SCANNING 0
#define PF_USE_GENERIC_FILESYSTEM_INFO_ACCESS 0
#define PF_USE_GENERIC_EVENT_TIMING_FUNCTIONS 1
#define PF_USE_GENERIC_EXECUTION_TIMING_FUNCTIONS 0
#define PF_USE_GENERIC_TASKING_FUNCTIONS 1
#define PF_USE_GENERIC_MISCELLANEOUS_FUNCTIONS 0

#endif /* _MSC_VER */
