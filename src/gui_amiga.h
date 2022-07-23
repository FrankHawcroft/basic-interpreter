/****** GUI_Amiga.h ******/

/* 
	$VER: gui_amiga.h 0.16A (1.17.2017)
	
	Graphics and windowing for the Amiga.
*/

#ifdef AMIGA

#include <clib/exec_protos.h>
#include <clib/intuition_protos.h>
#include <clib/graphics_protos.h>
#include <clib/layers_protos.h>
#include <clib/diskfont_protos.h>
#include <clib/alib_protos.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <exec/libraries.h>
#include <graphics/gfx.h>
#include <graphics/gels.h>
#include <graphics/clip.h>
#include <graphics/rastport.h>
#include <graphics/view.h>
#include <graphics/gfxbase.h>
#include <graphics/gfxmacros.h>

extern struct GfxBase *GfxBase;

typedef struct Window *PfWindowHandle;
typedef struct Screen *PfScreenHandle;
typedef struct VSprite *PfAnimatedObjectHandle;

#define NULL_SCREEN_HANDLE NULL
#define NULL_WINDOW_HANDLE NULL

static PfWindowHandle OutputWindow(void);
static bool WindowExists(const PfWindowHandle w);

typedef struct {
	USHORT code;
	USHORT qualifier;
	bool vanilla;
} PfKeypress;

typedef struct {
	BasicPoint current, starting, ending, btnDownPos, btnUpPos;
	/*PfSystemTimeStamp btnDownTime, btnUpTime, clickDownTime;*/
	unsigned short clicks;
	bool buttonDown;
} PfPointer;

typedef struct {
	int menuNum, itemNum, subItemNum;
	struct Window *window;
} PfMenuSelection;

typedef struct IntuiMessage PfWindowEvent;

static struct MsgPort *m_Port = NULL; /* Shared IDCMP. */

#define MAX_WINDOWS 20
#define MAX_SCREENS 4
#define MAX_OBJECTS 20 /* should be 'limited by available memory' */

#define MULTIPLE_VIRTUAL_SCREENS_SUPPORTED TRUE
#define DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED FALSE

/* Vector collection matrix for AreaInfo */
static UWORD *m_AreaVectorBuffer[MAX_WINDOWS];
/* Track whether an area is in progress. */
static bool m_AreaActive[MAX_WINDOWS];

#define MAX_POLYGON_VERTICES 20

#define MAX_MENU_NAME_LENGTH 40

static struct GelsInfo *m_Gels;

/* -- Start of helper code copied from https://wiki.amigaos.net/wiki/Graphics_Sprites,_Bobs_and_Animation#Initializing_the_GEL_System */

/* animtools.h */
#ifndef GELTOOLS_H
#define GELTOOLS_H

/*
** These data structures are used by the functions in animtools.c to
** allow for an easier interface to the animation system.
*/

/* Data structure to hold information for a new VSprite.                */
typedef struct newVSprite {
        WORD           *nvs_Image;      /* image data for the vsprite   */
        WORD           *nvs_ColorSet;   /* color array for the vsprite  */
        SHORT           nvs_WordWidth;  /* width in words               */
        SHORT           nvs_LineHeight; /* height in lines              */
        SHORT           nvs_ImageDepth; /* depth of the image           */
        SHORT           nvs_X;          /* initial x position           */
        SHORT           nvs_Y;          /* initial y position           */
        SHORT           nvs_Flags;      /* vsprite flags                */
        USHORT          nvs_HitMask;    /* Hit mask.                    */
        USHORT          nvs_MeMask;     /* Me mask.                     */
        } NEWVSPRITE;

/* Data structure to hold information for a new Bob.                */
typedef struct newBob {
        WORD       *nb_Image;       /* image data for the bob       */
        SHORT       nb_WordWidth;   /* width in words               */
        SHORT       nb_LineHeight;  /* height in lines              */
        SHORT       nb_ImageDepth;  /* depth of the image           */
        SHORT       nb_PlanePick;   /* planes that get image data   */
        SHORT       nb_PlaneOnOff;  /* unused planes to turn on     */
        SHORT       nb_BFlags;      /* bob flags                    */
        SHORT       nb_DBuf;        /* 1=double buf, 0=not          */
        SHORT       nb_RasDepth;    /* depth of the raster          */
        SHORT       nb_X;           /* initial x position           */
        SHORT       nb_Y;           /* initial y position           */
        USHORT      nb_HitMask;     /* Hit mask.                    */
        USHORT      nb_MeMask;      /* Me mask.                     */
        } NEWBOB ;

/* Data structure to hold information for a new animation component.       */
typedef struct newAnimComp {
        WORD  (*nac_Routine)(); /* routine called when Comp is displayed.   */
        SHORT   nac_Xt;         /* initial delta offset position.           */
        SHORT   nac_Yt;         /* initial delta offset position.           */
        SHORT   nac_Time;       /* Initial Timer value.                     */
        SHORT   nac_CFlags;     /* Flags for the Component.                 */
        } NEWANIMCOMP;

/* Data structure to hold information for a new animation sequence.         */
typedef struct newAnimSeq {
        struct AnimOb  *nas_HeadOb; /* common Head of Object.               */
        WORD   *nas_Images;         /* array of Comp image data             */
        SHORT  *nas_Xt;             /* arrays of initial offsets.           */
        SHORT  *nas_Yt;             /* arrays of initial offsets.           */
        SHORT  *nas_Times;          /* array of Initial Timer value.        */
        WORD (**nas_Routines)();    /* Array of fns called when comp drawn  */
        SHORT   nas_CFlags;         /* Flags for the Component.             */
        SHORT   nas_Count;          /* Num Comps in seq (= arrays size)     */
        SHORT   nas_SingleImage;    /* one (or count) images.               */
        } NEWANIMSEQ;

#define INTUITIONNAME "intuition.library" /* intuitionbase.h does not define a library name. */

/*#include "animtools_proto.h"     */         /* Include Prototyping. */
#endif


/* animtools_proto.h */
/*#include        <clib/dos_protos.h>
#include        <clib/exec_protos.h>
#include        <clib/graphics_protos.h>
#include        <clib/intuition_protos.h>*/

struct GelsInfo *setupGelSys(struct RastPort *rPort, BYTE reserved);
VOID            cleanupGelSys(struct GelsInfo *gInfo, struct RastPort *rPort);
struct VSprite  *makeVSprite(NEWVSPRITE *nVSprite);
struct Bob      *makeBob(NEWBOB *nBob);
struct AnimComp *makeComp(NEWBOB *nBob, NEWANIMCOMP *nAnimComp);
struct AnimComp *makeSeq(NEWBOB *nBob, NEWANIMSEQ *nAnimSeq);
VOID            freeVSprite(struct VSprite *vsprite);
VOID            freeBob(struct Bob *bob, LONG rasdepth);
VOID            freeComp(struct AnimComp *myComp, LONG rasdepth);
VOID            freeSeq(struct AnimComp *headComp, LONG rasdepth);
VOID            freeOb(struct AnimOb *headOb, LONG rasdepth);


/* animtools.c
**
** This file is a collection of tools which are used with the VSprite, Bob and Animation
** system software. It is intended as a useful EXAMPLE, and while it shows what must be
** done, it is not the only way to do it.  If Not Enough Memory, or error return, each
** cleans up after itself before returning.  NOTE that these routines assume a very specific
** structure to the GEL lists.  Make sure that you use the correct pairs together
** (i.e. makeOb()/freeOb(), etc.)
**
** Compile with SAS/C 5.10b: lc -b1 -cfist -v -y -oanimtools.o animtools.c
*/
/*#include <exec/types.h>
#include <exec/memory.h>
#include <graphics/gfx.h>
#include <graphics/gels.h>
#include <graphics/clip.h>
#include <graphics/rastport.h>
#include <graphics/view.h>
#include <graphics/gfxbase.h>*/
/*#include "animtools.h"*/

/* Setup the GELs system.  After this call is made you can use VSprites, Bobs, AnimComps
** and AnimObs.  Note that this links the GelsInfo structure into the RastPort, and calls
** InitGels().  It uses information in your RastPort structure to establish boundary collision
** defaults at the outer edges of the raster.  This routine sets up for everything - collision
** detection and all. You must already have run LoadView before ReadyGelSys is called.
*/
struct GelsInfo *setupGelSys(struct RastPort *rPort, BYTE reserved)
{
struct GelsInfo *gInfo;
struct VSprite  *vsHead;
struct VSprite  *vsTail;

if (NULL != (gInfo = (struct GelsInfo *)AllocMem(sizeof(struct GelsInfo), MEMF_CLEAR)))
        {
        if (NULL != (gInfo->nextLine = (WORD *)AllocMem(sizeof(WORD) * 8, MEMF_CLEAR)))
            {
            if (NULL != (gInfo->lastColor = (WORD **)AllocMem(sizeof(LONG) * 8, MEMF_CLEAR)))
                {
                if (NULL != (gInfo->collHandler = (struct collTable *)
                        AllocMem(sizeof(struct collTable),MEMF_CLEAR)))
                    {
                    if (NULL != (vsHead = (struct VSprite *)
                            AllocMem((LONG)sizeof(struct VSprite), MEMF_CLEAR)))
                        {
                        if (NULL != (vsTail = (struct VSprite *)
                                AllocMem(sizeof(struct VSprite), MEMF_CLEAR)))
                            {
                            gInfo->sprRsrvd   = reserved;
                            /* Set left- and top-most to 1 to better keep items */
                            /* inside the display boundaries.                   */
                            gInfo->leftmost   = gInfo->topmost    = 1;
                            gInfo->rightmost  = (rPort->BitMap->BytesPerRow << 3) - 1;
                            gInfo->bottommost = rPort->BitMap->Rows - 1;
                            rPort->GelsInfo = gInfo;
                            InitGels(vsHead, vsTail, gInfo);
                            return(gInfo);
                            }
                        FreeMem(vsHead, (LONG)sizeof(*vsHead));
                        }
                    FreeMem(gInfo->collHandler, (LONG)sizeof(struct collTable));
                    }
                FreeMem(gInfo->lastColor, (LONG)sizeof(LONG) * 8);
                }
            FreeMem(gInfo->nextLine, (LONG)sizeof(WORD) * 8);
            }
        FreeMem(gInfo, (LONG)sizeof(*gInfo));
        }
return(NULL);
}


/* Free all of the stuff allocated by setupGelSys().  Only call this routine if
** setupGelSys() returned successfully.  The GelsInfo structure is the one returned
** by setupGelSys().   It also unlinks the GelsInfo from the RastPort.
*/
VOID cleanupGelSys(struct GelsInfo *gInfo, struct RastPort *rPort)
{
rPort->GelsInfo = NULL;
FreeMem(gInfo->collHandler, (LONG)sizeof(struct collTable));
FreeMem(gInfo->lastColor, (LONG)sizeof(LONG) * 8);
FreeMem(gInfo->nextLine, (LONG)sizeof(WORD) * 8);
FreeMem(gInfo->gelHead, (LONG)sizeof(struct VSprite));
FreeMem(gInfo->gelTail, (LONG)sizeof(struct VSprite));
FreeMem(gInfo, (LONG)sizeof(*gInfo));
}


/* Create a VSprite from the information given in nVSprite.  Use freeVSprite()
** to free this GEL.
*/
struct VSprite *makeVSprite(NEWVSPRITE *nVSprite)
{
struct VSprite *vsprite;
LONG            line_size;
LONG            plane_size;

line_size = sizeof(WORD) * nVSprite->nvs_WordWidth;
plane_size = line_size * nVSprite->nvs_LineHeight;

if (NULL != (vsprite = (struct VSprite *)AllocMem((LONG)sizeof(struct VSprite), MEMF_CLEAR)))
        {
        if (NULL != (vsprite->BorderLine = (WORD *)AllocMem(line_size, MEMF_CHIP)))
            {
            if (NULL != (vsprite->CollMask = (WORD *)AllocMem(plane_size, MEMF_CHIP)))
                {
                vsprite->Y          = nVSprite->nvs_Y;
                vsprite->X          = nVSprite->nvs_X;
                vsprite->Flags      = nVSprite->nvs_Flags;
                vsprite->Width      = nVSprite->nvs_WordWidth;
                vsprite->Depth      = nVSprite->nvs_ImageDepth;
                vsprite->Height     = nVSprite->nvs_LineHeight;
                vsprite->MeMask     = nVSprite->nvs_MeMask;
                vsprite->HitMask    = nVSprite->nvs_HitMask;
                vsprite->ImageData  = nVSprite->nvs_Image;
                vsprite->SprColors  = nVSprite->nvs_ColorSet;
                vsprite->PlanePick  = vsprite->PlaneOnOff = 0x00;
                InitMasks(vsprite);
                return(vsprite);
                }
            FreeMem(vsprite->BorderLine, line_size);
            }
        FreeMem(vsprite, (LONG)sizeof(*vsprite));
        }
return(NULL);
}


/* Create a Bob from the information given in nBob.  Use freeBob() to free this GEL.
** A VSprite is created for this bob.  This routine properly allocates all double
** buffered information if it is required.
*/
struct Bob *makeBob(NEWBOB *nBob)
{
struct Bob         *bob;
struct VSprite     *vsprite;
NEWVSPRITE          nVSprite ;
LONG                rassize;

rassize = (LONG)sizeof(UWORD) * nBob->nb_WordWidth * nBob->nb_LineHeight * nBob->nb_RasDepth;

if (NULL != (bob = (struct Bob *)AllocMem((LONG)sizeof(struct Bob), MEMF_CLEAR)))
        {
        if (NULL != (bob->SaveBuffer = (WORD *)AllocMem(rassize, MEMF_CHIP)))
            {
            nVSprite.nvs_WordWidth  = nBob->nb_WordWidth;
            nVSprite.nvs_LineHeight = nBob->nb_LineHeight;
            nVSprite.nvs_ImageDepth = nBob->nb_ImageDepth;
            nVSprite.nvs_Image      = nBob->nb_Image;
            nVSprite.nvs_X          = nBob->nb_X;
            nVSprite.nvs_Y          = nBob->nb_Y;
            nVSprite.nvs_ColorSet   = NULL;
            nVSprite.nvs_Flags      = nBob->nb_BFlags;
            /* Push the values into the NEWVSPRITE structure for use in makeVSprite(). */
            nVSprite.nvs_MeMask     = nBob->nb_MeMask;
            nVSprite.nvs_HitMask    = nBob->nb_HitMask;

            if ((vsprite = makeVSprite(&nVSprite)) != NULL)
                {
                vsprite->PlanePick = nBob->nb_PlanePick;
                vsprite->PlaneOnOff = nBob->nb_PlaneOnOff;
                vsprite->VSBob   = bob;
                bob->BobVSprite  = vsprite;
                bob->ImageShadow = vsprite->CollMask;
                bob->Flags       = 0;
                bob->Before      = NULL;
                bob->After       = NULL;
                bob->BobComp     = NULL;

                if (nBob->nb_DBuf)
                    {
                    if (NULL != (bob->DBuffer = (struct DBufPacket *)
                            AllocMem((LONG)sizeof(struct DBufPacket), MEMF_CLEAR)))
                        {
                        if (NULL != (bob->DBuffer->BufBuffer = (WORD *)AllocMem(rassize, MEMF_CHIP)))
                            return(bob);
                        FreeMem(bob->DBuffer, (LONG)sizeof(struct DBufPacket));
                        }
                    }
                else
                    {
                    bob->DBuffer = NULL;
                    return(bob);
                    }
                freeVSprite(vsprite);
                }
            FreeMem(bob->SaveBuffer, rassize);
            }
        FreeMem(bob, (LONG)sizeof(*bob));
        }
return(NULL);
}


/*
** Create a Animation Component from the information given in nAnimComp and nBob.  Use
** freeComp() to free this GEL.  makeComp() calls makeBob(), and links the Bob into an AnimComp.
*/
struct AnimComp *makeComp(NEWBOB *nBob, NEWANIMCOMP *nAnimComp)
{
struct Bob      *compBob;
struct AnimComp *aComp;

if ((aComp = AllocMem((LONG)sizeof(struct AnimComp),MEMF_CLEAR)) != NULL)
        {
        if ((compBob = makeBob(nBob)) != NULL)
            {
            compBob->After   = compBob->Before  = NULL;
            compBob->BobComp = aComp;   /* Link 'em up. */
            aComp->AnimBob      = compBob;
            aComp->TimeSet      = nAnimComp->nac_Time; /* Num ticks active. */
            aComp->YTrans       = nAnimComp->nac_Yt; /* Offset rel to HeadOb */
            aComp->XTrans       = nAnimComp->nac_Xt;
            aComp->AnimCRoutine = nAnimComp->nac_Routine;
            aComp->Flags        = nAnimComp->nac_CFlags;
            aComp->Timer        = 0;
            aComp->NextSeq      = aComp->PrevSeq  = NULL;
            aComp->NextComp     = aComp->PrevComp = NULL;
            aComp->HeadOb       = NULL;
            return(aComp);
            }
        FreeMem(aComp, (LONG)sizeof(struct AnimComp));
        }
return(NULL);
}



/* Create an Animation Sequence from the information given in nAnimSeq and nBob.  Use
** freeSeq() to free this GEL.  This routine creates a linked list of animation components
** which make up the animation sequence.  It links them all up, making a circular list of
** the PrevSeq and NextSeq pointers. That is to say, the first component of the sequences'
** PrevSeq points to the last component; the last component of * the sequences' NextSeq
** points back to the first component.  If dbuf is on, the underlying Bobs will be set up
** for double buffering.  If singleImage is non-zero, the pImages pointer is assumed to
** point to an array of only one image, instead of an array of 'count' images, and all
** Bobs will use the same image.
*/
struct AnimComp *makeSeq(NEWBOB *nBob, NEWANIMSEQ *nAnimSeq)
{
int seq;
struct AnimComp *firstCompInSeq = NULL;
struct AnimComp *seqComp = NULL;
struct AnimComp *lastCompMade = NULL;
LONG image_size;
NEWANIMCOMP nAnimComp;

/* get the initial image.  this is the only image that is used
** if nAnimSeq->nas_SingleImage is non-zero.
*/
nBob->nb_Image = nAnimSeq->nas_Images;
image_size = nBob->nb_LineHeight * nBob->nb_ImageDepth * nBob->nb_WordWidth;

/* for each comp in the sequence */
for (seq = 0; seq < nAnimSeq->nas_Count; seq++)
        {
        nAnimComp.nac_Xt        = *(nAnimSeq->nas_Xt + seq);
        nAnimComp.nac_Yt        = *(nAnimSeq->nas_Yt + seq);
        nAnimComp.nac_Time      = *(nAnimSeq->nas_Times + seq);
        nAnimComp.nac_Routine   = nAnimSeq->nas_Routines[seq];
        nAnimComp.nac_CFlags    = nAnimSeq->nas_CFlags;
        if ((seqComp = makeComp(nBob, &nAnimComp)) == NULL)
            {
            if (firstCompInSeq != NULL)
                freeSeq(firstCompInSeq, (LONG)nBob->nb_RasDepth);
            return(NULL);
            }
        seqComp->HeadOb = nAnimSeq->nas_HeadOb;
        /* Make a note of where the first component is. */
        if (firstCompInSeq == NULL) firstCompInSeq = seqComp;
        /* link the component into the list */
        if (lastCompMade != NULL) lastCompMade->NextSeq = seqComp;
        seqComp->NextSeq = NULL;
        seqComp->PrevSeq = lastCompMade;
        lastCompMade = seqComp;
        /* If nAnimSeq->nas_SingleImage is zero, the image array has nAnimSeq->nas_Count images. */
        if (!nAnimSeq->nas_SingleImage)
            nBob->nb_Image += image_size;
        }
/* On The last component in the sequence, set Next/Prev to make */
/* the linked list a loop of components.                        */
lastCompMade->NextSeq = firstCompInSeq;
firstCompInSeq->PrevSeq = lastCompMade;

return(firstCompInSeq);
}


/* Free the data created by makeVSprite().  Assumes images deallocated elsewhere. */
VOID freeVSprite(struct VSprite *vsprite)
{
LONG    line_size;
LONG    plane_size;

line_size = (LONG)sizeof(WORD) * vsprite->Width;
plane_size = line_size * vsprite->Height;
FreeMem(vsprite->BorderLine, line_size);
FreeMem(vsprite->CollMask, plane_size);
FreeMem(vsprite, (LONG)sizeof(*vsprite));
}




/* Free the data created by makeBob().  It's important that rasdepth match the depth you */
/* passed to makeBob() when this GEL was made. Assumes images deallocated elsewhere.     */
VOID freeBob(struct Bob *bob, LONG rasdepth)
{
LONG    rassize =  sizeof(UWORD) * bob->BobVSprite->Width * bob->BobVSprite->Height * rasdepth;

if (bob->DBuffer != NULL)
        {
        FreeMem(bob->DBuffer->BufBuffer, rassize);
        FreeMem(bob->DBuffer, (LONG)sizeof(struct DBufPacket));
        }
FreeMem(bob->SaveBuffer, rassize);
freeVSprite(bob->BobVSprite);
FreeMem(bob, (LONG)sizeof(*bob));
}


/* Free the data created by makeComp().  It's important that rasdepth match the depth you */
/* passed to makeComp() when this GEL was made. Assumes images deallocated elsewhere.    */
VOID freeComp(struct AnimComp *myComp, LONG rasdepth)
{
freeBob(myComp->AnimBob, rasdepth);
FreeMem(myComp, (LONG)sizeof(struct AnimComp));
}


/* Free the data created by makeSeq().  Complimentary to makeSeq(), this routine goes through
** the NextSeq pointers and frees the Components.  This routine only goes forward through the
** list, and so it must be passed the first component in the sequence, or the sequence must
** be circular (which is guaranteed if you use makeSeq()).  It's important that rasdepth match
** the depth you passed to makeSeq() when this GEL was made.   Assumes images deallocated elsewhere!
*/
VOID freeSeq(struct AnimComp *headComp, LONG rasdepth)
{
struct AnimComp *curComp;
struct AnimComp *nextComp;

/* Break the NextSeq loop, so we get a NULL at the end of the list. */
headComp->PrevSeq->NextSeq = NULL;

curComp = headComp;         /* get the start of the list */
while (curComp != NULL)
        {
        nextComp = curComp->NextSeq;
        freeComp(curComp, rasdepth);
        curComp = nextComp;
        }
}


/* Free an animation object (list of sequences).  freeOb() goes through the NextComp
** pointers, starting at the AnimObs' HeadComp, and frees every sequence.  It only
** goes forward. It then frees the Object itself.  Assumes images deallocated elsewhere!
*/
VOID freeOb(struct AnimOb *headOb, LONG rasdepth)
{
struct AnimComp *curSeq;
struct AnimComp *nextSeq;

curSeq = headOb->HeadComp;          /* get the start of the list */
while (curSeq != NULL)
        {
        nextSeq = curSeq->NextComp;
        freeSeq(curSeq, rasdepth);
        curSeq = nextSeq;
        }
FreeMem(headOb, sizeof(struct AnimOb));
}

/* -- End of helper code copied from https://wiki.amigaos.net/wiki/Graphics_Sprites,_Bobs_and_Animation#Initializing_the_GEL_System */

static bool FeatureAvailable(void (*f)(BObject *, unsigned))
{
	return TRUE;
	/*return f == Screen_ || f == Window_ || f == Colour_ || f == Area_ || f == Beep_ || f == PReset_ || f == PSet_;*/
}

static bool FunctionAvailable(void (*f)(Scalar *, const BObject *, unsigned))
{
	return TRUE;
	/*return f == InKey_;*/
}

static PfScreenHandle GetScreenNative(const QString *title, int width, int height, int depth, unsigned long mode)
{
	struct Screen *theScreen;
	char *csTitle;
	struct ExtNewScreen newScreen;
	struct TagItem tags[6];
	UWORD dummyPen = ~0; /* Dummy pen spec to get new look. */

	csTitle = QsDupAsNTS(title);

	/* Get default video mode and/or dimensions from the default public screen. */

	if(mode == INVALID_ID || width == -1 || height == -1 || depth == -1) {
		struct Screen *wbScreen = LockPubScreen(NULL);
		if(wbScreen != NULL) {
			if(mode == INVALID_ID)
				mode = GetVPModeID(&wbScreen->ViewPort);
			if(width == -1)
				width = wbScreen->Width;
			if(height == -1)
				height = wbScreen->Height;
			if(depth == -1)
				depth = wbScreen->RastPort.BitMap->Depth;
			UnlockPubScreen(NULL, wbScreen);
		}
	}
	
	if(mode == INVALID_ID) {
		assert(GfxBase != NULL);
		mode = HIRES_KEY | ((GfxBase->DisplayFlags & PAL) ? PAL_MONITOR_ID : NTSC_MONITOR_ID);
	}
	
	if(width == -1)
		width = 640;
	
	if(height == -1)
		height = (mode & PAL_MONITOR_ID) ? 256 : 200;
	
	if(depth == -1)
		depth = 2;

	/* Adjust sizes if very small. */

	if(width < 320)
		width = 320;
	if(height < 200)
		height = 200;

	/* Set up tags for release 2+ extensions to screen options. */

	tags[0].ti_Tag = SA_DisplayID; /* Screen mode. */
	tags[0].ti_Data = mode;
	tags[1].ti_Tag = SA_Pens; /* Default new-look pens. */
	tags[1].ti_Data = (ULONG)&dummyPen;
	tags[2].ti_Tag = SA_FullPalette; /* Use full prefs palette. */
	tags[2].ti_Data = TRUE;
	tags[3].ti_Tag = SA_SysFont; /* Use prefs screen font. */
	tags[3].ti_Data = 1;
	tags[4].ti_Tag = SA_Quiet; /* Draw title bar etc.? */
	tags[4].ti_Data = *csTitle == NUL;
	tags[5].ti_Tag = TAG_DONE;	

	/* Set up screen structure. */

	newScreen.LeftEdge = newScreen.TopEdge = 0;
	newScreen.Width = width; 
	newScreen.Height = height;
	newScreen.Depth = depth;
	newScreen.DetailPen = 0;
	newScreen.BlockPen = 1;
	newScreen.ViewModes = HIRES; /* TODO not quite sure about this */
	newScreen.Type = CUSTOMSCREEN | NS_EXTENDED;
	newScreen.Font = NULL;
	newScreen.DefaultTitle = (UBYTE *)csTitle;
	newScreen.Gadgets = NULL;
	newScreen.CustomBitMap = NULL;
	newScreen.Extension = &tags[0];

	/* Attempt to open screen. */

	theScreen = OpenScreen((struct NewScreen *)&newScreen);
	if(theScreen == NULL)
		Dispose(csTitle);
	
	return theScreen;
}

static void ReleaseScreenNative(PfScreenHandle screen)
{
	char *title = (char *)screen->DefaultTitle;
	CloseScreen(screen);
	if(title != NULL)
		Dispose(title);
}

static bool WindowUsesScreenNative(PfWindowHandle w, PfScreenHandle s)
{
	return w->WScreen == s;
}

/* Set a clipping region for the window so its borders are protected from stray
graphics output. Returns TRUE if successful, FALSE if not: in which case the
system ran out of memory.
	This is adapted from the Libraries RKRM (3), pp. 723 - 725. */
static bool ClipWindowToBorders(struct Window *w)
{
	struct Region *region, *old;
	struct Rectangle rect;

	assert(w != NULL);

	if((region = NewRegion()) == NULL)
		return FALSE;

	rect.MinX = w->BorderLeft;
	rect.MinY = w->BorderTop;
	rect.MaxX = w->Width - w->BorderRight - 1;
	rect.MaxY = w->Height - w->BorderBottom - 1;
	
	if(!OrRectRegion(region, &rect)) {
		DisposeRegion(region);
		return FALSE;
	}

	old = InstallClipRegion(w->WLayer, region);
	if(old != NULL)
		DisposeRegion(old);

	return TRUE;
}

static PfWindowHandle OpenWindowNative(
	int id,
	const QString *title,
	const BasicRectangle *extent,
	PfScreenHandle screen,
	const QString *screenTitle,
	unsigned long flags)
{
	const short defaultTopLeftX = 100, defaultTopLeftY = 60;
	ULONG intuiFlags;
	char *csTitle, *csScreenTitle;
	struct Window *theWindow;
	struct TagItem tags[5];
	struct ExtNewWindow newWindow;
	bool succeeded = FALSE;

	/* Convert titles to C-strs. */

	csTitle = QsDupAsNTS(title);
	csScreenTitle = QsIsNull(screenTitle) ? NULL : QsDupAsNTS(screenTitle);

	/* Set up IDCMP flags. These are the same for _every_ window, because
	they all share the same msg port. Some events might never be received,
	depending on whether or not windows have attributes specified in the 
	'flags' argument. */

	intuiFlags = IDCMP_REFRESHWINDOW | IDCMP_CLOSEWINDOW
		   | IDCMP_ACTIVEWINDOW | IDCMP_INACTIVEWINDOW
		   | IDCMP_MOUSEBUTTONS | IDCMP_MENUPICK | IDCMP_MENUHELP
		   | IDCMP_RAWKEY | IDCMP_VANILLAKEY | IDCMP_NEWSIZE;

	/* Set up tagitems. */

	tags[0].ti_Tag = WA_ScreenTitle;
	tags[0].ti_Data = (ULONG)csScreenTitle;
	tags[1].ti_Tag = WA_InnerWidth;
	tags[1].ti_Data = extent->bottomRight.x  == -1 
		? 200 : extent->bottomRight.x - (extent->topLeft.x == -1 ? defaultTopLeftX : extent->topLeft.x);
	tags[2].ti_Tag = WA_InnerHeight;
	tags[2].ti_Data = extent->bottomRight.y  == -1 
		? 100 : extent->bottomRight.y - (extent->topLeft.y == -1 ? defaultTopLeftY : extent->topLeft.y);
	tags[3].ti_Tag = WA_AutoAdjust;
	tags[3].ti_Data = TRUE;
	tags[4].ti_Tag = TAG_DONE;

	/* Set up NewWindow structure. */

	newWindow.LeftEdge = extent->topLeft.x == -1 ? defaultTopLeftX : extent->topLeft.x;
	newWindow.TopEdge = extent->topLeft.y == -1 ? defaultTopLeftY : extent->topLeft.y;
	newWindow.Width = newWindow.Height = 0; /* Use WA_InnerWidth and WA_InnerHeight instead. */
	newWindow.DetailPen = newWindow.BlockPen = -1; /* Use screen's pens. */
	newWindow.IDCMPFlags = 0L; /* Set once open. */
	newWindow.Flags = WFLG_ACTIVATE | WFLG_SMART_REFRESH | WFLG_CLOSEGADGET
			| WFLG_DEPTHGADGET | WFLG_DRAGBAR | WFLG_SIZEBRIGHT
			| WFLG_SIZEGADGET | WFLG_NW_EXTENDED;
	if(flags & 1)
		newWindow.Flags &= ~WFLG_ACTIVATE;
	if(flags & 2)
		newWindow.Flags &= ~(WFLG_SIZEGADGET | WFLG_SIZEBRIGHT);
	if(flags & 4)
		newWindow.Flags &= ~WFLG_DRAGBAR;
	if(flags & 8)
		newWindow.Flags &= ~WFLG_DEPTHGADGET;
	if(flags & 16)	
		newWindow.Flags &= ~WFLG_CLOSEGADGET;
	if(flags & 32)
		newWindow.Flags |= WFLG_BORDERLESS;
	if(flags & 64)
		newWindow.Flags |= WFLG_BACKDROP;
	if(flags & 128)
		newWindow.Flags = WFLG_SIMPLE_REFRESH 
				| (newWindow.Flags & ~WFLG_SMART_REFRESH);
	if(flags & 256)
		newWindow.Flags = WFLG_NOCAREREFRESH | (newWindow.Flags 
				& ~(WFLG_SMART_REFRESH | WFLG_SIMPLE_REFRESH));

	/* Ask for new-look menus under V39+. See Menu_ for a discussion. */

#ifndef WFLG_NEWLOOKMENUS
#define WFLG_NEWLOOKMENUS 0x00200000
#endif

	if((*(struct Library **)4L)->lib_Version >= 39)
		newWindow.Flags |= WFLG_NEWLOOKMENUS;

	newWindow.FirstGadget = NULL; /* Gadgets are not (yet) supported. */
	newWindow.CheckMark = NULL;
	newWindow.Title = (UBYTE *)csTitle;
	newWindow.Screen = screen;
	newWindow.MinWidth = newWindow.MinHeight = 0; /* Set later. */
	newWindow.MaxWidth = newWindow.MaxHeight = ~0;
	newWindow.Type = screen == NULL ? WBENCHSCREEN : CUSTOMSCREEN;
	newWindow.Extension = &tags[0];

	/* Open the window. */

	theWindow = OpenWindow((struct NewWindow *)&newWindow);
	
	/* Set clipping region. */
	
	succeeded = theWindow != NULL && ClipWindowToBorders(theWindow);

	if(succeeded) {
		theWindow->MenuStrip = NULL; /* No menus to start with. */

		/* Now we have border sizes, so can set minimum size. */

		WindowLimits(theWindow,
		  theWindow->BorderLeft + theWindow->BorderRight + 1,
		  theWindow->BorderTop + theWindow->BorderBottom + 1,
		  ~0, ~0);

		/*if(!(flags & 1))
			focusWindow = theWindow;*/
			/* Whither a FOCUSCHANGE event???  Generate one?
			Leave this up to the OS? */
		/*outputWindow = theWindow;*/

		/* Set pen to top left. */
		
		Move(theWindow->RPort, theWindow->BorderLeft, theWindow->BorderTop);
		
		/* Initialise area buffer. */
		
		m_AreaVectorBuffer[id] = NULL;
		m_AreaActive[id] = FALSE;
	  
		/* Stash BASIC's window id in UserData. */
		
		theWindow->UserData = (APTR)id;

		/* Set up message port. */

		if(m_Port == NULL)
			succeeded = (m_Port = CreatePort(NULL, 0L)) != NULL;
		
		if(succeeded) {
			theWindow->UserPort = m_Port;
			ModifyIDCMP(theWindow, intuiFlags);
		}
	}
	
	if(!succeeded) {
		if(theWindow != NULL)
			CloseWindow(theWindow);
		Dispose(csTitle);
		if(csScreenTitle != NULL)
			Dispose(csScreenTitle);
	}
	
	return succeeded ? theWindow : NULL;
}

static void DisposeMenuItemList(struct MenuItem *head)
{
	struct MenuItem *savedNext;

	for( ; head != NULL; head = savedNext) {
		struct IntuiText *text;

		savedNext = head->NextItem;
		DisposeMenuItemList(head->SubItem);	
			/* may be null, always so if this is subitem */
		text = (struct IntuiText *)head->ItemFill;
		if(text->NextText != NULL) {
			/* "»" at end of item */
			Dispose((char *)text->NextText->IText);
			Dispose(text->NextText);
		}
		Dispose(text->IText);
		Dispose(text);
		Dispose(head);
	}
}

static void DisposeMenu(struct Menu *menu)
{
	DisposeMenuItemList(menu->FirstItem);	/* could be null */
	Dispose(menu->MenuName);
	Dispose(menu);
}

static void DisposeAllMenus(struct Menu *menuStrip)
{
	struct Menu *savedNext;

	for( ; menuStrip != NULL; menuStrip = savedNext) {
		savedNext = menuStrip->NextMenu;
		DisposeMenu(menuStrip);
	}
}

static void RemoveWindowNative(PfWindowHandle win, bool lastWindow)
{
	char *title, *screenTitle;
	struct IntuiMessage *m;
	struct Node *succ;
	struct Menu *menuStrip;
	struct Region *clipRegion;
	int id = (int)win->UserData;

	/* Dispose of menus if present. */
		
	menuStrip = win->MenuStrip;
	if(menuStrip != NULL) {
		ClearMenuStrip(win);
		win->MenuStrip = NULL;	/* make sure it's gone */
		/* if(ChangeMenuStripTo(menuStrip, NULL) <= 1 */
		/*if(fullCleanup) */
		DisposeAllMenus(menuStrip);
	}

	/* Remove and reply to all outstanding Intuition messages. */

	Forbid();
	
	for(m = (struct IntuiMessage *)win->UserPort->mp_MsgList.lh_Head;
	  (succ = m->ExecMessage.mn_Node.ln_Succ) != NULL;
	  m = (struct IntuiMessage *)succ)
		if(m->IDCMPWindow == win) {
			Remove((struct Node *)m);
			ReplyMsg((struct Message *)m);
		}
		
	win->UserPort = NULL;
	ModifyIDCMP(win, 0L);
	
	Permit();

	/* Clear events concerning this window from the queues. */

	/*if(fullCleanup)
		for(i = 0; i < NUMWINEVENTQUEUETYPES; i++)
			RemoveWindowEvents(&windowEvents[i]);*/

	/* Ensure rendering & activation handles don't refer to this window. 
	   Note that this can lead to situations where closing a window causes
	   a 'no output window' error on a subsequent gfx operation - tough.
	   The programmer should use an explicit WINDOWOUTPUT stmt. */

	/*if(outputWindow == win)
		outputWindow = NULL;*/
	/*if(focusWindow == win) {
		focusWindow = NULL;*/

		/* Invalidate mouse coords and assume button up. */

		/*mousePosition.x = mousePosition.y = -1;
		buttonDown = FALSE;
	}*/

	/* Remove AreaInfo polygon vertex storage. */

	if(m_AreaVectorBuffer[id] != NULL) {
		Dispose(m_AreaVectorBuffer[id]);
		m_AreaVectorBuffer[id] = NULL;
		m_AreaActive[id] = FALSE;		
		Dispose(win->RPort->AreaInfo);	
		win->RPort->AreaInfo = NULL;
	}

	/* Remove the temporary raster - used for area and flood fill. */
	
	if(win->RPort->TmpRas != NULL && win->RPort->TmpRas->RasPtr != NULL)
		FreeRaster(win->RPort->TmpRas->RasPtr, win->WScreen->Width, win->WScreen->Height);
	if(win->RPort->TmpRas != NULL) {
		Dispose(win->RPort->TmpRas);
		win->RPort->TmpRas = NULL;
	}
		
	/* Remove the clipping region. */
	
	clipRegion = InstallClipRegion(win->WLayer, NULL);
	if(clipRegion != NULL)
		DisposeRegion(clipRegion);

	/* Grab titles before closing the window. */
	
	title = (char *)win->Title;
	screenTitle = (char *)win->ScreenTitle;
	
	/* Close it. */

	CloseWindow(win);

	/* Free title and ScreenTitle memory if they were defined. */

	/*if(fullCleanup) {*/
		if(title != NULL)
			Dispose(title);
		if(screenTitle != NULL)
			Dispose(screenTitle);
		/*}*/

	/* Release message port if this was the last open window. */

	if(lastWindow) {
		DeletePort(m_Port);
		m_Port = NULL;
	}
}

static long GetWindowInfoNative(PfWindowHandle win, unsigned key)
{
	/* TODO */
	if(key == 8)
		return (long)win->RPort;
	else
		return 0;
}

static void EraseWindowInteriorNative(PfWindowHandle win)
{
	SetRast(win->RPort, win->RPort->BgPen);
	Move(win->RPort, win->BorderLeft, win->BorderTop);
}

static void SetWindowPenNative(PfWindowHandle win, bool foreground, short pen)
{
	if(foreground)
		SetAPen(win->RPort, (UBYTE)pen);
	else
		SetBPen(win->RPort, (UBYTE)pen);
}

static int GetPixelPaletteColourNative(PfWindowHandle win, const BasicPoint *p)
{
	return ReadPixel(win->RPort, p->x + win->BorderLeft, p->y + win->BorderTop);
}

static UBYTE PenForSpec(struct RastPort *rp, short penOrDefault)
{
	if(penOrDefault == PEN_BG) return rp->BgPen;
	if(penOrDefault == PEN_FG) return rp->FgPen;
	return (UBYTE)penOrDefault;
}

static void SetPixelNative(PfWindowHandle win, const BasicPoint *p, short penOrDefault)
{
	struct RastPort *rp = win->RPort;
	UBYTE savedFgPen = rp->FgPen;

	SetAPen(rp, (int)PenForSpec(rp, penOrDefault));
	WritePixel(rp, p->x + win->BorderLeft, p->y + win->BorderTop);
	SetAPen(rp, (int)savedFgPen);
}

static void MoveRegionNative(PfWindowHandle win, const BasicRectangle *region, short dx, short dy)
{
}

static void DrawLineNative(PfWindowHandle win, const BasicPoint *p1, const BasicPoint *p2)
{
	struct RastPort *rp = win->RPort;
	Move(rp, win->BorderLeft + p1->x, win->BorderTop + p1->y);
	Draw(rp, win->BorderLeft + p2->x, win->BorderTop + p2->y);
}

static struct TmpRas *EnsureTmpRas(struct Window *win)
{
	if(win->RPort->TmpRas == NULL) {
		struct TmpRas *newTmpRas = (struct TmpRas *)TolerantNew(sizeof(struct TmpRas));
		APTR tmpRasMemory;
		UWORD width = win->WScreen->Width; /*win->MaxWidth == 0 ? win->WScreen->Width : win->MaxWidth;*/
		UWORD height = win->WScreen->Height; /*win->MaxHeight == 0 ? win->WScreen->Height : win->MaxHeight;*/
		
		if(newTmpRas == NULL)
			return NULL;

		tmpRasMemory = (APTR)AllocRaster(width, height);
		if(tmpRasMemory == NULL) {
			Dispose(newTmpRas);
			return NULL;
		}
		
		InitTmpRas(newTmpRas, tmpRasMemory, RASSIZE(width, height));
		win->RPort->TmpRas = newTmpRas;
	}
	return win->RPort->TmpRas;
}

static bool AddVertexToPolygonNative(PfWindowHandle win, const BasicPoint *p)
{
	int id = (int)win->UserData;
	bool succeeded = TRUE;

	/* Perform initial area fill set up for this window if required. */
	if(m_AreaVectorBuffer[id] == NULL) {
		struct AreaInfo *newAreaInfo;
		
		m_AreaActive[id] = FALSE;
		
		/* TODO check how many vertices AmigaBasic allowed. */
		m_AreaVectorBuffer[id] = (UWORD *)New(MAX_POLYGON_VERTICES * 2 * 5); 
		newAreaInfo = (struct AreaInfo *)New(sizeof(struct AreaInfo));
		InitArea(newAreaInfo, m_AreaVectorBuffer[id], MAX_POLYGON_VERTICES * 2);

		if(EnsureTmpRas(win) != NULL)
			win->RPort->AreaInfo = newAreaInfo;
		else {
			Dispose(newAreaInfo);
			Dispose(m_AreaVectorBuffer[id]);
			m_AreaVectorBuffer[id] = NULL;
			succeeded = FALSE;
		}
	}

	if(succeeded) {
		if(!m_AreaActive[id]) {
			succeeded = AreaMove(win->RPort, p->x + win->BorderLeft, p->y + win->BorderTop) != -1;
			m_AreaActive[id] = succeeded;
		}
		else
			succeeded = AreaDraw(win->RPort, p->x + win->BorderLeft, p->y + win->BorderTop) != -1;
	}
	
	return succeeded;
}

static bool FillPolygonNative(PfWindowHandle win, short mode)
{
	int id = (int)win->UserData;
	bool success = FALSE;
	if(m_AreaVectorBuffer[id] != NULL && m_AreaActive[id])
	{
		BYTE savedDrawMode = win->RPort->DrawMode;
		m_AreaActive[id] = FALSE;
		SetDrMd(win->RPort, mode == 1 ? INVERSVID : savedDrawMode);
		success = AreaEnd(win->RPort) != -1;
		SetDrMd(win->RPort, savedDrawMode);
	}
	return success;
}

static bool FloodFillNative(PfWindowHandle win, const BasicPoint *p, short penOrDefault)
{
	struct RastPort *rp = win->RPort;
	UBYTE savedBgPen = rp->BgPen;
	bool succeeded;
	
	SetBPen(rp, (int)PenForSpec(rp, penOrDefault));
	/* Fill all adjacent pixels if they are:
	    Mode 0: not the same color as AOLPen
	    Mode 1: the same color as the pixel at (x,y) */
	succeeded = EnsureTmpRas(win) != NULL
		&& Flood(rp, 1, p->x + win->BorderLeft, p->y + win->BorderTop);
	SetBPen(rp, (int)savedBgPen);
	return succeeded;
}

static void SetWindowLinePatternNative(PfWindowHandle win, short pattern)
{
	struct RastPort *rp = win->RPort;
	SetDrPt(rp, pattern);
	SetDrMd(rp, JAM2);
}

static void SetWindowAreaPatternNative(PfWindowHandle win, short *pattern, size_t nWords)
{
	struct RastPort *rp = win->RPort;
	int powerOf2 = 1;
	while(powerOf2 < nWords / 2) powerOf2 <<= 1;
	SetAfPt(rp, pattern, powerOf2);
	SetDrMd(rp, JAM2);
}

static void DrawEllipseNative(PfWindowHandle win, const BasicRectangle *bounds)
{
	SHORT radiusX = (bounds->bottomRight.x - bounds->topLeft.x) / 2;
	SHORT radiusY = (bounds->bottomRight.y - bounds->topLeft.y) / 2;
	SHORT cx = win->BorderLeft + bounds->topLeft.x + radiusX;
	SHORT cy = win->BorderTop + bounds->topLeft.y + radiusY;
	DrawEllipse(win->RPort, cx, cy, radiusX, radiusY);
}

static void SetPaletteColourNative(PfWindowHandle win, int id, float r, float g, float b)
{
	/* if(win->WScreen->Flags & WBENCHSCREEN)
		EFail(CHANGEWBCOLOURS);
	*/
	/* TODO AGA version */
	SetRGB4(&win->WScreen->ViewPort, id, (int)(r * 15.0), (int)(g * 15.0), (int)(b * 15.0));
}

static void SetPenPositionNative(PfWindowHandle win, const BasicPoint *p)
{
	Move(win->RPort, win->BorderLeft + p->x, win->BorderTop + p->y);
}

static void GetCurrentPenPositionNative(PfWindowHandle win, BasicPoint *p)
{
	p->x = win->RPort->cp_x - win->BorderLeft;
	p->y = win->RPort->cp_y - win->BorderTop;
}

static void AlertUserNative(PfWindowHandle win)
{
	DisplayBeep(win->WScreen);
}

/* Returns the 'average' width of a character in the rastport's current
   font. This is a correct value if the font is non-proportional; otherwise,
   it is an average of the widths of the characters 'M' and 'N'. */
static short AverageFontCharWidth(struct RastPort *rp)
{
	struct TextFont *f = rp->Font;
	int width;
	int mWidth = TextLength(rp, "M", 1);
		/* f->tf_CharSpace['M'] + f->tf_CharKern['M']; */

	if(f->tf_Flags & FPF_PROPORTIONAL) {
		int nWidth = TextLength(rp, "n", 1);
			/* f->tf_CharSpace['N'] + f->tf_CharKern['N']; */

		width = (mWidth + nWidth) / 2;
	}
	else  
		/* Monospaced font */
		width = mWidth;

	return width;
}

/* The line spacing for the rastport's font. Not terribly sensitive to 
   characters with descenders (e.g. 'y'). */
static short FontLineHeight(struct RastPort *rp)
{
	struct TextFont *f = rp->Font;
	return f->tf_YSize;	/* TODO what about spacing? */
}

/* Distance from top to bottom _not including_ descenders. */
static short FontCharHeight(struct RastPort *rp)
{
	struct TextFont *f = rp->Font;
	return f->tf_Baseline;
}

static void GetFontCharacterDimensionsNative(PfWindowHandle win, short *w, short *h)
{
	*w = AverageFontCharWidth(win->RPort);
	*h = FontLineHeight(win->RPort);
}

/* Character sizes from a font description. Adapted from the Libs RKRM[3] p. 199 */
static short QueryDiskFont(struct TextAttr *ta, short fallback, short (*query)(struct RastPort *))
{
	struct TextFont *font;
	struct RastPort textRP;
	short val = fallback;

	memset(&textRP, NUL, sizeof(textRP));
	if((font = OpenDiskFont(ta)) != NULL) {
		SetFont(&textRP, font);
		val = query(&textRP);
		CloseFont(font);
	}
	return val;
}

static short AverageDiskFontCharWidth(struct TextAttr *ta)
{
	return QueryDiskFont(ta, 8, &AverageFontCharWidth);
}

static short DiskFontLineHeight(struct TextAttr *ta)
{
	return QueryDiskFont(ta, 10, &FontLineHeight);
}

static void RenderTextNative(PfWindowHandle win, const QsChar *text, size_t nChars, short tabInterval)
{
	struct RastPort *rp = win->RPort;
	short /*tabSize,*/ charHeight, charWidth;
	size_t n, renderCount;
	
	charWidth = AverageFontCharWidth(rp);
	/*tabSize = AverageFontCharWidth(rp) * windowPrintTabSize;*/
	charHeight = FontCharHeight(rp);
	
	/* Offset Y position: the Text() call draws the text with its baseline
	at the pen location, but we pretend that WPRINT prints from the current
	pen position _down_. */

	Move(rp, (int)rp->cp_x, rp->cp_y + charHeight);
	
	/* Tabs and newlines are treated specially.
	Other characters are just passed to the graphics library. */
	
	for(n = 0; n < nChars; n += renderCount) {
		renderCount = 1;
		if(text[n] == '\n')	
			Move(rp, win->BorderLeft, rp->cp_y + FontLineHeight(rp) - charHeight);
		else if(text[n] == '\t') {
			BasicPoint p;
			GetCurrentPenPositionNative(win, &p);
			while((p.x / charWidth) % tabInterval != 0) {
				Text(rp, " ", 1); /* Do it this way so previous gfx are overwritten. */
				GetCurrentPenPositionNative(win, &p);
			}
		}
		else {
			while(n + renderCount < nChars && text[n + renderCount] != '\n' && text[n + renderCount] != '\t')
				++renderCount;
			Text(rp, &text[n], renderCount);
		}
	}
	
	if(nChars != 0 && text[nChars - 1] != '\n')
		/* Correct for the baseline offset, so further WPRINT statements carry on along the same row. */
		Move(rp, rp->cp_x, rp->cp_y - charHeight);
}

/* Builds the full name of the menu, in the form "Menu[|Item[|SubItem]]".
   The menu number is assumed to be non-MENUNULL.  */
static void GetMenuName(QString *name, struct Menu *menuStrip, int menuNum, int itemNum, int subItemNum)
{
	static const QString bar = {"|", 1};
	struct Menu *m = NULL;
	struct MenuItem *i = NULL, *s = NULL;
	QString menuName, itemName, subItemName;

	QsInitNull(name);

	/* Menu */

	if(menuNum == NOMENU)
		return;

	for(m = menuStrip; m != NULL && menuNum > 0; m = m->NextMenu)
		--menuNum;

	if(m == NULL)
		return;

	QsCopyNTS(&menuName, m->MenuName);
	*name = menuName;
	
	/* Item */

	if(itemNum == NOITEM)
		return;

	for(i = m->FirstItem; i != NULL && itemNum > 0; i = i->NextItem)
		--itemNum;

	if(i == NULL)
		return;

	QsCopyNTS(&itemName, ((struct IntuiText *)i->ItemFill)->IText);
	QsAppend(name, &bar);
	QsAppend(name, &itemName);

	/* Subitem */

	if(subItemNum == NOSUB) {
		QsDispose(&itemName);
		return;
	}

	for(s = i->SubItem; s != NULL && subItemNum > 0; s = s->NextItem)
		--subItemNum;

	if(s == NULL) {
		QsDispose(&itemName);		
		return;
	}

	QsCopyNTS(&subItemName, ((struct IntuiText *)s->ItemFill)->IText);
	QsAppend(name, &bar);
	QsAppend(name, &subItemName);
	
	QsDispose(&itemName);
	QsDispose(&subItemName);
}

/* Finds menu with given name in menu strip. Pointer to previous Menu is set
   up if this is not the first menu in the strip. If the menu is not found,
   *prevMenu will point to the last menu. */
static struct Menu *FindMenu(struct Menu *menuStrip, const char *name, struct Menu **prevMenu)
{
	*prevMenu = NULL;

	for( ; 
	  menuStrip != NULL && stricmp(menuStrip->MenuName, name) != 0;
	  menuStrip = menuStrip->NextMenu)
		*prevMenu = menuStrip;

	return menuStrip;
}

/* Finds item or subitem with given name in list. Pointer to previous MenuItem
   is set up if this is not the first item in the list. If item is not found,
   *prevItem will point to last item in list. */
static struct MenuItem *FindMenuItem(struct MenuItem *item, const char *name, struct MenuItem **prevItem)
{
	*prevItem = NULL;

	for( ;
	  item != NULL && stricmp(((struct IntuiText *)item->ItemFill)->IText, name) != 0;
	  item = item->NextItem)
		*prevItem = item;

	return item;
}

static void AlterMenuNative(PfWindowHandle win, char *mName, char *iName, char *sName, char hotKey, unsigned long flags)
{
	struct Screen *screen;
	struct TextAttr *font;
	struct Menu *menuStrip = NULL, *menu = NULL, *prevMenu = NULL;
	struct MenuItem *item = NULL, *prevItem = NULL;
	struct MenuItem *subItem = NULL, *prevSubItem = NULL;
	struct Library *execLib;
	bool isLowResScreen;
	int charWidth, charHeight; /* Used for layout. */
	UBYTE frontPen, backPen; /* Menu text colours. */

	/*** Get screen characteristics ***/

	/* Determine whether screen is low-res: */

	screen = win->WScreen;
	isLowResScreen = !(screen->Flags & SCREENHIRES);
		/* TODO intuition/screens.h says that SCREENHIRES is _private_. 
			Change this to use an official way of determining whether the screen is low-res. */

	/* Get the screen font's average character width and height: */

	font = screen->Font;
	charWidth = AverageDiskFontCharWidth(font);
	charHeight = DiskFontLineHeight(font);

	/* Work out which pens to use for the menu text.

	   TODO this changed between V37 and V39. I'm not sure whether V38 follows
	   the V37 or the V39 convention; I assume it is like V37.
	   There are a couple of assumptions made here. One is that the exec.library
	   version number corresponds to the intuition.library version. Should use
	   LockIBase() and examine IntuitionBase directly. 
	   The other is using pen 1 for the fg, pen 2 for bg under V39 and up.
	   This is a 'works on my machine' assumption. Should use GetScreenDrawInfo() (2.0+)
	*/

	frontPen = 0; /* Old-style pens... */
	backPen = 1;
	execLib = *(struct Library **)4L; /* Ugly, but no global vars! */
	if(execLib->lib_Version >= 39) { /* New-style pens... */
		frontPen = 1;
		backPen = 2;
	}
	
	/*** Find existing menu [[sub]item] ***/
	
	menuStrip = win->MenuStrip;
	if(menuStrip != NULL) {
		/* ChangeMenuStripTo(menuStrip, NULL); */
		ClearMenuStrip(win);
		win->MenuStrip = NULL;	/* make sure it's gone */
	}

	/* If already a matching menu/item/subitem, release the memory used
	   temporarily to store the name. */

	if(mName != NULL) {
		menu = FindMenu(menuStrip, mName, &prevMenu);
		if(menu != NULL)
			Dispose(mName);
	}

	if(iName != NULL && menu != NULL) {
		item = FindMenuItem(menu->FirstItem, iName, &prevItem);
		if(item != NULL)
			Dispose(iName);
	}

	if(sName != NULL && item != NULL) {
		subItem = FindMenuItem(item->SubItem, sName, &prevSubItem);
		if(subItem != NULL)
			Dispose(sName);
	}

	/*** Check for contradictory or illegal changes ***/

	/* Check for attempted creation of an empty menu, or removal of the 
	   last item in a menu. Empty menus are illegal. See doc for
	   intuition.library/SetMenuStrip(): */

	if((menu == NULL && iName == NULL && !(flags & 64))
	|| (menu != NULL && item != NULL && item->NextItem == NULL 
		&& menu->FirstItem == item && (flags & 64))) {
		CauseError(ER_BAD_MENU_NAME);
		return;
	}

	/* Check for attempted 'removal' of nonexistent item: */

	if((flags & 64)
	&&  ((mName != NULL && menu == NULL)
	  || (iName != NULL && item == NULL) 
	  || (sName != NULL && subItem == NULL))) {
		CauseError(ER_NO_MENU);
		return;
	}
	  
	/*** Create menu if necessary ***/

	if(menu == NULL && mName != NULL) {
		menu = (struct Menu *)New(sizeof(struct Menu));

		menu->NextMenu = NULL;		/* Add to end of strip. */

		/* Calculate bounding box: */

		if(prevMenu != NULL)
			menu->LeftEdge = (isLowResScreen ? 3 : 6) + prevMenu->LeftEdge + prevMenu->Width; 
		else    
			/* First (leftmost) menu for the window. */
			menu->LeftEdge = 2;

		menu->Width = (strlen(mName) + 1) * charWidth;

		/* TopEdge and Height are ignored - see p. 188 Libs RKRM 3ed */

		menu->TopEdge = 0;
		menu->Height = charHeight + 2;
		menu->Flags = MENUENABLED;
		menu->MenuName = mName;
		menu->FirstItem = NULL;
		/* Init JazzX, JazzY, BeatX, BeatY ??? */

		/* Link into or create menu strip. */

		if(prevMenu != NULL)
			prevMenu->NextMenu = menu;
		else
			menuStrip = menu;
	}

	/*** Set up or change menu's flags ***/

	/* On/OffMenu(struct Window *, UWORD menunum) */

	if(menu != NULL && iName == NULL) {
		/* Change menu state only if no item specified. */
		if(flags & 1)	/* Disable */
			menu->Flags &= ~MENUENABLED;
		if(flags & 2)   /* Enable */
			menu->Flags |= MENUENABLED;
	}

	/*** Create item if necessary ***/ 

	if(item == NULL && iName != NULL) {
		struct IntuiText *itemText;

		assert(menu != NULL);

		/* Build text of item: */

		itemText = (struct IntuiText *)New(sizeof(struct IntuiText));
		
		itemText->FrontPen = frontPen;
		itemText->BackPen = backPen;
		itemText->DrawMode = JAM2;
		itemText->LeftEdge = 1;
		itemText->TopEdge = 1;
		itemText->ITextFont = font;
		itemText->IText = (UBYTE *)iName;
		itemText->NextText = NULL;

		/* Adjust text position for checkmark, if present: */

		if((flags & 4) && sName == NULL)
			itemText->LeftEdge += isLowResScreen ? LOWCHECKWIDTH : CHECKWIDTH;

		/* Build item: */

		item = (struct MenuItem *)New(sizeof(struct MenuItem));

		item->NextItem = NULL;
		item->LeftEdge = 0;      /* Relative to menu's leftedge. */
		if(prevItem == NULL)
			item->TopEdge = 0;
		else
			item->TopEdge = prevItem->TopEdge + prevItem->Height;
		item->Width = charWidth * (MAX_MENU_NAME_LENGTH + 1) + (isLowResScreen ? LOWCOMMWIDTH : COMMWIDTH);
		item->Height = charHeight + 1;
		if(item->Height < 9)
			item->Height = 9;
		item->Flags = ITEMTEXT | ITEMENABLED | HIGHCOMP;
		if(sName == NULL) {
			if(hotKey != '\0')
				item->Flags |= COMMSEQ;
			if(flags & 4)
				item->Flags |= CHECKIT | CHECKED;
		}
		item->MutualExclude = 0;
		item->ItemFill = (APTR)itemText;
		item->SelectFill = NULL;
		item->Command = (sName == NULL) ? hotKey : '\0';
		item->SubItem = NULL;
		item->NextSelect = MENUNULL;
	
		/* Link into or create menu's list of items: */

		if(prevItem == NULL)
			menu->FirstItem = item;
		else
			prevItem->NextItem = item;
	}

	/*** Set up or change item's flags ***/

	if(item != NULL && sName == NULL) {
		if(flags & 1)
			item->Flags &= ~ITEMENABLED;
		if(flags & 2)
			item->Flags |= ITEMENABLED;
		if(flags & 8)
			item->Flags |= CHECKED;
		if(flags & 16)
			item->Flags &= ~CHECKED;
	}

	/*** Create subitem if necessary ***/

	if(subItem == NULL && sName != NULL) {
		struct IntuiText *siText;

		assert(item != NULL);

		/* Build text of subitem: */

		siText = (struct IntuiText *)New(sizeof(struct IntuiText));
		
		siText->FrontPen = frontPen;
		siText->BackPen = backPen;
		siText->DrawMode = JAM2;
		siText->LeftEdge = 1;
		siText->TopEdge = 1;
		siText->ITextFont = font;
		siText->IText = (UBYTE *)sName;
		siText->NextText = NULL;

		/* Adjust text position for checkmark, if present: */

		if(flags & 4)
			siText->LeftEdge += isLowResScreen ? LOWCHECKWIDTH : CHECKWIDTH;

		/* Build subitem: */

		subItem = (struct MenuItem *)New(sizeof(struct MenuItem));

		subItem->NextItem = NULL;
		subItem->LeftEdge = item->LeftEdge + (item->Width * 3) / 4;
		if(prevSubItem == NULL)
			subItem->TopEdge = -1;
		else
			subItem->TopEdge = prevSubItem->TopEdge + prevSubItem->Height;
		subItem->Width = item->Width;
		subItem->Height = item->Height;
		subItem->Flags = ITEMTEXT | ITEMENABLED | HIGHCOMP;
		if(hotKey != '\0')
			subItem->Flags |= COMMSEQ;
		if(flags & 4)
			subItem->Flags |= CHECKIT | CHECKED;
		subItem->MutualExclude = 0;
		subItem->ItemFill = (APTR)siText;
		subItem->SelectFill = NULL;
		subItem->Command = hotKey;
		subItem->SubItem = NULL;
		subItem->NextSelect = MENUNULL;
	
		/* Link into or create the item's list of subitems: */

		if(prevSubItem == NULL) {
			struct IntuiText *arrow, *itemText;

			item->SubItem = subItem;

			/* Add "»" character to end of item name. */

			itemText = (struct IntuiText *)item->ItemFill;

			arrow = (struct IntuiText *)New(sizeof(struct IntuiText));
		
			arrow->FrontPen = frontPen;
			arrow->BackPen = backPen;
			arrow->DrawMode = JAM2;
			arrow->LeftEdge = item->LeftEdge + item->Width - charWidth - 2;
			arrow->TopEdge = 1;
			arrow->ITextFont = font;
			arrow->IText = (UBYTE *)New(2L);
			arrow->IText[0] = 0xBB; /* » */
			arrow->IText[1] = NUL;
			arrow->NextText = NULL;

			itemText->NextText = arrow;
		}
		else
			prevSubItem->NextItem = subItem;
	}

	/*** Set up or change subitem's flags. ***/

	if(subItem != NULL) {
		if(flags & 1)
			subItem->Flags &= ~ITEMENABLED;
		if(flags & 2)
			subItem->Flags |= ITEMENABLED;
		if(flags & 8)
			subItem->Flags |= CHECKED;
		if(flags & 16)
			subItem->Flags &= ~CHECKED;
	}

	/*** Remove menu strip/menu/item/subitem ***/

	/* Remove subitem/item/menu/whole menu strip from menu bar if desired. 
	Can only remove from one level. 
	Restrictions: can only remove last subitem in item's list, last item 
	   in menu's list (in which case all subitems go), last menu in menu
	   bar, or whole menu strip.
	Otherwise, would need to adjust positions of items! */

	if(flags & 64) {
		if(menu == NULL) {
			/* Whole menu strip */

			DisposeAllMenus(menuStrip);
			menuStrip = NULL;		/* don't reattach */
		}
		else if(item == NULL) {
			/* Whole menu */

			if(menu->NextMenu != NULL) {
				CauseError(ER_BAD_MENU_NAME);
				return;
			}

			DisposeMenu(menu);
			if(prevMenu != NULL)
				prevMenu->NextMenu = NULL;
			else
				menuStrip = NULL;	/* don't reattach */
		}
		else if(subItem == NULL) {
			/* Menu item */

			if(item->NextItem != NULL) {
				CauseError(ER_BAD_MENU_NAME);
				return;
			}

			DisposeMenuItemList(item);
			if(prevItem != NULL)
				prevItem->NextItem = NULL;
			else
				menu->FirstItem = NULL;
		}
		else {
			/* Subitem */

			if(subItem->NextItem != NULL) {
				CauseError(ER_BAD_MENU_NAME);
				return;
			}

			DisposeMenuItemList(subItem);
			if(prevSubItem != NULL)
				prevSubItem->NextItem = NULL;
			else
				item->SubItem = NULL;
			}
		}
				
	/*** (Re)attach menu strip to window ***/

	if(menuStrip != NULL)
		/* ChangeMenuStripTo(window, menuStrip); */
		SetMenuStrip(win, menuStrip);
}

static int GetMenuItemStateNative(PfWindowHandle win, char *menuName, char *itemName, char *subitemName)
{
	struct Menu *menu = NULL, *dummyPrevMenu;
	struct MenuItem *item = NULL, *dummyPrevItem;
	int state = 0;
	
	if(menuName != NULL) {
		menu = FindMenu(win->MenuStrip, menuName, &dummyPrevMenu);
		Dispose(menuName);
	}
	
	if(menu == NULL)
		CauseError(ER_NO_MENU);
	else {			
		if(itemName != NULL) {
			item = FindMenuItem(menu->FirstItem, itemName, &dummyPrevItem);
			if(item == NULL)
				CauseError(ER_NO_MENU);
		}
		else
			/* Whole menu. */
			state = (menu->Flags & MENUENABLED) ? 1 : 2;
		
		if(item != NULL && subitemName != NULL) {
			item = FindMenuItem(item->SubItem, subitemName, &dummyPrevItem);
			if(item == NULL)
				CauseError(ER_NO_MENU);
		}
		
		if(item != NULL) {
			state = (item->Flags & ITEMENABLED) ? 1 : 2;
			if((item->Flags & CHECKIT) && (item->Flags & CHECKED))
				state = -state;
		}
	}
	
	if(itemName != NULL)
		Dispose(itemName);
	if(subitemName != NULL)
		Dispose(subitemName);
	
	return state;
}

static PfAnimatedObjectHandle CreateAnimatedObjectNative(PfWindowHandle win/*, unsigned char *data */)
{
	PfAnimatedObjectHandle handle;
	struct newVSprite nspr;
	static WORD data[] = { -1, -1, -1, -1, -1 };
	static WORD colours[] = { 1 };
	
	nspr.nvs_Image = data;
    nspr.nvs_ColorSet = colours;
    nspr.nvs_WordWidth = 1;
    nspr.nvs_LineHeight = 5;
    nspr.nvs_ImageDepth = 1;
    nspr.nvs_X = 50;
    nspr.nvs_Y = 50;
    nspr.nvs_Flags = 0;
    nspr.nvs_HitMask = -1;
    nspr.nvs_MeMask = -1;
	
	if(m_Gels == NULL) /* TODO is this really global, or per-rastport? */
		m_Gels = setupGelSys(win->RPort, 0);
	
	handle = makeVSprite(&nspr);
	
	AddVSprite(handle, win->RPort);
	
	return handle;
}

static void FreeAnimatedObjectNative(PfWindowHandle win, PfAnimatedObjectHandle mob, bool last)
{
	RemVSprite(mob);
	freeVSprite(mob);
	if(last) {
		cleanupGelSys(m_Gels, win->RPort);
		m_Gels = NULL;
	}
}

PfEventNotificationHandle GetUIEventNotificationHandle(void)
{
	return m_Port != NULL ? m_Port->mp_SigBit : 0;
}

static bool GetNextWindowEventNative(PfWindowEvent *nextEvent)
{
	struct IntuiMessage *msg;
	
	if(m_Port != NULL && (msg = (struct IntuiMessage *)GetMsg(m_Port)) != NULL) {
		*nextEvent = *msg;
		ReplyMsg((struct Message *)msg);
		
		if(WindowExists(nextEvent->IDCMPWindow)) {
			if(nextEvent->Class == IDCMP_NEWSIZE)
				/* Performance improvement is possible here by checking whether the window
					actually changed size, but haven't bothered for now. */
				ClipWindowToBorders(nextEvent->IDCMPWindow);
			else if(nextEvent->Class == IDCMP_REFRESHWINDOW) {
				BeginRefresh(nextEvent->IDCMPWindow);
				EndRefresh(nextEvent->IDCMPWindow, TRUE);
			}
		}
		
		return TRUE;
	}
	else
		return FALSE;
}

static bool TranslateKeyPressNative(const PfWindowEvent *event, PfKeypress *press)
{
	if(event->Class == IDCMP_VANILLAKEY) {
		press->code = event->Code;
		press->qualifier = 0;
		press->vanilla = TRUE;
		return TRUE;
	}
	else if(event->Class == IDCMP_RAWKEY) {
		press->code = event->Code;
		press->qualifier = event->Qualifier;
		press->vanilla = FALSE;
		return TRUE;
	}
	else
		return FALSE;
}

static void GetKeyPressDataNative(QString *s, const PfKeypress *press)
{
	if(press->code == 0 && press->qualifier == 0)
		QsInitNull(s);
	else if(press->vanilla) 
		QsCopyChar(s, (char)press->code);
	else {
		USHORT keyData[2];
		keyData[0] = press->code;
		keyData[1] = press->qualifier;
		QsCopyData(s, (char *)&keyData[0], sizeof(keyData));
	}
}

static bool TranslatePointerActionNative(const PfWindowEvent *event, PfPointer *pointer)
{
	bool interestingEvent = FALSE;
	
	if(event->Class == IDCMP_MOUSEBUTTONS) {
		bool pressed = event->Code == SELECTDOWN;
		bool released = event->Code == SELECTUP;
		
		interestingEvent = TRUE;
		
		pointer->buttonDown = pressed;

		if(pressed) {
			pointer->starting.x = event->MouseX - event->IDCMPWindow->BorderLeft;
			pointer->starting.x = event->MouseY - event->IDCMPWindow->BorderTop;
		}
		else if(released) {
			pointer->ending.x = event->MouseX - event->IDCMPWindow->BorderLeft;
			pointer->ending.y = event->MouseY - event->IDCMPWindow->BorderTop;
		}
	}
	
	/* Get current coords from any event, but only if for output window.
		TODO this might not be quite what we want. Should have concept of focus window. */
	
	if(OutputWindow() == event->IDCMPWindow) {
		WORD prevX = pointer->current.x, prevY = pointer->current.y;
		pointer->current.x = event->MouseX - event->IDCMPWindow->BorderLeft;
		pointer->current.y = event->MouseY - event->IDCMPWindow->BorderTop;
		interestingEvent = prevX != pointer->current.x || prevY != pointer->current.y;
	}
	
	return interestingEvent;
}

static int GetPointerStateNative(const PfPointer *pointer, int param)
{
	/* AmigaBasic: 0 -> button position; 1, 2 -> cur x, y; 3, 4 -> starting x, y; 5, 6 -> ending x, y */
	
	switch(param) {
		case 0:
			return pointer->buttonDown; /* TODO all the possibilities ... */
		case 1:
			return pointer->current.x;
		case 2:
			return pointer->current.y;
		case 3:
			return pointer->starting.x;
		case 4:
			return pointer->starting.y;
		case 5:
			return pointer->ending.x;
		case 6:
			return pointer->ending.y;
	}
	return 0;
}

static bool TranslateMenuActionNative(const PfWindowEvent *event, PfMenuSelection *selection)
{
	if(event->Class == IDCMP_MENUPICK && event->Code != MENUNULL && event->IDCMPWindow->MenuStrip != NULL) {
		/* TO DO doesn't handle multiple selection or MENUHELP. Mind you, who ever used these? */	
		selection->menuNum = MENUNUM(event->Code);
		selection->itemNum = ITEMNUM(event->Code);
		selection->subItemNum = SUBNUM(event->Code);
		selection->window = event->IDCMPWindow;
		return TRUE;
	}
	else
		return FALSE;
}

static void GetMenuSelectionNative(QString *name, const PfMenuSelection *selection)
{ 
	QsInitNull(name);
	if(WindowExists(selection->window) && selection->window->MenuStrip != NULL)
		GetMenuName(name, selection->window->MenuStrip,
			selection->menuNum, selection->itemNum, selection->subItemNum);
}

#endif /* AMIGA */
