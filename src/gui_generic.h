/****** GUI_Generic.h ******/

/* 
	$VER: gui_generic.h 0.16A (1.17.2017)
	
	Stub graphics and GUI functions for platforms where it isn't supported/implemented yet.
*/

#ifndef AMIGA

typedef void *PfWindowHandle;
typedef void *PfScreenHandle;

#define NULL_SCREEN_HANDLE NULL
#define NULL_WINDOW_HANDLE NULL

typedef char PfKeypress;
typedef char PfPointer;
typedef char PfMenuSelection;

typedef int PfWindowEvent;

#define MAX_WINDOWS 1 /* Not really ... */
#define MAX_SCREENS 1 /* Not really ... */

#define MULTIPLE_VIRTUAL_SCREENS_SUPPORTED FALSE
#define DIRECT_TO_SCREEN_GRAPHICS_SUPPORTED FALSE

#define MAX_MENU_NAME_LENGTH 40

/* Check support for particular statements and functions: */

static bool FeatureAvailable(void (*f)(BObject *, unsigned)) { return FALSE; }

static bool FunctionAvailable(void (*f)(Scalar *, const BObject *, unsigned)) {	return FALSE; }

/* Screens and windows: */

static PfScreenHandle GetScreenNative(const QString *title, int width, int height, int depth, unsigned long mode)
	{ return NULL_SCREEN_HANDLE; }

static void ReleaseScreenNative(PfScreenHandle screen) { }

static bool WindowUsesScreenNative(PfWindowHandle w, PfScreenHandle s) { return FALSE; }

static PfWindowHandle OpenWindowNative(
	int id,
	const QString *title,
	const BasicRectangle *extent,
	PfScreenHandle screen,
	const QString *screenTitle,
	unsigned long flags)
{
	return NULL_WINDOW_HANDLE;
}

static void RemoveWindowNative(PfWindowHandle win, bool lastWindow) { }

static long GetWindowInfoNative(PfWindowHandle win, unsigned key) { return 0; }

/* Graphics and text rendering: */

static void EraseWindowInteriorNative(PfWindowHandle win) { }

static void SetWindowPenNative(PfWindowHandle win, bool foreground, short pen) { }

static int GetPixelPaletteColourNative(PfWindowHandle win, const BasicPoint *p) { return -1; }

static void SetPixelNative(PfWindowHandle win, const BasicPoint *p, short penOrDefault) { }

static void DrawLineNative(PfWindowHandle win, const BasicPoint *p1, const BasicPoint *p2) { }

static bool AddVertexToPolygonNative(PfWindowHandle win, const BasicPoint *p) { return FALSE; }

static bool FillPolygonNative(PfWindowHandle win) { return FALSE; }

static bool FloodFillNative(PfWindowHandle win, const BasicPoint *p, short penOrDefault) { return FALSE; }

static void DrawEllipseNative(PfWindowHandle win, const BasicRectangle *bounds) { }

static void SetPaletteColourNative(PfWindowHandle win, int id, float r, float g, float b) { }

static void SetPenPositionNative(PfWindowHandle win, const BasicPoint *p) { }

static void GetCurrentPenPositionNative(PfWindowHandle win, BasicPoint *p) { p->x = p->y = -1; }

static void AlertUserNative(PfWindowHandle win) { }

static void GetFontCharacterDimensionsNative(PfWindowHandle win, short *w, short *h) { *w = *h = -1; }

static void RenderTextNative(PfWindowHandle win, const QsChar *text, size_t nChars, short tabInterval) { }

/* Menus: */

static void AlterMenuNative(PfWindowHandle win, char *mName, char *iName, char *sName, char hotKey, unsigned long flags) { }

static int GetMenuItemStateNative(PfWindowHandle win, char *menuName, char *itemName, char *subitemName) { return 0; }

/* Events: */

static bool GetNextWindowEventNative(PfWindowEvent *nextEvent) { *nextEvent = -1; return FALSE; }

static bool TranslateKeyPressNative(const PfWindowEvent *event, PfKeypress *press) { return FALSE; }

static void GetKeyPressDataNative(QString *s, const PfKeypress *press) { QsInitNull(s); }

static bool TranslatePointerActionNative(const PfWindowEvent *event, PfPointer *pointer) { return FALSE; }

static int GetPointerStateNative(const PfPointer *pointer, int param) { return 0; }

static bool TranslateMenuActionNative(const PfWindowEvent *event, PfMenuSelection *selection) { return FALSE; }

static void GetMenuSelectionNative(QString *name, const PfMenuSelection *selection) { QsInitNull(name); }

#endif /* not AMIGA ... */
