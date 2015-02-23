
/* @(#)xroutines.c	5.3 09/23/94 */

/* standard includes */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <stdio.h>
#include <sys/time.h>
#include <signal.h>
extern void (*zignal())();

/*	the icon */
#include <pops.xbm>

/*	some constants */
#define TRUE 1
#define FALSE 0
#define MAXCOLOR 128
#define COLORMULT 65535
#define POPS_WHITE 15790320
#define POPS_BLACK 0

/*	useful macros */

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

/*	global variables */

static Display *display;
static Window window;
static Atom wm_delete_window;
static GC gc, gcb, gcc;
static Pixmap pixmap;
static Pixmap icon_pixmap;
static Cursor cursor;
static int screen;
static int depth;
Colormap cmap;
XColor colorcell_defs[MAXCOLOR];
unsigned long pixels[MAXCOLOR];
unsigned long AddColor();
unsigned long black, white;
static int icolor[MAXCOLOR], ncolor = 0, maxcol = 0, mono, Static;
static int private = FALSE;
static long fgpopscolor = -1;

static struct {
   int i, j;
} current, next;

static struct {
   int i, j;
} curs;

static float xscale, yscale, xzero, yzero;

/*		this is the internal unipops coord system */
static int xmin = 0, xmax = 1024, ymin = 0, ymax = 800;
static int resized = FALSE;

/*		this is the screen corrd system, set in xinit_ */
static int imin, imax, jmin, jmax;
static int pix_width, pix_height;
static unsigned int win_width, win_height;
float factor = 8.5 / 11.0;

void xinit_(iconic, name, geometry)
int *iconic;
char *name, *geometry;

{
   Visual *visual;
   Window parent;
   XSizeHints size_hints;
   XGCValues values;
   XSetWindowAttributes setwinattr;
   XWMHints wmhints;
   XVisualInfo VisualInfo;

   int cells;
   unsigned long value_mask;
   int x, y;
   unsigned int border_width = 4;
   unsigned int display_width, display_height;
   unsigned int display_widthMM, display_heightMM;
   XTextProperty winname_prop, iconname_prop;
   char *display_name = NULL;
   int cursor_cross=34;

   int i, status;
   float resol[2];



/*		connect to X server */

   if ((display = XOpenDisplay (display_name)) == NULL) {
      fprintf(stderr, "xinit: cannot connect to X server %s\n",
            XDisplayName(display_name));
      return;
   }

/*		Get screen size */

   screen = DefaultScreen(display);
   display_width = DisplayWidth(display, screen);
   display_height = DisplayHeight(display, screen);
   display_widthMM = DisplayWidthMM(display, screen);
   display_heightMM = DisplayHeightMM(display, screen);

/*		make sure it has the same aspect ratio as 8.5x11 paper */
   resol[0] = 25.4 * ((float) display_width / (float) display_widthMM);
   resol[1] = 25.4 * ((float) display_height / (float) display_heightMM);

/*		geometry can be specified on the command line */
/*		width takes precedence over height, if neither is specified */
/*		[i.e. both are 0], default to 1/3 of screen width */
/*		smallest allowable is 1/3 of screen width */

   status = XParseGeometry(geometry, &x, &y, &win_width, &win_height);
   if (!(status & XValue)) x = 0;
   if (!(status & YValue)) y = 0;
   if (!(status & WidthValue)) win_width = 0;
   if (!(status & HeightValue)) win_height = 0;
   if (win_width > 0) {
      win_width = min(win_width, display_width);
      win_width = max(win_width, display_width/3);
      win_height = factor * win_width;
   } else if (win_height > 0) {
      win_height = min(win_height, display_height);
      win_height = max(win_height, display_height/3);
      win_width = (float) win_height / factor;
   } else {
      win_width = display_width / 3;
      win_height = factor * win_width;
   }
   if (x != 0) {
      if (status & XNegative) x = display_width - win_width + x;
   } else {
      x = (display_width - win_width) / 2;
   }
   if (y != 0) {
      if (status & YNegative) y = display_height - win_height + y;
   } else {
      y = (display_height - win_height) / 2;
   }

/*		screen coordinates, allow for 1/4" border in window */

   imin = (int) (0.25 * resol[0] + 0.5);
   jmin = (int) (0.25 * resol[1] + 0.5);
   imax = win_width - imin - 1;
   jmax = win_height - jmin - 1;
   pix_width = imax - imin + 1;
   pix_height = jmax - jmin + 1;

/*		transformation values for screen to unipops coords */

   xscale = (float)(xmax - xmin) / (float)(imax - imin);
   xzero = (float) xmin - xscale * (float) imin;
   yscale = (float)(ymax - ymin) / (float)(jmin - jmax);
   yzero = (float) ymin - yscale * (float) jmax;

/*		classify the display and create a color map */

   depth = DisplayPlanes (display, screen);
   visual = DefaultVisual (display, screen);
   parent = RootWindow (display, screen);

   if (depth == 1) {
/*		its MONO */
      mono = TRUE;
   } else {
      switch (visual->class) {

         case PseudoColor :
/*		get the default color map */
            cmap = DefaultColormap(display, screen);

            value_mask = 0;
            mono = FALSE;
            Static = FALSE;
            break;
         case StaticColor :
/*		get the default color map */;
            cmap = DefaultColormap(display, screen);
            value_mask = 0;
            mono = FALSE;
            Static = TRUE;
            break;
         default:
/*		try and find one of type PseudoColor */
            if (!XMatchVisualInfo (display, screen, depth, PseudoColor,
                  &VisualInfo)) {
/*		give up and revert to mono */
               mono = TRUE;
            } else {
               visual = VisualInfo.visual;
               cmap = XCreateColormap(display, parent, visual, AllocNone);
               setwinattr.colormap = cmap;
               value_mask = CWColormap;
               mono = FALSE;
               Static = FALSE;
            }
            break;
      }
   }

/*	Create a window */

   if (!mono) {
      window = XCreateWindow (display, parent, x, y, win_width, win_height,
                border_width, depth, InputOutput, visual, value_mask,
                &setwinattr);
   } else {
      window = XCreateSimpleWindow(display, parent, x, y, win_width, win_height,
                border_width, white, BlackPixel(display,screen));
   }

/*		initialize the colormap */

   maxcol = 1;
   if (!mono) {
/*		how large can it be */
      cells = DisplayCells (display, screen);
      maxcol = min(cells, MAXCOLOR);
   }

/*	Load the color table, first pixel black, second white */

   if (!mono) {
      black = AddColor(POPS_BLACK);
      white = AddColor(POPS_WHITE);
   } else {
      black = BlackPixel(display, screen);
      white = WhitePixel(display, screen);
   }

/*		initial cursor position, center of window*/

   curs.i = (imax - imin) / 2;
   curs.j = (jmax - jmin) / 2;

/*		set the window colors */

   XSetWindowBackground (display, window, black);
   XSetWindowBorder (display, window, white);

/*		create icon pixmap */

   icon_pixmap = XCreateBitmapFromData (display, window,
      (char *)pops_icon_bits, pops_icon_width, pops_icon_height);

/*	size hints */

   size_hints.flags = PPosition | PSize | PMinSize | PAspect;
   size_hints.x = x;
   size_hints.y = y;
   size_hints.width = win_width;
   size_hints.height = win_height;
   size_hints.min_width = display_width / 3;
   size_hints.min_height = display_height / 3;
   size_hints.min_aspect.x = win_width;
   size_hints.min_aspect.y = win_height;
   size_hints.max_aspect.x = win_width;
   size_hints.max_aspect.y = win_height;

/*		other hints */

   wmhints.flags = InputHint | IconPixmapHint | StateHint;
   wmhints.input = TRUE;
   if (*iconic) {
      wmhints.initial_state = IconicState;
   } else {
      wmhints.initial_state = NormalState;
   }
   wmhints.icon_pixmap = icon_pixmap;

/*		and actually set them */

   XStringListToTextProperty(&name,(int) 1, &winname_prop);
   XStringListToTextProperty(&name,(int) 1, &iconname_prop);
   XSetWMProperties(display, window, &winname_prop, &iconname_prop, NULL, 0,
      &size_hints, &wmhints, NULL);

/*		tell manager we want DELETE_WINDOW events */
/*		we always ignore them, but we still want them */

   wm_delete_window = XInternAtom(display, "WM_DELETE_WINDOW", False);
   XSetWMProtocols(display, window, &wm_delete_window, 1);

/*	pixmap to write to, preserved for expose events */

   pixmap = XCreatePixmap (display, window, win_width, win_height, depth);

/*	graphics contexts */

   gc = XCreateGC (display, pixmap, 0, &values);
   gcb = XCreateGC (display, pixmap, 0, &values);
   gcc = XCreateGC (display, pixmap, 0, &values);
   XSetFillRule (display, gc, WindingRule);
   XSetFillStyle (display, gc, FillSolid);
   XSetFillRule (display, gcb, WindingRule);
   XSetFillStyle (display, gcb, FillSolid);
   XSetFillRule (display, gcc, WindingRule);
   XSetFillStyle (display, gcc, FillSolid);
   XSetFunction (display, gcc, GXxor);
   XSetForeground (display, gc, white);
   fgpopscolor = POPS_WHITE;
   XSetForeground (display, gcb, black);
   XSetForeground (display, gcc, black);

/*	clear the pixmap */

   XFillRectangle (display, pixmap, gcb, 0, 0, win_width, win_height);

/*	what events do we want */

   XSelectInput (display, window, StructureNotifyMask |
          ExposureMask | ButtonPressMask | 
          PointerMotionMask);

/*	define a cursor */

   cursor = XCreateFontCursor (display, cursor_cross);
   XDefineCursor (display, window, cursor);
   XRecolorCursor (display, cursor, &colorcell_defs[1], 
                      &colorcell_defs[0]);


/*	finally, display the window */

   XMapWindow(display, window);

}


void xclose_()
{
   XUndefineCursor(display, window);
   XFreeCursor(display, cursor);
   XUnmapWindow(display, window);
   XFreeGC (display, gc);
   XFreeGC (display, gcb);
   XFreeGC (display, gcc);
   XDestroyWindow(display, window);
   XFreePixmap (display, icon_pixmap);
   XFreePixmap (display, pixmap);
   if (!mono && !Static) XFreeColors(display, cmap, pixels, ncolor, 0);
   if (private) XFreeColormap(display,cmap);
   XCloseDisplay(display);
}

void xcursor_(icp, icpx, icpy)
int *icp, *icpx, *icpy;
{
   XEvent report;
   Window root, child, old_child;
   int root_x, root_y, pos_x, pos_y, offset_x, offset_y, 
       old_x, old_y;
   unsigned int keys_buttons;

   offset_x = (win_width - pix_width) / 2;
   offset_y = (win_height - pix_height) / 2;
   XDrawLine(display, pixmap, gcc, curs.i, jmin, curs.i, jmax);
   XDrawLine(display, pixmap, gcc, imin, curs.j, imax, curs.j);
   XCopyArea(display, pixmap, window, gc, imin, jmin, pix_width, pix_height, 
             offset_x, offset_y);

/*		find out where the pointer is so we can return it later */
/*		two queryies, one gets ID of child, second gets pos rel to */
/*		to child window */

   XQueryPointer(display, RootWindow(display, screen), &root, &old_child,
                 &root_x, &root_y, &pos_x, &pos_y, &keys_buttons);
   XQueryPointer(display, old_child, &root, &child,
                 &root_x, &root_y, &old_x, &old_y, &keys_buttons);

/*		now warp it to the graphics window */
   XWarpPointer(display, None, window, 0, 0, 0, 0, curs.i, curs.j);

   while (XCheckTypedEvent (display, ButtonPress, &report));

   while (1) {
      XNextEvent (display, &report);
      switch(report.type) {

      case Expose:
         XCopyArea(display, pixmap, window, gc, imin, jmin, pix_width, 
                   pix_height, offset_x, offset_y);
         break;
      case ButtonPress :
/*			ignore Button2 */
         if (report.xbutton.button == Button2) break;
/*			else return position and button pressed */
/*			this is how unipops likes it */
/*			but 1 => 1 */
/*                      but 3 => -1 */
         *icpx = (int)(xscale*curs.i + xzero + 0.5);
         *icpy = (int)(yscale*curs.j + yzero + 0.5);
         if (report.xbutton.button == Button1) {
            *icp = 1;
         } else {
            *icp = -1;
         }
         goto endcursor;
      case MotionNotify:
         if (!XQueryPointer(display, report.xmotion.window,
              &root, &child, &root_x, &root_y, &pos_x, &pos_y,
              &keys_buttons))
            break;
         XDrawLine(display, pixmap, gcc, curs.i, jmin, curs.i, jmax);
         XDrawLine(display, pixmap, gcc, imin, curs.j, imax, curs.j);
         curs.i = pos_x - offset_x + imin;
         curs.j = pos_y - offset_y + jmin;
         XDrawLine(display, pixmap, gcc, curs.i, jmin, curs.i, jmax);
         XDrawLine(display, pixmap, gcc, imin, curs.j, imax, curs.j);
         XCopyArea(display, pixmap, window, gc, imin, jmin, pix_width, 
                   pix_height, offset_x, offset_y);
         break;
      default:
         break;
      }
   }

   endcursor:

      XDrawLine(display, pixmap, gcc, curs.i, jmin, curs.i, jmax);
      XDrawLine(display, pixmap, gcc, imin, curs.j, imax, curs.j);
      XCopyArea(display, pixmap, window, gc, imin, jmin, pix_width, pix_height,
                offset_x, offset_y);
/*			return pointer to original location */
      XWarpPointer(display, None, old_child, 0,0,0,0, old_x, old_y);

}

void xplace_(pos1, pos2)
int *pos1, *pos2;
{
   current.i = (int) (((float)*pos1 - xzero) / xscale + 0.5);
   current.j = (int) (((float)*pos2 - yzero) / yscale + 0.5);
}

void xvctr_(pos1, pos2)
int *pos1, *pos2;
{

   next.i = (int) (((float)*pos1 - xzero) / xscale + 0.5);
   next.j = (int) (((float)*pos2 - yzero) / yscale + 0.5);
   XDrawLine(display, pixmap, gc, current.i, current.j, next.i, next.j);
   current.i = next.i;
   current.j = next.j;
}

void xpoint_(pos1, pos2)
int *pos1, *pos2;
{
   current.i = (int) (((float)*pos1 - xzero) / xscale + 0.5);
   current.j = (int) (((float)*pos2 - yzero) / yscale + 0.5);
   XDrawPoint(display, pixmap, gc, current.i, current.j);
}

void xcolor_(idx)
long *idx;
{
   float cred, cgreen, cblue;
   unsigned long color;
   int i;

   if (!mono) {
      color = AddColor(*idx);
      XSetForeground(display, gc, color);
      fgpopscolor = *idx;
   } else if (*idx == 0) {
/*		mono has only two choices, 0 => black, else white */
      XSetForeground(display, gc, black);
      fgpopscolor = POPS_BLACK;
   } else {
      XSetForeground(display, gc, white);
      fgpopscolor = POPS_WHITE;
   }
}

void xbox_(boxx, boxy)
float boxx[4], boxy[4];
{
   int npoints = 4, shape = Convex, mode = CoordModeOrigin, i;
   XPoint corner[4];

   for (i=0;i<npoints;i++) {
      corner[i].x = (int) ((boxx[i] - xzero) / xscale + 0.5);
      corner[i].y = (int) ((boxy[i] - yzero) / yscale + 0.5);
   }

   XFillPolygon(display, pixmap, gc, corner, npoints, shape, mode);
}

void xclrpage_()
{
   int tmp_width, tmp_height;
   unsigned long color;
   if (resized) {
      if ((int)(win_width*factor)  >  win_height) {
         tmp_height = win_height;
         tmp_width = (float) win_height / factor;
      } else {
         tmp_width = win_width;
         tmp_height = win_width * factor;
      }
      imax = tmp_width - imin - 1;
      jmax = tmp_height - jmin - 1;
      pix_width = imax - imin + 1;
      pix_height = jmax - jmin + 1;
      xscale = (float) (xmax - xmin) / (float) (imax - imin);
      xzero = (float) xmin - xscale * (float) imin;
      yscale = (float) (ymax - ymin) / (float) (jmin - jmax);
      yzero = (float) ymin - yscale * (float) jmax;
      XFreePixmap(display, pixmap);
      pixmap = XCreatePixmap(display, window, tmp_width, tmp_height, depth);
   }
   XFillRectangle(display, pixmap, gcb, 0, 0, win_width, win_height);
   XClearWindow(display, window);
   if (!mono && !Static) {
      XFreeColors(display, cmap, pixels, ncolor, 0);
      ncolor = 0;
/*			start over with the default cmap */
      if (private) {
         XFreeColormap(display, cmap);
         private = FALSE;
      }
      cmap = DefaultColormap(display, screen);
/*			and Add the black and white colors */
      black = AddColor(POPS_BLACK);
      white = AddColor(POPS_WHITE);
/*			and Add the current fgpopscolor if -1 */
      if (fgpopscolor != -1) {
         color = AddColor(fgpopscolor);
         XSetForeground(display, gc, color);
      }
   }
}

void coreflush_()
{
/*	coreflush is in shm_graph.c and is a dummy routine over here */
}

void xflush_()
{

/*		copy the pixmap onto the window */

   XCopyArea(display, pixmap, window, gc, imin, jmin, pix_width, pix_height,
             (win_width - pix_width) / 2, (win_height - pix_height) / 2);
}

void xfront_()
{
   XMapRaised(display, window);
}

void alarm_off_()
{
   zignal(SIGALRM, SIG_IGN);
}

void alarm_on_()
{
/*		SIGNAL stuff for expose even handler */
/*		Timer */

   void pops_expose_handler();

   static struct itimerval tvalue = {
      {0, 333333},			/* 1/3 second interval */
      {0, 333333}                       /* 1/3 second value */
   };

   zignal(SIGALRM, pops_expose_handler);
   setitimer(ITIMER_REAL, &tvalue, NULL);
}

void pops_expose_handler ()
{
   XEvent event;
   int event_mask;

   alarm_off_();
   event_mask = ExposureMask | StructureNotifyMask;

   while (XCheckWindowEvent (display, window, event_mask, &event)) {

      switch (event.type) {
         case Expose :
            XCopyArea(display, pixmap, window, gc, imin, jmin, pix_width, 
                      pix_height, (win_width - pix_width) / 2, 
                      (win_height - pix_height) / 2);
            break;
         case ConfigureNotify :
            if (event.xconfigure.width != win_width || 
                event.xconfigure.height != win_height) {
               win_width = event.xconfigure.width;
               win_height = event.xconfigure.height;
               resized = TRUE;
            }
            XCopyArea(display, pixmap, window, gc, imin, jmin, pix_width, 
                      pix_height, (win_width - pix_width) / 2, 
                      (win_height - pix_height) / 2);
            break;
         default:
            break;
      }
   }
   alarm_on_();
}

void sig_init_()
{
   void sig_close();

   zignal(SIGTERM, sig_close);
   zignal(SIGHUP, sig_close);
   zignal(SIGPIPE, sig_close);
}

void sig_close(sig)
int sig;
{
   alarm_off_();
   xclose_();
   exit(sig);
}

unsigned long AddColor(popscolor)
   long popscolor;
{
   int i;
   unsigned long color;
   float cred, cgreen, cblue;
   unsigned long plane_masks[MAXCOLOR];
/*		assume that it isn't mono, should be checked before calling */
/*		do we know about this color yet */
   for (i=0;i<ncolor;i++) {
      if (popscolor == icolor[i]) break;
   }
   if (i >= ncolor) {
/*		no, we need to add it, if we can */
      if (i >= maxcol) {
/*		default to white if full */
         if (i < 2) {
/*		white isn't defined yet, for some reason it won't fit */
/*		change to mono set things appropriately */
	   mono = FALSE;
           maxcol = 1;
           black = BlackPixel(display, screen);
           white = WhitePixel(display, screen);
           color = white;
         } else {
/*		white is pixels[1] */
            color = pixels[1];
         }
      } else if (!Static) {
/*		can we allocate a new one */
         if (!XAllocColorCells(display, cmap, True, plane_masks, 0, 
                  &pixels[i], 1)) {
/*		nope, need a private one, have we already done this? */
            if (private) {
/*		we've already done this, set maxcol to i-1 and return white */
               maxcol = i - 1;
               i = 1;
            } else {
               cmap = XCopyColormapAndFree(display, cmap);
               private = TRUE;
               XSetWindowColormap(display, window, cmap);
               if (!XAllocColorCells(display, cmap, True, plane_masks, 0,
                        &pixels[i], 1)) {
/*		oh well, we tried */
                  maxcol = i - 1;
                  i = 1;
               }
            }
         }
/*		  if i == 1 and ncolor < 2 that means that */
/*		  a problem occured above and we just return white */
/*		  as long as popscolor != POPS_WHITE */
         if (i==1 && ncolor < 2 && popscolor != POPS_WHITE) {
            color = pixels[1];
         } else {
            icolor[i] = popscolor;
            ncolor++;
/*			decode it */
            cred = (float) ((popscolor >> (16)) & ((int) 255)) / 255.0;
            cgreen = (float) ((popscolor >> (8)) & ((int) 255)) / 255.0;
            cblue = (float) ((popscolor) & ((int) 255)) / 255.0;
            colorcell_defs[i].pixel = pixels[i];
            colorcell_defs[i].red = (int)(cred*COLORMULT+0.5);
            colorcell_defs[i].green = (int)(cgreen*COLORMULT+0.5);
            colorcell_defs[i].blue = (int)(cblue*COLORMULT+0.5);
            colorcell_defs[i].flags = DoRed | DoGreen | DoBlue;
            colorcell_defs[i].pad = 0;

            XStoreColor(display, cmap, &colorcell_defs[i]);
            color = pixels[i];
         }
      } else {
         XAllocColor(display, cmap, &colorcell_defs[i]);
         pixels[i] = colorcell_defs[i].pixel;
         color = pixels[i];
      }
   } else {
      color = pixels[i];
   }
   return color;
}
