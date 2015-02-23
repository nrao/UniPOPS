C-------------------------------------------------------------------------------
C  @(#)appl.inc	5.2 09/10/98
C-------------------------------------------------------------------------------
C					Include	APPL.
C
      include 'params.inc'
c
      integer*2 MAXPLOTS
      parameter (MAXPLOTS=65)
      integer*4 RAPPLSIZE
c           we need to get the size of Rappl so that its at least
c           as large as necessary.  Go to the next highest 512
c           size. 
c        The terms here are:  twh (10) and accum = 11 HDUs
c                             xdata, ydata, word = 3 * MAX_DATA_POINTS,
c                             iplots/rplots/dplots
c                             xorg,yorg,xmax,ymin,showplot + dumappl - i*2s
c                             ax,ay,bx,by - r*4s
c                             astack + chshifts = 3*5120
c                             daindex,sumsq,sclfctx,sclfcty,sclchar = 105
c                             numappl, ichnext, ichnum = 3 I*4s
c                             rixt,riyt,breg = 3*32 I*2s
c                             26 remaining I*2s
c
c	This must be done by hand
c		11*(16384+395*2) + 3*16384 + 65*395*2 + (5*65+1)/2
c	        + 4*65 + 5120*3 + 105 + 3 + 3*32/2 + 26/2
c		 = 305368
c       Next higher multiple of 512 -> 305664
c                             
      parameter (RAPPLSIZE=305664)
c
      INTEGER*2  XORG(MAXPLOTS),  YORG(MAXPLOTS),  XMAX(MAXPLOTS),
     .		 YMAX(MAXPLOTS), UXA,       
     .           SPLT, RPLT,  itwh(HDU_FLOAT_SIZE*2, 10),
     .           idx, ibase, ires, lit, lxa, ix0, ixm,
     .		 iap_dum,  iy0,    iym,     limx,  limy,   ltype,
     .	        maxstck, maxregn,  maxmrk,naccum,
     .		showplot(MAXPLOTS),
     .        rixt(32),   isbg,riyt(32),breg(32), xorg0,   yorg0,
     .           xmax0,  ymax0,iaccum(HDU_FLOAT_SIZE*2), numplots,
     .		iplots(PADDED_HDR_SIZE*4, MAXPLOTS), idumappl 
      integer*4 ichnext, ichnum
      integer*4 numappl
      real*4 ax(MAXPLOTS), ay(MAXPLOTS), by(MAXPLOTS), bx(MAXPLOTS),
     .       sumsq,  sclfctx, sclfcty, 
     .       twh(HDU_FLOAT_SIZE,10), xdata(MAX_DATA_POINTS), sclchar, 
     .       daindex(101), ydata(MAX_DATA_POINTS), 
     .       work(MAX_DATA_POINTS), astack(5120), 
     .       raccum(HDU_FLOAT_SIZE),
     .       chshifts(5120,2), rplots(PADDED_HDR_SIZE*2, MAXPLOTS)
      double precision dtwh(HDU_FLOAT_SIZE/2, 10), 
     .                 daccum(HDU_FLOAT_SIZE/2), 
     .		       dplots(PADDED_HDR_SIZE, MAXPLOTS)
C
      COMMON /APPL/ 
     .     dtwh,  dplots, 
     .    sclfctx,  sclfcty,       ax,       ay,     bx,
     .         by,    sumsq,    xdata,    ydata,     work,
     .     astack,  sclchar,   raccum,  daindex,  chshifts,      
     .  ichnext, ichnum, 
     .     xorg,     yorg,     xmax,     ymax,     xorg0,   yorg0, 
     .    xmax0,    ymax0,   naccum,    ibase,      ires,   ltype,
     .      lit,      lxa,      uxa,     splt,      rplt,     idx,    
     .      limx,     limy,   ix0,     ixm, 
     .      iap_dum,  iy0,      iym,     isbg,    numplots,
     .    maxstck,  maxregn,   maxmrk,      rixt,    riyt,
     .     breg, showplot,  idumappl, numappl
c
c		Rappl is used in recover and exitpops and is included here
c		to make it easier to find
c
      integer*4 Rappl(RAPPLSIZE)
      equivalence (Rappl, dtwh)
c
c     APPL contain variables which the various algorithms need to
c   	pass to one onother.  
c
c     dtwh = arrays 0 thru 9.
c     sclfctx, sclfcty = ratio of present plot limits to full screen limits
c     ax, bx = channel# * ax + bx = pixel location for channel#
c     ay, by = T*ay + by = pixel location of specified Y point
c     sumsq = sum of square of residuals after gaussian fit.
c     xdata, ydata, work = working arrays
c     astack = STACKing array
c     sclchar = ratio of present character size to default
c     raccum = 'hidden' array for use of ACCUM.
c     daindex = stores scan numbers of ACCUMed data.
c     xorg,yorg,xmax,ymax = plotting limits within xorg0, yorg0, etc. for
c		next plot.
c     xorg0,yorg0,xmax0,ymax0 = overall plotting limits set in initappl
c     naccum = number of accumulated scans.
c     ibase = Number of data points to be used in baseline calculation
c     ires = Order of polynomial which resulted from baseline fit.
c     ltype = current line type
c     lit = Used by FLAG so that output strings from subsequent calls to 
c	FLAG will not overlap.
c     lxa = flag for determining whether velocity, channel, freq are to
c	be displayed along the lower x-axis.
c     uxa = Same as LXA but for upper x-axis.
c     splt = flag for type of plot; set by LINE, HISTOGRAM, POINTS
c     rplt = Same as splt but for RESHOWtype of plots.
c     iap_dum = place holder for alignment, feel free to use this if needed
c     idx = current color
c     limx, limy = Number of tick marks in the x and y directions
c     ilimit = number of bytes in a TWH subarray (i.e., max. number of storable
c	bytes in header + data
c     ix0,ixm,iy0,iym = plotting limits within xorg, yorg, etc. for
c		the actual graph (minus th border commentaray).
c     isbg = starting data point for various verbs such as GPARTS.
c     maxpts = max number of data points in various arrays
c     maxstck = max number of scans in stacks
c     maxregn = max number of baseline/gauss regions and LEVS
c     maxmrk = max number of plotting markers (HMARK, CMARK, etc.) and
c	gaussians
c     rixt, riyt = array containing the location x and y tick marks
c     breg = internnaly used for passing baseline regions; originnally part
c	of core but not needed there
c     showplot = flag as to the last kind of plot performed.
c     numappl = number of bytes in appl common block
c
c     chshifts = array of scans and associated shifts in channels.
c                set by SHIFT, cleared by SCLEAR, listed by LISTSHIFT
C     ichnext = next location to use in chshifts
c     icnum = total number of slots in use in chshifts, max of maxstck
c
c     rplots, dplots, iplots = Arrays to store headers for multiple plots
c			       on a page.
c     numplots = number of plots drawn to the screen.
c
      equivalence (dtwh, twh), (twh, itwh), (daccum,raccum),
     .            (raccum,iaccum)
      equivalence (dplots, rplots), (dplots, iplots)
c
