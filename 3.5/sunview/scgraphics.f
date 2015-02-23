      program graphics
c
c     @(#)scgraphics.f	5.1 06/22/94
c
c     Accepts graphics commands from the Fifo specified as the second arguement
c     on the command line and does the graphics command using SunCore.  If the
c     command is Flush, then it will read graphics commands from the
c     shared memory (which has the same name as this Fifo).  When a
c     cursor is asked for, the position and button pushed are returned to the
c     Fifo specified as the first command-line arguement. 
c
c     Starts up the 'CHECKUP.EXE' script to make sure that both this process
c     and its parrent process (whose ID is given as the third arguement)
c     remain running
c
      character*80 string
      character*16 fifoin, fifoout, shove, popsprocid, procid,
     .		   stch
      character*18 blank  
      character*1 char 
      character*5 memname
      logical front, open, testopen   
      integer*2 ilen, lastblnk, icp, icpx, icpy, short
      integer*4 iargc, inumarg, long, irtn, readshm2, cmnd, pos1, pos2,
     .          getpid, system, ifpread, ifpwrite, unlink, i, j
c
      data blank/'                  '/
c
      open(unit=0, form="print")
c
      inumarg = iargc()
c
      if (inumarg .lt. 3) then
	write(0,*) 'GRAPHICS: bad parameter list'
	call exit(long(6))
      else
	call getarg(long(1),fifoin)
	call getarg(long(2),fifoout)
	call getarg(long(3),popsprocid)
      endif
c     Gets the shared memory and Fifo names from the command line
c
      irtn = getpid()
      write(procid, 1) irtn
1     format(i8)
      irtn = system('checkup.exe ' // procid // ' ' // 
     .              popsprocid // ' & ')
c     Get this processes PID and start up 'CHECKUP.EXE'
c
      call openwrite(ifpwrite, fifoin(1:lastblnk(fifoin)) // '\0')
      call openread(ifpread, fifoout(1:lastblnk(fifoout)) // '\0')
c     open FIFO's
c
      ilen = lastblnk(memname)
c                               only 4 characters can be used for the
c                               shared memory name (its a long int)
c                               So, we use the last 4 digits of popsprocid
c                               substitute 0's if less than 4 digits
      ilen = lastblnk(popsprocid)
      j = 1
      do 105 i = ilen-3, ilen
         if (i .le. 0) then
            memname(j:j) = '0'
         else
            memname(j:j) = popsprocid(i:i)
         endif
         j = j + 1
 105  continue
c                               null terminate it
      memname(5:5) = char(0)
c
      call accessshm(memname)
c     Start up shared memory
c
      call coreint
c     Start up SunCore
c
10    call readfifo(ifpread, irtn, shove)
c     Read from FIFO
c
      if (shove(1:4) .eq. 'exit') then
	call coreclose()
	call closefifo(ifpread, irtn)
	call closefifo(ifpwrite, irtn)
	irtn = unlink(fifoin)
	irtn = unlink(fifoout)
	stop
c	Close down program
c
      else if (shove(1:6) .eq. 'cursor') then
	if ( testopen() ) then
	   write(0,*) char(27), '[5t'
	else
	   write(0,*) char(27), '[1t'
	endif
c	Opens window if it was closed; brings window forward if open.
c
	call corecursor(icp, icpx, icpy)
	write(stch,15) icp, icpx, icpy
15	format(3i5)
        call writefifo(ifpwrite, irtn, stch // '\0')
c       Start up a CURSOR session and return position and button number
c
      else if (shove(1:5) .eq. 'clear') then
 	call coreclrpage
c       Clear the screen
c
      else if (shove(1:5) .eq. 'flush') then
c       We have something in shared memory that needs to be processed
c
	if ( testopen() ) then
	   front = .false.
	   open = .true.
	else
	   front = .true.
	   open = .false.
	endif
c	Find out if window is opened.  If it is closed, then assume window will
c	be in front when opened; else, assume it is behind (you suffer no penalty
c	if it were in front and you assume it were behind).
c
20      irtn = readshm2(cmnd, pos1, pos2, string)
	if (irtn .gt. 0) then
c	   We haven't gotten to the end of shared memory yet
c
	   if (cmnd .eq. 2 .or. cmnd .ge. 10) then
	      if (.not. open) write(0,*) char(27), '[1t'
	      if (.not. front) write(0,*) char(27), '[5t'
	      open = .true.
	      front = .true.
	   endif
c	   Opens window (if it were closed); Fronts window;
c	   Only need to do this for VCTR, DITHER, BOX, and CHAR commands.
c
	   if (cmnd .eq. 1) then
	      call coreplace(short(pos1), short(pos2))
c	      Place command
c
	   else if (cmnd .eq. 2) then
	      call corevctr(short(pos1), short(pos2))
c	      Vctr command
c
	   else if (cmnd .eq. 4) then
	      call corecolor(pos1)
c	      Command for updating or changing the color table.
c
	   else if (cmnd .eq. 15) then
	      call corebox(string)
c	      Command for drawing a filled box.
c
	   else if (cmnd .eq. 16) then
	      call coredither(short(pos1), short(pos2), string)
c	      Command for half-tone box.
c
c***           else
c***	      write(0,*) 'Bad command:', cmnd
	   endif
	   goto 20
c
	else
c
	   call writefifo(ifpwrite, irtn, 'done')
c	   Shared memory is exausted and we should ask for more graphics commands
c
	endif
c
c***      else
c***	write(0,*) 'Bad shove'
      endif
c
      goto 10
c
99    write(0,*) 'GRAPHICS: Problem opening file'
      stop
      end
c
c
      subroutine coreflush
c
c     Dummy routine needed for linking purposes only.
c
      return
c
      end
c
c
      subroutine corebox(string)
c
c     Draws a filled box at the coordinate cooded into STRING
c
      character*(*) string
c
      real*4 boxx(4), boxy(4), xscale, yscale
      integer*4 long
      integer*2 i
c
      data xscale/0.8/, yscale/ 0.8/
c
      read(string,10) boxx, boxy
10    format(8f10.0)
c
      do 11 i = 1, 4
	boxx(i) = boxx(i)*xscale
	boxy(i) = boxy(i)*yscale
11	continue
c
      call polygonabs2(boxx, boxy, long(4))
c
      return
      end
c
c
      subroutine coredither(ix, iy, string)
c
c     Draws a half-tone box at the coordinate cooded into STRING
c
      character*(*) string
c
      integer*4 long, levmax, long 
      integer*2 nx, ny, ix, iy, nxold, nyold, ixdt(16384), iydt(16384),
     .		matset(16384), i, ii3, ierr
      real percent, rand, xscale, yscale, xdt4, ydt4
c
      data xscale/0.8/, yscale/ 0.8/
c
      save nxold, nyold
c
      read(string,10,iostat=ierr) nx, ny, percent
10    format(i5, i5, f10.0)
      nx = nint(xscale * nx)
      ny = nint(yscale * ny)     
      levmax = long(nx) * long(ny)
      levmax = min(long(16384),levmax)
c
      if (nx .ne. nxold .or. ny .ne. nyold) then
         do 100 i = 1, levmax
	   matset(i) = 0
100	   continue
c          Clear out set matrix
c
         do 130 i = 1, levmax
120	   ixdt(i) = nint(rand(long(0))*float(nx-1)+1.0)
	   iydt(i) = nint(rand(long(0))*float(ny-1)+1.0)
	   ii3 = ny*(ixdt(i)-1) + iydt(i)
	   if (matset(ii3) .eq. 1) goto 120
	   matset(ii3) = 1
130	   continue
      endif
c     Fill in pixel arrays and set metrix.
c
      ii3 = nint(rand(long(0))*float(levmax-1)+1.0)
      do 310 i = 1, nint(percent*float(levmax))
		ii3 = ii3 + 1
		if (ii3 .gt. levmax) ii3 = 1
		xdt4 = xscale*float(ix) + float(ixdt(ii3) - 1)
		ydt4 = yscale*float(iy) + float(iydt(ii3) - 1)
      		call MoveAbs2(xdt4, ydt4)
		call LineAbs2(xdt4, ydt4)
310		continue
c     Draw pixels
c
      nxold = nx
      nyold = ny
      return
      end
c
      subroutine corevctr(ix, iy)
c
c     Draws a line from the position last specified by
c     move to the position ix, iy -- SUNCORE
c
      integer*2 ix, iy
      real*4 xscale, yscale, x2, y2
c
      data xscale/0.8/, yscale/ 0.8/
c     Ratios between LINE/CONDAR internal screen units and SUNCORE
c	screen units
c
      x2 = xscale*ix
      y2 = yscale*iy
      call LineAbs2(x2, y2)
c
      return
      end
c
      subroutine coreplace (ix, iy)
c
c     Places cursor for next VCTR, BOX, CHAR call 
c
      integer*2 ix, iy
      real*4 xscale, yscale, x1, y1
c
      data xscale/0.8/, yscale/0.8/
c     Ratios between LINE/CONDAR screen size and SUNCORE screen size
c
      x1 = xscale*ix
      y1 = yscale*iy
      call MoveAbs2(x1,y1)
c
      return
      end
c
      subroutine corecursor(irtn, ix, iy)
c
c     Returns cursor position in SUNCORE
c
      integer*4 long, ibutton 
      integer*2 irtn, ix, iy
      real*4 xscale, yscale, xfact, yfact, xshift, yshift, x1, y1, x, y
c
      data xscale/1025/, yscale/1066.7/
      data xfact/1./, yfact/0.935266/, xshift/-0.5/, yshift/-0.375/
c     xscale, yscale = number of pixels in SUNCORE screen; used to convert
c	from screen coords to those used by the program
c     After button on mouse is pushed, the x,y coords of the center of the
c	cursor are given by x = (x + xshift)*xfact; y = (y + yshift)*yfact
c
c
      call SetSegVisibility(long(1), long(1))
      call SetSegHighlight(long(1), long(0))
c     Turns cursor on
c
      irtn = 0
      x1 = 0
      y1 = 0
c
10       call AwtButtonGetLoc2(long(0), long(1), ibutton, x, y)
         if (ibutton .eq. 0) then
	     if(x1.ne.x .or. y1.ne.y) then
                 call SetSegImgXLate2(long(1), xfact*(x+xshift), 
     1					   yfact*(y+yshift))
		 x1 = x
		 y1 = y
	     endif
	     goto 10
         endif
c     Main loop which updates cursor if the mouse is moved; exits loop
c	if buttons 1 or 3 are pushed
c
      call SetSegVisibility(long(1), long(0))
c     Turns cursor off
c
      if (ibutton .eq. 3) then
	irtn = -1
      else
	irtn = -2
      endif
      ix = x*xscale
      iy = y*yscale
c
      return
      end
c
      subroutine coreclrpage
c
c     Clears SUNCORE graphics screen
c
      integer*4 long
c
      call DelAllRetainSegs()
c
      call SetImgXFormType(long(2))
      call SetRasterOp(long(1))
      call CreateRetainSeg(long(1))
      call MoveAbs2(410.,320.)
      call LineRel2(0.,640.)
      call MoveAbs2(410.,320.)
      call LineRel2(0.,-640.)
      call MoveAbs2(410.,320.)
      call LineRel2(820.,0.)
      call MoveAbs2(410.,320.)
      call LineRel2(-820.,0.)
      call CloseRetainSeg()
      call SetSegVisibility(long(1),long(0))
c     Creates image of crosshair
c
      Call SetImgXFormType(long(1))
      call SetRasterOp(long(0))
      call CreateRetainSeg(long(2))
c     Opens Standard Graphics Segment
c
c
      return
      end
c
      subroutine coreint
c
c     Initializes SUNCORE Graphics Screen
c
      integer*4 loc, long, idx, i, vsurf(21), ncolor, icolor(128), 
     .		ddin, dynb, twod, fals, loc2, but2, iprec, ilast,
     .		cgpixwindd, noinpt, mapsize, cmapsize, InitializeCore,
     .		InitializeVwsurf, SelectVwsurf, pixwindd
      real cred(128), cblue(128), cgreen(128)
c
      external cgpixwindd
      external pixwindd
c
      save ncolor, cred, cgreen, cblue, icolor, ilast
c
      data ddin/12/, dynb/3/, twod/0/, fals/0/, loc2/3/, but2/2/, 
     .     iprec/1/, noinpt/0/, cmapsize/14/
c     These SUNCORE parameters were gotten from 'usr/include/f77/usercore77.h'
c
      data vsurf /21*0/,mapsize/127/,icolor/0,127*15790320/,ncolor/2/,
     1     cred/0.0,127*1.0/, cgreen/0.0,127*1.0/,cblue/0.0,127*1.0/,
     2     ilast/1/
c     Parameters used by or passed to SUNCORE routines. 
c     VSURF = array containing SUNCORE surface definistions. 
c     MAPSIZE = 1 less than number of colors in color table.
c     ICOLOR = array containing coded colors in table. -- first two colors
c		are black (background) and white.
c     NCOLOR = number of colors used in table.
c     CRED, CGREEN, CBLUE = colors in table.
c     ILAST = entry in color table that was last used.
c
      if(InitializeCore(dynb, noinpt, twod).ne.0) then
	write(0,*) 'Cannot initialize SUNCORE... Terminating'
	call exit(long(6))
      endif
c
      vsurf(ddin) = loc(cgpixwindd)
      vsurf(cmapsize) = mapsize + 1
      if (InitializeVwsurf(vsurf, fals) .ne. 0) then
        vsurf(ddin) = loc(pixwindd)
        vsurf(cmapsize) = mapsize + 1
        if (InitializeVwsurf(vsurf, fals) .ne. 0) then
	  write(0,*) 'Cannot initialize View Surface... Terminating'
	  call exit(long(6))
        endif
      endif
c     First, try COLOR; if unsuccessful, try B&W
c
      if(SelectVwsurf(vsurf) .ne. 0) then
	write(0,*) 'Cannot Select View Surface... Terminating'
	call exit(long(6))
      endif
c
      call SetViewport2(0., 1., 0., 0.7)
      call SetWindow(0., 820., 0., 640.)
      call SetOutputClip(long(1))
c     Sets up graphics window sizes
c
      call InitializeDevice(loc2, long(1))
      call InitializeDevice(but2, long(3))
      call InitializeDevice(but2, long(1))
      call SetEcho(loc2, long(1), long(0))
      call SetEchoSurface(loc2, long(1), vsurf)
c     Initializes the mouse for input
c
      call SetImgXFormType(long(2))
      call SetRasterOp(long(1))
      call CreateRetainSeg(long(1))
      call SetLineIndex(long(1))
      call MoveAbs2(410.,320.)
      call LineRel2(0.,640.)
      call MoveAbs2(410.,320.)
      call LineRel2(0.,-640.)
      call MoveAbs2(410.,320.)
      call LineRel2(820.,0.)
      call MoveAbs2(410.,320.)
      call LineRel2(-820.,0.)
      call CloseRetainSeg()
      call SetSegVisibility(long(1), long(0))
c     Creates image of crosshair
c
      Call SetImgXFormType(long(1))
      call SetRasterOp(long(0))
      call CreateRetainSeg(long(2))
c     Opens Standard Graphics Segment
c
      call DefColorIndices(vsurf, long(0), mapsize, cred, cgreen,
     .		 cblue)
      call SetFillIndex(long(1))
      call SetLineIndex(long(1))
      call SetTextIndex(long(1))
c     Sets up default color table
c
      return
c
      entry coreclose
c    
c     Close down SUNCORE
c
      call TerminateDevice(loc2, long(1))
      call TerminateDevice(but2, long(3))
      call TerminateDevice(but2, long(1))
      call CloseTempSeg()
      call DeselectVwsurf(vsurf)
      call TerminateCore()
c
      return
c
      entry corecolor(idx)
c
c     Adds a color to the color table
c
c     Color stored in IDX in RGB notation in compact format
c     IDX = RED*(16^5) + GREEN*(16^3) + BLUE*16 where RED,GREEN,BLUE goes
c	from 0 thru 15.  IDX for black = 0, for white = 15790320 
c
      do 100 i = 1, ncolor
	if (idx .eq. icolor(i)) goto 200
100	continue
c
      ncolor = ncolor+1
      if (ncolor .gt. mapsize + 1) ncolor = 3
c     Reset index to color in table if table overflows -- don't overwrite
c     first two default colors.
c
      icolor(ncolor) = idx
      i = ncolor
      cred(i) = and(rshift(idx,16),255)/255.
      cgreen(i) = and(rshift(idx,8),255)/255.
      cblue(i) = and(idx,255)/255.
      call DefColorIndices(vsurf, long(0), mapsize, cred, cgreen,
     .			   cblue)
c     Creates new color in index if the same color doesn't already exist.
c
200   if (i .ne. ilast) then
	call SetFillIndex(i-1)
        call SetLineIndex(i-1)
        call SetTextIndex(i-1)
	ilast = i
      endif
c     New graphics will now be done in new color (if different from 
c     color just previously done).     
c
      return
c
      end
c
      SUBROUTINE COPY (N,KFROM,KTO)
C
C     COPY TRANSFERS N INTEGER WORDS FROM KFROM TO KTO.
C
      integer*2 KFROM(*), KTO(*)
      integer*2 n, i
c
      DO 10 I=1,N
         KTO(I) = KFROM(I)
   10    CONTINUE
      RETURN
      END
c
      integer*2 function lastblnk(string)
c
c     Finds the position of the last non-blank character in STRING
c
      character*(*) string
      integer*4 lnblnk
c
      lastblnk = lnblnk(string)
c     MASSCOMP/SUN function LNBLNK
c
      return
      end
c
      INTEGER*2 FUNCTION IWPC (IP)
c
C----------------------------------------------------------------------
C	 IWPC calculates the number of integer words required to
C     hold IP number of	characters.
C----------------------------------------------------------------------
c
      integer*2 ncpi, ip
c
      DATA NCPI/2/
      IWPC = (IP-1)/NCPI + 1
      RETURN
      END
c
      logical function testopen()
c
c     Returns TRUE if the window is open, else it returns FALSE
c
      character*1 char, ans1
      integer*4 irtn
      logical first
c
      data first/.true./
c
      if (first) call setraw
c     Set terminal in RAW mode if this is the first time you are testing window.
c
      write(0,*) char(27), '[11t'
c     Write string out that terminal will respond to so that we can tell
c	whether window is open or closed.
c
5	continue
	call keystroke(irtn, ans1)
c	Read in terminal's response to above string
c	irtn = -1 for EOF, = 1 if keystroke successful, 0 if we have to wait 
c	a bit more.
c
	if (irtn .lt. 0 .or. (irtn .gt. 0 .and. ans1 .eq. '2')) then
	    testopen = .false.
	else if (irtn .gt. 0 .and. ans1 .eq. '1') then
	    testopen = .true.
	else 
	    goto 5
	endif
c	If string contains a 1 then window is open else if a 2 then closed;
c
      return
      end
        
