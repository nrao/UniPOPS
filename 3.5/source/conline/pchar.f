      subroutine fset ( iset , spacing)
c
c     @(#)pchar.f	5.3 03/19/96
c
c     Allows for changing the character set
c
c     iset = (I*2) number of character set to use
c          = 1 (Default) Roman simplex
c	   = 2 Greek simplex
c          = 3 Greek Gothic
c	   = 4 Italic Gothic
c	   = 5 English Gothic
c	   = 6 Roman Duplex
c	   = 7 Cyrillic Complex
c	   = 8 Script Complex
c	   = 9 Roman Complex
c	   = 10 Italic Complex
c	   = 11 Greek Complex
c 	   = 12 Roman Triplex
c	   = 13 Italic Triplex
c	   = 14 Special symbols
c	   = 15 Script Simplex
c
c     spacing = (L*2) TRUE if you want equally soaced characters, else they
c		will be spaced by the size of the character.
c
c     Note: Up to three character sets will reside in core at any one
c	   time, one of which (the first loaded) is the default font
c	   which cannot be overwritten and the other two are user
c	   chosable fonts.  If more than two additional are needed, the
c	   program will throw away one of the old sets and read in the
c	   new set.  Switching between three character sets will,
c	   therefore, be much faster than if you switch between four or
c	   more.
c
      integer*2 iset
      logical spacing
c
      include 'cio.inc'
      include 'foxplot.inc'
c
      integer*2 ier, lastblnk, n0, m1, kset, i, j, n112, n352, n372,
     .		n356, n120, m3
      character*10 files(NUMFONTS)
      character*1023 charfile
      logical first
c
      parameter (n0 = 0)
      parameter (n112 = 112)
      parameter (n120 = 120)
      parameter (n352 = 352)
      parameter (n356 = 356)
      parameter (n372 = 372)
      parameter (m1 = -1)
      parameter (m3 = -3)
c
      data files/'roman     ','greek     ','gothgr    ','gothit    ',
     1           'gothen    ','duplex    ','cyrillic  ','compsc    ',
     2           'compro    ','compit    ','compgr    ','triplro   ',
     3           'triplit   ','special   ','script    '/
      data first/.true./
c
      if (iset .le. 0 .or. iset .gt. NUMFONTS) 
     .		call oerror(n112, m1, 'Bad character set')
c
      if (first) then
	oldest = 1
	first = .false.
c       Initializes OLDEST if this is the first time CSET has been called.
c
      else
         do 10 i = 1, NUMSETS
      	   if (iset .eq. usesets(i)) then
	      spaces = spacing
	      nset = i
	      return
           endif
10         continue
c	   Checks whether desired set is already loaded.
c
      endif
c
      kset = oldest
      charfile = dirprefix(1)(1:lastblnk(dirprefix(1))) // 
     .		'fonts/' //files(iset)
c     It's not already loaded so lets try reading in the font
c
      open (unit=iiotmp,file=charfile,status='old',iostat=ier)
      if (ier .ne. 0) call oerror(n352, m1, 'FONTSET')
      rewind(iiotmp,iostat=ier)
      if (ier .ne. 0) call oerror(n372, m1, 'FONTSET')
c
      do 20 i = 1, MAXNUMCHARS
	numc(kset) = i
	read(iiotmp,*,err=70,end=50) nc(i, kset),
     1		 (vx(kset,i,j), vy(kset,i,j), j=1, MAXNUMVCTRS)
20      continue
c
50    numc(kset) = numc(kset) - 1
      close (iiotmp,iostat=ier)
      if (ier .ne. 0) call oerror(n356, n0, 'FONTSET')
c     Reads in character set
c
      if (numc(kset) .le. 1) goto 70
c
      nset = kset
      usesets(nset) = iset
      oldest = oldest + 1
      if (oldest .gt. NUMSETS) oldest = 2
      spaces = spacing
c     After loading in the 1st font, oldest will cycle between values of 2 
c	and NUMSETS
c
      return
c     Font was read in OK so we can set up variables and return
c
70    oldest = kset
      usesets(kset) = 0
      call oerror(n120,m3,'FONTSET')
c
      end
c
c--------------------------------------------------
c
      subroutine rotlet (angle1)
c
c     To specify angle of rotation for characters.
c
c     angle1 = (R*4) (Default = 0.) Specifies the counterclocwise rotation 
c	      in radians.
c
c     The angle is not changed until the next call to ROTLET.
c
      real angle1
c
      include 'foxplot.inc'
c
      angle = angle1
c
      return
      end
c
c--------------------------------------------------
c
      subroutine charbd ( iwidth)
c
c     Varies the boldness of plotted characters.  The routine does this by
c     redrawing characters iwidth**2 times with offsets between each redrawing.
c
c     iwidth = (I*2) Character boldness (1 < IWIDTH < 3).  The higher the 
c		     value of IWIDTH, the bolder the character drawn.  The
c		     default value is 1.
c
c     Note: Affects all calls which draw characters. 
c
c     An alternate way of producing bolder characters is to change the 
c     character set with FSET to a "duplex" or "triplex" set.
c
c
      integer*2 iwidth
c
      include 'foxplot.inc'
c
      numbold = max(1,min(3,iwidth))
c
      return
      end
c
c--------------------------------------------------
c
      subroutine charpl (pchar, x0, y0)
c
c     Plots a character at a given location
c
c     PCHAR = (C*1) character to be plotted
c     X0, Y0 = (R*4) locations at which to plot character.
c
      character*(*) pchar
      real x0, y0
c
      include 'foxplot.inc'
c
      logical move
      real*4 c, a, xsizrot, ysizrot, cs, x0b, y0b, xx1, yy1, xoff, 
     .	     yoff, ix, iy
      integer*2 n120, m3, numchar, ic, k1, k2, j, x1, y1, 
     .		kset, searchi2
      character*1 kchar
c
      parameter (n120 = 120)
      parameter (m3 = -3)
c
      xsizrot(ix,iy,c,a) = ix*c*cos(a) + iy*c*sin(a)
      ysizrot(ix,iy,c,a) = ix*c*sin(a) - iy*c*cos(a)
c
      if (len(pchar) .ne. 1) 
     .		call oerror(n120, m3, 'CHARPL: Internal error')
c
      cs = 0.66*csize
c     0.66 = ratio of UniPops default character size to Foxplot's
c
      kset = nset
      kchar = pchar
      numchar = ichar (kchar )
      if (numchar .eq. 0) return
c
      ic = searchi2(numchar, nc(1,kset), numc(kset))
c     Try to find character in current font
c
      if (ic .eq. 0) then
c	Its not there so try to find it in default font.
	ic = searchi2(numchar, nc(1,1), numc(1))
c
	if (ic .ne. 0) then
c	   It was in the default font!
	   kset = 1
	else
c	   Its not in the default font, so use a space and search current font
	   numchar = ichar(' ')
	   ic = searchi2(numchar, nc(1,kset), numc(kset))
	   if (ic .eq. 0) then
c		Woooow!!  A space isn't in the current font!!  One more try!
		kset = 1
		ic = searchi2(numchar, nc(1,1), numc(1))
		if (ic .eq. 0) 
     .			call oerror(n120, m3, 'CHARPL: Internal Error!!')
c		A space wasn't found in the default font so bail out.
	   endif
  	endif
      endif
c
      if (spaces) then
	xoff = XDEFAULTS/2.
      else
	xoff = abs(vx(kset,ic,1))
      endif
      yoff = -YDEFAULTS/2.
c
      do 750 k1 = 0, numbold - 1
       x0b = x0 + xsizrot(xoff,yoff,cs,angle) + cs*(k1-numbold/2)
       do 700 k2 = 0, numbold - 1
	y0b = y0 + ysizrot(xoff,yoff,cs,angle) + cs*(k2-numbold/2)
c
        move = .true.
        do 600 j = 2, MAXNUMVCTRS
	  xx1 = vx(kset,ic,j)
    	  yy1 = vy(kset,ic,j)
	  if(xx1 .eq. SIGPENUP .and. yy1 .eq. SIGPENUP) goto 700
	  if (move) then
		x1=nint(xsizrot(xx1,yy1,cs,angle)+x0b)
		y1=nint(ysizrot(xx1,yy1,cs,angle)+y0b)
		call place(x1, y1)
		move = .false.
	  else
	        if (xx1 .eq. SIGPENUP) then
		   move = .true.
		else
      		   x1=nint(xsizrot(xx1,yy1,cs,angle)+x0b)
      		   y1=nint(ysizrot(xx1,yy1,cs,angle)+y0b)
		   call vctr (x1, y1)
		   move = .false.
		endif
	  endif
600       continue
700	 continue
750	continue
c
      if (spaces) then
	widthchar = cs*XDEFAULTS
      else
	widthchar = cs*(abs(vx(kset,ic,1)) + abs(vy(kset,ic,1)))
      endif
      hghtchar = cs*YDEFAULTS
c
      return
c
      end
c
c----------------------------------------------------
c
      subroutine pchar(string, nchar)
c
c     Draws characters horizontally on the graphics screen
c
      integer*2 string(*), nchar
c
      integer*2 ioutstring(40), i, ilen, n80, igtmp, iwpc
      real oldangle, x0, y0
      character*80 outstring
      logical dodevice
c
      include 'foxplot.inc'
      include 'cio.inc'
c
      equivalence (ioutstring, outstring)
c
      parameter (n80 = 80)
c
      ilen = min(nchar,n80)
      if (ilen .le. 0) then
	ilen = 1
	outstring = ' '
      else
	call copy(iwpc(ilen), string, ioutstring)
      endif
c     Removes unprintable characters and stores results in OUTSTRING
c
      oldangle = angle
      call rotlet(0.)
      x0 = xpos
      y0 = ypos
c			if this is a TEK or v102 device, try and use
c			device characters if possible
      dodevice = (csize .eq. 1) .and. (nset .eq. 1) .and. 
     .           ((igraphtype .eq. 3) .or. (igraphtype .eq. 4))
      if (dodevice) then
c			turn off normal graphics device, remembering type
         igtmp = igraphtype
         igraphtype = 1
c			do the tek graphics
         call tekchar(outstring, ilen)
c			go on [to do the hardcopy if needed]
      endif
c
      do 200 i = 1, ilen
        call charpl(outstring(i:i), x0, y0)
        x0 = x0 + widthchar
200	continue
      xpos = nint(x0)
      ypos = nint(y0)
      call rotlet(oldangle)
c			reset igraphtype if dodevice
      if (dodevice) igraphtype = igtmp
c
      return
      end
c
c--------------------------------------------------------
c
      subroutine vchar(string, nchar, rotate)
c
c     Draws characters vertically on the graphics screen.
c     If rotate = true, the characters will be rotated through -90 deg.
c	and the string will be printed from the bottom up.
c
      integer*2 string(*), nchar
      logical rotate
c
      integer*2 ioutstring(40), i, ilen, n80, iwpc
      real oldangle, pi, x0, y0
      character*80 outstring
c
      include 'foxplot.inc'
c
      equivalence (ioutstring, outstring)
c
      parameter (n80 = 80)
      parameter (pi = 90./57.29577951)
c
      ilen = min(nchar,n80)
      if (ilen .le. 0) then
	ilen = 1
	outstring = ' '
      else
	call copy(iwpc(ilen), string, ioutstring)
      endif
c     Removes unprintable characters and stores results in OUTSTRING
c
      oldangle = angle
      if (rotate) then
	call rotlet (pi)
      else
	call rotlet(0.)
      endif
      x0 = xpos
      y0 = ypos
      do 200 i = 1, ilen
        call charpl(outstring(i:i), x0, y0)
	if (rotate) then
	    y0 = y0 + widthchar
	else
	    y0 = y0 - hghtchar
	endif
200	continue
      xpos = nint(x0)
      ypos = nint(y0)
      call rotlet(oldangle)
c
      return
      end
c
c--------------------------------
c
      integer*2 function searchi2(i2num, i2array, num)
c
c     Searches through I2ARRAY(NUM) for a value that matches
c     I2NUM.  Returns the index in the I2ARRAY where this happens
c     or zero if no element of I2ARRAY matches I2NUM.
c
      integer*2, i2num, i2array(*), num, i
c
      do 10 i = 1, num
	if (i2array(i) .eq. i2num) then
	   searchi2 = i
	   return
	endif
10	continue
c
      searchi2 = 0
c
      return
      end
