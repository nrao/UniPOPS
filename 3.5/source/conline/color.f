      integer*2 function color(huered, shdgrn, satblu)
c
c     @(#)color.f	5.2 09/23/94
c
c     Allows for the changing of colors on the graphics screen.  Only
c     128 different colors are allowed.   If COLORS is called twice with
c     the same parameters, the old color plane is used and a new one is
c     not assigned.  
c
c     huered = (R*4) Hue specification (0 <= ihuered <= 63), or, if < 0,
c		       Red specification (-16 <= ihuered <= -1).
c     shdgrn = (R*4) Shade or lightness specification (0 <= ishdgrn <= 8),
c			 or, if ihuered < 0, Green specification (-16 <=
c			 ishdgrn <= -1).
c     satblu = (R*4) Saturation specification (0 <= isatblu <= 6), or, if
c			ihuered < 0, Blue specification (-16 <= isatblu <=
c			-1).
c
c     COLOR returns RED*16*16 + GREEN*16 + BLUE (0 - 4095 range).
c
c     CAUTION: If huered < 0, then the absolute values of huered, 
c	       shdgrn, and satblu must be between 1 and 16.  If 
c	       huered >= 0, then the absolute values of huered must 
c	       be <= 63, shdgrn <= 8, and satblu <= 6.
c
c     NOTE: The default color is white.
c
      include 'cio.inc'
c
      real*4 huered, shdgrn, satblu
c
      integer*4 idxarray(64,9,7), idx, ihurd, ishgr, istbl
      integer*2 ilen, lastblnk, ierr
      integer*2 n261, n352, n357, m1, m2
      character*1023 filename
      logical first
c
      data first/.true./
      data n261, n352, n357, m1, m2 /261, 352, 357, -1, -2/
c
      save first, idxarray
c
      if (first) then
	ilen = lastblnk(dirprefix(1))
      	filename = dirprefix(1)(1:ilen) // '/sunbin/ihstorgb'
c
      	open(unit=iiotmp,file=filename,access='sequential',
     .		form='unformatted',status='old',iostat=ierr)
c
      	if (ierr .ne. 0) 
     .		call oerror(n352, m1, 'IHSTORGB file')
c
        rewind(iiotmp,iostat=ierr)
        read(iiotmp,iostat=ierr) idxarray
      	if (ierr .ne. 0) 
     .		call oerror(n357, m1, 'IHSTORGB file')
c
	close(iiotmp,iostat=ierr)
        first = .false.
      endif
c     Reads in the table that allows one to convert from IHS color system to
c	RGB.
c
      if (huered .ge. 0.) then
        ihurd = nint(abs(huered))
        ishgr = nint(abs(shdgrn))
        istbl = nint(abs(satblu))
      	if (ihurd .gt. 63) call oerror(n261, m2, ' ')
      	if (ishgr .gt. 8) call oerror(n261, m2, ' ')
      	if (istbl .gt. 6) call oerror(n261, m2, ' ')
	idx = idxarray(ihurd+1, ishgr+1, istbl+1)
	ihurd = and(rshift(idx,16),255) + 1
	ishgr = and(rshift(idx,8),255) + 1
	istbl = and(idx,255) + 1
      else
	ihurd = nint(abs(huered)*17.-16.)
	ishgr = nint(abs(shdgrn)*17.-16.)
	istbl = nint(abs(satblu)*17.-16.)
	if (ihurd .gt. 256 .or. ihurd .le. 0) call oerror(n261, m2, ' ')
	if (ishgr .gt. 256 .or. ishgr .le. 0) call oerror(n261, m2, ' ')
	if (istbl .gt. 256 .or. istbl .le. 0) call oerror(n261, m2, ' ')
        idx = (ihurd-1)*256*256 + (ishgr-1)*256 + (istbl-1)
      endif
c     Check inputs for IHS and RGB system; IDX = coded RGB color;
c	IHURD, ISHGR, and ISTBL are now between 1 and 256 and are the RGB
c	values of IDX.   
c
      if (igraphtype .eq. 2) then
	call corecolor(idx)
      else if (igraphtype .eq. 1)  then
	call nogcolor(idx)
      else if (igraphtype .gt. 0) then
	call othercolor(idx)
      endif
      if(iprinttype .eq. 1) then
	call noghardcolor(idx)
      else if (iprinttype .eq. 3) then
        call posthardcolor(idx)
      else if (iprinttype .gt. 1) then
        call othercolor(idx)
      endif
c
      color = nint((float(ihurd)-1.)/17.)*256 + 
     .	      nint((float(ishgr)-1.)/17.)*16 + 
     .	      nint((float(istbl)-1.)/17.)
c     COLOR will be the approximate color (last 4 bits of each color
c	has been lost).
c
      return
      end
c
c-----------------------------------------------
      subroutine revertcolor
c
c     Reverts the color to the currently defined color
c
      include 'cio.inc'
      include 'appl.inc'
c
      integer*4 IDXBLACK
      integer*2 color, IDXWHITE
      real*4 red, green, blue
c
      parameter (IDXBLACK=0)
      parameter (IDXWHITE = 4095)
c
      red = -(and(rshift(idx,8),15) + 1)
      green = -(and(rshift(idx,4),15) + 1)
      blue = -(and(idx,15) + 1)
c
      if (color(red, green, blue) .eq. IDXWHITE) then
        if(iprinttype .eq. 1) then
	  call noghardcolor(IDXBLACK)
        else if (iprinttype .eq. 3) then
          call posthardcolor(IDXBLACK)
        else if (iprinttype .gt. 1) then
          call othercolor(IDXBLACK)
       endif
      endif
c     Reset printers to black if screens are white
c
      return
      end
c
c-----------------------------------------------
      subroutine resetcolor
c
c     Resets the color to the default color (white for screen, 
c     black for printers)
c
      include 'appl.inc'
      include 'cio.inc'
c
      integer*4 IDXBLACK
      integer*2 color
      real*4 RM16
c
      parameter (RM16 = -16.)
      parameter (IDXBLACK = 0)
c
      idx = color(RM16, RM16, RM16)
c
      if(iprinttype .eq. 1) then
	  call noghardcolor(IDXBLACK)
      else if (iprinttype .eq. 3) then
          call posthardcolor(IDXBLACK)
      else if (iprinttype .gt. 1) then
          call othercolor(IDXBLACK)
      endif
c
      return
      end
c

