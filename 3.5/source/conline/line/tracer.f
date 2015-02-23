       subroutine tracer (z, isize, jsize, undefwp, quick)
c
c      @(#)tracer.f	5.1 06/22/94
c
c      Produces a contour map.  
c
c      z (isize, jsize) = r*4 array of data values to be contoured.
c      isize = i*4 first dimension limit of z
c      jsize = i*4 second dimewnsion limit of z
c      undefwp = r*4 undefined value 
c      quick = true if a quick contour plot is desired.     
c
      integer*4 isize, jsize
      real*4 z(isize, jsize), undefwp
      logical quick
c
      include 'appl.inc'
      include 'mappl.inc'
      include 'core.inc'
c
      real ctr, ep, temp, xa, ya, xb, yb, VX(3),XY(3),YY(3), rl, xl,
     .	     yl, rm, xm, ym, rs, xs, ys, savevec(4,32767), zmin, zmax
      integer*2 ictr, i, j, index, nseg, mindex(3,6), jj, nmaxseg, i2,
     .		ixmin, ixmax, iymin, iymax, curcur
c
      data ep/.001/,nmaxseg/32767/
      DATA MINDEX/1,3,2,3,2,1,3,1,2,2,1,3,1,2,3,2,3,1/
c
c
      call zlimit(z, isize, jsize, zmin, ixmin, iymin, zmax, ixmax, 
     .		  iymax, undefwp)
      ictr = 0
10      ictr = ictr + 1
        if (ictr .gt. maxregn .or. levs(ictr) .le. -999999.) return
        ctr = levs(ictr)
	if (ctr .lt. zmin .or. ctr .gt. zmax) goto 10
c	Find Contour level; return if last level found
c
        nseg = 0
c
        do 399 j = jmin+1, jmax
	  do 390 i = imin, imax-1
	   JJ=1
	   VX(1)=z(i,j)
	   VX(2)=z(i+1,j)
	   VX(3)=z(i,j-1)
	   XY(1)=i
	   XY(2)=i+1
	   XY(3)=i
	   YY(1)=j
	   YY(2)=j
	   YY(3)=j-1
c	    Try one-side of box first (jj=1)
c
   35	   if (vx(1) .eq. undefwp .or. vx(2) .eq. undefwp .or.
     .		vx(3) .eq. undefwp) goto 390
c	   By pass undefined points
c
	   INDEX=0
	   IF(VX(1).GT.VX(2)) INDEX=1
	   IF(VX(3).GE.VX(1)) INDEX=INDEX+2
	   IF(VX(2).GE.VX(3)) INDEX=INDEX+4
	   i2=mindex(1,INDEX)
	   RL=VX(i2)
	   XL=XY(i2)
	   YL=YY(i2)
	   i2=mindex(2,INDEX)
	   RM=VX(i2)
	   XM=XY(i2)
	   YM=YY(i2)
	   i2=mindex(3,INDEX)
	   RS=VX(i2)
	   XS=XY(i2)
	   YS=YY(i2)
c	   Rearrange coords of box corners.
c
	   IF (ctr.le.RL.and.RL-RS.gt.EP*ctr) then
	     IF (ctr.gt.RS) then
	       TEMP=(ctr-RS)/(RL-RS)
	       XA=TEMP*(XL-XS)+XS
	       YA=TEMP*(YL-YS)+YS
	       IF (RL-RM.LE.EP*ctr) then
    	          TEMP=(ctr-RS)/(RM-RS)
	          XB=TEMP*(XM-XS)+XS
	          YB=TEMP*(YM-YS)+YS
	       else IF ((RM-RS.LE.EP*ctr) .or. (ctr.GT.RM)) then
   	          TEMP=(ctr-RM)/(RL-RM)
	          XB=TEMP*(XL-XM)+XM
	          YB=TEMP*(YL-YM)+YM
	       else
    	          TEMP=(ctr-RS)/(RM-RS)
	          XB=TEMP*(XM-XS)+XS
	          YB=TEMP*(YM-YS)+YS
	       endif
c		Find end-points to vector to draw
c
	       if (quick) then
		  call place(nint(xa*ax(curcur())+bx(curcur())),
     .			 nint(ya*ay(curcur())+by(curcur())))
		  call vctr(nint(xb*ax(curcur())+bx(curcur())),
     .			 nint(yb*ay(curcur())+by(curcur())))
c		  Draw vectors without storing them away -- QUICK plot wanted
c
	       else
		  nseg = nseg + 1
	          savevec(1,nseg) = xa
	          savevec(2,nseg) = ya
	          savevec(3,nseg) = xb
	          savevec(4,nseg) = yb
c	          Store away moves and vectores
c
	          if (nseg .eq. nmaxseg) then
		     call drawcntr(savevec, nseg, undefwp,  z, isize, jsize, ictr)
		     nseg = 0
	          endif
c	          If the vector storage array is full, flush the vectors; store
c	          variables away when we look for more vectors.   
c
	       endif
	     endif
	   endif
c
c
   	   IF (JJ.eq.1) then
	      JJ=2
	      VX(1)=z(i+1,j-1)
	      XY(1)=i+1
	      YY(1)=j-1
	      GO TO 35
           endif
c	   Now try other side of box of points (jj=2)
c
390	   continue
399	 continue
c
      if (.not. quick) 
     .	call drawcntr(savevec, nseg, undefwp,  z, isize, jsize, ictr)
c
      goto 10
c
      end
c
c
