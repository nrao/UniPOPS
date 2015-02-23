      subroutine hlftne (z, isize, jsize, vals, hlevel, nvals, undefwp)
c
c     @(#)hlftne.f	5.1 06/22/94
c
c     Produces a half-tone grayscale image of the array z.
c
c     z (isize, jsize) = (R*4) Array to be gray-scaled.
c     isize = (I*4) First dimension limit of z.
c     jsize = (I*4) Second dimension limit of z.
c     vals (nvals) = (R*4) Array containing the z-values at which a break
c     			   in the gray-scale should occur.
c     hlevel (nvals) = (R*4) Density of gray-scale levels. (Values must 
c			     lie between 0. = empty and 1. = full)
c     nvals = (I*4) Number of gray-scale levels.  
c
c
c     Notes: Vals must be in ascending order.  Ignores any point with an
c    	     undefined value (see UNDEF).  
c
c     If z(i,j) <= vals(1), where i = imin, imax and j =jmin, jmax (set in
c     COMMON / MAPPL / ), then the area on the page occupied by z(i,j) is
c     hlevel(1) filled .  If vals(i) < z(i,j) <= vals(i+1), then the area is 
c     filled by the fraction specified by hlevel(i+1).  If z(i,j) >= vals(nval-1), 
c     then the fraction filled is hlevel(nval).  The value of vals(nval) is not
c     used for anything and is ignored.
c
      integer*4 isize, jsize
      integer*2 nvals
      real*4 z(isize, jsize), vals(nvals), hlevel(nvals), undefwp
c
      include 'mappl.inc'
      include 'appl.inc'
c
      integer*2 nx, ny, ival, k, ixl, iyb, ixr, iyt, curcur
      integer*2 m2, m3, n1, n120, n259, n260
      integer*4 i, j
      real xl0, yb0, xlmin, xlmax, ybmin, ybmax 
c
      data m2, m3, n1, n120, n259, n260
     .     /-2, -3, 1, 120, 259, 260/
c       
      if (nvals .le. 0) 
     .		call oerror(n259,m2, 'HLFTNE: All LEVS < -999999')
      if (isize .lt. 0 .or. jsize .lt. 0) 
     .		call oerror(n120, m3, 'HLFTNE: bad array dimensions')
c
      do 20 k = 1, nvals-1
	if(vals(k) .ge. vals(k+1) ) 
     .		call oerror(n259, m2, 'HLFTNE: Not in ascending order')
20	continue
c
      do 30 k = 1, nvals
	if(hlevel(k) .lt. 0. .or. hlevel(k) .gt. 1.) 
     .		call oerror(n120, m3, 'HLFTNE:  Bad hlevel')
30	continue
c     Check input parameters
c
      nx = nint(ax(curcur()))
      ny = nint(ay(curcur()))
c
      if (nx*ny .gt. 16384) 
     .		call oerror(n260, m2, 'HLFTHE: Box size too big')
c     Calculate size of cell; error if too big
c
      xlmin = float(imin)*ax(curcur())+bx(curcur())
      xlmax = float(imax)*ax(curcur())+bx(curcur())
      ybmin = float(jmin)*ay(curcur())+by(curcur())
      ybmax = float(jmax)*ay(curcur())+by(curcur())
c     Find corners of plot
c
      do 800 i = imin, imax
	 do 700 j = jmin, jmax
c
	   if (z(i,j) .ne. undefwp) then
c
	     do 140 k = 1, nvals-1
			ival = k
			if (z(i,j) .lt. vals(k) ) goto 150
140			continue
	     ival = nvals
c	     Find out what level of grayness is to be used for the
c		current cell.
c
150	     xl0 = float(i)*ax(curcur())+bx(curcur())
	     yb0 = float(j)*ay(curcur())+by(curcur())
	     ixl = nint ( max(xl0-float(nx)/2.,xlmin))
	     iyb = nint ( max(yb0-float(ny)/2.,ybmin))
	     ixr = nint ( min(xl0+float(nx)/2.,xlmax))
	     iyt = nint ( min(yb0+float(ny)/2.,ybmax))
	     call dither(ixl, iyb, ixr - ixl + n1, iyt - iyb + n1, hlevel(ival))
c	     Find pixel at lower left of cell and fill that cell up
c		with appropriate half-tone pattern
c
           endif
c	 
700	   continue
800	 continue
c
      return
c
      end
c
c
