      subroutine raster (z, isize, jsize, vals, hues, shds, 
     .			 sats, nval, undefwp)
c
c     @(#)raster.f	5.1 06/22/94
c
c     Color raster graphing of array z.  Only works for the graphics screen.
c
c     z(isize, jsize) = (R*4) Array to be colored in
c     isize = (I*4) First dimension of Z
c     jsize = (I*4) Second dimension of Z
c     vals(nval) = (R*4) Array containing the z-values at which a break
c		   in color should occur
c     hues(nval) = (R*4) Array containing the hues to be given (see below).
c     shds(nval) = (R*4) Array containing the shades to be given
c     sats(nval) = (R*4) Array containing the saturations to be given.
c     nval = (I*4) Number of colors (1 < nval < 255)
c
c     If z(i,j) <= vals(1), where i = imin, imax and j = jmin, jmax (set in
c     COMMON / MAPPL/ ), then the pixels representing the position of
c     z(i, j) are set to the color defined by hues(1), shds(1), sats(1).
c
c     If vals(i) < z(i,j) <= vals(i+1), then the pixels are set to the color
c     defined by hues(i+1), shds(i+1), sats(i+1).
c
c     If z(i,j) > vals(nval - 1) then the color hues(nval), shds(nval), 
c     sats(nval) is used.  The actually value for vals(nval) is never used
c     and can be arbitrary.
c
c     The scheme by which colors are chosen is described in COLOR.  Since
c     RASTER calls COLOR nval times, 127-nval open color slots remain for
c     the user to do the rest of graphing before or after calling RASTER.
c
c     CAUTION: vals(i+1) must be greater than vals(i)
c 
      integer*2 nval 
      integer*4 isize, jsize
      real z(isize, jsize), vals(nval), undefwp, hues(nval), 
     .     shds(nval), sats(nval)
c
      include 'mappl.inc'
      include 'appl.inc'
c
      real xl0, yb0, xlmin, xlmax, ybmin, ybmax
      integer*2 k, i, j, nx, ny, ixl, iyb, ixr, iyt, idm, color, curcur
      integer*2 m3, m2, n120, n259
c
      data m3, m2, n120, n259 /-3, -2, 120, 259/
c
      if (nval .le. 0) 
     .		call oerror(n259,m2, 'RASTER: All LEVS < -999999')
      if (isize .lt. 0 .or. jsize .lt. 0) 
     .		call oerror(n120, m3, 'RASTER: bad array dimensions')
c
      do 20 k = 2, nval-1
	if(vals(k-1) .ge. vals(k) ) 
     .		call oerror(n259, m2, 'RASTER: Not in ascending order')
20	continue
c
      do 30 k = 1, nval
	idm = color(hues(k),shds(k),sats(k))
30	continue
c     Set up color table
c
      nx = nint(ax(curcur()))
      ny = nint(ay(curcur()))
c     Calculate size of cell
c
      xlmin = float(imin)*ax(curcur())+bx(curcur())
      xlmax = float(imax)*ax(curcur())+bx(curcur())
      ybmin = float(jmin)*ay(curcur())+by(curcur())
      ybmax = float(jmax)*ay(curcur())+by(curcur())
c     Find corners of plot
c
      do 500 i = imin, imax
	do 450 j = jmin, jmax
c
	 if (z(i,j) .ne. undefwp) then
c
	   if (z(i,j) .le. vals(1) ) then 
		k = 0
	   else
	        do 300 k = 1, nval-2
	           if (z(i,j) .gt. vals(k) .and. 
     .		       z(i,j) .le. vals(k+1)) goto 400
300	           continue
		k = nval - 1
	   endif
c	   Find out what colors to use for current cell
c
400	   idm = color(hues(k+1), shds(k+1), sats(k+1) )
	   xl0 = float(i)*ax(curcur())+bx(curcur())
	   yb0 = float(j)*ay(curcur())+by(curcur())
	   ixl = nint ( max(xl0-float(nx)/2.,xlmin))
	   iyb = nint ( max(yb0-float(ny)/2.,ybmin))
	   ixr = nint ( min(xl0+float(nx)/2.,xlmax)) + 1
	   iyt = nint ( min(yb0+float(ny)/2.,ybmax)) + 1
	   call box(ixl, iyb, ixr, iyt)
c	   Find pixel at lower left of cell and fill that cell up
c		with appropriate color.
c
	   endif
c
450	 continue
500	continue
c
      call revertcolor
c     Return to the currently defined color
c
      return
c    
      end
c
