      subroutine pltdots (z, isize, jsize, type, undefwp)
c
c     @(#)pltdots.f	5.1 06/22/94
c
c     Draws a symbol wherever in array z there is a defined or
c     undefined data point.
c
c     z (isize, jsize) = (R*4) Array to be gray-scaled.
c     isize = (I*4) First dimension limit of z.
c     jsize = (I*4) Second dimension limit of z.
c     types = (I*2) = 0 or 1 for a dot at undefined points, 2 for +
c		    = -1 for a dot at defined point, -2 for a + 
c     undefwp = (R*4) Undefined data value
c
c     Only works on the area of the plot defined by (imin,imax), (jmin,jmax)
c     (set in COMMON / MAPPL / ).
c
      integer*4 isize, jsize
      integer*2 type, curcur
      real*4 z(isize, jsize), undefwp
c
      include 'mappl.inc'
      include 'appl.inc'
c
      integer*4 i, j
      integer*2 xl0, yb0, ipixel, sclc
      integer*2 m3, n1, n5, n120
c
      data m3, n1, n5, n120 /-3, 1, 5, 120/
c       
      sclc(ipixel) = max(n1, nint(sclchar*float(ipixel))) 
c
      if (isize .lt. 0 .or. jsize .lt. 0) 
     .		call oerror(n120, m3, 'PLTDOTS: bad array dimensions')
c
      do 800 i = imin, imax
	 do 700 j = jmin, jmax
c
	   if ( (z(i,j) .ne. undefwp .and. type .lt. 0) .or. 
     .		(z(i,j) .eq. undefwp .and. type .ge. 0) ) then
c
	     xl0 = nint(float(i)*ax(curcur())+bx(curcur()))
	     yb0 = nint(float(j)*ay(curcur())+by(curcur()))
c
	     if (abs(type) .le. 1) then
		call place(xl0,yb0)
		call vctr(xl0,yb0)
c    		Draw a dot
c
	     else
c
      		CALL PLACE (xl0-sclc(n5),yb0)
      		CALL VCTR (xl0+sclc(n5),yb0)
      		CALL PLACE (xl0,yb0-sclc(n5))
     		CALL VCTR(xl0,yb0+sclc(n5))
c    		Draw plus sign
c
	     endif
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
