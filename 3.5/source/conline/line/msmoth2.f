      subroutine msmoth2 (c, z, z2, isize, jsize, undefwp)
c
c     @(#)msmoth2.f	5.1 06/22/94
c
c     MSMOTH2 is similar to MSMOOTH except that each z point is adjusted by a
c     factor of its eight neighboring z points.  This should be used in 
c     preference to MSMOOTH when more smoothing is desired.
c
c     c = (R*4) Smoothing factor (see below)
c     z (isize, jsize) = (R*4) Input Array.
c     z2 (isize, jsize) = (R*4) Output array
c     isize = (I*4) First dimension limit of z
c     jsize = (I*4) Second dimension limit of z
c     undefwp = (R*4) Undefined data value
c
c     Note: The Z array is smoothed by the following:
c     z(i,j) = c*z(i,j) + (1-c)*(z(i-1,j-1) + z(i-1,j) + z(i-1,j+1) + z(i,j-1)
c                         + z(i,j+1) + z(i+1,j-1) + z(i+1,j) + z(i+1,,j+1))/8
c     where each neighboring z must be defined.  (Each new z is equal to
c     c times the old z plus (1-c) times the average of its eight neighboring
c     z's.)  A smaller z results in more smoothing.  Subsequent call to MSMOTH2
c     will result in even more smotthing.
c
c     Caution:  IMIN, IMAX, JMIN, JMAX must be set in COMMON /MAPPL/
c		MSMOTH2 only works on that part of the array defined by
c		IMIN, IMAX, JMIN, and JMAX.
c
c               C must be > 0. but less than 1.
c
      integer*4 isize, jsize
      integer*2 m2, n112
      real z(isize,jsize), c, z2(isize, jsize), undefwp
c
      include 'mappl.inc'
c
      integer*4 i, j
c
      data m2, n112 /-2, 112/
c
      if (c .le. 0. .or. c .ge. 1.) 
     .		call oerror(n112, m2, 'MSMOOTH: Bad smoothing factor')
c
      do 200 i = imin, imax
	do 100 j = jmin, jmax
                z2(i,j) = z(i,j)
		if (i-1 .lt. imin .or. j-1 .lt. jmin .or. 
     .		    i+1 .gt. imax .or. j+1 .gt. jmax) goto 100
		if (z(i,j) .eq. undefwp) goto 100
		if (z(i-1,j-1) .eq. undefwp) goto 100
		if (z(i-1,j) .eq. undefwp) goto 100
		if (z(i-1,j+1) .eq. undefwp) goto 100
		if (z(i,j-1) .eq. undefwp) goto 100
		if (z(i,j+1) .eq. undefwp) goto 100
		if (z(i+1,j-1) .eq. undefwp) goto 100
		if (z(i+1,j) .eq. undefwp) goto 100
		if (z(i+1,j+1) .eq. undefwp) goto 100
		z2(i,j) = c*z(i,j) + (1.-c)*
     1               (z(i-1,j-1) + z(i-1,j) + z(i-1,j+1) + z(i,j-1) +
     2		      z(i,j+1) + z(i+1,j-1) + z(i+1,j) + z(i+1,j+1))/8.
100		continue
200	continue
c
      return
      end
c
c
