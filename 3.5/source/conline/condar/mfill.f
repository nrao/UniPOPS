      subroutine mfill (z, z2, isize, jsize, irad, undefwp)
c
c     @(#)mfill.f	5.1 06/22/94
c
c     To fill in undefined areas of the z-array.  This routine finds 
c     points in the z-array that are undefined and if there are any
c     defined points within irad of it in the x and y directions and
c     in a direction at 45 degrees from these axis.  
c
c     z(isize, jsize) = (R*4) Input array.
c     z2(isize, jsize) = (R*4) Output array.
c     isize = (I*4) First dimension limit of z
c     jsize = (I*4) Second dimension limit of z
c     irad = (I*2) Distance in units of array steps within which to look
c                  for defined points to average (irad >= 1).
c     undefwp = (R*4) Undefined data value
c
c     Caution:  IMIN, IMAX, JMIN, JMAX must be set in COMMON /MAPPL/
c		MFILL only works on that part of the array defined by
c		IMIN, IMAX, JMIN, and JMAX.
c
      integer*4 isize, jsize
      integer*2 irad, m2, n112
      real z(isize, jsize), z2(isize,jsize), undefwp
c
      include 'mappl.inc'
c
      real sq2, weight, sum 
      integer*4 i, j, k, l
c
      data m2, n112 / -2, 112/
c
      if (irad .le. 0) call oerror(n112, m2, 'MFILL: distance < 1')
c
      sq2 = sqrt(2.)
c
      do 600 i = imin, imax
	do 500 j = jmin, jmax
c
	   z2(i,j) = z(i,j)
	   if(z(i,j) .eq. undefwp) then
c
		weight = 0.
		sum = 0.
c
		do 150 l = -1, 1, 2
		   do 100 k = l, l*irad, l
		      if (k+i .gt. imax .or. k+i .lt. imin) goto 150
		      if (z(k+i, j) .ne. undefwp) then
			   sum = sum + z(k+i, j) / float(iabs(k))
			   weight = weight + 1./float(iabs(k))
			   goto 150
		      endif
100		      continue
150		   continue
c		Looks along X direction
c
		do 250 l = -1, 1, 2
		   do 200 k = l, l*irad, l
		      if (k+j .gt. jmax .or. k+j .lt. jmin) goto 250
		      if (z(i, k+j) .ne. undefwp) then
			   sum = sum + z(i, k+j) / float(iabs(k))
			   weight = weight + 1./ float(iabs(k))
			   goto 250
		      endif
200		      continue
250		   continue
c		Looks along Y direction
c
		do 350 l = -1, 1, 2
		   do 300 k = l, l*irad, l
		      if (k+i .gt. imax .or. k+i .lt. imin) goto 350
		      if (k+j .gt. jmax .or. k+j .lt. jmin) goto 350
		      if (z(k+i, k+j) .ne. undefwp) then
			   sum = sum + z(k+i, k+j) / abs(sq2*float(k))
			   weight = weight + 1./ abs(sq2*float(k))
			   goto 350
		      endif
300		      continue
350		   continue
c		Looks along one diagnal
c
		do 450 l = -1, 1, 2
		   do 400 k = l, l*irad, l
		      if (k+i .gt. imax .or. k+i .lt. imin) goto 450
		      if (k-j .gt. jmax .or. k-j .lt. jmin) goto 450
		      if (z(k+i, k-j) .ne. undefwp) then
			   sum = sum + z(k+i, k-j) / abs(sq2*float(k))
			   weight = weight + 1./ abs(sq2*float(k))
			   goto 450
		      endif
400		      continue
450		   continue
c		Looks along other diagnal
c
		if (weight .gt. 0.0) z2(i,j) = sum / weight
c		Update interpolated value
c
	   endif
c
500	   continue
600	continue
c   
      return
c
      end
