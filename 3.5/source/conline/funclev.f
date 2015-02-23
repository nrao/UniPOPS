      subroutine funclev (z, isize, jsize, undefwp, func, lmin, lmax,  
     .			  levs, numlevs, ierr) 
c
c     @(#)funclev.f	5.3 09/10/98
c
c     Takes the contents of Matrix Z between IMIN, IMAX and JMIN, JMAX
c     and calculates contour levels LEVS(NUMLEVS) between LMIN and LMAX such 
c     that FUNC(1)/SUMFUNC of the matrix values are between LEVS(1) and (2), 
c     FUNC(2)/SUMFUNC are between LEVS(2) and (3), etc. (where 
c     SUMFUNC = summation of all values in FUNC).
c
c     If all NUMLEVS values of FUNC are equal, LEVS will result in an
c     equal-histogram distribution.
c
c     Z(ISIZE,JSIZE) = Matrix to work on
c     ISIZE,JSIZE = I*2 dimensions of matrix.
c     UNDEFWP = R*4 Undefined value in Matrix
c     FUNC(NUMLEVS) = Real*4 = Describes the percentages that will be in
c		each LEVS.
c     LMIN, LMAX = Real*4 = Min/Max matrix values taht will be covered
c		by LEVS
c     LEVS(NUMLEVS) = Real*4 = Output array containing levels
c     NUMLEVS = Integer*2 = number of levels
c     IERR = I*2 = return error code (0 if successful)
c
      real func(*), levs(*), lmin, lmax, z(isize, jsize), undefwp
      integer*2 numlevs, ierr
      integer*4 isize, jsize
c
      real sumfunc, delta, sum
      integer*2 i, j, ii, iptwh, nxdata
c
      parameter (IPTWH = 1)
c
      include 'mappl.inc'
      include 'mform.inc'
      include 'appl.inc'
c     Note that IMIN,IMAX,JMIN,JMAX must be set in COMMON/APPL/
c
      if (lmin .ge. lmax .or. numlevs .le. 1) then
	ierr = 1
	goto 99
      endif
c
      sumfunc = 0.0
      do 100 i = 1, numlevs
	if (func(i) .lt. 0.0) then
	    ierr = 3
	    goto 99
	endif
	sumfunc = sumfunc + func(i)
100	continue
      sumfunc = sumfunc * (numlevs - 1) / numlevs
c
      if (numlevs .eq. 2) then
	levs(1) = lmin
	levs(2) = lmax
	goto 99
      endif
c     Take care of degenerate cases
c
      do 150 i = 1, MAX_DATA_POINTS
	xdata(i) = 0.0
150	continue
c
      nxdata = 0
      delta = (lmax - lmin) / (MAX_DATA_POINTS - 1)
      do 190 i = imin, imax
	do 180 j = jmin, jmax
	    if (z(i,j) .ne. undefwp) then
	       ii = nint( (z(i,j)-lmin)/delta + 1.)
	       if (ii .gt. 0 .and. ii .le. MAX_DATA_POINTS) then
		   xdata(ii) = xdata(ii) + 1.
	           nxdata = nxdata + 1
	       endif
	    endif
180	    continue
190	continue
c     XDATA will contain the distribution of values in the matrix
c     to a gridding of DELTA.  NXDATA pixels are used from the matrix.
c
      if (nxdata .eq. 0) then
	ierr = 5
	goto 99
      endif
c     Watch out if NO pixel values are between LMIN and LMAX.
c 
      sumfunc = sumfunc/nxdata
      sum = 0.0
      levs(1) = lmin
      levs(numlevs) = lmax
      j = 2
      do 200 i = 1, MAX_DATA_POINTS
	sum = sum + xdata(i)
	if ( j .lt. numlevs .and. sum .ge. func(j)/sumfunc ) then
		levs(j) = float (i-1)*delta + lmin
		if (levs(j) .eq. levs(j-1)) levs(j) = levs(j) + delta/2.
		sum = sum - func(j)/sumfunc
		j = j + 1
	endif
200     continue
c
      if (j .ne. numlevs) then
	ierr = 4
	goto 99
      endif
c
99    return
      end
c
