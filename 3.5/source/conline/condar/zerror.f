      subroutine zerror (z, isize, jsize, undefwp, ierr)
c
c     @(#)zerror.f	5.1 06/22/94
c
c     To check COMMON / MAPPL / and the z array specifications for 
c     parameter errors.
c
c     z (isize, jsize) = (R*4) Array to be contoured
c     isize = (I*4) First dimension limit of z
c     jsize = (I*4) Second dimension limit of z
c     undefwp = (R*4) Undefined value in z
c     ierr = (I*4) Returns errors as sum of the following
c		1: IMIN < 1
c		2: IMAX-IMIN < 2
c		4: IMAX > ISIZE
c		8: JMIN < 1
c		16: JMAX-JMIN < 2
c		32: JMAX > JSIZE
c		256: Number of LEVS < 1
c		512: LEVS not in ascending order
c		1024: All LEVS < minimum of z array
c		2048: All LEVS > maximum of z array
c
c     Note: Errors 1 - 32 are major errors
c
c     Caution: COMMON /MAPPL/ must be set
c
c
      integer*4 isize, jsize
      integer*2 ierr, i, maxlev, ixmin, ixmax, iymin, iymax
      real z(isize,jsize), zmin, zmax, cmin, cmax, undefwp
c
      include 'mappl.inc'
      include 'appl.inc'
      include 'core.inc'
c
      real zmin, zmax
c
      ierr = 0
c
      if (imin .lt. 1) ierr = ierr + 1
c
      if (imax - imin .lt. 1) ierr = ierr + 2
c
      if (imax .gt. isize) ierr = ierr + 4
c
      if (jmin .lt. 1) ierr = ierr + 8
c
      if (jmax - jmin .lt. 1) ierr = ierr + 16
c
      if (jmax .gt. jsize) ierr = ierr + 32
c
      if (levs(1) .le. -999999) ierr = ierr + 256
c
      do 60 i = 1, maxregn-1
	maxlev = i
	if (levs(i+1) .le. -999999) goto 70
	if (levs(i) .gt. levs(i+1) ) then
	   ierr = ierr + 512
           goto 70
	endif
60	continue
      maxlev = maxregn
c
70    if (imin .ge. 1 .and. jmin .ge. 1 .and. jmax .le. jsize .and.
     .	  imax .le. isize) then
          call zlimit(z, isize, jsize, zmin, ixmin, iymin, zmax, ixmax,
     .		      iymax, undefwp)
	  cmin = levs(1)
	  cmax = levs(1)
          do 100 i = 2, maxlev
	     cmin = min(cmin, levs(i))
	     cmax = max(cmax, levs(i))
100	     continue
          if (cmax .lt. zmin) ierr = ierr + 1024
          if (cmin .gt. zmax) ierr = ierr + 2048
	endif
c
      return
c
      end
c
