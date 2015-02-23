      subroutine checkdate(iyr,imth,idy,ihr,imin,isec)
c
c     @(#)checkdate.f	5.1 06/22/94
c
c     Checks and corrects year, month, day, hour, min, and sec are within 
c     the correct limits
c
c     iyr, imth, idy, ihr, im, is = (I*4) year, month, day, hour, min, sec
c
      integer*4 iyr, imth, idy, ihr, imin, isec, imonth(12)
      logical correct
c
      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
c
1     correct = .true.
c
      if ( (iyr .le. 1582 .and. mod(iyr,4) .eq. 0) .or. (iyr .gt. 1582 
     1     .and. mod(iyr,100) .ne. 0 .and. mod(iyr,4) .eq. 0) ) 
     2     imonth(2) = 29
c     Checks for whether it is leap year
c
      if (isec .gt. 59) then
	imin = imin + 1
	isec = isec - 60
        correct = .false.
      endif
c
      if (isec .lt. 0) then
	imin = imin - 1
	isec = isec + 60
        correct = .false.
      endif
c
      if (imin .gt. 59) then
	ihr = ihr + 1
	imin = imin - 60
        correct = .false.
      endif
c
      if (imin .lt. 0) then
	ihr = ihr - 1 
	imin = imin + 60
        correct = .false.
      endif
c
      if (ihr .gt. 23) then
	idy = idy + 1
	ihr = ihr - 24
        correct = .false.
      endif
c
      if (ihr .lt. 0) then
	idy = idy - 1
	ihr = ihr + 24
        correct = .false.
      endif
c
      if (imth .gt. 12) then
	iyr = iyr + (imth-1)/12
	imth = mod(imth-1,12) + 1
	correct = .false.
      endif
c
      if (imth .le. 0) then
	iyr = iyr + (imth-12)/12
	imth = mod(imth-12,12) + 12
        correct = .false.
      endif
c
      if (idy .gt. imonth(imth) ) then
	idy = idy - imonth(imth)
	imth = imth + 1
	correct = .false.
      endif
c
      if (idy .le. 0) then
	if (imth .eq. 1) imth = 13
	idy = idy + imonth(imth-1)
	imth = imth - 1
      endif
c
      if (.not. correct) goto 1
c
      return
c
      end
