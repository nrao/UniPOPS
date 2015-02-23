      logical function lchkdate(iyr,imth,iday)
c
c     @(#)lchkdate.f	5.1 06/22/94
c
c     Returns a TRUE if the date make sense
c
c     iyr, imth, ... = (I*4) date
c
      integer*4 iyr, imth, iday
c
      integer*2 imonth(12)
c
      data imonth/31,28,31,30,31,30,31,31,30,31,30,31/
c
      lchkdate = .true.
c
      if ( (iyr .le. 1582 .and. mod(iyr,4) .eq. 0) .or. (iyr .gt. 1582 
     1     .and. mod(iyr,100) .ne. 0 .and. mod(iyr,4) .eq. 0) ) then
           imonth(2) = 29
      else
	   imonth(2) = 28
      endif
c     Checks for whether it is leap year
c
      if (imth .le. 0 .or. imth .gt. 12 .or. iday .le. 0 .or. 
     1    iday .gt. imonth(imth)) lchkdate = .false.
c
      return
      end
c
