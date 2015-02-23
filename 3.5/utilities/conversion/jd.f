      subroutine jd(iy, im, id, tjd)
c
c      @(#)jd.f	5.1 06/22/94
c
c     Calculates Julian date from date
c
c     iy, im, id = (I*4) Year, month, and day
c     tjd = (R*8) julian date generated 
c
      double precision tjd, ttjd, yy, rmm, a, b 
      integer*4 iy, im, id, ijd
c
      if (im .gt. 2) then
	yy = iy
	rmm = im
      else
	yy = iy -1
	rmm = im+12
      endif
c
      if (iy .gt. 1582 .or. (iy .eq. 1582 .and. im .gt. 10) .or.
     1    (iy .eq. 1582 .and. im .eq. 10 .and. id .ge. 15)  ) then
	a = int(yy/100.)
	b = 2 - a+int(a/4.)
      else
	b = 0.
      endif
c
      ijd = 365.25*yy
      ttjd = ijd + int(30.6001*(rmm+1)) + id + 1720994.5d0 + b
      tjd = ttjd
c
      return 
      end
c
