      subroutine jdtodate(jd, iyear, imonth, day)
C-------------------------------------------------------------------------------
C  @(#)jdtodate.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Converts from JD to Calender date
c     jd = (R*8) Julian date
c     iyear, imonth, day = (I*4) , (I*4), (R*8)
c                    Calculated year, month, and day
c
      double precision jd, day, z, f, a, alpha, b, c, d, e
      integer*4 iyear, imonth
c 
      if (jd .lt. 0) then
	stop
      endif
c
      z = aint(jd + 0.5d0)
      f = mod(jd + 0.5d0, 1.d0)
c
      if (z .lt. 229161.d0) then
	a = z
      else
	alpha = aint ((z - 1867216.25d0) / 36524.25d0)
	a = z + 1.d0 + alpha - aint(alpha / 4.d0)
      endif
c
      b = a + 1524.d0
      c = aint((b - 122.1d0)/365.25d0)
      d = aint(365.25d0*c)
      e = aint((b - d) / 30.6001d0)
c
      day = b - d - aint(30.6001*e) + f
c
      if (e .lt. 13.5) then
	imonth = int(e - 1.d0)
      else
	imonth = int(e - 13d0)
      endif
c
      if (imonth .ge. 3) then
	iyear = int(c - 4716.d0)
      else
	iyear = int(c - 4715.d0)
      endif
c
      return
      end

