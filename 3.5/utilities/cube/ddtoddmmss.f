      subroutine ddtoddmmss(din, ideg, imin, sec)
c
c     @(#)ddtoddmmss.f	5.1 06/22/94
c
c     Converts from DDD.ddddd to DD, MM, SS
c
c     din = (R*8) Angle to be converted
c     ideg, imin, sec = (I*4), (I*4), (R*8) Calculated Deg, min, secs.
c
      double precision din
      integer ideg, imin
      real sec
c
      double precision absdin, rmin
c 
      absdin = abs(din)
      ideg = int(absdin)
      rmin = 60.d0*(absdin - float(ideg))
      imin = int(rmin)
      sec = 60.d0*(rmin - float(imin))
c
      if (abs(sec-60.d0) .lt. 0.001d00) then
	sec = 0.d0
	imin = imin + 1
      endif
      if (imin .ge. 60) then
	imin = imin - 60
	ideg = ideg + 1
      endif
c
      if (din .lt. 0.d0) ideg = -ideg
c
      return
      end
