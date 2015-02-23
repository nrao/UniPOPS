      subroutine rns (value, mode, string)
c
c     @(#)rns.f	5.1 06/22/94
c
c     Converts 'value' from radians to degrees and divides it into 
c     deg, min, and sec and then places result in 'string'.
c     If 'mode' = 0, then value is divided by 15. before creating 
c                    'string' (i.e., converts degrees into hours).
c
      real*4 value, rad, fact, a, b, c
      integer*2 mode, i, j
      character*2 string(6)
      character*12 sch
      character*1 minus, space, sign
c
      data rad/57.29577951/, space/' '/, minus/'-'/
c
      fact = rad
      if(mode.eq.0) fact = fact / 15.
c
      a = abs(value * fact)
      b = 60. * amod (a, 1.)
      c = 60. * amod (b, 1.)
c
      sign = space
      if(value.lt.0.) sign = minus
      write(unit=sch, fmt=10) int(a), int(b), c
10    format(' ',i3,' ',i2,' ',f4.1)
c
      if(sch(2:2).ne.space) then
		sch = sign//sch(2:12)
      else if(sch(3:3).ne.space) then
		sch = space//sign//sch(3:12)
      else
		sch = space//space//sign//sch(4:12)
      endif
c
      do 100 i = 1, 6
        j = (i-1)*2 + 1
        string(i) = sch(j:j+1)
100     continue
c
      return
      end
c
c
