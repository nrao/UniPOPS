c
c @(#)qcomp.f	5.1 06/22/94
c
c 	comparison functions needed by qsort calls, these compare two
c	real*4 numbers.  qcomphi is used if the higher value is preferred
c	while qcomplow is used if the lower value is preferred.
c       qsort is a sun fortran quick sort routine.
c
      integer*2 function qcomphi(val1, val2)
c
      real*4 val1, val2
c
      if (val1 .gt. val2) then
         qcomphi = -1
      else if (val2 .gt. val1) then
         qcomphi = 1
      else
         qcomphi = 0
      endif
c
      return
c
      end
c
      integer*2 function qcomplow(val1, val2)
c
      real*4 val1, val2
c
      if (val1 .lt. val2) then
         qcomplow = -1
      else if (val2 .lt. val1) then
         qcomplow = 1
      else
         qcomplow = 0
      endif
c
      return
c
      end
