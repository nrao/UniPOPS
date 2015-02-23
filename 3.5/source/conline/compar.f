      integer*2 function compar(el1, el2)
c
c     @(#)compar.f	5.1 06/22/94
c
c     Function used by QSORT to sort an array
c
      character*10 el1, el2
c
      if (lgt(el1, el2)) then
	compar = 1
      else
	compar = -1
      endif
c
      return
      end
c
