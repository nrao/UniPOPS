      subroutine cvtmodieee(cinbuff, ccvtbuff, type, ntype, num)
c
c     @(#)cvtmodieee.f	5.1 06/22/94
c
c     Converts from Modcomp to IEEE binary representation
c
      character*(*) cinbuff, ccvtbuff
      character*1 type(*)
      integer*4 ntype(*), num, j, l
c
      character*1 null
      integer*4 kin, kcvt
c
      data null /z'00'/
c
      kin = 1
      kcvt = 1
c     kin, kcvt = position to work in next within cinbuff, ccvtbuff
c
      do 90 j = 1, num
		do 70 l = 1, ntype(j)
		   if (type(j) .eq. 'i') then
			ccvtbuff(kcvt:kcvt+1) = cinbuff(kin:kin+1)
			kin = kin + 2
			kcvt = kcvt + 2
		   else if (type(j) .eq. 'b') then
			kin = kin + 1
		   else if (type(j) .eq. 's') then
			ccvtbuff(kcvt:kcvt) = null
			kcvt = kcvt + 1
		   else if (type(j) .eq. 'r') then
			call cvtmodflt(cinbuff(kin:kin+3),
     1				        ccvtbuff(kcvt:kcvt+3) )
			kcvt = kcvt + 4
			kin = kin + 4
		   else if (type(j) .eq. 'd') then
			call cvtmoddbl(cinbuff(kin:kin+5), 
     1					ccvtbuff(kcvt:kcvt+7) )
			kcvt = kcvt + 8
			kin = kin + 6
		   else if (type(j) .eq. 'c') then
			ccvtbuff(kcvt:kcvt) = cinbuff(kin:kin)
			kin = kin + 1
			kcvt = kcvt + 1
		   endif
c
70		   continue
c
90	     continue
c
      return
c
      end
c
