      subroutine cvtpcieee(cinbuff, ccvtbuff, type, ntype, num)
c
c     @(#)cvtpcieee.f	5.1 06/22/94
c
c     Converts from IEEE to PC binary representation
c
      character*(*) cinbuff, ccvtbuff
      character*1 type(*)
      integer*4 ntype(*), num
c
      character*1 null
      integer*4 kin, kcvt, j, l
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
			ccvtbuff(kcvt:kcvt) = cinbuff(kin+1:kin+1)
			ccvtbuff(kcvt+1:kcvt+1) = cinbuff(kin:kin)
			kin = kin + 2
			kcvt = kcvt + 2
		   else if (type(j) .eq. 'b') then
			kin = kin + 1
		   else if (type(j) .eq. 's') then
			ccvtbuff(kcvt:kcvt) = null
			kcvt = kcvt + 1
		   else if (type(j) .eq. 'r') then
			ccvtbuff(kcvt:kcvt) = cinbuff(kin+3:kin+3)
			ccvtbuff(kcvt+1:kcvt+1) = cinbuff(kin+2:kin+2)
			ccvtbuff(kcvt+2:kcvt+2) = cinbuff(kin+1:kin+1)
			ccvtbuff(kcvt+3:kcvt+3) = cinbuff(kin:kin)
			kcvt = kcvt + 4
			kin = kin + 4
		   else if (type(j) .eq. 'd') then
			ccvtbuff(kcvt:kcvt) = cinbuff(kin+7:kin+7)
			ccvtbuff(kcvt+1:kcvt+1) = cinbuff(kin+6:kin+6)
			ccvtbuff(kcvt+2:kcvt+2) = cinbuff(kin+5:kin+5)
			ccvtbuff(kcvt+3:kcvt+3) = cinbuff(kin+4:kin+4)
			ccvtbuff(kcvt+4:kcvt+4) = cinbuff(kin+3:kin+3)
			ccvtbuff(kcvt+5:kcvt+5) = cinbuff(kin+2:kin+2)
			ccvtbuff(kcvt+6:kcvt+6) = cinbuff(kin+1:kin+1)
			ccvtbuff(kcvt+7:kcvt+7) = cinbuff(kin:kin)
			kcvt = kcvt + 8
			kin = kin + 8
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
