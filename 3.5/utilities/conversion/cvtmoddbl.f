      subroutine cvtmoddbl( cmod, cieee)
c
c     @(#)cvtmoddbl.f	5.1 06/22/94
c
c     Converts input R*6 Modcomp word into its equivalent 8 byte 
c     IEEE word.  
c
c     cieee = (C*8) output IEEE real
c     cmod = (C*6) input modcomp character string equivalent to modcomp
c		6-byte double precision word
c
c     Modcomp double precision contains a sign bit, a 9 bit exponent, and
c     a 38 bit mantissa.  For positive values, the representation is
c     F * 2**(exp-256) where the exponent ranges from 0 to 511 and F is
c     between 0.5 and 1.  For negative values, the representation is the
c     two's complement of the absolute value.
c
c     IEEE double precision contains a sign bit, 11 bit exponent, and a
c     52 bit mantissa.  The representation is 1.F * 2**(exp-1023) where the
c     exponent ranges from 0 to 2047 and F from 0. to 0.9999999.
c
c
      character*8 cieee
      character*6 cmod
c
      dimension ir(2)
      character*8 cdr
      real*8 two16, two20, two22, two32, two38, dr, frac, fracieee
      integer*4 izerosgn, ir, isign, ir2, iexp, ifrac2, ifrac1, 
     .		iexpieee, ifracieee1, ifracieee2
c
      equivalence (dr, ir(1)),(cdr, dr)
c
      data two20/1048576./,two32/4294967296./,izerosgn/z'7fffffff'/,
     1     two16/65536.d0/, two22/4194304.d0/, two38/274877906944.d0/
c
      cdr = cmod
c
      isign = rshift(ir(1),31)
      ir(2) = rshift(ir(2),16)
      if(isign.ne.0) then
	if(ir(2).eq.0) then
	   ir(1)=not(ir(1))+1
        else
	   ir(2) = not(ir(2)) + 1
           ir(1) = not(ir(1))
	endif
      endif
      ir2 = and(ir(1),izerosgn)
      iexp = rshift(ir2, 22) 
      ifrac1 = ir2 - lshift(iexp, 22)
      ifrac2 = ir(2)
      if (ifrac2 .lt. 0) ifrac2 = ifrac2 + two16 + 0.5
      frac = dble(ifrac1)/two22 + dble(ifrac2)/two38
c     Get the correct power of two for the exponent and the mantissa
c
      if(iexp.eq. 0 .and. frac.eq.0) then
	iexpieee=0
	fracieee=0.
      else
        iexpieee = iexp + 766
        fracieee = frac * 2.d0 - 1.
      endif
      ifracieee1 = fracieee*two20
      ifracieee2 = (fracieee*two20 - ifracieee1)*two16
c     Find IEEE exponent and mantissa.  Calculate the two integer word
c     representation of the mantissa.  
c
      ir(1) = or(lshift(isign,31),or(lshift(iexpieee,20),ifracieee1))
      ir(2) = lshift(ifracieee2,16)
c     Pack the bits back into dr ( = ir).  
c
      cieee = cdr
c
      return
      end
c
