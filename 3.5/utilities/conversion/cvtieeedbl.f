      subroutine cvtieeedbl(cieee, cmod)
c
c     @(#)cvtieeedbl.f	5.1 06/22/94
c
c     Converts input R*8 IEEE word into its equivalent 6 byte 
c     Modcomp word.  
c
c     cieee = (C*8) input IEEE real
c     cmod = (C*6) output modcomp character string equivalent to modcomp
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
      character*6 cdr, cnulls
      integer*4 nulls(2), ir, isign, ir2, iexp, ifrac2, ifrac1, itemp,
     .		iexpmod, ifracmod1, ifracmod2
      real*8 dr, frac, two16, two20, two22, two36, fracmod
c
      equivalence (dr, ir(1)),(cdr, dr),(cnulls,nulls)
c
      data nulls/z'00000000',z'00000000'/, two20/1048576.d0/,
     1     two16/65536.d0/, two22/4194304.d0/, two36/68719476736.d0/
c
      cdr = cieee
c
      if (dr .eq. 0.0d0) then
	cmod = cnulls(1:6)
      else
        isign = rshift(ir(1),31)
        ir2 = ir(1) - lshift(isign, 31)
        iexp = rshift(ir2, 20)  
        ifrac1 = ir2 - lshift(iexp, 20)
        ifrac2 = rshift(ir(2), 16)
        if (ifrac2 .lt. 0) ifrac2 = ifrac2 + two16 + 0.5
        frac = 1.d0 + dble(ifrac1)/two20 + dble(ifrac2)/two36
c       Get the correct power of two for the exponent and the mantissa
c
        iexpmod = iexp - 766
        fracmod = frac / 2.d0
        if (iexpmod.gt.511) then
	   iexpmod=511
	   fracmod = 0.99999999999
        else if(iexpmod.lt.0) then
	   iexpmod=0
	   fracmod=0.
        endif
        ifracmod1 = fracmod*two22
        ifracmod2 = (fracmod*two22 - ifracmod1)*two16
c       Find Modcomp exponent and mantissa.  Calculate the two integer word
c       representation of the mantissa.  
c
        ir(1) = or(lshift(iexpmod,22),ifracmod1)
        ir(2) = lshift(ifracmod2,16)
c       Pack the bits back into dr ( = ir).  Second half of ir(2) is zeros
c
        if (isign.ne.0) then
  	  itemp = ir(2)
	  ir(2) = not(ir(2)) + 1
	  ir(1) = not(ir(1))
	  if (itemp .eq. 0) ir(1)=ir(1)+1
c         Checks if all the low order bits were off before the two's complement
c         If they were, then a bit must be added to the complement of the high
c         order bits.
c
        endif
c       If negative, take two's complement of result
c
        cmod = cdr(1:6)
c
      endif
c
      return
      end
c
