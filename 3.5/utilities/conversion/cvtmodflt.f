      subroutine cvtmodflt(cmod, cieee)
c
c     @(#)cvtmodflt.f	5.1 06/22/94
c
c     Converts Modcomp single precision floating point into IEEE
c     floating point.
c
c     cieee = (C*4) output IEEE real
c     cmod = (C*4) input modcomp real
c
c     Modcomp real contains a sign bit, a 9 bit exponent, and
c     a 22 bit mantissa.  For positive values, the representation is
c     F * 2**(exp-256) where the exponent ranges from 0 to 511 and F is
c     between 0.5 and 1.  For negative values, the representation is the
c     two's complement of the absolute value.
c
c     IEEE real contain a sign bit, 8 bit exponent, and a 23 bit
c     mantissa.  The representation is 1.F * 2**(exp-127) where exp ranges
c     from 0 to 255 and F is between 0. and 0.9999999.  
c
      character*4 cmod, cieee, cin, cout
      real*8 two22, two20, two23
      real*4 frac, rout, rin, fracieee
      integer*4 ifracieee, iexpieee, iout, iin, ifrac, iexp, ir2, 
     .		isign, izerosgn
c
      equivalence (cin, rin),(cout,rout),(cin,iin),(rout,iout)
c
      data two22/4194304.d0/, two20/1048576.d0/, two23/8388608.d0/,
     1     izerosgn/z'7fffffff'/
c
      cin = cmod
c
      isign = rshift(iin,31)
      if(isign.ne.0) iin = not(iin) + 1
      ir2 = and(iin,izerosgn)
      iexp = rshift(ir2,22)
      ifrac = ir2 - lshift(iexp,22)
      frac = dble(ifrac) / two22
c
      iexpieee = iexp - 130
      fracieee = frac * 2.  - 1.
      if(iexpieee.ge.255) then
	iexpieee=254
	ifracieee=0
      else if(iexpieee.le.0) then
	iexpieee=0
	ifracieee=0
      else
        ifracieee = fracieee * two23
      endif
c
      iout = or(lshift(iexpieee,23),ifracieee)
      iout = or(lshift(isign,31),iout)
      cieee = cout
c
      return
c
      end
