      subroutine cvtieeeflt(cieee, cmod)
c
c     @(#)cvtieeeflt.f	5.1 06/22/94
c
c     Converts IEEE single precision floating point into Modcomp 
c     floating point.
c     cieee = (C*4) input IEEE real
c     cmod = (C*4) output modcomp real
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
      character*4 cieee, cmod
c
      character*4 cin, cout, cnulls
      integer*4 nulls, iin, iout, isign, ir2, iexp, ifrac, iexpmod,
     .		ifracmod
      real*4 rin, rout, frac, fracmod
      real*8 two22, two23
c
      equivalence (cin, rin),(cout,rout),(cin,iin),(nulls,cnulls),
     1		  (rout,iout)
c
      data nulls /z'00000000'/, two22/4194304.d0/, two23/8388608.d0/
c
      cin = cieee
c
      if (rin .eq. 0.0) then
	cmod = cnulls
      else
        isign = rshift(iin,31)
        ir2 = iin - lshift(isign,31)
        iexp = rshift(ir2,23)
        ifrac = ir2 - lshift(iexp,23)
        frac = 1. + dble(ifrac) / two23
c
        iexpmod = iexp + 130
        fracmod = frac / 2. 
        ifracmod = fracmod * two22
c
        iout = or(lshift(iexpmod,22),ifracmod)
        if(isign.ne.0) iout = not(iout) + 1
        cmod = cout
      endif
c
      return
c
      end
