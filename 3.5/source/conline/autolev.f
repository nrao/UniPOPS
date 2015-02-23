      subroutine autolev(zmin, zmax, numlevs, z1, z2, zdiff)
c
c     @(#)autolev.f	5.1 06/22/94
c
c     From ZMIN, ZMAX, and NUMLEVS, calculates reasonable values
c     for conmtour levels.
c
c     ZMIN, ZMAX = (R*4) Input Max and min values that are to be spanned
c     NUMLEVS = (I*2)Input  Number of levels  desired
c     Z1, Z2 = (R*4) Output Min, Max values that 'look' nice and span the data
c     ZDIFF = (R*4) Output 'nice' delta between levels
c
      real zmin, zmax, z1, z2, zdiff
      integer*2 numlevs
c
      integer*4 i, NUMTEST, nshift, nlevs
c
      parameter (NUMTEST=57)
c
      real r1, r2, adx, zexpon, test(NUMTEST), ztemp
c
      data test/1.00, 1.05, 1.10, 1.15, 1.20, 1.25, 1.30, 1.35, 1.40, 
     .		1.45, 1.50, 1.55, 1.60, 1.65, 1.70, 1.75, 1.80, 1.90,
     .	        2.00, 2.10, 2.20, 2.30, 2.40, 2.50, 2.60, 2.70, 2.80,
     .		2.90, 
     .		3.00, 3.10, 3.25, 3.3333333, 3.50, 3.6666666, 3.75, 3.90,
     .		4.00, 4.20, 4.25, 4.50, 4.75, 
     .		5.00, 5.25, 5.50, 5.75,
     .		6.00, 6.3333333, 6.6666666,
     .		7.00, 7.3333333, 7.6666666,
     . 		8.00, 8.3333333, 8.6666666,
     .		9.00, 9.50, 10.00 /
c     Values for TEST must go from 1 to 10 and must be separated by
c     the percentage of what you want as the maximum percentage of 
c     levels that go beyond the edge of the data.  5% is probably OK,
c     so test(i+1) should be approximately 1.05*test(i).
c
      r1 = min(zmin,zmax)
      r2 = max(zmin,zmax)
      nlevs = numlevs
c
c     First, let's take care of some degenerate cases
c
      if (nlevs .le. 0) then
	zdiff = 0
	z1 = r1
	z2 = r2
	return
      endif
c
      if (nlevs .eq. 1) then
	z1 = (r2+r1)/2.
	zdiff = 0
	z2 = z1
	return
      endif
c
      if (r1 .eq. r2) then
	r2 = r1 + abs(r1)
	r1 = r1 - abs(r1)
      endif
c
9     zdiff = (r2-r1)/(nlevs-1)
c     ZDIFF will always be positive
c
      zexpon = 10.**(int(log10(zdiff)))
      adx = zdiff / zexpon
10    if (adx .lt. 1.0) then
	adx = adx*10.
	zexpon = zexpon/10.
	goto 10
      endif
11    if (adx .gt. 10) then
	adx = adx/10.
	zexpon = zexpon*10.
	goto 11
      endif
c     ADX should fall between 1 and 10
c
      do 20 i = 1, NUMTEST
	   if (adx .le. test(i)) goto 25
20	   continue
      i = NUMTEST
c
25    zdiff = test(i)*zexpon
c     We now have a 'nice' value for zdiff
c
      z1 = zdiff * anint(r1 / zdiff)
26    if (z1 .gt. r1) then
	z1 = z1 - zdiff
	goto 26
      endif
c     Cannot let Z1 be larger tha the minimum bound so shift Z1 down
c
      z2 = z1 + (numlevs-1)*zdiff
      if (z2 .lt. r2) then
	i = i + 1
	if (i .gt. NUMTEST) then
	   i = 2
	   zexpon = zexpon*10
	endif
	goto 25
      endif
c     Cannot let Z2 be smaller than the maximum bound.  Must increase
c     the value of ZDIFF by using the next larger value from TEST.
c
      nshift = ( nint((r1-z1)/zdiff) - nint((z2-r2)/zdiff)  )/2
      z1 = z1 + nshift*zdiff
      z2 = z2 + nshift*zdiff
c     Now, balance the contour levels around the data so an equal
c     number go above and below the range in data.
c     NOTE:  In none of my tests was NSHIFT anything else but zero
c     		so we may want to delete this snippet of code.
c
      if (zmin .gt. zmax) then
	   zdiff = - zdiff
	   ztemp = z2
	   z2 = z1
	   z1 = ztemp
      endif
c     Flip the data if upper and lower bounds were switched on input.
c
      return
      end
c
