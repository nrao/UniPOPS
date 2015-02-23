      subroutine conversion3(input, output, ier)
C-------------------------------------------------------------------------------
C  @(#)conversion3.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Converts from internal format of UniPOPS to SDD #1 format
c
      include 'params.inc'
c
      integer*2 maxclass, i, k1, maxclasses, ifirst, j, ilast, i1, i2,
     .		ier
      double precision input(*), output(*)
c
      integer*2 maxperclass(20), iindat(16), ioutdat(16)
      double precision dindat(4), doutdat(4)
c
      equivalence (dindat, iindat), (doutdat, ioutdat)
c
      data maxclass/14/
      data maxperclass/25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
     .                 66, 25, 25, 25,  0,  0,  0,  0,  0,  0/
c     MAXCLASS = max. number of classes
c     MAXPERCLASS = max. number of R*8 entries in each class
c
      ier = 0
      do 10 i = 1, (maxclass+5)/4
	dindat(i) = input(i)
10	continue
c     Move class descriptors into DINDAT
c
      k1 = (maxclass+5)/4 + 1
c     K1 = location in OUTPUT where next class starts
c
      maxclasses = iindat(1)
      if (maxclasses .le. 0 .or. maxclasses .gt. maxclass) then
	ier = -1
	goto 99
      endif
      ioutdat(1) = maxclasses
      ioutdat(2) = (maxclasses+5)/4 + 1
c     Find max number of classes needed, up to above limit and
c     start puting new class descriptors into DOUTDAT
c
      do 100 i = 1, maxclasses
	ifirst = iindat(i+1)
	ilast = ifirst + iindat(i+2)-iindat(i+1) - 1
	if (ifirst .gt. ilast .or. (ilast-ifirst+1) .gt.
     .		maxperclass(i)) then
	    ier = -1
	    goto 99
	endif
	ioutdat(i+2) = ioutdat(i+1) + ilast - ifirst + 1
	if (i .ne. 1) k1 = k1 + maxperclass(i-1)
        do 50 j = ifirst, ilast
	   output(j) = input(j-ifirst+k1)
50	   continue
c	Move header items from INPUT to OUTPUT at their correct locations
c
100	continue
c
      do 200 i = maxclasses+1, maxclass+1
	k1 = k1 + maxperclass(i-1)
200	continue
c	Bump k1 pointer to end of header
c
      do 250 i = 1, (maxclasses+5)/4
	output(i) = doutdat(i)
250	continue
c	Move class descriptors in DOUTDAT back to OUTPUT
c
      output( (maxclasses+5)/4 + 1) = (ioutdat(maxclasses+2) - 1) * 8
c     Changes the Header Length in the OUTPUT array to its new value.
c
      i1 = iindat(maxclasses+2)
      i2 = i1 + input( (maxclasses+5)/4 + 2)/8 - 1
      if (mod( nint(input( (maxclasses+5)/4+2)),8) .ne. 0) i2 = i2 + 1
c     Find where the data is in INPUT
c
      if (i1 .gt. i2 .or. i1 .le. 0 .or. 
     .     (i2 - i1 + 1) .gt. MAX_DATA_POINTS) then
	ier = -1
	goto 99
      endif
      do 300 i = i1, i2
	output(i) = input(i-i1+k1)
300	continue
c	Move the data from INPUT to OUTPUT
c
99    return
      end
c
