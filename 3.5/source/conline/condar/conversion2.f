      subroutine conversion2(input, output, ier)
C-------------------------------------------------------------------------------
C  @(#)conversion2.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Converts from SDD #1 format to internal format of UniPOPS
c
      include 'params.inc'
      integer*4 maxclass, i, nextout, inclasses, ifirst, j, ilast,
     .		i1, i2, headsize, datalen, noint, spn, 
     .           maxperclass(20), bootsize
      double precision input(*), output(*)
c
      integer*2 iindat(16), ioutdat(16), ier
      double precision dindat(4), doutdat(4)
c
      equivalence (dindat, iindat), (doutdat, ioutdat)
c
      include 'cform.inc'
c
      data maxclass/14/
      data maxperclass/25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
     .                 66, 25, 25, 25,  0,  0,  0,  0,  0,  0/
c     MAXCLASS = max. number of classes
c     MAXPERCLASS = max. number of R*8 entries in each class
c
      ier = 0
      bootsize = (maxclass+5)/4
      do 10 i = 1, bootsize
	dindat(i) = input(i)
10	continue
c     Move class descriptors into DINDAT
c
      nextout = bootsize + 1
c     nextout = location in OUTPUT where next class starts
c
      headsize = input(iindat(2))/8.d0
      if (headsize*8 .ne. nint(input(iindat(2)))) 
     .		headsize = headsize + 1
c     headsize = num. R*8 words in input header
c
      inclasses = min(maxclass,iindat(1))
      ioutdat(1) = inclasses
      ioutdat(2) = (inclasses+5)/4 + 1
c     Find max number of classes needed, up to above limit and
c     start puting new class descriptors into DOUTDAT
c
      do 100 i = 1, inclasses
	ifirst = iindat(i+1)
	if (ifirst .lt. 0) then
	   ier = -1
	   goto 99
	endif
	ilast = ifirst + min(maxperclass(i), iindat(i+2)-ifirst) - 1
        if (i .eq. inclasses .and. ilast .lt. ifirst) then
	    ilast = min(ifirst+maxperclass(i), headsize )
	else if (i .ne. inclasses .and. ilast .lt. ifirst) then
	    ier = -1
	    goto 99
	endif
	ioutdat(i+2) = ioutdat(i+1) + ilast - ifirst + 1
	if (i .ne. 1) nextout = nextout + maxperclass(i-1)
        do 50 j = ifirst, ilast
	   output(j-ifirst+nextout) = input(j)
50	   continue
c	Move header items from INPUT to OUTPUT at their correct locations
c
100	continue
c
      do 200 i = inclasses+1, maxclass
	ioutdat(i+2) = 0
	nextout = nextout + maxperclass(i-1)
200	continue
      nextout = nextout + maxperclass(maxclass)
c     Zero out any unused class descriptors in DOUTDAT.
c     Bump nextout pointer to end of output header.
c
      do 250 i = 1, ioutdat(2) - 1
	output(i) = doutdat(i)
250	continue
c	Move class descriptors in DOUTDAT back to OUTPUT
c
      datalen = output(c1dln)
      if (datalen .gt. MAX_DATA_POINTS * 4) 
     .     datalen = MAX_DATA_POINTS * 4
      if (datalen .lt. 0) datalen = 0
      noint = output(c12ni)
      spn = output(c12spn)
      if (noint .lt. 0) noint = 0
      if (spn .lt. 1) spn = 1
      if (noint + spn - 1 .gt. MAX_DATA_POINTS) then
         if (spn .gt. MAX_DATA_POINTS) spn = MAX_DATA_POINTS
         noint = MAX_DATA_POINTS - spn + 1
      endif
c		make sure header values won't point to invalid array elements
      i1 = output(c1hln)/8 + 1
      i2 = i1 + datalen/8 - 1
      if (mod(datalen, 8) .ne. 0) i2 = i2 + 1
      if (i1 .lt. 0) then
	ier = -1
	goto 99
      endif
c     Find where the data is in INPUT give up if no header info
c
      if (i2 .ge. i1) then
c	Move the data from INPUT to OUTPUT
c		only do this if there really is some data there
c		this isn't strictly necessary, but I think its clearer
         do 300 i = i1, i2
	    output(i-i1+nextout) = input(i)
300	 continue
      endif
c
      output(c1hln) = (nextout - 1) * 8
c     Changes the Header Length in the OUTPUT array to its new value.
c   
99    return
      end
c
