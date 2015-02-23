      logical function rearrange(lnew, posit)
c
c     @(#)rearrange.f	5.1 06/22/94
c
c     Rearranges a continuum scan into 1 scan per feed, max of
c     1190 data points. If POSIT is TRUE, then the positions are
c     stored in the last two feeds
c
      logical lnew, posit
c
      character*4 ctempin, ctempout
      real*4 rtempin, rtempout
      integer*4 numfeed, numreal, j, i, j1, jincr, nextpoint, npts
c
      equivalence (rtempin, ctempin), (rtempout, ctempout)
c
      save nextpoint
c
      include 'cvttele.inc'
c
      rearrange = .false.
c
      numfeed = min(icvtbuff(klr),16)
      numreal = numfeed
      if (posit) numfeed = numfeed + 2
c     NUMFEED = number of records to be used in output (if POSIT is true,
c	this includes two records for x and y positions).
c     NUMREAL = number of feeds of real data (NUMREAL = NUMFEED if POSIT
c	is FALSE).
c
      if (lnew) then
c	(i.e., this is a new scan)
c
	do 50 j = 1, numfeed
c
	   do 45 i = 1, lrho
		routbuff(i,j) = rcvtbuff(i)
45		continue
	   do 46 i = lrho+1, outbuffsize/4
		routbuff(i,j) = 0.0
46		continue
c	   Move the header stuff common to all feeds into the output buffers.
c	   Clear out remainder of buffer.
c
	   ioutbuff(kstart,j) = ldbout+1
	   ioutbuff(kmpst,j) = ioutbuff(kstart,j)
c	   Add header info that is not passed in cvt buffers
c
	   if (j .le. numreal) then
	     routbuff(lsno,j) = rcvtbuff(lsno) + float(j)/100.
	     ioutbuff(kfeed,j) = j
	   else 
	     routbuff(lsno,j) = rcvtbuff(lsno) + (99-numfeed+j)/100.
	     ioutbuff(kfeed,j) = 99-numfeed+j
	   endif
c	   Modify scans numbers to be old scan no. + feed no. / 100. or
c	   a false feed number (near 99) if POSIT is true for posiition
c	   records
c
	   if (icvtbuff(klr) .le. 4 .and. j .le. numreal) then
	     routbuff(lrhoout,j) = rcvtbuff(lrho+j)
	     routbuff(ltheout,j) = rcvtbuff(lthe+j)
	     routbuff(lntout,j) = rcvtbuff(lnt+j)
	     if (rcvtbuff(lscle+j) .gt. 0.0) then
		routbuff(lscleout,j) = rcvtbuff(lscle+j)
 	     else
		routbuff(lscleout,j) = 1.0
	     endif
c	     Protect against a bad scale factor.
c
	     routbuff(lstm2out,j) = rcvtbuff(lstm2+j)
	   else
	     routbuff(lrhoout,j) = 0.0
	     routbuff(ltheout,j) = 0.0
	     routbuff(lntout,j) = 1.0
	     routbuff(lscleout,j) = 1.0
	     routbuff(lstm2out,j) = 0.0	  
	   endif   
50	   continue
c       Move over RHO, THETA, Noise Tube, Scale Factor, and System Temp for the
c	individual feeds into their correct locations in the output buffers.  
c	Use default values if number of feed is greater than 4.  
c
	nextpoint = 1
c	NEXTPOINT = the next data point to be written into the output buffers
c
      endif
c
c     For both old and new scans, do the following:
c
      if (nextpoint .le. 1190) then
c	(i.e., there is more room to add another subscan)
c
	npts = min(icvtbuff(kin)+nextpoint-1,1190)
c	NPTS = number of data points in this scan so far. Can't exceed 1190
c
	do 100 j = 1, numreal
c
	   ioutbuff(kstop,j) = npts + ioutbuff(kstart,j) - 1
	   ioutbuff(kmpstp,j) = ioutbuff(kstop,j)
	   ioutbuff(kin,j) = npts
c	   Update some header info
c
	   j1 = kdb + 4 + j
	   jincr= icvtbuff(klr) + 4
c	   J1 = starting I*2 data point in converted buffer
c	   JINCR = number of I*2 words between data points for the same feed.
c		NOTE: the number 4 comes from the 4 i*2 words (or 2 R*4 words)
c		which make up the H and V coordinates attached to each data
c		point.  (See description of telescope tape format.)
c
	    do 90 i = ldbout+nextpoint, ldbout+npts
		routbuff(i,j) = routbuff(lscleout,j)*float(icvtbuff(j1))
		j1 = j1 + jincr
90		continue
100	    continue
c	Scale data by appropriate scale factors and store in output buffer
c	as reals.
c
        if (posit) then
	 do 110 j = numreal+1, numfeed
c
	   ioutbuff(kstop,j) = npts + ioutbuff(kstart,j) - 1
	   ioutbuff(kmpstp,j) = ioutbuff(kstop,j)
	   ioutbuff(kin,j) = npts
c	   Update some header info
c
	   j1 = kdb/2 + (j-numreal)
	   jincr= (icvtbuff(klr) + 1)/2 + 2
c	   J1 = starting point in buffer for positions
c	   JINCR = number of R*4 words between positions words
	    do 109 i = ldbout+nextpoint, ldbout+npts
		rtempin = rcvtbuff(j1)
		call cvtmodflt(ctempin, ctempout)
		routbuff(i,j) = rtempout
		j1 = j1 + jincr
109		continue
110	    continue
	endif
c
        nextpoint = npts + 1
c
      endif
c
      rearrange = .true.
c
      return
      end






c

