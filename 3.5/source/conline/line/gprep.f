      SUBROUTINE GPREP (ierr) 
C-------------------------------------------------------------------------------
C  @(#)gprep.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
      integer*2 iptwh, istart, ieg, ibg, j, istop, i, is, ie, ierr
      integer*2 greg(48), gregtmp(48)
      integer*4 nseg, tseg, xtmp, jj
      logical okreal4
c
      INCLUDE 'cform.inc'
      include 'core.inc'
      INCLUDE 'appl.inc'
C
C=======================================================================
C
      IPTWH = 1
      IBASE=0
      ierr = 0
c
      istart = dtwh(c12spn, iptwh)
      istop = istart + dtwh(c12ni, iptwh) - 1
      if (istart .ge. istop) then
	ierr = 225
	goto 99
      endif
c
      do 160 i = 1,maxmrk*4
         greg(i) = 0
         gregtmp(i) = 0
 160  continue
c
      IF (gregon(1) .eq. 0.) then
         IEG=nint(EGAUSS)
         IBG=nint(BGAUSS)
         if (ieg.gt.istop .or. ibg.lt.istart .or. ibg.ge.ieg) then
	   ierr = 285
	   goto 99
	 endif
         DO 10 J = IBG,IEG
	    if (okreal4(TWH(J+idatoff,IPTWH))) then
			IBASE=IBASE+1
	        	XDATA(IBASE)=J + idatoff
	        	YDATA(IBASE)=TWH(J+idatoff,IPTWH) 
	    endif
   10       CONTINUE
      else 
c			how man regions are there, order each one to greg
        nseg = 0
        do 180 i = 1, maxmrk*4, 2
           if (gregon(i).ne.0.) then
              nseg = nseg + 1
              if (gregon(i+1) .lt. gregon(i)) then
                 greg(nseg*2-1) = nint(gregon(i+1))
                 greg(nseg*2) = nint(gregon(i))
              else if (gregon(i+1) .gt. gregon(i)) then
                 greg(nseg*2-1) = nint(gregon(i))
                 greg(nseg*2) = nint(gregon(i+1))
              else
                 ierr = 286
                 goto 99
              endif
              if (greg(nseg*2-1) .lt. istart .or. 
     .            greg(nseg*2) .gt. istop .or.
     .            greg(nseg*2-1) .eq. greg(nseg*2)) then
                 ierr = 286
                 goto 99
              endif
           endif
 180    continue
c			sort the sections so we can check for overlap
        gregtmp(1) = greg(1)
        gregtmp(2) = greg(2)
        tseg = 1
        if (nseg .ge. 2) then
           do 181 i = 2, nseg
              xtmp = greg(i*2-1)
              do 182 j = 1, tseg
                 if (xtmp .lt. gregtmp(j*2-1)) goto 183
 182          continue
c			it belongs at location j, insert it
 183          if (tseg .ge. j) then
                 do 184 jj = tseg, j, -1
                    gregtmp(jj*2+1) = gregtmp(jj*2-1)
                    gregtmp(jj*2+2) = gregtmp(jj*2)
 184             continue
              endif
              gregtmp(j*2-1) = greg(i*2-1)
              gregtmp(j*2) = greg(i*2)
              tseg = tseg + 1
 181       continue
        endif
c			check for overlap
        do 185 i = 2, nseg*2
           if (gregtmp(i) .le. gregtmp(i-1)) then
              ierr = 286
              goto 99
           endif
 185    continue
c			and finally, use them
        do 18 i = 1, maxmrk*4, 2
	   IF (gregtmp(i).ne.0.0) then
	     is = gregtmp(i)
	     ie = gregtmp(i+1)
	     if (is.lt.istart .or. ie.gt.istop .or. is.ge.ie) then
		ierr = 286
		goto 99
	     endif
	     DO 19 J=IS,IE
	        if (okreal4(TWH(J+idatoff,IPTWH))) then
			IBASE=IBASE+1
	        	XDATA(IBASE)=J + idatoff
	        	YDATA(IBASE)=TWH(J+idatoff,IPTWH) 
		endif
   19	        CONTINUE
c
	   endif
  18	   CONTINUE
      endif
c
  99  CONTINUE
c
      if (ibase .eq. 0) ierr = 268
c
      RETURN
      END
