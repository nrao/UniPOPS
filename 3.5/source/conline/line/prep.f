      SUBROUTINE PREP (ierr)
C-------------------------------------------------------------------------------
C  @(#)prep.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C
      integer*2 iptwh, i, nleft, is, ie, j, ierr, bregtmp(32)
      INTEGER*4 ISTART, ISTOP, nseg, tseg, xtmp, jj
      logical okreal4
c
      INCLUDE 'cform.inc'
      include 'core.inc'
      INCLUDE 'appl.inc'
      include 'cio.inc'
C
C=======================================================================
C
      IPTWH = 1
      ierr = 0
c
      istart = dtwh(c12spn,iptwh)
      istop = istart + dtwh(c12ni,iptwh) - 1
      if (istart .ge. istop) then
	ierr = 225
	goto 99
      endif
c
      do 10 i = 1, maxregn
   10    BREG(I)=0
c
      IF (NREGON(1).eq.0) then
	 nleft = istop-istart-edrop-bdrop+1
	 if (bdrop.lt.0 .or. edrop.lt.0 .or. nleft.le.0) then
	   ierr = 288
	   goto 99
	 endif 
	 nleft = nleft - bbase - ebase
	 if(bbase.lt.0. .or. ebase.lt.0 .or. nleft.lt.0) then
	   ierr = 287
	   goto 99
	 endif 
         BREG(1) = nint(DTWH(C12SPN,IPTWH) + BDROP)
         BREG(2) = BREG(1) + nint(BBASE) - 1
         BREG(4)=nint(DTWH(C12SPN,IPTWH) +  DTWH(C12NI,IPTWH)) - 
     .           EDROP - 1
         BREG(3)=BREG(4)-nint(EBASE) + 1
      else
         nseg = 0
         do 15 i = 1, maxregn, 2
	    if (nregon(i).ne.0.0) then
              nseg = nseg + 1
              IF (NREGON(I+1).lt.NREGON(I)) then
                 breg(nseg*2-1) = nint(nregon(i+1))
                 breg(nseg*2) = nint(nregon(i))
              else if (nregon(i+1) .gt. nregon(i)) then
                 breg(nseg*2-1) = nint(nregon(i))
                 breg(nseg*2) = nint(nregon(i+1))
              else
                 ierr = 233
                 goto 99
	      endif
	      if (breg(nseg*2-1).lt.istart.or.
     .             breg(nseg*2).gt.istop) then
	   	ierr = 233
	   	goto 99
	      endif 
	    endif
15	 continue
      endif
c			the individual regions are correct, now sort
c			the regions so they are increasing order
c			use a bridge hand sort
      bregtmp(1) = breg(1)
      bregtmp(2) = breg(2)
      tseg = 1
      if (nseg .ge. 2) then
         do 160 i = 2, nseg
            xtmp = breg(i*2-1)
            do 161 j = 1, tseg
               if (xtmp .lt. bregtmp(j*2-1)) goto 162
 161        continue
c				it belongs at location j, insert it
 162        if (tseg .ge. j) then
               do 163 jj = tseg, j, -1
                  bregtmp(jj*2+1) = bregtmp(jj*2-1)
                  bregtmp(jj*2+2) = bregtmp(jj*2)
 163           continue
            endif
            bregtmp(j*2-1) = breg(i*2-1)
            bregtmp(j*2) = breg(i*2)
            tseg = tseg + 1
 160     continue
      endif
c			copy it back to breg, checking for overlap
      breg(1) = bregtmp(1)
      do 164 i = 2, nseg*2
         breg(i) = bregtmp(i)
         if (breg(i) .le. breg(i-1)) then
c			an overlap is present, quit
            ierr = 236
            goto 99
         endif
 164  continue
c
      IBASE=0
      do 18 i = 1, maxregn, 2
	 if (breg(i).eq.0) goto 18
	 is = breg(i)
	 ie = breg(i+1) 
	 DO 19 J=IS,IE
	    if (okreal4(TWH(J+idatoff,IPTWH))) then
		IBASE=IBASE+1
	    	XDATA(IBASE)=FLOAT(J + idatoff)
	    	YDATA(IBASE)=TWH(J+idatoff,IPTWH)
	    endif
   19	    CONTINUE
  18	 CONTINUE
c
      goto 999
c
  99  CONTINUE
      do 90 i = 1, maxregn
   90    BREG(I)=0
 999  if (ibase .eq. 0) ierr = 268
      RETURN
      END
