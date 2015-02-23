      SUBROUTINE AU3 (J)
C-------------------------------------------------------------------------------
C  @(#)au3.f	5.2 07/07/94
C-------------------------------------------------------------------------------
c
c     au3 - Baseline, Bmodel, Dcbase, RMS, Bshow, Pcbase, Mdbase
c
C---------------------------------------------------------------------
c
      integer*2 j, iptwh, jj, istart, istop, i, ier, ix, iy, isort, i2,
     .		kk, ip, median, in, inn, md2
      integer*4 I4, mdsize
      integer*2 n0, n80, n120, n225, n237, n242, n297, n298, m3, m2
      integer*2 n25, n780, n1, n33, n268
      logical isfirst, okreal4
      real*4 x0, xd, t, y, sum, total, sum2, xx, zbase, ajj
c
      external qcomphi
c
      PARAMETER (I4 = 4)
c
      INCLUDE 'cform.inc'
      include 'core.inc'
      INCLUDE 'appl.inc'
      INCLUDE 'cio.inc'
      data n0, n80, n120, n225, n237, n242, n297, n298, m3, m2, n33
     .     /0, 80, 120, 225, 237, 242, 297, 298, -3, -2, 33/
      data n25, n780, n1, n268 /25, 780, 1, 268/
C
C=======================================================================
C
      IPTWH=1
c
      JJ=J
      GO TO (10,20,30,40,50,60,70,80), JJ
      call oerror(n120, m3, 'AU3')
C---------------------------------------------------------------------
C							  BASELINE
C---------------------------------------------------------------------
  10  CONTINUE
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'BASELINE')
c
      if (nint(nfit).lt. 0 .or. nint(nfit) .gt. 12.) 
     .		call oerror(n297, m2, 'BASELINE')
c
      CALL PREP (ier)
      if (ier .ne. 0) call oerror(ier, m2, 'BASELINE')
c
      do 15 i = 1, 15
	bparm(i) = 0.0
15	continue
c     Zero out BPARM so that no left over coefficients remain
c
      CALL BSHAPE (ier)
      if (ier .ne. 0) call oerror(n297, m2, 'BASELINE')
      X0=BPARM(14)
      XD=BPARM(15)
      DO 16 JJ=ISTART,ISTOP
         T=float(jj)*XD+X0
         CALL CNPS (Y,T,BPARM,IRES)
         if (okreal4(TWH(JJ,IPTWH))) TWH(JJ,IPTWH)=TWH(JJ,IPTWH)-Y
16       continue
      GO TO 99
C---------------------------------------------------------------------
C							  BMODEL
C---------------------------------------------------------------------
  20  CONTINUE
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'BMODEL')
c
      X0=BPARM(14)
      XD=BPARM(15)
      DO 26 JJ=ISTART,ISTOP
	    T=float(jj)*XD+X0
	    CALL CNPS (Y,T,BPARM,IRES)
	    TWH(JJ,IPTWH)=Y
26          continue
      GO TO 99
C---------------------------------------------------------------------
C							  BSHAPE
C---------------------------------------------------------------------
  30  CONTINUE
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'BSHAPE')
c
      if (nint(nfit).lt. 0 .or. nint(nfit) .gt. 12.) 
     .		call oerror(n297, m2, 'BSHAPE')
c
      CALL PREP (ier)
      if (ier .ne. 0) call oerror(ier, m2, 'BSHAPE')
c
      do 31 i = 1, 15
	bparm(i) = 0.0
31	continue
c     Zero out BPARM so that no left over coefficients remain
c
      CALL BSHAPE (ier)
      if (ier .ne. 0) call oerror(n297, m2, 'BSHAPE')
      GO TO 99
C---------------------------------------------------------------------
C							  DCBASE
C---------------------------------------------------------------------
  40  CONTINUE
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'DCBASE')
c
      CALL PREP (ier)
      if (ier .ne. 0) call oerror(ier, m2, 'DCBASE')
c
      SUM=0.0
      DO 44 I=1,IBASE
  	    SUM=SUM+YDATA(I)
44	    continue
      SUM=SUM/IBASE
      DO 46 JJ=ISTART,ISTOP
  	    if(okreal4(TWH(JJ,IPTWH))) TWH(JJ,IPTWH)=TWH(JJ,IPTWH)-SUM
46	    continue
      GO TO 99
C---------------------------------------------------------------------
C							  RMS
C---------------------------------------------------------------------
  50  CONTINUE
c
      CALL PREP(ier)
      if (ier .ne. 0) call oerror(ier, m2, 'RMS')
c
      TOTAL=FLOAT(IBASE)
      SUM=0.0
      SUM2=0.0 
      DO 54 I=1,IBASE
	    SUM=SUM+YDATA(I)
  	    SUM2=SUM2+YDATA(I)*YDATA(I)
54	    continue
      SUM=SUM2-SUM*SUM/TOTAL
      VRMS=SQRT(SUM/(TOTAL-1.0))
      WRITE (CPUF,2000,IOSTAT=IER) VRMS
      CALL PWRITE (IPUF,n80)
 2000 FORMAT(1X,'RMS VALUE',F8.4)
      GO TO 99
C-------------------------------------------------------------------
C                                          BSHOW
C--------------------------------------------------------------------
   60 CONTINUE
      if (showplot(numplots) .ne. 0) call oerror(n298, m2, 'BSHOW')
c
      ISTART = DTWH (C12SPN,IPTWH)
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'BSHOW')
c
      X0=BPARM(14)
      XD=BPARM(15)
      isfirst = .true.
      DO 66 ix = ix0, ixm, 2
         ajj = (float(ix)-bx(numplots))/ax(numplots)
         T=(ajj+float(idatoff))*XD+X0
         CALL CNPS (Y,T,BPARM,IRES)
         IY=min(max(iy0,nint(AY(numplots)*Y+BY(numplots))), iym)
         if (nint(ajj) .ge. isbg) then
           if (isfirst) then 
	      call placewp(ix, iy, n33)
              isfirst  = .false.
           else
	      CALL vctrwp(IX, IY, n33, sclchar, 0.)
           endif
         endif
   66 CONTINUE
      call place(n0, n780 - n25*(inline-n1))
      call pchar('',n0)
      GO TO 99
C-----------------------------------------------------------------------
C        PCBASE                                          
C-----------------------------------------------------------------------
   70 CONTINUE
c
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'PCBASE')
c
      if (dcpct .le. 0 .or. dcpct .ge. 100) 
     .		call oerror(n237, m2, 'PCBASE')
c
      CALL PREP (ier)
      if (ier .ne. 0) call oerror(ier, m2, 'PCBASE')
c
      ISORT=nint(DCPCT*FLOAT(ibase)*0.01)
c
      DO 62 J=1,ISORT
         I2=J+1
         DO 64 KK=I2,ibase
            IF (ydata(J).gt.ydata(KK)) then
              XX=ydata(J)
              ydata(J)=ydata(KK)
              ydata(KK)=XX
	    endif
   64       CONTINUE
   62    CONTINUE
c
      ZBASE=ydata(ISORT)
c
      DO 63 I=ISTART,ISTOP
         if(okreal4(TWH(I,IPTWH))) TWH(I,IPTWH)=TWH(I,IPTWH)-ZBASE
63	 continue
c
      GO TO 99
C--------------------------------------------------------------------
C                        MDBASE             
C--------------------------------------------------------------------
  80  CONTINUE
c
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'MDBASE')
c
      mdsize = mdbox
      IF (MOD(mdsize,2).ne.1 .or. mdsize.le.0 .or. 
     .	  mdsize.gt.dtwh(c12ni,iptwh)) call oerror(n242, m2, 'MDBASE')
C
      do 801 i = istart, istop
	if (.not.okreal4(twh(i,iptwh))) 
     .      call oerror(n268, m2, "MDBASE: Cannot proceed")
801     continue
c
      in = istop - istart + 1
      inn = 2*in
      median = mdsize/2 + 1
      md2 = 2*mdsize
c
      call copy(inn, twh(istart,iptwh), ydata)
c     Copy to YDATA the original data; YDATA never gets altered.
c
c     Do the first few (MDSIZE) channels by....
      call copy(md2, ydata(1), work(1))
      call qsort(work, mdsize, I4, qcomphi)
c     Copying 1st MDSIZE data points from YDATA to WORK, sort WORK....
      do 815 ip = 1, median
	xdata(ip) = ydata(ip) - work(median)
815	continue
c     Subtract the median value from the first MDSIZE data points and
c     store results in xdata.
c     
c     Now, the central part of the array
      do 840 ip = median+1, in - mdsize/2
	call copy(md2, ydata(ip-median+1), work(1))
	call qsort(work, mdsize, I4, qcomphi)
	xdata(ip) = ydata(ip) - work(median)
840	continue
c
c     The final MDSIZE data points...  We've already sorted the last
c     MDSIZE data points during the last execution of the above loop.
c
      do 845 ip = in - mdsize/2 + 1, in
	xdata(ip) = ydata(ip) - work(median)
845	continue
c
      call copy(inn, xdata, twh(istart,iptwh))
c     Copy XDATA back to the original array.
c
      GO TO 99
C
  99  CONTINUE
      RETURN
      END

