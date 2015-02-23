      SUBROUTINE AU2 (J)
C-------------------------------------------------------------------------------
C  @(#)au2.F	5.4 09/10/98
C-------------------------------------------------------------------------------
c
c     au2 - Accum, Sum, Ave, Boxcar, Hanning, Scale, Clip, DIff, Moment,
c            Smooth, Invert, Divide, Spike, Replace, Peak,
c            Bias, Chngres, Crossfcn, Convfcn, Decnvlv, Shift, Fshift,
c	     Vshift, Pdoc, Listshift, Plus, Multiply
c
C-------------------------------------------------------------------------------
c
      INCLUDE 'appl.inc'
c
      INTEGER*2 IPUF(40), j, iptwh, inn, ierr, lxa1, ishift,
     .          istart, istop, n, ier, nchn, num, ntmp
      integer*4 itemp, jj, istart0, istop0, istart1, istop1, lim1, lim2,
     .          lim, i, jj1, ib, ie, ll, i1, i2, ijk, ist, isp, ii,
     .          ibad, iswtch, ilist(4), ii2, in, jcnt
      integer*2 n224, m2, n120, m3, n238, n293, n227, n223, n228, n225,
     .		n288, n222, n292, n235, n80, n249, n251, n252, n253,
     .		n230, n248, n112, m1, n0, n3, n270, n268
      real*4 xscale, x, y, storetemp, wgt(12), 
     .       tmax, tch, hwdth, oldres, temporary(MAX_DATA_POINTS), aa, 
     .	     tvel, rmaxnormal, rminnormal, rinfinity, centchan
      logical*4 invers
      logical okarray, okreal4, first
      real*8 t12m, numaa1, numaa2, mag2, chantox
      CHARACTER*80 CPUF
c
#ifdef CONTINUUM
#else
      real*4 z, f
      integer*2 ifit
      logical useeff, okreal8, setrst
      real*8 xtochan, dinfinity
#endif 
C
      INCLUDE 'cform.inc'
      include 'stk.inc'
      include 'core.inc'
c
      equivalence (ipuf, cpuf)
c
#ifdef CONTINUUM
#else
      save f
#endif
c
      DATA ITEMP /2/
      data t12m/'NRAO 12M'/
      data n224/224/, m2/-2/, n120/120/, m3/-3/, n238/238/, n293/293/,
     .	   n227/227/, n223/223/, n228/228/, n225/225/,  n288/288/, 
     .	   n222/222/, n292/292/, n235/235/, n80/80/,  n249/249/,  
     .	   n251/251/, n252/252/, n253/253/, n230/230/, n248/248/
      data n112/112/, m1/-1/, n0/0/, n3/3/, n270/270/, n268/268/
c
C
C=======================================================================
C
      IPTWH = 1
c
      JJ=J
      GO TO (10,20,30,40,50,60,80,90,100,110,120,
     .	     160,200,210,220,280,290,300,310,320,
     .	     130,230,240,250,260,330,340 ), JJ
      call oerror(n120, m3, 'AU2')
C---------------------------------------------------------------------
C							  ACCUM
C---------------------------------------------------------------------
  10  CONTINUE
c
#ifdef CONTINUUM
      xscale = 1.
#else
      if (dtwh(c12it,iptwh) .le. 0.0 .or.
     1    dtwh(c12sst,iptwh) .le. 0.0) call oerror(n224, m2, 'ACCUM')
c
      setrst = .true.
      useeff = .true.
      if(dtwh(c1tel,iptwh).eq.t12m .or. 
     .   .not. okreal8(dtwh(c12eit,iptwh))) useeff = .false.
c
      if (useeff) then
         XSCALE=DTWH(c12eit,IPTWH)/DTWH(C12SST,IPTWH)**2
      else
         XSCALE=DTWH(c12it,IPTWH)/DTWH(C12SST,IPTWH)**2
      endif
#endif
c
      CALL ACCUMS(XSCALE, ier)
      if (ier .ne. 0) call oerror(ier, m2, 'ACCUM')
c
c
#ifdef CONTINUUM
c
      IF (NACCUM.ne.1) then
        DACCUM(c12it)=DACCUM(c12it) + DTWH(c12it,IPTWH)
      endif
c
#else
c
      IF (NACCUM.ne.1) then
        IF (  abs((DTWH(c12FR,IPTWH)-DACCUM(c12FR))/DACCUM(c12FR)) 
     .        .gt. 1.e-05) call oerror(n238, n0, 'Warning: ACCUM')
        DACCUM(c12it) = DACCUM(c12it) + DTWH(c12it,IPTWH)
        DACCUM(c12SST)=sqrt ( ( (DACCUM(c12wt)-xscale)*DACCUM(c12SST)**2
     .                  + XSCALE*DTWH(c12SST,IPTWH)**2) / daccum(c12wt))
        if (useeff) then
           DACCUM(c12eit) = DACCUM(c12eit) + DTWH(c12eit,IPTWH)
           DACCUM(c12rms) = f * daccum(c12sst) / sqrt( DACCUM(c12eit) )
           if (setrst .and. dtwh(c12rst,iptwh) .ne. 0 .and.
     .         okreal8(dtwh(c12rst,iptwh))) then
              DACCUM(c12rst)=sqrt ( daccum(c12eit)/ 
     .		 ( dtwh(c12eit,iptwh)/(DTWH(c12rst,IPTWH)**2) + 
     .		 (daccum(c12eit)-dtwh(c12eit,iptwh))/(DACCUM(c12rst)**2) ) )
           else
              setrst = .false.
              daccum(c12rst) = dinfinity()
           endif
	else
           DACCUM(c12rms) = f * daccum(c12sst) / sqrt( DACCUM(c12it) )
           if (setrst .and. dtwh(c12rst,iptwh) .ne. 0 .and.
     .         okreal8(dtwh(c12rst,iptwh))) then
       	      DACCUM(c12rst)=sqrt ( daccum(c12it)/ 
     .		 ( dtwh(c12it,iptwh)/(DTWH(c12rst,IPTWH)**2) + 
     .		 (daccum(c12it)-dtwh(c12it,iptwh))/(DACCUM(c12rst)**2) ) )
           else
              setrst = .false.
              daccum(c12rst) = dinfinity()
           endif
	endif
c       Update all necessary header parameters (see memo by R.Maddalena
c	dated Jan 22, 1992).
c
      else
        setrst = okreal8(daccum(c12rst)) .and. daccum(c12rst).ne.0
        if (useeff) then
	   F = dtwh(c12rms,iptwh) *  sqrt( daccum(c12eit) ) / daccum(c12sst)
	else
	   F = dtwh(c12rms,iptwh) *  sqrt( daccum(c12it) ) / daccum(c12sst)
	endif
c	Save away the conversion factor from T/TSYS**2 to rms for later use.
      endif
#endif
c
      GO TO 99
c
C-------------------------------------------------------------------------
C							 SUM
C-------------------------------------------------------------------------
  20  CONTINUE
c
      xscale=weight
c
      if (xscale .le. 0.0) call oerror(n293, m2, 'SUM')
c
      CALL ACCUMS(XSCALE, ier)
      if (ier .ne. 0) call oerror(ier, m2, 'SUM')
c
#ifdef CONTINUUM
      IF (NACCUM.ne.1) then
        DACCUM(c12IT)=DACCUM(c12IT) + DTWH(c12IT,IPTWH)
      endif
#else
      useeff = .true.
      if(dtwh(c1tel,iptwh).eq.t12m .or. 
     .	 .not. okreal8(dtwh(c12eit,iptwh)) ) useeff = .false.
c
      IF (NACCUM.ne.1) then
        IF (  abs((DTWH(c12FR,IPTWH)-DACCUM(c12FR))/DACCUM(c12FR)) 
     .        .gt. 1.e-05) call oerror(n238, n0, 'Warning: SUM')
        DACCUM(c12IT)=DACCUM(c12IT) + DTWH(c12IT,IPTWH)
        DACCUM(c12SST)=sqrt ( ( (DACCUM(c12wt)-xscale)*DACCUM(c12SST)**2
     .                  + XSCALE*DTWH(c12SST,IPTWH)**2) / daccum(c12wt))
        DACCUM(c12rst)=sqrt ( ( (DACCUM(c12wt)-xscale)*DACCUM(c12rst)**2
     .                  + XSCALE*DTWH(c12rst,IPTWH)**2) / daccum(c12wt))
        if (useeff) then
		DACCUM(c12eit) = DACCUM(c12eit) + DTWH(c12eit,IPTWH)
        	DACCUM(c12rms) = f * daccum(c12sst) / sqrt( DACCUM(c12eit) )
	else
        	DACCUM(c12rms) = f * daccum(c12sst) / sqrt( DACCUM(c12it) )
	endif
c
      else
        if (useeff) then
	   F = dtwh(c12rms,iptwh) *  sqrt( daccum(c12eit) ) / daccum(c12sst)
	else
	   F = dtwh(c12rms,iptwh) *  sqrt( daccum(c12it) ) / daccum(c12sst)
	endif
c	Save away the conversion factor from T/TSYS**2 to rms for later use.
      endif
#endif
c
      GO TO 99
c
C---------------------------------------------------------------------
C							  AVE
C---------------------------------------------------------------------
  30  continue
c 
      IF (NACCUM.le.0) call oerror(n227, m2, 'AVE')
c
      call copy2(HDU_FLOAT_SIZE*2, IACCUM(1), itwh(1,iptwh))
c
      NACCUM=0
      weight = 1
c
      GO TO 99
C---------------------------------------------------------------------
C							  BOXCAR
C---------------------------------------------------------------------
  40  CONTINUE
c
      N = nint(NBOX)
      IF (MOD(N,2).eq.0 .or. n.le.0 .or. n.gt.maxmrk)
     .		 call oerror(n228, m2, 'BOXCAR')
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     .        + IDATOFF - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'BOXCAR')
c
      DO 43 I = 1, N
         WGT(I)=1.0/float(n)
   43    CONTINUE
      CALL SMOOTH (TWH(1,IPTWH),ISTART,ISTOP,WGT,N,XDATA)
c
      GO TO 99
C---------------------------------------------------------------------
C							  HANNING
C---------------------------------------------------------------------
  50  CONTINUE
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     .        + IDATOFF - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'HANNING')
c
      WGT(1)=0.25
      WGT(2)=0.5
      WGT(3)=0.25
      N	= 3
      CALL SMOOTH (TWH(1,IPTWH),ISTART,ISTOP,WGT,N,XDATA)
c
      GO TO 99
C---------------------------------------------------------------------
C							   SCALE
C---------------------------------------------------------------------
  60  CONTINUE
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     .        + IDATOFF - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'SCALE')
c
      DO 62 JJ = ISTART,ISTOP
         if (okreal4(TWH(JJ,IPTWH))) TWH(JJ,IPTWH)=TWH(JJ,IPTWH) * FACT
   62    CONTINUE
c
      GO TO 99
C---------------------------------------------------------------------
C							   CLIP
C---------------------------------------------------------------------
  80  CONTINUE
c
      if(bdrop.lt.0.or.edrop.lt.0.or.
     1	   (bdrop+edrop.gt.dtwh(c12ni,iptwh))) 
     .		call oerror(n288, m2, 'CLIP')
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF + bdrop
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     1       + IDATOFF - 1 - edrop
c
      if (istart .gt. istop) call oerror(n225, m2, 'CLIP')
c
      DO 82 JJ = ISTART,ISTOP
         if (okreal4(TWH(JJ,IPTWH))) then
         	IF (TWH(JJ,IPTWH).GT.CPMAX) TWH(JJ,IPTWH)=CPMAX
         	IF (TWH(JJ,IPTWH).LT.CPMIN) TWH(JJ,IPTWH)=CPMIN
	 endif
   82    CONTINUE
c
      GO TO 99
C-----------------------------------------------------------------------
C                                       DIFF and MINUS                
C-----------------------------------------------------------------------
 90   CONTINUE
c
      istart0 = DTWH(C12SPN,IPTWH) + IDATOFF
      istop0 = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     1        + IDATOFF - 1
      istart1 = DTWH(C12SPN,itemp) + IDATOFF
      istop1 = DTWH(C12SPN,itemp) + DTWH(C12NI,itemp)
     1        + IDATOFF - 1
c
      if (istart0 .gt. istop0 .or. istart1 .gt. istop1) 
     .		call oerror(n222, m2, 'DIFF')
c
      DO 920 JJ = istart0, istop0
	 jj1 = jj - istart0 + istart1
         if (jj1 .le. istop1 .and. jj1 .ge. istart1) then
            if (okreal4(TWH(JJ,IPTWH)).and.okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=TWH(JJ,IPTWH)-TWH(jj1,ITEMP)
	    else if(defmode .ge. 0.5 .and. okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=-TWH(jj1,ITEMP)
	    else if (defmode .lt. 0.5) then
		twh(jj,iptwh)=rinfinity()
	    endif
	 endif	
920	 continue
c
      GO TO 99
C---------------------------------------------------------------------
C							    MOMENT
C---------------------------------------------------------------------
 100  CONTINUE
c
      IB = BWIND
      IE = EWIND
c
      if (bwind .le. 0.0 .or. bwind .ge. ewind .or.
     1    ewind .gt. (dtwh(c12ni,iptwh)+dtwh(c12spn,iptwh)-1) .or.
     1    bwind .lt. dtwh(c12spn,iptwh)) 
     1   call oerror(n292, m2, 'MOMENT')
c
      X=0.0
      Y=0.0
      first = .true.
c
      lxa1 = lxa
      if (.not. okarray(lxa, dtwh(1,iptwh)) ) then
         call oerror(n270, n0, 
     .		'MOMENT: Units of X-axis will be channels or samples')
	 lxa1 = 1
      endif
c
#ifdef CONTINUUM
      DO 102 JJ = IB,IE
	 if (okreal4(twh(jj+idatoff,iptwh))) then
	    x = x + twh(jj+idatoff,iptwh)
	    y = y + twh(jj+idatoff,iptwh) * jj
	 else if (first) then
	     call oerror(n268, n0, "MOMENT: Skipping points")
	     first = .false.
	 endif
  102    CONTINUE
      size(1) = x * dtwh(c12dx, iptwh)
      size(2) = chantox(lxa1, dtwh(1,iptwh), y, ierr) / x
#else
      DO 102 JJ = IB,IE
	 if (okreal4(twh(jj+idatoff,iptwh))) then
	    z = dabs(chantox(lxa1, dtwh(1,iptwh), float(jj)-0.5, ierr) -
     .		     chantox(lxa1, dtwh(1,iptwh), float(jj)+0.5, ierr))
	    x = x + twh(jj+idatoff,iptwh) * z
	    y = y + twh(jj+idatoff,iptwh) * 
     1	        chantox(lxa1, dtwh(1,iptwh), float(jj), ierr) * z
	 else if (first) then
	     call oerror(n268, n0, "MOMENT: Skipping points")
	     first = .false.
	 endif
  102    CONTINUE
c
      size(1) = x
      size(2) = y / x
#endif
c
      GO TO 99
C---------------------------------------------------------------------
C							    SMOOTH
C---------------------------------------------------------------------
 110  CONTINUE
c
      N = nint(SMWGT(1))
      IF (N.gt.maxmrk-1 .or. n.le.0) call oerror(n235, m2, 'SMOOTH')
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     .        + IDATOFF - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'SMOOTH')
c
      CALL SMOOTH (TWH(1,IPTWH),ISTART,ISTOP,SMWGT(2),N,XDATA)
c
      GO TO 99
C---------------------------------------------------------------------
C							    INVERT
C---------------------------------------------------------------------
 120  CONTINUE
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     .        + IDATOFF - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'INVERT')
c
      DO 121 I = ISTART, istart + (istop-istart+1)/2 - 1
         storetemp = TWH(I,IPTWH)
	 twh(i,iptwh) = twh(istop-i+istart,iptwh)
	 twh(istop-i+istart,iptwh) = storetemp
121      continue
c
      dtwh(c12dx,iptwh) = -dtwh(c12dx,iptwh)
      dtwh(c12rp,iptwh) = dtwh(c12ni,iptwh) - dtwh(c12rp,iptwh) + 
     .			   dtwh(c12spn,iptwh)
#ifdef CONTINUUM
      dtwh(c3srt,iptwh) = -dtwh(c3srt,iptwh)
#else
      dtwh(c12fr,iptwh) = -dtwh(c12fr,iptwh)
#endif
c
      GO TO 99
C---------------------------------------------------------------------
C					DIVIDE
C---------------------------------------------------------------------
 160  CONTINUE
c
      istart0 = DTWH(C12SPN,IPTWH) + IDATOFF
      istop0 = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     1        + IDATOFF - 1
      istart1 = DTWH(C12SPN,itemp) + IDATOFF
      istop1 = DTWH(C12SPN,itemp) + DTWH(C12NI,itemp)
     1        + IDATOFF - 1
c
      if (istart0 .gt. istop0 .or. istart1 .gt. istop1) 
     .    call oerror(n222, m2, 'DIVIDE')
c
      DO 161 JJ = istart0, istop0
	 jj1 = jj - istart0 + istart1
         if (jj1 .le. istop1 .and. jj1 .ge. istart1) then
            if (okreal4(TWH(JJ,IPTWH)).and.okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=TWH(JJ,IPTWH)/TWH(jj1,ITEMP)
	    else if(defmode .ge. 0.5 .and. okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=1./TWH(jj1,ITEMP)
	    else if (defmode .lt. 0.5) then
		twh(jj,iptwh)=rinfinity()
	    endif
	 endif
  161    CONTINUE
c
      GO TO 99
C----------------------------------------------------------------------
C                                      SPIKE   
C----------------------------------------------------------------------
  200 CONTINUE
c
      if(bdrop.lt.0.or.edrop.lt.0.or.
     1	   (bdrop+edrop.gt.dtwh(c12ni,iptwh))) 
     .		call oerror(n288, m2, 'SPIKE')
c
      ISTART=DTWH(C12SPN,IPTWH)+IDATOFF+BDROP
      ISTOP=DTWH(C12SPN,IPTWH)+DTWH(C12NI,IPTWH)
     .      + IDATOFF - 1 - EDROP
c
      if (istart .gt. istop) call oerror(n225, m2, 'SPIKE')
c
      LL=0
      DO 201 I = 1,maxmrk
         BADPT(I)=0.
201      continue
      DO 203 JJ=ISTART,ISTOP
         IF (okreal4(TWH(JJ,IPTWH)) .and. 
     .	     ABS(TWH(JJ,IPTWH)). ge. NSIGMA ) then
           LL=LL+1
           BADPT(LL)=JJ-IDATOFF
           IF (LL.EQ.maxmrk) GO TO 205
	 endif
  203    CONTINUE
c
  205 CONTINUE
c
      do 2081 i1 = 1, ll, 12
	i2 = min(ll, i1 + 11)
        WRITE (CPUF,1000,IOSTAT=IER) (nint(badpt(Ijk)),Ijk= i1, i2)
 1000 FORMAT ('BAD POINTS:',12I5)
        CALL PWRITE (IPUF,n80)
2081    continue
c
      GO TO 99
C----------------------------------------------------------------------
C                                    REPLACE   
C----------------------------------------------------------------------
  210 CONTINUE
c
       if(bdrop.lt.0.or.edrop.lt.0.or.
     1	   (bdrop+edrop.gt.dtwh(c12ni,iptwh))) 
     .		call oerror(n288, m2, 'REPLACE')
c
      IST=DTWH(C12SPN,IPTWH)+IDATOFF + bdrop
      ISP=DTWH(C12SPN,IPTWH)+DTWH(C12NI,IPTWH)
     .        + IDATOFF - 1 - edrop
c
      if (ist .gt. isp) call oerror(n225, m2, 'REPLACE')
c
      first = .true.
      DO 212 II = 1,maxmrk
         ibad=nint(BADPT(II))+IDATOFF
         IF (ibad.EQ.IST.OR.ibad.EQ.ISP) then
            TWH(ibad,IPTWH)=0.
         else IF (ibad.EQ.IST+1.OR.ibad.EQ.ISP-1) then
            if (okreal4(TWH(ibad-1,IPTWH)) .and. 
     .		okreal4(TWH(ibad+1,IPTWH))) then
     	     TWH(ibad,IPTWH)=(TWH(ibad-1,IPTWH)+TWH(ibad+1,IPTWH))/2.
	    else if (first) then
	     call oerror(n268, n0, "REPLACE: Skipping points")
	     first = .false.
	    endif
         else if(ibad.le.isp.and.ibad.ge.ist) then
            if (okreal4(TWH(ibad-1,IPTWH)) .and. 
     .		okreal4(TWH(ibad-2,IPTWH)) .and.
     .		okreal4(TWH(ibad+2,IPTWH)) .and.
     .		okreal4(TWH(ibad+1,IPTWH))) then
             TWH(ibad,IPTWH)=2*(TWH(ibad-1,IPTWH)+TWH(ibad+1,IPTWH))/3.-
     .               (TWH(ibad-2,IPTWH)+TWH(ibad+2,IPTWH))/6.
	    else if (first) then
	     call oerror(n268, n0, "REPLACE: Skipping points")
	     first = .false.
	    endif
	 endif
  212    CONTINUE
c
      GO TO 99
C----------------------------------------------------------------------
C                                  PEAK   
C----------------------------------------------------------------------
  220 CONTINUE
c
      if(bdrop.lt.0.or.edrop.lt.0.or.(bdrop+
     1   edrop.gt.dtwh(c12ni,iptwh) )) call oerror(n288, m2, 'PEAK')
c
      ISTART=DTWH(C12SPN,IPTWH)+BDROP+IDATOFF
      ISTOP=DTWH(C12SPN,IPTWH)+DTWH(C12NI,IPTWH)
     .      +IDATOFF-1-EDROP
c
      if (istart .gt. istop) call oerror(n225, m2, 'PEAK')
c
      TMAX=-1.e34
      TCH=0.
      DO 221 JJ = ISTART,ISTOP
         IF (okreal4(twh(jj,iptwh)).and.TMAX.lt.TWH(JJ,IPTWH)) then
            TMAX=TWH(JJ,IPTWH)
            TCH=FLOAT(JJ-IDATOFF)
	 endif
  221    CONTINUE
c
      LIM=nint(TCH)+IDATOFF
      DO 222 I=LIM,ISTOP
         IF(okreal4(twh(i,iptwh)).and.TWH(I,IPTWH).LT.TMAX/2.0) GOTO 223
  222    CONTINUE
      i = istop
  223 LIM1=I
      DO 224 I = LIM,ISTART,-1
         IF(okreal4(twh(i,iptwh)).and.TWH(I,IPTWH).LT.TMAX/2.0) GOTO 225
  224    CONTINUE
      i = istart
  225 lim2 = i
      HWIDTH(1)=LIM1-lim2+1
      CENTER(1)=TCH
      HEIGHT(1)=TMAX
      BGAUSS=TCH-HWIDTH(1)
      EGAUSS=TCH+HWIDTH(1)
c
      lxa1 = lxa
      if (.not. okarray(lxa, dtwh(1,iptwh)) ) then
         call oerror(n270, n0, 
     .		'PEAK: Units of X-axis will be channels or samples')
	 lxa1 = 1
      endif
      tvel = chantox(lxa1, dtwh(1,iptwh), tch, ierr)
      hwdth = chantox(lxa1, dtwh(1,iptwh), float(lim1+1-idatoff), ierr)
     1       - chantox(lxa1, dtwh(1,iptwh), float(lim2-idatoff), ierr)
c
      WRITE (CPUF,2000,IOSTAT=IER) TMAX,TVEL,nint(TCH),abs(hwdth)
      CALL PWRITE (IPUF,n80)
c
#ifdef CONTINUUM
 2000 FORMAT('Hght:',1pg13.6,'; Cntr:',g13.6,' (Sample:',i5,
     .		'); HW:',g13.6)
#else
 2000 FORMAT('Hght:',1pg13.6,'; Cntr:',g13.6,' (Channl:',i5,
     .		'); HW:',g13.6)
#endif
c
      GO TO 99
C---------------------------------------------------------------------
C						      BIAS  
C---------------------------------------------------------------------
 280  CONTINUE
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH(C12SPN,IPTWH)+DTWH(C12NI,IPTWH)
     .           + IDATOFF - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'BIAS')
c
      DO 282 JJ = ISTART,ISTOP
 282	    if (okreal4(TWH(JJ,IPTWH))) TWH(JJ,IPTWH)=TWH(JJ,IPTWH)+FACT
 284        CONTINUE
c
      GO TO 99
C----------------------------------------------------------------
C                                          CHNGRES
C----------------------------------------------------------------
  290 CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH)+IDATOFF
      ISTOP=DTWH(C12SPN,IPTWH)+DTWH(C12NI,IPTWH)
     .      + IDATOFF - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'CHNGRES')
c
      DO 292 JJ = ISTART,ISTOP
         XDATA(JJ-ISTART+1)=TWH(JJ,IPTWH)
  292    CONTINUE
c
      NCHN=ISTOP-ISTART+1
      OLDRES = 1.
      CALL CHGRES(XDATA,NCHN,abs(OLDRES),abs(NEWRES))
c
      DO 293 JJ=ISTART,ISTOP
         TWH(JJ,IPTWH)=XDATA(JJ-ISTART+1)
  293    CONTINUE
c
#ifdef CONTINUUM
      dtwh(c12dx,iptwh)=newres*dtwh(c12dx,iptwh)
      dtwh(c3srt,iptwh)=newres*dtwh(c3srt,iptwh)
      dtwh(c12it,iptwh)=newres*dtwh(c12it,iptwh)
#else
      DTWH(C12BW,IPTWH)=NEWRES*dtwh(c12bw,iptwh)
      DTWH(C12FR,IPTWH)=NEWRES*dtwh(c12fr,iptwh)
      dtwh(c12dx,iptwh)=newres*dtwh(c12dx,iptwh)
#endif
c		adjust reference channel if not at center of data
      ntmp = nint(dtwh(c12rp,iptwh)*2.0 - 1.0)
      if (ntmp .ne. nchn) then
         centchan = (float(nchn) + 1.0) / 2.0
         dtwh(c12rp,iptwh) = centchan - 
     .           (centchan - dtwh(c12rp,iptwh)) / newres 
      endif
c
      GO TO 99
C
C-----------------------------------------------------------------------
C     CROSSFCN                  
C-----------------------------------------------------------------------
  300 CONTINUE
c
      iswtch = -1
c
      istart = DTWH(C12SPN,IPTWH) + idatoff
      istop = istart + DTWH(C12NI,IPTWH) - 1
      istart1 = DTWH(C12SPN,itemp) + idatoff
      istop1 = istart1 + DTWH(C12NI,itemp) - 1
c
      if (istop-istart .ne. istop1-istart1) 
     .		call oerror(n249, m2, 'CROSSFCN')
      if (istart .ge. istop) call oerror(n222, m2, 'CROSSFCN')
c
      goto 315
C-----------------------------------------------------------------------
C     CONVFCN                  
C-----------------------------------------------------------------------
  310 CONTINUE
c
      iswtch = 1
c
      istart = DTWH(C12SPN,IPTWH) + idatoff
      istop = istart + DTWH(C12NI,IPTWH) - 1
      istart1 = DTWH(C12SPN,itemp) + idatoff
      istop1 = istart1 + DTWH(C12NI,itemp) - 1
c
      if (istop-istart .ne. istop1-istart1) 
     .		call oerror(n249, m2, 'CONVFCN')
      if (istart .ge. istop) call oerror(n222, m2, 'CONVFCN')
c
315   invers = .false.
      in = (istop - istart + 1)
      INN = IN*2
      CALL COPY (INN,TWH(istart,iptwh),XDATA)
      CALL COPY (INN,TWH(istart1,itemp),work)
      DO 15 I = 1,IN
	    if (.not. okreal4(xdata(i)) .or. 
     .		.not. okreal4(work(i)) )
     .		call oerror(n268, m2, "CONVFCN/CROSSFCN: Cannot proceed")
            YDATA(I)=0.0
            temporary(I)=0.0
   15       CONTINUE
c
      jCNT=0
   16   jCNT=jCNT+1
        IF (2**jcnt .lt. in) GO TO 16
c
      IF (2**jcnt .eq. in) THEN
         CALL FFT(XDATA,YDATA,jCNT,INVERS)
         CALL FFT(work, temporary,jCNT,INVERS)
      ELSE
         CALL DFT(XDATA,YDATA,IN,INVERS)
         CALL DFT(work, temporary,IN,INVERS)
      ENDIF
c
      do 327 i = 1, in
        aa = xdata(i)
	xdata(i) = aa*work(i) - iswtch*ydata(i)*temporary(i)
	ydata(i) = ydata(i)*work(i) + iswtch*aa*temporary(i)
327	continue
c
      invers = .true.
      IF (2**jcnt .eq. in) THEN
         CALL FFT(XDATA,YDATA,jCNT,INVERS)
      ELSE
         CALL DFT(XDATA,YDATA,IN,INVERS)
      ENDIF
c
      do 328 i = 1, in/2
	twh(i+istart-1,iptwh) = xdata(i+in/2)
328	continue
      do 329 i = in/2+1, in
	twh(i+istart-1,iptwh) = xdata(i-in/2)
329	continue
c
      goto 99
c
C-----------------------------------------------------------------------
C     DECNVLV                  
C-----------------------------------------------------------------------
  320 CONTINUE
c
      istart = DTWH(C12SPN,IPTWH) + idatoff
      istop = istart + DTWH(C12NI,IPTWH) - 1
      istart1 = DTWH(C12SPN,itemp) + idatoff
      istop1 = istart1 + DTWH(C12NI,itemp) - 1
c
      if (istop-istart .ne. istop1-istart1) 
     .		call oerror(n249, m2, 'DECNVLV')
      if (istart .ge. istop) call oerror(n222, m2, 'DECNVLV')
c
      invers = .false.
      in = (istop - istart + 1)
      INN = IN*2
      CALL COPY (INN,TWH(istart,iptwh),XDATA)
      CALL COPY (INN,TWH(istart1,itemp),work)
      DO 335 I = 1,IN
	    if (.not. okreal4(xdata(i)) .or. 
     .		.not. okreal4(work(i)) ) 
     .		call oerror(n268, m2, "DECNVLV: Cannot proceed")
            YDATA(I)=0.0
            temporary(I)=0.0
  335       CONTINUE
c
      jCNT=0
  336   jCNT=jCNT+1
        IF (2**jcnt .lt. in) GO TO 336
c
      IF (2**jcnt .eq. in) THEN
         CALL FFT(XDATA,YDATA,jCNT,INVERS)
         CALL FFT(work, temporary,jCNT,INVERS)
      ELSE
         CALL DFT(XDATA,YDATA,IN,INVERS)
         CALL DFT(work, temporary,IN,INVERS)
      ENDIF
c
      do 337 i = 1, in
        mag2 = dprod(work(i),work(i)) + dprod(temporary(i),temporary(i))
	numaa1 = dprod(xdata(i),work(i)) + dprod(ydata(i),temporary(i))
	numaa2 = dprod(ydata(i),work(i)) - dprod(xdata(i),temporary(i))
c
	if (mag2 .eq. 0.0d00) then
c	   Watch out for division by zero if MAG2 is zero.
c	   Make sure that something reasonable is placed into XDATA and YDATA.
c
	   if (numaa1 .ge. 0.0d00) then
	       xdata(i) = rmaxnormal()
	   else 
	       xdata(i) = rminnormal()
	   endif
	   if (numaa2 .ge. 0.0d00) then
	       ydata(i) = rmaxnormal()
	   else 
	       ydata(i) = rminnormal()
	   endif
	else
	   numaa1 = numaa1/mag2
	   numaa2 = numaa2/mag2
c	   MAG2 is not zero but the division could overflow R*4 representation
c	   Make sure that something reasonable is placed into XDATA and YDATA.
c
	   if (numaa1 .gt. rmaxnormal() ) then
	      xdata(i) = rmaxnormal()
	   else if (numaa1 .lt. rminnormal() ) then
	      xdata(i) = rminnormal()
	   else
	      xdata(i) = numaa1
	   endif
	   if (numaa2 .gt. rmaxnormal() ) then
	      ydata(i) = rmaxnormal()
	   else if (numaa2 .lt. rminnormal() ) then
	      ydata(i) = rminnormal()
	   else
	      ydata(i) = numaa2
	   endif
	endif
337	continue
c
      invers = .true.
      IF (2**jcnt .eq. in) THEN
         CALL FFT(XDATA,YDATA,jCNT,INVERS)
      ELSE
         CALL DFT(XDATA,YDATA,IN,INVERS)
      ENDIF
c
      do 338 i = 1, in/2
	twh(i+istart-1,iptwh) = xdata(i+in/2)
338	continue
      do 339 i = in/2+1, in
	twh(i+istart-1,iptwh) = xdata(i-in/2)
339	continue
c
      goto 99
c
C---------------------------------------------------------------------
C					SHIFT
C---------------------------------------------------------------------
  130 CONTINUE
c
      istart = DTWH(C12SPN,IPTWH) + idatoff
      istop = istart + DTWH(C12NI,IPTWH) - 1
c
      if (istart .gt. istop) call oerror(n225, m2, 'SHIFT')
c
      if (abs(ashift) .gt. dtwh(c12ni,iptwh)) 
     .		call oerror(n251, m2, 'SHIFT')
c
      num = nint(dtwh(c12ni, iptwh))
      call shift(twh(istart, iptwh), num, ashift, ishift, ierr)
      if (ierr .ne. 0) call oerror(ierr, m2, "SHIFT")
c
      dtwh(c12rp,iptwh) = dtwh(c12rp,iptwh) + ashift
c
      chshifts(ichnext, 1) = dtwh(c1sno, iptwh)
      chshifts(ichnext, 2) = ashift
      ichnext = ichnext + 1
      if (ichnext .gt. maxstck) ichnext = 1
      ichnum = min(ichnum+1, maxstck)
c
      GO TO 99
C-------------------------------------------------------------------------
C                              FSHIFT
C-----------------------------------------------------------------------
  230 CONTINUE
c
      IF (NACCUM.le.0) then
	ashift = 0.0
      else
c
#ifdef CONTINUUM
         if (dtwh(c3srt,iptwh) .ne. daccum(c3srt)) 
     .       call oerror(n230, n0, 'FSHIFT:  Time resolutions differ')
         ashift = 0.0
#else
         if (dtwh(c12fr,iptwh) .ne. daccum(c12fr)) 
     .       call oerror(n230, m2, 'FSHIFT:  Freq. resolutions differ')
c
         ashift=(DTWH(C12RF,iptwh)-daccum(C12RF))/daccum(C12FR)
c
         ashift = ashift - dtwh(c12rp,iptwh) + daccum(c12rp)
#endif
c
      endif
c
      GO TO 99
C-----------------------------------------------------------------------
C                              VSHIFT
C-----------------------------------------------------------------------
  240 CONTINUE
c
      IF (NACCUM.le.0) then
	 ashift = 0.0
      else
c
#ifdef CONTINUUM
         if (dtwh(c12dx,iptwh) .ne. daccum(c12dx)) 
     .      call oerror(n230, n0, 
     .              'FSHIFT:  Position resolutions differ')
         ashift = 0.0
#else
      if (.not. okarray(n3, dtwh(1,iptwh)) ) 
     .	call oerror(n270, m2, 'VSHIFT')
c
         ashift = daccum(c12rp) - 
     .		xtochan(n3, dtwh(1,iptwh), sngl(daccum(c12x0)), ier)
#endif
      endif
c
      GO TO 99
C-----------------------------------------------------------------------
C                              PDOC
C-----------------------------------------------------------------------
  250 CONTINUE
c
      if (dtwh(c1tel,iptwh) .ne. t12m) call oerror(n248, m2, 'PDOC')
c
      do 2509 i1 = 1, 10
         if (.not.okreal4(twh(idatoff+i1,iptwh))) 
     .		call oerror(n268, m2, "PDOC: Cannot proceed")
2509	 continue
c
#ifdef CONTINUUM
      call pdoc
#else
      if (sp .lt. 1) call oerror(n112, m1, 'PDOC: One needed')
c
      ifit = nint(v(sp))
      sp = sp - 1
c
      if (ifit .lt. 1 .or. ifit .gt. 2) 
     .   call oerror(n112, m1,'PDOC: argument must be 1 or 2')
c
      call pdoc(ifit)
#endif
c
      GO TO 99
C--------------------------------------------------------------------
C				LISTSHIFT
C--------------------------------------------------------------------
  260 continue
c
      if (ichnum .le. 0) then
         write(cpuf, 1250, iostat = ier)
         call pwrite(ipuf, n80)
 1250 format('LISTSHIFT: The list of shifts is empty!')
      else
         write(cpuf, 1251, iostat=ier)
 1251 format(4(3x,'scan',4x,'shift',2x))
         call pwrite(ipuf, n80)
         i1 = ichnext - ichnum
         if (i1 .lt. 1) i1 = i1 + maxstck
         do 251 i2 = 1, ichnum, 4
            do 252 ii = 1, min(4, ichnum-i2+1)
               if (i1 .gt. maxstck) i1 = i1 - maxstck
               ilist(ii) = i1
               i1 = i1 + 1
 252        continue
            write(cpuf, 1252, iostat=ier)
     .         (chshifts(ilist(ii2),1), chshifts(ilist(ii2), 2), 
     .         ii2=1,(ii-1))
 1252 format(4(x,f8.2,x,f6.1,2x))
            call pwrite(ipuf, n80)
 251     continue
      endif
c
      goto 99
C---------------------------------------------------------------------
C					PLUS
C---------------------------------------------------------------------
 330  CONTINUE
c
      istart0 = DTWH(C12SPN,IPTWH) + IDATOFF
      istop0 = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     1        + IDATOFF - 1
      istart1 = DTWH(C12SPN,itemp) + IDATOFF
      istop1 = DTWH(C12SPN,itemp) + DTWH(C12NI,itemp)
     1        + IDATOFF - 1
c
      if (istart0 .gt. istop0 .or. istart1 .gt. istop1) 
     .    call oerror(n222, m2, 'PLUS')
c
      DO 331 JJ = istart0, istop0
	 jj1 = jj - istart0 + istart1
         if (jj1 .le. istop1 .and. jj1 .ge. istart1) then
            if (okreal4(TWH(JJ,IPTWH)).and.okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=TWH(JJ,IPTWH)+TWH(jj1,ITEMP)
	    else if(defmode .ge. 0.5 .and. okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=TWH(jj1,ITEMP)
	    else if (defmode .lt. 0.5) then
		twh(jj,iptwh)=rinfinity()
	    endif
	 endif
  331    CONTINUE
c
      GO TO 99
C---------------------------------------------------------------------
C					MULTIPLY
C---------------------------------------------------------------------
 340  CONTINUE
c
      istart0 = DTWH(C12SPN,IPTWH) + IDATOFF
      istop0 = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     1        + IDATOFF - 1
      istart1 = DTWH(C12SPN,itemp) + IDATOFF
      istop1 = DTWH(C12SPN,itemp) + DTWH(C12NI,itemp)
     1        + IDATOFF - 1
c
      if (istart0 .gt. istop0 .or. istart1 .gt. istop1) 
     .    call oerror(n222, m2, 'MULTIPLY')
c
      DO 341 JJ = istart0, istop0
	 jj1 = jj - istart0 + istart1
         if (jj1 .le. istop1 .and. jj1 .ge. istart1) then
            if (okreal4(TWH(JJ,IPTWH)).and.okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=TWH(JJ,IPTWH)*TWH(jj1,ITEMP)
	    else if(defmode .ge. 0.5 .and. okreal4(TWH(jj1,ITEMP))) then
         	TWH(JJ,IPTWH)=TWH(jj1,ITEMP)
	    else if (defmode .lt. 0.5) then
		twh(jj,iptwh)=rinfinity()
	    endif
	 endif
  341    CONTINUE
c
      GO TO 99
C--------------------------------------------------------------------
  99  CONTINUE
C
      RETURN
      END
c
c
      subroutine shift(data, num, ashift, ishift, ierr)
c
c     Performs a shift of the DATA by ASHIFT channels
c
c     DATA(NUM) = R*4 data array
c     NUM = dI*2 imension of DATA
c     ASHIFT = R*4 magnitude of shift
c     ISHIFT = I*2 Returns the number of channels that have been set to
c		blank values at either end of the array.  If > 0, then
c		the first ISHIFT channels have been set to blank values,
c		else the last -ishift channels have been set to blank values. 
c     ierr = I*2 = 0 if all goes OK
c
      integer*2 num, ierr, ishift
      real ashift, data(*)
c
      real fshift, rinfinity, welch
      double precision amp, phs, phshft, pi
      logical okreal4
      integer*4 num4, jcnt, numexp, kk, i, k1, nskip, k2
      logical*4 t, f
c
      include 'appl.inc'
c
      parameter (pi = 3.1415926535898d0 )
c 
      welch(kk) = 1.0 - ( float(kk - numexp/2)/float(numexp/2) )**2
c     Definition of Welch windowing function
c
      ierr = 0
      ishift = 0
      num4 = num
c
      t = .true.
      f = .false.
      if (ashift .eq. 0.0) goto 99
c
      if (abs(ashift) .gt. num4) then
	ierr = 251
	goto 99
      endif
c
      ishift = int(ashift)
      fshift = mod(ashift,1.0)
c
      if (abs(fshift) .gt. 0.99) then
	ishift = ishift + sign(1,fshift)
	fshift = 0.0
      else if (abs(fshift) .lt. 0.01) then
	fshift = 0.0
      endif
c     Don't shift by fraction of a channel if the requested shift is
c     less than one percent of a channel width.
c
      if (fshift .gt. 0.0) then
	ishift = ishift + 1
	fshift = 1.0 - fshift
      else
	fshift = - fshift
      endif
c     Reverse sense of fshift
c
c     Take care of fractional part of shift.   
c
      if (fshift .ne. 0.0) then
c
	if (num4 .gt. MAX_DATA_POINTS) then
		ierr = 252
		goto 99
	endif
c
c       Step 1:  To prevent aliasing, expand array and pad with zeros
c
	  jcnt=0
336         jcnt=jcnt+1
            if (2**jcnt .le. num4) goto 336
c	2**JCNT should be larger than the number of points in array
c	
	numexp = min(2**jcnt,MAX_DATA_POINTS)
c	NUMEXP should be the size of the expanded array (up to the limit
c	set by MAX_DATA_POINTS). It will either be a power of 2 or 
c       MAX_DATA_POINTS (and even).
c
	  jcnt=0
337         jcnt=jcnt+1
            if (2**jcnt .lt. numexp) goto 337
c	2**JCNT should be equal to or larger than the number of points in the
c	expanded array
c		
	nskip = numexp - num4
c       NSKIP is the number of locataion that are padded out.
c
	do 331 i = 1, numexp
	   xdata(i) = 0.0
	   ydata(i) = 0.0
331	   continue
c	Zero out X/YDATA
c
c	Step 2: Move contents of DATA into XDATA
c
        do 335 i = 1, num4
	    k1 = i + nskip/2
	    if (okreal4(data(i)) ) then
		xdata(k1) = data(i)
	    else
		if (i .gt. 1) then
		   xdata(k1) = xdata(i-1)
		else
		   xdata(k1) = 0.0
		endif
	    endif
335         continue
c
c	  Step 3:  Do FFT
c
          if (2**jcnt .eq. numexp) then
            call fft(xdata,ydata,jcnt,f)
          else
            call dft(xdata,ydata,numexp,f)
          endif
c
c	  Step 4:  Calculate and add phase shift; Window Amplitudes with
c		   Welch function so as to reduce leakage.
c
	  phshft = 2.d0*pi*fshift/float(numexp)
          do 340 i = 1, numexp
	    if (i .le. numexp/2) then
		k1 = i
	    else
	        k1 = i - numexp
	    endif	
	    k2 = k1 + numexp/2
	    amp = welch(k2)*sqrt(xdata(i)**2 + ydata(i)**2)
	    phs = atan2(ydata(i), xdata(i)) + phshft*float(k1)
	    xdata(i) = amp*cos(phs)
	    ydata(i) = amp*sin(phs)
340	    continue
c
c	  Step 5:  Do IFFT
c
          if (2**jcnt .eq. numexp) then
            call fft(xdata,ydata,jcnt,t)
          else
            call dft(xdata,ydata,numexp,t)
          endif
c
c	  Step 6:  Move data from XDATA back into DATA
c
          do 350 i = 1,num4
	    k1 = i + nskip/2
	    if (okreal4(data(i)) ) then
		data(i) = xdata(k1)
	    else
		data(i) = rinfinity()
		if (i .gt. 1) data(i-1) = rinfinity()
	    endif
350	    continue
      endif
c
c     Now, the integer part of the shift
c
      if (ishift .gt. 0) then
	do 131 i = num4, ishift+1, -1
	   data(i) = data(i-ishift)
131	   continue
	do 132 i = ishift, 1, -1
	   data(i) = rinfinity()
132	   continue
      else if (ishift .lt. 0) then
	do 133 i = 1, num4 + ishift
	   data(i) = data(i-ishift)
133	   continue
	do 134 i = num4 + ishift + 1, num4
	   data(i) = rinfinity()
134	   continue
      endif
c     Moves the integer part of the desired shift.
c
99    return
      end
c
