      SUBROUTINE AUB (J)
C-------------------------------------------------------------------------------
C  @(#)caub.f	5.2 11/09/94
C-------------------------------------------------------------------------------
c
c     caub - Caldbe, Fixdbe, Zero, Switched, TotalPwr, Avgd, Ons,
c		Avg, Temps, Solvetip
c
c	For 12-m versions of these verbs (most of the contents here),
c	the following should be helpful:
c	There are three classes of verbs here:
c		(I) Verbs designed to work with "raw" continuum data.
c		    These verbs are CALDBE, FIXDBE, ZERO, SWITCHED,
c		    and TOTALPWR.  These verbs all expect the data to
c		    be in groups of 4, with the 4 values being in the
c		    the following order:
c			(1) Source + Cal
c			(2) Reference + Cal
c			(3) Source
c			(4) Reference
c		(II) Verbs designed to work with On/Off pairs after
c		     the data has been reduced by one of the above.
c		     These verbs are AVGD, ONS, AVG, and TEMPS.
c		     At this stage, the data STILL come in groups of
c		     4 values, with the following interpretation of
c		     those values:
c			(1) Off
c			(2) On
c			(3) On
c			(4) Off
c		(III) The SOLVETIP verb.  This verb expects there to
c		      to be 2*ntpts-1 (ntpts is parameter defined below) values
c		      These are really pairs (ntpts-1) values at a given
c		      airmass centered (in terms of location in the
c		      data vector) around a single value at airmass(ntpts).
c
c*********************************
c
      integer*2 ipuf(40), ier, j, iptwh, jj, istart, istop, nopt, i,
     .		nt, ns, itip, idbe, ich, iy, num, nlast, 
     .		nfirst, iend, kk, npairs, npts, npt, itype, ix, novcvg
      integer*4 ntpts
      parameter (ntpts = 6)
c
c		ntpts is the number of distict airmasses for 12-m
c		tips, used by solvetip.   If you change this number,
c		remember to update the airmass data statement below
c		to reflect all ntpts aipsmasses.  For GB, the number
c		of airmasses are stored in SECZ and are 2,3,4,5,6,7
c		and are defined in the data statement below.
c
      real*4 sinel, el, tsrc(ntpts), atm, tau, ph1, ph2, ph3, 
     .       ph4, tc, cal, ascale, exarg, t
     .       ypscn, ts, smean, srms, rjz, rjx, rjy, scl,
     .	     xmean, xrms, tp, tmp, tcal, tamb, tvan, frq, taug,
     .	     sumx, sumxy, sumx2, sumy2, tpsn, diff,
     .	     sumy, tm, try, yy, tsbr, ym, apscale,
     .	     tsky(ntpts), yi(ntpts), airmass(ntpts), eb, ea, deltau, 
     .       rs1, rs2, rs, zx, zy, a1, a0, deleta, chisqr, ysbr, yvalue
c
      integer*2 m3, m2, m1, n1, n5, n80, n112, n120, n225, n234, n248,
     .       n250, n265, n283, n298, n268
c
      real*8 nrao140, nrao300, t12m, digital, holograf, tipping, 
     .	     tip(3*ntpts), secz(ntpts), tau8, trx8 
      CHARACTER*80 CPUF
      logical okreal4
c
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
      include 'stk.inc'
      include 'core.inc'
c
      EQUIVALENCE (CPUF,IPUF)
c
      data digital/'DIGITAL '/, holograf/'HOLOGRAF'/
      data tipping/'CONTSTIP'/
      data t12m/'NRAO 12M'/,nrao140/'NRAO 43M'/,nrao300/'NRAO 93M'/
      data airmass / 2.6, 2.3, 2.0, 1.7, 1.4, 1.1 /
      data secz / 2.d0, 3.d0, 4.d0, 5.d0, 6.d0, 7.d0 /
c
      data m3, m2, m1, n1, n5, n80, n112, n120, n225, n234, n248,
     .       n250, n265, n283, n298, n268
     .   /-3, -2, -1, 1, 5, 80, 112, 120, 225, 234, 248, 250, 265,
     .    283, 298, 268/
c
C=======================================================================
C
      IPTWH = 1
c
      JJ=J
      GO TO (70, 80, 90, 100, 120, 180, 40, 110, 130, 170, 
     .       190), JJ
      call oerror(n120, m3, 'AUB')
c
C----------------------------------------------------------
C                                                  CALDBE
C-----------------------------------------------------------------------
   70 CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'CALDBE')
c
      if (dtwh(c1tel,iptwh) .ne. t12m) call oerror(n248, m2, 'CALDBE')
c
      NOPT=DTWH(C12NI,IPTWH)
      DO 71 I = 1,NOPT
	 if (.not. okreal4(TWH(I+istart-1,IPTWH)))
     .		call oerror(n268, m2, "CALDBE: Cannot proceed")
         XDATA(I)=TWH(I+istart-1,IPTWH)
   71    CONTINUE
c
      J=istart
      CALVAL=0.
      NS=NOPT/4
c			get value of Tcal and noise tube
      TC=DTWH(C12CT,IPTWH)
      NT=DTWH(C12FR,IPTWH)
c
      DO 72 I = 1,NOPT,4
         PH1=XDATA(I)
         PH2=XDATA(I+1)
         PH3=XDATA(I+2)
         PH4=XDATA(I+3)
         TWH(J,IPTWH)=(PH1-PH3+PH2-PH4)*0.5
         if (nt .eq. 1) CALVAL=CALVAL+TWH(J,IPTWH)
         J=J+1
   72    CONTINUE
c
      IF (NT.LT.1) then
         CALVAL=DTWH(C12RF,IPTWH)
      else if (nt .eq. 1) then
         CALVAL=CALVAL/float(NS)
      endif
c		special VANE cal case (nt = 2)
      if (nt .eq. 2) then
         cal = dtwh(c12rst,iptwh)
      else
         CAL=TC/CALVAL
      endif
c
      DTWH(C12NI,IPTWH)=DTWH(C12NI,IPTWH)/4.
c
      WRITE (CPUF,2789,IOSTAT=IER) CAL, TC, CALVAL
 2789 FORMAT (' CAL FACTOR, NOISE TUBE, CALVAL',3F10.4)
      CALL PWRITE (IPUF,n80)
c
      GO TO 99
C-------------------------------------------------------------------
C                                       FIXDBE
C-----------------------------------------------------------------------
   80 CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'FIXDBE')
c
      if (dtwh(c1tel,iptwh) .ne. t12m) call oerror(n248, m2, 'FIXDBE')
c
      NOPT=DTWH(C12NI,IPTWH)
      DO 81 I = 1,NOPT
	 if (.not. okreal4(TWH(I+istart-1,IPTWH)))
     .		call oerror(n268, m2, "FIXDBE: Cannot proceed")
         XDATA(I)=TWH(I+istart-1,IPTWH)
   81    CONTINUE
c
      NT = DTWH(C12FR,IPTWH)
      if (nt .ne. 2) then
         EL=DTWH(C4EL,IPTWH)/57.295779
         SINEL=SIN(EL)
         IF (EL.GT.0.) THEN
            ATM=1./SINEL
         ELSE
            ATM=0.
         ENDIF
         TAU=DTWH(C12WO,IPTWH)
         EXARG=TAU*ATM
         ASCALE=EXP(EXARG)
      else
         ascale = 1.0
      endif
c
      J=istart
      CALVAL=0.
      NS=NOPT/4
c
      DO 82 I = 1,NOPT,4
         PH1=XDATA(I)
         PH2=XDATA(I+1)
         PH3=XDATA(I+2)
         PH4=XDATA(I+3)
         TWH(J,IPTWH)=(PH1-PH4+PH3-PH2)*ASCALE*0.5
         TWH(J+NS,IPTWH)=(PH1+PH2+PH3+PH4)*ASCALE*0.25
         TWH(J+NS*2,IPTWH)=(PH1-PH3+PH2-PH4)*0.5
         CALVAL=CALVAL+TWH(J+NS*2,IPTWH)
         TWH(J+NS*3,IPTWH)=(PH1-PH2+PH4-PH3)*ASCALE
         J=J+1
   82    CONTINUE
c
      CALVAL=CALVAL/float(NS)
c
      GO TO 99
C------------------------------------------------------------------
C                                                 ZERO
C------------------------------------------------------------------
   90 CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'ZERO')
c
      if (dtwh(c1tel,iptwh) .ne. t12m) call oerror(n248, m2, 'ZERO')
c
      NOPT=DTWH(C12NI,IPTWH)
      DO 91 I = 1,NOPT
	 if (.not. okreal4(TWH(I+istart-1,IPTWH)))
     .		call oerror(n268, m2, "ZERO: Cannot proceed")
         XDATA(I)=TWH(I+istart-1,IPTWH)
   91    CONTINUE
c
      NT = DTWH(C12FR,IPTWH)
      if (nt .ne. 2) then
         EL=DTWH(C4EL,IPTWH)/57.295779
         SINEL=SIN(EL)
         IF (EL.GT.0.) THEN
            ATM=1./SINEL
         ELSE
            ATM=0.
         ENDIF
         TAU=DTWH(C12WO,IPTWH)
         EXARG=TAU*ATM
         ASCALE=EXP(EXARG)
      else
         ascale = 1.0
      endif
c
      CALVAL=0.
      NS=NOPT/4
c
      sumy = 0.
      sumy2 = 0.
      ns = dtwh(c12ni,iptwh) / 4.0
      DO 92 I = 1,NOPT,4
         PH1=XDATA(I)
         PH2=XDATA(I+1)
         PH3=XDATA(I+2)
         PH4=XDATA(I+3)
         yvalue=(PH1-PH2+PH4-PH3)*ASCALE*0.5
         sumy = sumy + yvalue
         sumy2 = sumy2 + yvalue*yvalue
   92    CONTINUE
c
      VRMS=SQRT((sumy2 - sumy*sumy/ns)/float(ns-1))
c
      WRITE (CPUF,1087,IOSTAT=IER) VRMS
 1087 FORMAT (' ZERO RMS=',F10.4)
      CALL PWRITE (IPUF,n80)
c
      GO TO 99
C------------------------------------------------------------------
C                                               SWITCHED
C Modified 890110 [PPM] Changes for vane calibrate...
C------------------------------------------------------------------
  100 CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'SWITCHED')
c
      if (dtwh(c1tel,iptwh) .ne. t12m) call oerror(n248, m2, 'SWITCHED')
c
      NOPT = DTWH(C12NI,IPTWH)
      DO 101 I = 1, NOPT
	 if (.not. okreal4(TWH(I+istart-1,IPTWH)))
     .		call oerror(n268, m2, "SWITCHED: Cannot proceed")
         XDATA(I) = TWH(I+istart-1, IPTWH)
  101 CONTINUE
C
      TYPSCN = 2.0
      TC = DTWH(C12CT,IPTWH)
      NT = DTWH(C12FR,IPTWH)
      if (nt .ne. 2) then
         EL = DTWH(C4EL,IPTWH)/57.295779
         SINEL = SIN(EL)
         IF (EL.GT.0.) THEN
            ATM = 1./SINEL
         ELSE
            ATM = 0.
         ENDIF
         TAU = DTWH(C12WO,IPTWH)
         EXARG = TAU*ATM
         ASCALE = EXP(EXARG)
      else
         ascale = 1.0
      endif
c
      J = istart
      CALVAL = 0.
      TS = 0.0
      TPSN = 0.0
      NS = NOPT/4
C
C     Sequence of data is (Sig+Cal, Ref+Cal, Sig, Ref) so get Sig-Ref
C     (stored in place in XDATA) and total Cal signal (in CALVAL)
C
      DO 102 I = 1, NOPT, 4
         PH1 = XDATA(I)
         PH2 = XDATA(I+1)
         PH3 = XDATA(I+2)
         PH4 = XDATA(I+3)
         TWH(J,IPTWH) = (PH1-PH4+PH3-PH2)*ASCALE*0.5
         if (nt .eq. 1) then
            CALVAL = CALVAL+(PH1-PH3+PH2-PH4)*0.5
            TS = TS+(PH4/(PH2-PH4))
         endif
         TPSN = TPSN+((PH1-PH2+PH3-PH4)/(PH3+PH4))
         J = J+1
  102    CONTINUE
c
      VRMS = TPSN/float(NS)
      DTWH(C12NI,IPTWH) = DTWH(C12NI,IPTWH)/4.
C
C     Check if noise tube was firing; if so, change system temperature
C
      if (nt .lt. 1) then
         calval = dtwh(c12rf, iptwh)
      else IF (NT.EQ.1) THEN
         CALVAL = CALVAL/float(NS)
         TS = (TS/float(NS))*TC
         DTWH(C12SST,IPTWH) = TS
      ENDIF
c		special case for VANE cal, NT = 2
      if (nt .eq. 2) then
         cal = dtwh(c12rst, iptwh)
      else
         CAL = TC/CALVAL
      endif
c               finally, scale the data
      DO 103 I = 1,NS
         TWH(I+istart-1,IPTWH) = TWH(I+istart-1,IPTWH)*CAL
  103    CONTINUE
c
      GO TO 99
C---------------------------------------------------------------------
C                                             TOTALPWR
C---------------------------------------------------------------------
 120  CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'TOTALPWR')
c
      if (dtwh(c1tel,iptwh) .ne. t12m) call oerror(n248, m2, 'TOTALPWR')
c
      NOPT=DTWH(C12NI,IPTWH)
      DO 121 I = 1,NOPT
	 if (.not. okreal4(TWH(I+istart-1,IPTWH)))
     .		call oerror(n268, m2, "TOTALPWR: Cannot proceed")
         XDATA(I)=TWH(I+istart-1,IPTWH)
  121    CONTINUE
c
      TYPSCN = 1.0
      IF (DTWH(C1STC,IPTWH) .EQ. TIPPING)THEN 
             ITIP=1
      ELSE
             ITIP=0
      ENDIF
c
      TC=DTWH(C12CT,IPTWH)
      NT=DTWH(C12FR,IPTWH)
      if (nt .ne. 2) then
         EL=DTWH(C4EL,IPTWH)/57.295779
         SINEL=SIN(EL)
         IF (EL.GT.0.) THEN
            ATM=1./SINEL
         ELSE
            ATM=0.
         ENDIF
         TAU=DTWH(C12WO,IPTWH)
         EXARG=TAU*ATM
         ASCALE=EXP(EXARG)
      else
         ascale = 1.0
      endif
c
      J=istart
      CALVAL=0.
      TS=0.0
      TPSN=0.0
c                  ns is used here to count the number of values going into
c                  quantities which will be averaged (TS, CALVAL, and TPSN)
      NS=0
c
      DO 122 I = 1,NOPT,4
         PH1=XDATA(I)
         PH2=XDATA(I+1)
         PH3=XDATA(I+2)
         PH4=XDATA(I+3)
         IF (ITIP .EQ. 0)  THEN
               TWH(J,IPTWH)=(PH1+PH2+PH3+PH4)*ASCALE*0.25
               IF (NT.EQ.1) then
                  TS=TS+(PH4/(PH2-PH4))
                  CALVAL=CALVAL+(PH1-PH3+PH2-PH4)*0.5
               endif
               TPSN=TPSN+((PH1-PH2+PH3-PH4)/(PH3+PH4))
               ns = ns + 1
         ELSE
               JJ=MOD(J,2)
               TWH(J,IPTWH)=(PH1+PH2+PH3+PH4)*0.25
               IF (JJ .EQ. 0) THEN
                  IF (NT .EQ. 1) then
                     TS=TS+(PH4/(PH2-PH4))
                     CALVAL=CALVAL+(PH1-PH3+PH2-PH4)
                  endif
                  TPSN=TPSN+((PH1-PH2+PH3-PH4)/(PH3+PH4))
                  ns = ns + 1
               ENDIF
         ENDIF
         J=J+1
  122    CONTINUE
c
      VRMS=TPSN/float(NS)
      DTWH(C12NI,IPTWH)=DTWH(C12NI,IPTWH)/4.
c			check on status of noise tube
      if (nt .lt. 1) then
         calval = dtwh(c12rf, iptwh)
      else IF (NT.EQ.1) THEN
         CALVAL=CALVAL/float(NS)
         TS=(TS/float(NS))*TC
         DTWH(C12SST,IPTWH)=TS
      ENDIF
c			special VANE cal case, NT = 2
      if (nt .eq. 2) then
         cal = dtwh(c12rst, iptwh)
      else
         CAL=TC/CALVAL
      endif
c                      scale the data
      DO 123 I = 1, nopt/4
         TWH(I+istart-1,IPTWH)=TWH(I+istart-1,IPTWH)*CAL
  123    CONTINUE
c
      GO TO 99
C---------------------------------------------------------------------
C                                         AVGD
C---------------------------------------------------------------------
 180  CONTINUE
c
      if (dtwh(c1tel,iptwh) .ne. t12m) call oerror(n248, m2, 'AVGD')
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'AVGD')
c
      DO 181 I = ISTART,ISTOP
         XDATA(I-istart+1)=TWH(I,IPTWH)
  181    CONTINUE
c
      if (dtwh(c1bke,iptwh) .eq. digital .or. 
     .    dtwh(c1bke,iptwh).eq.holograf) then
	IDBE=-1
      else
	idbe=0
      endif
c
      CALL MEAN (n1, istop-istart+n1,XDATA,IDBE,TYPSCN,SMEAN,SRMS)
c
      DTWH(C12ST,IPTWH)=SMEAN
      DTWH(C12RMS,IPTWH)=SRMS
      DTWH(C7OSN,IPTWH)=0           
c     Now appears as an analog scan
c
      GO TO 99
C---------------------------------------------------------------------
C					ONS   166
C---------------------------------------------------------------------
  40  CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'ONS')
c
      if (showplot(numplots) .ne. 0) call oerror(n298, m2, 'ONS')
c
c
      if(dtwh(c1tel,iptwh) .eq. nrao140 .or. 
     .        dtwh(c1tel,iptwh) .eq. nrao300) then
         ich = istop - istart + 1 - 3
	 kk = 2
      else if (dtwh(c1tel,iptwh) .eq. t12m) then
	 ich =  ISTOP-istart+1
	 kk = 4
      else
	 call oerror(n283, m2, 'ONS')
      endif
c
      DO 398 I = 2,ich, kk
         if (dtwh(c1tel,iptwh) .eq. t12m) then
	   if (okreal4(TWH(I+istart-1,IPTWH)) .and.
     .         okreal4(TWH(I+istart,IPTWH))) then
        	IX=nint(I*AX(numplots)+bx(numplots))
        	IY=nint(TWH(I+istart-1,IPTWH)*AY(numplots)+by(numplots))
         	CALL PLACE (IX-n5,IY)
         	CALL VCTR (IX+n5,IY)
         	CALL PLACE (IX,IY-n5)
         	CALL VCTR(IX,IY+n5)
         	IX=nint( (I+1)*AX(numplots)+bx(numplots))
         	IY=nint(TWH(I+istart,IPTWH)*AY(numplots)+by(numplots))
         	CALL PLACE (IX-n5,IY)
         	CALL VCTR (IX+n5,IY)
         	CALL PLACE (IX,IY-n5)
         	CALL VCTR(IX,IY+n5)
	   endif
	 else
	   if (okreal4(TWH(I+istart-1,IPTWH))) then
        	IX=nint(I*AX(numplots)+bx(numplots))
        	IY=nint(TWH(I+istart-1,IPTWH)*AY(numplots)+by(numplots))
         	CALL PLACE (IX-n5,IY)
         	CALL VCTR (IX+n5,IY)
         	CALL PLACE (IX,IY-n5)
         	CALL VCTR(IX,IY+n5)
	   endif
	 endif
  398    CONTINUE
c
      GO TO 99
C---------------------------------------------------------------------
C							    AVG
C---------------------------------------------------------------------
 110  CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH) + ISTART -1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'AVG')
c
      DO 111 I = 1,ISTOP-istart+1
         XDATA(I)=TWH(I+istart-1,IPTWH)
  111    CONTINUE
c
      if(dtwh(c1tel,iptwh) .eq. nrao140 .or. 
     .        dtwh(c1tel,iptwh) .eq. nrao300) then
c
	 if (.not.okreal4(TWH(ISTOP-1,IPTWH)) .or.
     .	     .not.okreal4(TWH(ISTOP-2,IPTWH)) .or.
     .	     .not.okreal4(TWH(ISTOP,IPTWH)))
     .		call oerror(n268, m2, "AVG: Cannot proceed")
         TMP = TWH(ISTOP-1,IPTWH)-(TWH(ISTOP-2,IPTWH)+
     .      TWH(ISTOP,IPTWH))/2.
         SCL = dtwh(c12ct,IPTWH) / TMP
c
	 call gbmean(xdata, ISTOP-istart+n1, smean, srms)
c
	 smean = scl * smean
	 srms = scl * srms
c
         WRITE (CPUF,1002,IOSTAT=IER) SMEAN,SRMS
 1002    FORMAT (' MEAN=',F10.5,' K  RMS=',F10.5)
         CALL PWRITE (IPUF,n80)
c
      else if (dtwh(c1tel,iptwh) .eq. t12m) then 
c
         if (dtwh(c1bke,iptwh) .eq. digital .or. 
     .    dtwh(c1bke,iptwh).eq.holograf) then
	   IDBE=-1
         else
	   idbe=0
         endif
c
         CALL MEAN (n1,istop-istart+n1,XDATA,IDBE,typscn,SMEAN,SRMS)
c			get noise tube value
         NT=DTWH(C12FR,IPTWH)
c			in the following 24.41 = 2 * k / (pi * (6**2))
c			where k = boltzman = 1.381 * 10**3 Jy m**2 / K
         if (nt .ne. 2) then
            RJY=SMEAN*(24.41/DTWH(C8AAE,IPTWH))
            RJZ=SRMS*(24.41/DTWH(C8AAE,IPTWH))
         else
c			vane calibration is slightly more complicated
            apscale = dtwh(c8aae,iptwh) / 
     .                    (dtwh(c8el,iptwh) * dtwh(c8ef,iptwh))
            RJY=SMEAN*(24.41/apscale)
            RJZ=SRMS*(24.41/apscale)
         endif
c
         IF (RJZ.EQ.0.0) then
           RJX=1.0
         else
           RJX=RJY/RJZ
         endif
c
         WRITE (CPUF,1001,IOSTAT=IER) SMEAN,SRMS,RJY,RJZ,RJX
 1001    FORMAT (' MEAN=',F10.5,' K  RMS=',F10.5,F11.5,' JY',
     .		 F11.5,' S/N',F9.3)
         CALL PWRITE (IPUF,n80)
c
      else
	call oerror(n283, m2, 'AVG')
      endif
c
      DTWH(C12ST,IPTWH)=SMEAN
      DTWH(C12RMS,IPTWH)=SRMS
c
      GO TO 99
C---------------------------------------------------------------------
C					SEDITS
C---------------------------------------------------------------------
  130 CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH)+ ISTART-1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'SEDITS')
c
      IF (SP.lt.2) call oerror(n112, m1, 'SEDITS')
c
      if(dtwh(c1tel,iptwh) .eq. nrao140 .or. 
     .        dtwh(c1tel,iptwh) .eq. nrao300) then
c
         nfirst=2*nint(V(SP-1))+istart-2
         nlast=2*nint(V(SP))+istart-1 
         SP=SP-2
c
	 if (nlast .le. nfirst .or. nlast .ge. istop-2 .or. 
     .	     nfirst .lt. istart .or. nlast-nfirst+4 .ge. istop-istart+1)
     .		 call oerror(n112, m2, 'SEDITS')
c
	 if (.not.okreal4(TWH(ISTOP-1,IPTWH)) .or.
     .	     .not.okreal4(TWH(ISTOP-2,IPTWH)) .or.
     .	     .not.okreal4(TWH(ISTOP,IPTWH)))
     .		call oerror(n268, m2, "SEDITS: Cannot proceed")
         TMP = TWH(ISTOP-1,IPTWH)-(TWH(ISTOP-2,IPTWH)+
     .      TWH(ISTOP,IPTWH))/2.
         SCL = dtwh(c12ct,IPTWH) / TMP
c
         NUM=nlast-nfirst+1
         IEND=ISTOP-NUM
         DO 133 I = nfirst,IEND
            TWH(I,IPTWH)=TWH(I+NUM,IPTWH)
  133       CONTINUE
c
         DO 134 I = 1,ISTOP-ISTART+1-num
            XDATA(I)=TWH(I+istart-1,IPTWH)
  134       CONTINUE
c
	 call gbmean(xdata, ISTOP-ISTART+n1-num, xmean, xrms)
c
	 xmean = scl * xmean
	 xrms = scl * xrms
c
      else if (dtwh(c1tel,iptwh) .eq. t12m) then
c          order of arguments: V(SP-1) is the first group number
c                              V(SP) is the last group number
c          SEDITS eliminates all the data from the first group through
c                 the last group.  Each group is 4 values long.
c
c          First, translate the group number into indicies in the data array
c
         nfirst=4*nint(V(SP-1))+istart-4
         nlast=4*nint(V(SP))+istart-1 
         SP=SP-2
c
	 if (nlast .le. nfirst .or. nfirst .lt. istart .or.
     .	     nlast-nfirst+1 .ge. istop-istart+1) 
     .		call oerror(n112, m2, 'SEDITS')
c
         NUM=nlast-nfirst+1
         IEND=ISTOP-NUM
         DO 131 I = nfirst,IEND
            TWH(I,IPTWH)=TWH(I+NUM,IPTWH)
  131       CONTINUE
c
         DO 132 I = 1,ISTOP-ISTART+1-num
            XDATA(I)=TWH(I+istart-1,IPTWH)
  132       CONTINUE
c
         CALL MEAN (n1,ISTOP-ISTART+n1-num,XDATA,IDBE,typscn,XMEAN,XRMS)
c
      else
	call oerror(n283, m2, 'SEDITS')
      endif
c
c
      DTWH(C12NI,IPTWH)=DTWH(C12NI,IPTWH)-NUM
      DTWH(C12ST,IPTWH)=XMEAN
      DTWH(C12RMS,IPTWH)=XRMS
c
      GO TO 99
C-----------------------------------------------------------------------
C						TEMPS
C----------------------------------------------------------------------
  170 CONTINUE
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH)+ ISTART-1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'TEMPS')
c
      if (dtwh(c1tel,iptwh) .eq. t12m) then
c
         KK=0
         DO 171 I = ISTART,ISTOP,4
            if (okreal4(TWH(I+1,IPTWH)) .and.
     .           okreal4(TWH(I+2,IPTWH)) .and.
     .           okreal4(TWH(I+3,IPTWH)) .and.
     .           okreal4(TWH(I,IPTWH))) then
               TP=(TWH(I+1,IPTWH)+TWH(I+2,IPTWH)-TWH(I,IPTWH)
     .              -TWH(I+3,IPTWH))/2.
               kk = kk + 1
               TSRC(kk) = TP
               if (mod(kk,6) .eq. 0) then
                  write(cpuf,233,iostat=ier) tsrc
 233              format(1x,1p6g12.5)
                  call pwrite(ipuf,n80)
                  kk = 0
               endif
            endif
 171     CONTINUE
c                     clean up in case kk is non-zero
         if (kk .ne. 0) then
            write(cpuf,233,iostat=ier) (tsrc(i), i=1,kk)
            call pwrite(ipuf, n80)
         endif
c     
      else if(dtwh(c1tel,iptwh) .eq. nrao140 .or. 
     .        dtwh(c1tel,iptwh) .eq. nrao300) then
c
	npairs = istop - istart - 2
	if (.not.okreal4(TWH(ISTOP-1,IPTWH)) .or.
     .	    .not.okreal4(TWH(ISTOP-2,IPTWH)) .or.
     .	    .not.okreal4(TWH(ISTOP,IPTWH)))
     .		call oerror(n268, m2, "TEMPS: Cannot proceed")
        TMP = TWH(ISTOP-1,IPTWH)-(TWH(ISTOP-2,IPTWH)+
     .      TWH(ISTOP,IPTWH))/2.
        SCL = dtwh(c12ct,IPTWH) / TMP
        kk = 0
        DO 231 I = 2,NPAIRS,2
          if (okreal4(TWH(ISTART+I-1,IPTWH)) .and.
     .        okreal4(TWH(ISTART+I-2,IPTWH)) .and.
     .        okreal4(TWH(ISTART+I,IPTWH))) then
		DIFF = TWH(ISTART+I-1,IPTWH)-(TWH(ISTART+I-2,IPTWH)+
     .              TWH(ISTART+I,IPTWH))/2.
	  	kk = kk + 1
          	TSRC(kk) = DIFF*SCL
	  	if (mod(kk,6) .eq. 0) then
		   write(cpuf,233,iostat=ier) tsrc
		   call pwrite(ipuf,n80)
		   kk = 0
		endif
	  endif
 231    CONTINUE
c
        if (kk .ne. 0) then
	   write(cpuf,233,iostat=ier) (tsrc(i), i = 1, kk)
	   call pwrite(ipuf,n80)
        endif
c
      else
	call oerror(n283, m2, 'TEMPS')
      endif
c
        GO TO 99
C------------------------------------------------------------------------
C							SOLVETIP
C-----------------------------------------------------------------------
  190 CONTINUE
c
c
      ISTART=DTWH(C12SPN,IPTWH) + idatoff
      ISTOP=DTWH(C12NI,IPTWH)+ ISTART-1
c
      if (istart .gt. istop .or. istart .le. 0) 
     .		call oerror(n225, m2, 'SOLVETIP')
c
      if(dtwh(c1tel,iptwh) .eq. nrao140 .or. 
     .        dtwh(c1tel,iptwh) .eq. nrao300) then
c
         tcal = dtwh(c12ct, iptwh)
         tamb = dtwh(c5at,iptwh) + 273.16
         npts = nint(dtwh(c12ni, iptwh))
   	 if (mod(npts,3) .ne. 0 .or. npts .gt. 3*ntpts) 
     .			call oerror(n250, m2, 'SOLVETIP')
c
         DO 165 J = 1, npts
	    if (.not. okreal4(TWH(istart - 1 + J,IPTWH)))
     .		call oerror(n268, m2, "SOLVETIP: Cannot proceed")
            TIP(J) = DBLE (TWH(istart - 1 + J, IPTWH))
  165       CONTINUE
c
         WRITE (unit=cpuf,fmt=90000) npts, tamb, tcal
90000    FORMAT ('NPTS, TAMB, TCAL:', 1X,  I5, 2G14.7)
         call pwrite(ipuf,n80) 
c     
         CALL TIPSLV (NPTS,dble(TAMB),dble(TCAL),TIP,secz,tau8,trx8)
	 tau0 = tau8
	 trcvr = trx8
c
      else if (dtwh(c1tel,iptwh) .eq. t12m) then 
c
         npt = (nint(dtwh(c12ni,iptwh)) + 1) / 2
         if (npt .ne. ntpts) call oerror(n265, m2, 'SOLVETIP')
c			verify that there are the correct number of pts
c	
        TAMB=DTWH(C5AT,IPTWH)
        TVAN=TVANE
        IF (TVANE .LE. 0.0) TVAN=TAMB
        FRQ=DTWH(C12CF,IPTWH)*0.001
        TAUG=TAU0
        SUMX=0.
        SUMY=0.
        SUMXY=0.
        SUMX2=0.
        SUMY2=0.
        ITYPE=IFIX(TYPETIP)
        IF (ITYPE .EQ. 1) THEN
           TM=FTM*(TAMB+273.15)
           TSBR=FTSBR*(TAMB+273.15)
           YSBR=(1.0-ETA)*TSBR
           YM=ETA*TM
           IF (TRCVR.le.0.0) then
             TRY=0.9187*DTWH(C12SST,IPTWH)
	   else
             TRY=TRCVR
 	   endif
 	   DO 191 I=1,npt-1
	      if (.not. okreal4(TWH(istart-1+I,IPTWH)) .or.
     .		  .not. okreal4(TWH(istart-1+2*npt-I,IPTWH)))
     .		call oerror(n268, m2, "SOLVETIP: Cannot proceed")
              TSKY(I)=(TWH(istart-1+I,IPTWH)
     .           + TWH(istart-1+2*npt-I,IPTWH))/2.-TRY
  191         CONTINUE
	   if (.not. okreal4(TWH(istart-1+npt,IPTWH)))
     .		call oerror(n268, m2, "SOLVETIP: Cannot proceed")
           TSKY(npt)=TWH(istart-1+npt,IPTWH)-TRY
           DO 192 I=1,npt
              YI(I)=1.0-(TSKY(I)-YSBR)/YM
              IF (YI(I).lt.0.0) call oerror(n234, m2, 'SOLVETIP')
192	      continue
           DO 193 I=1,npt
               YY=YI(I)
               SUMXY=SUMXY+AIRMASS(I)*ALOG(YY)
               SUMX=SUMX+AIRMASS(I)
               SUMY=SUMY+ALOG(YY)
               SUMX2=SUMX2+AIRMASS(I)*AIRMASS(I)
               SUMY2=SUMY2+ALOG(YY)**2
  193          CONTINUE
            EB=-((SUMXY-(SUMX*SUMY)/float(npt))/
     .           (SUMX2-(SUMX*SUMX)/float(npt)))
            EA=EXP(SUMY/float(npt)+EB*(SUMX/float(npt)))
            RS1=(SUMXY-(1./float(npt))*SUMX*SUMY)**2
            RS2=(SUMX2-(SUMX*SUMX)/float(npt))*
     .          (SUMY2-(SUMY*SUMY)/float(npt))
            RS=RS1/RS2
            DO 194 I=1,11
               JJ=I
               IF (JJ.GT.npt) JJ=2*npt-JJ
               TWH(istart-1+I,IPTWH)=TRY+YM*
     .                    (1.0-EXP(-EB*AIRMASS(JJ)))+YSBR
  194          CONTINUE
            TAU=EB
            DELTAU=RS
c
        endif
        IF (ITYPE .EQ. 2 .OR. (ITYPE .EQ. 3 .AND. TAU0 .LE. 0.)) THEN
c
            DO 197 I = 1, npt-1
	      if (.not. okreal4(TWH(istart-1+I,IPTWH)) .or.
     .		  .not. okreal4(TWH(istart-1+2*npt-I,IPTWH)))
     .		call oerror(n268, m2, "SOLVETIP: Cannot proceed")
              YI(I)=(TWH(istart-1+I,IPTWH)+
     .               TWH(istart-1+2*npt-I,IPTWH))/2.
              IF (ITYPE .EQ. 3) YI(I)=LOG(YI(I))
  197         CONTINUE
	    if (.not. okreal4(TWH(istart-1+npt,IPTWH)))
     .		call oerror(n268, m2, "SOLVETIP: Cannot proceed")
            YI(npt)=TWH(istart-1+npt,IPTWH)
            IF (ITYPE .EQ. 3) YI(npt)=LOG(YI(npt))
            DO 198 I = 1,npt
               SUMXY=SUMXY+AIRMASS(I)*YI(I)
               SUMX=SUMX+AIRMASS(I)
               SUMY=SUMY+YI(I)
               SUMX2=SUMX2+AIRMASS(I)*AIRMASS(I)
               SUMY2=SUMY2+YI(I)*YI(I)
  198          CONTINUE
            ZX=SUMX/float(npt)
            ZY=SUMY/float(npt)
            A1=(SUMXY-(SUMX*SUMY)/float(npt))/
     .         (SUMX2-(SUMX*SUMX)/float(npt))
            A0=ZY-A1*ZX
            RS1=SUMXY-(SUMX*SUMY)/float(npt)
            RS2=(SUMX2-(SUMX*SUMX)/float(npt))*
     .          (SUMY2-(SUMY*SUMY)/float(npt))
            RS=(RS1*RS1)/RS2
            IF (ITYPE .EQ. 2) THEN
               DO 199 I = 1,npt
                  TWH(I+istart-1,IPTWH)=A0+A1*AIRMASS(I)
  199             CONTINUE
               DO 299 I = npt+1,2*npt - 1
                  TWH(I+istart-1,IPTWH)=TWH(2*npt-I+istart-1,IPTWH)
  299             CONTINUE
            ENDIF
            EB=ABS(A1)
            TAUG=EB
            TAU=EB
            DELTAU=RS
	 else if (itype .eq. 3 .and. tau0 .gt. 0.) then
c	    Now, take care of the case the user wants to use the supplied value
c	    of adverb TAU0 as a first guess for the non-linear fit.
	    taug = tau0
	    tau =  tau0
	    deltau = 0.0
         endif
         IF (ITYPE.EQ.3) THEN
           DO 195 I = 1,npt-1
	      if (.not. okreal4(TWH(istart-1+I,IPTWH)) .or.
     .		  .not. okreal4(TWH(istart-1+2*npt-I,IPTWH)))
     .		call oerror(n268, m2, "SOLVETIP: Cannot proceed")
              YI(I)=(TWH(istart-1+I,IPTWH)+
     .               TWH(istart-1+2*npt-I,IPTWH))/2.
  195         CONTINUE
	   if (.not. okreal4(TWH(istart-1+npt,IPTWH)))
     .		call oerror(n268, m2, "SOLVETIP: Cannot proceed")
           YI(npt)=TWH(istart-1+npt,IPTWH)
           CALL VSNTIP(AIRMASS,YI,NPT,FRQ,TAMB,TVAN,FTSBR,FTM,TAU,
     .               DELTAU,ETA,DELETA,ETAFREE,NOVCVG,CHISQR)
           DO 196 I = 1,npt
              TWH(istart-1+I,IPTWH)=YI(I)
  196         CONTINUE
           DO 1903 I = npt+1,2*npt - 1
              TWH(I+istart-1,IPTWH)=TWH(2*npt-I+istart-1,IPTWH)
 1903         CONTINUE
        ENDIF
        IF (ITYPE .EQ. 1) THEN
           WRITE (CPUF,2000,IOSTAT=IER) TAU,DELTAU
 2000      FORMAT (16X,'TAU=',F6.4,' (',F6.4,')')
           CALL PWRITE (IPUF,n80)
           WRITE (CPUF,2001,IOSTAT=IER) trcvr, ETA
 2001 	   FORMAT (16X,'TRCVR = ', f7.2, '  ETAL=',F6.4)
           CALL PWRITE (IPUF,n80)
        else IF (ITYPE .EQ. 2) THEN
           WRITE (CPUF,2002,IOSTAT=IER) TAU, DELTAU
 2002      FORMAT (16X,'TAU=',F6.4,' (',F6.4,')')
           CALL PWRITE (IPUF,n80)
        else IF (ITYPE .EQ. 3) THEN
           WRITE (CPUF,2003,IOSTAT=IER) TAU,DELTAU,TAUG
 2003      FORMAT (16X,'TAU=',F6.4,' (',F6.4,')',26X,'TAU0=',F6.4)
           CALL PWRITE (IPUF,n80)
           WRITE (CPUF,2004,IOSTAT=IER) ETA,DELETA,ETAFREE
 2004      FORMAT (15X,'ETAL=',F6.4,' (',F6.4,')',26X,'ETAFREE=',F2.0)
           CALL PWRITE (IPUF,n80)
           WRITE (CPUF,2005,IOSTAT=IER) CHISQR
 2005      FORMAT (15X,'CHISQR=',F10.2)
           CALL PWRITE (IPUF,n80)
        ENDIF
c
      else
	call oerror(n283, m2, 'SOLVETIP')
      endif
C
      GO TO 99
C------------------------------------------------------------------------c

  99  CONTINUE
C-----------------------------------------------------------------------
C
      RETURN
      END
c

