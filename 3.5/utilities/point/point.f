      PROGRAM POINT
c
c    @(#)point.f	5.1 06/22/94
C
C   Generates input needed for PVLS, using output of pointing scans.
C   Prompts user for necessary information; fits scans either by
C   linearized least-squares Gaussian fit or convolution with
C   Gaussian beam.
C
      integer*4 kfeed,maxdata,hostnm, getlog
      INTEGER*2 ibuf(2560), idir, icp, itp, ipr, ird, ity, jadc, jara,
     .		method, ifeed, j, ipos, iin, istart, istop, ib, jname, 
     .		jj, iascan
      integer*4 i4a,nactual,i4b,iscan, irtn, iargc
      character*2  CON, LSQ, MDIR(2), NSOURE(6), MNEG, MPOS
      character*2 RS1(6), RS2(6), RS3(6), RS4(6), RS5(6), RS6(6), 
     .		RS7(6), RS8(6), rs9(6)
      character*2 cbuf(2560)
      character*3 nbuf
      character*8 projcode, oobs, site, projcod1
      logical*2 otherfiles
      REAL EXTRA(4), HVC(2), PFUDGE(2), P1, P2, P3, XMNRAD, deg, pi, 
     .	   cobs, dobs, scan, hhw1, temp, sext1, afac, ascan, dha, 
     .	   dra, zend, hhw2, tmper1, srtmp1, vhw2, sext3, ddc, srtmp2, 
     .	   tmper2, tcal2, sqerr1, rerr1, sqerr2, rerr2, denom, errsq,
     .	   sigma, deca2, hrang2, aveht, hhw, vhw, twopi, pim, strt, stp,
     .	   vhw1, rscans(32766)
      real*8 dprojcode, doobs, dsite
c
      include 'condappl.inc'
      include 'coneappl.inc'
      include 'concfmt.inc'
c
      EQUIVALENCE (RBUF, IBUF), (cbuf,ibuf)
      equivalence (projcode,dprojcode),(oobs,doobs),(site,dsite)
c
      DATA CON /'CO'/, DEG /57.29578/,
     +   IDIR /1/,
     +   ICP, itp, IPR, IRD, ITY /2, 3, 4, 5, 6/,
     +   JADC, JARA /57, 56/,
     +   KFEED /1/,
     +   LSQ /'LS'/,
     +   MDIR /'H ', 'V '/,
     +   MNEG, MPOS /'- ', '+ '/,
     +   XMNRAD /3437.75/
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
      otherfiles = .false.
      if (iargc() .ne. 0) otherfiles = .true.
c     otherfiles flags whether or not to generate point.pnt and 
c     point.cal files
c
      CALL UINIT
      PI=3.141593
      PIM=-PI
      TWOPI=PI+PI
C
C                   -- Read in user number --
      WRITE (ITY, 1)
      read (ird, 3, err=99) projcod1
      call uppercase(projcod1, projcode)
      if (projcode .eq. 'BYE' ) goto 99
c
      irtn = hostnm(site)
      irtn = getlog(oobs)
      call openaccess(dprojcode, doobs, dsite,irtn)
c
      if (irtn .ne. 0) then
         write (ity,*) 'Trouble with on-line data -- Terminating'
         go to 99
         endif
C
      open(unit=ipr,file='point.log',status='unknown')
      rewind(ipr)
      if (otherfiles) then
	open(unit=icp,file='point.pnt',status='unknown')
      	open(unit=itp,file='point.cal',status='unknown')
	rewind (icp)
	rewind (itp)
      endif
c
C                   -- Read in desired feed --
903   WRITE (ITY, 101)
      read (ird, 103, err=903) ifeed
      IF (IFEED .GT. 0 .AND. IFEED .LT. 17) KFEED = IFEED
C
C                   -- Read in scan range --
      STRT = 0.
      STP = -1.
906   WRITE (ITY, 4)
      READ (ird, 8, err=906) STRT, STP
      IF (STP .LE. 0.) STP = 999999.
C
C                   -- Read in desired fitting technique --
 1000 CONTINUE
      WRITE (ITY, 90000)
      READ (ird, 2) NBUF
      METHOD = 0
      IF (NBUF .EQ. 'LSQ' .or. nbuf .eq. 'lsq') METHOD = 1
      IF (NBUF .EQ. 'CON' .or. nbuf .eq. 'con') METHOD = 2
c
C
      if (method .eq. 2) then
	WRITE (ITY, 90200)
        READ (ird, 90300) HWIDTH
C                   -- Read in HWIDTH if convolution --
        if (hwidth .le. 0.0) then
	  write(ity,*) 'Bad half width!!  Must be > 0.0'
	  goto 1000
        endif
      else if (method .eq. 0) then
         WRITE (ITY, 90100)
         GO TO 1000
      endif
C
 2000 CONTINUE
      IF (METHOD .EQ. 1) WRITE (ipr, 90400)
      IF (METHOD .EQ. 2) WRITE (ipr, 90500)
c
      nactual = 32766
      maxdata=1190
      i4a = 0
      i4b = 32766
      call gcontlist2 (nactual,i4b,rscans,i4a)
      do 20000 j = 1, nactual
          iscan = rscans(j)
          if (iscan .lt. strt .or. iscan.gt.stp) goto 20000
          call gcont( iscan,kfeed,i4a,maxdata,ibuf(1),irtn)
          if (irtn .ne. 0) go to 20000
	    rbuf(ksno) = rbuf(ksno) + float(kfeed)/100.
            ipos = ibuf(kpoc)
            if (ipos.ne.1) then
              if (ipos .eq. 2) then
                  iin = jera
              else if (ipos .eq. 3) then
                  iin = jera + 2
              else if (ipos .eq. 4) then
                  iin = jera + 4
              else if (ipos .eq. 5) then
                  iin = jera+ 6
              else
                  iin = jera + 8
              endif
              rbuf(jera+6) = rbuf(iin)
              rbuf(jera+7) = rbuf(iin+1)
            else
              rbuf(jera+6) = rbuf(klst) - rbuf(jera+2)
              rbuf(jera+7) = rbuf(jera+3)
            endif
c           Moves the coordinates in the observewd system into 
c           OBS H and V
c
   25       IF (IBUF(KSTC).NE.4) GO TO 10000
            IF (IBUF(JFE).NE.KFEED) GO TO 10000
            IF (IBUF(KDIR).EQ.1) IDIR=1
            IF (IBUF(KDIR).EQ.IDIR) GO TO 3000
            IF (IDIR.EQ.1) GO TO 10000
            WRITE (IPR, 7)
            IDIR = 1
            GO TO 10000
C
 3000 CONTINUE
C
C                   -- Protect against negative cal factor --
      IF (RBUF(JCF) .GT. 0.) GO TO 3200
C
         ISTART = IBUF(JBEG)
         ISTOP = IBUF(JEND)
         DO 3100 IB = ISTART, ISTOP
            RBUF(IB) = -RBUF(IB)
 3100       CONTINUE
C
C                   -- Remove baseline and do the fit --
 3200 CONTINUE
      CALL BASELINE
C
      IF (METHOD .EQ. 1) CALL GAUSS
      IF (METHOD .EQ. 2) CALL CONVOLVE
C
C                   -- Print results of fit --
      IF (IABS (IBUF(KDIR)) .GT. 1) GO TO 4600
C
C                   -- Horizontal scans --
      COBS = RBUF(JERA + 6) + RBUF(KHR) * (CENTER - 1.)
     +       / 60. * RBUF(KSRT)
      HVC(1) = RBUF(JH1)
      HVC(2) = RBUF(JH2)
      PFUDGE(1) = RBUF(KHPF)
      PFUDGE(2) = RBUF(KVPF)
      P1 = RBUF(KP1)
      P2 = RBUF(KP2)
      P3 = RBUF(KP3)
      DOBS = COBS - HVC(1)
      IF (METHOD .EQ. 1)
     +   HWIDTH = ABS (RBUF(KHR)) / 60. * HWIDTH * RBUF(KSRT)
     +            * XMNRAD * COS (HVC(2))
      CALL RNS (HVC(1), 0, RS1)
      CALL RNS (COBS, 0, RS2)
      CALL RNS (DOBS, 0, RS3)
      IF (IBUF(KDIR) .LT. 0) GO TO 4500
C
C                   -- First scan (+H) --
         SCAN = RBUF(1)
         DO 4100 JNAME = 1, 6
            NSOURE(JNAME) = CBUF(JNAME + KSNA - 1)
 4100       CONTINUE
         WRITE (IPR, 9) NSOURE, SCAN
         if (otherfiles) WRITE (ICP, 2009) NSOURE
         IDIR = -1
         IF (GOK .GT. 0) GO TO 10000
C
C                   -- Print results of fit --
            WRITE (IPR, 140)
            WRITE (IPR, 141)
            WRITE (IPR, 142) SCAN, MPOS, MDIR(1), RS1, RS2, RS3,
     +                       HWIDTH, HEIGHT
            HHW1 = HWIDTH
            EXTRA(1) = DOBS
            GO TO 10000
C
C                   -- Second scan (-H) --
 4500    CONTINUE
         IF (GOK .GT. 0) GO TO 4550
C
            SCAN = RBUF(1)
            WRITE (IPR, 142) SCAN, MNEG, MDIR(1), RS1, RS2, RS3,
     +                       HWIDTH, HEIGHT
C
            TEMP = .5 * (EXTRA(1) + DOBS)
            EXTRA(1) = TEMP + PFUDGE(1) / 13750.99
            EXTRA(2) = EXTRA(1)+P2/3.437746E3+P1/3.437746E3/COS(HVC(2))
            CALL RNS (TEMP, 0, RS1)
            CALL RNS (EXTRA(1), 0, RS2)
            CALL RNS (EXTRA(2), 0, RS3)
            WRITE (IPR, 143)
            WRITE (IPR, 144) RS1, RS2, RS3
C
            SEXT1 = EXTRA(1)
            AFAC = FLOAT (IBUF(106)) / 8192.
c***           ASCAN = .1 * RBUF(1)
            ascan = rbuf(1)
            ZEND = RBUF(KZEN) * DEG
            DRA = RBUF(KIRA) - RBUF(JARA) + TEMP
            DHA = RBUF(KLST) - RBUF(JARA)
C
C                KEEP HA BETWEEN +12 AND -12
C
      IF(DHA.GE.PI) DHA=DHA-TWOPI
      IF(DHA.LE.PIM) DHA=DHA+TWOPI
C
            CALL RNS (RBUF(JADC), 1, RS4)
            CALL RNS (RBUF(KIRA), 0, RS5)
            CALL RNS (RBUF(KIDC), 1, RS6)
            CALL RNS (RBUF(JARA), 0, RS3)
            CALL RNS (DRA, 0, RS7)
            CALL RNS (DHA, 0, RS2)
            CALL RNS (RBUF(KLST), 0, RS1)
	    call rns (rbuf(kest), 0, rs9)
            WRITE (IPR, 156) (RS1(jj),jj=2,6), (rs9(jj),jj=2,6),
     +                       ibuf(kmon), ibuf(kday), ibuf(kyr), AFAC, 
     +			     ZEND,(RS2(jj),jj=2,6), (RS3(jj),jj=2,6),
     +                       rs5, (RS4(JJ),JJ=1,5),
     +			     (RS6(JJ),JJ=1,5), rs7 
            HHW2 = HWIDTH
            IASCAN = IFIX (ASCAN)
            if (otherfiles) WRITE (ICP, 2158) IASCAN, RS1, RS2, RS4,
     +                         RS7, ZEND, AFAC
            IDIR=2
            GO TO 10000
C                                          Horizontal fit failure.
 4550       EXTRA(1) = SEXT1
            GOK = -1
            IDIR=2
            WRITE (IPR,160)
            GO TO 10000
C
C                   -- Vertical scans --
 4600 CONTINUE
      COBS = RBUF(JERA + 7) + RBUF(KVR) * (CENTER - 1.)
     +       / 60. * RBUF(KSRT)
      HVC(1) = RBUF(JH1)
      HVC(2) = RBUF(JH2)
      PFUDGE(1) = RBUF(KHPF)
      PFUDGE(2) = RBUF(KVPF)
      P1 = RBUF(KP1)
      P2 = RBUF(KP2)
      P3 = RBUF(KP3)
      CALL RNS (HVC(2), 1, RS1)
      CALL RNS (COBS, 1, RS2)
      DOBS = COBS - HVC(2)
      CALL RNS (DOBS, 1, RS3)
      IF (METHOD .EQ. 1)
     +   HWIDTH = ABS (RBUF(KVR)) / 60. * HWIDTH * RBUF(KSRT) * XMNRAD
      IF (IBUF(KDIR) .LT. 0) GO TO 4700
C
C                   -- First scan (+V) --
         IDIR = -2
         IF (GOK.GT.0) GO TO 10000
            SCAN = RBUF(1)
            WRITE (IPR, 145)
            WRITE (IPR, 141)
            WRITE (IPR, 147) SCAN, MPOS, MDIR(2), (RS1(JJ), JJ = 1, 5),
     +                       (RS2(JJ), JJ = 1, 5), (RS3(JJ), JJ = 1, 5),
     +                       HWIDTH, HEIGHT
            VHW1 = HWIDTH
C------------------ CODE FOR BATRALA---------------
      TMPER1 = HERR
      SRTMP1=HEIGHT
C-----------------------------------------------------
            EXTRA(3)=DOBS
            GO TO 10000
C
C                   -- Second scan (-V) --
 4700    CONTINUE
         IF (GOK .GT. 0) GO TO 4750
            SCAN = RBUF(1)
            WRITE (IPR, 147) SCAN, MNEG, MDIR(2), (RS1(JJ), JJ = 1, 5),
     +                       (RS2(JJ), JJ = 1, 5), (RS3(JJ), JJ = 1, 5),
     +                       HWIDTH, HEIGHT
            TEMP = (EXTRA(3)+DOBS) / 2.0
            EXTRA(3) = TEMP + PFUDGE(2)/206264.8
            EXTRA(4) = EXTRA(3) + P3/3.437746E3
            CALL RNS (TEMP,1,RS1)
            CALL RNS (EXTRA(3), 1, RS2)
            CALL RNS (EXTRA(4), 1, RS3)
            WRITE (IPR, 143)
            WRITE (IPR, 146) (RS1(JJ), JJ = 1, 5), (RS2(JJ), JJ = 1, 5),
     +                       (RS3(JJ), JJ = 1, 5)
            VHW2 = HWIDTH
C
            SEXT3 = EXTRA(3)
            ZEND = RBUF(KZEN) * DEG
            DHA = RBUF(KLST) - RBUF(JARA)
C
C                KEEP HA BETWEEN +12 AND -12
C
      IF(DHA.GE.PI) DHA = DHA - TWOPI
      IF(DHA.LE.PIM) DHA = DHA + TWOPI
C
            DDC = RBUF(KIDC) - RBUF(JADC) + TEMP
            AFAC = IBUF(106) / 8192.
c***           ASCAN = .1 * RBUF(1)
	    ascan = rbuf(1)
C
            CALL RNS (RBUF(KIDC), 1, RS6)
            CALL RNS (RBUF(KLST), 0, RS1)
            CALL RNS (DHA, 0, RS2)
            CALL RNS (DDC, 1, RS8)
            CALL RNS (RBUF(JADC), 1, RS4)
            CALL RNS (RBUF(JARA), 0, RS3)
            CALL RNS (RBUF(KIRA), 0, RS5)
	    call rns (rbuf(kest), 0, rs9)
            WRITE (IPR, 159) (RS1(jj),jj=2,6), (rs9(jj),jj=2,6),
     +                       ibuf(kmon), ibuf(kday), ibuf(kyr), AFAC, 
     +			     ZEND,(RS2(jj),jj=2,6), (RS3(jj),jj=2,6),
     +                       rs5, (RS4(JJ),JJ=1,5),
     +			     (RS6(JJ),JJ=1,5), rs8 
            IASCAN = IFIX (ASCAN)
            DECA2 = RBUF(JADC)*DEG
            HRANG2 = DHA*DEG/15.
            SRTMP2 = HEIGHT
            TMPER2 = HERR
            TCAL2 = RBUF(JNT)
            SQERR1=TMPER1*TMPER1
            SQERR2=TMPER2*TMPER2
            RERR1=1. / SQERR1
            RERR2=1. / SQERR2
            DENOM=RERR1+RERR2
            AVEHT=(SRTMP1*RERR1+SRTMP2*RERR2)/DENOM
            ERRSQ= 1. / DENOM
            SIGMA = SQRT(ERRSQ)
            HHW = (HHW1+HHW2)/2.
            VHW = (VHW1+VHW2)/2.
            if (otherfiles) WRITE (itp,2161) NSOURE, IASCAN, DECA2, 
     .            HRANG2, AVEHT, SIGMA, TCAL2, ZEND, HHW, VHW
 2161       FORMAT (6A2,2X,I6,4(2X,F8.4,2X,F8.5))
            if (otherfiles) WRITE (ICP, 2159) IASCAN, RS1, RS2, 
     +                              RS4, RS8,ZEND, AFAC
            IDIR = 1
            GO TO 10000
C                                          Vertical fit failure.
 4750       CONTINUE
            EXTRA(3) = SEXT3
            GOK = -1
            IDIR = 1
            WRITE (IPR, 162)
C
10000       CONTINUE
20000    CONTINUE
C
      WRITE (IPR, 5)
      WRITE (ITY, 6)
C
C
 99   call closeaccess()
      STOP
C------------------------------------------------------------------------
    1 FORMAT (/ 'PROGRAM POINT' 
     +        //'Searches the analysis disk for all pointing scans'
     +        /'under the specified project code and prints pointing'
     +        /'information for the horizontal and vertical directions.'
     +	      //'The ouput will go to a file by the name of point.log;'
     +        /'if the file exists, it will be overwritten.'
     +        //'To continue, type project code (4 chars MIN, 8 Max);',
     +        ' else, to stop, type BYE.')
   2  FORMAT (a3)
   3  FORMAT(a8)
   4  FORMAT (/,'Desired scan range (0.0 implies all scans)',
     .  /'format',/'XXXXXX.  XXXXXX.')
    5 FORMAT (T30, 'End POINT',/ )
    6 FORMAT (1X, 'End POINT')
   7  FORMAT (/'Pointing Aborted')
   8  FORMAT (F7.0,2X,F7.0)
    9 FORMAT (80('_') /,'Source: ', 6A2,
     +        5X, 'First scan: ', F8.2)
  101 FORMAT (/ 'Type in desired feed; 0 implies feed 1')
  102 FORMAT (I2)
  103 FORMAT (I1)
  104 FORMAT (3X, I2)
  105 FORMAT (13X, A1, I2, I3, F5.1, 5X, A1, I2, F6.2, 9X, A1, I2, F6.2)
  110 FORMAT (// 'P1 =', F6.2, ' P2 = ', F6.2, ' P3 = ', F6.2)
  140 FORMAT (/, T25, '-- Horizontal position scans --')
  141 FORMAT (T5, 'Scan', T10, 'Dir', T15, 'Cat position',
     +        T30, 'Obs position', T46, 'Obs - Cat', T57,
     +        'FWHM('')', T70, 'Height')
  142 FORMAT (F8.2, T11, 2A1, T15, 6A2, T30, 6A2, T43, 6A2,
     +        T57, F6.2, T68, 1pg12.5)
  143 FORMAT ('Average offset', T20, 'New LPCs', T40,
     +        'DELTA for PVLS')
  144 FORMAT (6A2, T18, 6A2, T41, 6A2)
  145 FORMAT (/, T25, '-- Vertical position scans --')
  146 FORMAT (2X, 5A2, T18, 5A2, T41, 5A2)
  147 FORMAT (F8.2, T11, 2A1, T17, 5A2, T32, 5A2, T45, 5A2,
     +        T57, F6.2, T68, 1pg12.5)
 150  FORMAT (/,'Using LPCs last determined by POINT:',6A2,2X,5A2)

  156 FORMAT (/,'LST: ', 5A2,'  EST: ', 5a2, '  Date: ',i2,'/',i2,'/',i2,
     +              '  K Value:', F5.2,'  Z dist:', F6.2,/,
     +          'APP HA: ', 5A2,'  APP RA: ', 5A2,'    IND RA:', 6A2,/,
     +          'APP DEC:', 5A2,'  IND DEC:', 5A2,'  DELTA RA:', 6A2)
  159 FORMAT (/,'LST: ', 5A2,'  EST: ', 5a2, '  Date: ',i2,'/',i2,'/',i2,
     +              '  K Value:', F5.2,'  Z dist:', F6.2,/,
     +          'APP HA: ', 5A2,'  APP RA: ', 5A2,'    IND RA:', 6A2,/,
     +          'APP DEC:', 5A2,'  IND DEC:', 5A2,'  DELTA DEC:', 6A2)
 160  FORMAT ('Using last determined horizontal LPC.')
 162  FORMAT ('Using last determined vertical LPC.')
 2009 FORMAT (6A2)
 2158 FORMAT ('R', I5, 4(1X, 6A2), 1x, 2F6.2)
 2159 FORMAT ('D', I5, 4(1X, 6A2), 1x, 2F6.2)
 2160 FORMAT (1X, 6A2,2X,I6,2X,F8.4,2X,F8.5,3(2X,F8.4),2X,F8.5)
90000 FORMAT ('Enter desired fitting technique:'/
     +        '   least squares fit of Gaussian (LSQ), or'/
     +        '   convolution with Gaussian (CON):')
90100 FORMAT ('The fitting technique requested must be either',
     +        ' LSQ or CON.' / 'Please try again:')
90200 FORMAT ('Enter the full width at half maximum of the convolving',
     +        ' beam (F5.0):')
90300 FORMAT (F5.0)
90400 FORMAT ( T30, '*** Program POINT ***'/
     +        'Source positions calculated by linearized',
     +        ' least-squares fit of Gaussian beam')
90500 FORMAT ( T40, '*** Program POINT ***'/
     +        'Source positions calculated by convolution',
     +        ' with upper half of Gaussian beam')
      END
C
