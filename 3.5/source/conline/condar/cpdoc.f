      subroutine pdoc
C-------------------------------------------------------------------------------
C  @(#)cpdoc.f	5.2 09/10/98
C-------------------------------------------------------------------------------
C
      INTEGER*2 LSTRNG(6),DSTRNG(5),GSTRNG(5),STR1(4),STR2(4),
     .          STR5(4), STR6(4),STR7(4),STR8(4), ier, icn, icf, iptwh, 
     .          iscan, imon, iday, iyear, i, im, is, nn, ibw, jm, js, 
     .          ihp, ipos, jpos, ichsize, lastblnk, plen
      integer*2 m2, n0, n1, n3, n80, n234, n352, n355, n356
      integer*2 n50, n75, n100, n150, n175, n200, n250, n275, n300,
     .          n425, n450, n475, n600, n625, n650, n675, 
     .          n700, n725, n775, n800
      REAL LST,MBMA, MBME, MBAOF, MBEOF, S4(4), eps, factor, time, hp, 
     .	   az, el, pbma, pbaof, pbme, pbeof, azof, azo, elo, elof, ff,
     .     g1, r1, g5, g2, g4, r5, r2, r4, g3, r3, php, s2, x0l, x0, t1,
     .     t2, t3, wg(4), cg(4), hg(4), a, dt1dx0, dt2dx0, dt3dx0, 
     .	   dadx0, e, dedx0, daz, pjy, del, caz, acaz, cel, acel, bw1,
     .	   bw2, frq, ut, azchg, elchg, totchg, s_to_n, ln2, nt,
     .     apscale
      character*1 IPL,IMU,IPR,JPR
      character*1023 pfile
      REAL*8 SOURCE(2), OBSOPR, UDATE, rx_info(2), pt_model
      character*16 crx_info
      character*8 cpt_model
      INTEGER*4 IFEED, IOBSOPR(2)
c
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
      INCLUDE 'cio.inc'
c
      EQUIVALENCE (OBSOPR,IOBSOPR)
      EQUIVALENCE (SOURCE,S4)
      equivalence (cpt_model, pt_model)
      equivalence (crx_info, rx_info)
c
      DATA EPS/0.0001/
      DATA IPL/'+'/, IMU/'-'/, IPR/' '/, JPR/' '/
      DATA ICN/'N '/, ICF/'F '/
      data ichsize /14/
c
      data m2, n0, n1, n3, n80, n234, n352, n355, n356
     .    /-2, 0, 1, 3, 80, 234, 352, 355, 356/
      data n50, n75, n100, n150, n175, n200, n250, n275, n300
     .     /50, 75, 100, 150, 175, 200, 250, 275, 300/, 
     .     n425, n450, n475, n600, n625, n650, n675
     .     /425, 450, 475, 600, 625, 650, 675/,
     .     n700, n725, n775, n800
     .     /700, 725, 775, 800/
C
C=======================================================================
C
      ln2 = log(2.)
      factor = sqrt(8. * ln2)
c
      WRITE(CPUF,4000,IOSTAT=IER)
      call pfixchar(n1,n800,ipuf,n80,n0,ichsize)
c
      IPTWH = 1
      ISCAN=DTWH(C1SNO,IPTWH)
      IFEED = ifix ((dtwh(c1sno,iptwh) - float(iscan)) * 100. + 0.5)
      UDATE=DTWH(C3DAT,IPTWH)
      CALL UDAT(UDATE,IMON,IDAY,IYEAR)
      LST=DTWH(C3LST,IPTWH)
      CALL RNS(LST,0,LSTRNG)
      DO 231 I = 1,2
         SOURCE(I)=DTWH(C1SNA+I-1,IPTWH)
  231    CONTINUE
      TIME=DTWH(C3SRT,IPTWH)*DTWH(C12NI,IPTWH)
      IM=IFIX(TIME/60.)
      IS=IFIX(TIME-FLOAT(IM)*60.0+0.5)
      HP=DTWH(C7FW,IPTWH)
c
      WRITE(CPUF,4001,IOSTAT=IER) ISCAN,IFEED,IYEAR,IMON,IDAY,
     .               (LSTRNG(I), I=1,5), (S4(NN),NN=1,3),
     .               DTWH(C3SRT,IPTWH),IM,IS,DTWH(C12SST,IPTWH),
     .               DTWH(C5AT,IPTWH),HP
      call pfixchar(n1,n775,ipuf,n80,n0,ichsize)
c
      WRITE(CPUF,4002,IOSTAT=IER)
      call pfixchar(n1,n725,ipuf,n80,n0,ichsize)
c
      AZ=DTWH(C4AZ,IPTWH)
      CALL RNS(AZ,n3,DSTRNG)
      EL=DTWH(C4EL,IPTWH)
      CALL RNS(EL,n1,GSTRNG)
      PBMA=DTWH(C9PBM,IPTWH)
      PBAOF=PBMA+SIGN(0.49,PBMA)
      CALL RN1(PBAOF,STR5)
      PBME=DTWH(C9PBM+1,IPTWH)
      PBEOF=PBME+SIGN(0.49,PBME)
      CALL RN1 (PBEOF,STR6)
c
      WRITE(CPUF,4003,IOSTAT=IER) DSTRNG,GSTRNG,STR5,STR6
      call pfixchar(n1,n700,ipuf,n80,n0,ichsize)
c
      AZO = DTWH(C2UXP,IPTWH)
      AZOF=AZO + SIGN(0.49,AZO)
      CALL RN1(AZOF,STR1)
      ELO = DTWH(C2UYP,IPTWH)
      ELOF=ELO + SIGN(0.49,ELO)
      CALL RN1(ELOF,STR2)
      MBMA=DTWH(C9MBM,IPTWH)
      MBAOF=MBMA+SIGN(0.49,MBMA)
      CALL RN1 (MBAOF,STR7)
      MBME=DTWH(C9MBM+1,IPTWH)
      MBEOF=MBME+SIGN(0.49,MBME)
      CALL RN1 (MBEOF,STR8)
c
      WRITE(CPUF,4004,IOSTAT=IER) STR1,STR2,STR7,STR8
      call pfixchar(n1,n675,ipuf,n80,n0,ichsize)
c
      WRITE(CPUF,3000,IOSTAT=IER)
      call pfixchar(n1,n650,ipuf,n80,n0,ichsize)
c			get the noise tube value
      NT=DTWH(C12FR,IPTWH)
c                       in the following 24.41 = 2 * k / (pi * (6**2)) 
c                       where k = boltzman = 1.381 * 10**3 Jy m**2 / K 
      if (nt .ne. 2) then 
         FF=24.41/DTWH(C8AAE,iptwh)
      else 
c                       vane calibration is slightly more complicated 
         apscale = dtwh(c8aae,iptwh) / 
     .                    (dtwh(c8el,iptwh) * dtwh(c8ef,iptwh))
         FF=24.41/apscale
      endif 
c
      G1=TWH(IDATOFF+1,IPTWH)*FF
c
      WRITE(CPUF,3002,IOSTAT=IER) G1
      call pfixchar(n1,n625,ipuf,n80,n0,ichsize)
c
      R1=TWH(IDATOFF+6,IPTWH)*FF
c
      WRITE(CPUF,3002,IOSTAT=IER) R1
      call pfixchar(n1,n600,ipuf,n80,n0,ichsize)
c
      WRITE(CPUF,3001,IOSTAT=IER)
      call pfixchar(n1,n475,ipuf,n80,n0,ichsize)
c
      G5=TWH(IDATOFF+4,IPTWH)*FF
      G2=TWH(IDATOFF+3,IPTWH)*FF
      G4=TWH(IDATOFF+5,IPTWH)*FF
c
      WRITE(CPUF,3003,IOSTAT=IER) G5,G2,G4
      call pfixchar(n1,n450,ipuf,n80,n0,ichsize)
c
      R5=TWH(IDATOFF+9,IPTWH)*FF
      R2=TWH(IDATOFF+8,IPTWH)*FF
      R4=TWH(IDATOFF+10,IPTWH)*FF
c
      WRITE(CPUF,3003,IOSTAT=IER) R5,R2,R4
      call pfixchar(n1,n425,ipuf,n80,n0,ichsize)
c
      WRITE(CPUF,3000,IOSTAT=IER)
      call pfixchar(n1,n300,ipuf,n80,n0,ichsize)
c
      G3=TWH(IDATOFF+2,IPTWH)*FF
c
      WRITE(CPUF,3002,IOSTAT=IER) G3
      call pfixchar(n1,n275,ipuf,n80,n0,ichsize)
c
      R3=TWH(IDATOFF+7,IPTWH)*FF
c
      WRITE(CPUF,3002,IOSTAT=IER) R3
      call pfixchar(n1,n250,ipuf,n80,n0,ichsize)
c
      IF (G1.le.0.0.or.G2.le.0.0.or.G3.le.0.0.or.
     .    G4.le.0.0.or.G5.le.0.0) call oerror(n234, m2, 'PDOC')
C
      PHP=1.
      X0=(PHP/2.)*ALOG(G3/G1)/ALOG(G2**2/(G1*G3))
      S2=PHP*(2.*X0+PHP)/(2.*ALOG(G2/G1))
c
      IF (S2.lt.0.0) call oerror(n234, m2, 'PDOC')

      WG(1)=FACTOR*SQRT(S2)
      HG(1)=G2*EXP(X0**2/(2.*S2))
      CG(1)=X0+2.
      S2=(2.*PHP/FACTOR)**2
C
   25   X0L=X0
        T1=EXP(-(PHP+X0)**2/(2.*S2))
        T2=EXP(-X0**2/(2.*S2))
        T3=EXP(-(PHP-X0)**2/(2.*S2))
        A=(G1+G2+G3)/(T1+T2+T3)
        E=(PHP+X0)*T1*(G1-A*T1)+X0*T2*(G2-A*T2)-(PHP-X0)*T3
     .   *(G3-A*T3)
        DT1DX0=-2.*(PHP+X0)*T1/(2.*S2)
        DT2DX0=-2.*X0*T2/(2.*S2)
        DT3DX0=2.*(PHP-X0)*T3/(2.*S2)
        DADX0=-A*(DT1DX0+DT2DX0+DT3DX0)/(T1+T2+T3)
        DEDX0=T1*(G1-A*T1)+(PHP+X0)*DT1DX0*(G1-A*T1)
     .      -(PHP+X0)*T1*(DADX0*T1+A*DT1DX0)
        DEDX0=DEDX0+T2*(G2-A*T2)+X0*DT2DX0*(G2-A*T2)
     .      -X0*T2*(DADX0*T2+A*DT2DX0)
        DEDX0=DEDX0+T3*(G3-A*T3)+(X0-PHP)*DT3DX0*(G3-A*T3)
     .      -(X0-PHP)*T3*(DADX0*T3+A*DT3DX0)
        X0=X0-E/DEDX0
      IF (ABS(X0-X0L).GT.EPS) GO TO 25
c
      HG(2)=A
      WG(2)=2.
      CG(2)=X0+2.
      X0=(PHP/2.)*ALOG(G5/G4)/ALOG(G2**2/(G5*G4))
      S2=PHP*(2.*X0+PHP)/(2.*ALOG(G2/G4))
c
      IF (S2.lt.0.0) call oerror(n234, m2, 'PDOC')
c
      WG(4)=FACTOR*SQRT(S2)
      HG(4)=G2*EXP(X0**2/(2.*S2))
      CG(4)=X0+5.
      S2=(2.*PHP/FACTOR)**2
c
   35    X0L=X0
         T1=EXP(-(PHP+X0)**2/(2.*S2))
         T2=EXP(-X0**2/(2.*S2))
         T3=EXP(-(PHP-X0)**2/(2.*S2))
         A=(G4+G2+G5)/(T1+T2+T3)
         E=(PHP+X0)*T1*(G4-A*T1)+X0*T2*(G2-A*T2)-(PHP-X0)*T3
     .   *(G5-A*T3)
         DT1DX0=-2.*(PHP+X0)*T1/(2.*S2)
         DT2DX0=-2.*X0*T2/(2.*S2)
         DT3DX0=2.*(PHP-X0)*T3/(2.*S2)
         DADX0=-A*(DT1DX0+DT2DX0+DT3DX0)/(T1+T2+T3)
         DEDX0=T1*(G4-A*T1)+(PHP+X0)*DT1DX0*(G4-A*T1)
     .      -(PHP+X0)*T1*(DADX0*T1+A*DT1DX0)
         DEDX0=DEDX0+T2*(G2-A*T2)+X0*DT2DX0*(G2-A*T2)
     .      -X0*T2*(DADX0*T2+A*DT2DX0)
         DEDX0=DEDX0+T3*(G5-A*T3)+(X0-PHP)*DT3DX0*(G5-A*T3)
     .      -(X0-PHP)*T3*(DADX0*T3+A*DT3DX0)
         X0=X0-E/DEDX0
      IF (ABS(X0-X0L).GT.EPS) GO TO 35
c
      WRITE(CPUF,4006,IOSTAT=IER)
      call pfixchar(n1,n200,ipuf,n80,n0,ichsize)
c
      DAZ=(5.0-CG(4))*HP
      PJY=HG(4)*EXP(((2.0-CG(1))**2)*4*ln2/WG(1)**2)
      BW1=WG(4)*HP
c
      WRITE(CPUF,4007,IOSTAT=IER) DAZ,PJY,BW1
      call pfixchar(n1,n175,ipuf,n80,n0,ichsize)
c
      DEL=(2.0-CG(1))*HP
      PJY=HG(1)*EXP(((5.0-CG(4))**2)*4*ln2/WG(4)**2)
      BW2=WG(1)*HP
c
      IPR=IPL
      AZO = DTWH(C2UXP,IPTWH)
c			follows change of 19 jun 91 in old tuc pops
      azof = azo
c			CAZ is the tot. corrected Az. offset in (")
      CAZ=AZOF+(5.0-CG(4))*HP
      ACAZ=ABS(CAZ)
      IF (CAZ.LT.0.0) IPR=IMU
      IM=IFIX(ACAZ/60.)
      IS=IFIX(ACAZ-FLOAT(IM)*60.0+0.5)
c
      JPR=IPL
      ELO = DTWH(C2UYP,IPTWH)
c			follows change of 19 jun 91 in old tuc pops
      elof = elo
c			CEL is the tot. corrected El. offset in (")
      CEL=ELOF+(2.0-CG(1))*HP
      ACEL=ABS(CEL)
      IF (CEL.LT.0.0) JPR=IMU
      JM=IFIX(ACEL/60.0)
      JS=IFIX(ACEL-FLOAT(JM)*60.0+0.5)
c      		Compute the total change in new pointing correction
c		from the initial guess (follows 19jun91 change in old pops)
      azchg = caz - azof
      elchg = cel - elof
      totchg = sqrt(azchg*azchg + elchg*elchg)
c
c
      WRITE(CPUF,4008,IOSTAT=IER) DEL,PJY,BW2,IPR,IM,IS,JPR,JM,JS
      call pfixchar(n1,n150,ipuf,n80,n0,ichsize)
c
      IBW=IFIX(HP*2.0+0.5)
c
      WRITE(CPUF,4010,IOSTAT=IER) IBW
      call pfixchar(n1,n100,ipuf,n80,n0,ichsize)
c
      CG(3)=X0+5.
      DAZ=(5.0-CG(3))*HP
      HG(3)=A
      PJY=HG(3)*EXP((2.0-CG(2))**2*ln2)
c
      WRITE(CPUF,4011,IOSTAT=IER) DAZ,PJY
      call pfixchar(n1,n75,ipuf,n80,n0,ichsize)
c
      DEL=(2.0-CG(2))*HP
      PJY=HG(2)*EXP((5.0-CG(3))**2*ln2)
c
      WRITE(CPUF,4012,IOSTAT=IER) DEL,PJY
      call pfixchar(n1,n50,ipuf,n80,n0,ichsize)
c
      call getenv('POPSPOINTDAT',pfile)
      plen = lastblnk(pfile)
      if (plen .le. 0) then
         pfile = 'point.dat'
         plen = lastblnk(pfile)
      endif
      write(cpuf,4013,iostat=ier) pfile(max(1,plen-71):plen)
      call pwrite(cpuf, n80)
      open(unit=iiotmp,file=pfile, status='old', fileopt='eof', 
     .     iostat=ier)
      if (ier .ne. 0) then 
         call oerror(n352, n0, 'POINT.DAT')
c
      else
         OBSOPR = DTWH(C1OBS,IPTWH)
         FRQ=DTWH(C12CF,IPTWH)*0.001
         UT=DTWH(C3UT,IPTWH)
         CALL RNS (UT,n0,LSTRNG)
         IHP=IFIX(HP+0.5)
c		Let signal-to-noise value be 0.0 for now
         s_to_n = 0.0
         rx_info(1) = dtwh(c12rxi,iptwh)
         rx_info(2) = dtwh(c12rxi+1,iptwh)
         pt_model = dtwh(c2ptm, iptwh)
         WRITE (iiotmp,4015,IOSTAT=IER) ISCAN,SOURCE,IYEAR,IMON,
     .                   IDAY,FRQ, IOBSOPR(1),IFEED, az, el, caz,
     .                   cel, IHP, BW1, BW2, PJY, 
     .                   ut, DTWH(C5AT, IPTWH), totchg, s_to_n,
     .                   cpt_model, crx_info
c
c
         if (ier .ne. 0) call oerror(n355, n0, 'POINT.DAT : PDOC')
         close(iiotmp,iostat=ier)
         if (ier .ne. 0) call oerror(n356, n0, 'POINT.DAT : PDOC')
      endif
c
      IPOS=510+IFIX((5.0-CG(3))*182.+0.5)
      JPOS=475+IFIX((2.0-CG(2))*175.+0.5)
      call pfixchar(ipos,jpos,icf,n1,n0,ichsize)
      IPOS=510+IFIX((5.0-CG(4))*182.+0.5)
      JPOS=475+IFIX((2.0-CG(1))*175.+0.5)
      call pfixchar(ipos,jpos,icn,n1,n0,ichsize)
c		flush the graphics
      call gpflush()
c
      return
c
 3000 FORMAT (36X,'+')
 3001 FORMAT (23X,'+',12X,'+',12X,'+')
 3002 FORMAT (29X,F11.2)
 3003 FORMAT (16X,F11.2,2X,F11.2,2X,F11.2)
 4000 FORMAT (' SCAN',2X,'#CH',4X,'DATE',5X,'LST',
     .        6X,'SOURCE',8X,'SEC',3X,'TIME',3X,'TS',2X,'TAMB',2X,'HP')
 4001 FORMAT (I5,I3,I5,'-',I2.2,'-',I2.2,1X,5A2,2X,3A4,1X,
     .        F4.1,I4,':',I2,F6.0,F5.1,F5.0)
 4002 FORMAT(27X,'AZ',9X,'EL')
 4003 FORMAT(1X,' TELESCOPE COMMANDED:',5A2,1X,5A2,4X,'+BEAM:'4A2,
     .       2X,4A2)
 4004 FORMAT(9X,'POINTING OFF:',2X,4A2,3X,4A2,4X,'-BEAM:',4A2,2X,4A2)
 4006 FORMAT(1X,' FIT WITH NO RESTRICTIONS  (SYMBOL=N)',15x,
     .       'CORRECTED OFFSETS')
 4007 FORMAT(1X,' DAZ=',F5.1,2X,'PEAK(JY)=',F7.2,2X,'BW=',F5.1,
     .       18X,'AZ',4X,'EL')
 4008 FORMAT(1X,' DEL=',F5.1,2X,'PEAK(JY)=',F7.2,2X,'BW=',F5.1,
     .       15x,A1,I2.2':',I2.2,1X,A1,I2.2,':',I2.2)
 4010 FORMAT('  FIT WITH AZBW=ELBW=',I3,4X,'(SYMBOL=F)')
 4011 FORMAT(1X,' DAZ=',F5.1,2X,'PEAK(JY)=',F7.2)
 4012 FORMAT(1X,' DEL=',F5.1,2X,'PEAK(JY)=',F7.2)
 4013 format('PDOC: writing result to ',a)
 4015 FORMAT (I5,1X,2A8,I4,'-',I2.2,'-',I2.2,F10.5,1X,A4,I2,F8.3,F7.3,
     .        F7.1,F7.1,I4,F6.1,F6.1,F9.2,F8.4,F6.1,F7.1,F7.1,
     .        1x,a7,1x,a6)
C
      end
c
