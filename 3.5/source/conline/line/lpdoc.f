      SUBROUTINE pdoc(ifit)
C-----------------------------------------------------------------------
C @(#)lpdoc.f	5.3 09/10/98
C
C-----------------------------------------------------------------------
      integer*2 ipos(4), jpos(4), lstrng(6), dstrng(5), gstrng(5),
     .          str1(4), str2(4), ichsize, ifit, ier, imon, iday, iyr,
     .          il1, il2, il3, lastblnk
      integer*4 IM(3), IS(3), JM(3), JS(3), iptwh, nn, i, iazof, 
     .          ielof, iscan, ifb, itm, its, ihp, ns, isb, nloffset, 
     .          plen, ifeed
      integer*2 m2, n0, n1, n3, n72, n80, n234, n352, n355, n356
      integer*2 n25, n50, n75, n275, n300, n325, n450, n475, n500,
     .          n625, n650, n675, n700, n725, n750, n775, n800
      INTEGER*4 IOBSOPR(2)
      REAL*4 LST, CAZZ(2), CELL(2), s4(4), ln2, factor, eps, php, caz,
     .   cel, azof, elof, hp, az, el, g1, g2, g3, g4, g5, r1, r2, r3, 
     .   r4, r5, s2, x0, x0l, t1, t2, t3, pjy, pjy1, daz, daz1, bw1,
     .   del, del1, bw2, acaz, acel, ut, azchg, elchg, totchg, s_to_n,
     .   hg(4), cg(4), wg(4), a, dt1dx0, dt2dx0, dt3dx0, e, dedx0,
     .   dadx0
      REAL*8 SOURCE(2), DAT, OBSOPR, FRQ
      real*8 pt_model, rx_info(2)
      character*8 cpt_model
      character*16 crx_info
      character*1 ipl, ipr(3), jpr(3), ic(4), imu
      character*1023 pfile
c
      INCLUDE 'appl.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'cio.inc'
c
      EQUIVALENCE (SOURCE,s4)
      EQUIVALENCE (OBSOPR,IOBSOPR)
      equivalence (cpt_model, pt_model)
      equivalence (crx_info, rx_info)
c
      DATA EPS/0.0001/
      DATA IPL/'+'/, IMU/'-'/
      DATA IC/'A','B','C','D'/
      data ichsize /14/
      data m2, n0, n1, n3, n72, n80, n234, n352, n355, n356
     .    /-2,  0,  1,  3,  72, 80,  234,  352,  355,  356/
      data n25, n50, n75, n275, n300, n325, n450, n475, n500
     .     /25,  50,  75,  275,  300,  325,  450,  475,  500/,
     .     n625, n650, n675, n700, n725, n750, n775, n800
     .     /625,  650,  675,  700,  725,  750,  775,  800/
C
C=======================================================================
C
      ln2 = log(2.)
      factor = sqrt(8. * ln2)
c
      WRITE(CPUF,4000,IOSTAT=IER)
      call pfixchar(n1, n800, ipuf, n80, n0, ichsize)
c
      IPTWH = 1
      PHP=1.
      NN=1
      CAZ=0
      CEL=0
c
      DO 231 I = 1,2
         SOURCE(I)=DTWH(C1SNA+I-1,IPTWH)
  231 CONTINUE
c
      OBSOPR=DTWH(C1OBS,IPTWH)
      AZOF=DTWH(C2UXP,IPTWH)
      IAZOF=IFIX(AZOF+SIGN(0.5,AZOF))
      AZOF=FLOAT(IAZOF)
      ELOF=DTWH(C2UYP,IPTWH)
      IELOF=IFIX(ELOF+SIGN(0.5,ELOF))
      ELOF=FLOAT(IELOF)
      LST=DTWH(C3LST,IPTWH)
      CALL RNS(LST,n0,LSTRNG)
      ISCAN=DTWH(C1SNO,IPTWH)
      ITM=nint(DTWH(C3SRT,IPTWH))
      ITS=nint(DTWH(C12SST,IPTWH))
      IFB=nint(abs(DTWH(C12FR,IPTWH)*1000.0))
      hp = dtwh(c7fw, iptwh)
      IHP=IFIX(HP+0.5)
      NS=DTWH(C12IT,IPTWH)/DTWH(C3SRT,IPTWH)
      DAT=DTWH(C3DAT,IPTWH)
      CALL UDAT(DAT,IMON,IDAY,IYR)
      ISB=DTWH(C9SB,IPTWH)
      WRITE (CPUF,4001,IOSTAT=IER) ISCAN,IFB,IYR,IMON,IDAY,
     .               (LSTRNG(I), I=1,5), (s4(I),I=1,3),ITM,
     .               NS,ITS,IFB,ISB,IHP
      call pfixchar(n1, n775, ipuf, n80, n0, ichsize)
      WRITE (CPUF,4002,IOSTAT=IER) DTWH(C12RF,IPTWH)*0.001D0
      call pfixchar(n1, n750, ipuf, n80, n0, ichsize)
      AZ=DTWH(C4AZ,IPTWH)
      EL=DTWH(C4EL,IPTWH)
      CALL RNS(AZ,n3,DSTRNG)
      CALL RNS(EL,n1,GSTRNG)
      WRITE (CPUF,4003,IOSTAT=IER) DSTRNG,GSTRNG,
     .                      DTWH(C9SYN,IPTWH)*0.001D0
      call pfixchar(n1, n725, ipuf, n80, n0, ichsize)
      CALL RN1(AZOF,STR1)
      CALL RN1(ELOF,STR2)
      WRITE (CPUF,4004,IOSTAT=IER) STR1,STR2,DTWH(C7VR,IPTWH)
      call pfixchar(n1, n700, ipuf, n80, n0, ichsize)
      G1=TWH(IDATOFF+1,IPTWH)
      G2=TWH(IDATOFF+3,IPTWH)
      G3=TWH(IDATOFF+2,IPTWH)
      G4=TWH(IDATOFF+5,IPTWH)
      G5=TWH(IDATOFF+4,IPTWH)
      WRITE (CPUF,3000,IOSTAT=IER) 
      call pfixchar(n1, n675, ipuf, n80, n0, ichsize)
      WRITE (CPUF,3004,IOSTAT=IER) G1
      call pfixchar(n1, n650, ipuf, n80, n0, ichsize)
      WRITE (CPUF,3001,IOSTAT=IER) 
      call pfixchar(n1, n500, ipuf, n80, n0, ichsize)
      WRITE (CPUF,3002,IOSTAT=IER) G5,G2,G4
      call pfixchar(n1, n475, ipuf, n80, n0, ichsize)
      WRITE (CPUF,3000,IOSTAT=IER) 
      call pfixchar(n1, n325, ipuf, n80, n0, ichsize)
      WRITE (CPUF,3004,IOSTAT=IER) G3
      call pfixchar(n1, n300, ipuf, n80, n0, ichsize)
      IF (IFIT.EQ.2) then
         R1=TWH(IDATOFF+6,IPTWH)
         R2=TWH(IDATOFF+8,IPTWH)
         R3=TWH(IDATOFF+7,IPTWH)
         R4=TWH(IDATOFF+10,IPTWH)
         R5=TWH(IDATOFF+9,IPTWH)
         WRITE (CPUF,3004,IOSTAT=IER) R1
         call pfixchar(n1, n625, ipuf, n80, n0, ichsize)
         WRITE (CPUF,3002,IOSTAT=IER) R5,R2,R4
         call pfixchar(n1, n450, ipuf, n80, n0, ichsize)
         WRITE (CPUF,3004,IOSTAT=IER) R3
         call pfixchar(n1, n275, ipuf, n80, n0, ichsize)
      endif
c
c  Statement 226 seems to start a loop over the number of fits to 
c  do (e.g., integrated and peak intensity fits).  PRJ  25 Jul 91
c
  226 IF (G1.le.0.0.or.G2.le.0.0.or.G3.le.0.0.or.
     .    G4.le.0.0.or.G5.le.0.0) call oerror(n234, m2, 'PDOC')
C
   21 X0=(PHP/2.)*LOG(G3/G1)/LOG(G2**2/(G1*G3))
      S2=PHP*(2.*X0+PHP)/(2.*LOG(G2/G1))
c
      IF (S2.lt.0.0) call oerror(n234, m2, 'PDOC')
c
  221 WG(1)=FACTOR*SQRT(S2)
      HG(1)=G2*EXP(X0**2/(2.*S2))
      CG(1)=X0+2.
C
      S2=(2.*PHP/FACTOR)**2
   25 X0L=X0
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
      HG(2)=A
      WG(2)=2.
      CG(2)=X0+2.
C
      X0=(PHP/2.)*LOG(G5/G4)/LOG(G2**2/(G5*G4))
      S2=PHP*(2.*X0+PHP)/(2.*LOG(G2/G4))
c
      IF (S2.lt.0.0) call oerror(n234, m2, 'PDOC')
c
  222 WG(4)=FACTOR*SQRT(S2)
      HG(4)=G2*EXP(X0**2/(2.*S2))
      CG(4)=X0+5.
C
      S2=(2.*PHP/FACTOR)**2
   35 X0L=X0
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
      HG(3)=A
      WG(3)=2.
      CG(3)=X0+5.
C
      DAZ=(5.0-CG(3))*HP
      PJY=HG(3)*EXP((2.0-CG(2))**2*ln2)
      DAZ1=(5.0-CG(4))*HP
      PJY1=HG(4)*EXP((2.0-CG(1))**2*4*ln2/WG(1)**2)
      BW1=WG(4)*HP
      nloffset = (nn - 1) * 75
      il1 = 250 - nloffset
      il2 = 225 - nloffset
      il3 = 200 - nloffset
      WRITE (CPUF,4006,IOSTAT=IER) IC(NN*2-1),IC(NN*2)
      call pfixchar(n1, il1, ipuf, n80, n0, ichsize)
      WRITE (CPUF,4007,IOSTAT=IER) DAZ,PJY,DAZ1,PJY1,BW1
      call pfixchar(n1, il2, ipuf, n80, n0, ichsize)
      DEL=(2.0-CG(2))*HP
      PJY=HG(2)*EXP((5.0-CG(3))**2*ln2)
      DEL1=(2.0-CG(1))*HP
      PJY1=HG(1)*EXP((5.0-CG(4))**2*4*ln2/WG(4)**2)
      BW2=WG(1)*HP
      WRITE (CPUF,4008,IOSTAT=IER) DEL,PJY,DEL1,PJY1,BW2
      call pfixchar(n1, il3, ipuf, n80, n0, ichsize)
      CAZZ(NN)=AZOF+(5.0-CG(4))*HP
      CAZ=CAZ+CAZZ(NN)
      CELL(NN)=ELOF+(2.0-CG(1))*HP
      CEL=CEL+CELL(NN)
      IPOS(NN*2-1)=510+IFIX((5.0-CG(3))*182.+0.5)
      JPOS(NN*2-1)=475+IFIX((2.0-CG(1))*175.+0.5)
      IPOS(NN*2)=510+IFIX((5.0-CG(4))*182.+0.5)
      JPOS(NN*2)=475+IFIX((2.0-CG(2))*175.+0.5)
c
      IF (IFIT.EQ.NN) GO TO 250
c
      G1=R1
      G2=R2
      G3=R3
      G4=R4
      G5=R5
      NN=NN+1
      GO TO 226
C
  250 DO 253 I = 1,NN
         ACAZ=ABS(CAZZ(I))
         IM(I)=IFIX(ACAZ/60.)
         IS(I)=IFIX(ACAZ-FLOAT(IM(I))*60.+0.5)
         IPR(I)=IPL
         IF (CAZZ(I).LT.0.0) IPR(I)=IMU
         ACEL=ABS(CELL(I))
         JM(I)=IFIX(ACEL/60.0)
         JS(I)=IFIX(ACEL-FLOAT(JM(I))*60.0+0.5)
         JPR(I)=IPL
         IF (CELL(I).LT.0.0) JPR(I)=IMU
  253 CONTINUE
c
      if (IFIT.EQ.2) then
         CAZ=CAZ/NN
         ACAZ=ABS(CAZ)
         IM(3)=IFIX(ACAZ/60.)
         IS(3)=IFIX(ACAZ-FLOAT(IM(3))*60.+0.5)
         IPR(3)=IPL
         IF (CAZ.LT.0.0) IPR(3)=IMU
         CEL=CEL/NN
         ACEL=ABS(CEL)
         JM(3)=IFIX(ACEL/60.0)
         JS(3)=IFIX(ACEL-FLOAT(JM(3))*60.0+0.5)
         JPR(3)=IPL
         IF (CEL.LT.0.0) JPR(3)=IMU
         WRITE (CPUF,4013,IOSTAT=IER) 
         call pfixchar(n1, n75, ipuf, n80, n0, ichsize)
         WRITE (CPUF,4014,IOSTAT=IER) (IPR(I),IM(I),IS(I),I=1,3)
         call pfixchar(n1, n50, ipuf, n80, n0, ichsize)
         WRITE (CPUF,4015,IOSTAT=IER) (JPR(I),JM(I),JS(I),I=1,3)
         call pfixchar(n1, n25, ipuf, n80, n0, ichsize)
      else
         WRITE (CPUF,4009,IOSTAT=IER) 
         call pfixchar(n1, n75, ipuf, n80, n0, ichsize)
         WRITE (CPUF,4010,IOSTAT=IER) IPR(1),IM(1),IS(1)
         call pfixchar(n1, n50, ipuf, n80, n0, ichsize)
         WRITE (CPUF,4012,IOSTAT=IER) JPR(1),JM(1),JS(1)
         call pfixchar(n1, n25, ipuf, n80, n0, ichsize)
      endif
c
      call getenv('POPSPOINTDAT', pfile)
      plen = lastblnk(pfile)
      if (plen .le. 0) then
         pfile = 'point.dat'
         plen = lastblnk(pfile)
      endif
      write(cpuf, 4017, iostat=ier) pfile(max(1,plen-71):plen)
      call pwrite(cpuf, n80)
      open (unit=iiotmp,file=pfile, status='old', fileopt='eof',
     .      iostat = ier)
      if (ier .ne. 0) then
         call oerror(n352, n0, 'POINT.DAT')
c
      else
         FRQ=DTWH(C12RF,IPTWH)*0.001
         UT=DTWH(C3UT,IPTWH)
         CALL RNS (UT,n0,LSTRNG)
         IHP=IFIX(HP+0.5)
         IFEED = ifix ((dtwh(c1sno,iptwh) - float(iscan)) * 100. + 0.5) 
c
c  Compute the total change in new pointing correction from the
c  initial guess (PRJ  25 Jul 91):
         azchg = caz - azof
         elchg = cel - elof
         totchg = sqrt(azchg*azchg + elchg*elchg)
c  Let the signal-to-noise value be 0.0 for now (PRJ):
         s_to_n = 0.0
         pt_model = dtwh(c2ptm, iptwh)
         rx_info(1) = dtwh(c12rxi, iptwh)
         rx_info(2) = dtwh(c12rxi+1, iptwh)
         WRITE (iiotmp,4016,IOSTAT=IER) ISCAN,SOURCE,IYR,IMON,
     .                   IDAY,FRQ, IOBSOPR(1),IFEED,az,el,caz,
     .                   cel,IHP,BW1,BW2,PJY1,
     .                   ut, DTWH(C5AT,IPTWH), totchg, s_to_n,
     .                   cpt_model, crx_info
         if (ier .ne. 0) call oerror(n355, n0, 'POINT.DAT: PDOC')
         close(iiotmp, iostat=ier)
         if (ier .ne. 0) call oerror(n356, n0, 'POINT.DAT: PDOC')
      endif
c
      DO 251 I = 1,NN*2
         call pfixchar(ipos(i), jpos(i), ic(i), n1, n0, ichsize)
  251    CONTINUE
      call gpflush()
c
   99 CONTINUE
C---------------------------------------------------------------------
 3000 FORMAT (36X,'+')
 3001 FORMAT (23X,'+',12X,'+',12X,'+')
 3002 FORMAT (16X,F11.2,2X,F11.2,2X,F11.2)
 3004 FORMAT (29X,F11.2)
 4000 FORMAT (' SCAN',2X,'FB',4X,'DATE',6X,'LST',
     .        6X,'SOURCE',6X,'SEC',2X,'NS',3X,'TS',4X,'FBR',
     .        2X,'SB',2X,'HP')
 4001 FORMAT (I5,I3,I5,'-',I2.2,'-',I2.2,5A2,2X,3A4,
     .        2I4,2I6,2I4)
 4002 FORMAT(27X,'AZ',9X,'EL',8X,'REST FREQ=',F10.6)
 4003 FORMAT(1X,' TELESCOPE COMMANDED:',5A2,1X,5A2,9X,'SYNTH=',F12.8)
 4004 FORMAT(13X,'MAIN OFF:',2X,4A2,3X,4A2,6X,'VELOCITY=',F7.1)
 4006 FORMAT (' FIT WITH FIXED BW (SYMBOL=',A1,')',8X,
     .        ' NONRESTRICTED FIT (SYMBOL=',A1,')')
 4007 FORMAT(1X,' DAZ=',F5.1,2X,'PEAK=',F7.2,10X,'DAZ=',F5.1,2X,
     .       'PEAK=',F7.2,2X,'BW=',F5.1)
 4008 FORMAT(1X,' DEL=',F5.1,2X,'PEAK=',F7.2,10X,'DEL=',F5.1,2X,
     .       'PEAK=',F7.2,2X,'BW=',F5.1)
 4009 FORMAT (30X,'FIT 1')
 4010 FORMAT (' CORRECTED MAIN OFFSETS:    AZ=',A1,I2.2,':',I2.2)
 4012 FORMAT (28X,'EL=',A1,I2.2,':',I2.2)
 4013 FORMAT (33X,'FIT 1',4X,'FIT 2',6X,'AVG')
 4014 FORMAT (' CORRECTED MAIN OFFSETS:  AZ=  ',A1,I2.2,':',I2.2,4X,
     .        A1,I2.2,':',I2.2,4X,A1,I2.2,':',I2.2)
 4015 FORMAT (26X,'EL=',2X,A1,I2.2,':',I2.2,4X,A1,I2.2,':',I2.2,4X,A1,
     .        I2.2,':',I2.2)
 4016 FORMAT (I5,1X,2A8,I4,'-',I2.2,'-',I2.2,F10.5,1X,A4,I2,F8.3,F7.3,
     .        F7.1,F7.1,I4,F6.1,F6.1,F9.2,F8.4,F6.1,F7.1,F7.1,
     .        1x,a7,1x,a6)
 4017 FORMAT ('PDOC: writing result to ',a)
C---------------------------------------------------------------------
      END

