      subroutine header
C-------------------------------------------------------------------------------
C  @(#)header.F	5.3 09/10/98
C-------------------------------------------------------------------------------
c 
c     Prints either GB or 12M header
c
c************************************
      INTEGER*4 ISTC(2), IOBSOPR(2), ivdef(2), bsp
      INTEGER*2 STR1(4), STR2(4), STR5(4), STR6(4), STR7(4), STR8(4),
     .		RSTRNG2(6), DSTRNG3(6), estrng(6), lstrng(6), ipuf(40),
     .		imon, iday, iyear, ier, imonth, rrstr(6), rdstr(6)
      integer*2 m2, n0, n1, n2, n3, n80, n283
      integer*4 iptwh, i, j, icdtype 
      real*8  T03, ctype(8), t12m, nrao140, nrao300, vdef, SOURCE(2), 
     .	      OBSNAME(2), STC, OBSOPR, UDATE, digit, dfform(3), 
     .	      dcoordcode
      real*4 lst, ut, ra, dec, az, el, sx, sy, rx, ry
      character*80 cpuf
      character*24 cfform
      character*12 crstrng2, cdstrng3 , crrstr, crdstr
      character*8 coordcode
      character*4 estr, estr2, xhdr, yhdr
c
#ifdef CONTINUUM
      real*4 atm, pbma, pbaof, pbme, pbeof, eff, mbma, mbaof, mbme,
     .	     mbeof, azo, azof, elo, elof, time, tsys, noiset 
      integer*2 isamps, im, is, imm, iss, ieff, nch, gstrng(6)
      real*8 bw
#else
      integer*2 STR3(4), STR4(4), lastblnk
      integer*4 ns, iosb(2)
      real*4 rxoff, ryoff, sxoff, syoff, gb, gl, tintg, cosd, 
     .	     rrao, rdco, pbm(2), mbm(2), rao, dco, effint
      real*8 l0, l1, l2, if0, dinfinity, dosb
      character*5 vstr
      logical okreal8
      equivalence(iosb, dosb)
#endif
c
      include 'cform.inc'
      include 'appl.inc'
      include 'core.inc'
c
      equivalence (ivdef,vdef), (cfform,dfform), (dcoordcode, coordcode)
      equivalence (cpuf, ipuf), (stc,istc), (obsopr,iobsopr)
      equivalence (rstrng2, crstrng2), (dstrng3, cdstrng3)
      equivalence (rrstr, crrstr), (rdstr, crdstr)
c
      DATA BSP/'BSP '/
      data nrao140/'NRAO 43M'/,nrao300/'NRAO 93M'/,t12m/'NRAO 12M'/
      data T03 /0.001d0/, digit/'DIGITAL '/
c
      data ctype/'1950RADC','EPOCRADC','MEANRADC','APPRADC','APPHADC',
     2		 'USERDEF','2000RADC','INDRADC'/
c
      data m2, n0, n1, n2, n3, n80, n283
     .     /-2, 0, 1, 2, 3, 80, 283/
c
      iptwh = 1
c
      if(dtwh(c1tel,iptwh).eq.nrao140 .or. 
     .		dtwh(c1tel,iptwh).eq.nrao300) goto 20
      if(dtwh(c1tel,iptwh).ne.t12m) call oerror(n283, m2, 'HEADER')
c
      DO 11 I = 1,2
         SOURCE(I)=DTWH(C1SNA+I-1,IPTWH)
         OBSNAME(I)=DTWH(C1ONA+I-1,IPTWH)
   11    CONTINUE
      stc = dtwh(c1stc, iptwh)
      obsopr = dtwh(c1obs,iptwh)
c
      WRITE(CPUF,1000,IOSTAT=IER)
      CALL PWRITE(IPUF,n80)
c
      WRITE(CPUF,1001,IOSTAT=IER) dtwh(c1sno,iptwh),DTWH(C1TEL,IPTWH),
     .                     SOURCE,IOBSOPR(1)
      CALL PWRITE(IPUF,n80)
c
      WRITE(CPUF,1000,IOSTAT=IER)
      CALL PWRITE(IPUF,n80)
c
      WRITE(CPUF,1002,IOSTAT=IER)
      CALL PWRITE(IPUF,n80)
c
      UDATE=DTWH(C3DAT,IPTWH)
      CALL UDAT(UDATE,IMON,IDAY,IYEAR)
      LST  = DTWH (C3LST,IPTWH)
      CALL RNS (LST,n0,LSTRNG)
      UT   = DTWH (C3UT,IPTWH)
      CALL RNS (UT,n0,ESTRNG)
c
      WRITE(CPUF,1003,IOSTAT=IER) IYEAR,IMON,IDAY,LSTRNG,ESTRNG,
     .                     ISTC(2),IOBSOPR(2)
      CALL PWRITE(IPUF,n80)
c
      dcoordcode = dtwh(c4csc,iptwh)
      estr = coordcode(1:4)
      if (coordcode(1:3) .eq. "GAL") estr = 'GAL '
      if (coordcode(1:3) .eq. "APP") estr = 'APP '
      if (coordcode(1:3) .eq. "IND") estr = 'IND '
      estr2 = estr
      sx = dtwh(c4sx, iptwh)
      sy = dtwh(c4sy, iptwh)
      rx = dtwh(c4rx, iptwh)
      ry = dtwh(c4ry, iptwh)
      do 100 icdtype = 1, 8
         if (dcoordcode .eq. ctype(icdtype)) goto 110
 100  continue
 110  if (coordcode .eq. 'USERDEF ' .or. icdtype .gt. 8 .and.
     .    coordcode .ne. 'GALACTIC') then
         if (coordcode .eq. 'AZEL    ') then
c			AZEL gets displayed elswhere, be creative
            sx = dtwh(c4era, iptwh)
            sy = dtwh(c4edc, iptwh)
            xhdr = ' RA '
            yhdr = ' DEC'
            estr = 'EPOC'
            if (sx .lt. 0) sx = sx + 360.0
            if (sx .ge. 360) sx = sx - 360.0
            call rns(sx, n2, rstrng2)
            call rns(sy, n1, dstrng3)
         else
            xhdr = '  X '
            yhdr = '  Y '
            write(crstrng2,1100) sx
            write(cdstrng3,1105) sy
         endif
         write(crrstr,1100) rx
         write(crdstr,1105) ry
      else
         xhdr = ' RA '
         yhdr = ' DEC'
         if (coordcode .eq. 'GALACTIC') then
#ifdef CONTINUUM
            xhdr = 'GLON'
            yhdr = 'GLAT'
            estr = '    '
            if (sx .lt. 0) sx = sx + 360.0
            if (sx .ge. 360) sx = sx - 360.0
            call rns(sx, n3, rstrng2)
#else
c			galactic is displayed later, be creative
            sx = dtwh(c4era, iptwh)
            sy = dtwh(c4edc, iptwh)
            estr = 'EPOC'
            if (sx .lt. 0) sx = sx + 360.0
            if (sx .ge. 360) sx = sx - 360.0
            call rns(sx, n2, rstrng2)
            write(crrstr,1100) rx
            write(crdstr,1105) ry
#endif
         else
            if (coordcode .eq. 'APPHADC ') xhdr = ' HA '
            if (sx .lt. 0) sx = sx + 360.0
            if (sx .ge. 360) sx = sx - 360.0
            call rns(sx, n2, rstrng2)
            if (rx .lt. 0) rx = rx + 360.0
            if (rx .ge. 360) rx = rx - 360.0
            call rns(rx, n2, rrstr)
            call rns(ry, n1, rdstr)
         endif
         call rns(sy, n1, dstrng3)
      endif
c
#ifdef CONTINUUM
      AZ=DTWH(C4AZ,IPTWH)
      CALL RNS(AZ,n3,GSTRNG)
      EL=DTWH(C4EL,IPTWH)
      CALL RNS(EL,n1,ESTRNG)
c
      WRITE(CPUF,1005,IOSTAT=IER) xhdr, estr, yhdr
      CALL PWRITE(IPUF,n80)
c
      WRITE(CPUF,2000,IOSTAT=IER) RSTRNG2,(DSTRNG3(i),i=1,5),
     .				(GSTRNG(i),i=1,5),(ESTRNG(I),I=1,5)
      CALL PWRITE (IPUF,n80)
c
      WRITE(CPUF,2001,IOSTAT=IER)
      CALL PWRITE(IPUF,n80)
c
      ATM=1./SIN(EL/57.29578)
      ISAMPS = DTWH(C12NI,IPTWH)
      TIME=DTWH(C3SRT,IPTWH)*isamps
      IM=TIME/60.
      IS=TIME-FLOAT(IM)*60.
      IMM=DTWH(C9OT,IPTWH)/60.
      ISS=DTWH(C9OT,IPTWH)-FLOAT(IMM)*60.
c
      WRITE(CPUF,2002,IOSTAT=IER) DTWH(C12SST,IPTWH),DTWH(C12CT,IPTWH),
     . 		 DTWH(C12WO,IPTWH),ATM,DTWH(C5AT,IPTWH),ISAMPS,
     .		 DTWH(C3SRT,IPTWH), IM,IS,IMM,ISS
      CALL PWRITE(IPUF,n80)
c
      WRITE(CPUF,2003,IOSTAT=IER)
      CALL PWRITE(IPUF,n80)
c
      PBMA=DTWH(C9PBM,IPTWH)
      PBAOF=PBMA+SIGN(0.49,PBMA)
      CALL RN1(PBAOF,STR5)
      PBME=DTWH(C9PBM+1,IPTWH)
      PBEOF=PBME+SIGN(0.49,PBME)
      CALL RN1(PBEOF,STR6)
      IM=DTWH(C7FW,IPTWH)/60.
      IS=DTWH(C7FW,IPTWH)-FLOAT(IM)*60.
      EFF=DTWH(C8AAE,IPTWH)*100.
      IEFF=IFIX(EFF+0.5)
c
      WRITE(CPUF,2004,IOSTAT=IER) STR5,STR6,im,is, ieff, 
     .		nint(DTWH(C12FR,IPTWH)), nint(DTWH(C12RF,IPTWH)), 
     .		DTWH(C2FR,IPTWH)
      CALL PWRITE(IPUF,n80)
c
      MBMA=DTWH(C9MBM,IPTWH)
      MBAOF=MBMA+SIGN(0.49,MBMA)
      CALL RN1(MBAOF,STR7)
      MBME=DTWH(c9mbm+1,IPTWH)
      MBEOF=MBME+SIGN(0.49,MBME)
      CALL RN1(MBEOF,STR8)
c
      WRITE(CPUF,2005,IOSTAT=IER) STR7,STR8
      CALL PWRITE(IPUF,n80)
c
      AZO = DTWH(C2UXP,IPTWH)
      AZOF=AZO + SIGN(0.49,AZO)
      CALL RN1(AZOF,STR1)
      ELO = DTWH(C2UYP,IPTWH)
      ELOF=ELO + SIGN(0.49,ELO)
      CALL RN1(ELOF,STR2)
c
      WRITE (CPUF,2006,IOSTAT=IER) STR1,STR2
      CALL PWRITE (IPUF,n80)
#else
      SXOFF= DTWH (C2UXP,IPTWH)
      CALL RN1 (SXOFF,STR1)
      SYOFF= DTWH (C2UYP,IPTWH)
      CALL RN1 (SYOFF,STR2)
      RXOFF= DTWH (C9RAO,IPTWH)
      CALL RN1 (RXOFF,STR3)
      RYOFF= DTWH (C9REO,IPTWH)
      CALL RN1 (RYOFF,STR4)
      PBM(1)=DTWH(C9PBM,IPTWH)
      CALL RN1 (PBM(1),STR5)
      PBM(2)=DTWH(C9PBM+1,IPTWH)
      CALL RN1 (PBM(2),STR6)
      MBM(1)=DTWH(C9MBM,IPTWH)
      CALL RN1 (MBM(1),STR7)
      MBM(2)=DTWH(C9MBM+1,IPTWH)
      CALL RN1 (MBM(2),STR8)
c
      WRITE(CPUF,1004,IOSTAT=IER) xhdr, yhdr
      CALL PWRITE(IPUF,n80)
c
      IF (ISTC(2) .EQ. BSP) THEN
            WRITE (CPUF,3000,IOSTAT=IER)
            CALL PWRITE (IPUF,n80)
            WRITE (CPUF,3001,IOSTAT=IER) estr, RSTRNG2,
     .                   (DSTRNG3(i),i=1,5), STR1,STR5,STR7
            CALL PWRITE (IPUF,n80)
            WRITE (CPUF,3002,IOSTAT=IER) estr2, RRSTR,
     .                   (RDSTR(i),i=1,5), STR2,STR6,STR8
      ELSE
            WRITE (CPUF,3003,IOSTAT=IER)
            CALL PWRITE (IPUF,n80)
            WRITE (CPUF,3001,IOSTAT=IER) estr, RSTRNG2,
     .                   (DSTRNG3(i),i=1,5), STR1,STR5,STR3
            CALL PWRITE (IPUF,n80)
            WRITE (CPUF,3002,IOSTAT=IER) estr2, RRSTR,
     .                   (RDSTR(i),i=1,5), STR2,STR6,STR4
      ENDIF
      CALL PWRITE (IPUF,n80)
c
      COSD=COS(dtwh(c4edc,iptwh)/57.296778)
      GL=DTWH(C4GL,IPTWH)
      GB=DTWH(C4GB,IPTWH)
      RAO=(DTWH(C4SX,IPTWH)-DTWH(C4ERA,IPTWH))*COSD*3600.0
      CALL RN1 (RAO,STR1)
      RRAO=DTWH(C9SRO+2,IPTWH)
      CALL RN1 (RRAO,STR3)
c
      WRITE (CPUF,3004,IOSTAT=IER) GL,GB,STR1,STR3
      CALL PWRITE (IPUF,n80)
c
      AZ=DTWH(C4AZ,IPTWH)
      EL=DTWH(C4EL,IPTWH)
      DCO=(DTWH(C4SY,IPTWH)-DTWH(C4EDC,IPTWH))*3600.0
      CALL RN1 (DCO,STR2)
      RDCO=DTWH(C9SRO+3,IPTWH)
      CALL RN1 (RDCO,STR4)
c
      WRITE (CPUF,3005,IOSTAT=IER) AZ,EL,STR2,STR4
      CALL PWRITE (IPUF,n80)
c
      WRITE(CPUF,3006,IOSTAT=IER)
      CALL PWRITE(IPUF,n80)
c
      WRITE(CPUF,3007,IOSTAT=IER) DTWH(C12SST,IPTWH),DTWH(C12CT,IPTWH),
     .                 abs(DTWH(C12FR,IPTWH)*1000.0),DTWH(C12RF,IPTWH), 
     .		       dtwh(c12cf,iptwh),DTWH(C9LOI,IPTWH),
     .                 DTWH(C9SB,IPTWH)
      CALL PWRITE(IPUF,n80)
c
      WRITE(CPUF,3008,IOSTAT=IER) DTWH(C9SYN,IPTWH)*T03,
     .                            DTWH(C9FIF,IPTWH)
      CALL PWRITE(IPUF,n80)
c
      vdef = dtwh(c7vrd, iptwh)
      write(vstr,3011) ivdef(2)
      if (vstr(2:2) .eq. ' ') then
         do 120 i = 2, 4
            vstr(i:i) = vstr(i+1:i+1)
 120     continue
         vstr(5:5) = ' '
      endif
      j = lastblnk(vstr)
      do 130 i = 5, (6-j), -1
         vstr(i:i) = vstr(i-5+j:i-5+j)
 130  continue
      do 140 i = (5-j),1,-1
         vstr(i:i) = ' '
 140  continue
      WRITE(CPUF,3009,IOSTAT=IER) vstr
      CALL PWRITE(IPUF,n80)
c
      ns = dtwh(c12it,iptwh)/dtwh(c3srt,iptwh)
      dosb = dtwh(c10pv, iptwh)
      WRITE(CPUF,3010,IOSTAT=IER) DTWH(C12IT,IPTWH)/60.,
     .	  ns, DTWH(C3SRT,IPTWH),
     .    DTWH(C12X0,IPTWH), DTWH(C7VC,IPTWH),DTWH(C12DX,IPTWH),
     .    DTWH(C9OT,IPTWH),DTWH(C2FR,IPTWH), iosb(1)
      CALL PWRITE(IPUF,n80)
#endif
c
      GO TO 99
c
 1000 FORMAT(1X,68('-'))
 1001 FORMAT(1X,' SCAN',f8.2, 10X,A8,10X, 2A8, 10X, A4)
 1002 FORMAT(1X,'YYYY-MM-DD',12X,'LST',13X,'UTC',12X,'MODE',3X,' OPR')
 1003 FORMAT (1X,I4,'-',I2.2,'-',I2,7X,6A2,4X,6A2,8X,A4,4X,A4)
 1004 FORMAT(14X,a4,7X,a4,20X,'OFFSETS')
 1005 FORMAT(15X,a4,3x,a4,2x,a4,13X,'AZ',11x,'EL')
c
 1100 format(3x,f9.4)
 1105 format(1x,f9.4,2x)
c
 2000 FORMAT (2X,'COMMANDED',6A2,1X,5A2,6X,5A2,3X,5A2)
 2001 FORMAT (3X,'TS',5X,'TC',4X,'ATTN',3X,'ATM',2X,2X,'T(AMB)',
     .        2X,'SAMPLES',4X,'SEC',4X,'TIME',3X,'TOL')
 2002 FORMAT (1X,F6.0,F7.1,F7.3,F6.2,1X,F8.2,I8,F10.1,2X,I2,':',I2,
     .        1X,I2,':',I2)
 2003 FORMAT (11X,'AZ',6X,'EL',6X,'HP',5X,'%EFF',4X,'NT',
     .        5X,'DSF',4X,'F0')
 2004 FORMAT (1X,'+BEAM:',4A2,4A2,I4,':',I2,I8,2I7,F7.1)
 2005 FORMAT (1X,'-BEAM:',4A2,4A2)
 2006 FORMAT (1X,'POINT:',4A2,4A2)
c
 3000 FORMAT (41X,'POINTING',3X,'+BEAM',4X,'-BEAM')
 3001 FORMAT(1X,a4,4X,6A2,1X,5A2,4X,'AZ:',4A2,2X,4A2,2X,4A2)
 3002 FORMAT (1X,a4,' REF',6A2,1X,5A2,4X,'EL:',4A2,2X,4A2,2X,4A2)
 3003 FORMAT (41X,'POINTING',3X,'+BEAM',5X,'REF')
 3004 FORMAT (1X,'GALACTIC',4X,F8.4,3X,F8.4,4X,'RA:',4A2,12X,4A2)
 3005 FORMAT (1X,'AZ/EL',7X,F8.4,3X,F8.4,3X,'DEC:',4A2,12X,4A2)
 3006 FORMAT(2X,'TSYS',4X,'TC',4X,'RCVRS',3X,'BW/CHAN',3X,'REST FREQ',
     .       3X,'DOP FREQ',4X,'IFS',4X,'SB')
 3007 FORMAT(1X,F6.0,F7.1,4X,'1',5X,F7.1,2X,F10.3,2X,F10.3,
     .       3X,F5.0,F4.0)
 3008 FORMAT(33X,F10.8,15X,F5.0)
 3009 FORMAT(2X,'TIME',3X,'NS',4X,'SEC',7X,'VEL',4X,a5,'/VR',3X,
     .       'VEL/CHAN',3X,'TOL',5X,'F0')
 3010 FORMAT(1X,F5.1,I5,F9.1,F11.2,F9.3,F10.3,F7.0,F7.1,x,a4)
 3011 format('V',a4)
c
c
c---------------------------------------------------------------------------
c					GB line header
c---------------------------------------------------------------------------
20    continue
c
      write(unit=cpuf,fmt=253,iostat=ier)
  253 format (72('-'))
      call pwrite(ipuf,n80)
c
      write (unit=cpuf,fmt=252,iostat=ier) dtwh(c1sno,iptwh), 
     .              (dTWH(J,IPTWH),J=c1ona,c1ona+1),
     .              dtwh(c1obs,iptwh),
     .              (dTWH(J,IPTWH),J=c1sna,c1sna+1), dtwh(c1pid,iptwh)
  252 format ('Scan:',f8.2,4x,2a8,2x,a8,2x,2a8,3x,a8)
      call pwrite(ipuf,n80)
c
      LST  =  dTWH (c3lst,IPTWH)
      CALL RNS (LST,n0,LSTRNG)
      UT  =  dTWH (c3ut,IPTWH)
      CALL RNS (ut,n0,ESTRNG)
      udate = dtwh(c3dat,iptwh)
      call udat(udate,imonth,iday,iyear)
c
#ifdef CONTINUUM
      write (unit=cpuf,fmt=254,iostat=ier) iyear, imonth, iday,
     .				(estrng(i),i=2,6),
     .			        (lstrng(i),i=2,6)
  254 format (13x,'    Date:',i4,'-',i2.2,'-',i2.2,'    UT:',
     .	      5a2,'    LST:',5a2)
#else
      write (unit=cpuf,fmt=254,iostat=ier) dtwh(c7osn,iptwh),
     .				 iyear, imonth, iday,
     .				(estrng(i),i=2,6),
     .			        (lstrng(i),i=2,6)
  254 format (' Off:',f8.2,'    Date:',i4,'-',i2.2,'-',i2.2,'    UT:',
     .        5a2,'    LST:',5a2)
#endif
      call pwrite(ipuf,n80)
c
c
#ifdef CONTINUUM
      write (unit=cpuf,fmt=255,iostat=ier) dtwh(c1stc,iptwh),  
     .          dTWH(c2ori,IPTWH), dTWH (c2fr,IPTWH)
  255 format ('Type:',a8,'    Rotation:',f6.1,'    Focus:',f6.1)
#else
      if (dtwh(c12eit,iptwh) .eq. dinfinity) then
	effint  =  dTWH (c12it,IPTWH) / (60.*60.)
      else
	effint  =  dTWH (c12eit,IPTWH) / (60.*60.)
      endif
      CALL RNS (effint,n0,LSTRNG)
      write (unit=cpuf,fmt=255,iostat=ier) dtwh(c1stc,iptwh),  
     .          dTWH(c2ori,IPTWH), dTWH (c2fr,IPTWH), (lstrng(i),i=2,5)
  255 format ('Type:',a8,'   Rotation:',f6.1,'   Focus:',f6.1, 5x,
     .		'Eff. Int: ',4a2)
#endif
      call pwrite(ipuf,n80)
c
#ifdef CONTINUUM
#else
      if(okreal8(dtwh(c1recid,iptwh)) .and.
     .   okreal8(dtwh(c3phsid,iptwh)) .and. 
     .   (nint(dtwh(c1recid,iptwh)).ne.0 .or. 
     .    nint(dtwh(c3phsid,iptwh)).ne.0) ) then
	write(cpuf,609) nint(dtwh(c1recid,iptwh)), 
     .			nint(dtwh(c3phsid,iptwh))
609	format(' Rec:', i5, '      Phase:', i5)
        call pwrite(ipuf,n80)
       endif 
#endif
      write(unit=cpuf,fmt=271,iostat=ier)
  271 format(' ')
      call pwrite(ipuf,n80)
C
      RA   =  dTWH (c4ix,IPTWH)
      CALL RNS ( RA,n2,RSTRNG2)
      DEC  =  dTWH (c4iy,IPTWH)
      CALL RNS (DEC,n1,DSTRNG3)
      write (unit=cpuf,fmt=2545,iostat=ier) rstrng2,
     .			 (dstrng3(i),i=1,5),
     .			 dtwh(c4gl,iptwh), dtwh(c4gb,iptwh)
 2545 format ('Ind Pos:  ',11a2,'     Galactic: ',f7.3,f8.3)
      call pwrite(ipuf,n80)
c
      dcoordcode = dtwh(c4csc,iptwh)
      if (coordcode .eq. 'GALACTIC' .or. coordcode .eq. 'AZEL    ') then
	coordcode = 'EPOCRADC'
	RA = dtwh(c4era,iptwh)
	DEC = dtwh(c4edc,iptwh)
      else
	RA = dTWH(c4sx,IPTWH)
        DEC = dTWH(c4sy,IPTWH)
      endif
c     Don't repeat a coordinate that will already be on the output
c
      do 233 icdtype = 1, 8
	if (dcoordcode .eq. ctype(icdtype)) then
            CALL RNS ( RA,n2,RSTRNG2)
            CALL RNS (DEC,n1,DSTRNG3)
            write (unit=cpuf,fmt=2547,iostat=ier) coordcode,
     .			 rstrng2,(dstrng3(i),i=1,5),
     .			 dtwh(c4az,iptwh), dtwh(c4el,iptwh)
 2547       format (a8,': ',11a2,'     Az/El:    ',f7.3,f8.3)
	    goto 235
	endif
233	continue
      write (unit=cpuf,fmt=2548,iostat=ier) dtwh(c4csc,iptwh),
     .			 ra, dec, dtwh(c4az,iptwh), dtwh(c4el,iptwh)
 2548       format (a8,': ',2x,f7.3,f8.3,5x,'     Az/El: ',f7.3,f8.3)
235   call pwrite(ipuf,n80)
c
      write(unit=cpuf,fmt=271,iostat=ier)
      call pwrite(ipuf,n80)
C
#ifdef CONTINUUM
      write(unit=cpuf,fmt=257)
  257 format('Chan  Intg       Rates("/sec)  #Pts   BW    TSYS',
     .       '  N.Tube  (Counts/K)')
      call pwrite(ipuf,n80)
c
      if (dtwh(c1bke,iptwh) .eq. digit) then
	TSYS   = dTWH(c12sst,IPTWH)
        bw  = dTWH(c12bw,IPTWH)
      else
	tsys = 0.0
	bw = 0.0
      endif
      NOISET = dTWH(c12ct, IPTWH)
      nch = nint(100.*dmod(dtwh(c1sno,iptwh),1.d0))
c
      WRITE (unit=cpuf,fmt=256)  nch, dtwh(c12it,iptwh), 
     .		dtwh(c6dx,iptwh),dtwh(c6dy,iptwh), nint(dtwh(c12ni,iptwh)),
     .          nint(bw), tsys, noiset, dtwh(c12fr,iptwh)
  256 FORMAT (I4,1x,F5.2,2(1x,F9.3),2(1x,i4),2F8.2,1pg14.5)
      call pwrite(ipuf,n80)
c
#else
      VDEF = dTWH (c7vrd,IPTWH)
      write (unit=cpuf,fmt=261,iostat=ier)  ivdef
  261 FORMAT ('   Cntr Freq   Rest Freq  Band Width CV(',a4,')',
     .       '      Delta V   Vsys(',a4,')')
      call pwrite(ipuf,n80)
c
      WRITE (unit=cpuf,fmt=270, iostat=ier)  DTWH(c12CF,IPTWH), 
     .		DTWH(c12rf,IPTWH), dTWH(c12bw,IPTWH),
     $          dTWH(c12x0,IPTWH), dTWH(c12dx,IPTWH), dTWH(c7vc,IPTWH)
  270 FORMAT (2f12.5,2x,f10.3,1p3g13.4)
      call pwrite(ipuf,n80)
c
      write(unit=cpuf,fmt=271,iostat=ier)
      call pwrite(ipuf,n80)
c
      dfform(1) = dtwh(c9cff,iptwh)
      dfform(2) = dtwh(c9cff+1,iptwh)
      dfform(3) = dtwh(c9cff+2,iptwh)
      if (cfform(15:15) .eq. 'L') cfform = '        ' // cfform(1:16)
c
      TINTG   = dTWH(c12it,IPTWH) / (60.*60.)
      call rns(tintg, n0, lstrng)
c
      write(unit=cpuf,fmt=272,iostat=ier) cfform(7:24), 
     .			abs(dtwh(c12fr,iptwh)),(lstrng(i),i=2,5)
272   format('CF Formula:', a18,' Delta F:',1pg13.4,
     .       '  Int.: ',4a2)
      call pwrite(ipuf,n80)
c
      write(unit=cpuf,fmt=271,iostat=ier)
      call pwrite(ipuf,n80)
c
      write (unit=cpuf,fmt=281,iostat=ier) cfform(15:15),cfform(15:15),
     .					   cfform(15:15),cfform(24:24)
  281 FORMAT ('    L',a1,'F0       L',a1,'F1       L',a1,
     .	      'F2        L',a1,'       Tsig   Tref   N.Tube')
      call pwrite(ipuf,n80)
c
      if (cfform(15:15) .eq. '1') then
	l0 = dtwh(c9l1,iptwh)
	l1 = dtwh(c9l1f1,iptwh)
	l2 = dtwh(c9l1f2,iptwh)
      else
	l0 = dtwh(c9l2,iptwh)
	l1 = dtwh(c9l2f1,iptwh)
	l2 = dtwh(c9l2f2,iptwh)
      endif
      if (cfform(24:24) .eq. 'A') then
	if0 = dtwh(c9la,iptwh)
      else if (cfform(24:24) .eq. 'B') then
	if0 = dtwh(c9lb,iptwh)
      else if (cfform(24:24) .eq. 'C') then
	if0 = dtwh(c9lc,iptwh)
      else 
	if0 = dtwh(c9ld,iptwh)
      endif
c
      write(unit=cpuf,fmt=2335,iostat=ier) l0, l1, l2, if0,
     .			 dTWH(c12sst,IPTWH), dTWH(c12rst,IPTWH), 
     .			 dTWH(c12ct, IPTWH)
 2335 FORMAT (4f11.6,3F7.2,' K')
      call pwrite(ipuf,n80)
#endif
C
      write(unit=cpuf,fmt=253,iostat=ier)
      call pwrite(ipuf,n80)
c
      GO TO 99
c------------------------------------------------------------------
99    continue
      return
c
c
      end
c
 
