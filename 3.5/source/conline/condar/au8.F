      SUBROUTINE AU8 (J)
C-------------------------------------------------------------------------------
C  @(#)au8.F	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     au8 - Header, Table, Addstack, Delete, Empty, Sclear, Title,
c            Doc, Thumb, Osb, Select
c
C---------------------------------------------------------------------
C
      real*8 source(2), dat, nrao12m, nrao140, nrao300, ctype(6), 
     .       ctype2(8), dfreq, osb, b8, fshift
      real*4 LST, sincr, sbscan, si, sescan, ra,
     .       bw, scan, xcoord, ycoord, sa, se, decx, cosd, dco,
     .       rrao, rdco, re, rao
      integer*2 ier, j, iptwh, imon, iday, iyr
      integer*4 isb, ih, im, is, k1, icor1, icdtype, jj, ian, ia, i
      integer*2 m1, m2, m3, n0, n1, n2, n80, n90, n112, n120, n224,
     .          n231, n239, n248, n281, n283
#ifdef CONTINUUM
      real*8 robsmode
      character*8 obsmode
#endif
      character*100 cpuf
      character*1 BSORCE(16)
      integer*2 ipuf(50), rstrng(6), dstrng(6), lstrng(6)
      integer*2 str1(4), str2(4)
      logical okreal8
c
      INCLUDE 'cform.inc'
      include 'core.inc'
      INCLUDE 'appl.inc'
      include 'stk.inc'
c
      EQUIVALENCE (CPUF,IPUF)
      EQUIVALENCE (SOURCE,BSORCE)
#ifdef CONTINUUM
      equivalence (obsmode, robsmode)
#endif
c
      save icor1
c
      DATA nrao12m/'NRAO 12M'/, nrao140/'NRAO 43M'/, nrao300/'NRAO 93M'/
      data ctype/2*'        ','EPOCRADC','GALACTIC','AZEL    ',
     .             'INDICATD'/
      data ctype2/'1950RADC','EPOCRADC','MEANRADC','APPRADC','APPHADC',
     2		  'USERDEF','2000RADC','INDRADC'/
c
      data icor1 /1/
c
      data m1, m2, m3, n0, n1, n2, n80, n90, n112, n120, n224,
     .          n231, n239, n248, n281, n283
     .     /-1, -2, -3, 0, 1, 2, 80, 90, 112, 120, 224, 231, 239,
     .      248, 281, 283/
      data osb /'OSB     '/, b8 /'        '/
C
C=======================================================================
C
      IPTWH = 1
c
      JJ=J
      GO TO (10, 20, 80, 120,130,220,230,240,250,270, 280), JJ
      call oerror(n120, m3, 'AU8')
C---------------------------------------------------------------------
C                                    HEADER
C---------------------------------------------------------------------
   10 CONTINUE
      call header
      GO TO 99
C-----------------------------------------------------------------------
C                                     TABLE    
C-----------------------------------------------------------------------
   20 CONTINUE
      call table
      GO TO 99
C--------------------------------------------------------------------
C                                      ADDSTACK
C--------------------------------------------------------------------
   80 CONTINUE
      if (sp.lt.3) call oerror(n112, m1, 'ADDSTACK')
c
      sincr = v(sp)
      sp=sp-1
      sESCAN=V(SP)
      SP=SP-1
      sBSCAN=V(SP)
      SP=SP-1
      IF (sBSCAN.gt.sESCAN.or.sincr.lt.0.) 
     .        call oerror(n231, m2, 'ADDSTACK')
c
 84   IAN=nint(ACOUNT)
      if (ian .lt. 0 .or. ian .gt. maxstck) 
     .      call oerror(n281, m2, 'ADDSTACK')
      ian = ian + 1
      DO 89 sI = sBSCAN,sESCAN,sINCR
         if (ian.le.maxstck) then
           ASTACK(IAN)=sI
           ACOUNT=float(IAN)
           IAN=IAN+1
	 else
           call oerror(n239, n0, 'ADDSTACK')
	   goto 99
	 endif
  89     CONTINUE
c
      GO TO 99
C-------------------------------------------------------------------------
C     DELETE                                            
C-----------------------------------------------------------------------
  120 CONTINUE
      IF (SP.lt.1) call oerror(n112, m1, 'DELETE')
c
      sBSCAN=V(SP)
      SP=SP-1
      IA=nint(ACOUNT)
c
      IF (IA.EQ.0) GO TO 99
c
      if (ia.lt.0 .or. ia.gt.maxstck) call oerror(n281, m2, 'DELETE')
c
      DO 124 I=1,IA
         IF (ABS(ASTACK(I)).EQ.sBSCAN) GO TO 126
  124    CONTINUE
      GO TO 99
c
  126 IA=IA-1
      DO 128 J=I,IA
         ASTACK(J)=ASTACK(J+1)
  128    CONTINUE
      astack(ia+1) = 0.0
      ACOUNT=ACOUNT-1.0
c
      GO TO 99
C-------------------------------------------------------------------
C                              EMPTY
C-------------------------------------------------------------------
  130 CONTINUE
      ACOUNT=0.0
      do 131 i = 1, maxstck
         ASTACK(I)=0.0
  131    CONTINUE
      GO TO 99
C-----------------------------------------------------------------------
C                                SCLEAR
C-----------------------------------------------------------------------
  220 CONTINUE
      NACCUM=0
      weight = 1.0
      ichnext = 1
      ichnum = 0
      GO TO 99
C-----------------------------------------------------------------------
C                                TITLE
C-----------------------------------------------------------------------
  230 CONTINUE
      if (.not.okreal8(dtwh(c1sno,iptwh))) 
     .			call oerror(n224, m2, 'TITLE')
c
      ih = dtwh(c3lst, iptwh)
      im = (dtwh(c3lst, iptwh) - float(ih)) * 60.0
      CALL RNS(LST,n0,LSTRNG)
      DO 231 I = 1,2
  231    SOURCE(I)=DTWH(C1SNA+I-1,IPTWH)
      BW=abs(DTWH(C12FR,IPTWH))
      SCAN=DTWH(C1SNO,IPTWH)
      if(dtwh(c1tel,iptwh).eq.nrao12m) then
         isb = int(dtwh(c9sb,iptwh))
#ifdef CONTINUUM
         robsmode = dtwh(c1stc, iptwh)
         write(cpuf, 5000) scan, source, ih, im,
     .                dtwh(c12rf, iptwh), obsmode(5:8)
 5000    format(x,f8.2,x,2a8,' LST=',i4,':',i2,' FREQ=',f11.4,x,a4)
#else
         WRITE(CPUF,5000,IOSTAT=IER) SCAN,source, ih, im,
     .               DTWH(C12RF,IPTWH),nint(bw*1000.),isb
 5000    FORMAT(x,f8.2,x,2a8,' LST=',i4,':',i2,' FREQ=',F11.4,' RES=',
     .          i4,' SB=',i2)
#endif
      else
#ifdef CONTINUUM
         write(cpuf,5001) scan, source, ih, im,
     .			dtwh(c12ct,iptwh),bw
 5001    format(x,f8.2,x,2a8,' LST=',i4,':',i2,' NT=',f11.4,
     1       ' CALFCT=',1pg11.4)
#else
         write(cpuf,5001) scan, source, ih, im,
     .			dtwh(c12rf,iptwh),bw
 5001    format(x,f8.2,x,2a8,' LST=',i4,':',i2,' FREQ=',f11.4,
     1       ' RES=',1pg11.4)
#endif
      endif
      CALL PWRITE(IPUF,n80)
      GO TO 99
C-------------------------------------------------------------------
C                                       DOC
C-------------------------------------------------------------------
  240 CONTINUE
      if (.not.okreal8(dtwh(c1sno,iptwh))) 
     .			call oerror(n224, m2, 'DOC')
c
      IF (PFLAG.le.0.1) then
            WRITE (CPUF,6001,IOSTAT=IER) dtwh(c4csc, iptwh)

#ifdef CONTINUUM
 6001 	    FORMAT (' SCAN       SOURCE       DATE       ',
     .              'X (',a8,') Y     ITIME     CALFCT')
#else
 6001 	    FORMAT (' SCAN       SOURCE       DATE       ',
     .              'X (',a8,') Y     ITIME   REST FREQ')
#endif
c
        CALL PWRITE (IPUF,n80)
        PFLAG=1.0
      endif
c
      SCAN=DTWH(C1SNO,IPTWH)
c
      im = (DTWH(C12IT,IPTWH) / 60.)
      is = (dtwh(c12it,iptwh) - im * 60.)
c
#ifdef CONTINUUM
      dfreq = dtwh(c12fr, iptwh)
#else
      dfreq = dtwh(c12rf,iptwh)
#endif
c
      DO 242 I=1,2
         SOURCE(I)=DTWH(C1SNA+I-1,IPTWH)
  242    CONTINUE
C
      DAT=DTWH(C3DAT,IPTWH)
      call udat(dat, imon, iday, iyr)
c
      xcoord = dtwh(c4sx + icor1 - 1, iptwh)
      ycoord = dtwh(c4sx + icor1, iptwh)
c
      do 233 icdtype = 1, 8
	 if (dtwh(c4csc,iptwh) .eq. ctype2(icdtype)) then
      		CALL RNS(xcoord,n2,RSTRNG)
        	CALL RNS(ycoord,n1,DSTRNG)
c
        	WRITE(CPUF,6002,IOSTAT=IER) SCAN,(BSORCE(I),I=1,12),
     .               iyr, imon,iday,(RSTRNG(i),i=2,6),(DSTRNG(i),i=1,5),
     .               im,is,dfreq
 6002   	FORMAT (f8.2,x,12a1,x,i4,'-',i2.2,'-',i2.2,x,5A2,5A2,
     .                  x,i3,':',i2.2,x,F11.4)
c
	   	goto 235
	  endif
233       continue
c         Write out positions in HH:MM:SS DD:MM:SS form if the coordinate
c         system of the observations is one in CDTYPE2; else print in DDD.ddd
c         form
c
      WRITE(CPUF,6003,IOSTAT=IER) SCAN,(BSORCE(I),I=1,12),
     .               iyr,imon,iday,xcoord,ycoord,im,is,dfreq
 6003 FORMAT (f8.2,x,12a1,x,i4,'-',i2.2,'-',i2.2,x,2f10.4,
     .                  x,i3,':',i2.2,x,F11.4)
c
235   CALL PWRITE(IPUF,n90)
c
      GO TO 99
C----------------------------------------------------------------------
C                                            THUMB
C---------------------------------------------------------------------
  250 CONTINUE
      if (.not.okreal8(dtwh(c1sno,iptwh))) 
     .			call oerror(n224, m2, 'THUMB')
c
      if (dtwh(c1tel,iptwh).eq.nrao12m) then
        SA=DTWH(C2UXP,IPTWH)
        SE=DTWH(C2UYP,IPTWH)
        DECX=DTWH(C4EDC,IPTWH)
        COSD=COS(DECX/57.29578)
        RAO=(DTWH(C4SX,IPTWH)-DTWH(C4ERA,IPTWH))*COSD*3600.0
        DCO=(DTWH(C4SY,IPTWH)-DTWH(C4EDC,IPTWH))*3600.0
        RRAO=DTWH(C9SRO+2,IPTWH)
        RDCO=DTWH(C9SRO+3,IPTWH)
        RA=DTWH(C9RAO,IPTWH)
        RE=DTWH(C9REO,IPTWH)
        CALL RN1(SA,STR1)
        CALL RN1(RA,STR2)
        WRITE(CPUF,7000,IOSTAT=IER)
 7000   FORMAT (10X,'MAIN',8X,'REF')
        CALL PWRITE(IPUF,n80)
        WRITE(CPUF,7001,IOSTAT=IER) STR1,STR2
 7001   FORMAT (4X,'AZ',1X,4A2,3X,4A2)
        CALL PWRITE(IPUF,n80)
        CALL RN1(SE,STR1)
        CALL RN1(RE,STR2)
        WRITE(CPUF,7002,IOSTAT=IER) STR1,STR2
 7002   FORMAT (4X,'EL',1X,4A2,3X,4A2)
        CALL PWRITE(IPUF,n80)
        CALL RN1(RAO,STR1)
        CALL RN1(RRAO,STR2)
        WRITE (CPUF,7003,IOSTAT=IER) STR1,STR2
 7003   FORMAT (4X,'RA',1X,4A2,3X,4A2)
        CALL PWRITE (IPUF,n80)
        CALL RN1(DCO,STR1)
        CALL RN1(RDCO,STR2)
        WRITE(CPUF,7004,IOSTAT=IER) STR1,STR2
 7004   FORMAT (3X,'DEC',1X,4A2,3X,4A2)
        CALL PWRITE(IPUF,n80)
c
      else if (dtwh(c1tel,iptwh).eq.nrao140 .or.
     1         dtwh(c1tel,iptwh).eq.nrao300 ) then
c
	write(cpuf,9901) (dtwh(c2pc+k1-1,iptwh),k1=1,3)
9901    format(' PVLS: ',1p3g15.6,' Arcmin')
	call pwrite(ipuf,n80)
	write(cpuf,9902) dtwh(c2uxp,iptwh), dtwh(c2uyp,iptwh)
9902	format(' LPCS: ',1p2g15.6,15x,' Arcsec')
        call pwrite(ipuf,n80)
c
      else
	call oerror(n283, m2, 'THUMB')
      endif
      GO TO 99
C----------------------------------------------------------------
C                                        OSB
C----------------------------------------------------------------
  270 CONTINUE
      if (dtwh(c1tel,iptwh).eq.nrao12m) then
c				flip sign of c12fr, and c12dx
         dtwh(c12fr, iptwh) = - dtwh(c12fr, iptwh)
         dtwh(c12dx, iptwh) = - dtwh(c12dx, iptwh)
c			recalculate the frequencies, first the shift
         if (nint(dtwh(c9sb, iptwh)) .eq. 0 .or. 
     .       nint(dtwh(c9sb, iptwh)) .eq. 2) then
c				USB -> LSB
            fshift = - 2*dtwh(c9fif, iptwh)
         else
c				LSB -> USB
            fshift = 2*dtwh(c9fif, iptwh)
         endif
c			then the rest freq, depends on orig value for cf
c			this change in rest freq. preserves the velocity
c			axis reference position
         if (dtwh(c12cf, iptwh) .ne. 0.0) then
            dtwh(c12rf, iptwh) = dtwh(c12rf, iptwh) * 
     .                        (1.0 + fshift / dtwh(c12cf, iptwh))
         else
            dtwh(c12rf, iptwh) = dtwh(c12rf, iptwh) + fshift
         endif
c			then the center freq, which is just a straight shift
         dtwh(c12cf, iptwh) = dtwh(c12cf, iptwh) + fshift
c			reset the sideband value
         if (nint(dtwh(c9sb, iptwh)) .eq. 0) then
            dtwh(c9sb, iptwh) = 1
         else if (nint(dtwh(c9sb, iptwh)) .eq. 1) then
            dtwh(c9sb, iptwh) = 0
         else if (nint(dtwh(c9sb, iptwh)) .eq. 2) then
            dtwh(c9sb, iptwh) = 3
         else 
            dtwh(c9sb, iptwh) = 2
         endif
c			and set/unset a flag in class 10
         if (dtwh(c10pv, iptwh) .eq. osb) then
            dtwh(c10pv, iptwh) = b8
         else if (dtwh(c10pv, iptwh) .eq. b8 .or. 
     .       dtwh(c10pv, iptwh) .eq. 0) then
            dtwh(c10pv, iptwh) = osb
         endif
      else
         call oerror(n248, m2, 'OSB')
      endif
      GO TO 99
C----------------------------------------------------------------
C                                        SELECT
C----------------------------------------------------------------
  280 continue
      call select
      go to 99
c
   99 CONTINUE
      RETURN
C---------------------------------------------------------------------
      END
