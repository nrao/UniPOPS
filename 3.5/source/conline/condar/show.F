      subroutine show(ishow, iplt)
c
C-------------------------------------------------------------------------------
C  @(#)show.F	5.3 09/10/98
C-------------------------------------------------------------------------------
c
c     Draws a scan plot.
c
c     If ISHOW = 1, then must rescale plot and draw border
c        else use previous scaling, draw plot, but don't draw border
c     If IPLT = 0, plot using continuous line; = 1 plot using histogram;
c	= 2 plot using points.
c
      INTEGER*2  RSTR(6), DSTR(6), RRSTR(6), RDSTR(6), npp1, curcur
      CHARACTER*3  MON(12)
      character*24 cfform, vellbl
      character*12 xlabels(11)
      INTEGER*2  ISOURC(8), iptwh, iplt, ishow, istop, iy, istart, ixr, 
     .		 ixl, ix, ier, ilow, ihi, iy1, iy2, imon, iday, iyr, 
     .           short, ixlabels(6,11), sclc, limx2, ierr, lxa1, uxa1,
     .		 ivelcnt, lastblnk, ivellbl(12)
      integer*2 m2, n0, n1, n2, n5, n7, n8, n9, n10, n12, n14, n20, n25, 
     .          n30, n33, n35, n55, n72, n74, n75, n100, n125, n150, 
     .          n225, n288, n3, n270, n271, n272
      integer*4 iby, ibx, idelx, nchan, i, ilast, nch,
     .          ibe, isb, nac, icdtype, ipixel, idely
      REAL*4 dvel, bw, tsys, tim, ra, dec, rra, rdc, tcal, ymin, 
     .	     yincr, val, xz, av, ch, scn1, scn2, xf, xl, dx, xf1,
     .	     xf2
      real*8 nrao12m, nrao140, nrao300, dfform(3), chantox, xtochan
      real*8 rf, d03, udate, cf, rif, SOURCE(2), ctype(8)
      character*10 format, getformat
      logical okarray, okreal4, placenxt, okreal8
c
#ifdef CONTINUUM
#else
      integer*2 osb(4)
      real*4 vcnt
      character*8 veldef
      real*8 syn, dosb, dveldef
      equivalence (osb(1), dosb), (dveldef, veldef)
#endif
c
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
      include 'core.inc'
c
      EQUIVALENCE (SOURCE,ISOURC), (cfform,dfform)
      EQUIVALENCE (ixlabels, xlabels), (ivellbl, vellbl)
c
      data m2, n0, n1, n2, n5, n7, n8, n9, n10, n12, n14, n20, n25, 
     .     n30, n33, n35, n55, n72, n74, n75, n100, n125, n150, n225, 
     .     n288, n3, n270, n271, n272
     .     /-2, 0, 1, 2, 5, 7, 8, 9, 10, 12, 14, 20, 25, 30, 33, 35, 
     .     55, 72, 74, 75, 100, 125, 150, 225, 288, 3, 270, 271, 272/
c
      sclc(ipixel) = max(n1, nint(sclchar*float(ipixel))) 
c
      DATA D03/0.001D0/
      DATA MON/'JAN','FEB','MAR','APR','MAY','JUN',
     .         'JUL','AUG','SEP','OCT','NOV','DEC'/
      data nrao12m/'NRAO 12M'/, nrao300/'NRAO 93M'/, 
     1     nrao140/'NRAO 43M'/
c
      data ctype/'1950RADC','EPOCRADC','MEANRADC','APPRADC','APPHADC',
     2		 'USERDEF','2000RADC','INDRADC'/
c
#ifdef CONTINUUM
      data xlabels/'SAMPLES',
     .             'TIME(SEC)',
     .             'POS (DEG)',
     .             'X-POS (DEG)',
     .             'Y-POS (DEG)',
     .             'X-POS (DEG)',
     .             'Y-POS (DEG)',
     .             'LST (HOURS)',
     .             'UT (HOURS)',
     .		   ' ',
     .		   ' '/
#else
      data xlabels/'CHANNELS ',
     .		   'FREQUENCY',
     .             'VELOCITY',
     .             'VEL. (RADI)',
     .             'VEL. (OPTI)',
     .             'VEL. (RELV)',
     .             'VELOCITY',
     .		   'REST FREQ.',
     .		   'SKY FREQ.',
     .		   'IMAGE REST F',
     .		   'IMAGE SKY F'/
#endif
c
      IPTWH = 1
      npp1 = curcur()
      IX0=XORG(npp1) + sclfctx*140
      IY0=YORG(npp1) + sclfcty*150
      IXM=XMAX(npp1) - sclfctx*40
      IYM=YMAX(npp1) - sclfcty*60
c    
      if (ishow .eq. 1) then
	if(bdrop.lt.0.or.edrop.lt.0.or.
     1     (bdrop+edrop+1.ge.dtwh(c12ni,iptwh))) 
     2		call oerror(n288,m2,'SHOW')
c
      	ISTART=DTWH(C12SPN,IPTWH)+IDATOFF+BDROP
      	ISTOP=DTWH(C12SPN,IPTWH)+DTWH(C12NI,IPTWH)
     .      + IDATOFF - 1 - EDROP
     	ISBG=ISTART-IDATOFF
      	ILAST = istop - idatoff
      	NCHAN=ISTOP-ISTART+1
c    
      	if(istart .ge. istop) call oerror(n225, m2, 'SHOW')
C							Y - SCALING
      	limy = 5
c     	5 = number of Y tick marks desired and produced by YSCALE
c
      	IF (YLMIN.gt.-9.E09) then
           IF (YLMIN.gt.-9999.) then
	     YMIN = YLMIN
	     YINCR = YLINCR
           else
             CALL YSCALE (TWH(1,IPTWH),ISTART,ISTOP,YMIN,YINCR)
           endif
	endif
c
        AY(npp1)=float(iym-iy0)/(float(limy)*YINCR)
        BY(npp1)=IY0-YMIN*AY(npp1)
        AX(npp1)=FLOAT(IXM-IX0)/FLOAT(nchan)
	bx(npp1)=ix0 - (float(isbg)-0.5)*ax(npp1)
C							PLOT SPECTRA
      endif
c
      placenxt = .true.
c     PLACENXT = TRUE if the next point to plot should start off with a PLACE.
c
      DO 100 I=1, nchan
         VAL=TWH(I+istart-1,IPTWH)
	 if (.not.okreal4(val)) then
		placenxt = .true.
		goto 100
	 endif
c	 Skip undefined points
c
         IY = max(anint(min(AY(npp1)*VAL+BY(npp1),float(iym))),
     .                  float(iy0))
	 ix = max(anint(min(float(i+isbg-1)*ax(npp1) + bx(npp1),float(ixm))),
     .                  float(ix0))
	 if (iplt .eq. 0) then
	    if (placenxt) then
		call placewp(ix, iy, n33)
	    else
	    	call vctrwp(ix, iy, n33, sclchar, 0.)
	    endif
	 else if (iplt .eq. 1) then
	    if (placenxt) then
		call place(ix0, iy)
	    else
		call vctr(ixl, iy)
	    endif
	    ixr = max(nint(min((float(i+isbg-1)+.5)*
     .                            ax(npp1) + bx(npp1),float(ixm))),ix0)
	    call vctr(ixr, iy)
	    ixl = ixr
         else IF (IPLT.ge.2) then
            CALL PLACE(IX-sclc(n5),IY)
            CALL VCTR(IX+sclc(n5),IY)
            CALL PLACE(IX,IY-sclc(n5))
	    call vctr(ix, iy+sclc(n5))
	 endif
	 placenxt = .false.
  100    CONTINUE
c
      if (slabel .gt. 0 .and. sclchar .ne. 1.) call charsize(sclchar)
c
      IF (SLABEL.gt.1.0 .and. okreal8(dtwh(c1sno,iptwh)))  then
        NCH=nint(100.0/AX(npp1))
        IBE=max(istart,ISTOP-NCH)
        AV=0.0
        DO 200 I = IBE,ISTOP
           AV=AV+TWH(I,IPTWH)
 200      CONTINUE
        AV=AV/FLOAT(NCH+1)
        IBY=nint((AV-YMIN)*AY(npp1))+IY0-sclc(n25)
        WRITE (CPUF,219,IOSTAT=IER) DTWH(C1SNO,IPTWH)
  219   FORMAT(F8.2)
        CALL PLACE(IXM-sclc(n8*n14),short(IBY))
        CALL PCHAR(IPUF,n8)
      endif
c     Labels plots wih scan number along the right-hand side of plot.
c     Only if SLABEL > 1 and a scan is being plotted.
c
      IF (ISHOW.EQ.0) GO TO 99
c     Don't do anything more if this is a RESHOW
c
C							BORDER
      CALL PLACE (IX0,IY0)
      CALL VCTR	 (IXM,IY0)
      CALL VCTR	 (IXM,IYM)
      CALL VCTR	 (IX0,IYM)
      CALL VCTR	 (IX0,IY0)
c
C					       X TICKS AND ANNOTATION

      lxa1 = lxa
      if (.not. okarray(lxa, dtwh(1,iptwh)) ) then
         call oerror(n272, n0, 
     .		'SHOW: Units of lower X-axis will be channels or samples')
	 lxa1 = 1
      endif
      uxa1 = uxa
      if (.not. okarray(uxa, dtwh(1,iptwh)) ) then
         call oerror(n272, n0, 
     .		'SHOW: Units of upper X-axis will be channels or samples')
	 uxa1 = 1
      endif
c
      xf1 = chantox(lxa1, dtwh(1,iptwh), float(isbg), ierr)
      xf2 = chantox(lxa1, dtwh(1,iptwh), float(ilast), ierr)
      call tickmrk(xf1, xf2, n10, xf, xl, dx, limx)
c     Gets the tick mark parameters for lower axis labelling
c
      idelx = 1
250   if ( idelx*(ixm - ix0 + 1)/(limx+1) .le. sclchar*10*14 .and. 
     .     idelx .lt. limx-1) then
	idelx = idelx * 2
	goto 250
      endif
c     Prevents overlapping x labels
c
      do 300 i = 1, limx
	    xz = float(i-1)*dx + xf
	    ch = xtochan(lxa1, dtwh(1,iptwh), xz, ierr)
	    ix = nint( ch*ax(npp1) + bx(npp1))
c	    Finds the pixel number where the tick mark is to be drawn; ch
c	    is the channel number where the tick has been drawn.
c
	    rixt(i) = ix
c	    Store away location of tick marks into RIXT for use by FULLGRID
c 
            CALL PLACE (IX,IY0)
            CALL VCTR  (IX,IY0+sclc(n12))
            CALL PLACE (IX,IYM)
            CALL VCTR  (IX,IYM-sclc(n12))
c	    Draw tick marks
c
	    if (slabel .gt. 0.0 .and. mod(i-1,idelx) .eq. 0) then
c	    Label tick mark if desired and label wont overlap previous one.
c
              CALL PLACE (IX,IY0)
              CALL VCTR  (IX,IY0+sclc(n20))
              CALL PLACE (IX,IYM)
              CALL VCTR  (IX,IYM-sclc(n20))
	      format = getformat(xz, 2.*xz, n8)
	      write(cpuf, format, iostat=ier) xz
	      CALL PLACE (IX-sclc(n10*n14/n2),IY0-sclc(n30))
              CALL PCHAR (IPUF,n8)
c	      Label lower axis.
c
	    endif
c
  300       CONTINUE
c
      if (slabel .gt. 0.0) then
#if CONTINUUM
        vellbl = xlabels(lxa1)
#else
	dveldef = dtwh(c7vrd,iptwh)
	if (lxa1 .eq. 3 .and. veldef .ne. '        ') then
	      vellbl = xlabels(lxa1) // '(' // veldef // ')'
	else
	      vellbl = xlabels(lxa1)
	endif
#endif
	ivelcnt = lastblnk(vellbl)
        call place( (ixm+ix0)/n2-sclc(ivelcnt*n14/n2), iy0 - sclc(n55))
	call pchar(ivellbl,ivelcnt)
      endif
c
      if (uxa1 .eq. lxa1) goto 440
c
      xf1 = chantox(uxa1, dtwh(1,iptwh), float(isbg), ierr)
      xf2 = chantox(uxa1, dtwh(1,iptwh), float(ilast), ierr)
      call tickmrk(xf1, xf2, n10, xf, xl, dx, limx2)
c     Gets the tick mark parameters for lower axis labelling
c
      idelx = 1
350   if ( idelx*(ixm - ix0 + 1)/(limx2+1) .le. sclchar*10*14 .and. 
     .     idelx .lt. limx2-1) then
	idelx = idelx * 2
	goto 350
      endif
c     Prevents overlapping x labels
c
      do 400 i = 1, limx2
	    xz = float(i-1)*dx + xf
	    ch = xtochan(uxa1, dtwh(1,iptwh), xz, ierr)
	    ix = nint( ch*ax(npp1) + bx(npp1))
c	    Finds the pixel number where the tick mark is to be drawn; ch
c	    is the channel number where the tick has been drawn.
c
            CALL PLACE (IX,IYM)
            CALL VCTR  (IX,IYM+sclc(n5))
c	    Draw tick marks

	    if (slabel .gt. 0.0 .and. mod(i-1,idelx) .eq. 0) then
c	    Label tick mark if desired and label wont overlap previous one.
c
              CALL PLACE (IX,IYM)
              CALL VCTR  (IX,IYM+sclc(n10))
	      format = getformat(xz, 2.*xz, n8)
	      write(cpuf, format, iostat=ier) xz
	      call place(ix-sclc(n10*n14/n2),iym+sclc(n12))
	      call pchar(ipuf, n8)
c	      Label upper x-axis
c
	    endif
c
  400       CONTINUE
c
      if (uxa1 .ne. lxa1 .and. slabel .gt. 0.0) then
#if CONTINUUM
        vellbl = xlabels(uxa1)
#else
	dveldef = dtwh(c7vrd,iptwh)
	if (uxa1 .eq. 3 .and. veldef .ne. '        ') then
	      vellbl = xlabels(uxa1) // '(' // veldef // ')'
	else
	      vellbl = xlabels(uxa1)
	endif
#endif
	ivelcnt = lastblnk(vellbl)
        call place( (ixm+ix0)/n2-sclc(ivelcnt*n14/n2), iym + sclc(n35))
	call pchar(ivellbl,ivelcnt)
      endif
C					       Y TICKS AND ANNOTATION
c
440   idely = 1
450   if ( idely*(iym - iy0 + 1)/(limy+1) .le. sclchar*25 .and. 
     .     idely .lt. limy-1) then
	idely = idely * 2
	goto 450
      endif
c     Prevent overlapping y labels
c
      DO 500 I=1,limy+1
	 XZ=ymin+float(i-1)*yincr
	 IY=nint( xz*ay(npp1)+by(npp1))
	 riyt(i) = iy
	 CALL PLACE (IX0,IY)
	 CALL VCTR  (IX0+sclc(n12),IY)
	 CALL PLACE (IXM,IY)
  	 CALL VCTR  (IXM-sclc(n12),IY)
c
	 if (slabel .gt. 0.0 .and. mod(i-1,idely) .eq. 0) then
	      CALL PLACE (IX0,IY)
	      CALL VCTR  (IX0+sclc(n20),IY)
	      CALL PLACE (IXM,IY)
  	      CALL VCTR  (IXM-sclc(n20),IY)
	      format = getformat(xz, 2.*xz, n8)
	      write(cpuf, format, iostat=ier) xz
	      CALL PLACE (IX0-sclc(n8*n14),IY-sclc(n12))
              CALL PCHAR (IPUF,n8)
	 endif
  500    CONTINUE
c
C					       ZERO LINE
c
      IF (ZLLINE.ge.1.0) then
	 IBY=nint(BY(npp1))
	 if (iby .ge. iy0 .and. iby .le. iym) then
	    CALL PLACE (IX0,short(IBY))
	    CALL VCTR  (IXM,short(IBY))
	 endif
      endif
c
      DO 560 I = 1,maxmrk
	  if (cmark(i) .le. -999999.0) goto 561
	    IbX = CMARK(I) * AX(npp1) + bx(npp1) + 0.5
	    if (ibx .ge. ix0 .and. ibx .le. ixm) then
	       CALL PLACE (short(IbX),IYM)
	       CALL VCTR  (short(IbX),IY0)
	    endif
560	    continue
C           CHANNEL MARKERS
c
561   DO 562 I = 1,maxmrk
	  if (xmark(i) .le. -999999.0) goto 563
	    xz = xtochan(n3, dtwh(1,iptwh), xmark(i), ierr)
            if (ierr .ne. 0) call oerror(n270, n0, 'SHOW')
	    IbX = xz*ax(npp1) + bx(npp1) + 0.5
	    if (ibx .ge. ix0 .and. ibx .le. ixm) then
	       CALL PLACE (short(IbX),IYM)
               CALL VCTR  (short(IbX),IY0)
	    endif
562	    continue
C           VELOCITY MARKERS
c
563   DO 564 I = 1,maxmrk
	  if (fmark(i) .le. -999999.0) goto 565
	    xz = xtochan(n2, dtwh(1,iptwh), fmark(i), ierr)
            if (ierr .ne. 0) call oerror(n271, n0, 'SHOW')
	    IbX = xz*ax(npp1) + bx(npp1) + 0.5
	    if (ibx .ge. ix0 .and. ibx .le. ixm) then
               CALL PLACE(short(IbX),IY0)
               CALL VCTR (short(IbX),IYM)
	    endif
564	    continue
C           Frequency MARKERS
c
565   DO 566 I = 1,maxmrk
	  if (hmark(i) .le. -999999.0) goto 567
            IbY = HMARK(I)*AY(npp1)+by(npp1) + 0.5
	    if (iby .ge. iy0 .and. iby .le. iym) then
               CALL PLACE(IX0,short(IbY))
               CALL VCTR( IXM,short(IbY))
	    endif
566	    continue
c           Temperature Markers
C						BASELINE MARKERS
567   IF (BMARK.ge.1.0) then 
         DO 580 I=1,maxregn,2
	    IF ((BREG(I).ne.0)) then
	       IBY=nint(BY(npp1))
	       ILOW=min(max(BREG(I)*AX(npp1)+bx(npp1),ix0),ixm)
	       IHI =min(max(BREG(I+1)*AX(npp1)+bx(npp1),ix0),ixm)
	       IY1=min(max(short(IBY)+sclc(n75),iy0),iym)
	       IY2=min(max(short(IBY)-sclc(n75),iy0),iym)
	       CALL PLACE(ILOW,IY1)
	       CALL VCTR (ILOW,IY2)
	       CALL VCTR (IHI ,IY2)
	       CALL VCTR (IHI, IY1)
	       CALL VCTR (ILOW,IY1)
	    endif
 580	    CONTINUE
      endif
c
C                                                DOCUMENTATION
      if (.not.okreal8(dtwh(c1sno,iptwh)) .or. slabel .ne. 1.0) goto 99
c     Don't document plot if the array doesn't contain telescope data
c
#ifdef CONTINUUM
#else
      if(dtwh(c1tel,iptwh).ne.nrao12m .and.
     .   (nint(dtwh(c1recid,iptwh)).ne.0 .or. 
     .    nint(dtwh(c3phsid,iptwh)).ne.0) ) then
	write(cpuf,609) nint(dtwh(c1recid,iptwh)), 
     .			nint(dtwh(c3phsid,iptwh))
609	format('Rec:', i5, ' Phase:', i5)
        call place(ix0-sclc(n7*n14), iy0-sclc(n75))
        CALL pchar(IPUF,n72)
       endif 
#endif
      cpuf = ' '
      DO 600 I = 1,2
         SOURCE(I)=DTWH(C1SNA+I-1,IPTWH)
 600     CONTINUE
      DO 610 I = 1,6
         IPUF(I)=ISOURC(I)
 610     CONTINUE
c
      NAC=DTWH(C13NS,IPTWH)
      IF (NAC.GT.0.) THEN
         SCN1=DTWH(C13FS,IPTWH)
         SCN2=DTWH(C13LS,IPTWH)
         WRITE (CPUF(14:40),619,IOSTAT=IER) NAC,SCN1,SCN2
 619     FORMAT (I3,' SCANS:',f8.2,'-',f8.2)
      ELSE
	 write(cpuf(14:21),219,iostat=ier) dtwh(c1sno,iptwh)
      endif
c
      UDATE=DTWH(C3DAT,IPTWH)
      CALL UDAT(UDATE,IMON,IDAY,IYR)
c
#ifdef CONTINUUM
      TIM=DTWH(C12IT,IPTWH)
      WRITE (CPUF(42:73),629,IOSTAT=IER) tim,IDAY,MON(IMON),IYR
  629 FORMAT (' INT=',f8.2,' DATE:',1x,i2.2,1x,a3,1x,i4)
#else
      TIM=DTWH(C12IT,IPTWH)/(60.0*60.0)
      call rns(tim, n0, rstr)
      WRITE (CPUF(42:73),629,IOSTAT=IER) (rstr(i),i=2,5),IDAY,
     .					  MON(IMON),IYR
  629 FORMAT (' INT=',1x,4a2,' DATE:',1x,i2.2,1x,a3,1x,i4)
#endif

c
      call place(ix0-sclc(n7*n14), iy0-sclc(n100))
      CALL pchar(IPUF,n74)
      cpuf = ' '
c
      RA=DTWH(c4sx,IPTWH)
      DEC=DTWH(c4sy,IPTWH)
      RRA=DTWH(C4RX,IPTWH)
      IF (RRA .LT. 0.0 .OR. RRA .GT. 360.0) RRA=0.0
      RDC=DTWH(C4RY,IPTWH)
      IF (ABS(RDC) .GT. 90.0) RDC=0.0
      do 680 icdtype = 1, 8
	 if (dtwh(c4csc,iptwh) .eq. ctype(icdtype)) then
           CALL RNS(RA,n2,RSTR)
           CALL RNS(DEC,n1,DSTR)
           WRITE (CPUF(1:29),639,IOSTAT=IER) dtwh(c4csc,iptwh),
     .				(RSTR(I),I=2,6),(DSTR(I),I=1,5)
  639      FORMAT (a8,'=',5A2,5A2)
           CALL RNS (RRA,n2,RRSTR)
           CALL RNS (RDC,n1,RDSTR)
           IF (RRA.GT.0.1 .or. abs(rdc) .gt. 0.1) 
     .      WRITE (CPUF(31:52),649,IOSTAT=IER) (RRSTR(I),I=2,6),
     .      				        (RDSTR(I),I=1,5)
649        FORMAT ('(',5A2,5A2,')')
	   goto 700
	 endif
680      continue
c        If the coor. code is RA, HA, etc, must convert coordinates into
c	 hh:mm:ss DD:MM:SS format before printing
c
         write(cpuf(1:29),689,iostat=ier) dtwh(c4csc,iptwh), ra, dec
689      format(a8,'='2f10.4)
         IF (RRA.GT.0.1 .or. abs(rdc) .gt. 0.1) 
     .       WRITE (CPUF(31:52),699,IOSTAT=IER) rra, rdc
699      FORMAT ('(',2f10.4,')')
c        Else if the coord. code is anything else, use DDD.dddd format
c
700   tcal=DTWH(C12CT,IPTWH)
      TSYS=DTWH(C12SST,IPTWH)
c
#ifdef CONTINUUM
      write(cpuf(54:72),719,iostat=ier) tcal
719   format('CAL=',f7.2)
#else
      write(cpuf(54:73),719,iostat=ier) tcal, tsys
719   format('CAL=',f6.1,' TS=',f6.0)
#endif
c
      call place(ix0-sclc(n7*n14), iy0-sclc(n125))
      CALL pchar(IPUF,n74)
      cpuf = ' '
c
      RF=DTWH(C12RF,IPTWH)
      DVEL=DTWH(C12DX,IPTWH)
      bw = abs(dtwh(c12fr,iptwh))
      if(dtwh(c1tel,iptwh).eq.nrao12m) then
        isb = dtwh(c9sb, iptwh)
#ifdef CONTINUUM
        write(cpuf, 729, iostat = ier) dtwh(c12cf, iptwh), 
     .        dtwh(c1stc, iptwh), isb
 729    format('FREQ = ', f9.2, 20x, a8,10x, 'SB ',i2)
#else
        VCNT=DTWH(C12X0,IPTWH)
	SYN=DTWH(C9SYN,IPTWH)*D03
        dosb = dtwh(c10pv, iptwh)
	WRITE (CPUF,759,IOSTAT=IER) RF,SYN,VCNT,DVEL,nint(bw*1000),ISB,
     .         osb(1), osb(2)
 759    FORMAT ('FREQ=',F9.2,' SYN=',F10.8,' VEL=',F8.1,' DV=',
     .        F6.2,' FR=',I4,' SB=',I1, x, 2a2)
#endif
      else 
        cf = dtwh(c12cf,iptwh)
        dfform(1) = dtwh(c9cff,iptwh)
        dfform(2) = dtwh(c9cff+1,iptwh)
        dfform(3) = dtwh(c9cff+2,iptwh)
        if (cfform(15:15) .eq. 'L') cfform = '        ' // cfform(1:16)
c
        if (cfform(24:24) .eq. 'A') then
	  rif = dtwh(c9la,iptwh)
        else if (cfform(24:24) .eq. 'B') then
	  rif = dtwh(c9lb,iptwh)
        else if (cfform(24:24) .eq. 'C') then
	  rif = dtwh(c9lc,iptwh)
        else 
	  rif = dtwh(c9ld,iptwh)
        endif
c
#ifdef CONTINUUM
	write(cpuf,739,iostat=ier) 3600.*dvel/dtwh(c12it,iptwh), 
     .				    bw, dvel, 
     .		nint(dtwh(c12ni,iptwh))
  739   format('RATE=',f11.3,'  CAL FCTR=',f11.5,'  DELTA=',f11.6,
     1   '  SMPLS',i5)
#else
	write(cpuf,749,iostat=ier) rf, cf, rif, bw, dvel
  749   format('REST=',f11.5,' SKY=',f11.5,' IF=',f6.2,
     1   ' DFREQ=',1pg10.3,' DV=',g9.2)
#endif
c
      endif
      call place(ix0-sclc(n7*n14),iy0-sclc(n150))
      CALL pchar(IPUF,n72)
c
99    if (sclchar .ne. 1.) call charsize(1.)
c
      return
c
      end
