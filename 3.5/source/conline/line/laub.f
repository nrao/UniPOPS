      SUBROUTINE AUB (J)
C-------------------------------------------------------------------------------
C  @(#)laub.f	5.3 09/10/98
C-------------------------------------------------------------------------------
c
c     laub - Rap, Gmeasure, Temp, Getir, Getotf
c
c*********************************
c
      INCLUDE 'appl.inc'
c
      integer*2 j, ipuf(40), n283, m2, n120, m3, n291, n247, n112, 
     .          m1, n225, n221, n222, record, phase, ierr, num, inn,
     .		ishift
      integer*4 klf1, ij, kl, lm, istrt, istp, iptwh, jj, idf, 
     .		i, ihold, itemp, istart, istop, i1, i2, i3
      real*4 df, bm, sm, atemp, toff, off, diff, ton, pr1,
     .		tsyson, tsysoff, f, scanno, rinfinity, ref(MAX_DATA_POINTS)
      character*1 losgn
      real*8 dsmbm(3), nrao140, nrao300, t12m, dinfinity, specproc 
      character*24 csmbm
      CHARACTER*80 CPUF
      character*60 errstr
      logical okreal4
c
      INCLUDE 'cform.inc'
      include 'stk.inc'
      include 'core.inc'
c
      EQUIVALENCE (CPUF,IPUF)
      equivalence (csmbm, dsmbm)
c
      DATA IHOLD /3/, ITEMP /2/
      data t12m/'NRAO 12M'/,nrao140/'NRAO 43M'/,nrao300/'NRAO 93M'/
      data specproc/'SPECPROC'/
      data n283/283/, m2/-2/, n120/120/, m3/-3/, n291/291/, n247/247/,
     .	   n112/112/, m1/-1/, n225/225/, n221/221/, n222/222/
c
C=======================================================================
C
      IPTWH = 1
c
      JJ=J
      GO TO (170, 250, 300, 400, 500, 600), JJ
      call oerror(n120, m3, 'AUB')
C-----------------------------------------------------------------------
C						RAP
C----------------------------------------------------------------------
  170 CONTINUE
      if(dtwh(c1tel,iptwh).eq.t12m) then
	goto 1709
      else if(dtwh(c1tel,iptwh).eq.nrao140 .or. 
     1	      dtwh(c1tel,iptwh).eq.nrao300) then
	goto 1719
      else
        call oerror(n283, m2, 'RAP')
      endif
c                                        Tucson Version
c 			input adverbs : fr = signal frequency (MHz)
c                                       fs = referenc freq (MHz)
1709  continue
      OSHFT=(FR-FS)/DTWH(C12FR,IPTWH)
c				oshift number of channels
      if(oshft.eq.0.0) call oerror(n291, m2, 'RAP')
      IF (OSHFT.le.0.0) then
c				
         IDF=nint(ABS(OSHFT))
         ISTART=DTWH(C12SPN,IPTWH) + IDATOFF
         ISTOP=DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     .         + IDATOFF -IDF - 1
         DO 172 JJ = ISTART,ISTOP
            if(okreal4(TWH(JJ,IPTWH)) .and. 
     .	       okreal4(TWH(JJ+IDF,IPTWH))) then
                 TWH(JJ,IPTWH)=(TWH(JJ,IPTWH)-TWH(JJ+IDF,IPTWH))/2.0
	    else if (defmode .ge. 0.5 .and. okreal4(TWH(JJ+IDF,IPTWH))) then
		 twh(jj,iptwh)=-TWH(JJ+IDF,IPTWH)
	    else if (defmode .lt. 0.5) then
		 twh(jj,iptwh)=rinfinity()
	    endif
  172       CONTINUE
         DTWH(C12NI,IPTWH)=DTWH(C12NI,IPTWH)-IDF
      else
c
         IDF=nint(OSHFT+0.5)
         ISTART=DTWH(C12SPN,IPTWH)+IDATOFF+IDF
         ISTOP=DTWH(C12SPN,IPTWH)+DTWH(C12NI,IPTWH)+
     .         IDATOFF - 1
         DO 177 JJ=ISTOP,ISTART,-1
            if(okreal4(TWH(JJ,IPTWH)) .and. 
     .	       okreal4(TWH(JJ-IDF,IPTWH))) then
                 TWH(JJ,IPTWH)=(TWH(JJ,IPTWH)-TWH(JJ-IDF,IPTWH))/2.0
	    else if (defmode .ge. 0.5 .and. okreal4(TWH(JJ-IDF,IPTWH))) then
		 twh(jj,iptwh)=-TWH(JJ-IDF,IPTWH)
	    else if (defmode .lt. 0.5) then
		 twh(jj,iptwh)=rinfinity()
	    endif
  177       CONTINUE
         DTWH(C12SPN,IPTWH)=DTWH(C12SPN,IPTWH)+IDF
         DTWH(C12NI,IPTWH)=DTWH(C12NI,IPTWH)-IDF
      endif
c
      DF=-(FS)/DTWH(C12FR,IPTWH)
      IDF=nint(DF)
c				set ref point
      DTWH(C12RP,IPTWH) = DTWH(C12RP,IPTWH) + FLOAT(IDF)
      GO TO 99
c                                      Green Bank Version
1719  continue
      ISTRT = dTWH(c12spn,IPTWH) + idatoff
      ISTP  = dTWH(c12ni,IPTWH) + istrt - 1
c
      dsmbm(1) = dtwh(c9cff,iptwh)
      dsmbm(2) = dtwh(c9cff+1,iptwh)
      dsmbm(3) = dtwh(c9cff+2,iptwh)
      if (csmbm(15:15) .eq. 'L') csmbm = '        ' // csmbm(1:16)
      read(unit=csmbm,fmt=9601) losgn, lm, sm, bm
 9601 FORMAT (12x,a1,1x,i1,1x,F2.0,1X,F2.0)
c     Get the LO and frontend multiplication factors, as well as whether
c     Sky Freq increases with LO frequency. LM = LO number; SM = LO 
c     multiplication; BM = Frontend box multiplications; LOSGN = - if
c     LO decreases as Sky Freq increases, else it should be + or blank.
c
      if (lm .eq. 1) then
      	kl = c9l1
      	klf1 = c9l1f1
      else
	kl = c9l2
	klf1 = c9l2f1
      endif
c     Make sure you get the right LO frequencies.
c
      oshft = ((DTWH(kl,IPTWH) - DTWH(klf1,IPTWH))*SM*BM) /
     .             dtwh(c12fr,iptwh)
      if (losgn .eq. '-') oshft = -oshft
C     Determine number of channel shift.
c
      IF (oshft.eq.0) call oerror(n247, m2, 'RAP')
c     Make sure that this is a frequency switched observation
c
      num = istp - istrt + 1
      inn = 2*num
      call copy(inn, twh(istrt,iptwh), ref)
      oshft = -oshft
      call shift(ref, num, oshft, ishift, ierr)
      if (ierr .ne. 0) call oerror(n247, m2, "RAP")
c     Create Reference Spectra
c
      if (dtwh(c1bke,iptwh).eq. specproc) then
        tsyson = dTWH(c12rst,IPTWH)
        tsysoff = sqrt( (dTWH(c12sst,IPTWH)**2)*2. - tsyson**2 )
      else
        tsysoff = dTWH(c12rst,IPTWH)
        tsyson = sqrt( (dTWH(c12sst,IPTWH)**2)*2. - tsysoff**2 )
      endif
c     For SP data, reverse roles of Tsys.
c     TSYS in the header is the quadratic sum of the original signal and
c     reference system temperatures.  Need to get back the original signal
c     TSYS.
c
      if (dtwh(c12eit,iptwh) .eq. dinfinity()) then
        dTWH(c12it,IPTWH) = dTWH(c12it,IPTWH) * 2.
      else
        dTWH(c12eit,IPTWH) = dTWH(c12eit,IPTWH) * 2.
      endif
      dTWH(c12rms,IPTWH) = dTWH(c12rms,IPTWH) / sqrt(2.)
c     Update all necessary header parameters (see memo by R.Maddalena
c	dated Jan 22, 1992).
c
      if (ishift .gt. 0) then
	i1 = istrt+ishift
	i2 = istp
	i3 = 1
      else
	i1 = istp+ishift
	i2 = istrt
	i3 = -1
      endif
c
      DO 1209 jj = i1, i2, i3
	if (okreal4(twh(jj,iptwh)) .and. okreal4(ref(jj-istrt+1))) then
		atemp = -tsysoff*ref(jj-istrt+1)/(tsyson + ref(jj-istrt+1))
		TWH(JJ,IPTWH) = (TWH(JJ,IPTWH) + atemp)/2.0
	else if (defmode .ge. 0.5 .and. okreal4(ref(jj-istrt+1))) then
		TWH(JJ,IPTWH) = -tsysoff*ref(jj-istrt+1)/(tsyson + 
     .					ref(jj-istrt+1))
	else if (defmode .lt. 0.5) then
	        twh(jj,iptwh) = rinfinity()
	endif
 1209   CONTINUE
c
      GO TO 99
C--------------------------------------------------------------------
C                            GMEASURE
C--------------------------------------------------------------------
  250 CONTINUE
c
      if (sp.lt.1) call oerror(n112, m1, 'GMEASURE')
c
      pr1 = v(sp)
      sp = sp - 1
c
      call gmeasure(pr1)
c
      GO TO 99
C ------------------------------------------------------------------
C                                        TEMP
C  Tucson: (0) = ON  (1) = OFF (2) = GAINS
c  Green Bank: (0) = ON  (1) = OFF
C ------------------------------------------------------------------
  300 CONTINUE
      if(dtwh(c1tel,iptwh).eq.nrao140.or.
     1   dtwh(c1tel,iptwh).eq.nrao300) then
	goto 3021
      else if(dtwh(c1tel,iptwh).eq.t12m) then
	goto 3001
      else
	call oerror(n283, m2, 'TEMP')
      endif
c                        Tucson Version
3001  istart = dtwh(c12spn,iptwh)+idatoff
      istop = istart + dtwh(c12ni,iptwh) - 1
      if (istart .gt. istop) call oerror(n225, m2, 'TEMP')
      if( itwh(1,iptwh) .eq. 0 .or. itwh(1,ihold) .eq. 0 .or. 
     1    itwh(1,itemp) .eq. 0) call oerror(n221, m2, 'TEMP')
c
      DO 302 I = istart, istop
	 if (okreal4(twh(i,iptwh)) .and. okreal4(twh(i,itemp)) .and.
     .	     okreal4(twh(i,ihold))) then
	    off = twh(i,itemp)
	    diff = twh(i,iptwh) - off
	    if (off .eq. 0.0) off = 1.0
            TWH(i,iptwh)= (diff / off)*twh(i,ihold)
	 else
	    twh(i,iptwh) = rinfinity()
	 endif
  302    CONTINUE
      GO TO 99
c                                 Green Bank version
3021  continue
      dTWH (c7osn,IPTWH) = dTWH (c1sno,ITEMP)
      ISTART = dTWH (c12spn,IPTWH) + idatoff
      ISTOP  = dTWH (c12ni,IPTWH) + istart - 1
      if (istart .gt. istop) call oerror(n222, m2, 'TEMP')
      if( itwh(1,iptwh) .eq. 0 .or. itwh(1,itemp) .eq. 0) 
     .		call oerror(n222, m2, 'TEMP')
c
      TSYSON   = dTWH (c12sst,iptwh) 
      TSYSOFF = dtwh(c12sst, itemp) 
      if (dtwh(c12eit,iptwh) .eq. dinfinity()) then
        TON    =  dTWH (c12it,IPTWH)
        TOFF   =  dTWH (c12it,ITEMP)
      else
        TON    =  dTWH (c12eit,IPTWH)
        TOFF   =  dTWH (c12eit,ITEMP)
      endif
      F = dtwh(c12rms,iptwh) *  sqrt(ton) /  tsyson
c     Get the basic/needed header parameters
c
      dTWH(c12eit,IPTWH) = TON*TOFF / (TON + TOFF)
      dtwh(c12sst,iptwh) = sqrt( (toff*tsyson**2 + ton*tsysoff**2) /
     .					(ton + toff) )
      dTWH(c12rms,IPTWH) = f * dtwh(c12sst,iptwh) / 
     .					sqrt(dTWH(c12eit,IPTWH) )
      dtwh(c12it, iptwh) = dtwh(c12it,iptwh) + dtwh(c12it,itemp)
      dTWH (c12rst,IPTWH) = dTWH (c12sst,ITEMP)
      dTWH (c4rx,IPTWH) = dTWH (c4sx,ITEMP)
      dTWH (c4ry,IPTWH) = dTWH (c4sy,ITEMP)
c     Update all necessary header parameters (see memo by R.Maddalena
c	dated Jan 22, 1992).
c
      if (dtwh(c1bke,iptwh).eq. specproc) then
	tsyson = tsysoff
	istart = istart + 1
	istop = istop - 1
      endif
c     For SP data, use Tsys_ref instead of signal; skip channel 1.
c
      DO 1829 IJ=ISTART,ISTOP
	if (okreal4(twh(ij,iptwh)) .and. okreal4(twh(ij,itemp))) then
             TWH(IJ,IPTWH)=(TWH(IJ,IPTWH)-TWH(IJ,ITEMP))/TWH(IJ,ITEMP)
     .                      *TSYSON
	else
	     twh(ij,iptwh)=rinfinity()
	endif
 1829   CONTINUE
      goto 99
c
C--------------------------------------------------------------------
C                            GETIR
C--------------------------------------------------------------------
  400 CONTINUE
c
      if (sp.lt.3) call oerror(n112, m1, 'GETIR')
c
      phase = nint(v(sp))
      record = nint(v(sp-1))
      scanno = v(sp-2)
      sp = sp - 3
c
      call getir(scanno, record, phase, iptwh, ierr)
c
      if (ierr .lt. 0) then
         call oerror(-ierr, m1, 'GETIR')
      else if (ierr .gt. 0) then
         if (ierr .eq. 362) then
            write(errstr, 1310) scanno
 1310       format('SCAN ',f10.2,' : GETIR')
         else if (ierr .eq. 371) then
            ierr = 357
            write(errstr, 1320) scanno
 1320       format('SCAN ',f10.2,
     .             ' is not a valid scan number : GETIR')
         else
            write(errstr, 1330)
 1330       format('GETIR')
         endif
         call oerror(ierr, m2, errstr)
      endif
      GO TO 99
c
C--------------------------------------------------------------------
C                            GETOTF
C--------------------------------------------------------------------
  500 CONTINUE
c
      if (sp.lt.2) call oerror(n112, m1, 'GETOTF')
c
      record = nint(v(sp))
      scanno = v(sp-1)
      sp = sp - 2
c
      call getotf(scanno, record, iptwh, ierr)
c
      if (ierr .lt. 0) then
         call oerror(-ierr, m1, 'GETOTF')
      else if (ierr .gt. 0) then
         if (ierr .eq. 362) then
            write(errstr, 1510) scanno
 1510       format('SCAN ',f10.2,' : GETOTF')
         else
            write(errstr, 1530)
 1530       format('GETOTF')
         endif
         call oerror(ierr, m2, errstr)
      endif
      GO TO 99
C--------------------------------------------------------------------
C                            GETPOLZ
C--------------------------------------------------------------------
  600 CONTINUE
c
      if (sp.lt.2) call oerror(n112, m1, 'GETPOLZ')
c
      record = nint(v(sp))
      scanno = v(sp-1)
      sp = sp - 2
c
      call getpolz(scanno, record, iptwh, ierr)
c
      if (ierr .lt. 0) then
         call oerror(-ierr, m1, 'GETPOLZ')
      else if (ierr .gt. 0) then
         if (ierr .eq. 362) then
            write(errstr, 1610) scanno
 1610       format('SCAN ',f10.2,' : GETPOLZ')
         else
            write(errstr, 1630)
 1630       format('GETPOLZ')
         endif
         call oerror(ierr, m2, errstr)
      endif
      GO TO 99
C--------------------------------------------------------------------
  99  CONTINUE
C
      RETURN
      END
c

