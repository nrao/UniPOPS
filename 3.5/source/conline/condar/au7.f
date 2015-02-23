      SUBROUTINE AU7 (J)
C-------------------------------------------------------------------------------
C  @(#)au7.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     au7 - FFT, IFFT, Powspec
c
C-----------------------------------------------------------------------
c
      integer*2 j, m2, m3, n2, n120, n225, n288, n268
      integer*4 jj, iptwh, iwork, itemp, jcnt, in, i, it , ISTART,
     .		ISTOP, ISTT, ISPT, inn
      real rinfinity
      logical okreal4
      logical*4 invers
c
      include 'core.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
      include 'cio.inc'
c
      DATA IWORK/1/, ITEMP/2/
c
      data m2, m3, n2, n120, n225, n288, n268
     .    /-2, -3, 2, 120, 225, 288, 268/
C
C=======================================================================
C
      IPTWH=1
c
      JJ=J
      GO TO (10,10,20), JJ
      call oerror(n120, m3, 'AU7')
C-----------------------------------------------------------------------
C					 FFT, IFFT	  
C-----------------------------------------------------------------------
   10 CONTINUE
      if (bdrop.lt.0. .or. edrop.lt.0. .or.
     .    bdrop+edrop.gt.dtwh(c12ni,iptwh)) 
     .		call oerror(n288,m2,'FFT/IFFT')
c
      ISTART = DTWH(C12SPN,IPTWH) + IDATOFF + BDROP
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH)
     .        + IDATOFF - 1 - EDROP
c
      if (istart.ge.istop) call oerror(n225, m2, 'FFT/IFFT')
c
      if (jj .eq. 1) then
	 invers = .false.
	 in = (istop - istart + 1)
         DO 15 I = 1,IN
	    if (.not. okreal4(twh(istart+i-1,iwork)))
     .		call oerror(n268, m2, "FFT/IFFT: Cannot proceed")
	    xdata(i) = twh(istart+i-1,iwork)
            YDATA(I)=0.0
   15       CONTINUE
      ELSE
	 invers = .true.
         ISTT = DTWH(c12SPN,ITEMP) + IDATOFF + BDROP
         ISPT = DTWH(c12SPN,ITEMP) + DTWH(c12NI,ITEMP)
     .          + IDATOFF - 1 - EDROP
         if (istt.ge.ispt) call oerror(n288, m2, 'FFT/IFFT')
c
         IN = min(ISTOP-ISTART, ispt-istt) + 1
         DO 335 I = 1,IN
	    if (.not. okreal4(twh(istart+i-1,iwork)) .or. 
     .		.not. okreal4(twh(istt+i-1,itemp)) ) 
     .		call oerror(n268, m2, "FFT/IFFT: Cannot proceed")
	    xdata(i) = twh(istart+i-1,iwork)
	    ydata(i) = twh(istt+i-1,itemp)
  335       CONTINUE
      ENDIF
c     Fill ydata with zeros if it is a fft, else fill it with what is in 
c     array 2
c
      jCNT=0
   16   jCNT=jCNT+1
        IF (2**jcnt .lt. in) GO TO 16
c
      IF (2**jcnt .eq. in) THEN
         CALL FFT(XDATA,YDATA,jCNT,INVERS)
      ELSE
         CALL DFT(XDATA,YDATA,IN,INVERS)
      ENDIF
c
      inn = 2*in
      call copy (idatoff*n2, itwh(1,iwork), itwh(1,itemp))
      CALL COPY2 (INN,XDATA,TWH(ISTART,IWORK))
      CALL COPY2 (INN,YDATA,TWH(ISTART,ITEMP))
c
      GO TO 99
C----------------------------------------------------------------
C                                POWSPEC    
C----------------------------------------------------------------
   20 CONTINUE
      ISTART = DTWH(c12SPN,IWORK) + IDATOFF + BDROP
      ISTOP = DTWH(c12SPN,IWORK) + DTWH(c12NI,IWORK)
     .        + IDATOFF - 1 - EDROP
      ISTT = DTWH(c12SPN,ITEMP) + IDATOFF + BDROP
      ISPT = DTWH(c12SPN,ITEMP) + DTWH(c12NI,ITEMP)
     .       + IDATOFF - 1 - EDROP
c
      if (istart.ge.istop) call oerror(n288, m2, 'POWSPEC')
      if (istt.ge.ispt) call oerror(n288, m2, 'POWSPEC')
c
      DO 21 I = ISTART,ISTOP
         IT = ISTT + I - istart
	 if (it .le. ispt) then
	   if (okreal4(twh(i,iwork)) .and. 
     .	       okreal4(twh(it,itemp)) ) then
	     TWH(I,IWORK) = TWH(I,IWORK)**2+TWH(IT,ITEMP)**2
	   else if (defmode .ge. 0.5 .and. okreal4(twh(it,itemp))) then
	     TWH(I,IWORK) = TWH(IT,ITEMP)**2
	   else if (defmode .ge. 0.5 .and. okreal4(twh(i,iwork))) then
	     TWH(I,IWORK) = TWH(i,iwork)**2
	   else 
	     TWH(I,IWORK) = rinfinity()
	   endif
	 endif
   21    CONTINUE
c
      GO TO 99
C
C----------------------------------------------------------------
   99 CONTINUE
      RETURN
      END
