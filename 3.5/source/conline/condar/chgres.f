      SUBROUTINE CHGRES(TOLD,NCHAN,OLDRES,NEWRES)
C-------------------------------------------------------------------------------
C  @(#)chgres.f	5.2 09/10/98
C-------------------------------------------------------------------------------
C
C  THIS SUBROUTINE SMOOTHS SPECTRAL LINE DATA FROM ITS INITIAL
C  RESOLUTION TO A LOWER RESOLUTION SPECIFIED BY THE ADVERB
C  'NEWRES'.  THE ROUTINE WORKS BY STEPPING A GAUSSIAN OF
C  APPROPRIATE FWHM THROUGH THE EXISTING SPECTRUM AND PERFORMING
C  A CONVOLUTION AT EACH POSITION.  THE ROUTINE ASSUMES THAT
C  THERE ARE AN EVEN NUMBER OF CHANNELS IN THE INITIAL SPECTRUM
C  AND THAT THE CENTER FREQENCY IN THE SCAN HEADER CORRESPONDS
C  TO CHANNEL N/2 + 0.5, E.G., 64.5, 128.5, ETC.  EACH INDIVIDUAL
C  CONVOLUTION IS PERFORMED OUT TO A SPECIFIED NUMBER OF STANDARD
C  DEVIATIONS OF THE CONVOLVING GAUSSIAN, SAY 5 OR 6 SIGMA.  THE
C  SMOOTHING IS TERMINATED WHEN THE CONVOLVING GAUSSIAN IS WITHIN
C  A SPECIFIED NUMBER OF SIGMAS OF THE ENDS OF THE INITIAL SPECTRUM
C  SO THAT THE END POINTS OF THE NEW SPECTRUM REPRESENT VALID 
C  INFORMATION (2 OR 3 SIGMA IS A REASONABLE LIMIT).  THE REMAINING
C  POINTS IN THE NEW SPECTRUM ARE SET EQUAL TO ZERO;  THE USER CAN
C  DROP THESE POINTS FROM THE DISPLAYED SPECTRUM.
c
c  The center freq./reference channel need not be N/2 + 0.5, the
c   reference channel is corrected in au2 after chgres returns
C
C ** WRITTEN BY P. R. JEWELL AND E. B. STOBIE  7 NOV 1985 **
C ** LAST MODIFIED BY P.R.J. AND E.B.S. 9 NOV 1985 **
c   Modified 8903 [RJM] See CHANGES.DOC
C
      include 'params.inc'
c
      real*4 TOLD(*), TNEW(MAX_DATA_POINTS)
      REAL*4 NEWRES, oldres, conres, a2, rta2, conlim, conend,
     .       freqend, bord, conbord, cchan, chstep, chpos, xarg, xnorm,
     .       freqlc, chlim, xdist, arg, rinfinity
      integer*2 n120, n243, m2, m3, n269
      integer*2 nchan, ncon, i, j, ichan, istart, istop
      logical okreal4
c
      include 'cio.inc'
c
      data n120, n243, m2, m3 /120, 243, -2, -3/
      data n269 /269/
C
C  KEY TO VARIABLE NAMES:
C     	OLDRES = ORIGINAL FREQ. RESOLUTION OF THE SPECTRUM
C	NCHAN = TOTAL NUMBER OF CHANNELS IN THE SPECTRUM
C	NEWRES = DESIRED NEW FREQ. RESOLUTION OF THE SPECTRUM
c              = FWHM of convolving gaussian
C	CONRES = RESOLUTION OF THE CONVOLVING GAUSSIAN
C	CONLIM = NUMBER OF SIGMAS OF THE CONVOLVING GAUSSIAN TO
C                  CARRY AN INDIVIDUAL CONVOLUTION OUT TO.
C	CONEND = NUMBER OF SIGMAS OF THE CONVOLVING GAUSSIAN TO
C                  ALLOW AS A BORDER AT THE 2 ENDPOINTS OF THE SPEC.
C
c		newres must be > oldres
      IF(NEWRES .lt. OLDRES .or. min(newres,oldres).le.0.0) 
     .    call oerror(n243, m2, 'CHNGRES')
c		NCHAN must be even
      if (mod(nchan,2) .ne. 0) call oerror(n269, m2, 'CHNGRES')
c
      if (nchan .lt. 1 .or. nchan .gt. MAX_DATA_POINTS)
     .   call oerror(n120, m3, 'CHNGRES')
      CCHAN = NCHAN / 2. + 0.5
C
C  THE FWHM'S OF CONVOLVING GAUSSIANS ADD QUADRATICALLY:
C      CONRES = SQRT( NEWRES*NEWRES - OLDRES*OLDRES )
c			I don't understand why the below is preferred over
c                       the above - rwg.
      CONRES = NEWRES
      A2 = 4.0 * ALOG( 2. )
      RTA2 = SQRT( A2 )
      XARG = RTA2 * OLDRES / CONRES
      CONLIM = 4.
      CONEND = 1.
C  A STANDARD DEVIATION SIGMA = 0.4248 * FWHM
c			0.4248 = 1 / sqrt(8 ln(2))
C  CALCULATE THE NUMBER OF CONVOLUTIONS PERFORMED ON EITHER SIDE OF THE
C    CENTER POINT
      FREQEND = ( NCHAN - CCHAN ) * OLDRES
      CONBORD = CONEND * .4248 * CONRES
      NCON = FREQEND / NEWRES
      FREQLC = NCON * NEWRES
      BORD = FREQEND - FREQLC
      IF(BORD .LT. CONBORD) NCON = NCON - 1
C
C  IN THE LOOPS BELOW, THE OUTER LOOP STEPS THE CONVOLVING GAUSSIAN
C  THROUGH THE SPECTRUM WITH STEPSIZES = NEWRES;  THE INNER LOOP
C  PERFORMS THE INDIVIDUAL CONVOLUTIONS.  TWO SETS OF LOOPS ARE USED,
C  THE FIRST WORKING FROM THE CENTER TO HIGHER CHANNELS AND THE
C  SECOND FROM THE CENTER TO LOWER CHANNELS.
C
C  COMPUTE THE STEPSIZE, THE LIMIT OF THE CONVOLUTION, AND THE POSITION
C  OF THE CENTER OF THE CONV. GAUSSIAN IN FRACTIONAL CHANNEL NUMBERS.
C
      CHSTEP = NEWRES / OLDRES
      CHLIM = CONLIM * .4248 * CHSTEP
      CHPOS = CCHAN + 0.5 * CHSTEP
C
C  INITIALIZE THE NEW SPECTRUM TO ZERO
      DO 30 I = 1, NCHAN
        TNEW(I) = 0.0
  30  CONTINUE
C
C  DO THE CONVOLUTION FOR THE UPPER HALF OF THE SPECTRUM:
C
      ICHAN = CCHAN
      DO 60 I = 1, NCON
        ICHAN = ICHAN + 1
        ISTART = CHPOS - CHLIM + 0.5
        ISTOP = CHPOS + CHLIM + 0.5
        IF(ISTART .LT. 1) ISTART = 1
        IF(ISTOP .GT. NCHAN) ISTOP = NCHAN
	xnorm = 0.0
        DO 55 J = ISTART, ISTOP
          XDIST = CHPOS - J
          ARG = -XARG * XDIST * XARG * XDIST
          if (okreal4(told(j))) then
          	TNEW(ICHAN) = TOLD(J) * EXP(ARG) + TNEW(ICHAN)
		xnorm = xnorm + exp(arg)
	  endif
  55    CONTINUE
        if (xnorm .gt. 0.0) then
	   TNEW(ICHAN) = TNEW(ICHAN)/xnorm
	else
	   tnew(ichan) = rinfinity()
	endif
        CHPOS = CHPOS + CHSTEP
  60  CONTINUE
C
C  DO THE CONVOLUTION FOR THE LOWER HALF OF THE SPECTRUM:
C
      CHPOS = CCHAN - 0.5 * CHSTEP
      ICHAN = CCHAN + 1.
      DO 70 I = 1, NCON
        ICHAN = ICHAN - 1
        ISTART = CHPOS - CHLIM + 0.5
        ISTOP = CHPOS + CHLIM + 0.5
        IF(ISTART .LT. 1) ISTART = 1
        IF(ISTOP .GT. NCHAN) ISTOP = NCHAN
	xnorm = 0.0
        DO 65 J = ISTART, ISTOP
          XDIST = CHPOS - J
          ARG = -XARG * XDIST * XARG * XDIST
          if (okreal4(told(j))) then
          	TNEW(ICHAN) = TOLD(J) * EXP(ARG) + TNEW(ICHAN)
		xnorm = xnorm + exp(arg)
	  endif
  65    CONTINUE
        if (xnorm .gt. 0.0) then
	   TNEW(ICHAN) = TNEW(ICHAN)/xnorm
	else
	   tnew(ichan) = rinfinity()
	endif
        CHPOS = CHPOS - CHSTEP
  70  CONTINUE
C  
C  REPLACE THE OLD SPECTRUM AND HEADER VALUES WITH THE NEW VALUES:
C
      DO 80 I = 1, NCHAN
	if (i .lt. cchan-ncon .or. i .gt. cchan+ncon) then
          TOLD(I) = rinfinity()
	else
          TOLD(I) = TNEW(I)
	endif
  80  CONTINUE
  99  RETURN
      END
