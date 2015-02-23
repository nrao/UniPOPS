      SUBROUTINE CONVOLVE
c
c     @(#)convolve.f	5.1 06/22/94
c
      INTEGER*2 CONDAR, IBUF(1), i, itype, npts, ipto, iby, ng, 
     .		iptr, j, nconv, jc, jp, index, jmax  
      REAL XMNRAD, rate, cosfac
      DOUBLE PRECISION
     +   ALPHA, CONVS(116), CTR, DEXPON, DL2, DPI, DXMNRD, FWHM,
     +   GPROF(22), PSCAN(137), SIGMA, SMAX, SS, SUM, S2, WEIG, X0,
     +   Y1, Y2, Y3, QUOT1, QUOT2, dfloat
c
      include 'condappl.inc'
      include 'coneappl.inc'
      include 'concfmt.inc'
c
      EQUIVALENCE (IBUF, RBUF)
c
      DATA CONDAR, XMNRAD /3, 3437.75/
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C                   Statement function definition
      DFLOAT (I) = DBLE (FLOAT (I))
C
      ITYPE = IBUF(KTOS)
      IF (ITYPE .NE. CONDAR) GO TO 9000
      NPTS = IBUF(KIN)
C
C                   Do one buffer only
      IF (NPTS .GT. IFULL) GO TO 9000
      IPTO = JDB
C***  IBY = IBUF(KLR) + 4
C***  IF (MOD (IBY, 2) .NE. 0) IBY = IBY + 1
         IBY = 1
C
      DPI = 4.D0 * DATAN (1.D0)
      DXMNRD = 60.D0 * 180.D0 / DPI
      DL2 = DLOG (2.D0)
      RATE = ABS (RBUF(KHR + IABS (IBUF(KDIR)) - 1))
      IF (RATE .EQ. 0.) GO TO 9000
      COSFAC = 1.
      IF (IABS (IBUF(KDIR)) .EQ. 1) COSFAC = COS (RBUF(JERA + 3))
      FWHM = 4.D0 * DL2
C
C                   Full width half max (samples - 1)
      SIGMA = (60.D0 / DXMNRD) / DBLE (RATE * RBUF(KSRT)
     +   * COSFAC / HWIDTH)
C
C                   Gaussian denominator
      S2 = SIGMA ** 2 / FWHM
C
C                   Number of points in convolving beam
      NG = IDINT (SIGMA + 1.5D0)
C
C                   Center sample of Gaussian beam
      CTR = DFLOAT (NG / 2 + 1)
C
C                   Copy the scan into working array
      IPTR = IPTO
      DO 1000 J = 1, NPTS
         PSCAN(J) = DBLE (RBUF(IPTR))
         IPTR = IPTR + IBY
 1000    CONTINUE
C
C                   Create the convolving beam
C                   (zero beyond FWHM)
      WEIG = 0.D0
      DO 2000 J = 1, NG
         GPROF(J) = 0.D0
         DEXPON = (DFLOAT (J) - CTR) ** 2 / S2
         IF (DEXPON .GT. DL2) GO TO 2000
            GPROF(J) = DEXP (-DEXPON)
            WEIG = WEIG + GPROF(J)
 2000    CONTINUE
C
C                   Compute the convolution sums
      NCONV = NPTS - NG + 1
      SMAX = -1.D76
      DO 4000 JC = 1, NCONV
         SUM = 0.D0
         DO 3000 JP = 1, NG
            INDEX = JP + JC - 1
            SUM = SUM + GPROF(JP) * PSCAN(INDEX)
 3000       CONTINUE
C
         CONVS(JC) = SUM
C
C                   Update max
         IF (SUM .LE. SMAX) GO TO 4000
            SMAX = SUM
            JMAX = JC
 4000    CONTINUE
C
C                   Error if max on first or last point
      IF (JMAX .EQ. 1 .OR. JMAX .EQ. NCONV) GO TO 9000
C
C                   Run an interpolating Gaussian
C                   through max & two neighbors
      Y1 = CONVS(JMAX - 1)
      Y2 = CONVS(JMAX)
      Y3 = CONVS(JMAX + 1)
C
      QUOT1 = Y2 / Y1
      QUOT2 = Y2 / Y3
      IF (QUOT1 .LE. 0.D0 .OR. QUOT2 .LE. 0.D0) GO TO 9000
C
      ALPHA = DLOG (QUOT1) / DLOG (QUOT2)
C
      X0 = .5D0 * (ALPHA - 1.D0) / (ALPHA + 1.D0)
      SS = (2.D0 * X0 + 1.D0) / DLOG (Y2 / Y1)
C
      CENTER = FLOAT (JMAX - 1) + SNGL (CTR + X0)
      HEIGHT = SNGL (Y2 / DEXP (-X0 ** 2 / SS)) / WEIG
C
      GO TO 99
C
C                   Error conditions
 9000 CONTINUE
      GOK = 1
      WRITE (4, 90000)
      GO TO 99
C----------------------------------------------------------------------
 99   CONTINUE
      RETURN
C-----------------------------------------------------------------------
90000 FORMAT (' Convolution failed - - continuing')
      END
C
