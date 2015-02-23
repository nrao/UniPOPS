      SUBROUTINE VSNTIP(X, Y, NPTS, FREQ, TAMBC, TVANE, FTSBR, FTM, 
     &  TAU0, DELTAU, ETAL, DELETA, ETAFREE, NOCVG, CHISQR) 	
C-----------------------------------------------------------------------
C @(#)vsntip.f	5.1 06/22/94
C-----------------------------------------------------------------------
C
C  THIS SUBROUTINE PERFORMS A NON-LINEAR LEAST-SQUARES FIT TO VANE
C  SWITCHED TIPPING SCAN DATA.  THE FUNCTION FIT IS
C
C          TCAL = RTCHOP - ETAL*RTM*(1 - EXP(-TAU0 * X))
C          - (1 - ETAL)*RTSBR - ETAL*RTBG*EXP(-TAU0 * X)
C
C  THE SUBROUTINE IS DERIVED FROM THE PROGRAM NLIN, WHICH PERFORMS A
C  LEAST-SQUARES FIT OF AN ARBITRARY USER-SPECIFIED, NON-LINEAR
C  FUNCTION.  THE FITTING SUBROUTINES WERE TAKEN FROM BEVINGTON, P. R.
C  'DATA REDUCTION AND ERROR ANALYSIS FOR THE PHYSICAL SCIENCES' 1969,
C  MCGRAW-HILL.  THE FITTING ROUTINE IS BASED ON MARQUARDT'S COMPROMISE
C  AND IS DESCRIBED IN CHAP. 11 OF BEVINGTON. 
C
C  NLIN WAS DESIGNED TO BE AS GENERAL AS POSSIBLE.  TO CHANGE THE FITTING
C  FUNCTION, THE FOLLOWING SUBROUTINES MUST BE CHANGED:
C
C    FUNCTION SUBPROGRAM FUNCTN.   SPECIFIES THE FUNCTION TO BE FITTED
C    TO THE DATA.
C
C    SUBROUTINE FDERIV.   COMPUTES THE PARTIAL DERIVATIVES OF THE FITTING
C    FUNCTION WRT EACH PARAMETER OF THE FIT.  THE USER MAY SPECIFY THE
C    PARTIAL DERIVATIVE EXPLICITLY FOR ANALYTICAL COMPUTATION OR MAY 
C    CHOOSE TO HAVE THE PARTIALS COMPUTED NUMERICALLY.  IN THE LATTER
C    CASE, NO CHANGES NEED BE MADE TO THE SUBROUTINE FROM ONE FITTING
C    FUNCTION TO ANOTHER.
C
C  OTHER SUBROUTINES USED BUT NOT SUBJECT TO CHANGE ARE:
C
C    CURFIT,  THE BASIC NON-LINEAR LEAST-SQUARES FITTING ROUTINE
C    USING MARQUARDT'S METHOD (BEVINGTON, P. 237, PROG. 11-5);
C
C    FCHISQ,  WHICH CALCULATES THE REDUCED CHI-SQUARE OF THE FIT
C    FOR A GIVEN SET OF PARAMETERS (BEVINGTON, P. 194, PROG. 10-2);
C
C    MATINV,  WHICH INVERTS A SQUARE MATRIX AND COMPUTES ITS 
C    DETERMINANT (BEVINGTON, P. 302, PROG. B-2).
C
C  ARGUMENTS TO THE SUBROUTINE:
C
C    X      - THE ARRAY CONTAINING THE AIRMASS AT EACH OBSERVATION
C    Y      - THE ARRAY CONTAINING THE VALUES OF (TA_VANE - TA_SKY) AT EACH
C                AIRMASS 
C    NPTS   - THE NUMBER OF DATA POINTS
C    FREQ   - THE OBSERVING FREQUENCY IN GHZ
C    TAMBC  - THE AMBIENT TEMPERATURE IN DEGREES CELSIUS
C    TVANE  - THE TEMPERATURE OF THE VANE IN DEGREES CELCIUS
C    FTSBR  - THE FRACTION (T_SPILLOVER / T_AMBIENT)
C    FTM    - THE FRACTION (T_ATM / T_AMBIENT)
C    TAU0   - THE ZENITH OPTICAL DEPTH (UPON THE CALL TO THE SUBROUTINE,
C                TAU0 IS THE STARTING VALUE; UPON RETURN, THE FINAL FITTED
C                VALUE
C    DELTAU - THE STANDARD ERROR IN THE FITTED VALUE OF TAU0
C    ETAL   - THE REAR SPILLOVER AND SCATTERING EFFICIENCY (STARTING OR
C                FIXED VALUE ON THE CALL, FITTED VALUE ON THE RETURN)
C    DELETA - THE STANDARD ERROR IN THE FITTED VALUE OF ETAL
C    ETAFREE- SPECIFIES IF ETAL IS TO BE A FREE OF FIXED PARAMETER (0 = FIXED,
C                1 = FREE)
C    NOCVG  - A ERROR CONDITION PARAMETER SET TO 1 IF THE FIT DID NOT CONVERGE
C    CHISQR - THE REDUCED CHI-SQUARED OF THE FINAL FIT
C
C ************************************************************************
C    PROGRAM NLIN WAS FIRST WRITTEN BY P. R. JEWELL ON 1984 JAN 30
C    SUBROUTINE VSNTIP LAST MODIFIED ON 9 SEP 86.
C ************************************************************************
C
C
      real X(*), Y(*), SIGMAY(1000), YFIT(1000), PARMS(25), CONST(25),
     .     SIGPARM(25), DELTAA(25), freq, tambc, tvane, ftsbr, ftm, 
     .     tau0, deltau, etal, deleta, etafree, chisqr, cvgcrit, rtm, 
     .     tm, rt, rtsbr, tsbr, tbg, rtbg, rtchop, tchop, tamb, functn, 
     .     chisq1, fchisq, flamda, abdiff
      integer*2 npts, nocvg, niter, mode, mderiv, modfit, nparms,
     .		nfree, iter, i 
      LOGICAL ETAFRE
c
      ETAFRE = .FALSE.
      IF (ETAFREE .EQ. 1) ETAFRE = .TRUE.
      NOCVG = 0
C  DEFINE FIXED INPUT PARAMETERS:
      NITER = 20
      CVGCRIT = 0.0005
C  MODE = 0 MEANS NO WEIGHTING (I.E., ALL POINTS HAVE EQUAL WEIGHTING).
      MODE = 0
      TAMB = TAMBC + 273.15 
      TM = FTM*TAMB
      TSBR = FTSBR*TAMB
      RTM = RT(FREQ,TM)
      RTSBR = RT(FREQ,TSBR)
      TBG = 2.7
      RTBG = RT(FREQ,TBG)
      TCHOP = TVANE + 273.15
      RTCHOP = RT(FREQ,TCHOP)
C
C *** SET THE DERIVATIVE-CALCULATION-MODE FLAG TO 'ANALYTIC' AS
C *** THE NUMERICAL CALCULATION MODE IS NOT FULLY IMPLEMENTED.
      MDERIV = 'A'
C
C  THE FOLLOWING CONVENTION IS USED TO DETERMINE THE FIT MODE:
C    IF ETAL IS FIXED, MODFIT = 1
C    IF ETAL IS FREE, MODFIT = 2
C
      IF(.NOT.ETAFRE) MODFIT = 1
      IF(ETAFRE) MODFIT = 2
      CONST(1) = FREQ
      CONST(2) = TAMB
      CONST(3) = FTM
      CONST(4) = TM
      CONST(5) = RTM
      CONST(6) = FTSBR
      CONST(7) = TSBR
      CONST(8) = RTSBR
      CONST(9) = TBG
      CONST(10) = RTBG
      CONST(11) = 0.
      CONST(12) = 0.
      CONST(13) = TCHOP
      CONST(14) = RTCHOP
      CONST(15) = 0.
      CONST(16) = 0.
      PARMS(1) = TAU0
      NPARMS = 2
      GO TO (175,180) MODFIT
  175 CONTINUE
      CONST(15) = ETAL
      NPARMS = 1
      GO TO 191
  180 CONTINUE
      PARMS(2) = ETAL
  191 CONTINUE
      NFREE = NPTS - NPARMS
C
C  EVALUATE CHI-SQUARE FOR STARTING VALUES OF FIT:
C
      DO 20 I=1,NPTS
      YFIT(I) = FUNCTN(X,I,PARMS,CONST,MODFIT)
   20 CONTINUE
      ITER = 0
      CHISQ1 = FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
C
C  START ITERATION LOOP:
C
   30 CONTINUE
      ITER = ITER + 1
      IF(ITER.GT.NITER) GO TO 45
      FLAMDA = 0.001
      CALL CURFIT(X,Y,SIGMAY,NPTS,NPARMS,MODE,PARMS,DELTAA,
     & SIGPARM,FLAMDA,YFIT,CHISQR,CHISQ1,CONST,MDERIV,MODFIT)
C
C  CHECK FOR CONVERGENCE:
C
      ABDIFF = ABS(CHISQR - CHISQ1)
      IF(ABDIFF.LT.CVGCRIT) GO TO 40
C
      CHISQ1 = CHISQR
      GO TO 30
C
   40 CONTINUE
      TAU0 = PARMS(1)
      DELTAU = SIGPARM(1)
      IF (MODFIT .EQ. 2) THEN
         ETAL = PARMS(2)
         DELETA = SIGPARM(2)
         END IF
      DO 42 I = 1,NPTS
         Y(I)=FUNCTN(X,I,PARMS,CONST,MODFIT)
   42    CONTINUE
      GO TO 555
   45 CONTINUE
      NOCVG = 1
C
  555 RETURN
      END
C
C  ************************************************************************
C
