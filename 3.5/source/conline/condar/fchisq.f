      real FUNCTION FCHISQ(Y,SIGMAY,NPTS,NFREE,MODE,YFIT)
C-----------------------------------------------------------------------
C @(#)fchisq.f	5.1 06/22/94
C-----------------------------------------------------------------------
C
C  THIS FUNCTION SUBPROGRAM EVALUATES THE REDUCED CHI-SQUARE FOR A
C  FIT TO THE DATA WITH A GIVEN SET OF FIT PARAMETERS.  THE REDUCED
C  CHI-SQUARE FUNCTION IS
C      FCHISQ = SUM ((Y - YFIT)**2 / SIGMA**2) / NFREE
C
C  VARIABLES HAVE BEEN DESCRIBED IN THE OTHER SUBROUTINES.
C
      real Y(*), SIGMAY(*), YFIT(*), chisq, weight, free
      integer*2 npts, nfree, mode, i  
c
   11 CHISQ = 0.
   12 IF(NFREE) 13, 13, 20
   13 FCHISQ = 0.
      GO TO 40
C
C  ACCUMULATE CHI SQUARE
C
   20 DO 30 I=1,NPTS
   21 IF(MODE) 22, 27, 29
   22 IF(Y(I)) 25, 27, 23
   23 WEIGHT = 1. / Y(I)
      GO TO 30
   25 WEIGHT = 1. / (-Y(I))
      GO TO 30
   27 WEIGHT = 1.
      GO TO 30
   29 WEIGHT = 1. / SIGMAY(I)**2
   30 CHISQ = CHISQ + WEIGHT*(Y(I) - YFIT(I))**2
C
C  DIVIDE BY NUMBER OF DEGREES OF FREEDOM
C
   31 FREE = NFREE
   32 FCHISQ = CHISQ/FREE
   40 RETURN
      END

