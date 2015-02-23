      SUBROUTINE GAUSS
c
c     @(#)cptgauss.f	5.1 06/22/94
c
      INTEGER*2 IHOLD(18),BGAUSS,EGAUSS
      INTEGER*2 IBUF(2560), ipr, ity, istart, istop, kount, i, ii, mu,
     .		ihw, ictr, npts
      REAL    C(120), comp, sumsq, ht, half
c
      include 'condappl.inc'
      include 'concfmt.inc'
c
      EQUIVALENCE (IBUF(1),RBUF(1))
c
      DATA IPR/4/, ITY/6/
c
      ISTART=IBUF(JBEG)
      ISTOP=IBUF(JEND)
      NPTS=ISTOP-ISTART+1
      KOUNT=1
      COMP=1E34
      SUMSQ=0
      DO 10 I=1,18
 10      IHOLD(I)=0
      HT = -1.E34
C                                       FIND MAXIMUM POINT
      DO 20 I = ISTART, ISTOP
         IF (RBUF(I).LT.HT) GO TO 20
         CENTER=FLOAT(I-JDB+1)
         HT=RBUF(I)
         ICTR=I
  20     CONTINUE
      HEIGHT=HT
      HALF=HT*0.5
      DO 22 I = ICTR,ISTOP
         IF (RBUF(I).GT.HALF) GO TO 22
         HWIDTH=(FLOAT(I-JDB+1)-CENTER)*2.0
         IHW=(I-ICTR)
         EGAUSS=I-JDB+1
         BGAUSS=ICTR-IHW-JDB+1
         IF (IHW.LT.3) GO TO 42
         GO TO 24
  22     CONTINUE
C                                       Gaussian Fit Profile.
 24   CALL LSQ (0,IHOLD,SUMSQ,BGAUSS,EGAUSS,C)
      IF (ABS(SUMSQ-COMP).LT.1E-05) GO TO 30
      COMP=SUMSQ
      KOUNT=KOUNT+1
      IF (KOUNT.LE.8) GO TO 24
 30   MU=3
      CALL LSQ (1,IHOLD,SUMSQ,BGAUSS,EGAUSS,C)
      CALL MFS (C,MU,1E-06)
      CALL MIS (C,MU)
      SUMSQ=SQRT (SUMSQ/(NPTS-MU))
      II=1
      DO 40  I=1,MU
         C(I) = SUMSQ*SQRT(C(II))
 40      II=I+II+1
         HERR = C(1)
C     WRITE (IPR,999) HERR
C999  FORMAT (1X,'  HT. ERR. = ',F8.4)
      GO TO 99
C                                       FIT FAILED
  42  GOK = 1
      IF (IBUF(KDIR).EQ.1) WRITE (IPR,201)
      IF (IBUF(KDIR).EQ.-1) WRITE (IPR,202)
      IF (IBUF(KDIR).EQ.2) WRITE (IPR,203)
      IF (IBUF(KDIR).EQ.-2) WRITE (IPR,204)
C--------------------------------------------------------------------
 99   CONTINUE
      RETURN
C-----------------------------------------------------------------------
 201  FORMAT (' FIT FAILED IN THE EAST - WEST DIRECTION -- CONTINUING')
 202  FORMAT (' FIT FAILED IN THE WEST - EAST DIRECTION -- CONTINUING')
 203  FORMAT (' FIT FAILED IN THE SOUTH-NORTH DIRECTION -- CONTINUING')
 204  FORMAT (' FIT FAILED IN THE NORTH-SOUTH DIRECTION -- CONTINUING')
      END
C
