      SUBROUTINE LSQ ( GERR,IHOLD,SUMSQ,BGAUSS,EGAUSS,C)
c
c     @(#)cptlsq.f	5.1 06/22/94
c
      INTEGER*2 GERR, POS(18), IHOLD(1), BGAUSS, EGAUSS
      INTEGER*2 IBUF(2560), mu, i, j, ii, m, iii, iv, ier
      REAL C(120),VV(80),PART(120), sumsq, s, x, power, expon, term, arg
c
      include 'condappl.inc'
      include 'concfmt.inc'
c
      EQUIVALENCE (IBUF(1),RBUF(1))
c
      DATA POS/1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,
     .153,171/
C
      MU=3
      SUMSQ=0
      DO 10 I=1,120
10       C(I)=0
      DO 11 I=1,80
11       VV(I)=0
      DO 20 J=BGAUSS,EGAUSS
         S=0
         X=J
            POWER=(X-CENTER)/HWIDTH
            EXPON=-2.772*POWER**2
            IF (EXPON.LT.-172) EXPON=-172
            EXPON=EXP(EXPON)
            TERM=HEIGHT *EXPON
            ARG=5.544*POWER*TERM/HWIDTH
            S=S+TERM
            PART( 1)=EXPON
            PART( 2)=ARG
21          PART( 3)=ARG*POWER
         DO 22 I=1,MU
            II=I*(I-1)/2
            DO 23 M=1,I
23             C(II+M) = C(II+M) + PART(I) * PART(M)
 22         VV(I)=VV(I)+(RBUF(J+JDB-1)-S)*PART(I)
            SUMSQ=SUMSQ+(RBUF(J+JDB-1)-S)**2
20       CONTINUE
      DO 210 I=1,MU
         IF (IHOLD(I).EQ.0) GO TO 210
         II=POS(I)
         VV(I)=0
         C(II)=1.
         III=II-I+1
         IV=II-1
         DO 211 J=III,IV
211         C(J)=0
         III=MU-I
         DO 212 J=1, III
            II=II+J+I-1
212         C(II)=0
210      CONTINUE
      IF (GERR.EQ.1) GO TO 300
      CALL GEL (VV,C,MU,0.1E-06,IER,PART)
      IF (IER.EQ.-1) GO TO 250
         HEIGHT =HEIGHT +VV( 1) * .5
         CENTER =CENTER +VV( 2) * .5
         HWIDTH =HWIDTH +VV( 3) * .5
      GO TO 300
250      HEIGHT =-100.
300   CONTINUE
      RETURN
      END
C
