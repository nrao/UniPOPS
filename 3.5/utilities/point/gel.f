      SUBROUTINE GEL  (R, A, M,         EPS,      IER, AUX)
C-------------------------------------------------------------------------------
C  @(#)gel.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c****************************************
c Modified 8903 [RJM] See CHANGES.DOC
c****************************************
C--------------------------------------------------------------
C--------------------------------------------------------------
      integer*2 M,N,IER,L,K,I,LST,NM,LEND,LT,LL,LR,II,LLST,LLD,J
      REAL A(*),R(*),AUX(*),EPS,PIV,TB,TOL,PIVI
C                                         ON ERROR   GO TO 900
C
      IF (M.LE.0) GO TO      900
C    SEARCH FOR GREATEST MAIN DIAGONAL ELEMENT
      N=1
      IER=0
      PIV = 0.
      L = 0
      DO 15 K = 1,M
         L = L + K
         TB = ABS(A(L))
         IF (TB.LE.PIV) GO TO 15
            PIV=TB
            I=L
            J=K
15       CONTINUE
      TOL = EPS*PIV
C    MAIN DIAGONAL ELEMENT A(I) = A(J,J) IS FIRST PIVOT  ELEMENT.
C    PIV CONTAINS THE ABSOLUTE VALUE OF      A(I).
C    START ELIMINATION LOOP
      LST = 0
      NM = N*M
      LEND = M - 1
      DO 50 K = 1,M
C                      Test on usefulness of symnetric algorithm.
         IF (PIV.LE.0.) GO TO 900
         IF (IER.NE.0) GO TO 22
         IF (PIV.LE.TOL) IER=K-1
22       LT = J - K
         LST = LST + K
C    PIVOT ROW REDUCTION AND ROW INTERCHANGE IN RIGHT HAND SIDE  R
         PIVI =      1./A(I)
         DO 30 L = K , NM , M
            LL = L + LT
            TB = PIVI*R(LL)
            R(LL) = R(L)
30          R(L) = TB
C                                         Is eliminated terminated.
         IF (K.GE.M) GO TO 60
C    ROW AND COLUMN INTERCHANGE AND PIVOT ROW REDUCTION IN MATRIX A.
C    ELEMENTS OF PIVOT COLUMN ARE SAVED IN AUXILIARY VECTOR AUX.
         LR = LST + (LT*(K + J - 1))/2
         LL = LR
         L = LST
         DO 40 II = K ,  LEND
            L = L + II
            LL = LL + 1
            IF (L.LT.LR)  GO TO 42
            IF (L.GT.LR) GO TO 41
            A(LL)=A(LST)
            TB=A(L)
            GO TO      43
41          LL = L + LT
42          TB = A(LL)
            A(LL) = A(L)
43          AUX(II) = TB
40          A(L) = PIVI*TB
C    SAVE COLUMN INTERCHANGE INFORMATION
         A(LST) = LT
C    ELEMENT REDUCTION AND SEARCH FOR NEXT PIVOT
         PIV = 0.
         LLST = LST
         LT = 0
         DO 49 II = K ,  LEND
            PIVI = -AUX(II)
            LL = LLST
            LT = LT + 1
            DO 45 LLD = II ,  LEND
               LL = LL + LLD
               L = LL + LT
45             A(L) = A(L) + PIVI*A(LL)
            LLST = LLST +      II
            LR = LLST + LT
            TB = ABS(A(LR))
            IF (TB.LT.PIV) GO TO 46
            PIV=TB
            I=LR
            J=II + 1
46          DO 47 LR = K ,  NM , M
               LL = LR + LT
47             R(LL) = R(LL) + PIVI*R(LR)
49          CONTINUE
50       CONTINUE
C                                         End of elimination loop.
60    IF (LEND.LT.0) GO TO 900
      IF (LEND.EQ.0) GO TO 99
      II = M
      DO 70 I = 2,M
         LST = LST - II
         II = II - 1
         L = A(LST) + 0.5
         DO 71 J = II , NM , M
            TB = R(J)
            LL = J
            K = LST
            DO 72 LT = II , LEND
               LL = LL + 1
               K = K + LT
72             TB = TB - A(K)*R(LL)
            K = J + L
            R(J) = R(K)
            R(K) = TB
71          CONTINUE
70       CONTINUE
      GO TO 99
C                                          Error return.
 900  IER = -1
 99   RETURN
      END
