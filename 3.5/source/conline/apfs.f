      SUBROUTINE APFS (WORK,IP,IRES,IOP,EPS,ETA,IER)
C-------------------------------------------------------------------------------
C  @(#)apfs.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C*******************************************
* Modified 8903 [RJM] See CHANGES.DOC
c*******************************************
      integer*2 ip, ires, iop, ier, ipiv, ite, ipp1, iend, i, ja, je, 
     .          jk, k, iadr, jj, j
      real*4 eps, eta, tol, test, sum, piv
      real*4 WORK(*)
      IRES=0
C
C                                       Test specified dimension.
      IF (IP.GT.0) GO TO 2
         IER=-1
         RETURN
C
C        INITIALIZE FACTORIZATION PROCESS
    2 IPIV=0
      IPP1=IP+1
      IER=1
      ITE=IP*IPP1/2
      IEND=ITE+IPP1
      TOL=ABS(EPS*WORK(1))
      TEST=ABS(ETA*WORK(IEND))
C
C          START LOOP OVER ALL ROWS OF WORK
      DO 11 I=1,IP
       IPIV=IPIV+I
        JA=IPIV-IRES
        JE=IPIV-1
C
C          FORM SCALAR PRODUCT NEEDED TO MODIFY CURRENT ROW ELEMENTS
        JK=IPIV
        DO 9 K=I,IPP1
        SUM=0.
        IF (IRES)5,5,3
    3   JK=JK-IRES
        DO 4 J=JA,JE
           SUM=SUM+WORK(J)*WORK(JK)
    4       JK=JK+1
    5    IF (JK-IPIV)6,6,8
C
C                                   TEST FOR LOSS OF SIGNIFICANCE
    6    SUM=WORK(IPIV)-SUM
         IF (SUM-TOL) 12,12,7
    7    SUM=SQRT(SUM)
         WORK(IPIV)=SUM
         PIV=1./SUM
         GOTO 9
C
C                                      UPDATE OFF-DIAGONAL TERMS
    8    SUM=(WORK(JK)-SUM)*PIV
         WORK(JK)=SUM
    9    JK=JK+K
C
C           UPDATE SQUARE SUM OF ERRORS
         WORK(IEND)=WORK(IEND)-SUM*SUM
C
C                            RECORD ADDRESS OF LAST PIVOT ELEMENT
         IRES=IRES+1
         IADR=IPIV
C
C                              TEST FOR TOLERABLE ERROR IF SPECIFIED
         IF (IOP) 10,11,11
   10    IF (WORK(IEND)-TEST) 13,13,11
   11    CONTINUE
      IF (IOP) 12,22,12
C
C                             PERFORM BACK SUBSTITUTION IF SPECIFIED
   12 IF (IOP) 14,23,14
   13 IER=0
   14 IPIV=IRES
   15 IF (IPIV)23,23,16
   16 SUM=0.
      JA=ITE+IPIV
      JJ=IADR
      JK=IADR
      K=IPIV
      DO 19 I=1,IPIV
        WORK(JK)=(WORK(JA)-SUM)/WORK(JJ)
        IF (K-1)20,20,17
   17   JE=JJ-1
        SUM=0.
        DO 18 J=K,IPIV
           SUM=SUM+WORK(JK)*WORK(JE)
           JK=JK+1
  18       JE=JE+J
         JK=JE-IPIV
         JA=JA-1
         JJ=JJ-K
   19    K=K-1
   20 IF (IOP/2)21,23,21
   21 IADR=IADR-IPIV
      IPIV=IPIV-1
      GOTO 15
C
C                                           NORMAL RETURN
   22 IER=0
   23 RETURN
      END


