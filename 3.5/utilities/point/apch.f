      SUBROUTINE APCH(XDATA,YDATA,N,IP,XD,X0,WORK,IER)
C-------------------------------------------------------------------------------
C  @(#)apch.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c***********************************
c* Modified 8903 [RJM] See CHANGES.DOC
c***********************************
C
      integer*2 n, ip, ier, mt2, mt2m, i, m, j, k, ll, kk, jj, iend
      real*4 xd, x0, xa, xe, xm, sum, df, t
C                                   DIMENSIONED DUMMY VARIABLES
      real*4 XDATA(*), YDATA(*), WORK(*)
C
C                      CHECK FOR FORMAL ERRORS IN SPECIFIED DIMENSIONS
      IF(N-1)19,20,1
    1 IF(IP)19,19,2
C
C                                 SMALLEST AND LARGEST ARGUMENT
    2 IF(IP-N)3,3,19
    3 XA=XDATA(1)
      X0=XDATA(N)
      XE=0.
C
C                         INITIALIZE CALCULATION OF NORMAL EQUATIONS
      XD=X0-XA
      M=(IP*(IP+1))/2
      IEND=M+IP+1
      MT2=IP+IP
      MT2M=MT2-1
C
C                       SET WORKING STORAGE AND RIGHT HAND SIDE TO ZER
      DO 8 I=1,IP
         J=MT2-I
         WORK(J)=0.
         WORK(I)=0.
         K=M+I
    8    WORK(K)=0.
C
C                            CHECK FOR DEGENERATE ARGUMENT RANGE
      IF(XD)20,20,9
C
C                       CALCULATE CONSTANTS FOR REDUCTION OF ARGUMENTS
    9 X0=-(X0+XA)/XD
      XD=2./XD
      SUM=0.
C
C                            START GREAT LOOP OVER ALL GIVEN POINTS
      DO 15 I=1,N
         T=XDATA(I)*XD+X0
         DF=YDATA(I)
C
C                            CALCULATE AND STORE VALUES OF CHEBYSHEV
C                                       POLYNOMIALS FOR ARGUMENT T
         XA=1.
         XM=T 
C                                        WEIGHTING REMOVED
   11    T=T+T
         SUM=SUM+DF*DF
         DF=DF+DF
         J=1
   12    K=M+J
         WORK(K)=WORK(K)+DF*XA
   13    WORK(J)=WORK(J)+XA
         IF(J-MT2M)14,15,15
   14    J=J+1
         XE=T*XM-XA
         XA=XM
         XM=XE
         IF(J-IP)12,12,13
   15    CONTINUE
      WORK(IEND)=SUM+SUM
C
C                             CALCULATE MATRIX OF NORMAL EQUATIONS
      LL=M
      KK=MT2M
      JJ=1
      K=KK
      DO 18 J=1,M
         WORK(LL)=WORK(K)+WORK(JJ)
         LL=LL-1
         IF(K-JJ)16,16,17
   16    KK=KK-2
         K=KK
         JJ=1
         GO TO 18
   17    JJ=JJ+1
         K=K-1
   18    CONTINUE
      IER=0
      RETURN
C
C                           ERROR RETURN IN CASE OF FORMAL ERRORS
   19 IER=-1
      RETURN
C
C                         ERROR RETURN IN CASE OF COINCIDING ARGUMENTS
   20 IER=1
      RETURN
      END


