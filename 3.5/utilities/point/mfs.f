       SUBROUTINE MFS(A,N,EPS)
c
c      @(#)mfs.f	5.2 05/14/96
c
C *******************************************************************/
C *                                                                 */
C *         FACTORIZE SYMMETRIC POSITIVE DEFINITE MATRIX            */
C *                                                                 */
C *******************************************************************/
       integer*2 IND, IB,K,KL,L,N
       REAL EPS,A(*)
       DOUBLE PRECISION    SUM
c
       IND =0
       IB  =1
       DO 100 K=1 ,  N
          KL  =0
10        SUM =0
          IF (IB.GT.IND) GO TO 25
          DO 20 L=IB , IND
             KL  =KL+1
20           SUM =SUM+(A(L)*A(KL))
 25       KL =KL+1
          IND=IND+1
          SUM=A(IND)-SUM
          IF(IND.LE.KL) GO TO 31
          A(IND)=SUM/A(KL)
          GO TO 10
31        IF (SUM.GT.0) A(IND)=DSQRT(SUM)
          IF (SUM.LE.0) GO TO 101
          IB   =IB+K
100       CONTINUE
       RETURN
101    N=K-1
       RETURN
       END
