      SUBROUTINE MIS(A,N)
c
c     @(#)mis.f	5.1 06/22/94
c
C *******************************************************************/
C *                                                                 */
C *          INVERT SYMMETRIC POSITIVE DEFINITE MATRIX              */
C *                                                                 */
C *******************************************************************/
       integer*2 ICOL,IPIV,IROW,J,K,L,LN,M,N, ik
       REAL A(*),PIV
       DOUBLE PRECISION SUM
c
       LN =N
       J  =0
       DO  10 IK =1,LN
          K=IK-1
          IPIV =0
          J    =J+1
          PIV  =A(J+K)
          IF (PIV.NE.0) GO TO 11
c***          ERROR=S
          GO TO 99
11        PIV=1/PIV
          A(J+K)=PIV
          IF (K.EQ.0) GO TO 10
          DO 12  L =1 , K
             SUM  =0
             IROW =J
             IPIV=IPIV+L
             ICOL=IPIV
             DO 13  M =L , K
                SUM  =SUM+(A(IROW)*A(ICOL))
                ICOL =ICOL+M
13              IROW =IROW+1
             A(J) =-SUM*PIV
12           J    =J+1
10        CONTINUE
       J =0
       DO 20  K =1 , LN
          IROW =K
          DO 21 L =1 , K
             SUM  =0
             J=J+1
             ICOL=J
             IROW =IROW-1
             DO 22 M =K , LN
                SUM  =SUM+(A(ICOL)*A(ICOL+IROW))
22              ICOL =ICOL+M
21           A(J) =SUM
20        CONTINUE
99       CONTINUE
         RETURN
         END
