      SUBROUTINE move2 (AMOUNT,FROM,FP,TO,TP)
C-------------------------------------------------------------------------------
C  @(#)move2.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c*******************************************
c Modified 8903 [RJM] See CHANGES.DOC
c*******************************************
C-----------------------------------------------------------------------
C        MOVE is a general purpose array moving routine.
C     AMOUNT  =  Number of i*2 words to be moved.
C     FROM    =  Source array.
C     FP      =  Index of first element in FROM.
C     TO      =  Receiving array.
C     TP      =  Index of first element in TO.
C-----------------------------------------------------------------------
      integer*2 FROM(*), TO(*)
      integer*2 AMOUNT, FP, TP, i, iwpc
      DO 10 I = 1,iwpc(AMOUNT)
         TO(I-1+iwpc(TP)) = FROM(I-1+iwpc(FP))
 10      CONTINUE
      RETURN
      END


