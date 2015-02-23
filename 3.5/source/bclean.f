      SUBROUTINE BCLEAN
C-------------------------------------------------------------------------------
C  @(#)bclean.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C--------------------------------------------------------------------
C    B-STACK CLEAN ROUTINE
C	 POPS ITEMS FROM B-STACK ONTO A-STACK UNTIL THE	PRECEDENCE IN
C    BPR-STACK IS LESS THAN THE	PARAMETER NEXTP.   USED	IN GENERATING
C    POLISH POSTFIX STRING OF OPCODES AND OPERANDS.
C    Changed 890105 [PPM] includes to .inc and lowercase
c            8903   [RJM] see CHANGES.DOC
C--------------------------------------------------------------------
      INCLUDE 'smstuf.inc'
      include 'stk.inc'
      integer*2 m
c
10    IF (BP.LE.0) GO TO 99
	 M=B(BP)
	 IF (BPR(BP).LT.NEXTP) GO TO 99
	 CALL PUSH(A,AP,M)
	 BP=BP-1
	 GO TO 10
 99   RETURN
      END
