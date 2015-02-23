      SUBROUTINE  MASSGN
C-------------------------------------------------------------------------------
C  @(#)massgn.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C Modified 890105 [PPM] includes to .inc, lowercase
c          8903   [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
      integer*4 maxtemp
      parameter (maxtemp = 100)
c		size of temp array
      integer*2 JTEMP(40), iblank, nvals, isize, iwpr, j, m, irealp
      integer*2 m1, n4, n106, n107, n120, n126, fshort
      REAL*8 TEMP(maxtemp)
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
c
      DATA IBLANK/' '/
c
      data m1, n4, n106, n107, n120, n126 /-1, 4, 106, 107, 120, 126/
C
C=======================================================================
C
      if (sp .gt. slim .or. sp-3 .lt. 0) call oerror(n107, m1, '=')
      NVALS = STACK (SP)
      ISIZE = NVALS
      IF (STACK(SP-1).EQ.2 .or. stack(sp-1) .eq. 3) ISIZE = ISIZE * 4
      SP = SP -	ISIZE -	1
      IF (STACK(SP - 3).EQ.7) GOTO 20
      IF (STACK(SP - 3).ne.2) call oerror(n126, m1, ' ')
      J	= 2 * IWPR(ISIZE)
      if (isize .gt. maxtemp) call oerror(n120, m1, 'MASSGN')
c
      CALL COPY	(J,V(SP+1),TEMP)
      TAG = STACK(SP - 1)
      ISIZE = STACK(SP - 2)
      IF (ISIZE.lt.NVALS) call oerror(n106, m1, '=')
      SP = SP -	4
      J	= 0
      CALL COPY	(fshort(NVALS+4),J,STACK(SP+1))
      TAG = TAG	- 1
      DO 10 J =	1, NVALS
	   TAG = TAG + 1
	   SP =	SP + 2
	   if (sp-1 .le. 0 .or. sp .gt. slim) call oerror(n107, m1, '=')
	   STACK(SP - 1) = TAG
	   V(SP) = TEMP(J)
	   CALL	ASSGN
   10	   CONTINUE
      IF (ISIZE.EQ.NVALS) GOTO 99
	 M = 0
	 ISIZE = ISIZE - NVALS
	 TAG = TAG + 1
	 GOTO 40
   20 CALL COPY(ISIZE,STACK(SP+1),JTEMP)
      TAG = STACK(SP-1)
      ISIZE = STACK(SP - 2)
      SP = SP -	4
      DO 30 J =	1, NVALS
	   SP =	SP + 8
	   if (sp-7 .le. 0 .or. sp .gt. slim) call oerror(n107, m1, '=')
	   STACK(SP - 7) = 7
	   STACK(SP - 6) = ISIZE
	   STACK(SP - 5) = TAG
	   STACK(SP - 4) = 3
	   CALL	COPY (n4,JTEMP(4*(J-1)+1),STACK(SP-3))
	   M = STACK(SP-2)
	   IF (STACK(SP-3).EQ.14) M = IREALP(M)
	   CALL	ASSGN
	   ISIZE = ISIZE - M
	   TAG = TAG + M
   30	   CONTINUE
      IF (ISIZE.EQ.0) GOTO 99
	 M = IBLANK
   40 ISIZE = IWPR (ISIZE)
      if (tag .le. 0) call oerror(n107, m1, '=')
      CALL FILL (ISIZE,M,C(TAG))
   99 CONTINUE
      RETURN
      END

