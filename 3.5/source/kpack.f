      SUBROUTINE KPACK (N,KB,KBP,KB1,KBP1)
C-------------------------------------------------------------------------------
C  @(#)kpack.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C	 KPACK compress	a loose	to tight character buffer.  The
C     operation	is defines as:	X\b + Y\b = X\Y.
c      Modified 8903 [RJM] See CHANGES.DOC
C-----------------------------------------------------------------------
      integer*2	KB(*),KB1(*), n, kbp, kbp1, iblank, i, j, it
      integer*2 n120, m3, n0
      DATA IBLANK /'  '/
      data n120, m3, n0 /120, -3, 0/
c
      if (kbp .le. 0 .or. kbp1 .le. 0) call oerror(n120,m3,'KPACK')
c
      DO 10 I=1,N
         J = MOD(I-1,2)
         IF (J.EQ.0) KB1(KBP1) = IBLANK
	 CALL GTBYTE (IT,KB(KBP),n0)
	 CALL PTBYTE (IT,KB1(KBP1),J)
         KBP  = KBP  + 1
         KBP1 = KBP1 + J
 10	 CONTINUE
      IF (J.EQ.0) KBP1 = KBP1 + 1
      RETURN
      END
