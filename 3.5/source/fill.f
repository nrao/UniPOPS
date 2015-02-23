      SUBROUTINE FILL (B,C,A)
C-------------------------------------------------------------------------------
C  @(#)fill.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C------------------------------------------------------------------------
C	 FILL fills the	array A	with the constant C.  B	defines
C     the length of array A.
c     Modified 8903 [RJM] See CHANGES.DOC
C------------------------------------------------------------------------
      INTEGER*2 A(*), B, C
      integer*2 i
c
      DO 10 I =	1, B
	 A(I) =	C
 10	 CONTINUE
      RETURN
      END
