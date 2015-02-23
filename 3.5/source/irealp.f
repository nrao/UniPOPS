      INTEGER*2 FUNCTION IREALP  (IP)
C-------------------------------------------------------------------------------
C  @(#)irealp.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C	 IREALP	calculates the index of	a real array corresponding to
C     an equivalenced integer array given the integer index (IP).
C----------------------------------------------------------------------
      integer*2 ip, nipr
c
      DATA NIPR/2/
      IREALP=(IP+1)/NIPR
      RETURN
      END
