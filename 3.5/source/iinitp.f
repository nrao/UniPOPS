      INTEGER*2 FUNCTION IINITP	(IP)
C-------------------------------------------------------------------------------
C  @(#)iinitp.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C------------------------------------------------------------------------
C	 IINITP	calculates the index of	a integer array	element	cor-
C     responding to an equivalenced real array given the real index
C     (IP).
C------------------------------------------------------------------------
      integer*2 ip
c
      IINITP=IP*2-1
      RETURN
      END
