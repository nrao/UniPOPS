      INTEGER*2 FUNCTION IWPR (IP)
C-------------------------------------------------------------------------------
C  @(#)iwpr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C	 IWPR calculates the number of integer words required to
C     hold IP number of	real words.
C----------------------------------------------------------------------
      integer*2 nrpi, ip
c
      DATA NRPI/2/
      IWPR = IP	* NRPI
      RETURN
      END
