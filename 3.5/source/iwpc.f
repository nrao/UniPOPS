      INTEGER*2 FUNCTION IWPC (IP)
C-------------------------------------------------------------------------------
C  @(#)iwpc.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C	 IWPC calculates the number of integer words required to
C     hold IP number of	characters.
C----------------------------------------------------------------------
      integer*2 ncpi, ip
c
      DATA NCPI/2/
      IWPC = (IP-1)/NCPI + 1
      RETURN
      END
