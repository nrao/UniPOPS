      subroutine quichardvctr(ix1, iy1)
C-------------------------------------------------------------------------------
C  @(#)quichardvctr.f	4.2 11/24/92
C-------------------------------------------------------------------------------
c
c     Draws a line -- QMS (QUIC) code
c
      integer*2 ix1, iy1, iyo, ixo
      real*4 scale 
c
      include 'cio.inc'
      data scale /9.5/
c
      iyo = (840 - iy1) * scale
      ixo = ix1 * scale
      write(ioutgrph, 1000) ixo, iyo
      return
c
 1000 format('^D',i5.5,':'i5.5)
c
      end
c
