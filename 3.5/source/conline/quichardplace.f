      subroutine quichardplace(ix1, iy1)
C-------------------------------------------------------------------------------
C  @(#)quichardplace.f	4.2 11/24/92
C-------------------------------------------------------------------------------
c
c     Places cursor for next VCTR or PCHAR -- QMS (QUIC) code
c
      integer*2 ix1, iy1, ixo, iyo
      real*4 scale
      data scale /9.5/
c
      include 'cio.inc'
c
      ixo = ix1 * scale
      iyo = (840 - iy1) * scale
      write(ioutgrph, 1000) ixo, iyo
c
      return
c
 1000 format('^U',i5.5,':',i5.5)
c
      end
c
