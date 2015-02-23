      subroutine posthardplace(ix1, iy1)
C-------------------------------------------------------------------------------
C  @(#)posthplace.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Places cursor for next VCTR or PCHAR -- POSTCRIPT code
c*******************************
c 8904 [RJM]
c*******************************
c
      integer*2 ix1, iy1
      integer*2 n15
      include 'cio.inc'
c
      data n15 /15/
c
      character*40 stch
c
      write(stch,10) ix1, iy1
10    format( 2i6, ' m ')
      call putchar(stch,n15,ioutgrph)
      return
c
      end
c
