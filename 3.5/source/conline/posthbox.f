      subroutine posthardbox(ix0, iy0, ix1, iy1)
C-------------------------------------------------------------------------------
C  @(#)posthbox.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Draws a box -- POSTCRIPT code
c
      integer*2 ix0, iy0, ix1, iy1
c
      include 'cio.inc'
c
      character*80 stch
      integer*2 n52
c
      data n52/52/
c
      write(stch,10) ix0, iy0, ix0, iy1, ix1, iy1, ix1, iy0
10    format(8i6, ' bx ')
      call putchar(stch,n52,ioutgrph)
c
      return
c
      end
c
