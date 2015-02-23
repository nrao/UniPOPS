      subroutine posthardvctr(ix2, iy2)
C-------------------------------------------------------------------------------
C  @(#)posthvctr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Draws a line -- POSTSCRIPT code
c******************************
c 8904 [RJM]
c******************************
c
      integer*2 ix2, iy2
      integer*2 n15
      character*100 stch
c
      include 'cio.inc'
c
      data n15 /15/
c
      write(stch,11) ix2, iy2
11    format(2i6 ' v ')
      call putchar(stch,n15,ioutgrph)
      return
c
      end
c

