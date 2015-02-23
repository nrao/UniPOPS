      subroutine tekvctr(ix,iy)
C-------------------------------------------------------------------------------
C  @(#)tekvctr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Draws a line from the position last specified by
c     move to the position ix, iy -- TEKTRONIX 4010
c***************************************
c 8904 [RJM
c***************************************
c
      real xscale, yscale
      integer*2 iix, iiy
c			make all TEK-like devices 15/16th full size 15/16 = 0.9375
      parameter (yscale=0.9375)
      parameter (xscale=0.9375)
c
      integer*2 ix, iy
      integer*2 n4
      include 'cio.inc'
c
      character*4 cout
c
      data n4 /4/
c
      iix = nint(ix*xscale)
      iiy = nint(iy*yscale)
      call packer(iix,iiy,cout)
      call putchar(cout, n4, istdout)
c
      return
c
      end
c
