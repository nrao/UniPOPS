      subroutine qmshardvctr(ix2, iy2)
C-------------------------------------------------------------------------------
C  @(#)qmshardvctr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Draws a line -- QMS (TEK 4010) code
c******************************
c 8904 [RJM]
c******************************
c
      real xscale, yscale
      integer*2 iix, iiy
c			make all TEK-like devices 15/16th full size 15/16 = 0.9375
      parameter (yscale=0.9375)
      parameter (xscale=0.9375)
c
      integer*2 ix2, iy2
      integer*2 n4
      character*4 cout
c
      include 'cio.inc'
c
      data n4 /4/
c
      iix = nint(ix2*xscale)
      iiy = nint(iy2*yscale)
      call packer(iix, iiy, cout)
      call putchar(cout,n4,ioutgrph)
      return
c
      end
c
