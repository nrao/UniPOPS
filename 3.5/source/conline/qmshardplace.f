      subroutine qmshardplace(ix1, iy1)
C-------------------------------------------------------------------------------
C  @(#)qmshardplace.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Places cursor for next VCTR or PCHAR -- QMS (TEK 4010) code
c*******************************
c 8904 [RJM]
c*******************************
c
      real xscale, yscale
      integer*2 iix, iiy
c			make all TEK-like devices 15/16th full size 15/16 = 0.9375
      parameter (yscale=0.9375)
      parameter (xscale=0.9375)
c
      integer*2 ix1, iy1
      integer*2 n1, n4
      character*4 cout
c
      include 'cio.inc'
c
      data n1, n4 /1, 4/
c
      iix = nint(ix1*xscale)
      iiy = nint(iy1*yscale)
      call packer(iix, iiy, cout)
      call putchar(char(29),n1,ioutgrph)
      call putchar(cout,n4,ioutgrph)
c
      return
c
      end
c
