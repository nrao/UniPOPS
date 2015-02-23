      subroutine corevctr(ix, iy)
C-------------------------------------------------------------------------------
C  @(#)corevctr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Draws a line from the position last specified by
c     move to the position ix, iy -- uses graphics shared memory
c***************************************
c 8904 [RJM
c***************************************
c
      include 'cio.inc'
c
      integer*2 ix, iy
      integer*4 l2, lix, liy
      character*80 string
c
      data l2 /2/
c
      lix = ix
      liy = iy
      call writeshm2(l2, lix, liy, string)
c
      return
      end
c
