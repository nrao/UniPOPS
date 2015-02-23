      subroutine v102vctr(ix, iy)
C-------------------------------------------------------------------------------
C  @(#)v102vctr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Draws a line from the position last specified by
c     move to the position ix, iy -- TEKTRONIX 4010 style but for smaller
c     v102 retrographics screen
c***************************************
c 8904 [RJM
c***************************************
      integer*2 ix1, iy1, ix, iy
      real*4 scale
c
      data scale/0.7/
c     Ratio between v102 and tek 4010 screens
c
      ix1 = ix*scale
      iy1 = iy*scale
      call tekvctr(ix1, iy1)
c
      return
      end

