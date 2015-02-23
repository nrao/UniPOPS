      subroutine v102cursor(icode, ix, iy)
C-------------------------------------------------------------------------------
C  @(#)v102cursor.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Returns cursor position in TEKTRONIX style but for smaller V102 -
c	retrographics screen
c*************************************************************************
c 8904 [RJM]
c*************************************************************************
      integer*2 icode, ix1, iy1, ix, iy
      real*4 scale
c
      data scale /0.7/
c     Ratio of v102 screen to full TEK 4010 screen
c
      call tekcursor(icode, ix1, iy1)
c
      ix = int(float(ix1)/scale)
      iy = int(float(iy1)/scale)
c
      return
      end
c

