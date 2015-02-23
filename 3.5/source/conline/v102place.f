      subroutine v102place(ix, iy)
C-------------------------------------------------------------------------------
C  @(#)v102place.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Places cursor for next VCTR or PCHAR call -- Tektronix 4010 style but
c	for smaller v102 - retrographics screen
c****************************************************
c 8904 [RJM]
c****************************************************
c
      integer*2 ix1, iy1, ix, iy
      real*4 scale
c
      data scale /0.7/
c     Ratio between v102 and tek 4010 screen
c
      ix1 = ix*scale
      iy1 = iy*scale
      call tekplace(ix1, iy1)
c
      return
      end
c
