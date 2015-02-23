      subroutine box(ix0, iy0, ix1, iy1)
c
c     @(#)box.f	5.1 06/22/94
c
c     Draws a box with corners (IX0,IY0) and (IX1,IY1)
c
      include 'cio.inc'
c
      integer*2 ix0, iy0, ix1, iy1
c
      if (igraphtype .gt. 2) then
	call otherbox(ix0,iy0,ix1,iy1)
      else if (igraphtype .eq. 2) then
	call corebox(ix0,iy0,ix1,iy1)
      else if (igraphtype .eq. 1) then
	call nogbox(ix0,iy0,ix1,iy1)
      endif
      if (iprinttype .eq. 1) then
	call noghardbox(ix0,iy0,ix1,iy1)
      else if (iprinttype .eq. 3) then
        call posthardbox(ix0,iy0,ix1,iy1)
      else if (iprinttype .gt. 1) then
        call otherbox(ix0,iy0,ix1,iy1)
      endif
c
      return
      end
c
