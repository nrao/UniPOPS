      subroutine vctr(i3,i4)
C-------------------------------------------------------------------------------
C  @(#)vctr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Draws a line on the graphics screen
c
      integer*2 i3, i4
c
      include 'cio.inc'
      include 'foxplot.inc'
c
      xpos = i3
      ypos = i4
c
      if (igraphtype .eq. 4) then
	call v102vctr(i3,i4)
      else if (igraphtype .eq. 3) then
	call tekvctr(i3, i4)
      else if (igraphtype .eq. 2) then
	call corevctr(i3, i4)
      else if (igraphtype .eq. 1) then
	call nogvctr(i3, i4)
      endif
      if (iprinttype .eq. 5) then
        call quichardvctr(i3, i4)
      else if (iprinttype .eq. 4) then
	call hphardvctr(i3, i4)
      else if (iprinttype .eq. 3) then
	call posthardvctr(i3, i4)
      else if (iprinttype .eq. 2) then
	call qmshardvctr(i3, i4)
      else if (iprinttype .eq. 1) then
	call noghardvctr(i3, i4)
      endif
      return
      end
c
