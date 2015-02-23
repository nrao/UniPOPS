      subroutine cursor(i1, i2, i3)
C-------------------------------------------------------------------------------
C  @(#)cursor.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Displays graphics cursor
c***************************************
c 8904 [RJM
c***************************************
c
      integer*2 i1, i2, i3
      include 'cio.inc'
c
      if (igraphtype .eq. 4) then
	call v102cursor(i1, i2, i3)
      else if (igraphtype .eq. 3) then
	call tekcursor(i1, i2, i3)
      else if (igraphtype .eq. 2) then
	call corecursor(i1, i2, i3)
      else 
	call nogcursor(i1, i2, i3)
      endif
      return
      end
c
