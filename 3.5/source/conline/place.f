      subroutine place(i1, i2)
C-------------------------------------------------------------------------------
C  @(#)place.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Places the plotting pen for the next PCHAR or VCTR command
c
      integer*2 i1, i2
c
      include 'cio.inc'
      include 'foxplot.inc'
c
      xpos = i1
      ypos = i2
c
      if (igraphtype .eq. 4) then
	call v102place(i1, i2)
      else if (igraphtype .eq. 3) then
	call tekplace(i1, i2)
      else if (igraphtype .eq. 2) then
	call coreplace(i1, i2)
      else if (igraphtype .eq. 1) then
	call nogplace(i1, i2)
      endif
      if (iprinttype .eq. 5) then
        call quichardplace(i1, i2)
      else if (iprinttype .eq. 4) then
	call hphardplace(i1, i2)
      else if (iprinttype .eq. 3) then
	call posthardplace(i1, i2)
      else if (iprinttype .eq. 2) then
	call qmshardplace(i1, i2)
      else if (iprinttype .eq. 1) then
	call noghardplace(i1, i2)
      endif
      return
      end
c
