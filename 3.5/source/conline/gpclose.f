      subroutine gpclose
C-------------------------------------------------------------------------------
C  @(#)gpclose.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Closes down graphics screens and text screens
c***************************************
c 8904 [RJM
c***************************************
c
      include 'cio.inc'
c
      if (igraphtype .eq. 4) then
	call v102close
      else if (igraphtype .eq. 3) then
	call tekclose
      else if (igraphtype .eq. 2) then
	call coreclose
      else 
	call nogclose
      endif
c
      return
      end
c
