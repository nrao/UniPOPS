      subroutine coreclrpage
C-------------------------------------------------------------------------------
C  @(#)coreclrpage.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Clears graphics screen by sending clear command to graphics Fifo
c*********************************************
c 8904 [RJM]
c*********************************************
c
      integer*4 long, irtn
c
      include 'cio.inc'
c
      call writefifo(long(igraphout),irtn, 'clear')
c
      return
      end
c
