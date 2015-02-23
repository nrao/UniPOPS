      subroutine initmess
C-------------------------------------------------------------------------------
C  @(#)initmess.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Echos startup mesages to the terminal
c****************************************
c 8904 [RJM]
c****************************************
c
      include 'cio.inc'
c
      integer*4 irtn, system
c
      irtn = system('mess.exe ' // program(2:2))
c
      return
      end
c
