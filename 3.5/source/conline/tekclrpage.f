      subroutine tekclrpage
C-------------------------------------------------------------------------------
C  @(#)tekclrpage.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Clears TEKTRONIX graphics screen
c*********************************************
c 8904 [RJM]
c*********************************************
c
      include 'cio.inc' 
c
      integer*2 ipage
      integer*2 n0, n1, n2, n750
      character*2 cpage
c
      equivalence (cpage, ipage)
      data ipage/6924/
c
      data n0, n1, n2, n750 / 0, 1, 2, 750/
c
      call tekchar(cpage,n2)
      call place(n0,n750)
      call putchar(char(31),n1,istdout)
c
      return
      end
c
c*******************
c
