      subroutine tekchar(string, nchar)
C-------------------------------------------------------------------------------
C  @(#)tekchar.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Outputs a string in a Tektronix style
c***********************************************
c 8904  [RJM]
c***********************************************
c
      include 'cio.inc'
c
      integer*2 string(*), istch(50), nchar, nchar2, iwpc, lastblnk
      integer*2 n1, n80
      character*100 stch
c
      data n1, n80 /1, 80/
c
      equivalence (stch, istch)
c
      stch = ' '
      nchar2 = min(nchar,n80)
      call copy(iwpc(nchar2), string, istch)
      nchar2 = min(lastblnk(stch),nchar2)
c
      call putchar(char(31),n1,istdout)
      if (nchar2 .ne. 0) call putchar(stch(1:nchar2), nchar2, istdout)
c
      return
      end
c
