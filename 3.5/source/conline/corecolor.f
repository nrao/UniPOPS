      subroutine corecolor(icolor)
c
c     @(#)corecolor.f	5.1 06/22/94
c
c     Changes the color of the next piece of graphics -- SUNCORE
c
      integer*4 icolor
      integer*4 l4, l0
      character*80 string
c
      data l0, l4 /0, 4/
c
      call writeshm2(l4, icolor, l0, string)
c
      return
      end
c
