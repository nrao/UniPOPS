      subroutine corebox(ix0, iy0, ix1, iy1)
c
c     @(#)corebox.f	5.1 06/22/94
c
c     Draws a filled rectangle -- SUNCORE
c
      integer*2 ix0, iy0, ix1, iy1, ierr
      integer*4 l0, l15
      character*80 string
c
      data l0, l15 /0, 15/
c
      write(string,10,iostat=ierr) ix0, ix1, ix1, ix0, iy0, iy0,
     .				   iy1, iy1
10    format(8i10)
c
      call writeshm2(l15, l0, l0, string)
c
      return
      end
c
