      subroutine coredither(ix, iy, nx, ny, percent)
c
c     @(#)coredither.f	5.1 06/22/94
c
c     Produces a half-tone box at the coordinates (IX,IY) with size (NX,NY)
c	and that is PERCENT filled -- SUNCORE
c
      integer*2 ix, iy, nx, ny
      real percent
c
      integer*2 ierr
      integer*4 l16, lix, liy
      character*80 string
c
      data l16 /16/
c
      write(string,10,iostat=ierr) nx, ny, percent
10    format(i5,i5,f10.7)
c     Box size and percent gray are stored in STRING to be passed to window
c	process.
c
      lix = ix
      liy = iy
      call writeshm2(l16, lix, liy, string)
c
      return
      end
c
