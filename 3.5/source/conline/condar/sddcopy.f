      subroutine sddcopy (tmpbuf, twhbuf, isize, irtn)
c----------------------------------------------------------------------
c @(#)sddcopy.f	5.1 06/22/94
c
c    just copies tmpbuf to twhbuf
c    isize = size in bytes
c
c-----------------------------------------------------------------------
      real*8 tmpbuf(*), twhbuf(*)
      integer*4 irtn, isize, i, nelem
c
c
      nelem = isize / 8
      if (nelem*8 .lt. isize) nelem = nelem + 1
      do 100 i = 1, nelem
         twhbuf(i) = tmpbuf(i)
 100  continue
c
      irtn = 0
      return
      end
