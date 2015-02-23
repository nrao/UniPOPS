      subroutine nogint
C-------------------------------------------------------------------------------
C  @(#)nogint.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Dummy routines for non-graphics screens
c
      integer*2 string(*), i, j, icursor, k, l
      integer*4 idx
c
      return
c
      entry nogvctr(i,j)
      return
c
      entry nogchar(string,i)
      return
c
      entry nogplace(i,j)
      return
c
      entry nogcursor(icursor,i,j)
      icursor = -1
      return
c
      entry nogclose
      return
c
      entry nogclrpage
      return
c
      entry nogcsize(i)
      return
c
      entry nogbox(i,j,k,l)
      return
c
      entry nogcolor(idx)
      return
c
      end
c
