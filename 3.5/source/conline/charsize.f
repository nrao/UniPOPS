      subroutine charsize(csize1)
c
c     @(#)charsize.f	5.1 06/22/94
c
c     To specify character size to be output as CSIZE basic character size
c     (1 = default)
c
c     csize1 = (R*4) Specifies character size factor
c
      real*4 csize1
c
      include 'foxplot.inc'
c
      csize = csize1
c     Change SIZE into number of 'points' (CSIZE)
c
      return
      end
c
