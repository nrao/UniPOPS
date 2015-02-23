      subroutine posthardcolor(icolor)
C-------------------------------------------------------------------------------
C  @(#)posthcolor.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Changes colors -- POSTCRIPT code
c
      integer*4 icolor
c
      real r, g, b
c
      include 'cio.inc'
c
      character*40 stch
      integer*2 n27
c
      data n27/27/
c
      r = min(1.,max(0.,float(and(rshift(icolor,16),255))/255.))
      g = min(1.,max(0.,float(and(rshift(icolor,8),255))/255.))
      b = min(1.,max(0.,float(and(icolor,255))/255.))
      write(stch,10) r, g, b
10    format( 3f8.5, ' c ')
      call putchar(stch,n27,ioutgrph)
      return
c
      end
c
