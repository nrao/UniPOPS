      subroutine pfixchar(ix, iy, string, nchar, iangle, ichsize)
C-------------------------------------------------------------------------------
C @(#)pfixchar.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c	puts the characters in string on the graphics screen starting
c	at the position ix, iy, and moving away at iangle putting each
c	character ichsize away from the last character.
c
c	iangle is with respect to the horizontal, angles increase going
c	counter clockwise.  Characters are NOT themselves rotated.
c	Hence:  angle = 0 goes left to right
c		angle = 90 goes up the page
c		angle = 180 goes right to left
c		angle = 270 goes down the page
c	negative angles are fine.
c	iangle is in degrees
c     
      integer*2 nchar, ix, iy, iangle, ichsize
      integer*2 icx, icy, i
      integer*2 n1
      real*4 pi, angle, x, y, dx, dy
      character*1 string(*), strtmp(2)
c
      data n1 /1/
c
      pi = 4.0 * atan(1.0)
c
      x = float(ix)
      y = float(iy)
      angle = float(iangle) * pi / 180.0
      dx = float(ichsize) * cos(angle)
      dy = float(ichsize) * sin(angle)
c
      do 100 i = 1, nchar
         icx = nint(x)
         icy = nint(y)
         call place(icx, icy)
         strtmp(1) = string(i)
         call pchar(strtmp(n1), n1)
         x = x + dx
         y = y + dy
 100  continue
c
      return
      end
c
