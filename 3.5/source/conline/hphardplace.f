      subroutine hphardplace(ix1, iy1)
C-------------------------------------------------------------------------------
C  @(#)hphardplace.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Places cursor for next VCTR or PCHAR -- HPGL code
c*******************************
c 8904 [RJM]
c*******************************
c
      integer*2 ix1, ix2, ix3, iy1, iy2, iy3
      integer*2 n19, n39
      include 'cio.inc'
c
      character*40 stch
c
      data n19, n39 /19, 39/
c
      save ix3, iy3
c
	write(stch,10) 'PU;PA ', ix1, iy1, ';'
10	format( a, 2i6, a)
	call putchar(stch,n19,ioutgrph)
	ix3 = ix1
	iy3 = iy1
	return
c
      entry hphardvctr(ix2, iy2)
c
c     Draws a line -- HPGL code
c
	write(stch,11) 'PU;PA ',ix3, iy3, '; PD;PA ',ix2, iy2, ';'
11	format( a, 2i6, a, 2i6, a)
	call putchar(stch,n39,ioutgrph)
	ix3 = ix2
	iy3 = iy2
	return
c
      end
c
