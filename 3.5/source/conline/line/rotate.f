      subroutine rotate (z, z2, isize, jsize, invertxy, flipx, flipy)
c
c     @(#)rotate.f	5.1 06/22/94
c
c     Rotates and flips the matrix Z and places the results into
c     matrix Z2.
c
c     z (isize, jsize) = (R*4) Input Array.
c     z2 (isize, jsize) = (R*4) Output array
c     isize = (I*4) First dimension limit of z
c     jsize = (I*4) Second dimension limit of z
c     flipx = (L) if TRUE, Flip X-axis
c     flipy = (L) if TRUE, Flip Y-axis
c     invertxy = (L) if TRUE, exchange X and Y axis
c
c     NOTE:  Flipping the x and y axis is performed before the exchanging of
c	     X and Y axis.
c
      integer*4 isize, jsize
      logical flipx, flipy, invertxy
      real z(isize,jsize), z2(*)
c
      include 'mappl.inc'
c
      integer*4 i, j, inv, ninv, k, l
c
      inv(k,l) = l + jsize*(k-1)
      ninv(k,l) = k + isize*(l-1)
c
      if (invertxy) then
c
	if (flipx .and. .not. flipy) then
	  do 120 i = 1, isize
	    do 110 j = 1, jsize
		z2( inv(i,j) ) = z(i,jsize-j+1)
110	        continue
120	    continue
	else if (flipy .and. .not. flipx) then
	  do 140 i = 1, isize
	    do 130 j = 1, jsize
		z2( inv(i,j) ) = z(isize-i+1,j)
130	        continue
140	    continue
        else if (.not. flipx .and. .not. flipy) then
	  do 160 i = 1, isize
	    do 150 j = 1, jsize
		z2( inv(i,j) ) = z(i,j)
150	        continue
160	    continue
        else
c	  Flipping x and y
	  do 180 i = 1, isize
	    do 170 j = 1, jsize
		z2( inv(i,j) ) = z(isize-i+1,jsize-j+1)
170	        continue
180	    continue
	endif
c
      else
c
	if (flipx .and. .not. flipy) then
	  do 20 i = 1, isize
	    do 10 j = 1, jsize
		z2( ninv(i,j) ) = z(isize-i+1,j)
10	        continue
20	    continue
	else if (flipy .and. .not. flipx) then
	  do 40 i = 1, isize
	    do 30 j = 1, jsize
		z2( ninv(i,j) ) = z(i,jsize-j+1)
30	        continue
40	    continue
        else if (.not. flipx .and. .not. flipy) then
	  do 60 i = 1, isize
	    do 50 j = 1, jsize
		z2( ninv(i,j) ) = z(i,j)
50	        continue
60	    continue
        else
c	  Flipping x and y
	  do 80 i = 1, isize
	    do 70 j = 1, jsize
		z2( ninv(i,j) ) = z(isize-i+1,jsize-j+1)
70	        continue
80	    continue
	endif
c
      endif  
c     Copy data to Z2; invert x/y if required.
c
      return
      end
c
c
