      subroutine dither(ix, iy, nx, ny, percent)
c
c     @(#)dither.f	5.1 06/22/94
c
c     Draws a half-tone pattern of size (NX,NY) with corner (IX,IY) that is
c	PERCENT filled.
c
      include 'cio.inc'
c
      integer*2 ix, iy, nx, ny
      real percent
c
      integer*2 nxold, nyold, ixdt(16384), iydt(16384), ixdt4, iydt4,
     .		matset(16384), i, ii3
      integer*4 levmax, l16384, l0, lnx, lny
      real rand
c
      save nxold, nyold
c
      data nxold/0/, nyold/0/
      data l16384, l0 /16384, 0/
c
      if (igraphtype .eq. 2) call coredither(ix,iy,nx,ny,percent)
c
      if (igraphtype .le. 2 .and. iprinttype .le. 1) return
c     If SUNCORE or NO printer or graphics, then do nothing else.
c
      if (nx .ne. nxold .or. ny .ne. nyold) then
c	If a previous box was filled with the same size, then use the
c	old matrices; else fill in a new one
c 
         lnx = nx
         lny = ny
         levmax = lnx * lny
         levmax = min(l16384,levmax)
c	 LEVMAX = number of pixels in a cell
c
         do 100 i = 1, levmax
	   matset(i) = 0
100	   continue
c          Clear out set matrix
c
         do 130 i = 1, levmax
120	   ii3 = nint(rand(l0)*float(levmax-1)+1.0)
	   if (matset(ii3) .eq. 1) goto 120
	   matset(ii3) = 1
           ixdt(i) = ifix(ii3/ny) + 1
	   iydt(i) = ii3 - (ixdt(i)-1)*ny
130	   continue
      endif
c     Fill in pixel arrays and set matrix.
c
      ii3 = nint(rand(l0)*float(levmax-1)+1.0)
      do 310 i = 1, nint(percent*levmax)
		ii3 = ii3 + 1
		if (ii3 .gt. levmax) ii3 = 1
		ixdt4 = ix + ixdt(ii3) - 1
		iydt4 = iy + iydt(ii3) - 1
      	        if (igraphtype .eq. 4) then
			call v102place(ixdt4,iydt4)
			call v102vctr(ixdt4,iydt4)
      	        else if (igraphtype .eq. 3) then
			call tekplace(ixdt4, iydt4)
			call tekvctr(ixdt4, iydt4)
		endif
      		if (iprinttype .eq. 5) then
        		call quichardplace(ixdt4, iydt4)
        		call quichardvctr(ixdt4, iydt4)
      		else if (iprinttype .eq. 4) then
			call hphardplace(ixdt4, iydt4)
			call hphardvctr(ixdt4, iydt4)
      		else if (iprinttype .eq. 3) then
			call posthardplace(ixdt4, iydt4)
			call posthardvctr(ixdt4, iydt4)
      		else if (iprinttype .eq. 2) then
			call qmshardplace(ixdt4, iydt4)
			call qmshardvctr(ixdt4, iydt4)
		endif
310		continue
c		Draws the correct number of pixels in the box defined by
c		the above set limits.
c
      nxold = nx
      nyold = ny
c     You don't want to redo the matrices the next time through for the
c	same size box so store away box size.
c
      return
      end
c
