      subroutine mtable (z, isize, jsize)
c
c     @(#)mtable.f	5.1 06/22/94
c
c     Print the Z matrix in a readable form which is arranged so
c     that the x and y directions correspond to the contour plot.  
c     Ten columns are printed per page from IMIN to IMAX and each
c     column is printed from JMAX to JMIN.  Above the first column
c     on each page is printed the column numbers.  The output is
c     written via PWRITE.
c
c     Z(isize,jsize) = (R*4) Array to be listed
c     ISIZE,JSIZE  = I*2 dimensions of Z
c
c     CAUTION:  IMIN, IMAX, JMIN, and JMAX must be set in 
c		COMMON/ MAPPL/.
c
c
      real z(isize,jsize)
      integer*4 isize, jsize
c
      include 'mappl.inc'
c
      integer*2 m2, n80, n254
      integer*2 npages, n1, n2, np, j, j1, istch(40), ierr, i
      character*80 stch
c
      equivalence(istch, stch)
c
      data m2, n80, n254 /-2, 80, 254/
c
      if (imin .gt. imax .or. imin .le. 0 .or. imax .gt. isize)
     .		call oerror(n254, m2, 'MTABLE')
      if (jmin .gt. jmax .or. jmin .le. 0 .or. jmax .gt. jsize)
     .		call oerror(n254, m2, 'MTABLE')
c     Check IMIN, IMAX, JMIN, JMAX
c
      npages = 1 + iabs(imax - imin + 1) / 6
      if (mod (imax - imin + 1, 6) .eq. 0) npages = npages - 1
c     NPAGES = number of pages along x axis
c
      n1 = imin
      n2 = min (n1 + 5, imax)
c
      do 100 np = 1, npages
        j1 = 0
        do 50 j = jmax, jmin, -1
c
	   if (mod(j1,35) .eq. 0) then
	   	write(stch,4,iostat=ierr)
4	   	format(80('-'))
	   	call pwrite(istch, n80)
c
		write(stch,5,iostat=ierr) (i,i=n1,n2)
5          	format ('  J', 6(i6,6x))
	   	call pwrite(istch, n80)
c
	   	write(stch,4,iostat=ierr)
	  	call pwrite(istch, n80)
   	   endif
c	   Write out header lines every 35 output lines
c
           write(stch, 10, iostat=ierr) j, (z(i,j), i = n1, n2)
10	   format (1x, i5, 1p6g12.4)
	   call pwrite(istch, n80)
c	   Write out data values
c
           j1 = j1 + 1
50	   continue
c
	n1 = n1 + 6
        n2 = min (n1 + 5, imax)
        if (imin .gt. imax) n2 = max (n1 - 5, imax)
c
100	continue
c
      write (stch, 105)
105   format (' ')
      call pwrite(istch, n80)
c
      return
c
      end
c
c
