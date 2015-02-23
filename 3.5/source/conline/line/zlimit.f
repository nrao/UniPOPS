      subroutine zlimit (z, isize, jsize, zmin, ixmin, iymin, zmax, 
     .			 ixmax, iymax, undefwp)
c
c     @(#)zlimit.f	5.1 06/22/94
c
c     To find the maximum and minimum in the Z array.  I = IMIN to
c     IMAX, J = JMIN to JMAX (set in COMMON/MAPPL/).
c
c     z (isize, jsize) = (R*4) array for which max-min is desired
c     isize = (I*4) first dimension limit of Z
c     jsize = (I*4) second dimension limit of Z
c     zmin = (R*4) Minimum value in Z-array
c     zmax = (R*4) Maximum value in Z-array
c     ixmin, iymin, ixmax, iymax = (I*2) Pixels where Max/Min are located.
c     undefwp = (R*4) undefined value
c
c     Note: zmin and zmax are output, all others are input only.
c	    Undefined points will be ignored.
c
c     Caution: IMIN, IMAX, JMIN, JMAX must be set in COMMON/APPL/
c
c
      real*4 z(isize, jsize), zmin, zmax, undefwp
      integer*4 isize, jsize
      integer*2 n254, m2, ixmin, ixmax, iymin, iymax
c
      include 'mappl.inc'
c
      integer*2 i, j
c
      data m2, n254 /-2, 254/
c
      if (imin .gt. imax .or. imin .le. 0 .or. imax .gt. isize)
     .		call oerror(n254, m2, 'MLIMIT')
      if (jmin .gt. jmax .or. jmin .le. 0 .or. jmax .gt. jsize)
     .		call oerror(n254, m2, 'MLIMIT')
c     Checks that IMIN, IMAX, JMIN, and JMAX are defined correctly.
c
      zmin = z(imin,jmin)
      zmax = zmin
      ixmax = imin
      ixmin = imin
      iymax = jmin
      iymin = jmin
c
      do 200 i = imin, imax
	   do 100 j = jmin, jmax
c
		if (z(i,j) .ne. undefwp) then
                    if (zmin .eq. undefwp) then
                       zmin = z(i,j)
                       zmax = z(i,j)
                       ixmin = i
                       ixmax = i
                       iymin = j
                       iymax = j
		    else 
                       if (z(i,j) .lt. zmin) then
			   zmin = z(i,j)
			   ixmin = i
			   iymin = j
		       endif
		       if (z(i,j) .gt. zmax) then
			   zmax = z(i,j)
			   ixmax = i
			   iymax = j
                       endif
		    endif
		endif
c
100		continue
200	   continue
c
      return
c
c    
      end
c
c
