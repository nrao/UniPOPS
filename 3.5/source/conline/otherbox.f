      subroutine otherbox(ix0, iy0, ix1, iy1)
c
c     @(#)otherbox.f	5.2 09/23/94
c
c     Draws a box with corners (IX0,IY0), and (IX1,IY); mimics color
c	for non-color devices.
c
c     Color is mimiced by drawing a set of lines in the box that depend
c	in a random way on the chosen color.  The same color will always
c	give the same pattern of lines.
c
c     The pattern is determined by:
c	NSTEP -- distance between lines in x and y (1 - 4)
c	ANGLE1 -- angle with respect to horizontal at which the first set
c		of lines will be drawn (0, 45, 90, or 135).
c	NANGLE -- how many different set of lines at various angles are
c		to be drawn.  Angles are seperated by 45 degrees (1 - 4).
c
      integer*2 ix0, iy0, ix1, iy1, ixstart, ixend, iystart, iyend, i,
     .		nx, ny, j, nangle, nstep, angle1, angle
      integer*4 idx
      real cred, cblue, cgreen, cfill
c
      save nangle, cfill, angle1
c
      data nangle/1/, nstep/4/, angle1/0/
c
      nx = ix1 - ix0 + 1
      ny = iy1 - iy0 + 1
      nstep = 2 + nint(3.*(1.-cfill))
c
      do 20 i = 1, nangle
c
	  angle = mod(angle1 + 45*(i-1), 180)
c
	  if (angle .eq. 0) then
	    do 8 j = iy0+nstep, iy1, nstep
	      ixstart = ix0
	      iystart = j
	      ixend = ix1
	      iyend = j
	      call otherdraw(ixstart, iystart, ixend, iyend)
8	      continue
c
	  else if (angle .eq. 90) then
	    do 9 j = ix0+nstep, ix1, nstep
	      ixstart = j
	      iystart = iy0
	      ixend = j
	      iyend = iy1
	      call otherdraw(ixstart, iystart, ixend, iyend)
9	      continue
c
	  else if (angle .eq. 45) then
	    do 10 j = ix0, ix1, nstep
	      ixstart = j
	      iystart = iy0
	      ixend = min(ix1, j+min(nx,ny)-1)
	      iyend = iy0 + ixend - ixstart
	      call otherdraw(ixstart, iystart, ixend, iyend)
10	      continue
	    do 11 j = iy0+nstep, iy1, nstep
	      ixstart = ix0
	      iystart = j
	      iyend = min(iy1, j+min(nx,ny)-1)
	      ixend = ix0 + iyend - iystart
	      call otherdraw(ixstart, iystart, ixend, iyend)
11	      continue
c
	  else if (angle .eq. 135) then
	    do 12 j = ix1, ix0, -nstep
	      ixstart = j
	      iystart = iy0
	      ixend = max(ix0, j-min(nx,ny)+1)
	      iyend = iy0 + ixstart - ixend
	      call otherdraw(ixstart, iystart, ixend, iyend)
12	      continue
	    do 13 j = iy0+nstep, iy1, nstep
	      ixstart = ix1
	      iystart = j
	      iyend = min(iy1, j+min(nx,ny)-1)
	      ixend = ix1 - (iyend - iystart)
	      call otherdraw(ixstart, iystart, ixend, iyend)
13	      continue
	  endif
20	  continue
c
      return
c
      entry othercolor(idx)
c
c     Mimics color by drawing random lines; uses IDX as a seed for the
c	random number generator to calculate NSTEP, ANGLE1, NANGLE;
c
      cred = and(rshift(idx,16),255)/255.
      cgreen = and(rshift(idx,8),255)/255.
      cblue = and(idx,255)/255.
c     Calculate RGB values from IDX -- range 0.0 to 1.
c
      cfill = cred*cgreen*cblue
c	CFILL will be the percentage to fill in the box
c
      angle1 = 45*nint(3.*abs(cred-cgreen))
c	ANGLE1 has an equal probability of being 0, 45, 90, and 135.
c
      nangle = 1 + nint(3.*(1.-cred)*(1.-cblue))
c	NANGLE has a higher priority of being 1 and a lower probability of
c	being 4.
c
      return
      end
c
c
      subroutine otherdraw(ixstart, iystart, ixend, iyend)
c
c     Draws a vector from (IXSTART,IYSTART) to (IXEND,IYEND) on
c     all devices except SUNCORE and NOG's.
c
      integer*2 ixstart, iystart, ixend, iyend
c
      include 'cio.inc'
c
      if (ixend .ne. ixstart .or. iyend .ne. iystart) then
      	      if (igraphtype .eq. 4) then
			call v102place(ixstart,iystart)
			call v102vctr(ixend,iyend)
     	      else if (igraphtype .eq. 3) then
			call tekplace(ixstart,iystart)
			call tekvctr(ixend,iyend)
	      endif
      	      if (iprinttype .eq. 5) then
        		call quichardplace(ixstart,iystart)
        		call quichardvctr(ixend,iyend)
      	      else if (iprinttype .eq. 4) then
			call hphardplace(ixstart,iystart)
			call hphardvctr(ixend,iyend)
      	      else if (iprinttype .eq. 3) then
			call posthardplace(ixstart,iystart)
			call posthardvctr(ixend,iyend)
      	      else if (iprinttype .eq. 2) then
			call qmshardplace(ixstart,iystart)
			call qmshardvctr(ixend,iyend)
      	      endif
      endif
c
      return
      end
c
