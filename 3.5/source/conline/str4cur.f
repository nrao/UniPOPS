      subroutine str4cur(plottype)
c
c     @(#)str4cur.f	5.2 09/10/98
c
c     Stores the necessary information for PLOTTYPE plottype for
c     later use by a cursor routine.  Provides the capability to use
c     the cursors on multiple plots on the same page.
c
      integer*2 plottype
c
      integer*2 n32, n120, m1, n128, fshort, curcur, i2hdrsize
c
      include 'appl.inc'
      include 'core.inc'
      include 'mappl.inc'
c
      parameter (n32=32)
      parameter (n120=120)
      parameter (n128=128)
      parameter (m1=-1)
      parameter (i2hdrsize=PADDED_HDR_SIZE*4)
c
      numplots = curcur()
c
      showplot(numplots) = plottype
c
      if (plottype .eq. 0) then
	call copy(i2hdrsize, dtwh(1,1), dplots(1, numplots))
      else if (plottype .eq. 1) then
	call copy(fshort(n32), title, dplots(1, numplots))
	dplots(5, numplots) = xlog
	dplots(6, numplots) = ylog
      else if (plottype .ge. 2 .and. plottype .le. 5) then
	call copy(n128, mhead(1,1), dplots(1, numplots))
      else
        call oerror(n120, m1, "STR4CUR")
      endif
c
      showplot(curcur()) = showplot(numplots)
      ax(curcur()) = ax(numplots)
      ay(curcur()) = ay(numplots)
      bx(curcur()) = bx(numplots)
      by(curcur()) = by(numplots)
      xorg(curcur()) = xorg(numplots)
      yorg(curcur()) = yorg(numplots)
      xmax(curcur()) = xmax(numplots)
      ymax(curcur()) = ymax(numplots)

      return
      end
c
c----------------------------------
c
      subroutine rtrn4cur(ix, iy, whichplot, array, ierr)
c
c     Returns values from storage for usage by a cursor routine.
c     IX, IY = pixel position for which parameters are to be found.
c     IERR = 0 if all goes OK
c     whichplot = location in cursor arrays where information about the
c		desired plot is kept.
c
      integer*2 ix, iy, ierr, whichplot, i2hdrsize
c
      double precision array(*)
c
      integer*2 n32, n120, m1, n128, fshort, i, j, plottype
c
      include 'appl.inc'
      include 'core.inc'
      include 'mappl.inc'
c
      parameter (n32=32)
      parameter (n120=120)
      parameter (n128=128)
      parameter (m1=-1)
      parameter (i2hdrsize=PADDED_HDR_SIZE*4)
c
      j = numplots + 1
      do 10 i = 1, MAXPLOTS - 1
	j = j + 1
	if (j .gt. MAXPLOTS) j = 1
        if (showplot(j) .ge. 0) then
	    if ( ix .ge. xorg(j) .and. ix .le. xmax(j) .and.
     .		 iy .ge. yorg(j) .and. iy .le. ymax(j)) then
		     plottype = showplot(j)
      		     if (plottype .eq. 0) then
			call copy(i2hdrsize, dplots(1, j), array)
      		     else if (plottype .eq. 1) then
			call copy(fshort(n32), dplots(1, j), array)
      		     else if (plottype .ge. 2 .and. plottype .le. 5) then
			call copy(n128, dplots(1, j), array)
      		     else
        		call oerror(n120, m1, "STR4CUR")
      		     endif
		     whichplot = j
		     ierr = 0
		     goto 99
	    endif
	endif
10	continue
c
      ierr = 1
c
99    return
      end
c
c-------------------------------------------
c
      subroutine resetcur
c
c     Resets cursor arrays to their default values
c
      integer*2 i, j, i1, curcur, i2hdrsize
      double precision dinfinity
      logical first  
c
      include 'appl.inc'
c
      parameter (i2hdrsize=PADDED_HDR_SIZE*4)
      data first/.true./
c
      if (.not. first) then
	if (curcur() .ne. 1) then
	  xmax(1) = xmax(curcur())
	  ymax(1) = ymax(curcur())
	  xorg(1) = xorg(curcur())
	  yorg(1) = yorg(curcur())
	  ax(1) = ax(curcur())
	  ay(1) = ay(curcur())
	  bx(1) = bx(curcur())
	  by(1) = by(curcur())
	  call copy(i2hdrsize, dplots(1,curcur()), dplots(1,1))
	endif
	showplot(1) = -1
	i1 = 2
      else
	i1 = 1
	first = .false.
      endif
c     If first initialization, initialize all parts of the cursor arrays
c     If not, initialize all but slot one and store in slot 1 the current
c     contents of the cursor arrays.
c
      numplots = 0
      do 905 i = i1, MAXPLOTS
        XORG(i)=1
        YORG(i)=1
        XMAX(i)=1024
        YMAX(i)=800
        ax(i) = 0.0
        ay(i) = 0.0
        bx(i) = 0.0
        by(i) = 0.0
        showplot(i) = -1
        do 906 j = 1, PADDED_HDR_SIZE
	   dplots(j, i) = dinfinity()
906	   continue
905	continue
c
      return
      end
c
c-------------------------------------------
c
      integer*2 function curcur()
c
c     Returns the array location for the current plot
c
      include 'appl.inc'
c
      curcur = numplots + 1
      if (curcur .gt. MAXPLOTS) curcur = 1
c
      return
      end
c
