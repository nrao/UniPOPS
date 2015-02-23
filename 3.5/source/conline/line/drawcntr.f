      subroutine drawcntr(savevec, nseg, undefwp,  
     .			  z, isize, jsize, ictr)
c
c     @(#)drawcntr.f	5.1 06/22/94
c
c     Draws contours found by TRACER
c
      real z(isize,jsize)
      integer*2 nseg, ictr, sclc, ipixel, jseg1, curcur
      integer*2 ixtmp, iytmp, n1, n2, n8, n14
      real savevec(4,*), undefwp
      integer*4 isize, jsize
c
      include 'mappl.inc'
      include 'appl.inc'
      include 'core.inc'
c
      logical bothways, found
      character*8 stch
      character*10 fmt, getformat
      real alabel,xold,yold,xfst,yfst,rad,direc,alength,
     .	   xlst,ylst, ep4 
      integer*2 kseg, jseg, lseg, ierr
c
      data n1, n2, n8, n14 /1, 2, 8, 14/
c
      sclc(ipixel) = max(n1, nint(sclchar*float(ipixel))) 
c
      data rad /57.29577951/, ep4/0.0001/
c
      jseg = 0
c
      alabel = min( 100./ax(curcur()), 100./bx(curcur()) )
c     Contours will be labeled if they are longer than 100 pixels.
c 
601   continue
c
      do 610 kseg = jseg+1, nseg
	if (savevec(1,kseg) .gt. 0.0) then
c
c         We have found a starting point so initialize variables
          xold = savevec(1,kseg)
          yold = savevec(2,kseg)
          xfst = xold
          yfst = yold
          bothways = .false.
          jseg = kseg
	  jseg1 = kseg
          alength = 0.0
c         ALENGTH = square of the length of the current drawn contour
c         XOLD, YOLD = pixels where we are to search for a continuation
c         XFRST, YFRST = pixels at which current contour started
c         BOTHWAYS = true if we have looked in both directions from XFST,YFST
c	    = false if we haven't tried the second direction.
c         JSEG = location in SAVEVEC where we should start looking for new
c	    starting points for a contour
c         JSEG1 = location in SAVEVEC where we should start looking for 
c	    continuation from XOLD,YOLD
c
          ixtmp = nint(xold*ax(curcur())+bx(curcur()))
          iytmp = nint(yold*ay(curcur())+by(curcur()))
          call placewp(ixtmp,iytmp,ictr)
	  xlst = xold
	  ylst = yold
c         Move the pen to starting position.
c
500       found = .false.
c  	  FOUND = whether or not any new vectors were found starting from
c	  	XOLD,YOLD
c
603       do 600 lseg = jseg1, nseg
c
c	     Start looking for vectors that continue from (XOLD,YOLD)
c	     FORWARD SEARCH
c
	     if (savevec(2,lseg)-yold .ge. 2) then
		if (.not. found) goto 620
		jseg1 = lseg - 1
		found = .false.
		goto 605
	     endif
c	     Since SAVEVEC is in almost sorted order in Y, if YOLD is > than
c		current SAVEVEC, we have gone beyond where any possible
c		continuations can be found.  Jump out of loop and restart
c		REVERSE search.
c
  	     if (abs(xold-savevec(1,lseg)) .lt. ep4 .and.
     .	         abs(yold-savevec(2,lseg)) .lt. ep4) then
	        if (alength .lt. alabel) alength = alength + 
     .		     (xold-savevec(3,lseg))**2 + 
     .		     (yold-savevec(4,lseg))**2 
	        xold = savevec(3,lseg)
	        yold = savevec(4,lseg)
  	     else if (savevec(1,lseg) .gt. 0.0 .and. 
     .	  	      abs(xold-savevec(3,lseg)) .lt. ep4 .and.
     .	    	      abs(yold-savevec(4,lseg)) .lt. ep4) then
	        if (alength .lt. alabel) alength = alength + 
     .		     (xold-savevec(1,lseg))**2 + 
     .		     (yold-savevec(2,lseg))**2 
	        xold = savevec(1,lseg)
	        yold = savevec(2,lseg)
	     else
		goto 600
  	     endif
c	     Look for vectors which continue on from XOLD and YOLD
c
	     if ( mod(xold,1.) .lt. ep4) then
		direc = sign(90., z(nint(xold),int(yold+ep4))- 
     .		     		  z(nint(xold),1+int(yold+ep4)))
	     else if (mod(yold,1.) .lt. ep4) then
		direc = sign(180., z(int(xold+ep4),nint(yold))- 
     .		     		   z(1+int(xold+ep4),nint(yold)))
		if (direc .gt. 170.) direc = 0.
	     else if (mod(xlst,1.) .lt. ep4) then
		direc = sign(90., z(nint(xlst),int(ylst+ep4))- 
     .		     		  z(nint(xlst),1+int(ylst+ep4)))
	     else
		direc = sign(180., z(int(xlst+ep4),nint(ylst))- 
     .		     		   z(1+int(xlst+ep4),nint(ylst)))
		if (direc .gt. 170.) direc = 0.
	     endif
	     direc = direc / rad
c            Finds the direction in which the contours are decreasing in height.
c
             ixtmp = nint(xold*ax(curcur())+bx(curcur()))
             iytmp = nint(yold*ay(curcur())+by(curcur()))
     	     call vctrwp(ixtmp,iytmp,ictr,sclchar,direc)
	     xlst = xold
	     ylst = yold
	     savevec(1,lseg) = -1
	     found = .true.
c  	     If such vectors exist, plot them, store away new end points
c
600  	     continue
c
	  if (.not. found) goto 620
	  jseg1 = nseg
	  found = .false.
c
605       do 609 lseg = jseg1, jseg, -1
c
c	     Start looking for vectors that continue from (XOLD,YOLD)
c		REVERSE SEARCH
c
	     if (yold - savevec(2,lseg) .ge. 2) then
		if (.not. found) goto 620
		jseg1 = lseg + 1
		found = .false.
		goto 603
	     endif
c	     Since SAVEVEC is in almost sorted order in Y, if YOLD is > than
c		current SAVEVEC, we have gone beyond where any possible
c		continuations can be found.  Jump out of loop and restart
c		FORWARD SEARCH.
c
  	     if (abs(xold-savevec(1,lseg)) .lt. ep4 .and.
     .	         abs(yold-savevec(2,lseg)) .lt. ep4) then
	        if (alength .lt. alabel) alength = alength + 
     .		     (xold-savevec(3,lseg))**2 + 
     .		     (yold-savevec(4,lseg))**2 
	        xold = savevec(3,lseg)
	        yold = savevec(4,lseg)
  	     else if (savevec(1,lseg) .gt. 0.0 .and. 
     .	  	      abs(xold-savevec(3,lseg)) .lt. ep4 .and.
     .	    	      abs(yold-savevec(4,lseg)) .lt. ep4) then
	        if (alength .lt. alabel) alength = alength + 
     .		     (xold-savevec(1,lseg))**2 + 
     .		     (yold-savevec(2,lseg))**2 
	        xold = savevec(1,lseg)
	        yold = savevec(2,lseg)
	     else
		goto 609
  	     endif
c	     Look for vectors which continue on from XOLD and YOLD
c
	     if ( mod(xold,1.) .lt. ep4) then
		direc = sign(90., z(nint(xold),int(yold+ep4))- 
     .		     		  z(nint(xold),1+int(yold+ep4)))
	     else if (mod(yold,1.) .lt. ep4) then
		direc = sign(180., z(int(xold+ep4),nint(yold))- 
     .		     		   z(1+int(xold+ep4),nint(yold)))
		if (direc .gt. 170.) direc = 0.
	     else if (mod(xlst,1.) .lt. ep4) then
		direc = sign(90., z(nint(xlst),int(ylst+ep4))- 
     .		     		  z(nint(xlst),1+int(ylst+ep4)))
	     else
		direc = sign(180., z(int(xlst+ep4),nint(ylst))- 
     .		     		   z(1+int(xlst+ep4),nint(ylst)))
		if (direc .gt. 170.) direc = 0.
	     endif
	     direc = direc / rad
c            Finds the direction in which the contours are decreasing in height.
c
             ixtmp = nint(xold*ax(curcur())+bx(curcur()))
             iytmp = nint(yold*ay(curcur())+by(curcur()))
     	     call vctrwp(ixtmp,iytmp,ictr,sclchar,direc)
	     xlst = xold
	     ylst = yold
	     savevec(1,lseg) = -1
	     found = .true.
c  	     If such vectors exist, plot them, store away new end points
c
609  	     continue
c
	  if (found) then
	    jseg1 = jseg
	    found = .false.
	    goto 603
	  endif
c
620       if (bothways) then
            if (conline(ictr) .lt. 0 .and. alength .gt. alabel) then
               ixtmp = nint(xfst*ax(curcur())+bx(curcur())) - 
     .			sclc(n8*n14/n2)
               iytmp = nint(yfst*ay(curcur())+by(curcur()))
	       if (sclchar .ne. 1.) call charsize(sclchar)
               call place(ixtmp, iytmp)
	       fmt = getformat(levs(ictr), levs(ictr), n8)
	       write(stch,fmt,iostat=ierr) levs(ictr)
	       call pchar(stch, n8)
	       if (sclchar .ne. 1.) call charsize(1.)
            endif  
c	    Label contour (if desired and it is long enough)
c   
	    goto 601
c           You have already looked from the starting point in both 
c           directions.  Look for another starting point.
c
          else
	    xold = xfst
	    yold = yfst
	    bothways = .true.
            ixtmp = nint(xold*ax(curcur())+bx(curcur()))
            iytmp = nint(yold*ay(curcur())+by(curcur()))
            call placewp(ixtmp, iytmp, ictr)
	    xlst = xold
	    ylst = yold
	    jseg1 = kseg
  	    goto 500
          endif
c         Finished following all vectors which started from original 
c         (XOLD,YOLD).  Now try to look for any more positions in the
c         reverse direction from original point.  
c
        endif
c
610     continue
c
      return
c     No more vectors in SAVEVEC so we can go back and get some more.
c     Next contour level if FINISH is true, else continue with old level.
c
      end
c
c
