      subroutine getscan(scanno, iptwh, ierr)
C-------------------------------------------------------------------------------
C  @(#)getscan.F	5.2 09/10/98
C-------------------------------------------------------------------------------
c
      include 'appl.inc'
      include 'core.inc'
      include 'cio.inc'
#ifdef CONTINUUM
      integer*4 iactual, if5
#endif
c
      integer*4 is4, if4, irtn, deffeed
      integer*2 iptwh, irtn2, ierr, ier
      real*4 scanno, ascanno, bscanno
      real*8 xtemp(HDU_FLOAT_SIZE/2)
c
      ierr = 0
      ascanno = abs(scanno)
      is4 = int(ascanno+0.001)
      if4 = mod(100.*ascanno+0.5,100.)
      if(if4.eq.0 .or. if4.gt.99) if4 = deffeed()
      bscanno = is4 + float(if4) / 100.0
c     Find the abs value of scan number (IS4) and feed number (IF4)
c
      irtn = -1
c     Set up irtn with a 'scan not found' flag in case online file or
c     ofline file doesn't exist.  IRTN will only be cleared when a scan
c     is found and correctly converted to SDD.
c
      if (scanno .gt. 0) then
c       First search on-line file and then the offline file in the user's 
c	directory if scan number > 0.
c
	if (online) then
c       Try to get from online file first, then offline
c
#ifdef CONTINUUM
	   iactual = MAX_DATA_POINTS
	   if (if4 .eq. 99) then
		if5 = -1
	   else if (if4 .eq. 98) then
		if5 = 0
	   else
		if5 = if4
	   endif
	   call gcont(is4, if5, iactual, MAX_DATA_POINTS, xtemp, irtn)
#else
	   call gscan(is4, if4, xtemp, irtn)
#endif
c
           if (irtn .eq. 0) then 
              call raz(dtwh(1,iptwh))
              call conversion2(xtemp, dtwh(1,iptwh), ier)
              if (ier .ne. 0) then
                 ierr = 360
                 goto 99
              endif
	   endif
        endif
c       If scan was found, expand it to padded SDD format 
c	ier is zero if everything goes well.
c 
        if (irtn .ne. 0 .and. iounit(1) .gt. 0) then
c	    Scan wasn't in online and offline file exists so...
c
     	    call getbynum(dtwh(1,iptwh), bscanno, irtn2, iounit(1), 
     1                    ciounit(1),ier)
	    if (ier .ne. 0) then
		ierr = ier
		goto 99
	    endif
	    if (irtn2 .ge. 0) irtn = 0
	endif
c	Gets scan from user's data file in SDD format; irtn = 0 if
c	all goes well.
c
      else 
c       Try offline first of scan < 0.
c
	if (iounit(1) .gt. 0) then
            call getbynum(dtwh(1,iptwh), bscanno, irtn2, iounit(1), 
     1                    ciounit(1),ier)
	    if (ier .ne. 0) then
		ierr = ier
		goto 99
	    endif
	    if (irtn2 .ge. 0) irtn = 0
	endif
c	Gets scan from user's data file in SDD format; irtn = 0 if
c	all goes well.
c
        if (irtn .lt. 0 .and. online) then
c	    Scan wasn't in offline and online file exists so...
c
#ifdef CONTINUUM
	   call gcont(is4, if4, iactual, MAX_DATA_POINTS, xtemp, irtn)
#else
	   call gscan(is4, if4, xtemp, irtn)
#endif
c
           if (irtn .eq. 0) then 
              call raz(dtwh(1,iptwh))
              call conversion2(xtemp, dtwh(1,iptwh), ier)
              if (ier .ne. 0) then
                 ierr = 360
                 goto 99
              endif
	   endif
c             If scan was found, convert it to SDD format; irtn should be
c	      zero if everything goes well.
c 
         endif
      endif
c
c     At this point, irtn = 0 if scan was found and the scan is in SDD
c     format.  If not found, irtn is not zero.
c
      if (irtn .ne. 0) ierr = 362
c
99    return
c
      end
c
