      subroutine getir(scanno, record, phase, iptwh, ierr)
C-------------------------------------------------------------------------------
C  @(#)getir.f	5.2 09/10/98
c
c  Gets data for a particular scan, record, or phase combination.
c
C-------------------------------------------------------------------------------
c
      include 'appl.inc'
      include 'core.inc'
      include 'cio.inc'
      include 'cform.inc'
c
      integer*4 iptwh
c
      integer*4 onlinesite, is4, if4, irtn, deffeed, long
      integer*2 ierr, record, phase, irtn2, ier
      real*4 scanno, ascanno, bscanno
      real*8 xtemp(HDU_FLOAT_SIZE/2), specproc, ac
c
      data specproc/'SPECPROC'/, ac/'1024ACIV'/
c
      if (onlinesite() .eq. 1) then
c	Only for GB data
	ierr = -240
	goto 99
      endif
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
	  call gscan(is4, if4, xtemp, irtn)
	  if (irtn .eq. 0) then
             call getrecio(long(phase), long(record), if4, xtemp, irtn)
c			if irtn is zero, everthing went well, pad it out
             if (irtn .eq. 0) then
	        call raz(dtwh(1,iptwh))
 	        call conversion2(xtemp, dtwh(1,iptwh), irtn2)
	        if (irtn2 .ne. 0) then
	    	   ierr = -360
		   goto 99
	        endif
             else
c			getrecio returns appropriate error message codes
                ierr = irtn
                goto 99
             endif
	   endif
        endif
c       At this point, irtn = 0 if scan was found and the scan is in SDD
c       format.  If not found, irtn is not zero.
c
        if (irtn .ne. 0 .and. iounit(5) .gt. 0) then
c	    Scan wasn't in online and offline file exists so...
c
     	    call getbyrec(dtwh(1,iptwh), bscanno, record, phase,
     .			  irtn2, iounit(5), ciounit(5),ier)
	    if (ier .ne. 0) then
		ierr = ier
		goto 99
	    endif
	    if (irtn2 .ge. 0) irtn = 0
	endif
c	Gets scan from user's RECORDS file in SDD format; irtn = 0 if
c	all goes well.
c
      else
c       Try offline first of scan < 0.
c
	if (iounit(5) .gt. 0) then
     	    call getbyrec(dtwh(1,iptwh), bscanno, record, phase,
     .			  irtn2, iounit(5), ciounit(5),ier)
	    if (ier .ne. 0) then
		ierr = ier
		goto 99
	    endif
	    if (irtn2 .ge. 0) irtn = 0
	endif
c	Gets scan from user's RECORDS file in SDD format; irtn = 0 if
c	all goes well.
c
        if (irtn .lt. 0 .and. online) then
c	   Scan wasn't in offline and online file exists so...
c
	   call gscan(is4, if4, xtemp, irtn)
	   if (irtn .eq. 0) then
             call getrecio(long(phase), long(record), if4, xtemp, irtn)
c			if irtn is zero, everthing went well, pad it out
             if (irtn .eq. 0) then
	        call raz(dtwh(1,iptwh))
 	        call conversion2(xtemp, dtwh(1,iptwh), irtn2)
	        if (irtn2 .ne. 0) then
	    	   ierr = -360
		   goto 99
	        endif
             else
c			getrecio returns appropriate error message codes
                ierr = irtn
                goto 99
             endif
           endif
	endif
c       At this point, irtn = 0 if scan was found and the scan is in SDD
c       format.  If not found, irtn is not zero.
c
      endif
c
      if (irtn .ne. 0) ierr = 362
c
99    return
c
      end
c
