      subroutine gget(scanno, iptwh, icur, ierr)
C-------------------------------------------------------------------------------
C  @(#)gget.f	5.1 06/22/94
C   Get a GAINS scan
C      scanno = scan_number + [rec_number/100]
C      iptwh = which array to put it in
C      icur = 0 => by scan number
C             1 => get current scan from on-line file
C      ierr = return error value
C-------------------------------------------------------------------------------
c
      include 'appl.inc'
      include 'core.inc'
      include 'cio.inc'
      integer*4 isize
      parameter (isize = 11200)
c
      integer*4 is4, if4, irtn, deffeed 
      integer*4 icur
      integer*2 iptwh, irtn2, ierr, ier
      real*4 scanno, ascanno, bscanno
      real*8 xtemp(isize/8)
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
      if (scanno .gt. 0 .or. icur .eq. 1) then
c       First search on-line file and then the offline file in the user's 
c	directory if scan number > 0 
c	if icur = 1 then just check the online file
c
	if (online) then
c       Try to get from online file first, then offline
c
           if (icur .eq. 0) then
	      call ggscan(is4, if4, xtemp, irtn)
           else
              if4 = int(scanno+0.001)
              call glgscan(xtemp, if4, irtn)
           endif
c
c          get gains/zero scan from on_line file this will fail unless
c	   this is from tucson so there is no need to check onlinesite.
c
           if (irtn .eq. 0) then 
c	if current scan requested, then scanno is REALLY a receiver number
c	so we need to set if4 appropriately in that case
              call raz(dtwh(1,iptwh))
              call conversion2(xtemp, dtwh(1,iptwh), ier)
              if (irtn .ne. 0) then
                 ierr = 360
                 goto 99
              endif
	   endif
        endif
c       If scan was found, convert it to SDD format; irtn should be
c	zero if everything goes well.
c 
        if (irtn .ne. 0 .and. iounit(4) .gt. 0 .and. icur .eq. 0) then
c	    Scan wasn't in online and offline file exists and the current
c           scan wasn't requested so ....
c
     	    call getbynum(dtwh(1,iptwh), bscanno, irtn2, iounit(4), 
     1                    ciounit(4),ier)
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
	if (iounit(4) .gt. 0) then
            call getbynum(dtwh(1,iptwh), bscanno, irtn2, iounit(4), 
     1                    ciounit(4),ier)
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
c	    we don't want current scan either
           icur = 0
c
	   call ggscan(is4, if4, xtemp, irtn)
c          get scan from on_line file,  must be from tucson
c
           if (irtn .eq. 0) then 
              call raz(dtwh(1,iptwh))
              call conversion2(xtemp, dtwh(1,iptwh), ier)
              if (irtn .ne. 0) then
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
