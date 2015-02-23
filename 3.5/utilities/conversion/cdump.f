C
      PROGRAM CDUMP
C
c      @(#)cdump.f	5.1 06/22/94
c
C Searches through scans and finds only those spectral line scans with
C matching projectcodes.  These matching scans are placed in a file
C ldata.'projectcode'
C
      INTEGER*2 IBUF(2560), j, ifeed, istp, istrt, numarg, ierr
      character*8 projcode, cstrt, cstp, cfeed,site,oobs, projcod1
      integer*4 irtn, iargc, ii1, ii2, ii3, i5120, ierr4, ii4, m1, n0
      integer*4 i4a,nactual,i4b,iscan, maxdata
      integer*4 kfeed, lfeed, hostnm, getlog
      logical pos
      real RBUF(1280), stp, strt, rscans(32766)
      real*8 dprojcode,doobs,dsite
c
      equivalence (ibuf(1),rbuf(1))
      equivalence (projcode,dprojcode),(oobs,doobs),(site,dsite)
c
      DATA   ii1/1/, ii2/2/, ii3/3/, i5120/5120/, ii4/4/, maxdata/1190/,
     .	     m1/-1/, n0/0/ 
C
      numarg = iargc()
      if (numarg .le. 0) then
	write(0,*) 'Must specify projectcode'
	call exit(ii1)
      else
	call getarg(ii1, projcod1)
	call uppercase(projcod1, projcode)
	irtn = hostnm(site)
	irtn = getlog(oobs)
      endif
      call openaccess(dprojcode,doobs,dsite,irtn)
c
      if (irtn .ne. 0) then
         write (0,*) 'Trouble with on-line data -- Terminating'
         call exit(ii1)
      endif
C
      if (numarg .gt. 1) then
	call getarg(ii2, cstrt)
	read(cstrt, 10,iostat=ierr) istrt
10	format(i8)
	if (ierr .ne. 0 .or. istrt .lt. 0) then
	   write(0,*) 'Bad starting scan number: ', cstrt
	   call exit(ii1)
	endif
	strt = float(istrt)
      else
	strt = 0.0
      endif
c
      if (numarg .gt. 2) then
	call getarg(ii3, cstp)
	read(cstp, 10,iostat=ierr) istp
	if (ierr .ne. 0 .or. istp .lt. istrt) then
	   write(0,*) 'Bad starting or stoping scan number: ',
     .		 cstrt, ' ', cstp
	   call exit(ii1)
	endif
	stp = float(istp)
      else
	stp = 999999.0
      endif
c
      pos = .false.
      kfeed = 0
      if (numarg .gt. 3) then
	call getarg(ii4, cfeed)
	if (cfeed .eq. 'P' .or. cfeed .eq. 'p') then
	   pos = .true.
	else
	   read(cfeed, 10,iostat=ierr) ifeed
	   if (ierr .ne. 0) then
	      write(0,*) 'Bad feed number: ',cfeed
	      call exit(ii1)
	   endif
           if (ifeed .eq. 99) then 
	      kfeed = -1
           else if (ifeed .eq. 98) then
	      kfeed = 0
           else if (ifeed .gt. 0 .and. ifeed .le. 16) then
	      kfeed = ifeed
           else if (ifeed .ne. 0) then
	      write(0,*) 'Bad feed number: ',cfeed
	      call exit(ii1)
           endif
	endif
      endif
C
  70  nactual = 32766
      i4a = 0
      i4b = 32766
      call gcontlist2(nactual,i4b,rscans,i4a)
      do 90 j = 1, nactual
         iscan = rscans(j)
	 if (iscan.ge.strt .and. iscan.le.stp) then
	    if (ifeed.ne.0) then
               call gcont(iscan,kfeed,i4a,maxdata,ibuf(1),irtn)
               if ((irtn.eq.0).and.(rbuf(1).ge.strt).and.
     +            (rbuf(1).le.stp)) then
		  rbuf(1)=rbuf(1)+float(ifeed)/100
                  call writebuff (ierr4, i5120, rbuf)
	       endif
	    else
	       do 80 lfeed=1,16
                 call gcont(iscan,lfeed,i4a,maxdata,ibuf(1),irtn)
                 if ((irtn.eq.0).and.(rbuf(1).ge.strt).and.
     +              (rbuf(1).le.stp)) then
		    rbuf(1)=rbuf(1)+float(lfeed)/100
                    call writebuff (ierr4, i5120, rbuf)
	 	 else
		    goto 81
		 endif
  80           continue
  81	       if (pos) then
		 call gcont(iscan,n0,i4a,maxdata,ibuf(1),irtn)
                 if (irtn.eq.0) then
		    rbuf(1)=rbuf(1)+.98
                    call writebuff (ierr4, i5120, rbuf)
		 endif
		 call gcont(iscan,m1,i4a,maxdata,ibuf(1),irtn)
                 if (irtn.eq.0) then
		    rbuf(1)=rbuf(1)+.99
                    call writebuff (ierr4, i5120, rbuf)
		 endif
	       endif
	    endif
	 endif
  90  CONTINUE
      call closeaccess()
      STOP
C----------------------------------------------------------------------
      END
