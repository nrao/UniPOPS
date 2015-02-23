C
      PROGRAM LDUMP
C
c      @(#)ldump.f	5.1 06/22/94
c
C Searches through scans and finds only those spectral line scans with
C matching projectcodes.  These matching scans are placed in a file
C ldata.'projectcode'
C
      INTEGER*2 IBUF(2560), j, istrt, ierr, numarg, istp
      character*8 projcode, cstrt, cstp, oobs, site, projcod1
      integer*4 irtn, iargc, ii1, ii2, ii3, i5120, ierr4
      integer*4 i4a,nactual,i4b,iscan, hostnm, getlog, if4
      real RBUF(1280), strt, stp, rscans(32766)
      real*8 dprojcode,doobs,dsite
c
      equivalence (ibuf(1),rbuf(1))
      equivalence (projcode,dprojcode),(oobs,doobs),(site,dsite)
c
      DATA   ii1/1/, ii2/2/, ii3/3/, i5120/5120/ 
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
C
  70  nactual = 32766
      i4a = 0
      i4b = 32766
      call gscanlist2(nactual,i4b,rscans,i4a)
      do 90 j = 1, nactual
         iscan = rscans(j)
	 if (iscan.ge.strt .and. iscan.le.stp) then
            call gscan(iscan,if4,ibuf(1),irtn)
            if (irtn.eq.0) call writebuff (ierr4, i5120, rbuf)
	 endif
  90  CONTINUE
      call closeaccess()
      STOP
C----------------------------------------------------------------------
      END
