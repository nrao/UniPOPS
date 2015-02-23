      subroutine exitpops(iexit4)
C-------------------------------------------------------------------------------
C  @(#)exitpops.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Does some housecleaning before stopping POPS
c     iexit4 = exit status
c     0 = normal termination through EXIT
c     1 = end of STDIN in PREAD
c     2 = Unable to open sun window
c     3 = Bad project code in INITONLINE
c     4 = Existing LOCK file
c     5 = Bad file specification in initfiles
c     6 = Ctrl-C interrupt at start up
c     7 = Bad recovery file
c     8 = error in initlog (bad open or read)
c
      integer*2 istat, ierr
      real*4 memvers
c
      include 'cio.inc'
      include 'appl.inc'
      include 'core.inc'
      include 'lsf.inc'
c
      character*8 oobs2, site2, time2, procid2
      character*10 date2
      character*1023 fullname
      character*24 zdatetime
      character*1 answr
c
      integer*4 irtn, chmod, iexit4, unlink, i4, long, i
      logical*2 opened, inquirefile
      integer*4 nummax, lunit, ioput
c
      equivalence (memvers, kx(13))
c
      data nummax /32766/
c
      call ignorectrlc()
c
      write(istdout,*) ' '
      if(iexit4 .eq. 0) then
	write(istdout,*) 'Normal Termnination...'
21	write(istdout,*) ' '
	write(istdout,*) 'Do you want to update the RECOVER file?'
	write(istdout,20) '(Y or N): '
20	format(1x,a,$)
        call flush(long(istdout))
	read(istdin,22) answr
22      format(a)
	if (answr .eq. 'y' .or. answr .eq. 'Y') then
	  write(istdout,*) ' '
	  write(istdout,*) 'Creating RECOVER file'
          call flush(long(istdout))
	  open(unit=irecover,file=crecover,status='unknown',
     1       form='unformatted',access='sequential')
	  rewind (irecover)
c			make sure VERS stored with value of version
          vers = version
c
	  write(irecover) version, numcio, numappl, nummax, nummax, 
     .			  nummax
 	  write(irecover) (Rio(i4),i4=1,numcio)
	  write(irecover) (Rappl(i4),i4=1,numappl)
	  write(irecover) (k(i4),i4=1,nummax)
	  write(irecover) (kx(i4),i4=1,nummax)
	  write(irecover) (listf(i4),i4=1,nummax)
	  close (irecover)
	else if (answr .eq. 'n' .or. answr .eq. 'N') then
	  write(istdout,*) ' '
	  write(istdout,*) 'RECOVER file has not been updated'
          call flush(long(istdout))
	else
	  goto 21
	endif
      else
	write(istderr,*) 'Abnormal Termination ... Error code ', iexit4
      endif
c
      call donemenu
c     Close down menu application, if it is running
c
      call gpclose
c     Close down graphics screens
c
      call closeall
c     Close down all opened data files
c
c					close any opened batch files
      do 100 i = 1, iinptr
         if (iinlist(iinptr, 2) .eq. ibatchin) then
            call filecomp(cinlist(iinptr), fullname)
            if (opened(fullname)) then
               close (iinlist(iinptr,1),iostat=istat)
               lunit = iinlist(iinptr, 1)
               lunit = ioput(lunit)
c			ignore ioput errors, we don't really care
            endif
         endif
 100  continue
c
      do 110 i = 1, nfiles
	if (ffile(i,1) .gt. 0) close(ffile(i,1),iostat=istat)
110	continue
c
      if (opened(csetupin)) close(isetupin,iostat=istat)
      if (opened(cmemory)) call closms(imemory, istat)
      if (opened(coutgrph)) close(ioutgrph,iostat=istat)
      if (inquirefile(coutgrph)) irtn = unlink(coutgrph)
      if (opened(couttxt)) close(iouttxt,iostat=istat)
      close(ishelpin,iostat=istat)
      close(iiotmp,iostat=istat)
      if (inquirefile(couttxt)) irtn = unlink(couttxt)
      call filecomp(clogout,fullname)
      if (dolog) write(ilogout, 1030, iostat=istat)
     .		'# Command logging OFF -- ', zdatetime()
 1030 format(2a)
      if (opened(fullname)) close(ilogout,iostat=istat)
      call filecomp(ccube,fullname)
      if (opened(fullname)) close(icube,iostat=istat)
      call filecomp(cprtout,fullname)
      if (opened(fullname)) close(iprtout,iostat=istat)
c     Close down all opened input/output/ etc files
c
      if (iexit4 .ne. 4) then
        if (iexit4 .eq. 0) call closeaccess
        open(unit=iiotmp, file=program(1:1)//'LOCKFILE',status='old',
     1     iostat=ierr)
        if (ierr .eq. 0) then
	 rewind(iiotmp,iostat=ierr)
         if(ierr.eq.0) read(iiotmp,10,iostat=ierr) oobs2, site2, date2, 
     1					      time2, procid2
10       format(1x,a8,1x,a8,1x,a10,1x,a8,1x,a8)
	 if (ierr .eq. 0) close(iiotmp,iostat=istat)
	 if (ierr.eq.0 .and. procid2.eq.procid) then
	      irtn = chmod(program(1:1) // 'LOCKFILE', 'u+w')
      	      irtn = unlink(program(1:1) // 'LOCKFILE')
	 else
	      write(istderr,*) ' '
	      write(istderr,*) 'WARNING: Bad lockfile?!? ',
     .			'Someone may be using this directory!!'
	 endif
        else
	 write(istderr,*) ' '
         write(istderr,*) 'WARNING: your lockfile could not be opened.'
         write(istderr,*) 
     .      'Someone has been and may still be using this directory!!'
        endif
      endif
c     Clear lock file
c
      write(istdout,*) ' '
      write(istdout,*) 'Goodbye ', oobs
      write(istdout,*) ' '
      call flush(long(istdout))
c
      call exit(iexit4)
c
      end
