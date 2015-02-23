      logical function headline()
c
c     @(#)headline.f	5.3 05/04/98
c
c     Produces and writes the one record header for the cube file
c
      character*80 hmask(36)
      integer ierr, nend, ndchars, lnblnk
c
      include 'cube.inc'
c
      write(6,*) 'Creating FITS header'
      write(6,*) ' '
      headline = .false.
c
      open(unit=iheadmask,file=headerfile,
     1     access='sequential',form='formatted',status='old',
     2     iostat=ierr)
c
      if (ierr .ne. 0) then
	write(0,*) 'Major error!  Template header-file not found...'
	goto 99
      endif
c
      read(iheadmask,10,iostat=ierr) hmask
10    format(a)
c
      if (ierr .ne. 0) then
	write(0,*) 'Major error!  Template header-file cannot be read...'
	goto 99
      endif
c     Reads in FITS header template.  Program must know of any changes to
c     template.
c
      close (iheadmask)
      numhead = 1
c		We now always write (v,a,d) but we can add to (v,d,a) if nec.
      afirst = .true.
      write(unit=hmask(4) (24:30),fmt=20) nv
      write(unit=hmask(5) (24:30),fmt=20) na
      write(unit=hmask(6) (24:30),fmt=20) nd
      write(unit=hmask(26) (24:30),fmt=20) ibad
20    format(i7)
c
      hmask(7) (12:19) = coordv
      hmask(12) (12:19) = coorda
      hmask(17) (12:19) = coordd
      hmask(22) (12:19) = appepch
      hmask(23) (12:19) = object(1)
      hmask(23) (20:27) = object(2)
      hmask(24) (12:19) = origin
      write(*,*) hmask(25)
      hmask(25) (11:11) = '\''
      write(*,*) hmask(25)
      ndchars = lnblnk(date)
      nend = ndchars + 11
      write(*,*) ndchars, nend
      write(*,*) date(1:ndchars)
      hmask(25) (12:nend) = date(1:ndchars)
      write(*,*) hmask(25)
      nend = nend + 1
      hmask(25) (nend:nend) = '\''
      write(*,*) hmask(25)
      hmask(29) (12:19) = units
c
      write(unit=hmask(8) (15:30),fmt=40) v0*1.e03
      write(unit=hmask(9) (15:30),fmt=40) dvc*1.e03
      write(unit=hmask(10) (15:30),fmt=40) vp
      write(unit=hmask(11) (15:30),fmt=40) errv*1.e03
c     Converts velocities from km/sec to m/sec
c
      write(unit=hmask(13) (15:30),fmt=40) a0
      write(unit=hmask(14) (15:30),fmt=40) dac
      write(unit=hmask(15) (15:30),fmt=40) ap
      write(unit=hmask(16) (15:30),fmt=40) erra
      write(unit=hmask(18) (15:30),fmt=40) d0
      write(unit=hmask(19) (15:30),fmt=40) ddc
      write(unit=hmask(20) (15:30),fmt=40) dp
      write(unit=hmask(21) (15:30),fmt=40) errd
      write(unit=hmask(27) (15:30),fmt=40) tscale
      write(unit=hmask(28) (15:30),fmt=40) tzero
      write(unit=hmask(30) (15:30),fmt=40) tmin
      write(unit=hmask(31) (15:30),fmt=40) tmax
      if (coordd(1:4) .eq. 'DEC-') then
         write(unit=hmask(32) (15:30),fmt=40) equinox
         hmask(33) (11:70) = label(1)
         hmask(34) (11:70) = label(2)
         hmask(35) (11:70) = label(3)
      else
	 hmask(32) = "COMMENT 0"
         hmask(33) (11:70) = label(1)
         hmask(34) (11:70) = label(2)
         hmask(35) (11:70) = label(3)
      endif
40    format(1pe16.9)
c
      write(6,*) 'Header = '
      write(6,*) hmask
      write(6,*) '  '
c
      write(ioutdev,rec=1,iostat=ierr) hmask
      if (ierr .ne. 0) then
	write(0,*) 'Error on writing header to CUBE file...'
	goto 99
      endif
c
      headline = .true.
c	
99    return
c
      end
c
