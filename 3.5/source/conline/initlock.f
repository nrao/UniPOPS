      subroutine initlock()
C-------------------------------------------------------------------------------
C  @(#)initlock.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Creates a file which, if it exists, locks other users from running
c     LINE/CONDAR in that directory
c
      integer*2 ierr, irtn
      integer*4 chmod
      include 'cio.inc'
c
      integer*4 ia1(3), ia2(3), four, system 
      character*8 oobs2, site2, time2
      character*10 date2
      character*1 answr, answr2
c
      data four/4/
c
      oobs2 = ' '
      site2 = ' '
      time2 = ' '
      date2 = ' '
      call idate(ia1)
      call itime(ia2)
c     Gets date, and time
c
102   open(unit=iiotmp,file=program(1:1)// 'LOCKFILE',
     1     access='sequential',status='unknown',iostat=ierr)
      if(ierr .eq. 0) then
	write(iiotmp,1199,iostat=ierr) oobs, site, ia1, ia2, procid
1199	format(1x,a8,1x,a8,1x,i2,'/',i2,'/',i4,1x,i2,':',i2,':',
     1     	       i2,1x,a8)
      endif
      if (ierr .ne. 0) then
	rewind(iiotmp,iostat=ierr) 
	read(iiotmp, 1299, iostat=ierr) oobs2, site2, date2, time2
1299	format(1x,a8,1x,a8,1x,a10,1x,a8)
	close(iiotmp,iostat=ierr)
        write(istderr,5) ' '
5       format(1x,10(a,' '))
        write(istderr,10)
10      format(1x,70('*'))
	write(istderr,5) 'LOCKFILE exists.  '
        write(istderr,5) 'Another user has control of the files',
     .    'in this directory which may'
	write(istderr,5) 'lead to unpredictable results when files are',
     .	  'read from or written to.'
	write(istderr,5) ' '
        if (ierr .eq. 0) then
 	   write(istderr,5) 'Possible user is:'
	   write(istderr,5) 'Obs:', oobs2,' Machine:',site2, 
     1	  	            ' Date/Time:', date2, time2
           write(istderr,5) ' '
        else
	   write(istderr,5) 'Cannot ascertain who the user is'
	   write(istderr,5) ' '
	endif
        write(istderr,10)
101     write(istderr,5) ' '
	write(istderr,5) 'You have the following options:'
	write(istderr,5) '   To exit the program, type: 0'
	write(istderr,5) '   To remove a leftover LOCKFILE, type: 1'
	write(istderr,5) '   To ignore the LOCKFILE, type: 2'
	write(istderr,6) 'Enter 0, 1, or 2 [ default : 0 ]: '
6	format(1x, a, $)
	read(istdin, 7) answr
7	format(a)
	if (answr .eq. ' ' .or. answr .eq. '0') then
	   call exitpops(four)
	else if (answr .eq. '1' .or. answr .eq. '2') then
           write(istderr,5) ' '
	   write(istderr,5) 'WARNING:  This may be very dangerous!! ',
     .			'Proceed at your own risk!!'
	   write(istderr,6) 'Continue (y or n)? [default : n] '
	   read(5,*) answr2
	   if (answr2 .eq. ' ' .or. answr2 .eq. 'n' .or. answr2 .eq. 'N') then
		call exitpops(four)
	   else if (answr2 .eq. 'Y' .or. answr2 .eq. 'y') then
		if (answr .eq. '1') then
			irtn = system('clearlock.exe ' // program(1:1))
			if (irtn .ne. 0) then
			  write(istderr,*) 'Error in clearing LOCKFILE.',
     .				'  Cannot continue!!'
			  write(istderr,*) 'Get help from your local ',
     .				'UniPops guru!'
			  call exitpops(four)
			endif
			goto 102
		endif
	   else
		goto 101
	   endif
	else
	   goto 101
	endif
c	
      else
	close(iiotmp,iostat=ierr)
	irtn = chmod(program(1:1)// 'LOCKFILE', 'a-w')
      endif
c
      return
      end
c
