      integer*2 function chngfile(answr2, inum, filename)
C-------------------------------------------------------------------------------
C  @(#)chngfile.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Function for changing data files
c     ANSWR2 = one of the possible commands (HELP, DIR, SUBTRACT, etc.)
c     INUM = file type number (1,2,3...)
c     FILENAME = name of new files
c
      include 'cio.inc'
c
      character*4 modes(12)
      integer*2 inum, n0, n9, GAINS, RECORDS
      character*(*) filename, answr2
      character*1023 fullname
      character*24 zdatetime
      logical*2 opened
      integer*4 irtn, istat, access, liounit, system, onlinesite
c
      data modes/'r ','rw ','rw ','r ','r ',5*' ','w ','w '/
      data n0, n9 /0, 9/
c
      parameter (GAINS=4)
      parameter (RECORDS=5)
c
      chngfile = -1
c
      if (answr2 .eq. ' ') then
	chngfile = 0
	goto 99
      endif
c
      if (filename .eq. cmemory) then
	   write(istderr,1) 'CHNGFILE: Cannot name a file ',cmemory,' ***'
	   goto 99
      endif
c
      if (inum .ne. 11 .and. inum .ne. 12) then
	if (inum .gt. 0 .and. inum .le. 10 ) then
	  if (iounit(inum) .eq. -32767) then
		write(istderr,1) 'CHNGFILE: Bad file type.'
		goto 99
	   endif
	endif
      endif
c
      if ( (inum .eq. RECORDS .and. onlinesite() .gt. 0) .or. 
     .     (inum .eq. GAINS .and. onlinesite() .eq. 0) ) then
     		write(istderr,1) 'CHNGFILE: Bad file type.'
		goto 99
      endif
c
46    if(answr2 .eq. 'SUBTRACT') then
	if (inum .eq. 11 ) then
            call filecomp(cprtout, fullname)
	    if (opened(fullname)) close(abs(iprtout),iostat=istat)
	    iprtout = -abs(iprtout)
	    cprtout = ' '
      	    iout = istdout
	else if (inum .eq. 12 ) then
            call filecomp(clogout, fullname)
	    if (opened(fullname)) then
	        if (dolog) write(ilogout, 1030, iostat=istat)
     .			'# Command logging OFF -- ', zdatetime()
 1030 	        format(2a)
		close(abs(ilogout),iostat=istat)
	    endif
	    ilogout = -abs(ilogout)
	    clogout = ' '
	    dolog = .false.
	else 
            liounit = iounit(inum)
            call sddclose(liounit)
	    iounit(inum) = -abs(iounit(inum))
	    ciounit(inum) = ' '
	endif
c
      else if(answr2 .eq. 'CHANGE') then
        call filecomp(filename, fullname)
	if (access(fullname,modes(inum)).eq.0) then
	  if (inum .eq. 11) then
           call filecomp(cprtout, fullname)
	   if(opened(fullname)) close(abs(iprtout),iostat=istat)
	   iprtout = abs(iprtout)
	   cprtout = filename
	   if (iout .eq. iprtout) then
        	call filecomp(cprtout, fullname)
		open(unit=iprtout, file=fullname, 
     1		     status='old', fileopt = 'eof', iostat=istat)
           endif
	  else if (inum .eq. 12) then
           call filecomp(clogout, fullname)
	   if(opened(fullname)) then
	        if (dolog) write(ilogout, 1030, iostat=istat)
     .			'# Command logging OFF -- ', zdatetime()
		close(abs(ilogout),iostat=istat)
	   endif
	   ilogout = abs(ilogout)
	   clogout = filename
	   if (dolog) then
        	call filecomp(clogout, fullname)
	   	open(unit=ilogout, file=fullname, 
     1		     status='old', fileopt = 'eof', iostat=istat)
	   	write(ilogout, 1030, iostat=istat)'# Command logging ON -- ', 
     .				zdatetime()
           endif
	  else 
	   iounit(inum) = abs(iounit(inum))
	   ciounit(inum) = filename
           liounit = -iounit(inum)
           call sddopen(liounit,fullname,modes(inum), irtn)
c			if irtn is non-zero, an error occured
c
	   if (inum .eq. RECORDS) then
	     if (irtn .eq. -1) then
		irtn = 0
	     else if (irtn .eq. 0) then
		call sddclose(liounit)
		write(istderr,1) 'CHNGFILE:  Trying to attach a file that',
     .			' is not in RECORDS format.'
                goto 99
	     endif
	   else
	     if (irtn .eq. -1) then
		call sddclose(liounit)
                write(istderr,1) 'CHNGFILE: Cannot attach a RECORDS ',
     .			'file as a data file.'
                goto 99
	     endif
	   endif
c	   If RECORDS file, an irtn of -1 is OK but 0 is bad; for all other
c	   an irtn of 0 is OK but a -1 is bad.
c
           if (irtn .ne. 0) then
	      if (irtn .le. 3) then
                  write(istderr,1) 'CHNGFILE: File by that name ',
     .	  	  'doesn"t exist or has the wrong access privelages.'
                  goto 99
               else if (irtn .eq. 4 .or. irtn .eq. 5) then
                  write(istderr,1) 
     .              'CHNGFILE: An impossible error occured.'
                  write(istderr,2)
     .              'Send a report of this error to a unipops guru ',
     .              ' ... sddopen err no ', irtn
                  goto 99
               else if (irtn .eq. 6) then
                  write(istderr,1) 'CHNGFILE: Could not open new file',
     .              ' -- Old file WAS closed.'
                  iounit(inum) = -iounit(inum)
                  ciounit(inum) = ' '
                  goto 99
               else if (irtn .eq. 7) then
                  write(istderr, 1) 
     .              'CHNGFILE: Error reading bootstrap ',
     .              'or index information -- File may be corrupt.'
                  write(istderr, 1) 'CHNGFILE: Old file was closed.'
               else if (irtn .eq. 8) then
                  write(istderr,1) 
     .              'CHNGFILE: File is in an improper format.'
                  iounit(inum) = -iounit(inum)
                  ciounit(inum) = ' '
                  goto 99
               else 
                  write(istderr, 1) 
     .              'CHNGFILE: An unrecognized error was returned ',
     .              'by sddopen.'
                  write(istderr,2)
     .              'Send a report of this error to a unipops guru ',
     .              ' ... sddopen err no ', irtn
               endif
            endif
	  endif
	else
	  write(istderr,1) 'CHNGFILE: File by that name ',
     1		'doesn"t exist or has the wrong access privelages.'
	  goto 99
	endif
c
      else if(answr2 .eq. 'CREATE') then
	  if (inum .eq. 11) then
            call filecomp(cprtout, fullname)
	    if (opened(fullname)) close(abs(iprtout),iostat=istat)
            call filecomp(filename, fullname)
	    open(unit = abs(iprtout), file = fullname, status='new',
     1		iostat = irtn, fileopt = 'eof')
	    if (irtn .eq. 0) then
	      iprtout = abs(iprtout)
	      cprtout = filename
	      if (iout .ne. iprtout) close(unit=abs(iprtout),iostat=istat)
	    else
	      write(istderr,1) 'CHNGFILE: Cannot create file.'
	      goto 99
	    endif
c	    Creates printout file if it doesn't already exist
c
	  else if (inum .eq. 12) then
            call filecomp(clogout, fullname)
	    if (opened(fullname)) then
	        if (dolog) write(ilogout, 1030, iostat=istat)
     .			'# Command logging OFF -- ', zdatetime()
		close(abs(ilogout),iostat=istat)
	    endif
            call filecomp(filename, fullname)
	    open(unit = abs(ilogout), file = fullname, status='new',
     1		iostat = irtn)
	    if (irtn .eq. 0) then
	      ilogout = abs(ilogout)
	      clogout = filename
	      if (dolog) then
	   	write(ilogout, 1030, iostat=istat) '# Command logging ON -- ', 
     .				zdatetime()
	      else
		close(unit=abs(ilogout),iostat=istat)
	      endif
	    else
	      write(istderr,1) 'CHNGFILE: Cannot create file.'
	      goto 99
	    endif
c	    Creates printout file if it doesn't already exist
c
	  else if (inum .ne. RECORDS) then
	    irtn = system('makefile.exe ' // filename  )
            if (irtn .ne. 0) call oerror(n9, n0, 'CHNGFILE')
	    iounit(inum) = abs(iounit(inum))
	    ciounit(inum) = filename
            liounit = -iounit(inum)
            call filecomp(filename, fullname)
            call sddopen(liounit,fullname,modes(inum), irtn)
c			if irtn is non-zero, an error occured
            if (irtn .ne. 0) then
              if (irtn .le. 3) then
                  write(istderr,1) 'CHNGFILE: File by that name ',
     .	  	  'doesn"t exist or has the wrong access privelages.'
                  goto 99
               else if (irtn .eq. 4 .or. irtn .eq. 5) then
                  write(istderr,1) 
     .              'CHNGFILE: An impossible error occured.'
                  write(istderr,2)
     .              'Send a report of this error to a unipops guru ',
     .              ' ... sddopen err no ', irtn
                  goto 99
               else if (irtn .eq. 6) then
                  write(istderr,1) 'CHNGFILE: Could not open new file',
     .              ' -- Old file WAS closed.'
                  iounit(inum) = -iounit(inum)
                  ciounit(inum) = ' '
                  goto 99
               else if (irtn .eq. 7) then
                  write(istderr, 1) 
     .              'CHNGFILE: Error reading bootstrap ',
     .              'or index information -- File may be corrupt.'
                  write(istderr, 1) 'CHNGFILE: Old file was closed.'
               else if (irtn .eq. 8) then
                  write(istderr,1) 
     .              'CHNGFILE: File is in an improper format.'
                  iounit(inum) = -iounit(inum)
                  ciounit(inum) = ' '
                  goto 99
               else 
                  write(istderr, 1) 
     .              'CHNGFILE: An unrecognized error was returned ',
     .              'by sddopen.'
                  write(istderr,2)
     .              'Send a report of this error to a unipops guru ',
     .              ' ... sddopen err no ', irtn
               endif
            endif
	  else
            write(istderr, 1)
     .		'CHNGFILE: Cannot create the desired file type.'
	    goto 99
	  endif
c
      else
	write(istderr,1) 'CHNGFILE: Unrecognized command.'
	goto 99
      endif
c 
      chngfile = 0
c
99    return
c
 1    format(1x, 10a)
 2    format(1x,2a,i)
C
      end	
