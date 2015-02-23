      program cube
c
c     @(#)cube.f	5.3 03/01/99
c
      integer ierr, irtn, getenv, lnblnk, ilen, jold, iargc
      logical exs, position, readindex, readscan, headline, toilet2
      character*3 dir
      character*1 answr
      character*70 filename1, filename2, input
      real first, last, ffirst, flast
c
      include 'cube.inc'
      include 'dform.inc'
c                   look for undocumented -y2k argument
      if (iargc() .eq. 1) then
         call getarg(1, input)
         if (input .eq. "-y2k") then
            irtn = 1
            call datestyle(irtn)
         endif
      endif
c
      open(unit=6, form="print")
      open(unit=0, form="print")
c
      write(6,*) ' '
      write(6,*) 'Program for creating a new CUBE file or updating an',
     .           ' existing CUBE file.'
      write(6,*) 'Input is a file of scans in the SDD#1 format and the',
     .	         ' output is a 3-d Fits '
      write(6,*) 'Image file which can be read into AIPS, IRAF, etc.'
      write(6,*) ' '
      write(6,*) 'The final cube can have a maximum of 47 x 10^6 pixels'
      write(6,*) ' '
c
91    write(6,*) 'Do you want debugging on?'
      write(6,101) 'Y or N'
101   format(1x,a,' -> ',$)
      read(5,110) answr
110   format(a)
      write(6,*) ' '
      if (answr .eq. 'y' .or. answr .eq. 'Y') then
	debug = .true.
      else if (answr .eq. 'n' .or. answr .eq. 'N') then
	debug = .false.
      else
	write(6,*) 'Bad answer... Try again...'
	goto 91
      endif
      write(6,*) ' '
c
      irtn = getenv('popsdir', headerfile)
      ilen = lnblnk(headerfile)
      if (irtn .ne. 0 .or. ilen .eq. 0 .or. ilen .gt. 50) then
	write(0,*) 'Environmental variable POPSDIR is improperly set'
	goto 99
      endif
      headerfile = headerfile(1:ilen) // 'utilities/head.mask'
c     Find out where FITS template is found.
c
      write(6,101) 'Enter name of the SDD file (70 chars. max.)'
      read(5,110) filename1
      write(6,*) ' '
c
      if (.not. readindex(filename1, indev, inreclngth, jrec)) goto 99
c
      jold = jrec
      if (.not. readscan(indev, inreclngth, jrec, (HDU_FLOAT_SIZE*4), 
     .     itwh)) goto 99
      jrec = jold
      call adjustclass
c     Read in the first scan so that we can be sure that file is readable,
c     that a scan exists in the file, and for possible later use.  Make sure
c     that when we get to the meat of the program that we again read in the 
c     first scan (i.e., we must reset JREC back to its original value).	
c
      write(6,101) 'Enter name of the output CUBE file (70 chars. max.)'
      read(5,110) filename2
      write(6,*) ' '
c
      inquire (file=filename2,exist=exs,direct=dir)
      if (exs .and. (dir .eq. 'NO' .or. dir .eq. 'no') ) then
        write(0,*) 'File exist, but wrong format...'
        goto 99
      endif
c
      open (unit=ioutdev,file=filename2,status='unknown',
     1	    access='direct',form='unformatted',recl=outreclngth,
     2      iostat=ierr)
      if (ierr .ne. 0) then
	write(0,*) 'Cannot open....'
	goto 99
      endif
c
      if (exs) then
c
        write(6,*) 'CUBE file already exists.'
      	write(6,*) ' '
	if (.not. toilet2()) goto 99
c
      else
c
	call setuphead(filename2)
c       Initializes header parameters; uses scan read in above.
c
      	call init  
      	if (.not. headline()) goto 99
c	Initializes the cube with bad data points.  Writes out header
c
      endif
c
3     write(6,*) 'What scans do you want to process (enter zeros for',
     .		 ' all scans)?'
309   write(6,101) 'Enter lowest scan number'
      read(5,110) input
      read(input,*,iostat=ierr) first
      write(6,*) ' '
      if (ierr .gt. 0) then
	write(6,*) 'Bad entry... Try again...'
	goto 309
      endif
31    write(6,101) 'Enter highest scan number'
      read(5,110) input
      read(input,*,iostat=ierr) last
      write(6,*) ' '
      if (ierr .gt. 0) then
	write(6,*) 'Bad entry... Try again...'
	goto 31
      endif
      if (first .eq. 0. .and. last .eq. 0) then
	last = 1.e10
	first = -1.e10
      else if (first .gt. last) then
	write(6,*) 'Bad entries.... Try again...'
	goto 3
      endif
c
4     write(6,*) 'What feeds do you want to process (enter zeros for',
     .		 ' all feeds)?'
409   write(6,101) 'Enter lowest feed number'
      read(5,110) input
      read(input,*,iostat=ierr) ffirst
      write(6,*) ' '
      if (ierr .gt. 0) then
	write(6,*) 'Bad entry... Try again...'
	goto 409
      endif
41    write(6,101) 'Enter highest feed number'
      read(5,110) input
      read(input,*,iostat=ierr) flast
      write(6,*) ' '
      if (ierr .gt. 0) then
	write(6,*) 'Bad entry... Try again...'
	goto 41
      endif
      if (ffirst .eq. 0. .and. flast .eq. 0) then
	flast = 1.e10
	ffirst = -1.e10
      else if (ffirst .gt. flast) then
	write(6,*) 'Bad entries.... Try again...'
	goto 4
      endif
c
5     if (readscan(indev, inreclngth, jrec, HDU_FLOAT_SIZE*4, itwh)) 
     .     then
c
        call adjustclass	
        write(6,51) '------- Scan no. = ',dtwh(c1sno),' --------'
51	format(1x,a,1pg16.9,a)
c
        if (dtwh(c7osn) .lt. 0.) then
	   write(6,*) 'Scan has been labelled as bad.... Skipping....'
c
	else if (dtwh(c1sno) .lt. first .or. dtwh(c1sno) .ge. (last+1)) then
	   write(6,*) 'Scan number out of range... Skipping....'
c
	else if (nint(100.*mod(dtwh(c1sno),1.d00)) .lt. ffirst .or.
     .	         nint(100.*mod(dtwh(c1sno),1.d00)) .gt. flast) then
	   write(6,*) 'Feed number out of range... Skipping....'
c
	else if (isys .le. 14 .and. ctwh(c4csc).ne.testcoord(isys)) then
	   write(6,*) 'Data not taken in ', testcoord(isys),'... Skipping...'
c
	else if (ctwh(c1stc)(1:4) .ne. 'LINE') then
	   write(6,*) 'Not a spectral-line observation... Skipping....'
c
	else
c
           if (position() ) call velocity
c
        endif
        go to 5
c
      endif
c
99    write(6,*) ' '
      write(6,*) 'Terminating...'
c
      close (ioutdev)
      close (indev)
c
      stop
c
      end
c
