      subroutine recover(iret)
C-------------------------------------------------------------------------------
C  @(#)recover.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Initializes files, locks, K-array, etc. from a disk RECOVER file
c     or initializes K-array from MEMORY file.
c
c     iret = 0 if no recover happened
c                1 if a recover happened
c
      character*1 answr
      real*4 recversion
      logical*2 inquirefile, onlinetmp, memcreate
      integer*2 igtmp, iptmp, istat, ierr, inlinetmp, igintmp, 
     .		igouttmp, imintmp, iouttmp, ffiletmp(10,4),
     .		iinlisttmp(50,2), iinptrtmp, iinunittmp, iintypetmp
      logical*2 lsetuptmp
      character*8  oobstmp, sitetmp, termtmp, proctmp
      character*64 dirprefixtmp(10), ccubetmp, dirtmp, cfiletmp(10),
     .			cinlisttmp(50)
      character*16 couttxttmp, coutgrphtmp, cmenuintmp, cmenuouttmp, 
     .		   cgraphintmp, cgraphouttmp
      character*72 str1, str2
      integer*4 numcio2, numappl2, numlsf2, numcore2, nummax, err7, 
     1	        access, long, iloc, long, ii, numkx,
     2		maxcio, i4, iret, chmod
      integer*2 n1, lastblnk, memory(8)
      real memversion
      logical rcvrerr
c
      include 'cio.inc'
      include 'appl.inc'
      include 'core.inc'
      include 'lsf.inc'
c
      equivalence (cmemory, memory(1))
      equivalence (memversion, kx(13) )
c     13 is the location in KX of VERSION adverb.
c
      data nummax/32766/, err7/7/, maxcio/5800/
      data n1 /1/
c
      iret = 0
c
101   if (.not. inquirefile(cmemory)) then
	write(istderr,*) ' '
	write(istderr,*) '***ERROR***  Memory file does not exist!!'
	if (.not. memcreate()) goto 110
	goto 101
      endif
      if (access(cmemory,'rw').ne.0 .and. chmod(cmemory,'rw').ne.0) then
	write(istderr,*) ' '
	write(istderr,*) '***ERROR***  Memory file has wrong access ',
     .		'privileges!!'
	if (.not. memcreate()) goto 110
	goto 101
      endif
c     Checks for existence of MEMORY file (Needed even if RECOVERing).
c
C					Read in	K & LISTF arrays.
      call openms(imemory, memory, n1, istat)
      ILOC = 1
      call readms(imemory, iloc, long(kblk), k, ierr)
      ILOC = iloc + KBLK
      if (ierr .ge. 0) call readms(imemory, iloc, long(kxblk), kx, ierr)
      ILOC = iloc + KxBLK
      if (ierr .ge. 0) call readms(imemory,iloc,long(lblk),listf,ierr)
      IF (IERR.LT.0) then
	write(istderr,*) ' '
	write(istderr,*) '***ERROR***  Memory file has an internal problem!!'
	if (.not. memcreate()) goto 110
	goto 101
      endif
      call closms(imemory, istat)
c
      if (version .ge. 1.0) then
         if (int(memversion) .ne. int(version)) then
	    write(istderr,*) ' '
            write(istderr,*) 'ERROR: Memory file and program ',
     1      	             'are not compatable!!'
	    write(istderr,*) 'Versions: ', memversion, version
	    if (.not. memcreate()) goto 110
	    goto 101
        endif
      else
         if (memversion .lt. 1.0) memversion = memversion*1000.0
         version = version * 1000.0
	 if (int(memversion*100.0) .ne. int(version*100.0)) then
	    write(istderr,*) ' '
	    write(istderr,*) 'ERROR: Memory file and program ',
     1  	              'are not compatable!!'
	    write(istderr,*) 'Versions: ', memversion, version
            version = version / 1000.0
	    if (.not. memcreate()) goto 110
	    goto 101
         endif
         version = version / 1000.0
      endif
c
c     Checks that the MEMORY file is up to date with the executble code.
c
c---------------------------------
c
c
      if (inquirefile(crecover)) then
	str1 = " "
	str2 = " "
	rcvrerr = .false.
c
	if (access(crecover,'rw').ne.0 .and. chmod(crecover,'rw') .ne. 0) then
	   write(str1,34) 'Recover file has wrong access privelages!!'
	   rcvrerr = .true.
	   goto 120
	endif
c
	rcvrerr = .true.
	str1 = "ERROR: Problem in opening or reading recover file"
	open(unit=irecover,file=crecover,status='old',
     1       form='unformatted',access='sequential',err=120)
	rewind(irecover,err=120)
	read(irecover,err=120,end=120) recversion, numcio2, numappl2, 
     1     			         numcore2, numkx, numlsf2
	rcvrerr = .false.
c
        if (version .ge. 1) then
	     if (int(recversion) .ne. int(version)) then
	        write(str1,34) 'ERROR: Recovery file and program ',
     1  	  	         'are not compatable!!'
34		format(10a)
	        write(str2,351) 'Versions: ', recversion, version
351		format(a, f10.2, f10.2)
	   	rcvrerr = .true.
		goto 120
	     endif
        else
             if (recversion .lt. 1) recversion = recversion*1000.0
             version = version*1000.0
	     if (int(recversion*100.0) .ne. int(version*100.0)) then
	        write(str1,34) 'ERROR: Recovery file and program ',
     1  	  	         'are not compatable!!'
	        write(str2,351) 'Versions: ', recversion, version
	   	rcvrerr = .true.
                version = version / 1000.0
		goto 120
	     endif
             version = version / 1000.0
	endif
c
        if (numcio2 .ne. numcio .or. numcio .gt. maxcio .or. 
     1        numcio2 .gt. maxcio) then
	     write(str1,352) 'ERROR: Bad recovery file (NUMCIO):',
     1		numcio2, numcio
352	     format(a, i7, i7)
	     rcvrerr = .true.
	     goto 120
	 else if (numappl2 .ne. numappl .or. numappl .gt. RAPPLSIZE .or.
     1        numappl2 .gt. RAPPLSIZE) then
	     write(str1,352) 'ERROR: Bad recovery file (NUMAPPL):',
     1	  	numappl2, numappl
	     rcvrerr = .true.
	     goto 120
	else if (numcore2 .ne. nummax .or. nummax .gt. 32766 .or.
     1        numcore2 .gt. 32766) then
	     write(str1,352) 'ERROR: Bad recovery file (NUMCORE):',
     1		numcore2, nummax
	     rcvrerr = .true.
	     goto 120
	else if (numkx .ne. nummax .or. nummax .gt. 32766 .or.
     1        numkx .gt. 32766) then
	     write(str1,352) 'ERROR: Bad recovery file (NUMKX):',
     1		numkx, nummax
	     rcvrerr = .true.
	     goto 120
	else if (numlsf2 .ne. nummax .or. nummax .gt. 32766 .or.
     1        numlsf2 .gt. 32766) then
	     write(str1,352) 'ERROR: Bad recovery file (NUMLSF):',
     1		numlsf2, nummax
	     rcvrerr = .true.
	     goto 120
	endif
c	Check that all info in RECOVER file is correct
c
120     if (rcvrerr) then
	   write(istderr,*) ' '
	   if (str1 .ne. ' ') write(istderr,*) str1(1:lastblnk(str1))
	   if (str2 .ne. ' ') write(istderr,*) str2(1:lastblnk(str2))
	   write(istderr,*) 'No attempt will be made to recover!'
	   write(istderr,*) ' '
	else
	   write(istdout,*) ' '
	    write(istdout,*) 'Do you want to recover your environment ', 
     .			 ' from the last time you ran'
      	   write(istdout,20) 'the program? (y or n)? [ default : n ]: '
20         format(1x,a,$)
	   call flush(long(istdout))
           read(istdin,30) answr
30         format(a)
           write(istdout,*) ' '
c
           if (answr .eq. 'y' .or. answr .eq. 'Y') then
	     igtmp = igraphtype
	     igintmp = igraphin
	     igouttmp = igraphout
	     imintmp = imenuin
	     iouttmp = iout
	     couttxttmp = couttxt
	     coutgrphtmp = coutgrph
	     cmenuintmp = cmenuin
	     cmenuouttmp = cmenuout
	     cgraphintmp = cgraphin
	     cgraphouttmp = cgraphout
	     ccubetmp = ccube
	     iptmp = iprinttype
	     oobstmp = oobs
	     sitetmp = site
	     dirtmp = dirname
	     termtmp = termtype
	     proctmp = procid
	     onlinetmp = online
             inlinetmp = inline
	     lsetuptmp = lsetup
             do 35 ii = 1, 10
                dirprefixtmp(ii) = dirprefix(ii)
		ffiletmp(ii,1) = ffile(ii,1)
		ffiletmp(ii,2) = ffile(ii,2)
		ffiletmp(ii,3) = ffile(ii,3)
		ffiletmp(ii,4) = ffile(ii,4)
	        cfiletmp(ii) = cfile(ii)
 35          continue
             do 3509 ii = 1, 50
	        iinlisttmp(ii, 1) = iinlist(ii, 1)
	        iinlisttmp(ii, 2) = iinlist(ii, 2)
		cinlisttmp(ii) = cinlist(ii)
3509		continue
	     iinptrtmp = iinptr
	     iinunittmp = iinunit
	     iintypetmp = iintype
c            Temporarily store those items in CIO that are NOT to be overwritten
c	     by RECOVER.
c
             read(irecover,err=100,end=100) (Rio(i4),i4=1,numcio)
	     read(irecover,err=100,end=100) (Rappl(i4),i4=1,numappl)
	     read(irecover,err=100,end=100) (k(i4),i4=1,nummax)
	     read(irecover,err=100,end=100) (kx(i4),i4=1,nummax)
	     read(irecover,err=100,end=100) (listf(i4),i4=1,nummax)
             if (vers .lt. 1) vers = vers * 1000.0
	     close(irecover)
c	     Read in arrays from RECOVER file.
c	
             iret = 1
             igraphtype = igtmp 
	     igraphin = igintmp
	     igraphout = igouttmp
	     imenuin = imintmp
	     iout = iouttmp
	     couttxt = couttxttmp 
	     coutgrph = coutgrphtmp
	     cmenuin = cmenuintmp 
	     cmenuout = cmenuouttmp 
	     cgraphin = cgraphintmp 
	     cgraphout = cgraphouttmp
	     ccube = ccubetmp
             iprinttype = sign(iptmp, iprinttype)
             oobs = oobstmp
             site = sitetmp
             dirname = dirtmp
             termtype = termtmp
             procid = proctmp
	     online = onlinetmp
             inline = inlinetmp
	     lsetup = lsetuptmp
             do 36 ii = 1, 10
                dirprefix(ii) = dirprefixtmp(ii)
		ffile(ii,1) = ffiletmp(ii,1)
		ffile(ii,2) = ffiletmp(ii,2)
		ffile(ii,3) = ffiletmp(ii,3)
		ffile(ii,4) = ffiletmp(ii,4)
	        cfile(ii) = cfiletmp(ii)
 36          continue
             do 361 ii = 1, 50
	        iinlist(ii, 1) = iinlisttmp(ii, 1)
	        iinlist(ii, 2) = iinlisttmp(ii, 2)
		cinlist(ii) = cinlisttmp(ii)
361		continue
	     iinptr = iinptrtmp
	     iinunit = iinunittmp 
	     iintype =  iintypetmp
c            If all goes well, then put back temp. values into CIO.
c
	   else if (answr .ne. 'n' .and. answr .ne. 'N' .and. 
     .		   answr .ne. ' ') then
	     goto 120
c
	   endif
c
        endif 
      endif
c			set prompt if not already set
      if (lastblnk(cuserprompt) .le. 0) then
         if (program(1:1) .eq. 'L') then
            cuserprompt = 'Line >'
         else if (program(1:1) .eq. 'C') then
            cuserprompt = 'Condar >'
         endif
      endif
c     Also, if by some bizare chain of events
c     program is not L or C, prompt stays the same
c
      return
c------------------------------
c
100   igraphtype = igtmp 
      igraphin = igintmp
      igraphout = igouttmp
      imenuin = imintmp
      iout = iouttmp
      couttxt = couttxttmp 
      coutgrph = coutgrphtmp
      cmenuin = cmenuintmp 
      cmenuout = cmenuouttmp 
      cgraphin = cgraphintmp 
      cgraphout = cgraphouttmp
      ccube = ccubetmp
      iprinttype = sign(iptmp, iprinttype)
      oobs = oobstmp
      site = sitetmp
      dirname = dirtmp
      termtype = termtmp
      procid = proctmp
      online = onlinetmp
      inline = inlinetmp
      lsetup = lsetuptmp
      do 105 ii = 1, 10
         dirprefix(ii) = dirprefixtmp(ii)
	 ffile(ii,1) = ffiletmp(ii,1)
	 ffile(ii,2) = ffiletmp(ii,2)
	 ffile(ii,3) = ffiletmp(ii,3)
	 ffile(ii,4) = ffiletmp(ii,4)
	 cfile(ii) = cfiletmp(ii)
 105  continue
      do 1051 ii = 1, 50
	iinlist(ii, 1) = iinlisttmp(ii, 1)
	iinlist(ii, 2) = iinlisttmp(ii, 2)
	cinlist(ii) = cinlisttmp(ii)
1051	continue
      iinptr = iinptrtmp
      iinunit = iinunittmp 
      iintype =  iintypetmp
      call exitpops(err7)
c     Error in RECOVER file so go to EXITPOPS... Do some cleanup before hand.
c
110   call exitpops(err7)
c     Exit if the memory file has a problem
c
      end
c
c---------------------------------------
c
      logical*2 function memcreate()
c
c     Memory file error trap and recovery
c
      include 'cio.inc'
c
      character*1 answr
      integer*4 system, unlink, long
      logical*2 inquirefile
c
      memcreate = .false.
c
1     write(istdout,*) ' '
      write(istdout,5) 'Do you want to recreate your memory file '
      write(istdout,5) '(y or n)?  [ default : n ]: '
5     format(1x,a,$)
      call flush(long(istdout))
      read(istdin,6) answr
6     format(a)
c
      if (answr .eq. ' ' .or. answr .eq. 'n' .or. answr .eq. 'N') then
	memcreate = .false.
      else if (answr .eq. 'y' .or. answr .eq. 'Y') then
	if (inquirefile(cmemory) .and. unlink(cmemory) .ne. 0) then
      	   write(istderr,5) ' '
	   write(istderr,*) "Cannot delete old memory file and cannot ",
     .			"continue!!"
	   write(istderr,*) "Get help from your local UniPops guru!"
	   memcreate = .false.
	else if (system("makefile.exe " // cmemory) .ne. 0) then
      	   write(istderr,5) ' '
	   write(istderr,*) "Cannot create new memory file and cannot ",
     .			"continue!"
	   write(istderr,*) "Get help from your local UniPops guru!"
	   memcreate = .false.
	else
	   memcreate = .true.
	endif
      else
	goto 1
      endif
c
      return
      end
