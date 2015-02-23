      SUBROUTINE PREAD (KB)
C-------------------------------------------------------------------------------
C  @(#)pread.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C	 PREAD reads input text	and unpacks the	buffer one byte	per
C     word.  The buffer	is assumed to be 160 characters.  The
C     buffer is	padded with blanks.
C
C-----------------------------------------------------------------------
c
      integer*2 kb(*), i1, ierr, istat, ilen3, lastblnk, iwpc
      integer*2 n104, n0
      integer*2 ilenp
      integer*4 ione, long, istat4, ioput, lunit, iptr
      logical*2 padme
      character*200 cbuff2
      character*1 promptchar
      character*8 promptused
      character*64 cbatchin
c
      INCLUDE 'cio.inc'
      include 'core.inc'
      include 'stk.inc'
c
      data ione /1/
c
      data n104, n0 /140, 0/
c
      promptchar = cpt(1:1)
c
9     cbuff2 = ' '
      i1 = 1
c     i1 = the starting character position to be read in in case of
c     multiple line input
c
1     if (iintype .eq. istdin .or. iintype .eq. imenuin) then
c
c		use the value of userprompt in core for the promt if
c               promptchar == '>' (standard pops prompt)
c		otherwise use promptchar.  Also, if prompt is all
c		blanks, use promptchar.
c
	promptused = promptchar
        if (promptchar .eq. '>' .and. cuserprompt .ne. ' ')
     .				promptused = cuserprompt
c
	ilenp = lastblnk(promptused)
c
        write(istdout, 10) promptused(1:ilenp)
 10     format(1x,a,$)
	call flush(long(istdout))
	if (iintype .eq. istdin) then
	   read(iinunit,2000,iostat=ierr) cbuff2(i1:)
2000       format(a)
	else
	   call readfifo(long(iinunit), istat4, cbuff2(i1:))
	   if (istat4 .lt. 0) then
		ierr = istat4
	   else
		ierr = 0
	   endif
c			readfifo is a c routine that ALWAYS will return
c			a null terminated string, fortran, however,
c			likes strings that are padded with blanks
c			so ... look for null string and pad from there
c			to end
           padme = .false.
           do 100 iptr = i1, 200
              if (.not. padme) then
                 if (ichar(cbuff2(iptr:iptr)) .eq. 0) then
                    padme = .true.
                 endif
              endif
              if (padme) cbuff2(iptr:iptr) = ' '
 100       continue
c
	endif
c       Generates screen prompt and reads screen if input unit is istdin (the
c	terminal) or reads from menus FIFO
c
      else 
	 read(iinunit,2000,iostat=ierr) cbuff2(i1:)
c        Reads from any other device
c
      endif
c
      if (ierr .ne. 0) then
      	 if (.not. lsetup) write(istderr,*) ' '
	 if (iintype .eq. ibatchin) then
                cbatchin = cinlist(iinptr)
		if (.not. lsetup) write(istderr,*) 'End of batch file ', 
     .			cbatchin(1:lastblnk(cbatchin))
	        close(iinunit,iostat=istat)
                lunit = iinunit
                lunit = ioput(lunit)
c			ignore ioput errors, we don't really care
	 else if(iintype .eq. isetupin) then	
		write(istderr,*) 'End of SETUP file'
	        close(iinunit,iostat=istat)
                lsetup = .false.
	 else if(iintype .eq. istdin) then
      		write(istderr,*) 'END of input file or ERROR on read'
      		write(istderr,*) 'TERMINATING....'
      		call exitpops(ione)
	 else if(iintype .eq. imenuin) then
      		write(istderr,*) 'Problem with MENUS... Returning'
      		write(istderr,*) 'control to standard window'
		call donemenu
	 endif
      	 if (.not. lsetup) write(istderr,*) ' '
         iinptr = max(1, iinptr - 1)
         iinunit = iinlist(iinptr, 1)
         iintype = iinlist(iinptr, 2)
	 cbuff = ' '
	 lastline = ' '
	 ilen3 = 0
c
      else
c
	 ilen3 = lastblnk(cbuff2)
      	 if (.not. lsetup)
     .       call updateline(promptused(1:ilenp)//cbuff2(i1:ilen3))

	 if (ilen3 .eq. i1-1) then
		goto 1
c               Skip blank lines
C				
	 else if (cbuff2(ilen3:ilen3) .eq. '\\') then
	   	i1 = ilen3
		promptchar = ' '
	    	goto 1
	 else
	    	cbuff = cbuff2(1:ilen3)
	    	lastline = cbuff
	 endif
c        Was a continuation character entered as a last character?
c
	 if (ilen3 .gt. karlim) then
		call oerror(n104, n0, 'Line ignored')
		cbuff = ' '
		ilen3 = 0
		i1 = 1
		promptchar = cpt(1:1)
		goto 9
c		Check whether input line exceeds KARLIM length
	 endif
c
         if (iintype .ne. ishelpin) then
	    if (mode .eq. 0 .and. 
     .		(iintype .eq. istdin .or. iintype .eq. isetupin) ) 
     .			call updatehist(cbuff2(1:ilen3))
	    if (iintype .ne. istdin .and. iintype .ne. imenuin .and.
     .          .not. lsetup) then
                write(iout,*) promptchar,cbuff2(1:ilen3)
		call flush(long(iout))
	    else if (iintype .eq. imenuin) then
                write(iout,*) cbuff2(1:ilen3)
		call flush(long(iout))
	    endif
         endif
c
      endif
c
      nbytes = min(ilen3+1, karlim)
      CALL UNPACK (iwpc(nbytes),JBUFF,KB)
c
      RETURN
c
      END
