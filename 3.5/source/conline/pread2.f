      SUBROUTINE PREAD2 (prompt, stch, nb)
C-------------------------------------------------------------------------------
C  @(#)pread2.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C	 PREAD2 reads input text and unpacks the buffer one byte per
C     word.  The buffer	is assumed to be 160 characters.  The
C     buffer is	padded with blanks.
C
c  stch = output buffer (packed)
c  prompt = prompt string
c  nb = number of bytes in output buffer
c
c              8903   [RJM] See CHANGES.DOC
C-----------------------------------------------------------------------
c
      character*(*) stch
      character*160 stch2, prompt2
      character*64 cbatchin
      character*(*) prompt
      integer*2 nb, i1, ierr, ione, istat, ilen3, lastblnk, ilenp
      integer*2 n104, n0
      integer*4 len, long, istat4, ioput, lunit, iptr
      logical*2 padme
c
      INCLUDE 'cio.inc'
c
      data n104, n0 /104, 0/
c
      ilenp = lastblnk(prompt)
      prompt2 = prompt
c
9     stch2 = ' '
      i1 = 1
c     i1 = the starting character position to be read in in case of
c     multiple line input
c
1     if (iintype .eq. istdin .or. iintype .eq. imenuin) then
	write(istdout, 10) prompt2(1:ilenp)
10	format(1x,a,$)
	call flush(long(istdout))
	if (iintype .eq. istdin) then
	   read(iinunit,2000,iostat=ierr) stch2(i1:)
2000       format(a)
	else
	   call readfifo(long(iinunit), istat4, stch2(i1:))
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
           do 100 iptr = i1, 160
              if (.not. padme) then
                 if (ichar(stch2(iptr:iptr)) .eq. 0) then
                    padme = .true.
                 endif
              endif
              if (padme) stch2(iptr:iptr) = ' '
 100       continue
c
	endif
c       Generates screen prompt and reads screen if input unit is istdin (the
c	terminal)
c
      else if (iintype .ne. 0) then
	 read(iinunit,2000,iostat=ierr) stch2(i1:)
c        Reads from any other device
c
      else if (iintype .eq. 0) then
        iinptr = max(1, iinptr - 1)
        iintype = iinlist(iinptr, 1)
        iinunit = iinlist(iinptr, 2)
        stch2 = stch
        ierr = 0
c       Takes care of when iintype = 0; that is, when we are using the 
c	! or !# history command which uses an entry from HIST to regenerate
c       CBUFF
c
      endif
c
      if (ierr .ne. 0) then
	 if (iintype .eq. ibatchin) then
                cbatchin = cinlist(iinptr)
		write(istderr,*) 'End of batch file ', 
     .			cbatchin(1:lastblnk(cbatchin))
	        close(iinunit,iostat=istat)
                lunit = iinunit
                lunit = ioput(lunit)
c			ignore ioput errors, we don't really care
	 else if(iintype .eq. isetupin) then	
		write(istderr,*) 'End of SETUP file'
	        close(iinunit,iostat=istat)
	 else if(iintype .eq. istdin) then
      		write(istderr,*) 'END of input file or ERROR on read'
      		write(istderr,*) 'TERMINATING....'
      		call exitpops(ione)
	 else if(iintype .eq. imenuin) then
      		write(istderr,*) 'Problem with MENUS... Returning'
      		write(istderr,*) 'control to standard window'
		call donemenu
	 endif
      	 write(istderr,*) ' '
         iinptr = max(1, iinptr - 1)
	 iinunit = iinlist(iinptr, 1)
         iintype = iinlist(iinptr, 2)
	 stch = ' '
	 ilen3 = 0
c
      else
c
	 ilen3 = lastblnk(stch2)
      	 call updateline(prompt2(1:ilenp)//stch2(i1:ilen3))

	 if (ilen3 .gt. len(stch) ) then
		call oerror(n104, n0, 'Line Ignored')
		stch = ' '
		ilen3 = 0
		i1 = 1
		prompt2 = prompt
		goto 9
c		Check whether input line exceeds KARLIM length
c
	 else if (stch2(ilen3:ilen3) .eq. '\\') then
	   	i1 = ilen3
		prompt2 = '  '
	    	goto 1
	 else
	    	stch = stch2(1:ilen3)
	 endif
c        Was a continuation character entered as a last character?
c	
	 if (iintype .ne. istdin .and. iintype .ne. imenuin) then
                write(iout,*) prompt2,stch(1:ilen3)
		call flush(long(iout))
	 else if (iintype .eq. imenuin) then
                write(iout,*) stch(1:ilen3)
		call flush(long(iout))
	 endif
c
      endif
c
      nb = min(ilen3+1, karlim)
c
      RETURN
c
      END
