      program manonepage
c
c     @(#)manonepage.f	5.1 06/22/94
c
c     Creates manual pages for a specified topic
c
c     Syntax:  info_file  topic   pp_def  [pagesize]
c
c     PAGESIZE = number of lines on a page (default = 60)
c     INFOFILE = explain's information file
c     TOPIC = explain topic which is to be printed, along with its
c	adjacent neighbors
c     PP_DEF = preprocessor def's to use
c
      character*64 infofile, topic, ppdef
      character*80 string, stack(4096)
      integer*4 iargc, findminmatch, lnblnk, loc, long, top, ierr,
     .		MAXSTCK, i, pagesize
      integer*2 ISTDOUT, ISTDERR, IOUNIT
      logical preproc, resetproc
c
      parameter (ISTDOUT = 6)
      parameter (ISTDERR = 0)
      parameter (IOUNIT =11)
c     I/O unit numbers
c
      parameter (MAXSTCK = 4096)
c     Maximum size of STACK and TOPICLIST
c
      include 'explain.inc'
c
      open(unit=ISTDERR, form="print")
      open(unit=ISTDOUT, form="print")
c
      if (iargc() .lt. 3) then
	write(ISTDERR,*) 'Syntax: info_file  topic pp_def'
	call exit(long(1))
      endif
c
      call getarg(long(1), infofile)
      call getarg(long(2), topic)
      call getarg(long(3), ppdef)
      if (iargc() .ge. 4) then
	call getarg(long(4), string)
	read(string,iostat=ierr,fmt='(i10)') pagesize
	if (ierr .ne. 0 .or. pagesize .le. 0) then
	   write(ISTDERR,*) 'Syntax: info_file  topic  pp_def [pagesize]'
	   call exit(long(1))
        endif
      else
	pagesize = 60
      endif
c     Check and get arguments
c
      call readinfo(infofile)
c
      loc = findminmatch(nodes, next, start, topic, ISTDERR)
c
      if (loc .eq. NULL) then
	  write(ISTDERR,*) 'No topic with the name: ', topic(1:lnblnk(topic))
	  call exit(long(1))
      endif
c     Checks that the desired topic exists
c
      top = 0
c     TOP = number of character strings placed on the stack
c
      open(unit=IOUNIT, file=filenames(loc),
     .	       status='old',form='formatted',iostat=ierr)
      if (ierr .ne. 0) then
		call pushchar(stack, top, MAXSTCK, 'Cannot find explain file')
		call pushchar(stack, top, MAXSTCK, ' ')
      else
		rewind(IOUNIT,iostat=ierr)
c
	        resetproc = .true.
20		read(IOUNIT,iostat=ierr,fmt='(a)') string
		if (ierr .eq. 0) then
		   if (preproc(string,ppdef,resetproc)  ) then
c		   Pass the string through the preproccessor -- the pre-
c			proccessor should be reset for each file read in.
c
			if (string(1:1) .eq. char(12)) then
 			    do 21 i = 1, pagesize - mod(top, pagesize)
			        call pushchar(stack, top, MAXSTCK, ' ')
21				continue
c			    We have encountered a line-feed -- skip lines to
c			    start of next page.
			else
    		            call pushchar(stack, top, MAXSTCK, string)
			endif
		   endif
		   goto 20
		endif
		close(IOUNIT,iostat=ierr)
c		Read until end of file; place text onto stack
c
      endif
c
      write(ISTDOUT,fmt='(1x,a)') 
     .		(stack(i)(1:lnblnk(stack(i))), i = 1, top)
c     List out the contents of the stack
c
c
      call exit(long(0))
c
      end
c
