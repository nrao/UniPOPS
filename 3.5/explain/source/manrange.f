      program manrange
c
c     @(#)manrange.f	5.2 08/31/94
c
c     Creates manual pages starting at a specified topic between two other
c	sub topics
c
c     Syntax:  info_file  topic  pp_def first last [pagesize]
c
c     PAGESIZE = number of lines on a page (default = 60)
c     INFOFILE = explain's information file
c     TOPIC = explain topic which is to be printed, along with its
c	adjacent neighbors
c     FIRST = character string indicating where to start in subtopic
c	list.
c     LAST = character string indicating where to stop; FIRST must be lex. <=
c	LAST.
c     PP_DEF = preprocessor def's to use
c
      character*64 infofile, topic, topiclist(1024), ppdef, first, last
      character*80 string, stack(4096)
      integer*4 iargc, ierr, place, pagesize, top, findminmatch, lnblnk,
     .		loc, MAXSTCK, locother, i, ptr, numedges, available, 
     .		docsize, NFIELD, BORDER, lnblnk, numtopics, k, leftover,
     .		nextplace, skip, findchar, long, i1, BOTBORDER, i2
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
      parameter (NFIELD = 4)
      parameter (BORDER = 10)
      parameter (BOTBORDER = 2)
c     NFIELD = Number of adjacent neighbors printed by LISTEDGES per output line.
c     BORDER = Number of output lines at the start of a page in which a
c	listing cannot stop in.
c     BOTBORDER = Number of blank lines after each item.
c
      include 'explain.inc'
c
      open(unit=ISTDOUT, form="print")
      open(unit=ISTDERR, form="print")
c
      if (iargc() .lt. 5) then
	write(ISTDERR,*) 'Syntax:info_file topic pp_def first last [pagesize]'
	call exit(long(1))
      endif
c
      call getarg(long(1), infofile)
      call getarg(long(2), topic)
      call getarg(long(3), ppdef)
      call getarg(long(4), first)
      call getarg(long(5), last)
      if (llt(last,first)) then
	write(ISTDERR,*) 'Syntax:info_file topic pp_def first last [pagesize]'
	call exit(long(1))
      endif
      if (iargc() .ge. 6) then
	call getarg(long(6), string)
	read(string,iostat=ierr,fmt='(i10)') pagesize
	if (ierr .ne. 0 .or. pagesize .le. 0) then
	   write(ISTDERR,*) 'Syntax:info_file topic pp_def first last [pagesize]'
	   call exit(long(1))
        endif
      else
	pagesize = 60
      endif
c     Check and get arguments
c
      call readinfo(infofile)
c
      place = 0
c     PLACE = location of start of next output on the page
c
      loc = findminmatch(nodes, next, start, topic, ISTDERR)
      if (loc .eq. NULL) then
	  write(ISTDERR,*) 'No topic with the name: ', topic(1:lnblnk(topic))
	  call exit(long(1))
      endif
c     Checks that the desired topic exists
c
      numtopics = 0
      call pushchar(topiclist, numtopics, MAXSTCK, nodes(loc))
c     TOPICLIST will be the list of topics.
c     NUMTOPICS = number of topics in TOPICLIST
c
      ptr = adj(loc)
7     if (ptr .ne. NULL) then
        if (dest(ptr) .lt. 0) call pushchar(topiclist, 
     .			numtopics, MAXSTCK, nodes(abs(dest(ptr))) )
c	Only add adjacent topic to the list if it is a HARD adjacency.
c
	ptr = link(ptr)
	goto 7
      endif
      call heapsort(topiclist(2), numtopics-1)
c     Don't sort in the top topic;      
c
      do 100 k = 1, numtopics
c
	top = 0
c       TOP = number of character strings placed on the stack
c
        loc = findchar(nodes, next, start, topiclist(k) )
c
        if (loc .eq. NULL) then
	  write(ISTDERR,*) 'No topic with the name: ', topic(1:lnblnk(topic))
	  call exit(long(1))
        endif
c       Checks that the desired topic exists
c
	if (k .eq. 1 .and. first .ne. '.') goto 100
	if (llt(nodes(loc),first)) goto 100
	if (k .ne. 1 .and. lgt(nodes(loc),last)) goto 101
c
        call pushchar(stack, top, MAXSTCK, '        ' // nodes(loc))
        call pushchar(stack, top, MAXSTCK, ' ')
c       Pushes the help file title onto the stack first
c
	do 8 ptr = 1, k
            locother = findchar(nodes, next, start, topiclist(ptr) )
	    if (filenames(loc) .eq. filenames(locother)) goto 9
8	    continue
	ptr = k
c       Looks for other topic that use the same file
c
9       if (ptr .ne. k .and. 
     .      topiclist(ptr)(1:1) .eq. topiclist(k)(1:1)) then
c	  Such a topic exists and its pretty close to the current one
c	  so we don't want to repeat the documentation
c
	  call pushchar(stack, top, MAXSTCK, '        Same as: ' // 
     .					    nodes(locother) )     
          call pushchar(stack, top, MAXSTCK, ' ')
c
	else
c	  Such a topic does not exist so we display the documentation
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
 			    do 21 i = 1, pagesize - mod(top+place,pagesize)
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
	endif
c
 	numedges = available(link, adj(loc))
c	NUMEDGES = the number of adjacent neighbors
c
	docsize = top + numedges/NFIELD
	if (mod(numedges,NFIELD) .ne. 0) docsize = docsize + 1
	if (numedges .gt. 0) docsize = docsize + 3
	docsize = docsize + 2 + BOTBORDER
c	DOCSIZE = number of lines in current explain output; it is
c	   TOP adjusted for "Related topics" listing and extra
c	   lines of aesthetic documentaion.
c
	leftover = pagesize - place
	nextplace = place + docsize - pagesize
c	LEFTOVER = number of lines left over on the current page
c	NEXTPLACE = where the next output listing will start after
c		processing this one.
c
	skip = 0
	if  ( (docsize .gt. pagesize/2) .or.
     .        (leftover .lt. BORDER .and. docsize .gt. leftover) .or.
     .        (nextplace .gt. 0 .and. nextplace .lt. BORDER) ) 
     .	      skip = leftover
c	SKIP = number of extra lines to place at the start of the current
c	   listing for an aesthetic listing.
c
	if (skip .gt. 0 .and. skip .lt. pagesize) then 
	   place = place + skip
	   write(ISTDOUT,fmt='(1x,a)') (' ', i = 1, skip) 
	endif
c	Add extra lines to top of listing
c
	place = mod(place + docsize, pagesize)
c	Adjust PLACE to be where the next listing is to start
c
	write(ISTDOUT,fmt='(1x,a)') (stack(i)(1:lnblnk(stack(i))), i = 1, top)
c	List out the contents of the stack
c
	if (numedges .gt. 0) then
	   write(ISTDOUT,*) ' '
	   write(ISTDOUT,*) 'SEE ALSO:'
	   write(ISTDOUT,*) ' '
	   call listedges(nodes,dest,link,adj(loc),'',ISTDOUT)
	endif
c       Write out "See also" list
c
        write(ISTDOUT,*) ' '
        write(ISTDOUT,fmt="(1x,72('_'))")
c 
	if (BOTBORDER .ge. place) then
	   i2 = BOTBORDER - place
	   place = 0
	else
	   i2 = BOTBORDER
	endif
	if (i2 .ge. 1) then
	    do 95 i1 = 1, i2
	    	write(ISTDOUT,*) ' '
95	    	continue
	endif
c       Add aesthetic stuff; don't print out last BOTBORDER blank lines
c	if the last line that fits on the page was the '------' line.
c
	if (docsize .gt. pagesize*2 .and. place .gt. 0) then
	   skip = pagesize - place
	   place = mod(place+skip, pagesize)
	   write(ISTDOUT,fmt='(1x,a)') (' ', i = 1, skip) 
	endif
c	Add extra lines to end of long listings
c
100	continue
c
101   call exit(long(0))
c
      end
c
