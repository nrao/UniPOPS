      program explain
c
c     @(#)explain.f	5.1 06/22/94
c
c     User explain program
c
c     Syntax:  info_file  [topic]
c
      character*64 infofile, topic, uppercase, outname
      character*80 string
      integer*4 iargc, findminmatch, stack(32767), lnblnk, loc, top,
     .		MAXSTCK, system, findchar, long, newloc, getpid
      integer*2 ISTDOUT, ISTDERR, ISTDIN, OUTFILE, INFILE, irtn
      logical preproc, resetproc
      character*8 spid
c
      parameter (ISTDERR = 0)
      parameter (ISTDIN = 5)
      parameter (ISTDOUT = 6)
      parameter (OUTFILE = 11)
      parameter (INFILE = 12)
      parameter (MAXSTCK = 32767)
c
      include 'explain.inc'
c
      open(unit=ISTDERR, form="print")
      open(unit=ISTDOUT, form="print")
c
      if (iargc() .lt. 1) then
	write(ISTDERR,*) 'Syntax: info_file  [topic]'
	call exit(long(1))
      endif
c
      call getarg(long(1), infofile)
      if (iargc() .ge. 2) then
	call getarg(long(2), topic)
      else
	topic = 'HELP'
      endif
c     Check and get arguments
c
      write(spid, fmt='(i8)') getpid()
      irtn = system("echo " // spid // " >> $INFANTS")
      call readinfo(infofile)
c
      loc = findchar(nodes, next, start, '.')
      call push(stack, top, MAXSTCK, loc)
c     Pushes the root help file onto the stack first
c
10    continue
c
        open(unit=OUTFILE,status='scratch')
        inquire(unit=OUTFILE,name=outname)
	rewind(OUTFILE)
c
        if (uppercase(topic) .eq. 'HELP')  then
	    newloc = findchar(nodes, next, start, '.')
      	    call push(stack, top, MAXSTCK, newloc)
	else if (uppercase(topic) .eq. 'DONE')then
	    call exit(long(0))
	else if (topic .eq. '.') then
	     topic = nodes(loc)
	     newloc = loc
	else if (topic .eq. '..') then
	     call pop(stack, top, newloc)
	     if (top .eq. 0) then
	    	newloc = findchar(nodes, next, start, '.')
      	    	call push(stack, top, MAXSTCK, newloc)
	     endif
	     call pop(stack, top, newloc)
	     if (top .eq. 0) newloc = findchar(nodes, next, start, '.')
      	     call push(stack, top, MAXSTCK, newloc)
c	     Make sure that you always have the root help file on the stack
c
	     topic = nodes(newloc)
	else 
	     newloc = findminmatch(nodes, next, start, topic, OUTFILE)
	     if (newloc .ne. NULL) call push(stack, top, MAXSTCK, newloc)
	endif
c
	write(OUTFILE,*) ' '
	if (newloc .ge. 1) then
	     loc = newloc
	     write(OUTFILE,*) 'Topic: ', nodes(loc)(1:lnblnk(nodes(loc)))
	     write(OUTFILE,*) ' '
	     open(unit=INFILE, file=filenames(loc),
     .	       status='old',form='formatted',iostat=irtn)
	     if (irtn .ne. 0) then
     		write(OUTFILE,*) 'There is a problem with documentation for',
     .		nodes(loc)(1:lnblnk(nodes(loc))),'.  Inform computer staff'
	     else
		rewind(INFILE,iostat=irtn)
c
	        resetproc = .true.
20		read(INFILE,iostat=irtn,fmt='(a)') string
		if (irtn .eq. 0) then
		   if (preproc(string,'XPLN',resetproc)  )
     .			   write(OUTFILE,*) string(1:lnblnk(string))
c		   Pass the string through the preproccessor -- the pre-
c			proccessor should be reset for each file read in.
		   goto 20
		endif
		close(INFILE,iostat=irtn)
c		Read until end of file; place text in output file.
c
	     endif
c
	    
	else if (newloc .eq. NULL) then
	      write(OUTFILE,*) 'Sorry... There is no topic by the name of ',
     .				topic(1:lnblnk(topic))
	endif
c
	write(OUTFILE,*) ' '
	write(OUTFILE,*) 'The following are related topics:'
	write(OUTFILE,*) ' '
	call listedges(nodes, dest, link, adj(loc), '',OUTFILE)
	write(OUTFILE,*) ' '
	call flush(long(OUTFILE))
	irtn = system('more -d ' // outname)
c
1091 	write(ISTDOUT,fmt='(1x,2a,/,a,$)') 'Enter topic (or: HELP for help,',
     .		' DONE to quit, P to print last topic, ',
     .  	' .. to backup to previous topic, . to repeat last topic): '
	read(ISTDIN,*) topic
        if (uppercase(topic) .eq. 'P') then
	    irtn = system('printit none text ' // outname)
	    goto 1091
	endif
c
	close(OUTFILE)
c
	goto 10
c
      end
