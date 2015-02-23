      program mkexp
c
c     @(#)mkexp.f	5.1 06/22/94
c
c     Programers interface for updating explain graph
c
      include 'explain.inc'
c
      character*192 inputline, file, crushed, longarg2
      character*8 cmnd
      character*1 answr
      character*64 arg(2), uppercase
      integer*4 ploc, findminmatch, findchar, i0, i1, i2, narg, 
     .		available, lnblnk, itemp, findedge, long, itemp2,
     .		itemp3, irtn, system, access
      integer*2 ISTDOUT, ISTDIN
      logical insnodechar, delnode, insedge, deledge
c
      parameter (ISTDOUT = 6)
      parameter (ISTDIN = 5)
c
      open(unit=ISTDOUT, form="print")
c
      write(ISTDOUT,*) ' '
      write(ISTDOUT,fmt='(1x,a,$)') 'Enter name of information file: '
      read(ISTDIN,*) file
c
1     call readinfo(file)
      ploc = 0
c     PLOC = Location in linked-list of current topic
c
10    if (ploc .eq. 0) ploc = max(1,findchar(nodes, next, start, '.'))
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) '--------------------------------------- '
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) 'Current topic: ', 
     .		nodes(ploc)(1:lnblnk(nodes(ploc))), '  ',
     .		filenames(ploc)(1:lnblnk(filenames(ploc)))
c
      write(ISTDOUT,*) ' '
      write(ISTDOUT,fmt='(1x,a,$)') 'Enter command (H for help): '
      read(ISTDIN,fmt='(a)') inputline
      write(ISTDOUT,*) ' '
c
      call rmblnk(inputline, crushed)
      i0 = index(crushed, ' ')
      cmnd = uppercase(crushed(1:i0))
c     Get the command
c
      arg(1) = ' '
      arg(2) = ' '
      i1 = index(crushed(i0+1:), ' ') + i0
      narg = 0
      if (i1 .gt. i0+1) then
	arg(1) = crushed(i0+1:i1)
	narg = 1
	i2 = index(crushed(i1+1:), ' ') + i1
	if (i2 .gt. i1+1) then
	   narg = 2
c			filenames can be > 64 chars, keep a long vers of arg(2)
	   arg(2) = crushed(i1+1:i2)
           longarg2 = crushed(i1+1:i2)
c
	   if (index(crushed(i2+1:), ' ') .gt. 1) then
		write(ISTDOUT,*) '** Too many arguments **'
		goto 10
	   endif
	endif
      endif
c     Get command arguments
c
c--------------------
      if (cmnd .eq. 'MV' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist **'
	else
	   ploc = itemp
	endif
c
c--------------------
      else if(cmnd .eq. 'CT' .and. narg .eq. 2) then
	itemp = findchar(nodes, next, start, arg(1))
	if (itemp .ne. NULL) then
	   write(ISTDOUT,*) '** Topic already exists: ',
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' **'
	else if (insnodechar(arg(1))) then
	   itemp = findchar(nodes, next, start, arg(1))
	   if (itemp .ne. NULL) then
		ploc = itemp
c				use longarg2
		filenames(ploc) = longarg2
		if (access(filenames(ploc),' ') .ne. 0) 
     .		     write(ISTDOUT,*) '** WARNING:  File does not exist **'
	   else
		write(ISTDOUT,*) '** Internal Problem **'
	   endif
	else
	   write(ISTDOUT,*) '** Cannot create topic **'
	endif
c
c--------------------
      else if(cmnd .eq. 'DT' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist **' 
	else
           write(ISTDOUT,*) 'Are you sure you want to delete ',
     .			     nodes(itemp)(1:lnblnk(nodes(itemp)))
           write(ISTDOUT,fmt='(1x,a,$)') '(Y or N : N = default)? '
           read(ISTDIN,fmt='(a)') answr
	   if (answr .eq. 'y' .or. answr .eq. 'Y') then
	     if (nodes(ploc) .eq. nodes(itemp)) 
     .		ploc = findchar(nodes, next, start, '.')
	     if (.not. delnode(nodes(itemp)) ) 
     .	        write(ISTDOUT,*) '** Cannot destroy topic **'
	   endif
	endif
c
c--------------------
      else if(cmnd .eq. 'RT' .and. narg .eq. 2) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	itemp2 = findchar(nodes, next, start, arg(2))
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (itemp2 .ne. NULL) then
	   write(ISTDOUT,*) '** Topic already exists: ', 
     .			    nodes(itemp2)(1:lnblnk(nodes(itemp2))),
     .			    ' **'
	else
	   nodes(itemp) = arg(2)
	endif
c
c--------------------
      else if(cmnd .eq. 'RF' .and. narg .eq. 2) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .ne. NULL) then
c			use longarg2
	   filenames(itemp) = longarg2
	   if (access(filenames(itemp),' ') .ne. 0) 
     .		write(ISTDOUT,*) '** WARNING:  File does not exist **'
	else
	   write(ISTDOUT,*) '** Topic does not exist **'
	endif
c
c--------------------
      else if(cmnd .eq. 'TO' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (findedge(nodes(ploc), nodes(itemp)) .ne. NULL) then
	   write(ISTDOUT,*) '** Path already exists: ', 
     .			    nodes(ploc)(1:lnblnk(nodes(ploc))),
     .			    ' -> ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' **'
	else
	   if (.not. insedge(nodes(ploc), nodes(itemp))) 
     .		write(ISTDOUT,*) '** Cannot create path: ',
     .				 nodes(ploc)(1:lnblnk(nodes(ploc))),
     .		  		 ' -> ',
     .				 nodes(itemp)(1:lnblnk(nodes(itemp))),
     .				 ' **'
	endif
c
c--------------------
      else if(cmnd .eq. 'TO*' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (findedge(nodes(ploc), nodes(itemp)) .ne. NULL) then
	   write(ISTDOUT,*) '** Path already exists: ', 
     .			    nodes(ploc)(1:lnblnk(nodes(ploc))),
     .			    ' -> ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' **'
	else
	   if (.not. insedge(nodes(ploc), nodes(itemp))) then
	        write(ISTDOUT,*) '** Cannot create path: ',
     .		 	        nodes(ploc)(1:lnblnk(nodes(ploc))), 
     .			        ' -> ',
     .				nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			        ' **'
	   else
	       itemp2 = findedge(nodes(ploc), nodes(itemp))
	       if (itemp2 .ne. NULL) then
		  dest(itemp2) = - dest(itemp2)
	       else
		  write(ISTDOUT,*) '** Internal Problem **'
	       endif
	   endif
	endif
c
c--------------------
      else if(cmnd .eq. 'FROM' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (findedge(nodes(itemp), nodes(ploc)) .ne. NULL) then
	   write(ISTDOUT,*) '** Path already exists: ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' -> ', 
     .			    nodes(ploc)(1:lnblnk(nodes(ploc))),
     .			    ' **'
	else
	   if (.not. insedge(nodes(itemp), nodes(ploc))) 
     .		write(ISTDOUT,*) '** Cannot create path: ',
     .				 nodes(itemp)(1:lnblnk(nodes(itemp))),
     .				 ' -> ',
     .		 		 nodes(ploc)(1:lnblnk(nodes(ploc))),
     .				 ' **'
	endif
c
c--------------------
      else if(cmnd .eq. 'FROM*' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (findedge(nodes(itemp), nodes(ploc)) .ne. NULL) then
	   write(ISTDOUT,*) '** Path already exists: ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' -> ', 
     .			    nodes(ploc)(1:lnblnk(nodes(ploc))),
     .			    ' **'
	else
	   if (.not. insedge(nodes(itemp), nodes(ploc))) then
     		write(ISTDOUT,*) '** Cannot create path: ',
     .				 nodes(itemp)(1:lnblnk(nodes(itemp))),
     .				 ' -> ',
     .		 		 nodes(ploc)(1:lnblnk(nodes(ploc))),
     .				 ' **'
	   else
	       itemp2 = findedge(nodes(itemp), nodes(ploc))
	       if (itemp2 .ne. NULL) then
		  dest(itemp2) = - dest(itemp2)
	       else
		  write(ISTDOUT,*) '** Internal Problem **'
	       endif
	   endif
	endif
c
c--------------------
      else if(cmnd .eq. 'CP' .and. narg .eq. 2) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	itemp2 = findminmatch(nodes, next, start, arg(2), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (itemp2 .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(2)(1:lnblnk(arg(2))), 
     .			    ' **'
	else if (findedge(nodes(itemp), nodes(itemp2)) .ne. NULL) then
	   write(ISTDOUT,*) '** Path already exists: ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' -> ', 
     .			    nodes(itemp2)(1:lnblnk(nodes(itemp2))),
     .			    ' **'
	else
	   if (.not. insedge(nodes(itemp),nodes(itemp2))) 
     .		write(ISTDOUT,*) '** Cannot create path: ',
     .				 nodes(itemp)(1:lnblnk(nodes(itemp))),
     .				 ' -> ',
     .		 		 nodes(itemp2)(1:lnblnk(nodes(itemp2))),
     .				 ' **'
	endif
c
c--------------------
      else if(cmnd .eq. 'CP*' .and. narg .eq. 2) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	itemp2 = findminmatch(nodes, next, start, arg(2), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (itemp2 .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(2)(1:lnblnk(arg(2))), 
     .			    ' **'
	else if (findedge(nodes(itemp), nodes(itemp2)) .ne. NULL) then
	   write(ISTDOUT,*) '** Path already exists: ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' -> ', 
     .			    nodes(itemp2)(1:lnblnk(nodes(itemp2))),
     .			    ' **'
	else
	   if (.not. insedge(nodes(itemp),nodes(itemp2))) then
     		write(ISTDOUT,*) '** Cannot create path: ',
     .				 nodes(itemp)(1:lnblnk(nodes(itemp))),
     .				 ' -> ',
     .		 		 nodes(itemp2)(1:lnblnk(nodes(itemp2))),
     .				 ' **'
	   else
	  	itemp3 = findedge(nodes(itemp),nodes(itemp2))
	   	if (itemp3 .ne. NULL) then
		    dest(itemp3) = - dest(itemp3)
	   	else
		    write(ISTDOUT,*) '** Internal Problem **'
	  	endif
	    endif
	endif
c
c--------------------
      else if(cmnd .eq. 'DPTO' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist **'
	else if (findedge(nodes(ploc), nodes(itemp)) .eq. NULL) then
	   write(ISTDOUT,*) '** Path does not exist: ', 
     .			    nodes(ploc)(1:lnblnk(nodes(ploc))),
     .			    ' -> ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' **'
	else
           write(ISTDOUT,*) 'Are you sure you want to delete path ',
     .			     nodes(ploc)(1:lnblnk(nodes(ploc))),
     .			     ' -> ',
     .			     nodes(itemp)(1:lnblnk(nodes(itemp)))
           write(ISTDOUT,fmt='(1x,a,$)') '(Y or N : N = default)? '
           read(ISTDIN,fmt='(a)') answr
	   if (answr .eq. 'y' .or. answr .eq. 'Y') then
	      if (.not. deledge(nodes(ploc), nodes(itemp)))
     .		 write(ISTDOUT,*) '** Cannot delete path **'
	   endif
	endif
c
c--------------------
      else if(cmnd .eq. 'DPFROM' .and. narg .eq. 1) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist **'
	else if (findedge(nodes(itemp), nodes(ploc)) .eq. NULL) then
	   write(ISTDOUT,*) '** Path does not exist: ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' -> ', 
     .			    nodes(ploc)(1:lnblnk(nodes(ploc))),
     .			    ' **'
	else
           write(ISTDOUT,*) 'Are you sure you want to delete path ',
     .			     nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			     ' -> ',
     .			     nodes(ploc)(1:lnblnk(nodes(ploc)))
           write(ISTDOUT,fmt='(1x,a,$)') '(Y or N : N = default)? '
           read(ISTDIN,fmt='(a)') answr
	   if (answr .eq. 'y' .or. answr .eq. 'Y') then
	      if (.not. deledge(nodes(itemp), nodes(ploc)))
     .		 write(ISTDOUT,*) '** Cannot delete path **'
	   endif
	endif
c
c--------------------
      else if(cmnd .eq. 'DP' .and. narg .eq. 2) then
	itemp = findminmatch(nodes, next, start, arg(1), ISTDOUT)
	itemp2 = findminmatch(nodes, next, start, arg(2), ISTDOUT)
	if (itemp .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(1)(1:lnblnk(arg(1))),
     .			    ' **'
	else if (itemp2 .eq. NULL) then
	   write(ISTDOUT,*) '** Topic does not exist: ', 
     .			    arg(2)(1:lnblnk(arg(2))), 
     .			    ' **'
	else if (findedge(nodes(itemp), nodes(itemp2)) .eq. NULL) then
	   write(ISTDOUT,*) '** Path does not exist: ', 
     .			    nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			    ' -> ', 
     .			    nodes(itemp2)(1:lnblnk(nodes(itemp2))),
     .			    ' **'
	else
           write(ISTDOUT,*) 'Are you sure you want to delete path ',
     .			     nodes(itemp)(1:lnblnk(nodes(itemp))),
     .			     ' -> ',
     .			     nodes(itemp2)(1:lnblnk(nodes(itemp2)))
           write(ISTDOUT,fmt='(1x,a,$)') '(Y or N : N = default)? '
           read(ISTDIN,fmt='(a)') answr
	   if (answr .eq. 'y' .or. answr .eq. 'Y') then
	      if (.not. deledge(nodes(itemp), nodes(itemp2))) 
     .		 write(ISTDOUT,*) '** Cannot delete path **'
	   endif
	endif
c
c--------------------
      else if(cmnd .eq. 'LDOWN' .and. narg .eq. 0) then
	call listedges2(nodes, dest, link, adj(ploc),'     ', ISTDOUT)
c
c--------------------
      else if(cmnd .eq. 'LUP' .and. narg .eq. 0) then
	call listnodes2(nodes(ploc), ISTDOUT)
c
c--------------------
      else if(cmnd .eq. 'LT' .and. narg .eq. 0) then
	call listnodes(nodes, next, start, '', ISTDOUT)
c
c--------------------
      else if(cmnd .eq. 'LA' .and. narg .eq. 0) then
	call listall(ISTDOUT)
c
c--------------------
      else if(cmnd .eq. 'DSP' .and. narg .eq. 0) then
	irtn = system('more ' // filenames(ploc))
c
c--------------------
      else if(cmnd .eq. 'AV' .and. narg .eq. 0) then
	write(ISTDOUT,*) available(next,availn), 
     .			 ' available in NODE linked-list'
	write(ISTDOUT,*) available(link,availe), 
     .			 ' available in DEST linked-list'
c
c--------------------
      else if(cmnd .eq. 'RST' .and. narg .eq. 0) then
        write(ISTDOUT,fmt='(1x,2a,$)') 
     .	  'Are you sure you want to restart (Y or N : N = default)? '
        read(ISTDIN,fmt='(a)') answr
	if (answr .eq. 'y' .or. answr .eq. 'Y') goto 1
c
c--------------------
      else if(cmnd .eq. 'SAVE' .and. narg .eq. 0) then
        write(ISTDOUT,fmt='(1x,2a,$)') 
     .	   'Are you sure you want to save (Y or N : N = default)? '
        read(ISTDIN,fmt='(a)') answr
	if (answr .eq. 'y' .or. answr .eq. 'Y') call writeinfo(file)
c
c--------------------
      else if(cmnd .eq. 'EXIT' .and. narg .eq. 0) then
	write(ISTDOUT,fmt='(1x,2a,$)') 
     .	   	'Are you sure you want to save everything ',
     .	        'and exit (Y or N : N = default)? '
        read(ISTDIN,fmt='(a)') answr
	if (answr .eq. 'y' .or. answr .eq. 'Y') then
	   write(ISTDOUT,*) ' '
	   call writeinfo(file)
	   call exit(long(0))
	endif
c
c--------------------
      else if(cmnd .eq. 'QUIT' .and. narg .eq. 0) then
	write(ISTDOUT,fmt='(1x,2a,$)') 'Are you sure you want to quit without',
     .			 ' saving (Y or N : N = default)? '
        read(ISTDIN,fmt='(a)') answr
	if (answr .eq. 'y' .or. answr .eq. 'Y') then
	   write(ISTDOUT,*) ' '
	   call exit(long(0))
	endif
c
c--------------------
      else if( (cmnd .eq. 'H' .or. cmnd .eq. 'HELP')  
     .		.and. narg .eq. 0) then
	write(ISTDOUT,*) 'MV topic_name           : Move to topic '
	write(ISTDOUT,*) 'CT topic_name file_name : Create topic '
	write(ISTDOUT,*) 'DT topic_name           : Delete topic'
	write(ISTDOUT,*) 'RT old_name new_name    : Rename topic'
	write(ISTDOUT,*) 'RF topic new_filename   : Rename filename for topic'
	write(ISTDOUT,*) 'TO destination          : Create path from current topic'
	write(ISTDOUT,*) 'TO* destination         : Create HARD path from current topic'
	write(ISTDOUT,*) 'FROM origin             : Create path to current topic'
	write(ISTDOUT,*) 'FROM* origin            : Create HARD path to current topic'
	write(ISTDOUT,*) 'CP origin destination   : Create path '
	write(ISTDOUT,*) 'CP* origin destination  : Create HARD path '
	write(ISTDOUT,*) 'DPTO destination        : Delete path from current topic'
	write(ISTDOUT,*) 'DPFROM origin           : Delete path to current topic'
	write(ISTDOUT,*) 'DP origin destination   : Delete path '
	write(ISTDOUT,*) 'LDOWN                   : List paths from current topic'
	write(ISTDOUT,*) 'LUP                     : List paths to current topic'
	write(ISTDOUT,*) 'LT                      : List all topics '
	write(ISTDOUT,*) 'LA                      : List all topics and paths'
	write(ISTDOUT,*) 'DSP                     : Display file for current topic'
	write(ISTDOUT,*) 'AV                      : Available space'
	write(ISTDOUT,*) 'RST                     : Restart -- ignore all changes'
	write(ISTDOUT,*) 'SAVE                    : Save changes'
	write(ISTDOUT,*) 'EXIT                    : Save changes and exit'
	write(ISTDOUT,*) 'QUIT                    : Quit without saving changes'
	write(ISTDOUT,*) 'H or Help               : Prints this table'
c
c--------------------
      else if(cmnd .ne. ' ') then
	write(ISTDOUT,*) '** Bad command or wrong number of arguments ',
     .			 ' --- Try again **'
c
c--------------------
      endif
c
      goto 10
c
      end
c
