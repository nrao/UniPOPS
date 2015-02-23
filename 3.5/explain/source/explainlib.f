c
c     Library of functions for explain facility
c
c     @(#)explainlib.f	5.2 08/30/94
c
c----------------------------------------------------------
c
      integer function available(link, avail)
c
c     Returns the number of available locations in the linked-list LINK
c     using the available pointer AVAIL.
c
c     LINK = I*4 = Linked list that is to be searched
c     AVAIL = I*4 = First location in the link to start the search
c
      integer*4 link(*), avail
c
      integer*4 ptr, NULL, num
c
      parameter (NULL = 0)
c
      ptr = avail
      num = 0
c
10    if (ptr .ne. NULL) then
	num = num + 1
	ptr = link(ptr)
	goto 10
      endif
c
      available = num
c
      return
      end
c
c----------------------------------------------------------
c
      integer*4 function findchar(info, link, start, item)
c
c     Returns the location of the first node containing ITEM or
c     returns NULL if ITEM doesn't exist.
c
c     INFO = C(*) = character array containing items in linked-list.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     ITEM = C(*) = character string to look for
c
      character*(*) info(*), item
      integer*4 link(*), start
c
      integer*4 ptr, NULL
      character*64 uppercase, item1
c
      parameter (NULL = 0)
c
      ptr = start
      item1 = uppercase(item)
c
10    if (ptr .ne. NULL) then
	if (item1 .eq. uppercase(info(ptr))) then
	   findchar = ptr
	else
	   ptr = link(ptr)
	   goto 10
	endif
      else
	findchar = NULL
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      integer*4 function findcharmx(info, link, start, item)
c
c     Returns the location of the first node containing ITEM or
c     returns NULL if ITEM doesn't exist.  Only compares the
c     the first few characters of INFO to ITEM.
c
c     INFO = C(*) = character array containing items in linked-list.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     ITEM = C(*) = character string to look for
c
      character*(*) info(*), item
      integer*4 link(*), start
c
      integer*4 ptr, NULL, ilen, lnblnk
      character*64 uppercase, item1
c
      parameter (NULL = 0)
c
      ptr = start
      ilen = lnblnk(item)
      item1 = uppercase(item)
c
10    if (ptr .ne. NULL) then
	if (item1(1:ilen) .eq. uppercase(info(ptr)(1:ilen))) then
	   findcharmx = ptr
	else
	   ptr = link(ptr)
	   goto 10
	endif
      else
	findcharmx = NULL
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      integer*4 function findminmatch(info, link, start, item, iodevice)
c
c     Returns the location of the first node containing ITEM or
c     returns NULL if ITEM doesn't exist.  Uses Min/Match.
c
c     INFO = C(*) = character array containing items in linked-list.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     ITEM = C(*) = character string to look for
c     IODEVICE = (I*2) Output device number
c
      character*(*) info(*), item
      integer*4 link(*), start
      integer*2 iodevice

      integer*4 NULL, NAMBIG
c
      parameter (NULL = 0)
      parameter (NAMBIG = 1024)
c
      integer*4 locs, nlocs, findcharmx, findchar, last
      character*64 names(NAMBIG)
c
      nlocs = 1
      locs = findchar(info, link, start, item)
c     Look for first occurance of ITEM without using min/match
c
      if (locs .ne. NULL) then
c	There is a perfect match so we don't need to do anything more
c
	findminmatch = locs
c
      else
c       No perfect match; Look for first occurance of ITEM using min/match
c
        locs = findcharmx(info, link, start, item)
c
        if (locs .eq. NULL) then
c	  It doesn't appear in list even when using min/match.
c
	  findminmatch = NULL
c
        else
c	  It does appear in list
c
10	  names(nlocs) = info(locs)
	  nlocs = nlocs + 1
c
	  if (nlocs .gt. NAMBIG) then
c
c	    Don't overflow the storage array LOC.
c
	    write(iodevice,*) ' '
	    write(iodevice,*) 'More than', NAMBIG, ' ambiguous topics by that name'
	    write(iodevice,*) ' '
c
	  else
c
	    last = locs
	    locs = findcharmx(info, link, link(locs), item)
c	    Look for more occurances of ITEM starting where we left off
c
	    if (locs .ne. NULL) goto 10
c	    More than 1 node by the name of ITEM was found; look for some more
c
	  endif
c
	  if (nlocs .le. 2) then
c	     If one only found, then return its LOC
c
	     findminmatch = last
c
	  else
c	     Else, write out a list of ambiguous names
c
	     write(iodevice,*) ' '
	     write(iodevice,*) 'Ambiguous topic name... Your choices are:'
	     write(iodevice,*) ' '
c
	     call writelist(names, nlocs-1, '',iodevice)
	     findminmatch = NULL
	     write(iodevice,*) ' '
c
	  endif
c
        endif
c
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      integer*4 function findi4(info, link, start, item)
c
c     Returns the location of the first node containing ITEM or
c     return NULL if ITEM doesn't exist.
c
c     INFO = I*4 = array containing items in linked-list.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     ITEM = I*4 = value to look for
c
      integer*4 info(*), item, link(*), start
c
      integer*4 ptr, NULL, item1
c
      parameter (NULL = 0)
c
      ptr = start
      item1 = abs(item)
c
10    if (ptr .ne. NULL) then
	if (item1 .eq. abs(info(ptr))) then
	   findi4 = ptr
	else
	   ptr = link(ptr)
	   goto 10
	endif
      else
	findi4 = NULL
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      integer*4 function findedge(a, b)
c
c     Finds the node edge (A->B) in the graph; returns the location in the
c     DEST linked-list of the edge or NULL if edge is not found.
c
c     A = C*(*) = character string for staring node
c     B = C*(*) = character string for ending node
c
      character*(*) A, B
c
      include 'explain.inc'
c
      integer*4 loca, locb, findminmatch, findi4
c
      loca = findminmatch(nodes, next, start, a, iodevice)
      locb = findminmatch(nodes, next, start, b, iodevice)
c
      if (loca .eq. NULL .or. locb .eq. NULL) then
	findedge = NULL
      else
	findedge = findi4(dest, link, adj(loca), locb)
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      logical function deletechar(info, link, start, avail, item)
c
c     Returns TRUE if ITEM is in the INFO linked-list and was succesfully
c     deleted.
c
c     INFO = C(*) = character array containing items in linked-list.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     AVAIL = I*4 = pointer to first available (free) location in LINK/INFO
c     ITEM = C(*) = character string to look for
c
      character*(*) info(*), item
      integer*4 link(*), start, avail
c
      integer*4 ptr, NULL, save
      character*64 uppercase, item1
c
      parameter (NULL = 0)
c
      item1 = uppercase(item)
c
      if (start .eq. NULL) then
c
c	List empty
c
	deletechar = .false.
c
      else if (uppercase(info(start)) .eq. item1) then
c
c	ITEM in first node
c
	ptr = start
	start = link(start)
	link(ptr) = avail
	avail = ptr
	deletechar = .true.
c
      else
c
c       List not empty and ITEM not in first node
c
	ptr = link(start)
	save = start
10      if (ptr .ne. NULL) then
	   if (uppercase(info(ptr)) .eq. item1) then
		link(save) = link(ptr)
		link(ptr) = avail
		avail = ptr
		deletechar = .true.
	   else
		save = ptr
		ptr = link(ptr)
		goto 10
	   endif
	else
	   deletechar = .false.
	endif
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      logical function deletei4(info, link, start, avail, item)
c
c     Returns TRUE if ITEM is in the INFO linked-list and was succesfully
c     deleted.
c
c     INFO = I*4 = array containing items in linked-list.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     AVAIL = I*4 = pointer to first available (free) location in LINK/INFO
c     ITEM = I*4 = value to look for
c
      integer*4 link(*), start, avail, info(*), item
c
      integer*4 ptr, NULL, save, item1
c
      parameter (NULL = 0)
c
      item1 = abs(item)
c
      if (start .eq. NULL) then
c
c	List empty
c
	deletei4 = .false.
c
      else if (abs(info(start)) .eq. item1) then
c
c	ITEM in first node
c
	ptr = start
	start = link(start)
	link(ptr) = avail
	avail = ptr
	deletei4 = .true.
c
      else
c
c       List not empty and ITEM not in first node
c
	ptr = link(start)
	save = start
10      if (ptr .ne. NULL) then
	   if (abs(info(ptr)) .eq. item1) then
		link(save) = link(ptr)
		link(ptr) = avail
		avail = ptr
		deletei4 = .true.
	   else
		save = ptr
		ptr = link(ptr)
		goto 10
	   endif
	else
	   deletei4 = .false.
	endif
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine writelist(info, nloc, startup, iodevice)
c
c     Writes out the names in the INFO array; NLOC names will be listed.
c
c     INFO = C(*) = character array containing items to be listed; only
c	the first 18 chars of INFO will be printed.
c     NLOC = maximum number of items to print out.
c     STARTUP = C*(*) = character string which is to precede output
c     IODEVICE = (I*2) = output device number
c
c     WARNING:  INFO is altered (sorted) by this routine
c
      character*(*) info(*), startup
      integer*4 nloc
      integer*2 iodevice
c
      integer*4 len, i, ilen, j, field, nfield
c
      parameter (FIELD = 18)
      parameter (NFIELD = 4)
c
      character*18 blanks
c
      blanks = ' '
c
      if (nloc .gt. 0) then
        ilen = min(FIELD-1,len(info(1)))
c
        call heapsort(info, nloc)
c	Sorts the list
c
	write(iodevice,fmt='(1x,9a)') ( startup, ( info(j)(1:ilen),
     .                      blanks(1:FIELD-ilen),
     .       		    j = i, min(nloc,i+NFIELD-1) ),
     .			    i = 1, nloc, NFIELD)
c
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine readinfo(iofile)
c
c     Reads information from IOFILE into internal linked-list arrays.
c
c     IOFILE = C*(*) = Character string containing name of file
c
      character*(*) iofile
c
      include 'explain.inc'
c
      integer*2 IFILE, ierr, ISTDERR
      integer*4 lnblnk, long
c
      parameter (IFILE = 10)
      parameter (ISTDERR = 0)
c
      open(unit=IFILE, file=iofile, status='old', form='unformatted', 
     .     iostat = ierr)
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot open input file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
c
      rewind(IFILE,iostat=ierr)
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot rewind input file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
c
      read(IFILE,iostat=ierr) start, availn, availe
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot read from input file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
      read(IFILE) nodes
      read(IFILE) filenames
      read(IFILE) next
      read(IFILE) adj
      read(IFILE) dest
      read(IFILE) link
c
      close(IFILE,iostat=ierr)
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot close input file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine writeinfo(iofile)
c
c     Writes information from linked-list arrays into IOFILE.
c
c     IOFILE = C*(*) = Character string containing name of file
c
      character*(*) iofile
c
      include 'explain.inc'
c
      integer*2 IFILE, ierr, ISTDERR
      integer*4 lnblnk, long
c
      parameter (IFILE = 10)
      parameter (ISTDERR = 10)
c
      open(unit=IFILE, file=iofile, status='unknown',  
     .     form='unformatted', iostat = ierr)
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot open output file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
c
      rewind(IFILE,iostat=ierr)
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot rewind output file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
c
      write(IFILE,iostat=ierr) start, availn, availe
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot write to output file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
      write(IFILE) nodes
      write(IFILE) filenames
      write(IFILE) next
      write(IFILE) adj
      write(IFILE) dest
      write(IFILE) link
c
      close(IFILE,iostat=ierr)
      if (ierr .ne. 0) then
	write(ISTDERR,*) 'Cannot close output file: ', iofile(1:lnblnk(iofile))
	call exit(long(1))
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      logical function insnodechar(new)
c
c     Inserts the node NEW in the graph; returns TRUE if successful.
c
c     NEW = C*(*) = character string for new node
c
      character*(*) new
c
      include 'explain.inc'
c
      integer*4 newloc, findchar, lnblnk
      integer*2 ISTDERR
c
      parameter (ISTDERR = 0)
c
      if (availn .eq. NULL) then
	write(ISTDERR, *) 'Overflow of NODE linked-list'
	insnodechar = .false.
      else if(findchar(nodes, next, start, new) .ne. NULL) then
	write(ISTDERR,*) 'Topic already exists: ', new(1:lnblnk(new))
	insnodechar = .false.
      else
	adj(availn) = NULL
	newloc = availn
	availn = next(availn)
	nodes(newloc) = new
	next(newloc) = start
	start = newloc
	insnodechar = .true.
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      logical function insedge(a, b)
c
c     Creates the node edge (A->B) in the graph; returns TRUE if successful
c
c     A = C*(*) = character string for staring node
c     B = C*(*) = character string for ending node
c
      character*(*) A, B
c
      include 'explain.inc'
c
      integer*4 loca, locb, findminmatch, new, findedge, lnblnk
      integer*2 ISTDERR
c
      parameter (ISTDERR = 0)
c
      loca = findminmatch(nodes, next, start, a, iodevice)
      locb = findminmatch(nodes, next, start, b, iodevice)
c
      if (loca .eq. NULL) write(ISTDERR,*) 'Topic does not exist: ',
     .			  a(1:lnblnk(a))
      if (locb .eq. NULL) write(ISTDERR,*) 'Topic does not exist: ',
     .		          b(1:lnblnk(b)) 
c
      if (loca .eq. NULL .or. locb .eq. NULL) then
	insedge = .false.
      else if (findedge(a,b) .ne. NULL) then
	 write(ISTDERR, *) 'Path ', a(1:lnblnk(a)), '->', b(1:lnblnk(b)),
     .			   ' already exists'
	 insedge = .false.
      else
	if (availe .eq. null) then
	   write(ISTDERR, *) 'Overflow of DEST linked-list'
	   insedge = .false.
	else
	   new = availe
	   availe = link(availe)
	   dest(new) = locb
	   link(new) = adj(loca)
	   adj(loca) = new
	   insedge = .true.
	endif
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      logical function delnode(old)
c
c     Deletes the node OLD in the graph; returns TRUE if successful
c
c     OLD = C*(*) = character string for old node
c
      character*(*) old
c
      include 'explain.inc'
c
      integer*4 loc, findminmatch, ptr, beg,
     .	        end, lnblnk
      integer*2 ISTDERR
      logical deletei4, anything, deletechar
c
      parameter (ISTDERR = 0)
c
      loc = findminmatch(nodes, next, start, old, iodevice)
c
      if (loc .eq. NULL) then
c
	write(ISTDERR,*) 'Topic does not exist: ', old(1:lnblnk(old))
	delnode = .false.
c
      else
c
	ptr = start
10	if (ptr .ne. NULL) then
	    anything = deletei4(dest, link, adj(ptr), availe, loc) 
	    ptr = next(ptr)
	    goto 10
	endif
c
	if (adj(loc) .ne. NULL) then
	   beg = adj(loc)
	   end = adj(loc)
	   ptr = link(end)
20	   if (ptr .ne. NULL) then
		end = ptr
		ptr = link(ptr)
		goto 20
	   endif
c
	   link(end) = availe
	   availe = beg
	endif
c
	delnode = deletechar(nodes, next, start, availn, old)
c
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      logical function deledge(a, b)
c
c     Deletes the node edge (A->B) in the graph; returns TRUE if successful
c
c     A = C*(*) = character string for staring node
c     B = C*(*) = character string for ending node
c
      character*(*) A, B
c
      include 'explain.inc'
c
      integer*4 loca, locb, findminmatch, findedge, lnblnk
      integer*2 ISTDERR
      logical deletei4
c
      parameter (ISTDERR = 0)
c
      loca = findminmatch(nodes, next, start, a, iodevice)
      locb = findminmatch(nodes, next, start, b, iodevice)
c
      if (loca .eq. NULL) write(ISTDERR,*) 'Topic does not exist: ',
     .			  a(1:lnblnk(a))
      if (locb .eq. NULL) write(ISTDERR,*) 'Topic does not exist: ',
     .			  b(1:lnblnk(b))
c
      if (loca .eq. NULL .or. locb .eq. NULL) then
	 deledge = .false.
      else if (findedge(a,b) .eq. NULL) then
	 write(ISTDERR, *) 'Path ', a(1:lnblnk(b)), '->', b(1:lnblnk(b)),
     .			   ' does not exist'
	 deledge = .false.
      else
	 deledge = deletei4(dest,link, adj(loca), availe, locb)
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine listnodes(info, link, start, startup, iodevice)
c
c     Lists all nodes in linked-list starting at START.  Precedes output
c     with string STARTUP.
c
c     INFO = C(*) = character array containing items in linked-list.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     STARTUP = C*(*) = character string which is to precede output
c     IODEVICE = (I*2) Output device number
c
      integer*4 link(*), start
      character*(*) info(*), startup
      integer*2 iodevice
c
      integer*4 ptr, NULL, i
      character*64 output(1024)
c
      parameter (NULL = 0)
c
      ptr = start
      i = 1
c
10    if (ptr .ne. NULL) then
	output(i) = info(ptr)
	i = i + 1
	ptr = link(ptr)
	goto 10
      endif
      if (i .ne. 1) call writelist(output, i - 1, startup, iodevice)
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine listnodes2(wanted, device)
c
c     Lists all nodes in linked-list starting at START that point to
c     destination WANTED.  
c
c     WANTED = Desired item 
c     DEVICE = (I*2) Output device number
c
      character*(*) wanted
      integer*2 device
c
      include 'explain.inc'
c
      integer*4 ptrnext, ptrnodes, i
      character*64 output(1024)
c
      ptrnext = start
      i = 1
c
10    if (ptrnext .ne. NULL) then
	ptrnodes = adj(ptrnext)
11	if (ptrnodes .ne. NULL) then
	    if (nodes(abs(dest(ptrnodes))) .eq. wanted) then
		if (dest(ptrnodes) .lt. 0) then
		   output(i) = '*' // nodes(ptrnext)
		else
		   output(i) = nodes(ptrnext)
		endif
		i = i + 1
	    else
	        ptrnodes = link(ptrnodes)
		goto 11
	    endif
	endif	    
	ptrnext = next(ptrnext)
	goto 10
      endif
      if (i .ne. 1) call writelist(output, i - 1, '    ', device)
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine listedges(names, info, link, start, startup, iodevice)
c
c     Lists all nodes in linked-list starting at START.  Precedes output
c     with string STARTUP.
c
c     NAMES = C(*) = character array containing items in linked-list.
c     INFO = I*4 = array containing locations in NAMES where INFO is
c		described.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     STARTUP = C*(*) = character string which is to precede output
c     IODEVICE = (I*2) Output device number
c
      integer*4 link(*),info(*), start
      character*(*) names(*), startup
      integer*2 iodevice
c
      integer*4 ptr, NULL, i
      character*64 output(1024)
c
      parameter (NULL = 0)
c
      ptr = start
      i = 1
c
10    if (ptr .ne. NULL) then
	output(i) = names(abs(info(ptr)))
	i = i + 1
	ptr = link(ptr)
	goto 10
      endif
      if (i .ne. 1) call writelist(output, i - 1, startup, iodevice)
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine listedges2(names, info, link, start, startup, iodevice)
c
c     Lists all nodes in linked-list starting at START.  Precedes output
c     with string STARTUP.  HARD edges are indicated by a '*'
c
c     NAMES = C(*) = character array containing items in linked-list.
c     INFO = I*4 = array containing locations in NAMES where INFO is
c		described.
c     LINK = I*4 = array containing links.
c     START = I*4 = starting location in LINK and INFO
c     STARTUP = C*(*) = character string which is to precede output
c     IODEVICE = (I*2) Output device number
c
      integer*4 link(*),info(*), start
      character*(*) names(*), startup
      integer*2 iodevice
c
      integer*4 ptr, NULL, i
      character*64 output(1024)
c
      parameter (NULL = 0)
c
      ptr = start
      i = 1
c
10    if (ptr .ne. NULL) then
	if (info(ptr) .lt. 0) then
	   output(i) = '*' // names(abs(info(ptr)))
	else
	   output(i) = names(info(ptr))
	endif
	i = i + 1
	ptr = link(ptr)
	goto 10
      endif
      if (i .ne. 1) call writelist(output, i - 1, startup, iodevice)
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine listall(device)
c
c     Lists all nodes in linked-list starting at START.  
c
c     DEVICE = (I*2) Output device number
c
      integer*2 device
c
      include 'explain.inc'
c
      integer*4 ptr, lnblnk
c
      ptr = start
c
10    if (ptr .ne. NULL) then
	write(device,*) ' '
	write(device,*) 'Topic: ', nodes(ptr)(1:lnblnk(nodes(ptr))),
     .		'  ', filenames(ptr)(1:lnblnk(filenames(ptr)))
	call listedges2(nodes, dest, link, adj(ptr),'    ', device)
	ptr = next(ptr)
	goto 10
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine pushchar(stack, top, maxstck, item)
c
c     Pushes ITEM onto the STACK; checks for overflow.
c
c     STACK = C*(*) = Stack array
c     TOP = I*4 = Ponter to top of stack
c     MAXSTCK = I*4 = max size of stack
c     ITEM = C*(*) = item to be placed on stack
c
      character*(*) stack(*), item
      integer*4 top, maxstck
c
      integer*2 ISTDERR
c
      parameter (ISTDERR = 0)
c
      if (top .eq. maxstck) then
	write(ISTDERR,*) 'Stack overflow'
      else
	top = top + 1
	stack(top) = item
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine push(stack, top, maxstck, item)
c
c     Pushes ITEM onto the STACK; checks for overflow.
c
c     STACK = I*4 = Stack array
c     TOP = I*4 = Ponter to top of stack
c     MAXSTCK = I*4 = max size of stack
c     ITEM = I*4 = item to be placed on stack
c
      integer*4 stack(*), top, maxstck, item
c
      integer*2 ISTDERR
c
      parameter (ISTDERR = 0)
c
      if (top .eq. maxstck) then
	write(ISTDERR,*) 'Stack overflow'
      else
	top = top + 1
	stack(top) = item
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine pop(stack, top, item)
c
c     Pops ITEM from the STACK; checks for underflow.
c
c     STACK = I*4 = Stack array
c     TOP = I*4 = Ponter to top of stack
c     ITEM = I*4 = item to be placed on stack
c
      integer*4 stack(*), top, item
c
      integer*2 ISTDERR
c
      parameter (ISTDERR = 0)
c
      if (top .eq. 0) then
	write(ISTDERR,*) 'Stack undeflow'
      else
	item = stack(top)
	top = top - 1
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      character*64 function uppercase(lc)
c
c     Converts any lower case characters in string lc to upper case.
c     Maximum of 64 chars converted.
c
c     LC = C*(*) = input character strings.
c
      character*(*) lc
c
      character*64 uc
      integer*4 lnblnk, ic1, i
c
      uc = lc
c
      do 100 i = 1, min(64,lnblnk(uc))
	ic1 = ichar(uc(i:i))
        if (ic1 .ge. 97 .and. ic1 .le. 122) uc(i:i) = char(ic1-32)
100     continue
c
      uppercase = uc
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine rmblnk(stringin, stringout)
c
c     Removes all extra blanbks from stringin and places output into
c     stringout.
c
c     STRINGIN/STRINGOUT = C*(*) = input/output strings
c
      character*(*) stringin, stringout
c
      integer*4 lnblnk, i
c
      stringout = stringin
      if (lnblnk(stringin) .eq. 0) return
c
1     continue
      if (stringout(1:1) .eq. ' ') then
	stringout = stringout(2:)
	goto 1
      endif
c     Removes preceeding blanks
c
10    continue
      do 20 i = 1, lnblnk(stringout)
	if (stringout(i:i+1) .eq. '  ') then
	   stringout(i+1:) = stringout(i+2:)
	   goto 10
	endif
20	continue
c     Deletes all extra blanks
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine heapsort(a, n)
c
c     Sorts the A list, which has N locations.
c
c     a = C*(*) = Character array to be sorted
c     n = I*4 = number of cells in INFO
c
      character*(*) a(*)
      integer*4 n
c
      integer*4 j
      character*64 item
c
      do 10 j = 1, n-1
	item = a(j+1)
	call insheap(a, j, item)
10	continue
c
      do 20 j = n, 2, -1
	call delheap(a, j, item)
	a(j)  = item
20	continue
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine insheap(tree, n, item)
c
c     Adds ITEM to the heap with N elements stored in TREE.
c
c     TREE = C(*) = Character array which will contain the HEAP
c     N = Number of items in heap
c     ITEM = the item to add to the heap
c     
      character*(*) tree(*), item
      integer*4 n
c
      integer*4 ptr, par
c
      ptr = n + 1
c     PTR = location of ITEM as it rises in the tree.
c     PAR = location of the parent of ITEM.
c
10    if (ptr .gt. 1) then
	par = ptr / 2
	if (lle(item,tree(par)) ) then
	   tree(ptr) = item
	else
	   tree(ptr) = tree(par)
	   ptr = par
	   goto 10
	endif
      else
	tree(1) = item
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      subroutine delheap(tree, n, item)
c
c     A heap is contained in the TREE array with N elements.  Assigns to
c     ITEM the root of the tree and then reheaps the remaining elements.
c
c     TREE = C(*) = Character array containing HEAP
c     N = Number of items in heap
c     ITEM = Returns the root of the heap
c
      character*(*) tree(*), item
      integer*4 n
c
      integer*4 ptr, right, left
      character*64 last
c
      item = tree(1)
      last  = tree(n)
      ptr = 1
      left = 2
      right = 3
c     LAST = value of originbal last node
c     PTR, LEFT, RIGHT = locations of LAST and its left and right children
c	as LAST sinks in the tree.
c
10    if (right .le. n) then
	if ( lge(last,tree(left)) .and. lge(last,tree(right)) ) then
	   tree(ptr) = last
	else
	   if (lle(tree(right),tree(left)) ) then
		tree(ptr) = tree(left)
		ptr = left
	   else
		tree(ptr) = tree(right)
		ptr = right
	   endif
	   left = 2*ptr
	   right = left + 1
	   goto 10
	endif
      else
	if (left .eq. n .and. llt(last,tree(left)) ) ptr = left
	tree(ptr) = last
      endif
c
      return
      end
c
c----------------------------------------------------------
c
      logical function preproc(string, topic, reset)
c
c     Takes a series of strings and preprocesses them; PREPROC will
c     be true if the STRING should be considered something dealing with
c     TOPIC.
c 
c     string = char*(*) = input string
c     topic = char*(*) = topic to preprocess for
c     reset = logical = if TRUE, preproc will reset itself to its initial
c		conditions and will set RESET to false.
c
c     Currently, preprocess lines that look like:
c
c     #if topic1 topic2 topic3 ...
c		If TOPIC matches any of the topic1, topic2, etc., then
c		everything between the #if and #end will be accepted.
c		If TOPIC doesn't match any of the topic1, ... then
c		everything between the #if and #end will be ignored.
c		You can't embed #if or #ifnot staatements inside other
c			#if or #ifnot statements
c     #ifnot topic1 topic2 topic3 ...
c		If TOPIC doesn't match any of the topic1, ... then
c		everything between the #if and #end will be accepted.
c		If TOPIC matches any of the topic1, topic2, etc., then
c		everything between the #if and #end will be ignored.
c		You can't embed #if or #ifnot staatements inside other
c			#if or #ifnot statements
c     #else
c		Reverses the sense of the last #if or #ifnot
c     #end
c		Singles the end of an #if or #ifnot statement
c     #def name anything
c		Will substitute 'anything' for 'name' anywhere it appears
c		in the document.  Up to 100 #def's are supported per document;
c		'name' and 'anything' can't be more than 80 characters long each.
c     #anything else
c		Comment lines.
c
      character*(*) string, topic
      logical reset
c
      integer*4 lnblnk, i, i1, ilen, i2, i3, ii1, ilennew, 
     .		istrt, istp
c
      character*80 names(100), anything(100), parsed
      logical topicok, ifon, maxnames
      integer numnames
c     NAMES/ANYTHING = arrays containing the contents of 'def' lines
c     NUMNAMES = number of 'def' lines so far
c     MAXNAMES = maximum number of #def's
c     PARSED = parsed # lines
c     IFON = true if one is in the middle of an #if/#ifnot - #end
c	construct
c     TOPICOK = true if the last #if matches TOPIC or if the last #ifnot
c		doesn't match TOPIC. 
c
      parameter (maxnames = 100)
c
      data topicok/.true./, ifon/.false./, numnames/0/
c
      preproc = .false.
c
      if (reset) then
	numnames = 0
	topicok = .true.
	ifon = .false.
	reset = .false.
      endif
c
      if (string(1:1) .eq. '#') then
c	Something to br pre-processed
c
	call rmblnk(string,parsed)
c	Remove all double blanks in input line
c
	if ((parsed(2:5) .eq. 'def ' .or. parsed(3:6) .eq. 'def ') .and.
     .		topicok .and. numnames .lt. maxnames) then
c		We can try to assign a 'def'
c
		i1 = index(parsed(5:),' ') + 4
		i2 = index(parsed(i1+1:),' ') + i1
		if (i1+1 .lt. i2-1) then
c		Protects against empty #def statements; empty #def's are ignored
c
		   do 20 i = 1, numnames
		    if (names(i) .eq. parsed(i1+1:i2-1)) then
		        i3 = index(string, names(i)(1:i2-i1)) + i2 - i1
		        anything(i) = string(i3:)
			goto 25
		    endif
20		    continue
		   numnames = numnames + 1
		   names(numnames) = parsed(i1+1:i2-1)
		   i3 = index(string, names(numnames)(1:i2-i1)) + i2 - i1
		   anything(numnames) = string(i3:)
c		   Only create a new def if no existing one with the same name
c
25		   continue
c
	      	endif
c
	else if((parsed(2:4) .eq. 'if ' .or. parsed(3:5) .eq. 'if ') .and.
     .		.not. ifon) then
c		We can try to process an #if -- ignore all embedded #if's.
c
		ifon = .true.
		
		if (index(parsed(2:),' ' // topic(1:lnblnk(topic)) // 
     .				' ') .ne. 0) then
		   topicok = .true.
		else
		   topicok = .false.
		endif
c
	else if((parsed(2:7) .eq. 'ifnot ' .or. parsed(3:8) .eq. 'ifnot ') .and.
     .		.not. ifon) then
c		We can try to process an #ifnot -- ignore all embedded #if's.
c
		ifon = .true.
		if (index(parsed(2:),' ' // topic(1:lnblnk(topic)) // 
     .				' ') .eq. 0) then
		   topicok = .true.
		else
		   topicok = .false.
		endif
c
	else if((parsed(2:6) .eq. 'else ' .or. parsed(3:7) .eq. 'else ') .and.
     .		ifon) then
c		We can try to process an #else -- ignore all embedded #if's.
c
		topicok = .not. topicok
c
	else if((parsed(2:5) .eq. 'end ' .or. parsed(3:6) .eq. 'end ') .and.
     .		ifon) then
c		We can try to process an #end -- ignore all extra #end's.
c
		ifon = .false.
		topicok = .true.
c
	endif
c
      else
c	No pre-processing is to be done -- must check if topicok is true to
c	see if we want this line.
c
	if (topicok) preproc = .true.
c
        do 10 i = 1, numnames
	  ii1 = 1
	  ilen = lnblnk(names(i))
	  ilennew = lnblnk(anything(i))
5	  i1 = index(string(ii1:), names(i)(1:ilen))
	  if (i1 .ne. 0) then
	    istrt = ii1 + i1 - 1
	    istp = istrt + ilen
	    if (ilennew .eq. 0) then
		string(istrt:) = string(istp:)
	    else
		string(istrt:) = anything(i)(1:ilennew) // 
     .		   		string(istp:)
	    endif
	    ii1 = ii1 + ilennew
	    goto 5
	  endif
10	  continue
c       Takes care of all substitutions for all previously defined "def's"
c
      endif
c
      return
      end
c
		
