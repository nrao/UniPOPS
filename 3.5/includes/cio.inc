C					Include	CIO.
C----------------------------------------------------------------------------
C  @(#)cio.inc	5.1 06/22/94
C----------------------------------------------------------------------------
c
c			This has been organized to help align things 
c			in memory.  Try and keep it that way.
c
c			The restrictions are:
c				(1) version must be first
c				(2) cpt must be second
c				(3) numcio must be last
c
c		Current size of IO : 23176 bytes
c
c				8 bytes
      real*4 version
      character*2 cpt, program
c				obvious multiples of 8 bytes
c				(total here is 21816 bytes)
      CHARACTER*8  OOBS,SITE, termtype, procid, projcode
      character*16 csetupin, couttxt, coutgrph, cmenuin,
     .		   cmenuout, cgraphin, cgraphout, crecover,
     .		   prntfmt, printer, printtype
      character*64 dirprefix(10), dirname, cprtout, cmemory,
     .             ciounit(10), ccube, clogout, cinlist(50),
     .		   cfile(10)
      CHARACTER*160 instring(50), hist(50), lastline
c				(total here is 1240 bytes)
      integer*2 karbuf(160), ioutbuf(160), jbuff(80), ipuf(80),
     .          iinlist(50, 2), ffile(10,4)
c				18 bytes
      logical*2 pagefull, graphon, online, dolog, undoon, lsetup
      integer*2 linelngth, inline, inlast
c				24 bytes
      integer*2 iounit(10), nbytes, karlim
c				70 bytes
      integer*2 numline, inhist, maxhist, igraphtype, 
     .          kxblk, iprinttype, iinptr, 
     .          iout, iinunit, iintype, istdin, 
     .          istdout, istderr, iprtout, ibatchin, 
     .          isetupin, icube, imenuin, imenuout, 
     .          ilogout, igraphin, igraphout, iouttxt, 
     .          ioutgrph, iiotmp, imemory, ishelpin, 
     .          irecover, irecsiz, kblk, lblk, 
     .          iglinex, igliney, nfiles, icio_dum
c			padded out so numcio starts at an
c			appropriate place and IO ends on
c			a R*8 word boundary
c			numcio comes last
      integer*4 numcio
c			the following do not appear explicitly in
c			IO but are equivalenced to members of IO
      character*160 cpuf, cbuff
      character*320 outbuf
      integer*2 ipt
c
      EQUIVALENCE  (CPUF,IPUF), (ipt, cpt), (cbuff, jbuff), 
     1             (outbuf,ioutbuf)
c
c	this is in the order of the declarations above
c
      common /io/ version, cpt, program, OOBS,SITE, termtype, 
     .   procid, projcode, csetupin, couttxt, coutgrph, 
     .   cmenuin, cmenuout, cgraphin, cgraphout, crecover, prntfmt,
     .   printer, printtype,
     .   dirprefix, dirname, cprtout, cmemory, ciounit, 
     .   ccube, clogout, cinlist, cfile, instring, hist, lastline,
     .   karbuf, ioutbuf, jbuff, ipuf, iinlist, ffile,
     .   pagefull, graphon, online, dolog, undoon,lsetup,
     .   linelngth, inline, inlast, nbytes, karlim,  
     .   numline, iounit, inhist, maxhist, igraphtype, 
     .   iprinttype, iinptr, iout, iinunit, 
     .   iintype, istdin, istdout, istderr, iprtout, ibatchin, 
     .   isetupin, icube, imenuin, imenuout, ilogout, 
     .   igraphin, igraphout, iouttxt, ioutgrph, iiotmp, 
     .   imemory, ishelpin, irecover, irecsiz, kxblk, kblk, lblk, 
     .   iglinex, igliney, nfiles, icio_dum, numcio
c**     .   igline, icio_dum, numcio
c
c	it is usefull to have the following here so that future changes only
c	need be made here.  Rio is the array that is used to write out the
c       contents of cio.  Its size must simply be larger than numcio which
c       is set in initcio.
c
      integer*4 Rio(5800)
      equivalence (Rio, cpt)
c					made in one location
C						       End CIO.
c     version = variable containing  version of program in the format: yymmdd 
c     cpt = present prompt character
c     program = name of program
c     oobs = observer's name or initials
c     site = name of site or computer
c     termtype = terminal type
c     procid = proccess ID string
c     projcode = User's project code.
c
c     csetupin = name of aux. (setup) input file
c     couttxt = name of text screen dump file
c     coutgrph = name of graphics dump file
c     cmenuin = name of aux. (menu) input file
c     cmenuout = name of aux. (menu) output file
c     cgraphin = name of graphics screen input file
c     cgraphout = name of graphics screen output file
c     crecover = name of recovery file
c
c     dirprefix = character arrays containg directory names where certain 
c	files are to be found: 1 = where executables can be found;
c	2 = where files can be found; 3 - 5 = where user/system procedures
c	can be found
c     dirname = current working directory
c
c     cprtout = name of aux. (printout) output file
c     cmemory = name of memory file
c     ciounit# = name of possible IO data files
c     ccube = name of cube file
c     clogout = name of aux. (cmd log file) output file
c     cinlist = name of file associated with iinlist, if applicable
c
c     instring = character array which stores lines read or written
c     hist = character array which stores lines read
c     lastline = last line read in by PREAD (Raw line - no processing performed) 
c     karbuf = unpacked string read in by PREAD
c     ioutbuf = packed string used for output
c     jbuff = packed string read in by PREAD
c     ipuf = Temporary buffer for storing input/output strings
c
c     iinlist = log of active input devices
c
c     pagefull = (logical) true if the user wants to stop scrolling after
c	every page of text
c     graphon = true when output is to go to the graphics screen, false
c	if it is to go somewhere else.
c     online = TRUE if on-line data file exists and is accessible, otherwise
c	it is false.
c     dolog = TRUE if all commands should be echoed to a log file.
c     undoon = TRUE if undo feature is active.
c     lsetup = TRUE if LSETUP is being read
c
c     linelngth = width of screen in characters; set by GPHINT
c     inline = present line number read or written in current text window
c	Reset by GPHINT, CLRPAGE, and UPDATELINE
c     inlast = last line number read or written in curent text window
c     nbytes = number of bytes in the line read by pread (Can't exceed
c	karlim)
c     karlim = max. length of an input/output line
c     numline = maximum number of lines for the text window and depends upon
c	graphics screen; set by GPHINT
c
c     iounit# = unit numbers for possible I/O data files
c           1 = DATA type of file (Read only, by scan #, read by
c		GET, ON, OFF, etc. verbs)
c	    2 = KEEP type of file (Read/write, by scan #, read by 
c		UNKEEP verb, written by KEEP, REKEEP verbs)
c	    3 = SAVE type of file (Read/write, by NSAVE, read by
c		RECALL verb, written by SAVE verb)
c	    4 = Gains type of file (Read only, by scan #, read by GGET)
c           5 = Zeros type of file (Read only, by scan #, read by ZGET)
c
c
c     inhist = present line numberread in; used for HISTORY
c     maxhist = maximum number of lines storable in hist
c     igraphtype = type of graphics screen; set by GPHINT
c     iprinttype = type of printer; set by GPHINT
c
c     iinptr = number of active input devices
c     iout = present output unit number
c     iinunit = set to current input unit number = iinlist(iinptr,1)
c     iintype = set to "file type" (istdin, ibatchin, imenuin, etc)
c          = iinlist(iinptr, 2)
c          (if < 0, then no min-match and all symbols must be new ones)      
c     istdin = terminal input unit number
c     istdout = terminal output unit number
c     istderr = error reporting unit number
c     iprtout = auxillary output unit number (printout file)
c     ibatchin = auxillary input unit number (batch file)
c     isetupin = auxillary input unit number (setup file)
c     icube = unit number for cube file
c     imenuin = auxillary input unit number (menus)
c     imenuout = auxillary output unit number (menus)
c     ilogout = auxillary output unit number (command log file)
c     igraphin = graphics screen input unit number
c     igraphout = graphics screen output unit number
c     iouttxt = text screen dump output unit number
c     ioutgrph = graphics command storage output unit number
c     iiotmp = temporary unit (i.e., a file that is opened and then
c	almost immediately closed)
c     imemory = unit number for memory file
c     ishelpin = auxillary input unit number (shelp file)
c     irecover = unit number for recovery file
c
c     prntfmt = format that PRINT will use
c
c     nfiles = number of possible open sequential access files.
c     cfile = names of the open files
c     ffile = array containing the charactersistics of the open files.
c	ffile(*.1) = unit #; ffile(*,2) = RW access; ffile(*,3) = file
c	position; ffile(*,4) = file existence.
c
c     irecsiz = number of short integer words in each record of above files
c     kblk, kxblk, lblk = number of K, KX, and LISTF blocks in memory file
c
c     iglinex, igliney = x,y pixels for next write to graphics screen 
c
c     numcio = numbert of bytes in cio common block
c
