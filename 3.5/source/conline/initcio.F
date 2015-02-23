      subroutine initcio
c
c---------------------------------------------------------
c   @(#)initcio.F	5.2 11/19/98
c---------------------------------------------------------
c
c     Initializes most of the variables in CIO.INC
c
      include 'cio.inc'
c
      integer*4 irtn, getpid, getlog, hostnm, getenv, getcwd, inf,
     1          iicio, ist4, ii
      logical first, flse
      real*4 rinf, rinfinity
      integer*2 ksize, kxsize, lsize, i, n1, lastblnk, lenpid
      character*6 pidtag
c
      equivalence (rinf, inf)
c
      data ksize/32766/,  kxsize/32766/, lsize/32766/, first/.true./
c
      parameter (n1=1)
      parameter (flse = .false.)
c
      rinf = rinfinity()
      if (first) then
	numcio = inf
	iicio = 1
19      if (Rio(iicio) .eq. inf) then
	   numcio = iicio
	else
	   iicio = iicio + 1
	   goto 19
	endif
c       Finds out the number of I*4 words in CIO common block
c
	first = .false.
        imenuin = 15
        igraphin = 18
        igraphout = 19
        igraphtype = 1
        iprinttype = 1
	projcode = ' '
c       Only reset IGRAPH, IPRINTTYPE if this is the first time INITCIO is called
c     
      endif
c
      version = VERSION
      cpt = '  '
c
      irtn = getlog(oobs)
      irtn = hostnm(site)
      irtn = getcwd(dirname)
      call progname(program)
      irtn = getpid()
      write(unit=procid, 10) irtn
10    format (i8)
c
      nbytes = 0
      outbuf = ' '
      cbuff = ' '
      karlim = 160
      linelngth = 80
      pagefull = .false.
      cpuf = ' '
c
      inline = 1
      inlast = 1
      numline = 23
      inhist = 0
      maxhist = 50
      lastline = ' '
      do 100 i = 1, 50
	hist(i) = ' '
	instring(i) = ' '
100	continue
c
      nfiles = 10
      do 110 i = 1, nfiles
	ffile(i,1) = 0
	ffile(i,2) = 0
	ffile(i,3) = 0
	ffile(i,4) = 0
	cfile(i) = ' '
110	continue
c
      prntfmt = '(1pg15.7.2)'
c
      lenpid = lastblnk(procid) 
      pidtag = '      '
      ii = 0
      do 115 i= lenpid-5, lenpid
         if (i .gt. 0) then
            if (procid(i:i) .ge. '0' .and. procid(i:i) .le. '9') then
               ii = ii + 1
               pidtag(ii:ii) = procid(i:i)
            endif
         endif
 115  continue
c            
      cprtout = program(1:1)//'PRINTOUT'
      csetupin = program(1:1) // 'SETUP'
      couttxt = '/tmp/' // program(1:1) // 'TEXT' // pidtag
      coutgrph = '/tmp/' // program(1:1) // 'GRAPH' // pidtag
      cmemory = program(1:1) // 'MEMORY'
      crecover = program(1:1) // 'RECOVER'
      cmenuin = '/tmp/' // program(1:1) // 'MIN' // pidtag
      cmenuout = '/tmp/' // program(1:1) // 'MOUT' // pidtag
      clogout = program(1:1) // 'CMDLOG'
      cgraphin = '/tmp/' // program(1:1) // 'GIN' // pidtag
      cgraphout = '/tmp/' // program(1:1) // 'GOUT' // pidtag
      ccube = ''
c
      istderr = 0
      istdin = 5
      istdout = 6
      ibatchin = 7
      iprtout = 8
      iouttxt = 9
      ioutgrph = 10
      iiotmp = 11
      isetupin = 12
      ishelpin = 13
      irecover = 14
      imenuout = 16
      ilogout = 17
      icube = 18
      imemory = 21
c
      open(unit=istdout, form='print')
      open(unit=istderr, form='print')
      write(istdout,109)
109   format('1')
c     Redefine the output FORM of STDOUT so that carriage control characters
c     will be properly interpreted.  Clears Text screen.
c	
      iout = istdout
      iinptr = 1
      iinlist(iinptr, 1) = istdin
      iinlist(iinptr, 2) = istdin
      cinlist(iinptr) = 'stdin'
      iinunit = iinlist(iinptr, 1)
      iintype = iinlist(iinptr, 2)
c
      online = .false.
      dolog = .false.
      lsetup = .false.
c
      do 101 i = 1, 10
	iounit(i) = -32767
	ciounit(i) = ' '
101	continue
      iounit(1) = 31
      iounit(2) = 32
      iounit(3) = 33
      ciounit(1) = program(1:1)//'DATA'
      ciounit(2) = program(1:1)//'KEEP'
      ciounit(3) = program(1:1)//'SAVE'
      if (program(1:1) .eq. 'L') then
         iounit(4) = 34
         ciounit(4) = program(1:1)//'GAINS'
         iounit(5) = 35
         ciounit(5) = program(1:1)//'RECORDS'
      endif
c     Sets only those iounit#  which will be used
c
      irecsiz = 256
      kblk = ksize / irecsiz
      if (kblk*irecsiz .ne. ksize) kblk = kblk + 1
      kxblk = kxsize / irecsiz
      if (kxblk*irecsiz .ne. kxsize) kxblk = kxblk + 1
      lblk = lsize / irecsiz
      if (lblk*irecsiz .ne. lsize) lblk = lblk + 1
c
      irtn = getenv('popsdir', dirprefix(1))
      irtn = getenv('popsutil1', dirprefix(2))
      irtn = getenv('popsutil2', dirprefix(3))
      irtn = getenv('popsproc1', dirprefix(4))
      irtn = getenv('popsproc2', dirprefix(5))
c
      iglinex = 1
      igliney = 780
      graphon = .false.
c
      undoon = .false.
c
      call fset(n1, flse)
      call rotlet(0.)
      call charbd(n1)
      call charsize(1.)
c     Initializes default character font.
c
      if (version .lt. 1) then
         write(istdout,*) 'Version: ', version*1000.0
      else
         write(istdout,*) 'Version: ', version
      endif
c
      ist4 = istdout
      call flush(ist4)
c
      return
      end
c


