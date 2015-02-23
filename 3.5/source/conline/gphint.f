      subroutine gphint
C-------------------------------------------------------------------------------
C  @(#)gphint.f	5.3 03/02/95
C-------------------------------------------------------------------------------
c
c     Sets up graphics and output devices
c
      integer*2 getenv, irtn, lastblnk, iptemp
      integer*4 long, i, ilist(100), istat, itmp
      character*1 colon, dummy1, dummy2
      character*64 pcomment
      character*1024 pline, popsdir, printfile, tprinter
      logical first, popened
c
      save first
c
      include 'cio.inc'
c
      data first /.true./
      data colon /':'/
c
      if (first) then
         first = .false.
         irtn = getenv('DISPLAY',termtype)
         if (lastblnk(termtype) .ne. 0) then
c			This must be an X terminal
            termtype = 'xwindow'
            igraphtype = 2
         else
            irtn = getenv('TERM',termtype)
            if (termtype(1:4) .eq. '4014') then
               termtype = 'tek4014'
            else if(termtype(1:3) .eq. 'sun') then
               irtn = getenv('WINDOW_PARENT', termtype)
            endif
c
            if (termtype(1:7) .eq. 'tek4014') then
               igraphtype = 3
            else if(termtype(1:8) .eq. '/dev/win') then
               igraphtype = 2
            endif
         endif
c
 1       if (igraphtype .eq. 0 .or. igraphtype .eq. 1) then
 105        write(istdout,10) ' '
            write(istdout,10) 
     .              'The following Graphics devices are supported:'
            write(istdout,10) '1: None (Non-graphics terminal)'
            write(istdout,10) '2: SUN workstation (POPSTOOL)'
            write(istdout,10) '3: TEK4010 or emulation'
            write(istdout,10) '4: Retrographics-like board'
            write(istdout,10) 'X-Windows is available, however, the'
            write(istdout,10) 'DISPLAY environment variable MUST be set'
            write(istdout,10) 'before starting this program.  When'
            write(istdout,10) 'DISPLAY is set, X-Windows is assumed.'
            write(istdout,10) ' '
            write(istdout,10) 
     .               'Enter <cr> to use default or number to change'
            write(istdout,12) 'GRAPHICS DEVICE (DEFAULT:1) '
 10   format(1x,a)
 12   format(1x,a,$)
            call flush(long(istdout))
            read(istdin,11,err=1) igraphtype
	    if (igraphtype .ge. 5) goto 105
 11   format(i1)
         endif
c
         if (igraphtype .eq. 5) then
            igraphtype = 2
            termtype = 'xwindow'
            call coreint
         else if (igraphtype .eq. 4) then
            call v102int
         else if (igraphtype .eq. 3) then
            call tekint
         else if (igraphtype .eq. 2) then
            call coreint
         else if (igraphtype .eq. 1 .or. igraphtype .eq. 0) then
            call nogint
         else 
            goto 1
         endif
c
         call clrpage
c
         irtn = getenv('popsprinter', printer)
         irtn = getenv('popsdir',popsdir)
c			use ioutgrph as the unit for IO to the printer
c			file, it should be safe since it isn't used until
c			this is settled
         iptemp = 0
         printfile = popsdir(1:lastblnk(popsdir)) // 'sunbin/' 
     .                   // 'printers'
         popened = .false.
         open(unit=ioutgrph,file=printfile(1:lastblnk(printfile)),
     .        status='old',iostat=istat)
         if (istat .eq. 0) popened = .true.
         if (popened .and. lastblnk(printer) .ne. 0) then
            printtype = ' '
 100        read(unit=ioutgrph,fmt='(a)',end=200,iostat=istat) pline
            if (istat.eq.0) then
               call getprel(pline,tprinter,printtype,
     .                            dummy1,dummy2,pcomment)
 115           if (tprinter(1:lastblnk(tprinter)) .ne.
     .              printer(1:lastblnk(printer))) goto 100
c				we only get here if we've matched printer
c				Ok, does printtype make sense?
               if (printtype(1:10) .eq. 'postscript') then
                  iptemp = 3
               else if (printtype(1:3) .eq. 'qms') then
                  iptemp = 2
               else if (printtype(1:4) .eq. 'hpgl') then
                  iptemp = 4
               else if (printtype(1:4) .eq. 'quic') then
                  iptemp = 5
               else if (printtype(1:4) .eq. 'text') then
                  iptemp = 1
               else
                  goto 100
               endif
            endif
         endif
c
 200     continue
         if (iptemp .eq. 0 .and. popened) then
 2          rewind(unit=ioutgrph,iostat=istat)
            write(istdout,10) ' '
            write(istdout,10) 
     .               'The following Hardcopy devices are supported:'
            itmp = 1
            write(istdout,13) itmp,': None (Non-graphics printer)'
 13         format(x,i2,a)
c
            do 205 i = 1, 1000
               read(unit=ioutgrph,fmt='(a)',end=300,
     .                 iostat=istat) pline
               if (istat .ne. 0) go to 300
               call getprel(pline,tprinter,printtype,
     .                   dummy1,dummy2,pcomment)
               if (lastblnk(tprinter).eq.0) goto 205
               if (tprinter .eq. "none") goto 205
               itmp = itmp + 1
               ilist(itmp) = i
               write(istdout,14) itmp,
     .                        (tprinter(1:lastblnk(tprinter))),
     .                        (pcomment(1:lastblnk(pcomment)))
 14            format(x,i2,': ',a,x,'(',a,')')
 205        continue
c
 300        write(istdout,10) ' '
            write(istdout,10) 
     .               'Enter <cr> to use default or number to change'
            write(istdout,12) 'HARDCOPY DEVICE (DEFAULT:1) '
            call flush(long(istdout))
            read(istdin,11,err=2) iptemp
	    if (iptemp .eq. 0) then
		iptemp = 1
	    else if (iptemp .lt. 0 .or. iptemp .gt. itmp) then
		goto 2
	    endif
            if (iptemp .gt. 1) then
               itmp = iptemp
               iptemp = 0
               rewind(unit=ioutgrph,iostat=istat)
               do 305 i = 1, ilist(itmp)
                  read(unit=ioutgrph,fmt='(a)',end=400,
     .                    iostat=istat) pline
                  if (istat .ne. 0) goto 400
 305           continue
               call getprel(pline,tprinter,printtype,
     .                    dummy1,dummy2,pcomment)
               if (lastblnk(tprinter).eq.0) goto 400
               printer = tprinter
               if (printtype(1:10) .eq. 'postscript') then
                  iptemp = 3
               else if (printtype(1:3) .eq. 'qms') then
                  iptemp = 2
               else if (printtype(1:4) .eq. 'hpgl') then
                  iptemp = 4
               else if (printtype(1:4) .eq. 'quic') then
                  iptemp = 5
               else
                  goto 400
               endif
            else
               printer = 'none'
               printtype = 'text'
            endif
         endif
c			something is wrong in the printers file if we get here
c			and iptemp = 0
 400     if (iptemp .eq. 0) then
            write(istdout,10) ' '
            write(istdout,10) 
     .          ' WARNING: printers file does not exists or is corrupt.'
            write(istdout,10) 
     .          ' Consult your local unipops guru for help.'
            write(istdout,10) ' A non-graphics printer will be assumed.'
            write(istdout,10) ' '
            printer = 'none'
            printtype = 'text'
            iptemp = 1
         endif
c
         if (popened) close(ioutgrph)
	 if (iprinttype .ne. 0) then
		iprinttype = sign(iptemp, iprinttype)
	 else
		iprinttype = iptemp
	 endif
c
         inline=1
         inlast=1
         call clrpage
c     Sets up number of line, etc. for the size of the text screen
c
      endif
c		skip everything if not first time through
c
      numline=23
      linelngth = 80
      if (igraphtype .eq. 2) numline=20
      if (igraphtype .eq. 3) numline=33
      if (igraphtype .ge. 3) linelngth=72
      if (igraphtype .eq. 5) linelngth=80
c
      return
      end
c
