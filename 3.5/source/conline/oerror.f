      SUBROUTINE OERROR	(IDD, isever, string)
C-------------------------------------------------------------------------------
C  @(#)oerror.f	5.2 09/10/98
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C	 ERROR displays	the error message on the users terminal
C     and returns not to the calling program, but to the GTLINE
C     routine.
c
c     IDD = error number
c     isever = error severity (-3, -2, -1, 0, 1)
c        -3 = internal POPS error
c	 -2 = severe execution error that the user can attempt to catch
c        -1 = fatal execution error
c         0 = warning message
c	  1 = interpretor/compiler error
c     string = aux. string to be printed with error
C
C Modified 890105 [PPM] includes to .inc, lowercase
c          8903   [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
      CHARACTER*60 MESAGE(500)
      character*80 command
      INTEGER*2 icommand(5), lastblnk, idd, iambiguous, karat, nerr, i,
     .          ilen, i1, i2, ier, j, isever, ilenstrng, ilencp, 
     .		iamath(2), n1
      character*(*) string
      character*1 svr
      logical*2 skipoe2
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'errors.inc'
      INCLUDE 'cio.inc'
      include 'ambig.inc'
      include 'appl.inc'
      include 'stk.inc'
c
      equivalence (command, icommand)
c
      include 'tags.inc'
      INCLUDE 'mesage.inc'
c
      DATA karat/' >'/, n1/1/
C
C=======================================================================
C
      skipoe2 = .false.
c
      if (amath .and. (idd .eq. iamath(1) .or. idd .eq. iamath(2))) then
	nspacen = MAX_DATA_POINTS + 1
	call amatherror
	call togtline
	amath = .false.
      endif
c     If array math is in progress and the error is that which is produced
c     when an array bound is exceeded, then go on to next command in interp
c     and then ask for next input line.
c
      errcode = float(idd)
c
      NERR = IDD
      ilen = lastblnk(mesage(nerr))
      ilenstrng = max(lastblnk(string),1)

      if (isever .eq. -1) then
	svr = 'F'
      else if (isever .eq. -2) then
	svr = 'S'
      else if (isever .eq. 0) then
	svr = 'W'
      else if (isever .eq. 1) then
	svr = 'I'
      else
	svr = 'P'
      endif
c
      IF (NERR .GT. 500 .OR. NERR .LT. 1) THEN
	  write(istderr, 11) svr, nerr, string(1:ilenstrng)
11        format( 1x, '(', a1, i3, ') ', a, ' : ', a)
      else if (ilen .le. 0) then
	  write(istderr, 11) svr, nerr, string(1:ilenstrng)
c         Prints out a special message if an unknown error code is found
c
      else 
c
	if (isever .gt. 0) then
           command = cbuff(max(1,kbptr-nkar):
     .                  max(1,kbptr-nkar) + min(nkar,10) - 1) //
     .			' : ' // string(1:ilenstrng)
c	   If isever>0, then print out command being interpreted as
c	   well as error message and aux. string.
	else
	   command = string(1:ilenstrng)
c	   if NERR > 100, then print out only aux. string
c
	endif
c
        ilencp = lastblnk(command)
c
        if (erron .or. isever .ne. -2) then
c	           be silent if it is a severe error being caught
           if (ilencp + ilen + 10 .gt. linelngth) then
              write(istderr,10) svr, nerr, mesage(nerr)(1:ilen),
     .                          command(1:ilencp)
10            format( 1x, '(', a1, i3, ') ', a, ' : ', /, 1x, a)
	   else
	      write(istderr,11) svr,nerr, mesage(nerr)(1:ilen),
     .                          command(1:ilencp)
	   endif
        endif
c
        if (nerr.eq.iambiguous) then
 	   write(istderr,*) ' '
           do 77 i1 = 1,napp,6
	      i2 = min(i1+5,napp)
	      write(cpuf, 3, iostat=ier) ((mname(j,i),j=1,5),i=i1,i2)
   3  	      format (6(2x,5a2))
	      ilen = lastblnk(cpuf)
	      write(istderr,*) cpuf(1:ilen)
   77         continue
 	   write(istderr,*) ' '
        endif
c       Takes care of the special case of an error caused by an ambiguous
c       symbol.
c
      endif
c
      IF (IDEBUG .GT. 0) write(istderr,1) idd,kbptr,nkar
   1  FORMAT ('	POPS ERROR NO. ',3I5,' : ')
c
      if (isever .eq. 0) then
	return
c       If type 0 error, just return.
c
      else if (isever .gt. 0) then
	write(istderr,*) 'Line ignored -- no commands executed'
c	Interpretor error so no commands should not be executed
c
      else if (isever .eq. -2) then
	if (erron) then
     	  write(istderr,*) 'Command skipped -- Rest of command line is ignored'
c	  You can ignore rest of commands on comand line
c
	else
	   erron = .true.
           skipoe2 = .true.
	   call execerror
c	   Try to silently continue executing rest of commands on line
c
	endif
c
      else 
     	write(istderr,*) 'Command skipped -- Rest of command line is ignored'
      endif
c     Give supplemental info as to what will go on next with command line.
c
      if (mode .eq. -1) mode = iedit
c     Was adding a decleration -- go back to mode prior to start of decleration
c
      if (mode .eq. 1 .and. iinunit .ne. istdin) then
     	        write(istderr,*) 'Inserting a FINISH into the procedure.'
     		call pseudo(ifinish-ipseudo+1)
		call push(a, ap, iomega)
		defexec = 0
	        lastline = "FINISH"
		call stores(n1)
      endif
c     Was compiling but input was not the terminal.
c
      if (mode .eq. -2 .or. mode .eq. 2 .or. mode .eq. 0) then
         L = 1
	 iedit = 0
	 mode = 0
	 llit = 0
	 call prompt(karat, ipt)
      endif
C     Reset control variables.
c
      if (.not. skipoe2) call oerror2
c
      erron = .true.
      call togtline
c     Alawys go to GTLINE.  GTLINE will never return control back to OERROR.
c
      END
c
