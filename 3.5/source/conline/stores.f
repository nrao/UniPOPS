      SUBROUTINE STORES	(J)
C-------------------------------------------------------------------------------
C  @(#)stores.f	5.2 07/25/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C   STORES-OPERATIONS
C   -----------------
C	 STORES	stores either the procedure source code, procedure
C     object code, or handles the source code.	Note opcode 64 is a
C     internal operator	that stores the	source and object code for
C     procedures.
C
C     LISTF is the storage location for	PROC source code.
C
C---------------------------------------------------------------------
C
      integer*2 memory(8), itempstring(80), inum, chngfile
      character*160 tempstring, query
      character*80 command1, filename, command2, cnfile
      integer*2 iwpc, j, llocat, listl, i3, lastblnk, ibyte, ii, ier, 
     .          unused, ilen3, numnames, unalias, istat
      integer*4 iloc, istat4, long, onlinesite, system, ix, it, ll, 
     .          lll, i, jj, ix1, nmem, irtn, oldk3, oldkx3, oldlistf3,
     .          lscratch, iftype, sign, zero4, lunit, ioput
      real*4 oldvers
      logical*2 changeproj, lrtn
      character*24 zdatetime
      character*1023 fullname
      logical*2 opened
      character*60 cix, ckpak
      character*8 project
      character*11 names(128)
      character*1 wintype
      integer*2 n0, n1, n6, n11, n17, n29, n30, n31, n80, n120, n351,
     .          n352, n355, n356, n357, m1, m3, fshort, n2, n9, n28,
     .          n26, n232, n39, n367
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
      include 'lsf.inc'
c
      include 'tags.inc'
c
      parameter (numnames=128)
      parameter (zero4=4)
c
      equivalence (memory(1), cmemory), (itempstring(1), tempstring)
      equivalence (cix, x(1)), (ckpak, kpak)
C
      data n0, n1, n6, n9, n11, n17, n29, n30, n31, n80, n120, n351,
     .          n352, n355, n356, n357, m1, m3, n2
     .    /0, 1, 6, 9, 11, 17, 29, 30, 31, 80, 120, 351, 352, 355,
     .     356, 357, -1, -3, 2/
      data n28, n26 /28, 26/, n232 /232/, n39 /39/, n367 /367/
c

c
C=======================================================================
C
C					Get LISTF.
      JJ = J
      GO TO (10,20,30,40,50,60,80,90,100,110,120,130,
     1       210,220,230,240,250,260,270,280,290,291,
     2       292,293,300,310,320,330,331,332), JJ
      call oerror(n120, m3, 'STORES')
C
C---------------------------------------------------------------------
C   				   Store PROC source and object	code.
C---------------------------------------------------------------------
C					Store object code.
 10   continue
      L=LLOCAT(fshort(AP+2),K,LPGM)
      LPGM=L
      L=L+1
      LISTL=1
      tempstring = lastline
      ibyte = min( lastblnk(tempstring), kbptr-1)
c     Because of processing by a preparser like AMTHPARSER, must now use the
c     input string stored in HIST as opposed to that in KARBUF or CBUFF.
c     Also, the 'case' of the input string is preserved.
c
      ii = iwpc(ibyte) + 1
      K(L)=LLOCAT(II,LISTF,LISTL)
      L=L+1
      CALL COPY	(AP,A,K(L))
C					Store source code.
      listf(listl) = ibyte
      LISTL=LISTL+1
      call copy(ibyte, itempstring, listf(listl) )
      GO TO 99
C---------------------------------------------------------------------
C					STORE -	all memory.
C---------------------------------------------------------------------
 20   CONTINUE
c
      if (mode .ne. 0) call oerror(n17, m1, 'STORE')
c
      CALL GETFLD
      IF (TYPE.ne.11) call oerror(n29, m1, 
     .		'STORE: One needed and must be a number')
c
      NMEM=X(1)
      IF (NMEM.lt.1.or.NMEM.gt.3) call oerror(n30, m1, 'STORE')
c
      call openms(imemory, memory, n1, ier)
      if (ier .ne. 0) then
	call oerror(n352, n0, 'STORE: ' // cmemory)
	goto 21
      endif
C					   Store K array.
      ILOC = NMEM*(KBLK+kxblk+LBLK)+1
      call writms(imemory, iloc, long(kblk), k, ier)
      IF (IER.ne.0) then
	call oerror(n355, n0, 'STORE: ' // cmemory)
	goto 21
      endif
C					   Store Kx array.
      iloc = iloc + kblk
c				set vers before storing, reset after
      oldvers = vers
      vers = version
      call writms(imemory, iloc, long(kxblk), kx, ier)
      vers = oldvers
      IF (IER.ne.0) then
	call oerror(n355, n0, 'STORE: ' // cmemory)
	goto 21
      endif
C					   Store LISTF array.
      ILOC = ILOC + KxBLK
      call writms(imemory, iloc, long(lblk), listf, ier)
      IF (IER.ne.0) call oerror(n355, n0, 'STORE: ' // cmemory)
c
21    call closms(imemory, ier)
      IF (IER.ne.0) call oerror(n356, n0, 'STORE: ' // cmemory)
      GO TO 99
C---------------------------------------------------------------------
C					RESTORE	- all memory.
C---------------------------------------------------------------------
 30   continue
c
      if (mode .ne. 0) call oerror(n17, m1, 'RESTORE')
c
      CALL GETFLD
      IF (TYPE.ne.11) call oerror(n29, m1, 
     .		'RESTORE: One needed and must be a number')
c
      NMEM=X(1)
      IF (NMEM.lt.1.or.NMEM.gt.3) call oerror(n30, m1, 'RESTORE')
c
      call openms(imemory, memory, n1, ier)
      if (ier .ne. 0) then
	call oerror(n352, n0, 'RESTORE: ' // cmemory)
	goto 31
      endif
c
C					   Restore K array.
      ILOC = NMEM*(KBLK+kxblk+LBLK)+1
      call readms(imemory, iloc, long(kblk), k, ier)
      IF (IER.ne.0) then
	call oerror(n357, n0, 'RESTORE: ' // cmemory)
	goto 31
      endif
C					   Restore Kx array.
      ILOC = ILOC + KBLK
      call readms(imemory, iloc, long(kxblk), kx, ier)
      IF (IER.ne.0) then
	call oerror(n357, n0, 'RESTORE: ' // cmemory)
	goto 31
      endif
c					reset vers
      if (vers .lt. 1) vers = vers * 1000.0
C					   Restore LISTF array.
      ILOC = ILOC + KxBLK
      call readms(imemory, iloc, long(lblk), listf, ier)
      IF (IER.ne.0) call oerror(n357, n0, 'RESTORE: ' // cmemory)
c
31    call closms(imemory, ier)
      IF (IER.ne.0) call oerror(n356, n0, 'RESTORE: ' // cmemory)
      GO TO 99
C---------------------------------------------------------------------
C					List a Procedure/Alias.
C---------------------------------------------------------------------
   40 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'LIST')
c
      CALL GETFLD
c
      IF (TYPE.eq.3) then
        IX = 1
        LINK = TAG
  42    L=LINK
        LINK = K(L)
	 IT = K(L+1)
	 IF (IT.EQ.0) GO TO 42
	    LL = IT+1
	    LLL	= IWPC(LISTF(IT)) + LL - 1
	    WRITE(outbuf,401,IOSTAT=IER) IX, (LISTF(I), I = LL,LLL)
  401 	    FORMAT (1x, I4, 2X, 35A2)
	    CALL PWRITE	(ioutbuf,n80)
	    IX = IX+1
	    IF (LINK.NE.0) GO TO 42
c
      else if (type .eq. 6) then
c
	 ll = locsym+4
	 lll = k(locsym+1)/16 + ll - 1
	 l = k(1)
43	 if (k(l+2) .eq. tag .and. mod(k(l+1),16) .ne. type) then
		ix = l+4
		ix1 = k(l+1)/16 + ix - 1
		write(outbuf, 431,iostat=ier) (k(i),i=ll,lll),' -',
     .					    '> ',(k(i),i=ix,ix1)
431		format(80a2)
	        CALL PWRITE(ioutbuf,n80)
	 else
		l = k(l)
		if (l .ne. 0) goto 43
	 endif
c
      else
         call oerror(n11, m1, 'LIST')
      endif
c
      GO TO 99
C---------------------------------------------------------------------
C					Report CORE utilization.
C---------------------------------------------------------------------
50    continue
      if (mode .ne. 0) call oerror(n17, m1, 'CORE')
      call memusage
      GO TO 99
C---------------------------------------------------------------------
C					Scratch	a Verb/Adverb.
C---------------------------------------------------------------------
60    continue
      if (mode .ne. 0) call oerror(n17, m1, 'SCRATCH')
c
      CALL GETFLD
c
      if (type .eq. 0) call oerror(n2, m1, 'SCRATCH')
c
      lscratch = l
      if (k(7) .gt. lscratch) call oerror(n6, m1, 'SCRATCH')
c     Make sure it is not a built-in operator/operand.
c
      if (type .eq. 1 .or. type .eq. 2 .or. type .eq. 7 .or. 
     .	  type .eq. 8) then
	irtn = unused(k, n1, fshort(-tag), names, numnames)
      else if (type .eq. 3 .or. type .eq. 6) then
	irtn = unused(k, n1, tag, names, numnames)
	if (type .eq. 3) irtn = irtn + 
     .		unalias(k, n1, tag, names(irtn+1),fshort(numnames-irtn))
      else 
	call oerror(n6, m1, 'SCRATCH: Symbol-type cannot be scratched')
      endif
c
      if (irtn .ne. 0) then
c
	i3 = iwpc(nkar)
	call copy(i3, k(l+4), ioutbuf)
c
	write(istderr,*) ' '
	write(istderr,*) 'The following PROCEDURES/ALIASES use ',
     .				outbuf(1:nkar), ':'
	if (irtn .gt. numnames) write(istderr,*)
     .		 '(Only the first ', numnames, ' will be listed.)'
	write(istderr,*) ' '
	i = min(irtn,numnames)
619	write(istderr,*) ((names(ll), ll = j, min(i,j+5) ), j = 1, i, 6)
	write(istderr,*) ' '
	write(istderr,*) 'Cannot SCRATCH ', outbuf(1:nkar)
	write(istderr,*) ' '
c
      else
c
C       Search for the operator/operand.
        L = 1
  62    LAST=L
        L=K(L)
        IF (l .ne. lscratch) GO TO 62
c
        IF (K(L).eq.0) then
C	  Last symbol entry.
c
	   K(9)=LAST
	   K(LAST)=0
        else
C	   Middle symbol entry.
c
	   K(LAST) = K(L)
        endif
C       Delete symbol from table.
c
      endif
c
      GO TO 99
C-----------------------------------------------------------------------
C                                  BATCH 
C-------------------------------------------------------
   80 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'BATCH')
c
      call nextfield(cnfile)
      call batch(cnfile)
c
      goto 99
c
c-------------------------------------------------------
c                                  FILES
c-------------------------------------------------------
  90  continue
      if (mode .ne. 0) call oerror(n17, m1, 'FILES')
      call files
      goto 99
c-------------------------------------------------------
c                                   CHGPRJ
c-------------------------------------------------------
  100 continue
      if (mode .ne. 0) call oerror(n17, m1, 'CHGPRJ')
      call nextfield(project)
      lrtn = changeproj(project)
      goto 99
c-------------------------------------------------------
c				    SYSTEM
c-------------------------------------------------------
  110 continue
      if (mode .ne. 0) call oerror(n17, m1, 'SYSTEM')
      irtn = system(cbuff(kbptr:min(nbytes,karlim)))
      if (irtn .ne. 0) call oerror(n9,n0,'SYSTEM')
      kbptr = max(karlim, nbytes)
      goto 99
c----------------------------------------------------
c                                    DOCUMENT
c----------------------------------------------------
120   continue
      if (mode .ne. 0) call oerror(n17, m1, 'DOCUMENT')
      ilen3 = lastblnk(cbuff(kbptr:min(nbytes,karlim)))
      call pwrite(cbuff(kbptr:min(nbytes,karlim)), ilen3)
      kbptr = max(karlim, nbytes)
      goto 99
c---------------------------------------------------
c        				UNDO
c---------------------------------------------------
130   continue
      if (mode .ne. 0) call oerror(n17, m1, 'UNDO')
      call undo
C-----------------------------------------------------------------------
C					 EDIT	  210
C-----------------------------------------------------------------------
  210 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'EDIT')
      irtn = system('edit.exe ' // cbuff(kbptr:min(nbytes,karlim)))
      if (irtn .ne. 0) call oerror(n9,n0,'EDIT')
      kbptr = max(karlim, nbytes)
      GO TO 99
C--------------------------------------------------------------------
C                                    TYPE  
C---------------------------------------------------------------------
  220 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'TYPE')
      irtn=system('more.exe ' // cbuff(kbptr:min(nbytes,karlim)))
      if (irtn .ne. 0) call oerror(n9,n0,'TYPE')
      kbptr = max(karlim, nbytes)
      GO TO 99
C---------------------------------------------------------------------
C                                    DIR
C--------------------------------------------------------------------
  230 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'DIR')
      irtn=system('ls.exe ' // cbuff(kbptr:min(nbytes,karlim)))
      if (irtn .ne. 0) call oerror(n9,n0,'DIR')
      kbptr = max(karlim, nbytes)
      GO TO 99
C--------------------------------------------------------------------
C                                  SYSHELP
C--------------------------------------------------------------------
  240 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'SYSHELP')
      irtn=system('man.exe ' // cbuff(kbptr:min(nbytes,karlim)))
      if (irtn .ne. 0) call oerror(n9,n0,'SYSHELP')
      kbptr = max(karlim, nbytes)
      GO TO 99
C---------------------------------------------------------------
C                                  LASER
C---------------------------------------------------------------
  250 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'LASER')
      irtn=system('printit ' // printer // ' text ' // 
     .             cbuff(kbptr:min(nbytes,karlim)))
      if (irtn .ne. 0) call oerror(n9,n0,'LASER')
      kbptr = max(karlim, nbytes)
      GO TO 99
C--------------------------------------------------------------------
C               COMMENTS
C--------------------------------------------------------------------
 260  CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'COMMENTS')
      irtn=system('comments.exe ' // program(2:2) )
      if (irtn .ne. 0) call oerror(n9,n0,'COMMENTS')
      GOTO 99
C----------------------------------------------------------------
c           PRCDIR
c--------------------------------------------------------------------
270   continue
      if (mode .ne. 0) call oerror(n17, m1, 'PRCDIR')
      irtn=system('prcdir2.exe')
      if (irtn .ne. 0) call oerror(n9,n0,'PRCDIR')
      goto 99
C----------------------------------------------------------------
c           MENUS
c--------------------------------------------------------------------
280   continue
      if (mode .ne. 0) call oerror(n17, m1, 'MENUS')
C     Make sure it is in EXEC mode
c
      do 281 i = 1, iinptr
         if (iinlist(i,2) .eq. imenuin)
     .     call oerror(n26, m1,' ')
 281  continue
c			make sure there aren't any current menus open
c
      open(unit=imenuout, file=cmenuout, status='unknown', 
     .     access='sequential',iostat=irtn)
      if (irtn .eq. 0) rewind(imenuout,iostat=irtn)
      if (irtn .eq. 0) call menus(irtn)
c
      if (irtn .eq. 0) irtn = system("makemenu.exe " // cmenuin //  
     .                  " " //cmenuout // " " // program(2:2) //  
     .			" " //procid)
      if (irtn .ne. 0) call oerror(n351, m1, 'MENUS')
c
      call openread(istat4, cmenuin(1:lastblnk(cmenuin)) // '\0')
      if (istat4 .lt. 0) then
	call oerror(n351, n0, 'MENUS')
	call donemenu
	goto 99
      endif
C     Create menus, open menus input file
c
      if (iinptr .ge. 50) then
c			no more room in iinlist, should NEVER happen
         call oerror(n28, n0, 'MENUS')
         call donemenu
         goto 99
      endif
c
      iinptr = iinptr + 1
      iinlist(iinptr, 1) = istat4
      iinlist(iinptr, 2) = imenuin
      cinlist(iinptr) = cmenuin
      iinunit = iinlist(iinptr, 1)
      iintype = iinlist(iinptr, 2)
c     Store away the current Input unit number for use after MENUS
c     are no longer wanted.
c			istat4 is the unix file descriptor which we'll
c			treat as if it were the unit number in iinlist
c
      goto 99
C----------------------------------------------------------------
c           DONEMENU
c--------------------------------------------------------------------
290   continue
      call donemenu
      goto 99
C----------------------------------------------------------------
c           CHNGFILE
c--------------------------------------------------------------------
291   continue
      if (mode .ne. 0) call oerror(n17, m1, 'CHNGFILE')
C     Make sure it is in ECEC mode
c
C     Parse the command line: command file_number file_name (optional)
      call nextfield(command1)
      ii = lastblnk(command1)
      if (ii .eq. 0) then
	call initfiles(zero4)
c	User has not types in any arguments to CHNGFILE so go through
c	Q/A session in INITFILES.

      else
        call uppercase(command1, command2)
c       Get command
c
	if (command2 .eq. 'C') then
            call oerror(n31, m1, 'CHNGFILE:  Ambiguous symbol')
	else if (index("CHANGE", command2(1:ii)) .eq. 1) then
	    command2 = "CHANGE"
	else if (index("SUBTRACT", command2(1:ii)) .eq. 1) then		
	    command2 = "SUBTRACT"
	else if (index("CREATE", command2(1:ii)) .eq. 1) then		
	    command2 = "CREATE"
	else 
            call oerror(n31, m1, 'CHNGFILE')
	endif
c
	inum = 0
        if (command2 .eq. 'CHANGE' .or. command2 .eq. 'SUBTRACT' .or.
     1    command2 .eq. 'CREATE' ) then
         call getfld
         if (type.eq.1 .or. type.eq.8 .or. type.eq.11) then
c		scalar, pointer or entered value, use tag returned by getfld
            inum = c(tag)
         else
c		too complicated to deal
            call oerror(n31, m1, 'CHNGFILE')
         endif
c	 Get file_number
c
	 if (command2 .eq. 'CHANGE' .or. command2 .eq. 'CREATE') 
     1		call nextfield(filename)
c         get file name
c
        endif
        irtn = chngfile(command2, inum, filename)
        if (irtn .ne. 0) call oerror(n31, m1, 'CHNGFILE')
c
      endif
c
      goto 99
C---------------------------------------------------------------
C                                  IDENTIFY
C---------------------------------------------------------------
  292 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'IDENTIFY')
      irtn=system('identify.exe ' // cbuff(kbptr:min(nbytes,karlim)))
      if (irtn .ne. 0) call oerror(n9,n0,'IDENTIFY')
      kbptr = max(karlim, nbytes)
      GO TO 99
C---------------------------------------------------------------
C                                  KILLGRPH
C---------------------------------------------------------------
  293 CONTINUE
      if (mode .ne. 0) call oerror(n17, m1, 'KILLGRAPH')
      wintype = 'S'
      if (termtype .eq. 'xwindow') wintype = 'X'
      irtn = system('killgraph.exe ' // cgraphin // ' ' // cgraphout //
     .  	     ' ' // procid // ' ' // program(1:1) // 
     .               ' ' // wintype)
      if (irtn.ne. 0) call oerror(n9,n0,'KILLGRAPH')
      GO TO 99
c
C---------------------------------------------------------------------
C		   E  X	 I  T		
C---------------------------------------------------------------------
300   continue
      if (mode .ne. 0) call oerror(n17, m1, 'EXIT')
      call exitpops(n0)
c
C---------------------------------------------------------------------
C		   W   I   P   E		
C---------------------------------------------------------------------
310   continue
      if (mode .ne. 0) call oerror(n17, m1, 'RESTART')
c
      call donemenu
      call closeall
c
      do 1009 i = 1, iinptr
         if (iinlist(iinptr, 2) .eq. ibatchin) then
            call filecomp(cinlist(iinptr), fullname)
            if (opened(fullname)) then
               close (iinlist(iinptr,1),iostat=irtn)
               lunit = iinlist(iinptr, 1)
               lunit = ioput(lunit)
c			ignore ioput errors, we don't really care
            endif
         endif
1009  continue
c
      do 1109 i = 1, nfiles
	if (ffile(i,1) .gt. 0) close(ffile(i,1),iostat=istat)
1109	continue
c
      if (opened(csetupin)) close(isetupin,iostat=irtn)
      if (opened(cmemory)) call closms(imemory, istat)
      if (opened(coutgrph)) close(ioutgrph,iostat=istat)
      if (opened(couttxt)) close(iouttxt,iostat=istat)
      close(ishelpin,iostat=istat)
      close(iiotmp,iostat=istat)
c
      call filecomp(clogout,fullname)
      if (dolog) write(ilogout, 1030, iostat=irtn)
     .		'# Command logging OFF -- ', zdatetime()
 1030 format(2a)
      if (opened(fullname)) close(ilogout,iostat=irtn)
      dolog = .false.
c
      call filecomp(ccube,fullname)
      if (opened(fullname)) close(icube,iostat=irtn)
      call filecomp(cprtout,fullname)
      if (opened(fullname)) close(iprtout,iostat=istat)
c
      close(ishelpin,iostat=irtn)
      close(iiotmp,iostat=irtn)
c
      CALL WIPE
c
C---------------------------------------------------------------------
C		   COMPRESS		
C---------------------------------------------------------------------
320   continue
      if (mode .ne. 0) call oerror(n17, m1, 'COMPRESS')
      oldk3 = k(3)
      oldkx3 = kx(3)
      oldlistf3 = listf(3)
c     Store away old array usage
c
      call compress
c
c     Now report on savings
c
      write(outbuf,3205,iostat=ier) 
3205  format(' ')
      call pwrite(ioutbuf,n80)
c
      write(outbuf,321,iostat=ier) 
321   format(13x,'SPACE SAVED FOR:')
      call pwrite(ioutbuf,n80)
c
      write(outbuf,322,iostat=ier) 
322   format('     PROGRAMS   VARIABLES     SOURCE')
      call pwrite(ioutbuf,n80)
c
      write(outbuf,323,iostat=ier) oldk3-k(3), oldkx3-kx(3), 
     .				   oldlistf3-listf(3)
323   format(7x,3(i5,7x))
      call pwrite(ioutbuf,n80)
c
      oldk3 = nint(100.*float(oldk3-k(3))/float(oldk3))
      oldkx3 = nint(100.*float(oldkx3-kx(3))/float(oldkx3))
      oldlistf3 = nint(100.*float(oldlistf3-listf(3))/float(oldlistf3))
c
      write(outbuf,324,iostat=ier) oldk3, oldkx3, oldlistf3
324   format(6x,'(',i5,'%',6x,i5,'%',6x,i5,'%)')
      call pwrite(ioutbuf,n80)
c
      write(outbuf,3205,iostat=ier) 
      call pwrite(ioutbuf,n80)
c
      goto 99
c
C---------------------------------------------------------------------
C		   OLFILES
C---------------------------------------------------------------------
330   continue
c			currently only applicable to tucson on-line files
c			but there must BE an online data connection
      if (.not. online) call oerror(n367, m1, 'OLFILES')
      if (onlinesite() .ne. 1) call oerror(n232, m1, 'OLFILES')
c
      call olfiles()
c
      goto 99
C---------------------------------------------------------------------
C		   WHATIS
C---------------------------------------------------------------------
331   continue
c
      call getfld
c
      tempstring = ' '
      query = ' '
      if (type .eq. 11) then
	tempstring = cbuff(kbptr-nkar:kbptr-1)
      else if (type .eq. 14) then
	tempstring = cix(1:nkar)
      else if (type .ge. 1 .and. type .le. 15) then
	i3 = k(l+1)/16
	call copy(i3, k(l+4), itempstring)
      else
	tempstring = cbuff(kbptr-nkar:kbptr-1)
      endif
c
      if (type .eq. 1) then
        query = ' => Scalar Adverb'
      else if (type .eq. 2) then
        query = ' => Array Adverb'
      else if (type .eq. 3) then
        query = ' => Procedure'
      else if (type .eq. 4) then
	if (tag .ge. idhfunc1 .and. tag .le. idhfunc2) then
           query = ' => Internal Array Verb'
	else if (tag .ge. ifunct .and. tag .lt. igenop) then
           query = ' => Function Verb'
	else
           query = ' => Regular Verb'
	endif
      else if (type .eq. 5) then
        query = ' => Pseudo Verb'
      else if (type .eq. 6) then
        query = ' => Alias'
      else if (type .eq. 7) then
        query = ' => String Adverb'
      else if (type .eq. 8) then
        query = ' => Keyword or Pointer Adverb'
      else if (type .eq. 11) then
        query = ' => Real Constant'
      else if (type .eq. 14) then
        query = ' => String Constant'
      else if (type .eq. 15) then
        query = ' => Logical Constant'
      else 
        query = ' => Unknown Symbol'
      endif
c
      tempstring = tempstring(1:lastblnk(tempstring)) // query
c
      call pwrite(itempstring, 80)
c
      goto 99
C---------------------------------------------------------------------
C		   CHNGVER
C---------------------------------------------------------------------
332   continue
c			Tucson only, must be in execute mode, online avail
      if (mode .ne. 0) call oerror(n17, m1, 'CHNGVER')
      if (.not. online) call oerror(n367, m1, 'CHNGVER')
      if (onlinesite() .ne. 1) call oerror(n232, m1, 'CHNGVER')
c			Parse the command line: CHNVER type (optional)
      call getfld
c
      if (type .eq. 0) then
c			no arguments on the command line
         iftype = -1
      else if (type .eq. 1 .or. type .eq. 8 .or. type .eq. 11) then
c			scalar, pointer or const, getfld places it in c array
         iftype = nint(c(tag))
      else if (tag .eq. iuplus .or. tag .eq. iuminus) then
c			or, it could be a signed one of these
c			this is as complicated as we'll allow
         sign = 1
         if (tag .eq. iuminus) sign = -1
         call getfld
         if (type .eq. 1 .or. type .eq. 8 .or. type .eq. 11) then
            iftype = nint(c(tag)) * sign
         else
c			bad syntax, give an error
            call oerror(n29, m1,
     .         'CHNGVER: must be a SCALAR, POINTER, or REAL CONSTANT')
         endif
      else
c			bad syntax, give an error
         call oerror(n29, m1,
     .      'CHNGVER: must be a SCALAR, POINTER, or REAL CONSTANT')
      endif
c			make sure at this point that there is NOTHING
c			remaining on the command line
      call getfld
      if (type .ne. 0 .or. tag .ne. 0) then
         call oerror(n39, m1, 'CHNGVER')
      endif
c
      call chngver(iftype)
      goto 99
c---------------------------------------------------
C
   99 CONTINUE
      RETURN
      END
c
