      SUBROUTINE GTLINE
C-------------------------------------------------------------------------------
C  @(#)gtline.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
      integer*2 ktlp, ktlpsv, llocat, ibyte, lastblnk, istat
      integer*2 n21, n351, n355, n0, fshort, finish, nt, n1, n107
c
      INCLUDE 'smstuf.inc'
      include 'core.inc'
      include 'stk.inc'
      include 'cio.inc'
c
      data n21, n107, n351, n355, n0, n1 /21, 107, 351, 355, 0, 1/
C
C=======================================================================
C
C					Read a line, skip comments.
1     KKT(1) = 0
      KKT(2) = 0
      KKT(3) = 11
      KKT(4) = 0
      KKT(5) = 140
      KKT(10) = 4
      KTLP = 2
c
 2    if (iintype .eq. imenuin .and. mode .eq. 0) call menus(istat)
      if (istat .ne. 0) call oerror(n351, n0, ' ')
c     Only update the Menusout file when the program is in menus mode
c     and it is not in compile mode.
c
      call checkfperror
c     Print out any Floating point errors generated between last input line
c     and the one about to be read in.  
c
      CALL PREAD (KARBUF)
c
      call histparser(karbuf)
c     Do history substitutions
c
c					echo karbuf out to the log file
      if (dolog .and. mode .eq. 0 .and. 
     .	  (iintype .eq. istdin .or. iintype .eq. isetupin) ) then
         ibyte = lastblnk(lastline)
	 if (ibyte .gt. 0) write(ilogout, 1110, iostat=istat) lastline(1:ibyte)
 1110    format(a)
         if (istat .ne. 0) call oerror(n355, n0, clogout)
      end if
c
      KBPTR = 1
      errcode = 0.0
C					   Compile a line.
      CALL POLISH
c 
      IF ((AP.EQ.0).OR.(MODE.NE.0)) then
C					Mode 1 -> Compilation.
	 if (mode .eq. -2) then
	   lpgm = 2
	   iedit = 0
	   llit = 0
	   mode = 0
	 else if (mode .eq. -1) then
	   lpgm = 2
	   mode = iedit
	   iedit = 0
	 endif
         goto 2
c
      else
C					Mode 0 -> Immed. execute.
	 KTLPSV	= KTLP
	 L = LLOCAT(fshort(AP+2),KKT,KTLP)
	 KKT(KTLPSV) = KKT(KTLPSV) + KT	- 1
C					Address	relative to K array.
	 L = L+2
c
	 if (ap .gt. 150-l+1) call oerror(n107, n1, 
     .		'Too many commands on the input line') 
	 CALL COPY (AP,A(1),KKT(L))
c
         nt = finish(k, kt + l - 3, .true.)
         IF (NT .eq. 1) then
	   call oerror(n21,n1,'Too many IF, WHILE, or FOR constructs')
         else if (nt .eq. 2) then
	   call oerror(n21,n1,'Too many END, THEN, or ELSE statements')
         else if (nt .eq. 3) then
	   call oerror(n21,n1,'No closing END statement')
         endif
c
	 CALL INTERP (KKT(2))
	 goto 1
c
      endif
c
      END
