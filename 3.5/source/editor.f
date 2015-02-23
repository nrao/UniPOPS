      SUBROUTINE EDITOR	(J)
C-------------------------------------------------------------------------------
C  @(#)editor.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C   POPS SOURCE	CODE EDIT FUNCTIONS
C-------------------------------------------------------------------------------
      integer*2 j, iblank, kedit, karat, jj, ll, i, nt, finish, lp, ip,
     .		lp1, lp2, lcount
      integer*2 n0, n1, n6, n11, n12, n13, n17, n21, n40, n120, m1, m3
      real*4 epsiln
      logical lhunt
c
      data n0, n1, n6, n11, n12, n13, n17, n21, n40, n120, m1, m3
     +     /0, 1, 6, 11, 12, 13, 17, 21, 40, 120, -1, -3/
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      include 'cio.inc'
c
      save lp, ip
c
      include 'tags.inc'
c
      DATA EPSILN/1.0E-6/
      DATA IBLANK/' '/,	KEDIT/'	:'/
      DATA KARAT/' >'/
C
C=======================================================================
C
      JJ = J
      GO TO (10,50), JJ
      call oerror(n120,m3,'EDITOR')
C---------------------------------------------------------------------
C					E D I T	 <name>	 <line>
C---------------------------------------------------------------------
  10  if (mode .ne. 0) call oerror(n17,n1,'')
c
      iedit = mode
      mode = -1
      symp = 0
c
      CALL GETFLD
C					   Procedure name.
      IF (TYPE.ne.3) call oerror(n11,n1,'')
      if (k(7) .gt. locsym) call oerror(n6, n1, '')
      IP=TAG
      if (ip .le. 0) call oerror(n120,m3,'')
      NAMEP=LOCSYM+2
      lclstart = locsym + 3
C					   Line	number.
      CALL GETFLD
      IF (TYPE.ne.11) call oerror(n12,n1,'')
c
      if (x(1) .lt. 1.+epsiln) call oerror(n40, n1, 
     .		'Cannot POPSEDIT the first line of a PROCEDURE')
c
      LL=X(1)
      DO 15 I=1,LL
	 LP=IP
	 IP=K(IP)
	 IF (IP.EQ.0) GOTO 20 
  15	 CONTINUE
  20  IF (IP.NE.0) IP=K(IP)
      if (ip .eq. 0) call oerror(n40, n1, 
     .		'Cannot POPSEDIT the last line of a PROCEDURE')
      XX=LL
c
      if (k(k(lp)+2) .eq. iprotect .and. ABS(X(1)-XX).le.EPSILN) 
     .	call oerror(n40, n1, 'Cannot POPSEDIT a decleration')
c
      IF (ABS(X(1)-XX).GT.EPSILN) LP=K(LP)
      LPGM=LP
      AP=0
c
      lcllast = lclstart
101   if (k(lcllast) .ne. 0) then
	lcllast = k(lcllast)
	goto 101
      endif
c     Find the location of the last local operand.
c
c     Now, step thru the proc to find out whether or not the user is
c	editing between or after declerations.  Also, prepare list of
c	operands already in use by procedure (in case we need to trap 
c	the later desire to create a local symbol from one already
c	used as a global).
c 
      lp1 = k(namep)
      defexec = -1
      lcount = 0
120     if (k(lp1) .ne. 0) then
	   lp2 = lp1 + 2
c
	   lcount = lcount + 1
	   if (lcount .le. ll+2 .and. lcount .gt. 2) then
		if (k(lp2) .ne. iprotect .and. k(lp2) .ne. iomega) then
		  if (defexec .eq. -1) then
		    defexec = 1
		  else
		    defexec = 0
		  endif
		endif
	   endif 
c	   Set DEFEXEC according to whether the edit is occuring before (1),
c	   between (-1), or after (0) declerations.
c
130	   if(k(lp2) .eq. iomega) then
		lp1 = k(lp1)
		goto 120
	   else
		if (k(lp2) .lt. -200 .and. 
     .		    .not. lhunt(-k(lp2)) ) call push(symstack, symp, -k(lp2))
c		Place into SYMSTACK array the tags of all operands in use 
c		by the procedure.
c
		lp2 = lp2 + 1
		goto 130
	   endif
	else
	   if (lcount .le. ll+2 .and. defexec .lt. 0) defexec = 1
c	   Take care of the case of an empty procedure.
c
	endif
c
      mode = 1
      llit = 1
      CALL PROMPT (KEDIT,IPT)
c
      GO TO 99
C---------------------------------------------------------------------
C	       E  N  D	E  D  I	 T
C---------------------------------------------------------------------
  50  if (mode .ne. 1 .or. llit .ne. 1) call oerror(n13,n1,'')
c
      mode = -2
      llit = 0
      lclstart = 0
      lcllast = 0
      defexec = 0
      symp = 0
      CALL PROMPT (KARAT,IPT)
c
      AP=0
      K(LPGM)=IP
      lpgm = 2
c
      NT=0
      IF (NAMEP.lt.200.or.NAMEP.gt.KXORG) call oerror(n13,n1,'')
c     200 = tag of first item in linked list
c
      L=NAMEP-2
      IF (L.eq.0) call oerror(n13,n1,'')
c
      NAMEP=0
c
      nt = finish(k, k(l+2), .false.)
c
      IF (NT .eq. 1) then
	call oerror(n21,n0,'Too many IF, WHILE, or FOR constructs')
      else if (nt .eq. 2) then
	call oerror(n21,n0,'Too many END, THEN, or ELSE statements')
      else if (nt .eq. 3) then
	call oerror(n21,n0,'No closing END statement')
      endif
c
      CALL PWRITE (IBLANK,n1)
      CALL PUSH (A,AP,iomega)
      IF (LEVEL.ne.0) call oerror(n21,n0,'')
      call memusage
c
      mode = 0
c
      GO TO 99
c
C----------------------------------------------------------------------
   99 CONTINUE
      RETURN
      END
