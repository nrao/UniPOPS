      SUBROUTINE PSEUDO	(J)
C-------------------------------------------------------------------------------
C @(#)pseudo.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C   PSEUDO handles procedure and array declarations and the FINISH
C   operator.
C----------------------------------------------------------------------
c
      integer*2 j, i1, karat, kolon, iblank,
     .          jj, narg, n, llocat, ll, i, iwpr, nt, ier, 
     .          nsize, i7, ls, ndim, np, ioldtag, mm, lli, isize,
     .		oldtag, finish, iwpc, kbptrold, numvars
      integer*2 n0, n1, n2, n6, n14, n16, n17, n18, n19, n21, n22,
     .          n25, n36, n41, n80, n120, m3, n3, procname(40), 
     .		procchar, n15, n8, n29, n42, nsizeold, ndimold
      real*4 r4blank, valueold, valuenew
      logical exists, allok, global, lhunt
      character*1 answr
      character*60 ckpak
      character*10 names(400)
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      include 'cio.inc'
c
      equivalence (ckpak, kpak)
c
      include 'tags.inc'
c
      DATA I1/1/, I7/7/
      DATA KARAT/' >'/,	KOLON/'	:'/, IBLANK/' '/, r4blank/'    '/
      data n0, n1, n2, n6, n14, n16, n17, n18, n19, n21, n22,
     .     n25, n36, n41, n80, n120, m3, n3, n15, n8, n29, n42
     .     /0, 1, 2, 6, 14, 16, 17, 18, 19, 21, 22, 25, 36, 41, 80, 120,
     .      -3, 3, 15, 8, 29, 42/
C
C=======================================================================
c
      global = .false.
c
      JJ = J
100   GO TO (151,155,230,152,158,200,210,220), JJ
      call oerror(n120,m3,'PSEUDO')
c
C------------------------------------------------------------------------
C					P R O C	E D U R	E.
C------------------------------------------------------------------------
 151  if (mode .ne. 0) call oerror(n17,n1,'')
c
      MODE=2
c
c     Get name of procedure
c
      CALL GETFLD
c
      if (type .eq. 3) then
c
            IF (locsym.lt.K(7)) call oerror(n6,n1,'')
c	    Cannot overwrite a builtin procedure
c
	    write(istderr,*) ' '
	    write(istderr,991) 'Procedure already exists...  '
991	    format(1x,a)
	    write(istderr,992) 'Do you want to overwrite (y or n)? '
992	    format(1x,a,$)
	    read(istdin,993) answr
993	    format(a)
	    write(istderr,*) ' '
	    if (answr .eq. 'n' .or. answr .eq. 'N') call oerror(n3, n1, '')
	    exists = .true.
c	    Protects against overwriting an existing procedure
c
            NAMEP=LOCSYM+2
            oldtag = k(namep)
c           If redifinig a procedure, oldtag will be the old tag of the
c           old code.  Needed when relinking any other procedures that may have
c           called the procedure that is being overwritten.
c
      else if (type .eq. 0) then
c
	    exists = .false.
c
      else
c
	    call oerror(n36, n1, '')
c	    Name of procedure already exists as that of an adverb or verb
c
      endif
c
      procchar = nkar
      nkar = iwpc(nkar)
      call copy(nkar, kpak, procname)
c     Store away name for later use.
c
      NARG=0
c
c     Get arguments, if any (triggered by a left paren.)
c
      CALL GETFLD
c
      IF (TAG.EQ.ilparen) then
c
1510	call getfld
        IF (TAG.ne.irparen) then
           IF (TAG.EQ.icomma) goto 1510
           IF (TAG.EQ.i0) 
     .		call oerror(n18,n1,'No closing ) in argument list')
c          Must have a closing paren; if none, an error
c
	   if (type .eq. 11 .or. type .eq. 14) 
     .		call oerror(n18, n1, 'Bad argument list')
c
	   do 1511 i = 1, narg
		if (ckpak(1:nkar) .eq. names(i)) call oerror(n41, n1,
     .			 'Bad argument list')
1511		continue
c
	   call push(tstack, narg, nkar) 
	   names(narg) = ckpak(1:nkar)
c
           GO TO 1510
	endif
c
      endif
c
      if (tag .ne.  i0 .and. tag .ne. icolon .and.
     .    tag .ne. irparen) call oerror(n18, n1, '')
c
      if (.not. exists) then
	    call creatsym(n3, procname, procchar,
     .				      n0, n0, locsym)
            NAMEP=LOCSYM+2
      endif
c
c     Process arguements
      N=NARG+4
      LL=LLOCAT(N,K,LPGM)
c
      lclstart = locsym + 3
      k(lclstart) = 0
      lcllast = locsym + 3
c
      LPGM=LL
      K(NAMEP)=LL
      LL=LL+2
c
      if (narg .gt. 0) then
        do 1513 j = 1, narg
	  nkar = tstack(j)
	  ckpak = names(j)
     	  call creatspc(n2, tag)
      	  c(tag) = 0.0
      	  call creatloc(n1, kpak, nkar, tag, n0, locsym)
          k(ll) = -tag
	  ll = ll + 1
1513	  continue
         K(LL)=igetargs
         LL=LL+1
      endif
      K(LL)=iomega
c
      if (exists) call relink(oldtag, k(namep), n0, n1)
c     If redefing a procedure, relink all procs. that called the
c     old procedure with the new one.
c
      mode = 1
      llit = 0
      defexec = 1
      CALL PROMPT (KOLON,IPT)
c
      GO TO 99
c
C---------------------------------------------------------------------
C					F I N I	S H.
C---------------------------------------------------------------------
 155  continue
      if (mode .ne. 1) call oerror(n16,n1,'')
c
      mode = -2
      llit = 0
      lclstart = 0
      lcllast = 0
      defexec = 0
      symp = 0
      CALL PROMPT (KARAT,IPT)
c
      NT=0
c
      IF (IDEBUG.GT.0) then
	WRITE (outbuf,19901,IOSTAT=IER) NAMEP,K(NAMEP)
19901   FORMAT ('NAMEP,K(NAMEP)=',2I10)
	call pwrite(ioutbuf, n80)
      endif
c
      IF (NAMEP.lt.200.or.NAMEP.gt.KXORG) call oerror(n16,n1,'')
c     200 = tag of first item in linked list
c
      L=NAMEP-2
      IF (L.eq.0) call oerror(n16,n1,'')
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
      call bclean
      call push(b,bp,ireturn)
      bpr(bp) = level
      call bclean
c     Add RETURN to stack
c
      if (.not. lsetup) then
         CALL PWRITE(IBLANK,I1)
         call memusage
      endif
c
      goto 99
c
C---------------------------------------------------------------------
C					G L O B A L
C---------------------------------------------------------------------
 230  CONTINUE
c
      if (mode .eq. 1 .and. defexec .eq. 0) call oerror(n42, n1,
     .	'Cannot add declerations among executables') 
c
      call getfld
      if (tag .ge. iarray .and. tag .le. ialias) then
	jj = tag - ipseudo + 1
	global = .true.
	goto 100
      else
	call oerror(n29, n1, '')
      endif
c
      goto 99
c
C---------------------------------------------------------------------
C					A R R A	Y <name> <(n)>.
C---------------------------------------------------------------------
 152  continue
c
      if (mode .eq. 1 .and. defexec .eq. 0) call oerror(n42, n1,
     .		'Cannot add declerations among executables')
c
      iedit = mode
      mode = -1
      kbptrold = kbptr
      allok = .false.
      numvars = 0
c     KBPTROLD = The pointer to the next field in the input line
c     ALLOK = true when we have parsed the input line and no errors have
c	been found; we can then reparse the line and execute what it says
c	If false, then we are still going thru the initial trial parsing.
c
c     Get name of array
1528  CALL GETFLD
c
      IF (TAG.EQ.i0) call oerror(n22,n1,'Must give the name of array ')
c
1532  if (type .eq. 11 .or. type .eq. 14) then
	call oerror(n29, n1, 'Bad symbol name')
c
      else if ( (type .eq. 0) .or.
     .	   (iedit .eq. 1 .and. .not. global .and. .not. local .and.
     .	    .not. lhunt(tag) )  ) then
c
	   do 1531 i = 1, numvars
		if (ckpak(1:nkar) .eq. names(i)) call oerror(n41, n1, 
     .			'Bad set of ARRAY names')
1531		continue
c
	   numvars = numvars + 1
	   names(numvars) = ckpak(1:nkar)
	   procchar = nkar	   
	   exists = .false.
c          Store away name for later use.
c
      else if (lhunt(tag) .and. .not. global .and. 
     .		.not. local)  then
	   call oerror(n3, n1, 'Symbol already in use in procedure')
c
      else if (type .eq. 2) then
	   if (iedit .eq. 1 .and. global .and. local) call oerror(n3, n1, '')
	   exists = .true.
           LS = k(LOCSYM + 3)
	   nsizeold = k(ls)
	   ndimold = k(ls+1)
      	   LLI=ls+2
	   n = 2*ndimold
	   call copy(n, k(lli), cstack)
c	   Array already exists; copy old dimensions to see if new ones are the
c	   same
c
      else 
	   call oerror(n3, n1, '')	
      endif
c
      NDIM=0
      NP=0
c
C     Parse Dimensions.
c
      CALL GETFLD
      IF (TAG.ne.ilparen) call oerror(n19,n1,'')
c
      CALL GETFLD
c
1521  IF (TYPE.ne.11) then
          if (type .eq. 4 .and. (tag.eq.iuplus.or.tag.eq.iuminus)) then
	    ioldtag = tag
	    call getfld
	    if (type .ne. 11) call oerror(n22, n1, 'Dimensions must be numbers')
	    if (ioldtag .eq. iuminus) c(tag) = -c(tag)
          else
            call oerror(n22, n1, 'Dimensions must be numbers')
          endif
      endif
c     Takes care of negative dimensions
c
      NDIM=NDIM+1
      LL=nint(C(TAG))
c
      CALL GETFLD
      IF ((TAG.ne.iloopto).or.(TYPE.ne.4)) then
          MM=LL
          if (mm .le. 0) call oerror(n22,n1,'Number of elements <= 0')
c         Makes sure that the number of elements in the array (MM) is > 0
c
          LL=1
      else
          CALL GETFLD
          IF (TYPE.ne.11) then
            if (type.eq.4 .and. (tag.eq.iuplus.or.tag.eq.iuminus)) then
	      ioldtag = tag
	      call getfld
	      if (type .ne. 11) call oerror(n22, n1, 'Dimensions must be numbers')
	      if (ioldtag .eq. iuminus) c(tag) = -c(tag)
            else
              call oerror(n22,n1,'Dimensions must be numbers')
            endif
	  endif
c	  Takes care of negative dimensions
c
          MM=nint(C(TAG))-LL+1
          if (mm .le. 0) call oerror(n22,n1,'Number of elements <= 0 ')
c         Makes sure that the number of elements in the array (MM) is > 0
c
          CALL GETFLD
      endif
c
      CALL PUSH (TSTACK,NP,LL)
      CALL PUSH (TSTACK,NP,MM)
c
      if (tag .eq. icomma) call getfld
      IF (TAG.NE.irparen) GO TO 1521
c
      NSIZE=1
      DO 1529 I=2,2*ndim,2
 1529	   NSIZE=NSIZE*TSTACK(I)
c
      if (exists) then
c
	   if (nsizeold .ne. nsize .or. ndimold .ne. ndim) 
     .		call oerror(n3, n1, '')
	    do 1530 j = 1, 2*ndim
		if (tstack(j) .ne. cstack(j)) 
     .		      call oerror(n3, n1, '')
1530	     continue
c	     if symbol exists, compare new dims with old.
c
      else if (allok) then
c		
           isize = IWPR(NSIZE)
	   call creatspc(isize, tag)
	   ckpak = names(numvars)
      	   DO 1539 J = tag, tag + nsize - 1
 	       c(J) = 0.0
1539           continue
           if (iedit .eq. 1 .and. .not. global) then
      	     call creatloc(n2, kpak, procchar, tag, n0, locsym)
	   else
      	     call creatsym(n2, kpak, procchar, tag, n0, locsym)
	   endif
c
C          Allocate remaining space.
           LS = locsym + 3
           N = 2*NDIM + 2
           LL = LLOCAT(N,K,LS)
           K(LL) = nsize
           K(LL+1) = NDIM
           LLI=LL+2
           N = 2*ndim
           CALL COPY (N,TSTACK,K(LLI))
c	   If symbol doesn't exist, create a new one (local if compiling a proc).
c
      endif
c
      call getfld
      if (tag .eq. icomma) call getfld
      IF (TAG.ne.i0 .and. tag .ne. icolon) goto 1532
c     More names exist on the input line
c 
      if (.not. allok) then
	allok=.true.
	kbptr = kbptrold
        numvars = 0
	goto 1528
c	We have parsed the line and no errors were found; reset the KBPTR
c	pointer and set ALLOK so we can reparse the line and execute its
c	contents.
c
      endif
c
      mode = iedit
      if (mode .eq. 1) call push(a, ap, iprotect)
c
      GO TO 99
c
C---------------------------------------------------------------------
C				      S	T R I N	G * n <name>
C---------------------------------------------------------------------
 158  continue
c
      if (mode .eq. 1 .and. defexec .eq. 0) call oerror(n42, n1,
     .		'Cannot add declerations among executables')
c
      iedit = mode
      mode = -1
      numvars = 0
c
c     First parse string size
      CALL GETFLD
c
      NSIZE = 1
      IF (TAG.eq.itimes) then
	 CALL GETFLD
         IF (TYPE.ne.11) call oerror(n25,n1,
     .		'String size must be a number')
         if (nint(c(tag)).gt.60) call oerror(n14,n1,' ')
	 if (mod(nint(c(tag)),4).ne.0) call oerror(n25,n1, 
     .		'String lengths must be multiples of 4')
c        Number of characters must be a multiple of 4 and less than 60
c
	 NSIZE = (3 + INT(C(TAG))) / 4
	 call getfld
c
      else
         IF (TAG.EQ.i0) call oerror(n25,n1,' ')
      endif
c
C     Parse name of string.
c
      IF (TAG.EQ.i0) call oerror(n25,n1,'Must give the name of string ')
c
1582  if (type .eq. 11 .or. type .eq. 14) then
	call oerror(n29, n1, 'Bad symbol name')
c
      else if ( (type .eq. 0) .or.
     .	   (iedit .eq. 1 .and. .not. global .and. .not. local .and.
     .	    .not. lhunt(tag) )  ) then
c
	   do 1581 i = 1, numvars
		if (ckpak(1:nkar) .eq. names(i)) call oerror(n41, n1, 
     .			'Bad set of STRING names')
1581		continue
c
	   call push(tstack, numvars, nkar)
	   names(numvars) = ckpak(1:nkar)
	   exists = .false.
c          Store away name for later use.
c
      else if (lhunt(tag) .and. .not. global .and. 
     .		.not. local)  then
	   call oerror(n3, n1, 'Symbol already in use in procedure')
c
      else if (type .eq. 7) then
	   if (iedit .eq. 1 .and. global .and. local) call oerror(n3, n1, '')
           LS = k(LOCSYM + 3)
	   nsizeold = k(ls)
	   exists = .true.
c	   String already exists; copy old size to see if new one is the
c	   same
c
      else 
	   call oerror(n3, n1, '')	
      endif
c
      if (exists .and. nsizeold .ne. nsize) 
     .			call oerror(n3, n1, '')
c
      call getfld
      if (tag .eq. icomma) call getfld
      IF (TAG.ne.i0 .and. tag .ne. icolon) goto 1582
c     More names exist on the input line
c 
      ISIZE = IWPR(nsize)
      do 1583 i = 1, numvars
	    call creatspc(isize, tag)
	    ckpak = names(i)
      	    DO 1589 J = tag, tag + nsize - 1
 	        c(J) = r4blank
1589            continue
            if (iedit .eq. 1 .and. .not. global) then
      	      call creatloc(i7, kpak, tstack(i), tag, n0, locsym)
	    else
      	      call creatsym(i7, kpak, tstack(i), tag, n0, locsym)
	    endif
c
            LS = LOCSYM + 3
            N = 4
            LL = LLOCAT (N,K,LS)
            K(LL) = nsize
            K(LL + 1) = 1
            K(LL + 2) = 1
            K(LL + 3) = nsize
1583	    continue
c     Create new symbol (local if compiling a procedure).
c
      mode = iedit
      if (mode .eq. 1) call push(a, ap, iprotect)
c
      GO TO 99
C---------------------------------------------------------------------
C					S C A L A R
C---------------------------------------------------------------------
 200  CONTINUE
c
      if (mode .eq. 1 .and. defexec .eq. 0) call oerror(n42, n1,
     .		'Cannot add declerations among executables')
c
      iedit = mode
      mode = -1
      numvars = 0
c
      CALL GETFLD
c
      IF (TAG.EQ.i0) call oerror(n29,n1,'Must give the name of scalar')
c
202   if (type .eq. 11 .or. type .eq. 14) then
	call oerror(n29, n1, 'Bad symbol name')
c
      else if ( (type .eq. 0) .or.
     .	   (iedit .eq. 1 .and. .not. global .and. .not. local .and.
     .	    .not. lhunt(tag) )  ) then
c
	   do 201 i = 1, numvars
		if (ckpak(1:nkar) .eq. names(i)) call oerror(n41, n1, 
     .			'Bad list of SCALAR names')
201		continue
c
	   call push(tstack, numvars, nkar)
	   names(numvars) = ckpak(1:nkar)
c          Store away new scalar's name for later use.
c
      else if (lhunt(tag) .and. .not. global .and. 
     .		.not. local)  then
	   call oerror(n3, n1, 'Symbol already in use in procedure')
c
      else if (type .eq. 1) then
	   if (iedit .eq. 1 .and. global .and. local) call oerror(n3, n1, '')
c  
      else 
	   call oerror(n3, n1, '')	
      endif
c
      call getfld
      if (tag .eq. icomma) call getfld
      IF (TAG.ne.i0 .and. tag .ne. icolon) goto 202
c     More names exist on the input line
c 
      do 203 i = 1, numvars
	   call creatspc(n2, tag)
      	   c(tag) = 0.0
	   ckpak = names(i)
           if (iedit .eq. 1 .and. .not. global) then
      	     call creatloc(n1, kpak, tstack(i), tag, n0, locsym)
	   else
      	     call creatsym(n1, kpak, tstack(i), tag, n0, locsym)
	   endif
203	   continue
c     Create new symbol (local if compiling a procedure).
c
      mode = iedit
      if (mode .eq. 1) call push(a, ap, iprotect)
c
      GO TO 99
C---------------------------------------------------------------------
C					P O I N T E R
C---------------------------------------------------------------------
 210  CONTINUE
c
      if (mode .eq. 1 .and. defexec .eq. 0) call oerror(n42, n1,
     .		'Cannot add declerations among executables')
c
      iedit = mode
      mode = -1
      numvars = 0
c
      CALL GETFLD
c
      IF (TAG.EQ.i0) 
     .	call oerror(n29,n1,'Must give the name of pointer ')
c
212   if (type .eq. 11 .or. type .eq. 14) then
	call oerror(n29, n1, 'Bad symbol name')
c
      else if ( (type .eq. 0) .or.
     .	   (iedit .eq. 1 .and. .not. global .and. .not. local .and.
     .	    .not. lhunt(tag) )  ) then
c
	   do 211 i = 1, numvars
		if (ckpak(1:nkar) .eq. names(i)) call oerror(n41, n1, 
     .			'Bad set of POINTER names')
211		continue
c
	   call push(tstack, numvars, nkar)
	   names(numvars) = ckpak(1:nkar)
	   exists = .false.
c          Store away pointer name for later use.
c
      else if (lhunt(tag) .and. .not. global .and. 
     .		.not. local)  then
	   call oerror(n3, n1, 'Symbol already in use in procedure')
c
      else if (type .eq. 8) then
	   if (iedit .eq. 1 .and. global .and. local) call oerror(n3, n1, '')
	   valueold = c(tag)
	   exists = .true.
c	   Pointer already exists; copy old value to see if new one is the
c	   same
c
      else 
	   call oerror(n3, n1, '')	
      endif
c
c     Get the value of the pointer
      CALL GETFLD
      IF (TYPE.ne.11) then
          if (type .eq. 4 .and. (tag.eq.iuplus.or.tag.eq.iuminus)) then
	    ioldtag = tag
	    call getfld
	    if (type .ne. 11) call oerror(n15, n1, '')
	    if (ioldtag .eq. iuminus) c(tag) = -c(tag)
          else
            call oerror(n15, n1, '')
          endif
      endif
      valuenew = c(tag)
      cstack(numvars) = tag
c
      if (exists .and. valuenew .ne. valueold) 
     .			call oerror(n3, n1, '')
c
      call getfld
      if (tag .eq. icomma) call getfld
      IF (TAG.ne.i0 .and. tag .ne. icolon) goto 212
c     More names exist on the input line
c 
      do 213 i = 1, numvars
	   call creatspc(n2, tag)
	   c(tag) = c(cstack(i))
	   ckpak = names(i)
           if (iedit .eq. 1 .and. .not. global) then
      	     call creatloc(n8, kpak, tstack(i), tag, n0, locsym)
	   else
      	     call creatsym(n8, kpak, tstack(i), tag, n0, locsym)
	   endif
213	   continue
c     Create new symbol (local if compiling a procedure).
c
      mode = iedit
      if (mode .eq. 1) call push(a, ap, iprotect)
c
      GO TO 99
C---------------------------------------------------------------------
C					A L I A S
C---------------------------------------------------------------------
 220  CONTINUE
c
      if (mode .eq. 1 .and. defexec .eq. 0) call oerror(n42, n1,
     .		'Cannot add declerations among executables')
c
      iedit = mode
      mode = -1
      numvars = 0
c
      CALL GETFLD
c
      IF (TAG.EQ.i0) call oerror(n29,n1,'Must give the name of alias')
c
222   if (type .eq. 11 .or. type .eq. 14) then
	call oerror(n29, n1, 'Bad symbol name')
c
      else if ( (type .eq. 0) .or.
     .	   (iedit .eq. 1 .and. .not. global .and. .not. local .and.
     .	    .not. lhunt(tag) )  ) then
c
	   do 221 i = 1, numvars
		if (ckpak(1:nkar) .eq. names(i)) call oerror(n41, n1, 
     .			'Bad set of ALIAS names')
221		continue
c
	   call push(tstack, numvars, nkar)
	   names(numvars) = ckpak(1:nkar)
	   exists = .false.
c          Store away alias name for later use.
c
      else if (lhunt(tag) .and. .not. global .and. 
     .		.not. local)  then
	   call oerror(n3, n1, 'Symbol already in use in procedure')
c
      else if (type .eq. 6) then
	   if (iedit .eq. 1 .and. global .and. local) call oerror(n3, n1, '')
           oldtag = k(LOCSYM + 2)
	   exists = .true.
c	   Alias already exists; copy old value to see if new one is the
c	   same
c
      else 
	   call oerror(n3, n1, '')	
      endif
c
c     Get the name to be aliased.
      CALL GETFLD
      IF (TYPE.eq.0) call oerror(n2, n1, '')
      IF (TYPE.ne.3 .and. type.ne.4 .and. type.ne.5) 
     .	call oerror(n29, n1, 'Can only ALIAS to a VERB or PROCEDURE')
      cstack(numvars) = tag
c
      if (exists .and. tag .ne. oldtag) call oerror(n3, n1, '')
c
      call getfld
      if (tag .eq. icomma) call getfld
      IF (TAG.ne.i0 .and. tag .ne. icolon) goto 222
c     More names exist on the input line
c 
      do 223 i = 1, numvars
	   ckpak = names(i)
           if (iedit .eq. 1 .and. .not. global) then
      	     call creatloc(n6, kpak, tstack(i), cstack(i), n0, locsym)
	   else
      	     call creatsym(n6, kpak, tstack(i), cstack(i), n0, locsym)
	   endif
223	   continue
c     Create new symbol (local if compiling a procedure).
c
      mode = iedit
      if (mode .eq. 1) call push(a, ap, iprotect)
c
      GO TO 99
c
C---------------------------------------------------------------------
c
  99  return
      END
c
      SUBROUTINE RTOPS	(J)
C----------------------------------------------------------------------
C   RTOPS handles setups for run-time operators IF, THEN, ELSE, WHILE
C   (which require forward references and an additional clean-up pass).
C----------------------------------------------------------------------
c
      integer*2 m3, n120, nothng, j, jj, it
c
      include 'stk.inc'
      include 'smstuf.inc'
c
      include 'tags.inc'
c
      data n120, m3/120,-3/, NOTHNG/0/ 
c
      JJ = J
      GO TO (153,154,157,159), JJ
      call oerror(n120,m3,'PSEUDO')
c
C---------------------------------------------------------------------
C					E L S E.
C---------------------------------------------------------------------
 153  continue
      GO TO 154
C---------------------------------------------------------------------
C					T H E N.
C---------------------------------------------------------------------
 154  CONTINUE
      IFFLAG=0
      NEXTP=0
      CALL BCLEAN
      CALL PUSH	(A,AP,I0)
      CALL PUSH	(A,AP,I0)
      CALL PUSH	(A,AP,TAG)
      LAST=NOTHNG
      GO TO 99
C---------------------------------------------------------------------
C			     I F
C---------------------------------------------------------------------
 157  IFFLAG=1
      NEXTP=0
      CALL BCLEAN
      LAST=NOTHNG
      GO TO 99
C---------------------------------------------------------------------
C					W H I L	E.
C---------------------------------------------------------------------
 159  IFFLAG=1
      IT=iwhilesetup
      NEXTP=0
      CALL BCLEAN
      CALL PUSH	(A,AP,IT)
      CALL PUSH	(B,BP,TAG)
      BPR(BP)=0
      CALL PUSH	(B,BP,I0)
      BPR(BP)=0
      CALL PUSH	(B,BP,I0)
      BPR(BP)=0
      GO TO 99
C---------------------------------------------------------------------
c
  99  return
      END
c
      logical function lhunt(tag)
c
c     Searches the SYMSTACK stack for TAG.  If it is there, return TRUE.
c
      integer*2 i, tag
c
      include 'stk.inc'
c
      lhunt = .false.
c
      do 10 i = 1, symp
	if (tag .eq. symstack(i)) then
	   lhunt = .true.
	   return
	endif
10	continue
c
      return
      end
c
