      SUBROUTINE COMPIL
C-------------------------------------------------------------------------------
C  @(#)compil.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------------------
C     Compiles an input line
C-------------------------------------------------------------------------------
      integer*2 pr(23), eq
      integer*2 maxlev, nothng, leftside, i, j, n, it
      integer*2 n1, n8, n10, fshort, n2, n39, n29
      logical dhset, reading
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
c
      include 'tags.inc'
c
      DATA MAXLEV/11/, NOTHNG/0/
      data n1, n8, n10, n2, n29, n39 / 1, 8, 10, 2, 29, 39/
      DATA PR/0,0,0,1,7, 7,8,8,9, 5,5,0,8,4, 5, 5, 5, 6, 6,5,2,3,0/
C             , ( ) = +	 - * / ** > < + - ~ ~= >= <= TO BY = | & ;
C
C=======================================================================
C
      leftside = 0
      EQ = 0
      dhset = .false.
      reading = .false.
c
   10 CALL GETFLD
c
      if (type .eq. 5 .and. (ap .ne. 0  .or. bp .ne. 0) .and.
     .	  tag .ge. ipseudo) call oerror(n39, n1,' ')
c     Protects against a misplaced PSEUDO command
c
      IF ((NKAR.EQ.0).OR.(TAG.eq.0).OR.(TYPE.EQ.5)) GO TO 98
      IF ((TYPE.EQ.3).OR.(TYPE.EQ.4).or.(type.eq.6)) GO TO 50
      if (type .eq. 0) call oerror(n2, n1, ' ')
C---------------------------------------------------------------------
C		   O P E R A N D    P R	O C E S	S O R
C---------------------------------------------------------------------
      IF (LAST.le.0) then
	 NEXTP=1+LEVEL
	 CALL BCLEAN
      endif
C
      IF (TYPE.eq.2) then
         N=K(LOCSYM+3)
         N=K(N)
         CALL PUSH (A,AP,fshort(-TYPE))
	 CALL PUSH (A,AP,fshort(-N))
         CALL PUSH (A,AP,fshort(-TAG))
	 CALL PUSH (A, AP,fshort(-2))
c	 Operand is an array
c
      else if (type.eq.7) then
         N=K(LOCSYM+3)
         N=K(N)
         CALL PUSH (A,AP,fshort(-TYPE))
	 CALL PUSH (A,AP,fshort(-N))
         CALL PUSH (A,AP,fshort(-TAG))
	 CALL PUSH (A, AP, fshort(-3))
c	 Operand is a string
c
      else if (TYPE.eq.14) then
	if (level .eq. 0 .and. reading) 
     .		call oerror(n29, n1, 'Trying to READ a literal constant')
         N=K(LOCSYM+1)/16
         CALL PUSH (A,AP,fshort(-TYPE))
	 CALL PUSH (A,AP,fshort(-N))
         CALL PUSH (A,AP,fshort(-TAG))
	 CALL PUSH (A,AP,fshort(-3))
c	 Operand is a literal
c
      else
	 if (level .eq. 0 .and. reading .and. type .eq. 8) then
     		call oerror(n29, n1, 'Trying to READ a POINTER adverb')
	 else if (level .eq. 0 .and. reading .and. type .eq. 11) then
     		call oerror(n29, n1, 'Trying to READ a real constant')
	 else
 		CALL PUSH (A,AP,fshort(-TAG))
	 endif
C	 All other operands.
c
      endif
c
      LAST = -TAG
c
      if (level .eq. 0) then
	if (type .eq. 8 .or. type .eq. 11 .or. type .eq. 15) then
	   leftside = 0
c          Flag that the last operator/operand not enclosed in () is a
c           protected operand and thus cannot be assigned a value.
c
        else 
	   leftside = -1
c          Flag that the last operator/operand not enclosed in () is an
c          unprotected operand and thus can be assigned a value.
c
	endif
      endif
      GO TO 10
C---------------------------------------------------------------------
C		   O P E R A T O R    P	R O C E	S S O R
C---------------------------------------------------------------------
C					Comma (,) operator.
50    IF (TAG.eq.icomma) then
	 LAST=NOTHNG
	 NEXTP=1+LEVEL
	 CALL BCLEAN
C					Array assign ?
	 IF (LEVEL.GT.0) GOTO 10
c
         if (level .eq. 0 .and. .not. dhset) leftside = 1
	 IF (EQ.EQ.0) GOTO 10
	 EQ = EQ + 1
	 IF (EQ.GT.0) GOTO 10
	 EQ = 2
	 AP = AP - 1
	 GO TO 10
C					Left Paren "(" operator.
      else IF (TAG.eq.ilparen) then
	 LEVEL=LEVEL+MAXLEV
	 IF (LAST.ge.nothng .or. a(ap) .ne. -2 .or. ap .lt. 4) GO TO 10
c
C					     CASE OF ARRAY SUBSCRIPT
	 J= level - 1
	 CALL PUSH (B,BP,isubs)
	 BPR(BP)=J
	 CALL PUSH (B,BP,fshort(-K(LOCSYM+3)))
	 BPR(BP)=J
	 DO 2601 I=1,4
	    CALL POP (A,AP,IT)
	    CALL PUSH (B,BP,IT)
	    BPR(BP)=J
2601	    continue
	 last = nothng
c        Allows for unary +/- for array subscripts
c
	 GO TO 10
C					Right paren ")".
      else IF (TAG.eq.irparen) then
	 LEVEL=LEVEL-MAXLEV
	 GO TO 10
C					Semi colon ";".
      else IF (TAG.eq.icolon) then
	 leftside = 0
	 dhset = .false.
	 reading = .false.
	 NEXTP=0
	 IFFLAG=0
	 CALL BCLEAN
	 LAST=NOTHNG
	 IF (EQ.gt.0) then
	    CALL PUSH (A,AP,fshort(-EQ))
	    CALL PUSH (A,AP,igets)
	 endif
         EQ = 0
	 GO TO 10
c
      else if (reading .and. level .eq. 0) then
	call oerror(n29, n1, 'Trying to READ a PROC or VERB')
c       Check that everyting to the left of a READ is an Adverb.
c
C					Unary plus "+"
      else IF (tag .eq. iuplus) then
           if (last .ge. 0) GO TO 10
C					Unary minus "-"
      else IF (TAG.EQ.iuminus) then
	    if (last .ge. 0) TAG = iminus 
C					"=" . .	. compare or store?
      else IF (TAG.eq.icompare) then
	 if (ifflag .eq. 0 .and. level .ne. 0) call oerror(n8,n1,' ')
     	 if (ifflag .eq. 0 .and. leftside .ge. 0)
     +      call oerror(n10,n1,' ')
c        Traps an error if the left hand side of a equals sign is NOT a virtual
c        or if all ( are not closed.
c
         if (ifflag .eq. 0 .and. dhset) then
		dhset = .false.
		last = 1
		nextp = 11 + level
		call bclean
		goto 10
	 endif
c	 Skip over = if a H0-9 type of function is on the left of the
c	 equal sign
c
	 IF (IFFLAG.NE.1) EQ = -1
	 IF (IFFLAG.EQ.1) TAG= iassign
	 IFFLAG=1
c
      endif
C					All other operators.
      if (level .eq. 0 .and. leftside .eq. 0 .and. 
     .    ifflag .ne. 1 .and.
     .    tag .ge. idhfunc1 .and. tag .le. idhfunc2) then
		 tag = tag - idhfunc1 + idhset1
		 dhset = .true.
		 leftside = -1
      endif
c     Change last commands tag if it is the H0-H9, etc. type of function
c     to the left of the = sign.
c
      IF (TAG .eq. iread .or. tag .eq. isread .or.
     .	tag .eq. ifread) reading = .true.
c
      IF (TAG .ge. ifunct) then
	if (tag .ge. igenop .and. (last .le. 0 .or. (last .ge. igenop .or.
     .			(last .gt. iprint .and. last .lt. ifunct) ) ) ) then
	   NEXTP=LEVEL+3
	else
	   NEXTP=LEVEL+ maxlev - 1
      	endif
      else IF (TAG.le.icolon) then
	NEXTP=LEVEL+PR(TAG)
      else
	nextp = level
      endif
      CALL BCLEAN
      CALL PUSH	(B,BP,TAG)
      BPR(BP)=NEXTP
      LAST = TAG
      if (tag .ge. ifunfunct .and. tag .lt. igenop) last = -last
c     Reverse the sign of LAST if it is a function which doesn't take a
c     value off of the stack but does return one.  In compiling a line
c     containing such an operator, it looks like it were an operand.
c
      if (level .eq. 0 .and. 
     .   (tag .lt. idhset1 .or. tag .gt. idhset2 .or. 
     .   .not. dhset) ) leftside = 1
c     Flag that the last operator/operand not enclosed in () is an operator
c     and thus cannot be assigned a value.
c
      GO TO 10
C-----------------------------------------------------------------------
   98 IF (EQ.gt.0) then
	 NEXTP = 0
	 CALL BCLEAN
	 CALL PUSH(A,AP,fshort(-EQ))
	 CALL PUSH(A,AP,igets)
      endif
c
      RETURN
      END
