      SUBROUTINE  execpol
C-------------------------------------------------------------------------------
C  @(#)execpol.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C--------------------------------------------------------------------
C   Specialized polish scanner
C      Parses the character string in KARBUF, translating to polish
C  postfix notation.  Result is a sequence of integers, representing
C  code for the POPS interpreter.  Negative tokens are operand pointers
C  while positive tokens are operation codes.  The array, A, holds
C  these; AP points to the last entry.
C      Specialized from the standard polish.f : pseudo operators are
C  not allowed and mode MUST be 0.  Used by the EXEC verb as well as
C  other verbs that execute strings (called by execcomp.f).
c      Does NOT call gpflush().
C--------------------------------------------------------------------
      integer*2 nothing, i, savel
      integer*2 n1, n17, n34, n43, n44
c
      logical isreturn
c
      INCLUDE 'smstuf.inc'
      include 'stk.inc'
c
      include 'tags.inc'
c
      data nothing/0/
      data n1, n17, n34, n43, n44 / 1, 17, 34, 43, 44/
C
C=======================================================================
c
      LEVEL=0
      AP=0
      BP=0
      IFFLAG=0
      LAST=nothing
C					Parse tokens.
c			save and restore value of l
 10   savel = l
      CALL COMPIL
      l = savel
C					   Pseudo operators not allowed.
      IF (TYPE.eq.5) then
c
         call oerror(n43, n1, 'Line Ignored')
c
      else
C					Line finished.	-  Clean up.
         NEXTP=0
         CALL BCLEAN
c					Add a return only if there is none.
         isreturn = .false.
         do 20 i = 1, ap
            if (a(i) .eq. ireturn) isreturn = .true.
 20      continue
	 if (.not. isreturn) call push(a, ap, ireturn)
c
	 i = 1
c
52	 if (i .le. ap) then
c
	    IF (A(I) .le. 0) then
c
		i = i + 1
		goto 52
c		Keep on looking for an operator
c
	    endif
c
	 else if (ap .ne. 0) then
c
     	    CALL oerror(n44, n1,'')
C           NO OPERATOR ON LINE
c
	 endif
c
c	 Add OMEGA operator.
	 CALL PUSH(A,AP,iomega)
c
C        UNBALANCED PARENTHESES?
         IF (LEVEL.ne.0)  CALL oerror(n34, n1,'Line Ignored')
c
c
      endif
c			mode must be 0
      IF (MODE.NE.0) then
         call oerror(n17, n1, 'Line Ignored')
      endif
c
99    continue
c
      RETURN
c
      END
