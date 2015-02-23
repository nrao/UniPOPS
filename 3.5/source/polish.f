      SUBROUTINE  POLISH
C-------------------------------------------------------------------------------
C  @(#)polish.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C--------------------------------------------------------------------
C   Polish scanner
C      Parses the character string in KARBUF, translating to polish
C  postfix notation.  Result is a sequence of integers, representing
C  code for the POPS interpreter.  Negative tokens are operand pointers
C  while positive tokens are operation codes.  The array, A, holds
C  these; AP points to the last entry.
C--------------------------------------------------------------------
      integer*2 nothing, i
      integer*2 fshort, n0, n1, n34, n38, n39, n42
c
      INCLUDE 'smstuf.inc'
      include 'stk.inc'
c
      include 'tags.inc'
c
      data nothing/0/
      data n0, n1, n34, n38, n39, n42 /0, 1, 34, 38, 39, 42/
C
C=======================================================================
c
      LEVEL=0
      AP=0
      BP=0
      IFFLAG=0
      LAST=nothing
C					Parse tokens.
 10   CALL COMPIL
C					   Pseudo operators.
      IF (TYPE.eq.5) then
c
	 IF (TAG.GE.ihelps) then
  	    CALL HELPS (fshort(TAG-ihelps+1))
	 else IF (TAG.GE.istores) then
	    CALL STORES (fshort(TAG-istores+1))
	 else IF (TAG.GE.ieditor) then
	    CALL EDITOR (fshort(TAG-ieditor+1))
	 else if (tag .ge. ipseudo) then
	    CALL PSEUDO (fshort(TAG-ipseudo+1))
	    CALL PUSH(A,AP,iomega)
	 else 
	    CALL RTOPS (fshort(TAG-irtops+1))
            goto 10
	 endif
	 call getfld
	 if (tag .ne. i0) then
     		call oerror(n39, n0, 'Extra commands ignored')
		goto 99
	 endif
c
      else
C					Line finished.	-  Clean up.
         NEXTP=0
         CALL BCLEAN
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
     	    CALL oerror(n38, n1,'')
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
c
      IF ((MODE.NE.0).AND.(AP.NE.0)) then
	if (ap .gt. 1) then
	   if (defexec .eq. 1 .and. a(1) .ne. iprotect) then
	      defexec=0
	   else if (defexec .eq. -1 .and. a(1) .ne. iprotect) then
	      call oerror(n42, n1, 
     .		'Cannot add executables among declerations')
	   else if (defexec .eq. 0 .and. a(1) .eq. iprotect) then
	      call oerror(n42, n1, 
     .		'Cannot add declerations among executables')
	   endif
	endif
c	Has the user started entering executable lines to the procedure or
c	are they still symbol declerations.
c
	CALL STORES (n1)
      endif
c
99    call gpflush()
c
      RETURN
c
      END
