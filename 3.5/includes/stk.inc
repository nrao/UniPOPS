C					Include	STACK.
C-------------------------------------------------------------------------------
C  @(#)stk.inc	5.1 06/22/94
C-------------------------------------------------------------------------------
C
      INTEGER*2  AP, BP, ONE, ZERO, TRUE, FALSE, STACK(400), 
     .           CSTACK(400), SP, CP, SP0, SLIM, a(400), b(400), 
     .           bpr(400), tstack(400), kt, lpgm, llit, last, idebug,
     .		 mode, ifflag, link, l, namep, iedit, lclstart,lcllast,
     .		 defexec, symstack(400), symp
      logical*2 local
      real*8 v(400), xx
C
C
      COMMON /GLOBAL/ v, xx, KT, LPGM, LLIT, LAST, IDEBUG, MODE, IFFLAG, 
     .                LINK, L, NAMEP, SLIM, AP, BP, ONE, ZERO,
     .		      TRUE, FALSE, STACK, CSTACK, a, b, bpr, tstack, SP,
     .		      CP, SP0, iedit, lclstart, lcllast, local, defexec,
     .		      symstack, symp
 
c
c     KT = subscript in K of immediate execute link-list.
c     LPGM = subscript in K where 1st program chunk is located.
c     LLIT = keeps track of whether one is in compile mode because of a call
c	 to PROC (llit=0) or a call to POPSEDIT (llit=1).  ENDEDIT makes sure
c	 that LLIT=1 before it does its thing.
c     LAST = TAG of last operator processed on command line (or
c	operand if LAST < 0)
c     IDEBUG = whether or not to use debug print statements (>0 debug on)
c
c     MODE = 0 -> Execute mode, min/match in affect, store literals 
c  		in temporary space, don't store lines in LISTF array.
c  		ENDEDIT and FINISH change the mode back to 0.
c          = 1 -> Compile mode, no min/match, store literals permanently,
c  		store lines in LISTF.  PROC and POPSEDIT change the
c  		mode to 1.
c          = -1 -> Variable decleration mode, no min/match,  store literals 
c  		in temporary space, do not store lines in LISTF array.  ARRAY,
c  		ALIAS, SCALAR, etc. place the program temporarily into
c  		this mode while it is creating adverbs/aliases/etc.
c          = -2 -> Proc finish mode, no min/match,  store literals 
c  		in temporary space, do not store lines in LISTF array.  
c  		ENDEDIT and FINISH place the program temporarily into
c  		this mode while they are doing their clean-up operations.
c          = 2 -> Createing PROC mode, no min/match, store literals
c  		permanently, store lines in LISTF.  PROC chnages the
c  		mode temporarily to 2 when it is disecting the proc.
c  		decleration and creating the slot for the procedure.
c        [ Modes -1, -2, and 2 are very fleeting.]
c
c     IFFLAG = 1 if IF statement is being processed; 0 if not an IF
c     LINK = Used to store the next program chunk link in the K array.
c     L = Used to store present location in linked-list.
c     NAMEP = Number which reflects the location in the K array as to
c	where the compiled, linked-list of a procedure is stored.
c     SLIM = number of items which can be put on the stacks (Set by INITCOM)
c     AP = Pointer to last item in A Stack
c     BP = pointer to last item in B stack
c     ONE = Tag of where the binary representation of 1 is stored in K array
c     ZERO = Tag of where the binary rep. of zero is stored in K array
c     TRUE = Tag of where the character string TRUE is stored in K array
c     A  = During compilation of input line, contains list of input operands.  
c     STACK = At completion of input line, contains the polish-postfix code of
c	operators and operands arranged according to precedences.
c     B = At completion of input line, contains stack of operators which,
c	  according to precedences stored in the BPR stack, are placed on the
c	  A stack with BCLEAN routine.  
c     CSTACK = used as the control stack where the run-time locations, the
c	program chunk link (LINK), and stack pointers are stored.
c     BPR = At compilation, contains the precedences for the items in the B stack. c     V = run-time value stack containing, in the corresponding position in
c	STACK, the floating point number found in the C array at the address
c	specified by the number in STACK.
c     TSTACK = temporary stack used for processing such things as WHILE; ELSE;
c	END; etc.
c     XX = Where the results of functions store their results.
c     SP = V, STACK stack pointer
c     CP = C stack pointer
c     SP0 = Value of SP before executing a FOR loop, or running a procedure;
c	used by END and RETURN so as to return the SP pointer to its correct
c	value after the loop or procedure finishes.
c     IEDIT = the old value of MODE when creating adverbs/aliases so that,
c  	after the creation the program can return to the mode in which	
c  	the call to the creation veb was made.  
c     LCLSTART = index in K where local operand table starts.
c     LCLLAST = index in K where the local operand table ends.
c     DEFEXEC = 0 if not compiling a procedure or, when compiling, the
c			user is entering executable lines.
c		1 if compiling procedure and the user is still enter symbol
c			declerations.
c		-1 if popsediting a procedure and the user is trying to edit
c			lines between two decleration statements.
c     LOCAL = true if SYMBOL has found a local symbol, else symbol undefined
c		or it was a global.
c     SYMSTACK = stack of tags of symbols in use by a procedure -- created
c		when you POPSEDIT a procedure and checked when you try
c		to create a local symbol.
c     SYMP = symstack pointer
c
c
