      SUBROUTINE  QUICK(LFLAG,J, locl, loclink)
C-------------------------------------------------------------------------------
C  @(#)quick.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C	 QUICK contains	the most frequently used interpreter codes.
C
C---------------------------------------------------------------------
      integer*2 lflag, j, i, m, mi, iwpr, lastblnk, unit, 
     .		n, mj, isize1, isize2, RO, WO, locl, loclink,
     .		ifrmt(30), iadvrb(30), loc1, loc2, itype,
     .          kloc, execcomp, isize, irealp
      integer*2 n1, n16, n80, n107, n111, n112, n115, n116, n118, n120,
     .          n130, m1, m2, m3
      real*4 epsiln, xi, xl, rstring1(30)
      integer*4 long, loc
      character*60 frmt, advrb, string1
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
      INCLUDE 'errors.inc'
c
      EQUIVALENCE (ifrmt, frmt), (iadvrb, advrb), (string1, rstring1)
c
      include 'tags.inc'
c
      data n1, n16, n80, n107, n111, n112, n115, n116, n118, n120,
     .          n130, m1, m2, m3 
     .        /1, 16, 80, 107, 111, 112, 115, 116, 118, 120, 
     .           130, -1, -2, -3/
      DATA EPSILN/1.0E-8/
c
      parameter (RO = 2)
      parameter (WO = 1)
C
C=======================================================================
C
      GO TO ( 1,99,3, 4, 5, 6, 7, 8, 9,10,
     .	 11,99,13,14,141,142,143,15,16,17,18,19,99,
     .	 21,22,23,24,25,27,28,31,56,500,33,34,35,36), J
      IF (J.EQ.iifelse) GO TO 53
      IF (J.EQ.iifthen) GO TO 54
      IF (J.EQ.iwhile) GO TO 59
      if (j .eq. idumend) goto 96
      IF (J.EQ.iwhilesetup) GO TO 97
      IF (J.EQ.isubs) GO TO 98
      IF (J .GT. ilastverb-1) GO TO 2801
      call oerror(n120, m3, 'QUICK')
C---------------------------------------------------------------------
C		   O M E G A   O P E R A T O R			  
C---------------------------------------------------------------------
    1 Locl=locLINK
      IF (locL.EQ.0) GO TO 9999
      locLINK=K(locL)
      GO TO 900
C---------------------------------------------------------------------
C		   G E T   A R G U M E N T
C---------------------------------------------------------------------
3     continue
      if (cp .le. 0 .or. cp .gt. slim) call oerror(n107, m1, ' ')
      IF (CSTACK(CP).ne.-irun) call oerror(n120, m3, ' ')
      I=2*SP0-SP-1
      IF (I.le.0) call oerror(n112, m1, ' ')
      J=SP0
300   IF ((I.lt.SP0).and.(J.le.SP)) then
         if (j .le. 0) call oerror(n107, m1, ' ')
         M=STACK(J)
         if (m .le. 0) call oerror(n107, m1, ' ')
         C(M)=V(I)
         I=I+1
         J=J+1
         GO TO 300
      endif
      CP=CP+2
      if (cp .gt. slim) call oerror(n107, m1, ' ')
      CSTACK(CP-1)=SP0
      CSTACK(CP)=-3
      SP0=SP+1
      if (sp0 .gt. slim .or. sp0 .le. 0) call oerror(n107, m1, ' ')
      GO TO 1000
C---------------------------------------------------------------------
C		   =	S T O R	E   O P	E R A T	O R		  
C---------------------------------------------------------------------
    4 continue
      CALL ASSGN
      GOTO 1000
C---------------------------------------------------------------------
C		   +   O P E R A T O R				 
C---------------------------------------------------------------------
5     continue
      if (sp .lt. 2) call oerror(n112, m1, '+')
      XX=V(SP-1)+V(SP)
      GO TO 5000
C---------------------------------------------------------------------
C		   -   O P E R A T O R				  
C---------------------------------------------------------------------
6     continue
      if (sp .lt. 2) call oerror(n112, m1, '-')
      XX=V(SP-1)-V(SP)
      GO TO 5000
C---------------------------------------------------------------------
C		   *   O P E R A T O R				 
C---------------------------------------------------------------------
7     continue
      if (sp .lt. 2) call oerror(n112, m1, '*')
      XX=V(SP-1)*V(SP)
      GO TO 5000
C---------------------------------------------------------------------
C		   /   O P E R A T O R				
C---------------------------------------------------------------------
8     continue
      if (sp .lt. 2) call oerror(n112, m1, '/')
      XX=V(SP-1)/V(SP)
      GO TO 5000
C---------------------------------------------------------------------
C		   * *	  O P E	R A T O	R		
C---------------------------------------------------------------------
9     continue
      if (sp .lt. 2) call oerror(n112, m1, '**')
      XX=V(SP-1)**V(SP)
      GO TO 5000
C---------------------------------------------------------------------
C		   >   C O M P A R I S O N   O P E R A T O R	   
C---------------------------------------------------------------------
   10 continue
      if (sp .lt. 2) call oerror(n112, m1, '>')
      SP=SP-1
      IF (sngl(V(SP)).GT.sngl(V(SP+1))) GO TO 1702
      GO TO 1701
C---------------------------------------------------------------------
C		   <   C O M P A R I S O N   O P E R A T O R	  
C---------------------------------------------------------------------
   11 continue
      if (sp .lt. 2) call oerror(n112, m1, '<')
      SP=SP-1
      IF (sngl(V(SP)).LT.sngl(V(SP+1))) GO TO 1702
      GO TO 1701
C---------------------------------------------------------------------
C		   U N A R Y   ( - )   O P E R A T O R		 
C---------------------------------------------------------------------
   13 continue
      if (sp .lt. 1) call oerror(n112, m1, '- (Unary)')
      CONTINUE
      XX=-V(SP)
      GO TO 5001
C---------------------------------------------------------------------
C		   ~   O P E R A T O R				
C---------------------------------------------------------------------
   14 continue
      if (sp .lt. 1) call oerror(n112, m1, '~')
      IF (sngl(V(SP)).GT.0) then
	xx = -1.0
      else
	xx = 1.0
      endif
      GO TO 5001
C---------------------------------------------------------------------
C		  ~=   C O M P A R I S O N   O P E R A T O R	  
C---------------------------------------------------------------------
  141 continue
      if (sp .lt. 2) call oerror(n112, m1, '~=')
      SP=SP-1
      IF (sngl(V(SP)).ne.sngl(V(SP+1))) GO TO 1702
      GO TO 1701
C---------------------------------------------------------------------
C		  >=   C O M P A R I S O N   O P E R A T O R	  
C---------------------------------------------------------------------
  142 continue
      if (sp .lt. 2) call oerror(n112, m1, '>=')
      SP=SP-1
      IF (sngl(V(SP)).ge.sngl(V(SP+1))) GO TO 1702
      GO TO 1701
C---------------------------------------------------------------------
C		  <=   C O M P A R I S O N   O P E R A T O R	  
C---------------------------------------------------------------------
  143 continue
      if (sp .lt. 2) call oerror(n112, m1, '<=')
      SP=SP-1
      IF (sngl(V(SP)).le.sngl(V(SP+1))) GO TO 1702
      GO TO 1701
C---------------------------------------------------------------------
C		   T O	 O P E R A T O R		
C---------------------------------------------------------------------
15    CONTINUE
      if (sp .lt. 3 .or. sp+2 .gt. slim .or. cp .lt. 0 .or. 
     1    cp .gt. slim-3 .or. sp0 .gt. slim-3 .or. sp0 .le. 0) 
     2    call oerror(n107, m1, 'TO')
      CSTACK(CP+1)=SP
      CSTACK(CP+2)=SP0
      CSTACK(CP+3)=-iloopto
      CP=CP+3
      SP0=SP0+3
      V(SP+2)=V(SP-1)
      STACK(SP+1)=STACK(SP-2)
      STACK(SP+2) = 0
      V(SP-1)=V(SP)
      V(SP)=1.0
      SP=SP+2
      GO TO 1000
C---------------------------------------------------------------------
C		   B Y	 O P E R A T O R	
C---------------------------------------------------------------------
   16 continue
      if (cp .le. 0 .or. cp .gt. slim) call oerror(n107, m1, 'BY')
      IF (CSTACK(CP).ne.-iloopto) call oerror(n115, m1, 'BY')
      if (sp .lt. 4 .or. sp .gt. slim) call oerror(n107, m1, 'BY')
      V(SP-3)=V(SP)
      SP=SP-1
      GO TO 1000
C---------------------------------------------------------------------
C		   =   E Q U A L S   O P E R A T O R		 
C---------------------------------------------------------------------
   17 continue
      if (sp .lt. 2) call oerror(n112, m1, '= (Comparison)')
      SP=SP-1
      IF (sngl(V(SP)).EQ.sngl(V(SP+1))) GO TO 1702
 1701 V(SP)=-1.0
      GO TO 1000
 1702 V(SP)=1.0
      GO TO 1000
C---------------------------------------------------------------------
C		   |	' O R '	   O P E R A T O R		
C---------------------------------------------------------------------
18    continue
      if (sp .lt. 2) call oerror(n112, m1, '|')
      SP=SP-1
      IF ((sngl(V(SP)).gt.0).or.(sngl(V(SP+1)).gt.0)) GO TO 1702
      GO TO 1701
C---------------------------------------------------------------------
C		   &   ' A N D '    O P	E R A T	O R	
C---------------------------------------------------------------------
19    continue
      if (sp .lt. 2) call oerror(n112, m1, '&')
      SP=SP-1
      IF ((sngl(V(SP)).GT.0).AND.(sngl(V(SP+1)).GT.0)) GO TO 1702
      GO TO 1701
C---------------------------------------------------------------------
C		   F O R   O P E R A T O R	
C---------------------------------------------------------------------
   21 continue
      if (cp .le. 0 .or. cp+2 .gt. slim) call oerror(n107, m1, 'FOR')
      IF (CSTACK(CP).ne.-iloopto) call oerror(n115, m1, ' ')
 2165 CSTACK(CP)=locL+1
      CSTACK(CP+1)=locLINK
      CP=CP+2
      CSTACK(CP)=-iloopfor
      GO TO 1000
C---------------------------------------------------------------------
C		   E N D    O P	E R A T	O R
C---------------------------------------------------------------------
22    continue
      if (cp .le. 0 .or. cp+2 .gt. slim) call oerror(n107, m1, 'END')
      IF (CSTACK(CP).NE.-iloopfor) GO TO 2202
      if (cp .le. 4) call oerror(n107, m1, 'END')
      MI=CSTACK(CP-4)
      if (mi .ne. sp .and. .not. amath) call oerror(n130, m1, 'FOR/END')
      if (mi-2 .le. 0 .or. mi .gt. slim) call oerror(n107, m1, 'END')
      XI=V(MI)
      XL=V(MI-1)
      M=STACK(MI-2)
      IF (ABS(XI). lt. EPSILN) call oerror(n118, m1, ' ')
      if (m .le. 0 .or. xi .eq. 0.0 .or. cp-2 .le. 0) 
     .      call oerror(n107, m1, 'END')
      C(M)=C(M)+XI
      IF ((C(M)-XL)/XI .le. 0) then
         locLINK=CSTACK(CP-1)
         locL=CSTACK(CP-2)
         GO TO 1001
      endif
      CP=CP-5
      if (cp .lt. 0 .or. cp+2 .gt. slim) call oerror(n107, m1, 'END')
      SP=CSTACK(CP+1)-3
      SP0=CSTACK(CP+2)
      if (sp .lt. 0 .or. sp .gt. slim .or. sp0 .lt. 0 .or. 
     1    sp0 .gt. slim) call oerror(n107, m1, 'END')
      GO TO 1000
c
2202  if (cp-2 .le. 0 .or. cp .gt. slim) call oerror(n107, m1, 'END')
      IF (CSTACK(CP) .NE. -iprocedure) GO TO 1000
      locL=CSTACK(CP-2)
      locLINK=CSTACK(CP-1)
      GO TO 1001
C---------------------------------------------------------------------
C		   R E A D    O	P E R A	T O R			 
C---------------------------------------------------------------------
23    continue
c
      call ufread(m1)
c
      sp=sp0-1
      if (sp .lt. 0) call oerror(n107, m1, 'read')
c
      go to 1000
C---------------------------------------------------------------------
C		   P R I N T   O P E R A T O R			 
C---------------------------------------------------------------------
24    continue
c
      call ufwrite( m1)
c
      sp=sp0-1
      if (sp .lt. 0) call oerror(n107, m1, 'PRINT')
c
      go to 1000
C---------------------------------------------------------------------
C		   R E T U R N					 
C---------------------------------------------------------------------
25    CONTINUE
      if (cp-1 .le. 0 .or. cp .gt. slim .or. sp0 .lt. 0 .or. 
     1    sp0 .gt. slim) call oerror(n107, m1, 'RETURN')
      IF (CSTACK(CP).NE.-3) GO TO 2503
      I=CSTACK(CP-1)
      N=SP0-I
      J=I-N
      MI=I
2500  IF ((J.GE.MI).OR.(I.GE.SP0)) GO TO 2501
      if (j .le. 0 .or. j .gt. slim .or. i .le. 0 .or. i .gt. slim) 
     .     call oerror(n107, m1, 'RETURN')
      MJ=STACK(J)
      M=STACK(I)
      if (mj .gt. 0 .and. m .gt. 0) C(MJ)=C(M)
      I=I+1
      J=J+1
      GO TO 2500
c
2501  SP0=MI
      if (mi .le. 0 .or. mi .gt. slim .or. mi+n .le. 0 .or.
     1    mi+n .gt. slim) call oerror(n107, m1, 'RETURN')
      STACK(MI) = STACK(MI+N)
      V(MI)=V(MI+N)
      SP=SP-N
      CP=CP-2
      if (sp .lt. 0 .or. sp .gt. slim) call oerror(n107, m1, 'RETURN')
2503  if (cp .le. 0 .or. cp+2 .gt. slim) call oerror(n107, m1, 'RETURN')
      IF (CSTACK(CP).NE.-irun) GO TO 2505
      IF (CSTACK(CP+2) .NE. -3)	GO TO 2504
      MJ=SP0-N
      if (mj .le. 0 .or. mj .gt. slim .or. mj+n .le. 0 .or.
     1    mj+n .gt. slim) call oerror(n107, m1, 'RETURN')
      STACK(MJ)=STACK(MJ+N)
      V(MJ)=V(MJ+N)
      SP=SP-N
      if (sp .lt. 0 .or. sp .gt. slim) call oerror(n107, m1, 'RETURN')
2504  if (cp-3 .le. 0 .or. cp .gt. slim) call oerror(n107, m1, 'RETURN')
      locL=CSTACK(CP-3)
      locLINK=CSTACK(CP-2)
      SP0=CSTACK(CP-1)
      CP=CP-4
      GO TO 1000
2505  if (cp-6 .lt. 0 .or. cp .gt. slim) call oerror(n107, m1, 'RETURN')
      IF (CSTACK(CP).NE.-iloopfor) GOTO 2506
      CP=CP-6
      GO TO 25
 2506 if (cp-3 .lt. 0 .or. cp .gt. slim) call oerror(n107, m1, 'RETURN')
      IF (CSTACK(CP).ne.-iprocedure) call oerror(n116, m1, ' ')
      CP=CP-3
      GO TO 25
C------------------------------------------------------------------
C		     V E C T O R   A S S I G N M E N T ( G E T S )
C---------------------------------------------------------------------
   27 CALL MASSGN
      GOTO 1000
C----------------------------------------------------------------------
c                   EXEC
C----------------------------------------------------------------------
 500  continue
C
      if (sp .lt. 4) call oerror(n112, m1, 'EXEC')
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'EXEC')
c		check that the argument on the stack is a string
c		check the correct isize which depends if its a litteral
c		or variable (types 7 or 14).
      loc = stack(sp-1)
      itype = stack(sp-3)
      if (itype .eq. 14) then
         isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
         isize = stack(sp-2)
      else
         call oerror(n112, m1, 'EXEC')
      endif
c		Ok, copy the string to string1
      string1 = ' '
      do 501 i = 1, isize
         rstring1(i) = c(loc+i-1)
 501  continue
c
      sp = sp - 4
c	   execcomp(string1) translates and compiles and returns a location in
c	   the K array that can be used
c	   Errors are caught by execcomp and execintp.
      kloc = execcomp(string1)
c	   however, if kloc returns a 0, it means do nothing (a NOP)
      if (kloc .le. 0) goto 1000
      if (cp+2 .lt. 0 .or. cp+4 .gt. slim) call oerror(n120, m3, ' ')
      CP=CP+4
      CSTACK(CP+2) = 0
      CSTACK(CP-3)=locL
      CSTACK(CP-2)=locLINK
      CSTACK(CP-1)=SP0
      CSTACK(CP)=-irun
      locL=kloc
      if (locl .le. 0) call oerror(n120, m3, ' ')
      locLINK=K(locL)
      SP0=SP+1
      if (sp0 .gt. slim) call oerror(n120, m3, ' ')
      GO TO 900
c
C---------------------------------------------------------------------
C		     R U N    O	P E R A	T O R			   
C---------------------------------------------------------------------
28    GO TO 1000
 2801 IF (J.gt.KXORG) call oerror(n111, m3, ' ')
      if (cp+2 .lt. 0 .or. cp+4 .gt. slim) call oerror(n120, m3, ' ')
      CP=CP+4
      CSTACK(CP+2) = 0
      CSTACK(CP-3)=locL
      CSTACK(CP-2)=locLINK
      CSTACK(CP-1)=SP0
      CSTACK(CP)=-irun
      locL=J
      if (locl .le. 0) call oerror(n120, m3, ' ')
      locLINK=K(locL)
      SP0=SP+1
      if (sp0 .gt. slim) call oerror(n120, m3, ' ')
      GO TO 900
C---------------------------------------------------------------------
C		U  S  I	 N  G				
C---------------------------------------------------------------------
31    GO TO 1000
C----------------------------------------------------------------------
C		   E L S E    O	P E R A	T O R			
C---------------------------------------------------------------------
53    continue
      if (sp .lt. 2 .or. sp .gt. slim) call oerror(n107, m1, 'ELSE')
      SP=SP-2
      locL=STACK(SP+1)
      locLINK=STACK(SP+2)
      if (loclink .eq. 1) loclink = 0
      GO TO 1001
C---------------------------------------------------------------------
C		   T H E N    O	P E R A	T O R			 
C---------------------------------------------------------------------
54    if (sp .lt. 3 .or. sp .gt. slim) call oerror(n107, m1, 'THEN')
      SP=SP-3
      IF (sngl(V(SP+1)) .gt. 0) GO TO 1000
      locL=STACK(SP+2)
      locLINK=STACK(SP+3)
      if (loclink .eq. 1) loclink = 0
      GO TO 1001
C---------------------------------------------------------------------
C		   W  H	 I  L  E				 
C---------------------------------------------------------------------
59    if (sp .lt. 3 .or. sp .gt. slim) call oerror(n107, m1, 'WHILE')
      SP=SP-3
      IF (sngl(V(SP+1)) .GT. 0) GO TO	1000
      locL=STACK(SP+2)
      locLINK=STACK(SP+3)
      if (loclink .eq. 1) loclink = 0
      if (cp-3 .lt. 0) call oerror(n107, m1, 'WHILE')
      CP=CP-3
      GO TO 1001
C---------------------------------------------------------------------
C		   D U M M Y   E N D   (IF-THEN-ELSE)		 
C---------------------------------------------------------------------
96    GO TO 1000
C---------------------------------------------------------------------
C		   W H I L E	S E T -	U P			 
C---------------------------------------------------------------------
97    if (cp+3 .gt. slim .or. cp .lt. 0) call oerror(n107, m1, 'WHILE')
      CP=CP+3
      CSTACK(CP)=-iprocedure
      CSTACK(CP-1)=locLINK
      CSTACK(CP-2)=locL+1
      GO TO 1000
C---------------------------------------------------------------------
C		   S U B S   O P E R A T O R			 
C---------------------------------------------------------------------
   98 CALL SUBS
      GOTO 1000
C---------------------------------------------------------------------
C		   N O P				       
C---------------------------------------------------------------------
   99 CONTINUE
      GO TO 1000
C---------------------------------------------------------------------
C		   P R O T E C T				       
C---------------------------------------------------------------------
   56 CONTINUE
      GO TO 1000
C---------------------------------------------------------------------
C		   SPRINT				       
C---------------------------------------------------------------------
   33 CONTINUE
c
      i = sp0
      if (i+7 .gt. sp) 
     .		call oerror(n112, m1, 'SPRINT : Two strings needed')
c
      if (stack(i+3) .ne. 3) call oerror(n112, m1, 'SPRINT')
      if (stack(i+7) .ne. 3) call oerror(n112, m1, 'SPRINT')
c
c     First string first
c
      itype = stack(i)
      isize1 = iwpr(stack(i+1))
      loc1 = stack(i+2)
      if  (itype .ne. 7) call oerror(n112, m1, 'SPRINT')
c
c     Now for 2nd string
c
      itype = stack(i+4)
      if  (itype .eq. 14) then
	isize2 = stack(i+5)
      else if (itype .eq. 7) then
	isize2 = iwpr(stack(i+5))
      else 
	call oerror(n112, m1, 'SPRINT')
      endif
      loc2 = stack(i+6)
      frmt = ' '
      call copy( isize2, c(loc2), ifrmt)
c
      sp0 = sp0  + 8
c
      if (lastblnk(frmt) .ne. 0) then
         call swrite( frmt(1:2*isize2), advrb(1:2*isize1) )
      else
         call uswrite( advrb(1:2*isize1) )
      endif
      call copy( isize1, iadvrb, c(loc1))
c
      sp0 = sp0  - 8
      sp = sp0  - 1
      if (sp .lt. 0) call oerror(n107, m1, 'SPRINT')
c
      GO TO 1000
C---------------------------------------------------------------------
C		   SREAD				       
C---------------------------------------------------------------------
   34 CONTINUE
c
      i = sp0
      if (i+7 .gt. sp) 
     .		call oerror(n112, m1, 'SREAD : Two strings needed')
c
      if (stack(i+3) .ne. 3) call oerror(n112, m1, 'SREAD')
      if (stack(i+7) .ne. 3) call oerror(n112, m1, 'SREAD')
c
c     First string first
c
      itype = stack(i)
      if  (itype .eq. 14) then
	isize1 = stack(i+1)
      else if (itype .eq. 7) then
	isize1 = iwpr(stack(i+1))
      else 
	call oerror(n112, m1, 'SREAD')
      endif
      loc1=stack(i+2)
c
      call copy( isize1, c(loc1), advrb)
c
c     Now for 2nd string
c
      itype = stack(i+4)
      if  (itype .eq. 14) then
	isize2 = stack(i+5)
      else if (itype .eq. 7) then
	isize2 = iwpr(stack(i+5))
      else 
	call oerror(n112, m1, 'SREAD')
      endif
      frmt = ' '
      loc2=stack(i+6)
      call copy( isize2, c(loc2), ifrmt)
c
      sp0 = sp0  + 8
c
      if (lastblnk(frmt) .ne. 0) then
	call sread( frmt(1:2*isize2), advrb(1:2*isize1) )
      else
	call usread(advrb(1:2*isize1) )
      endif
c
      sp0 = sp0  - 8
      sp = sp0  - 1
      if (sp .lt. 0) call oerror(n107, m1, 'SREAD')
c
      GO TO 1000
C---------------------------------------------------------------------
C		   FPRINT				       
C---------------------------------------------------------------------
   35 CONTINUE
c
      i = sp0
      if (i+4 .gt. sp) 
     .		call oerror(n112, m1, 'FPRINT : Two needed')
c
      if (stack(i+4) .ne. 3) call oerror(n112, m1, 'FPRINT')
c
c     First get the unit number
c
      unit = nint(v(i))
      if (unit .lt. 0) then
	unit = iout
      else if (unit .gt. 0) then
        do 351 j = 1, nfiles
	  if (unit .eq. ffile(j,1)) goto 352
351	  continue
        call oerror(n112, m1, 'FPRINT: No file opened with that unit')
c
352     if (ffile(j,2) .eq. RO) call oerror(n112, m1, 
     .	  'FPRINT: File not open with write permission')
      endif
c
c     Now for string
c
      itype = stack(i+1)
      if  (itype .eq. 14) then
	isize2 = stack(i+2)
      else if (itype .eq. 7) then
	isize2 = iwpr(stack(i+2))
      else 
	call oerror(n112, m1, 'FPRINT')
      endif
      loc2 = stack(i+3)
      frmt = ' '
      call copy( isize2, c(loc2), ifrmt)
c
      sp0 = sp0  + 5
c
      if (lastblnk(frmt) .ne. 0) then
         call fwrite( frmt(1:2*isize2), unit )
      else
         call ufwrite( unit )
      endif
      call flush(long(unit))
c
      sp0 = sp0  - 5
      sp = sp0  - 1
      if (sp .lt. 0) call oerror(n107, m1, 'FPRINT')
c
      GO TO 1000
C---------------------------------------------------------------------
C		   FREAD				       
C---------------------------------------------------------------------
   36 CONTINUE
c
      i = sp0
      if (i+4 .gt. sp) 
     .		call oerror(n112, m1, 'FREAD : Two needed')
c
      if (stack(i+4) .ne. 3) call oerror(n112, m1, 'FREAD')
c
c     First get the unit number
c
      unit = nint(v(i))
      if (unit .lt. 0) then
	unit = iinunit
      else if (unit .gt. 0) then
        do 361 j = 1, nfiles
	  if (unit .eq. ffile(j,1)) goto 362
361	  continue
        call oerror(n112, m1, 'FREAD: No file opened with that unit')
c
362     if (ffile(j,2) .eq. WO) call oerror(n112, m1, 
     .	  'FREAD: File not open with read permission')
      else
	call oerror(n112, m1, 'FREAD: Cannot read from standard error') 
      endif
c
c     Now for string
c
      itype = stack(i+1)
      if  (itype .eq. 14) then
	isize2 = stack(i+2)
      else if (itype .eq. 7) then
	isize2 = iwpr(stack(i+2))
      else 
	call oerror(n112, m1, 'FREAD')
      endif
      frmt = ' '
      loc2=stack(i+3)
      call copy( isize2, c(loc2), ifrmt)
c
      sp0 = sp0  + 5
c
      if (lastblnk(frmt) .ne. 0) then
	call fread( frmt(1:2*isize2), unit )
      else
	call ufread( unit )
      endif
c
      sp0 = sp0  - 5
      sp = sp0  - 1
      if (sp .lt. 0) call oerror(n107, m1, 'FREAD')
c
      GO TO 1000
C---------------------------------------------------------------------
C	       E    X	 I    T
C---------------------------------------------------------------------
900   LFLAG=1
      GO TO 10000
1000  LFLAG=2
      GO TO 10000
1001  LFLAG=3
      GO TO 10000
5000  LFLAG=4
      GO TO 10000
5001  LFLAG=5
      GO TO 10000
9999  LFLAG=6
10000 CONTINUE
10001 RETURN
C-----------------------------------------------------------------------
 9601 FORMAT (1pG15.7,1x)
      END

