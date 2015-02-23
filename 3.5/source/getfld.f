      SUBROUTINE GETFLD
C-------------------------------------------------------------------------------
C  @(#)getfld.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C--------------------------------------------------------------------
C   Field scanning routine
C     Finds the	next non-blank character in KARBUF.  Determines
C  whether the token begun with	that character is symbolic
C  (1st	char is	A->Z), numeric (1st char 0->9,	or .), or a hollerith
C  constant begin with apostrophe.  After the field length is found,
C  appropriate calls are made to the symbol processing routine,
C  number scanning routine, etc.  Communication	back to	polish is
C  via TYPE and	TAG parameters,	determined by the processors
C  SYMBOL, GETNUM, LTSTOR, etc.
c
c  NTYPE = type of symbol that SYMBOL is to create if the symbol doesn't
c	already exist. 
C
C  Modified 890105 [PPM] changed includes to .inc, lowercase and made
C                        the above header more readable.
c           8903   [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
C
      character*60 cix
      character*160 cbuff2
c
      integer*2 isp, icom, nine, izero, ia, iz, idot, iquote, istar,
     .          igt, ilt, iul, iequal, ibrace, inot, ilb, 
     .          iblank, i0, limit, iia, iiz, iinine, iizero, iiul, 
     .          iilb, m, it, j, kp, iwpc, iwpr, lastblnk,
     .          m1, irealp, ibrace2, iquote1, iquote2
      integer*2 n1, n2, n4, n5, n14, n37, fshort
c
      INCLUDE     'smstuf.inc'
      include     'stk.inc'
      INCLUDE     'cio.inc'
c
      equivalence (cix, x(1))
C
      DATA ISP/' '/,ICOM/','/,NINE/'9'/,IZERO/'0'/,IA/'A'/,IZ/'Z'/,
     .	IDOT/'.'/,IQUOTE1/''''/,ISTAR/'*'/,IGT/'>'/,ILT/'<'/,
     .      IUL/'_'/, IEQUAL/'='/, ibrace/'{'/, inot/'~'/, ilb/'#'/
      data iblank/'  '/,ibrace2/'}'/, IQUOTE2/'"'/
      DATA I0/0/
c
      data n1, n2, n4, n5, n14, n37 /1, 2, 4, 5, 14, 37/
C
C=======================================================================
c
C---------------------------------------  ZERO PAD CHAR.
C
      limit = min(nbytes, karlim)
c
      CALL GTBYTE (IIA,IA,I0)
      CALL GTBYTE (IIZ,IZ,I0)
      CALL GTBYTE (IININE,NINE,I0)
      CALL GTBYTE (IIZERO,IZERO,I0)
      CALL GTBYTE (IIUL,IUL,I0)
      call gtbyte (iilb, ilb, i0)
C--------------------------------------------------------------------
5      TAG=0
      NKAR=0
      TYPE=0
      X(1)=0
C					SKIP LEADING BLANKS.
   10 IF (KBPTR.GT.LIMIT) GO TO	99
	 M=KARBUF(KBPTR)
	 IF (M.NE.ISP) GO TO 20
	 KBPTR=KBPTR+1
	 GO TO 10
C					CLASSIFY TYPE BY FIRST CHAR.
20    CALL GTBYTE (IT,M,I0)
      if (kbptr .eq. 1 .and. it .eq. iilb) then
	kbptr = limit+1
	goto 99
      endif
c     Skips parsing the input line if the first character is a # sign 
c     (indicating a comment line).
c
      IF ((IT.LE.IIZ).AND.(IT.GE.IIA)) GO TO 40
      IF (M.EQ.IDOT) GO	TO 60
      IF (M.EQ.IQUOTE1  .or. m .eq. iquote2)  GO TO 90
      if (m .eq. ibrace .or. m .eq. ibrace2) goto 110
      IF ((IT.LE.IININE).AND.(IT.GE.IIZERO)) GO	TO 60
      GO TO 80
C--------------------------------------------------------------------
C      S Y M B O L I C	  F I E	L D
C--------------------------------------------------------------------
   40 CONTINUE
      J=KBPTR
   42 NKAR=NKAR+1
c		Look for end of line or end of symbol (flagged by 
c		non-alphanumeric character)
	 J=J+1
         IF (J.gt.LIMIT) call oerror(n4,n1,' ')
         M=KARBUF(J)
	 CALL GTBYTE (IT,M,I0)
	 IF ((IT.LE.IIZ).AND.(IT.GE.IIA)) GO TO	42
	 IF (IT.EQ.IIUL) GO TO 42
	 IF ((IT.LE.IININE).AND.(IT.GE.IIZERO))	GO TO 42
 44   KP=1
c
c		Pack input string from start of field to end into
c		KPAK; fill rest of KPAK with blanks
c
      if (nkar .gt. 10) call oerror(n5,n1,' ')
      CALL KPACK (NKAR,KARBUF,KBPTR,KPAK,KP)
      call fill (fshort(30-iwpc(nkar)), iblank, kpak(iwpc(nkar)+1))
      CALL SYMBOL
      GO TO 99
C--------------------------------------------------------------------
C	 N U M E R I C	  F I E	L D
C--------------------------------------------------------------------
 60   J=KBPTR
      CALL GETNUM (cbuff(1:limit), kbptr, x(1))
      kbptr = kbptr - 1
      NKAR=KBPTR-J
      LX = IWPR(n1)
      TYPE=11
      CALL LTSTOR
      GO TO 99
c----------------------------------------------
c    Brace -- AMATH parsing
c----------------------------------------------
110   continue
      call amathparser(cbuff(kbptr:nbytes), cbuff2)
      if (lastblnk(cbuff2) + kbptr - 1 .ge. karlim) 
     +       call oerror(n4,n1,' ')
      cbuff(kbptr:) = cbuff2
      nbytes = lastblnk(cbuff) + 1
      call unpack(iwpc(nbytes), jbuff, karbuf)
      goto 10
c
C--------------------------------------------------------------------
C	 ' O T H E R ' (SUCH THINGS AS + - * / ** ETC.)
C--------------------------------------------------------------------
 80   J=KBPTR
   82 NKAR=NKAR+1
	 J=J+1
         IF (NKAR.gt.2) call oerror(n2,n1,' ')
         M=KARBUF(J)
	 M1=KARBUF(J-1)
c
c		Watches out for double symbols **, <=, >=, etc.
c
	 IF ((M1.EQ.ISTAR).AND.(M.EQ.ISTAR)) GO	TO 82
	 IF  (M.NE.IEQUAL) GO TO 44
	 IF ((M1.EQ.ILT).OR.(M1.EQ.IGT).or.(m1.eq.inot) ) GO TO 82
      GO TO 44
C--------------------------------------------------------------------
C	 H O L L E R I T H    F	I E L D	S
C--------------------------------------------------------------------
 90   CONTINUE
c
      iquote = m
c
      TYPE=14
      KBPTR=KBPTR+1
      J=KBPTR
c
c		Hollerith field can only hold 60 characters (set by
c		dimension of IX array (= X array))
c
c		Steps through line until next closing quote is found
c		or until end of line (generates an error then)
c
  95  IF (KARBUF(J).ne.IQUOTE) then
	 NKAR=NKAR+1
         IF (NKAR.gt.60) call oerror(n14,n1,' ')
         J=J+1
	 if (j .gt. limit) call oerror(n37,n1,' ')
	 cix(nkar:nkar) = cbuff(j-1:j-1)
	 GO TO 95
      else
	 if (j .lt. limit .and. karbuf(j+1) .eq. iquote) then
	    nkar = nkar + 1
            IF (NKAR.gt.60) call oerror(n14,n1,' ')
            J=J+2
	    if (j .gt. limit) call oerror(n37,n1,' ')
	    cix(nkar:nkar) = cbuff(j-1:j-1)
	    goto 95
	 endif
      endif
c
      if (mod(nkar,2).ne.0) cix(nkar+1:nkar+1) = ' '
      lx = irealp(nkar)
      kbptr = kbptr + nkar
c     Use the packed, non-case-converted string instead of unpacked,
c     uppercase string
c
      CALL LTSTOR
      KBPTR=J+1
 99   CONTINUE
      RETURN
      END
