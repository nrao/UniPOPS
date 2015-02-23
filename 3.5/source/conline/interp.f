      SUBROUTINE INTERP	(KENTRY)
C-------------------------------------------------------------------------------
C  @(#)interp.f	5.2 09/10/98
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C   POPS INTERPRETER
C---------------------------------------------------------------------
c
      integer*2 kentry, ier, j, kk, j1, lflag, inspacen
      integer*2 m1, m3, n107, n120, n130, n0, fshort
      logical*2 amatherr
c
      INCLUDE 'core.inc'
      include 'appl.inc'
      include 'stk.inc'
      include 'cio.inc'
      include 'smstuf.inc'
c
      include 'tags.inc'
c
      data m1, m3, n107, n120, n130, n0 /-1, -3, 107, 120, 130, 0/
c
C
C=======================================================================
C
      call savestatus
      call clrruntag
c
      nspacen = 0.
      amatherr = .false.
c     Both nspacen and amatherr are used to keep track of errors occuring
c     during Array Math processing.  If all is going well with Array Math,
c     then amatherr = .false. and nspacen has a value between 1 and MAX_DATA_POINTS.
c     If amatherr = true, then we must skip over all commands until the end
c     of the array math processing.  The end of array math processing is
c     flaged when nspacen takes on the value of MAX_DATA_POINTS+1.  
c
      SP=0
      CP=0
      SP0=1
      LINK=K(KENTRY)
      L=KENTRY
C					Program	chunk begins.
 10   L=L+1
C					   Advance program counter.
 20   L=L+1
 25   J=K(L)
c
      if (amatherr .and. j .ne. iforend .and. j .ne. i0 ) goto 20
c     Array math error has occured so the only allowed command is the 'END' 
c     command which flags the end of array math processing.  All other
c     commands are skiped.  Stop processing if nothing more to process (i.e.,
c     J = 0)
c     
C					Debug.
      IF (IDEBUG.gt.0) then
	 KK=CSTACK(CP)
	 WRITE (iout,200,IOSTAT=IER) L,J,SP,SP0,STACK(SP),V(SP),CP,
     .                   CSTACK(CP), C(KK)
 200     FORMAT (' INTERP: ',2I6,2I3,I6,G12.5,2I6,G12.5)
      endif
C					Operand	- push on stack.
      IF(J.lt.0) then
	 SP=SP+1
         IF (SP.lt.1.or.SP.gt.SLIM.or.CP.lt.0.or.CP.gt.SLIM) 
     .		call oerror(n107, m1, ' ')
  	 J1=-J
	 STACK(SP)=J1
         V(SP)=C(J1)
	 GO TO 20
      endif
C					Verb linkages.
      call setflags
      IF ((J.ge.iaubegin(1)).and.(J.lt.ilastverb)) then
	 CALL VERBS (J)
	 lflag = 2
      else if (j.ge.ifunct.and. j.lt.ifunfunct) then
	CALL FUNCT(LFLAG,fshort(J-ifunct+1))
      else if (j.ge.ifunfunct.and. j.lt.igenop) then
	CALL funfunct(LFLAG,fshort(J-ifunfunct+1))
      else if (j.ge. igenop .and. j .lt. irtops) then
	call genop(lflag, fshort(j-igenop+1))
      else
         CALL QUICK(LFLAG,J, l, link)
      endif
c
110   call gpflush
c     Flush out any pending graphics commands
c
      inspacen = int(nspacen + 0.5)
      if (amatherr .and. inspacen .ge. MAX_DATA_POINTS + 1) then
	amatherr = .false.
	nspacen = 0.
c	Array math error has occured and the END of the array math processing
c	has been found.  We can now continue processing stuff after that END
c
      else if(amatherr) then
	goto 20
c       An array math  error has occured as well as an END, however, that 
c       END was NOT the END which defines the end of array math processing.
c       We need to continue skipping until the correct END is found.
c
      endif
c
      goto (10, 20, 25, 5000, 5001, 99 ), lflag
      call oerror(n120, m3, 'INTERP')
C---------------------------------------------------------------------
C		   S T O R E   I N T E R M E D I A T E	 V A L U E
C---------------------------------------------------------------------
5000  SP=SP-1
5001  STACK(SP)=0
      V(SP)=XX
      GO TO 20
C					Normal exit.
99    if (sp .ne. 0) call oerror(n130, n0, 'Consequences are unknown!!')
      RETURN
C--------------------------------------------------------------------
c
c****************************
c
      entry execerror
c
      lflag = 2
      goto 110
c
c     You will re-enter interp at this point if a run-time error occured
c     in one of the above routines.
c
c****************************
c
      entry amatherror

      nspacen = MAX_DATA_POINTS
      amatherr = .true.
      lflag = 2
      goto 110
c     You will enter interp at this point if an array math error occured.
c
      END
