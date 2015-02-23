      PROGRAM POPSGEN
C-------------------------------------------------------------------------------
C  @(#)genmain.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C     POPSGEN	is the core initializer	for the	POPS driver.
C     It initializes the K array with permanent	symbols, opcodes,
C     procedures, etc. that are	needed to run POPS.
C---------------------------------------------------------------------
C
      integer*2 ITRUE(2), IFALSE(3), istar, icmmnt, ispace, ksize,
     .          lsize, inumarg, istat, llocat, irealp, ktlp, 
     .          i1, i2, ll, n, i, idt, lli, ieof, ii, kxsize, itmp,
     .          lastblnk
      integer*2 n0, n1, n2, n12, n15, n20, n40, n50, n150, n120, m3,
     .		fshort, n10
      real*4 VX(2)
      INTEGER*2 KPAC(5), FSIZE
      integer*4 iargc, ione, iloc, long
      character*10 filedat
      character*10 ckpac
c
      include 'lsf.inc'
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
c
      EQUIVALENCE (KPAK(1),KPAC(1)), (ckpac, kpac)
c
      DATA ITRUE/'TR','UE'/, IFALSE/'FA','LS','E '/
      DATA ISTAR/'* '/
      DATA ICMMNT/'C-'/, ispace/'  '/
      DATA KSIZE/32766/, LSIZE/32766/, kxsize/32766/
      data ione /1/
c
      data n0, n1, n2, n12, n15, n20, n40, n50, n150, n120, m3
     +     /0, 1, 2, 12, 15, 20, 40, 50, 150, 120, -3/
      data n10 /10/
c
      irecsiz = 256
      KBLK = KSIZE / irecsiz
      IF (KBLK*irecsiz .LT. KSIZE) KBLK=KBLK+1
      LBLK = LSIZE / irecsiz
      IF (LBLK*irecsiz .LT. LSIZE) LBLK=LBLK+1
      kxBLK = kxSIZE / irecsiz
      IF (kxBLK*irecsiz .LT. kxSIZE) kxBLK=kxBLK+1
c
      iout = 6
      istderr = 6
      iinunit = 5
      iiotmp = 8
      istdout = 6
      istdin = 5
      imemory = 21
      karlim = 160
c     Sets up units for I/O.  Needed by PWRITE, RWDISK, etc.
c
      write(iout,*) 'Generates Line or Condar Memory files'
c
      inumarg = iargc()
c
1099  if (inumarg .le. 0) then
	write(iout,*) 'Enter L for Line or C for Condar'
        read(iinunit,1091) program
1091    format(a)
      else
	call getarg(ione,program(1:1))
	inumarg = 0
      endif
c     Gets the file name from the command line, if it is specified or
c     from standard input.
c
      if (program(1:1) .eq. 'L' .or. program(1:1) .eq. 'l') then
	program = 'Ll'
      else if(program(1:1) .eq. 'C' .or. program(1:1) .eq. 'c') then
	program = 'Cc'
      else
	write(iout,*) 'Bad arguement: ', program(1:1),' .... Try again'
	goto 1099
      endif
      cmemory = program(1:1) // 'MEMORY'
c     Finds out what memory file to generate
c
      filedat = program(1:1) // 'POPSDAT'
c     Input POPSDAT file
c
      iinunit = iiotmp
c     Changes input unit for future reads
C                                          Open files.
      FSIZE = (KBLK + LBLK + kxblk) * 4
      CALL CRE8MS(imemory,cmemory,FSIZE,ISTAT)
      if (istat .ne. 0) then
	write(istderr,*) 'Memory file cannot be created.... Terminating'
	goto 99
      endif
c
      CALL OPENMS(imemory,cmemory,n1,ISTAT)
      if (istat .ne. 0) then
	write(istderr,*) 'Memory file cannot be opened.... Terminating'
	goto 99
      endif
c
      open(unit=iinunit, file=filedat, status='old',iostat=istat)
      if (istat .ne. 0) then
	write(istderr,*) 'Popsdat file cannot be opened.... Terminating'
	goto 99
      endif
c
      REWIND (iinunit,err=99)
C                                      Initialize memory file.
C                                          Initialize.
      CALL FILL	(KSIZE,n0,K)
      CALL FILL	(LSIZE,n0,LISTF)
      CALL FILL	(kxsize,n0,Kx)
c
      MODE=1
      iedit = -1
      KTLP = 2
      slim = 400
      NEXTP=0
      LPGM=2
c
      K(3)=11
      K(5) = ksize
      K(8) = ksize
      K(9)=1
      K(10)=4
      Kx(3)= 11
      Kx(5)= kxsize
      LISTF(3)=11
      LISTF(5) = LSIZE
      kt = 6
      kt = LLOCAT(n40,K,kt) 
C					Note:  KT is the subscript of
C					       K for immediate exec.
      KT=51
      KT=LLOCAT(n150,K,KT)
      KKT(3) = 11
      KKT(5) = 140
C					One.
      LX=2
      X(1)=1.0
      TYPE=11
      CALL LTSTOR
      ONE=TAG
C					Zero.
      X(1)=0.0
      CALL LTSTOR
      ZERO=TAG
C					True.
      NKAR=4
      KPAK(1)=ITRUE(1)
      KPAK(2)=ITRUE(2)
      call creatspc(n2, tag)
      CALL creatsym (n15, kpak, nkar, tag, n0, locsym)
      C(TAG)=+1.0
      TRUE=TAG
C					False.
      NKAR=5
      KPAK(1)=IFALSE(1)
      KPAK(2)=IFALSE(2)
      KPAK(3)=IFALSE(3)
      call creatspc(n2, tag)
      CALL creatsym (n15, kpak, nkar, tag, n0, locsym)
      C(TAG)=-1.0
      FALSE=TAG
C					Compile	symbols	and opcodes.
 50   READ (iinunit,1000,END=160) JBUFF
      IF (JBUFF(1).EQ.ICMMNT .or. jbuff(1).eq.ispace) GO TO 50
      READ (CBUFF,5050,ERR=500) KPAC,NKAR,IDT,I1,I2,VX
      WRITE(istdout,5051) KPAC,NKAR,IDT,I1,I2,VX
      itmp = min(lastblnk(ckpac),n10)
      if (itmp .ne. nkar) then
         write(istderr, *) 
     .     'Inconsistent number of characters ... exiting'
         goto 99
      endif
c					oops, there is a problem with POPSDAT
c				bad number of characters, just exit
      GO TO (110,120,130,140,150,160,120,110), IDT
      call oerror(n120, m3, 'GENMAIN')
C					Real symbols.
 110  call creatspc(n2, tag)
      CALL creatsym (idt, kpak, nkar, tag, n0, locsym)
      C(TAG)=VX(1)
      GO TO 50
C					Arrays.
 120  call creatspc(n2, tag)
      CALL creatsym (idt, kpak, nkar, tag, n0, locsym)
      L=LOCSYM+3
      LL=LLOCAT(fshort(2*I1+2),K,L)
      N=1
      LLI=LL+2
      DO 122 I=1,I1
	 N=N*VX(I)
	 K(LLI)=1
	 K(LLI+1)=VX(I)+.1
 122 	 LLI=LLI+2
      K(LL)=N
      K(LL+1)=I1
      L=1
      LL=LLOCAT(fshort(N*2-2),Kx,L)
      GO TO 50
c
 130  CONTINUE
      GO TO 50
C					Operators.
 140  CONTINUE
C					Pseudo operators.
 150  CALL creatsym (idt, kpak, nkar, i1, n1, locsym)
      GO TO 50
C
 160  CONTINUE
      CALL ARAINIT
      CALL KDUMP (n1,n12,K,C)
      CALL KDUMP (K(1),fshort(K(1)+n20),K,C)
      CALL KDUMP (fshort(n50+n1),fshort(n50+n12),K,C)
      CALL KDUMP (n1,n12,Kx,C(irealp(ksize)+n1) )
      CALL KDUMP (n1,n12,listf,listf)
C
      ILOC = KBLK+kxblk+1
      call writms(imemory, iloc, long(lblk), listf, istat)
      if (istat .ne. 0) then
	write(istderr,*) 'Cannot write to Memory file.... Terminating'
	goto 99
      endif
C					Compile	Procedures.
      mode = 0
 200  CALL PREAD (KARBUF,IEOF)
      WRITE (istdout,1022) (KARBUF(I),I=1,nbytes)
 1022 FORMAT (1X,160a1)
      GO TO (210,390), IEOF
      call oerror(n120, m3, 'GENMAIN')
C					   Comment Card.
 210  IF (KARBUF(1).EQ.ISTAR .or. karbuf(1).eq.ispace) GO TO 200
      KBPTR=1
      CALL POLISH
      IF ((AP.ne.0).and.(MODE.eq.0)) call oerror(n2, n1, ' ')
C					   Mode	0 -> Immediate exec.
C					   Mode	1 -> Compilation.
      IF (MODE.EQ.1) GO	TO 200
      LPGM = 2
      MODE = 0
      GO TO 200
C
 390  WRITE (istdout,1010)
C					Lock of	resident PROCEDURES.
      K(7)=K(3)
      listf(7) = listf(3)
      kx(7)= kx(3)
C					Save core resident memories.
      DO 400 II=1,4
         ILOC = (II-1)*(KBLK+kxblk+LBLK)+1
         WRITE (istdout,1999) II,ILOC,KBLK
 1999    FORMAT (' WRITING TO DISK',3I8)
         call writms(imemory, iloc, long(kblk), k, istat)
         if (istat .ne. 0) then
	   write(istderr,*) 'Cannot write to Memory file.... Terminating'
	   goto 99
         endif
         ILOC = ILOC + KBLK
         call writms(imemory, iloc, long(kxblk), kx, istat)
         if (istat .ne. 0) then
	   write(istderr,*) 'Cannot write to Memory file.... Terminating'
	   goto 99
         endif
         ILOC = ILOC + kxblk
	 call writms(imemory, iloc, long(lblk), listf, istat)
         if (istat .ne. 0) then
	   write(istderr,*) 'Cannot write to Memory file.... Terminating'
	   goto 99
         endif
 400	 CONTINUE
      call closms(imemory, istat)
      if (istat .ne. 0) then
	   write(istderr,*) 'Cannot close Memory file.... Terminating'
	   goto 99
      endif
   99 CONTINUE
      STOP
C----------------------------------------- Unit istdout read error.
 500  WRITE (istderr,5100) JBUFF,KPAC,NKAR,IDT,I1,I2,VX
      GO TO 50
C-----------------------------------------------------------------------
 1000 FORMAT (80A2)
 5000 FORMAT (1X,80A2)
 5050 FORMAT(5A2,1X,I3,1X,I3,1X,I4,1X,I4,2(1X,F7.2))
 5051 FORMAT(1X,5A2,1X,I3,1X,I3,1X,I4,1X,I4,2(1X,F10.2))
 5100 FORMAT (1X,80A2,/,' READ AS:',/,' KPAC:',5A2,/,
     .        ' NKAR:',I3,/,  ' IDT:',I3,/,
     .        ' I1:',I3,/,    ' I2:',I3,/,
     .        ' VX:',2F7.3)
 1010 FORMAT(' POPSGEN COMPLETE')
      END
