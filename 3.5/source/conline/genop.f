      SUBROUTINE  genop(LFLAG,J)
C-------------------------------------------------------------------------------
C  @(#)genop.f	5.4 09/10/98
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C GENOP contains frequently used commands
C---------------------------------------------------------------------
      INCLUDE 'appl.inc'
c
      INTEGER*2 RSTRNG(6), DSTRNG(6)
      integer*2 lflag, j, ix, iwpr, itype, irealp, blank, isize, jcnt, 
     .          iy, inum, chngfile, iptwh2, i1, i2, ier, lastblnk
      integer*2 n0, n1, n9, n31, n33, n72, n112, n114, n120, n245, n301, 
     .          n352, n353, n354, n356, n370, n372, n373, m1, m2, m3,
     .		fshort, ifont, ispace, inum4, n355, n266, n133
      integer*4 ilct, system, jj, loc, istat, icnum, istop,
     .          itag, i, irtn, iunit, nchars, l0
      character*72 stch
      character*60 string1, string2, string3
      character*1023 fullname
      logical*2 opened, inquirefile, string, okfile, fptfmt
      real*4 xtime, rstring1(30), rstring2(30), testvalue
      real*8 pi, raddeg, radsec, aaa, ddd, ahr, dsgn, ddeg, ff1, ff2, 
     .       ff3, c62, s62, decl, cd, sd, alpha, ca, sa, sinb, cosb, 
     .       glong, glat, twopi, cb, sb, cl, sl, sind, cosd, ra, tand, 
     .       dec, value, amn, dmn, cterm, tanb, sterm, asec, 
     .       dsec, ddate
      real*8 nrao140, nrao300, proto12m
      character*16 existence(0:3), position(0:1), access(0:2)
      character*24 zdatetime
c
      INCLUDE 'cform.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
      include 'core.inc'
      include 'errors.inc'
      include 'mappl.inc'
      include 'mform.inc'
c
      equivalence (string1, rstring1), (string2, rstring2)
C
      data n0, n1, n9, n31, n33, n72, n112, n114, n120, n245, n301, 
     .     n355, n352, n353, n354, n356, n370, n372,  n373, m1, m2, m3
     .   / 0, 1, 9, 31, 33, 72, 112, 114, 120, 245, 301, 355, 
     .     352, 353, 354, 356, 370, 372, 373, -1, -2, -3/
      data n266, n133 /266, 133/
      data l0 /0/
      data blank/'  '/, nrao140/'NRAO 43M'/, nrao300/'NRAO 93M'/
      data proto12m /'PROTO12M'/
      data existence /'Unknown','Old','New','Scratch'/
      data position /'Rewind', 'End of file'/
      data access /'Read and write', 'Write only', 'Read only'/
C
      JJ=J
      GO TO (32,33,34,35,52,53,54,55,56,57,90,100,120,130,240,250,
     .       260,270,295,141,142,143,144,145,146,147,148,149,150,151,
     .	     152,153,154,155,156,157,158,159,160,410,411,412,413,420,
     .	     421,422,423,280,290,291,292,293,294,310,320,330,340,350,
     .	     351,352,353,354,355,356,357), JJ
      call oerror(n120, m3, 'GENOP')
c-------------------------
C                      PLACE  VCTR  CHAR        
c-------------------------
   32 continue
      if (sp .lt. 2) call oerror(n112, m1, 'PLACE : Two needed')
      IX=V(SP-1)
      IY=V(SP)
      SP=SP-2
      CALL PLACEwp (IX,IY,n33)
      iglinex = ix
      igliney = iy
c     Update the position where the next 'GRAPHICS' cammand will
c     start to write characters.
c
      GO TO 1000
c-------------------------
   33 continue
      if (sp .lt. 2) call oerror(n112, m1, 'VCTR : Two needed')
      IX=V(SP-1)
      IY=V(SP)
      SP=SP-2
      CALL VCTRwp(IX,IY,n33,sclchar,0.)
      iglinex = ix
      igliney = iy
      GO TO 1000
c-------------------------
   34 continue
      if (sp .lt. 4) call oerror(n112, m1, 'CHAR')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'CHAR')
c     Make sure we have a array, string, or literal on the stack
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	jcnt = 4*irealp(stack(sp-2))
      else if (itype .eq. 7) then
	jcnt = 4*stack(sp-2)
      else 
	call oerror(n112, m1, 'CHAR')
      endif
c     Get the correct JCNT which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      SP=SP-4
      if (sclchar .ne. 1.) call charsize(sclchar)
      CALL PCHAR (c(LOC),jcnt)
      if (sclchar .ne. 1.) call charsize(1.)
      GO TO 1000
c-------------------------
c    PAGE
c-------------------------
   35 CONTINUE
      call clrpage
      call resetcur
      GO TO 1000
c-------------------------
C            EQTOGAL   -   RA, DEC TO L, B   
c-------------------------
  52  CONTINUE
      if (sp .lt. 2) call oerror(n112, m1, 'EQTOGAL : Two needed')
      PI = 4. * ATAN (1.)
      RADDEG = PI / 180.
      RADSEC = RADDEG / 3600.
      FF1 = 282.25 * RADDEG
      FF2 = 62.6 * RADDEG
      FF3 = 33. * RADDEG
C
      AAA = V(SP - 1)
      DDD = V(SP)
      SP = SP - 2
C
      AHR = AINT (AAA * 1.E-4)
      AMN = AINT (AAA * 1.E-2) - 1.E2 * AHR
      ASEC = AAA - AHR * 1.E4 - AMN * 1.E2
      ALPHA = (ASEC + 60. * AMN + 3600. * AHR) * 15. * RADSEC
C
      DSGN = 1.
      IF (DDD .LT. 0.) DSGN = -1.
      DDD = ABS (DDD)
      DDEG = AINT (DDD * 1.E-4)
      DMN = AINT (DDD * 1.E-2) - DDEG * 1.E2
      DSEC = DDD - DDEG * 1.E4 - DMN * 1.E2
      DECL = (DSEC + 60. * DMN + 3600. * DDEG) * DSGN * RADSEC
C
      C62 = COS (FF2)
      S62 = SIN (FF2)
      CD = COS (DECL)
      SD = SIN (DECL)
      CA = COS (ALPHA - FF1)
      SA = SIN (ALPHA - FF1)
C
      STERM = CD * SA * C62 + SD * S62
      CTERM = CD * CA
      SINB = SD * C62 - CD * SA *S62
      COSB = SQRT(1. - SINB * SINB)
      TANB = SINB / COSB
C
      GLONG = (ATAN2 (STERM,CTERM) + FF3) / RADDEG
      IF (GLONG .LT. 0.) GLONG = GLONG + 360.
      GLAT = ATAN (TANB) / RADDEG
      stch = '*** ERROR in EQTOGAL ***'
      write (stch,9111,iostat=istat) GLONG, GLAT
      call pwrite(stch, n72)
 9111 FORMAT (10X, 2F12.4)
C
      GO TO 1000
c-------------------------
C                     GALTOEQ - L, B TO RA, DEC          
c-------------------------
  53  CONTINUE
      if (sp .lt. 2) call oerror(n112, m1, 'GALTOEQ : Two needed')
      TWOPI = 8. * ATAN (1.)
      RADDEG = TWOPI / 360.
      FF1 = 282.25 * RADDEG
      FF2 = 62.6 * RADDEG
      FF3 = 33. * RADDEG
C
      GLONG = V(SP - 1) * RADDEG
      GLAT = V(SP) * RADDEG
      SP = SP - 2
C
      C62 = COS (FF2)
      S62 = SIN (FF2)
      CB = COS (GLAT)
      SB = SIN (GLAT)
      CL = COS (GLONG - FF3)
      SL = SIN (GLONG - FF3)
C
      STERM = CB * SL * C62 - SB * S62
      CTERM = CB * CL
      SIND = CB * SL * S62 + SB * C62
      COSD = SQRT(1. - SIND * SIND)
      TAND = SIND / COSD
C
      RA = ATAN2 (STERM, CTERM) + FF1
      RA = MOD (RA, TWOPI)
      RA = ( RA / TWOPI ) * 24.
      DEC = ATAN (TAND)
      DEC = ( DEC / TWOPI ) * 360.
C
      CALL RNS8(RA, n0, RSTRNG)
      CALL RNS8(DEC, n1, DSTRNG)
C
      stch = '*** ERROR in GALTOEQ ***'
      write(stch, 9112, iostat=istat) rstrng, dstrng
      call pwrite(stch, n72)
 9112 FORMAT ( 10X, 6A2, 5X, 6A2)
C
      GO TO 1000
c-------------------------
C    Dump the K array.
c-------------------------
   54 CONTINUE
      if (sp .lt. 2) call oerror(n112, m1, 'DUMP : Two needed')
      IX=V(SP-1)
      IY=V(SP)+IX
      SP=SP-2
      CALL KDUMP (IX,IY,K,C)
      GO TO 1000
c-------------------------
C                                          CRT,PRINTER,OUTPUT
c-------------------------
   55 CONTINUE
      iout = istdout
      graphon = .false.
c
      call filecomp(cprtout, fullname)
      if (opened(fullname)) then
	close(unit=abs(iprtout),iostat=istat)
        if (istat .ne. 0) call oerror(n356, n0, cprtout)
      endif
      GO TO 1000
C
c-------------------------
   56 CONTINUE
      if (iprtout .gt. 0) then
        call filecomp(cprtout, fullname)
        if (.not. opened(fullname)) then
		open(unit=iprtout, file=fullname, 
     1		     status='unknown', fileopt = 'eof', iostat=istat)
     		if (istat .ne. 0) call oerror(n352, m2, cprtout)
        endif
        iout = iprtout
        graphon = .false.
        GO TO 1000
      else
	call oerror(n353, m2, cprtout)
      endif
C
c-------------------------
   57 CONTINUE
      call filecomp(cprtout, fullname)
      if (opened(fullname)) then
	CLOSE (UNIT=abs(iprtout),iostat=istat)
	if (istat .ne. 0) call oerror(n356, n0, cprtout)
      endif
c     File must be closed before we can print it
c
      if (inquirefile(fullname) ) then
	 IX=system('printit ' // printer // ' text ' // 
     .               fullname(1:lastblnk(fullname)))
         if (ix .ne. 0) call oerror(n9,n0,'OUTPUT')
c	 Print it if it exists
c
         if (iout .eq. iprtout) then
c
	   if (iprtout .gt. 0) then
	     open(unit=iprtout, file=fullname,status='old',
     1            fileopt='eof',iostat = istat)
	     if (istat .ne. 0) call oerror(n352, m2, cprtout)
	   else
	     call oerror(n352, m2, 'Printout file is not available')
           endif
c	   Reopen printout file if the output device is the file,
c	   the file exists, and has a unit number > 0
c
	endif
      else
	call oerror(n353, m2, cprtout)
      endif
c
      GO TO 1000
c------------------------------------------------------------------------
c     TCOPY
c------------------------------------------------------------------------
90    continue
      call tcopy
      goto 1000
c-------------------------------------------------------------------------
c     GCOPY
c-------------------------------------------------------------------------
100   continue
      call gcopy
      goto 1000
c-------------------------------------------------------------------------
c     PAUSE
c-------------------------------------------------------------------------
120   continue
      if (sp .lt. 1) call oerror(n112, m1, 'PAUSE : One needed')
      xtime = v(sp)
      sp = sp - 1
      call zwait(xtime)
      goto 1000
c-------------------------------------------------------------------------
c     ERRON
c-------------------------------------------------------------------------
130   continue
      erron = .true.
      goto 1000
c-------------------------------------------------------------------------
c     ERROFF
c-------------------------------------------------------------------------
240   continue
      erron = .false.
      errcode = 0
      goto 1000
c-------------------------------------------------------------------------
c    FULLON
c-------------------------------------------------------------------------
250   continue
      pagefull = .true.
      goto 1000
c-------------------------------------------------------------------------
c    FULLOFF
c-------------------------------------------------------------------------
260   continue
      pagefull = .false.
      goto 1000
c
C-------------------------------------------------------------------
c    Graphon
c-------------------------------------------------------------------
270   continue
      graphon = .true.
      goto 1000
c----------------------------------
c     SETD0
c----------------------------------
  141 iptwh2 = 1
      goto 1209
c----------------------------------
c     SETD1
c----------------------------------
  142 iptwh2 = 2
      goto 1209
c----------------------------------
c     SETD2
c----------------------------------
  143 iptwh2 = 3
      goto 1209
c----------------------------------
c     SETD3
c----------------------------------
  144 iptwh2 = 4
      goto 1209
c----------------------------------
c     SETD4
c----------------------------------
  145 iptwh2 = 5
      goto 1209
c----------------------------------
c     SETD5
c----------------------------------
  146 iptwh2 = 6
      goto 1209
c----------------------------------
c     SETD6
c----------------------------------
  147 iptwh2 = 7
      goto 1209
c----------------------------------
c     SETD7
c----------------------------------
  148 iptwh2 = 8
      goto 1209
c----------------------------------
c     SETD8
c----------------------------------
  149 iptwh2 = 9
      goto 1209
c----------------------------------
c     SETD9
c----------------------------------
  150 iptwh2 = 10
c
 1209 if (sp .ge. 2) then
	 value = v(sp)
         icnum = nint(v(sp-1))
         sp = sp - 2
         istop = nint(dtwh(c12spn,iptwh2) + dtwh(c12ni,iptwh2)) -1
         if (icnum .gt. istop .or. icnum .le. 0) then
	    call oerror(n245, m2, 'D0/D9')
         else
            twh(idatoff+icnum,iptwh2) = value
         endif
      else
         call oerror(n112, m1, 'D0/D9: Two needed')
      endif
      goto 1000
c----------------------------------
c     SETH0
c----------------------------------
  151 iptwh2 = 1
      goto 1239
c----------------------------------
c     SETH1
c----------------------------------
  152 iptwh2 = 2
      goto 1239
c----------------------------------
c     SETH2
c----------------------------------
  153 iptwh2 = 3
      goto 1239
c----------------------------------
c     SETH3
c----------------------------------
  154 iptwh2 = 4
      goto 1239
c----------------------------------
c     SETH4
c----------------------------------
  155 iptwh2 = 5
      goto 1239
c----------------------------------
c     SETH5
c----------------------------------
  156 iptwh2 = 6
      goto 1239
c----------------------------------
c     SETH6
c----------------------------------
  157 iptwh2 = 7
      goto 1239
c----------------------------------
c     SETH7
c----------------------------------
  158 iptwh2 = 8
      goto 1239
c----------------------------------
c     SETH8
c----------------------------------
  159 iptwh2 = 9
      goto 1239
c----------------------------------
c     SETH9
c----------------------------------
  160 iptwh2 = 10
c
 1239 if (sp .ge. 5 .and. stack(sp) .eq. 3) then
c
c	String header parametr
c
         itag = stack(sp-1)
         itype = stack(sp-3)
         if (itype .eq. 14) then
            isize = irealp(stack(sp-2))
         else if (itype .eq. 7) then
            isize = stack(sp-2)
         else 
            call oerror(n112, m1, 'H0/H9: Bad arguments')
         endif
c	Get the correct isize which depends if its a literal or variable.
c	Check to make sure that it is a literal or string variables (types
c	7 and 14).
c
         i1 = nint(v(sp-4))
         sp = sp - 5
c       i1 = the location in DTWH in which the user wants to change the
c       header word
c
         if (i1 .le. 0 .or. i1 .gt. idatoff/2) call oerror(n245, m2,
     .		'H0/H9')
c
         string = .false.
         do 1238 i = 1, numastrings
            if (i1 .eq. astrings(i)) string = .true.
 1238    continue
c			deal with special cases, class 9 and 11
         if (i1 .eq. c9cff .and. dtwh(c1tel,iptwh2) .ne. nrao140 .and.
     .		dtwh(c1tel,iptwh2) .ne. nrao300) string = .false.		
         if (i1 .gt. c11ovv(1) .and. i1 .lt. c12fr .and. string) then
            string = .false.
            if (dtwh(c3typ11,iptwh2) .eq. proto12m) then
               do 1240 i = 1, 10
                  if (i1 .eq. c11pvd(i) .or. i1 .eq. c11ppt(i)) 
     .                        string = .true.
 1240          continue
            else
               do 1241 i = 1, 22
                  if (i1 .eq. c11ovd(i) .or. i1 .eq. c11opt(i))
     .                        string = .true.
 1241          continue
            endif
         endif
c
         if (.not. string) 
     .		call oerror(n245, m2, 'H0/H9: String header word')
c	Make sure we are trying to set a string header parameter.
c
         if (i1 .eq. c1ona .or. i1 .eq. c1sna .or.
     .       i1 .eq. c12rxi) then
            inum4 = 4
          else if (i1 .eq. c9cff) then
            inum4 = 6
         else
            inum4 = 2
            do 1242 i = 1, 10
               if (i1 .eq. c11ppt(i)) inum4 = 8
 1242       continue
	 endif
c	INUM4 = the maximum number of R*4 words which can be stored in 
c	header words
c
         if (isize .gt. inum4) call oerror(n114, m1, 
     .		'H0/H9: Too many characters')
c 	Header words can only hold inum4 R*4 words
c
         i1 = (i1-1)*4 + 1
         call copy(iwpr(isize), c(itag), itwh(i1, iptwh2) )
         if (inum4 .gt. isize) call fill(iwpr(fshort(inum4-isize)),
     1         blank, itwh(i1+iwpr(isize),iptwh2) )
c	Copy over the header words; fill the rest of the header word
c	with blanks
c
      else if (sp .ge. 2) then
c
c	Real value header parametr
c
         value = v(sp)
         i1 = nint(v(sp-1))
         sp = sp - 2
c       i1 = the location in DTWH in which the user wants to change the
c       header word
c
         if (i1 .le. 0 .or. i1 .gt. idatoff/2) call oerror(n245, m2,
     .		'H0/H9')
c
         string = .false.
         do 1237 i = 1, numastrings
            if (i1 .eq. astrings(i)) string = .true.
 1237    continue
c			deal with special cases, class 9 and 11
         if (i1 .eq. c9cff .and. dtwh(c1tel,iptwh2) .ne. nrao140 .and.
     .		dtwh(c1tel,iptwh2) .ne. nrao300) string = .false.		
         if (string .and. i1 .gt. c11ovv(1) .and. i1 .lt. c12cf) then
            string = .false.
            if (dtwh(c3typ11,iptwh2) .eq. proto12m) then
               do 1243 i = 1, 10
                  if (i1 .eq. c11pvd(i) .or. i1 .eq. c11ppt(i)) 
     .                       string = .true.
 1243          continue
            else
               do 1244 i = 1, 22
                  if (i1 .eq. c11ovd(i) .and. i1 .eq. c11opt(i))
     .                       string = .true.
 1244          continue
            endif
         endif
c
         if (string)
     .	     call oerror(n245, m2, 'H0/H9: Real-Value header word')
c	Make sure we are trying to set a real-value header parameter.
c
         if (i1 .eq. c12ni) then
            if (value .lt. 0 .or.
     .          value + dtwh(c12spn,iptwh2) - 1 .gt. MAX_DATA_POINTS)
     .        call oerror(n266, m2, 'H0/H9')
         else if (i1 .eq. c12spn) then
            if (value .lt. 1 .or.
     .          dtwh(c12ni,iptwh2) + value - 1 .gt. MAX_DATA_POINTS)
     .        call oerror(n266, m2, 'H0/H9')
         endif
c		make sure array limit can't be exceeded
         dtwh(i1,iptwh2) = value
c
      else
c
         call oerror(n112, m1, 'H0/H9: Two needed')
c
      endif
      goto 1000
c
c-------------------------------------------------------------------
c    LOGON
c-------------------------------------------------------------------
280   continue
c
      if (ilogout .gt. 0) then
        call filecomp(clogout, fullname)
        if (.not. opened(fullname)) then
	   open(unit=ilogout, file=fullname, 
     1		     status='unknown', fileopt = 'eof', iostat=istat)
     	   if (istat .ne. 0) call oerror(n352, m2, clogout)
	endif
	if (.not. dolog) then
	   dolog = .true.
	   write(ilogout, 1030, iostat=istat)'# Command logging ON -- ', 
     .				zdatetime()
 1030 	   format(2a)
           if (istat .ne. 0) call oerror(n355, m2, clogout)
        endif
      else
	call oerror(n354, m2, clogout)
      endif
      goto 1000
c-------------------------------------------------------------------
c    LOGOFF
c-------------------------------------------------------------------
290   continue
c
      call filecomp(clogout, fullname)
      if (opened(fullname)) then
	if (dolog) write(ilogout, 1030, iostat=istat)
     .		'# Command logging OFF -- ', zdatetime()
	close(unit=abs(ilogout),iostat=istat)
        if (istat .ne. 0) call oerror(n356, m2, clogout)
      endif
      dolog = .false.
      goto 1000
c-------------------------------------------------------------------
c    UNDOON
c-------------------------------------------------------------------
291   continue
      undoon = .true.
      goto 1000
c-------------------------------------------------------------------
c    UNDOOFF
c-------------------------------------------------------------------
292   continue
      undoon = .false.
      goto 1000
c-------------------------------------------------------------------
c    AMATHON
c-------------------------------------------------------------------
293   continue
      if (amath) call oerror(n133, m1, '')
      amath = .true.
      goto 1000
c-------------------------------------------------------------------
c    AMATHOFF
c-------------------------------------------------------------------
294   continue
      amath = .false.
      goto 1000
c-------------------------------------------------------------------
c    Z_SYSTEM
c-------------------------------------------------------------------
310   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'Z_SYSTEM')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'Z_SYSTEM')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'Z_SYSTEM')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      string1 = ' '
      do 311 i = 1, isize
	rstring1(i) = c(loc+i-1)
311	continue
c
      sp = sp - 4
c
      irtn = system(string1(1:lastblnk(string1)))
      if (irtn .ne. 0) call oerror(n9,n0,'Z_SYSTEM')
c
      goto 1000
c
c-------------------------------------------------------------------
c    Z_CHNGFIL
c-------------------------------------------------------------------
320   continue
      if (sp .lt. 9) call oerror(n112, m1, 'Z_CHNGFIL')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'Z_CHNGFIL')
      if (stack(sp-5) .ne. 3) call oerror(n112, m1, 'Z_CHNGFIL')
c
c     First string first
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'Z_CHNGFIL')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      string1 = ' '
      do 321 i = 1, isize
	rstring1(i) = c(loc+i-1)
321	continue
c
c     Now for file type
c
      inum = nint(v(sp-4))
c
c     Now for 2nd string
c
      LOC=STACK(SP-6)
      itype = stack(sp-8)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-7))
      else if (itype .eq. 7) then
	isize = stack(sp-7)
      else 
	call oerror(n112, m1, 'Z_CHNGFIL')
      endif
      string2 = ' '
      do 322 i = 1, isize
	rstring2(i) = c(loc+i-1)
322	continue
c
      sp = sp - 9
c
      string3 = ' '
      i1 = lastblnk(string2)
      call uppercase(string2(1:i1), string3)
c
      if (string3 .eq. 'C') then
            call oerror(n31, m1, 'Z_CHNGFIL:  Ambiguous symbol')
      else if (index("CHANGE", string3(1:i1)) .eq. 1) then
	    string3 = "CHANGE"
      else if (index("SUBTRACT", string3(1:i1)) .eq. 1) then		
	    string3 = "SUBTRACT"
      else if (index("CREATE", string3(1:i1)) .eq. 1) then		
	    string3 = "CREATE"
      else 
            call oerror(n31, m1, 'Z_CHNGFIL')
      endif
c
      irtn = chngfile(string3(1:lastblnk(string3)), inum, 
     .		string1(1:lastblnk(string1)))
      if (irtn .ne. 0) call oerror(n31, m1, 'Z_CHNGFIL')
c
      goto 1000
c
c-------------------------------------------------------------------
c    Z_BATCH
c-------------------------------------------------------------------
330   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'Z_BATCH')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'Z_BATCH')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'Z_BATCH')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      string1 = ' '
      do 331 i = 1, isize
	rstring1(i) = c(loc+i-1)
331	continue
c
      sp = sp - 4
c
      call batch(string1(1:lastblnk(string1)))
c
      goto 1000
c
c-------------------------------------------------------------------
c    COPYON
c-------------------------------------------------------------------
340   continue
      iprinttype = abs(iprinttype)
      goto 1000
c-------------------------------------------------------------------
c    COPYOFF
c-------------------------------------------------------------------
350   continue
      iprinttype = -abs(iprinttype)
      goto 1000
c-------------------------------------------------------------------
c    FREWIND
c-------------------------------------------------------------------
352   continue
c
      iunit = nint(v(sp))
      sp = sp - 1
      if (iunit .le. 0) call oerror(n370, m1, 'FREWIND')
c
      do 3521 i = 1, nfiles
	if (iunit .eq. ffile(i,1) ) goto 3522
3521	continue
      call oerror(n370, m1, 'FREWIND')
c
3522  rewind(iunit,iostat=ier)
      if (ier .ne. 0) call oerror(n372, m1, 'FREWIND')
c
      goto 1000
c-------------------------------------------------------------------
c    FCLOSE
c-------------------------------------------------------------------
353   continue
c
      iunit = nint(v(sp))
      sp = sp - 1
      if (iunit .le. 0) call oerror(n370, m1, 'FCLOSE')
c
      do 3531 i = 1, nfiles
	if (iunit .eq. ffile(i,1) ) goto 3532
3531	continue
      call oerror(n370, m1, 'FCLOSE')
c
3532  close(iunit,iostat=ier)
      if (ier .ne. 0) call oerror(n372, m1, 'FCLOSE')
      call ioput(iunit)
      ffile(i,1) = 0
      cfile(i) = ' '
c
      goto 1000
c-------------------------------------------------------------------
c    FSTATUS
c-------------------------------------------------------------------
354   continue
c
      okfile = .false.
      do 3543 i = 1, nfiles
	if (ffile(i,1) .gt. 0) then
	    okfile = .true.
	    write(stch,fmt="(a,a)",iostat=ier) 'File name = ',
     .		cfile(i)(1:max(1,lastblnk(cfile(i))))
	    call pwrite(stch, n72)
	    write(stch,fmt="(a,i3,a,a)",iostat=ier) 'Unit = ', 
     .		ffile(i,1), '     Access = ', access(ffile(i,2)) 
	    call pwrite(stch, n72)
	    write(stch,fmt="(4a)",iostat=ier) 'Init. Pos. = ', 
     .			position(ffile(i,3)), 
     .			' Existence = ', existence(ffile(i,4))
	    call pwrite(stch, n72)
	    write(stch,fmt="(a)",iostat=ier) ' '
	    call pwrite(stch, n72)
	endif
3543	continue
c
      if (.not. okfile) then
	write(stch,fmt="(a)") 'No files opened'
	call pwrite(stch, n72)
      endif
c
      goto 1000
c-------------------------------------------------------------------
c    PRNTFMT
c-------------------------------------------------------------------
355   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'PRNTFMT')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'PRNTFMT')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'PRNTFMT')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      string1 = ' '
      do 3551 i = 1, isize
	rstring1(i) = c(loc+i-1)
3551	continue
c
      sp = sp - 4
c
      i = min(14,lastblnk(string1))
      if (i .ne. 0) then
	if (.not. fptfmt(string1(1:i)) ) call oerror(n373, m1, 'PRNTFMT')
	string1 = '(' // string1(1:i) // ')'
        testvalue = 1.0
        write(string2, string1, iostat=ier) testvalue
        if (ier .ne. 0 .or. index(string2,'*') .ne. 0) 
     .		call oerror(n373, m1, 'PRNTFMT')
        read(string2, '(f60.0)', iostat=ier) testvalue
        if (ier .ne. 0 .or. testvalue .ne. 1.0) 
     .		call oerror(n373, m1, 'PRNTFMT')
        prntfmt = string1
      else
	prntfmt = '(1PG15.7.2)'
      endif
c
      goto 1000
c-------------------------------------------------------------------------
c     FONTSET
c-------------------------------------------------------------------------
356   continue
c
      if (sp .lt. 2) call oerror(n112, m1, 'FONTSET : One needed')
c
      ispace = nint(v(sp))
      ifont = nint(v(sp-1))
      sp = sp - 2
c
      if (ispace .gt. 0) then
	call fset(ifont, .true.)
      else
	call fset(ifont, .false.)
      endif
c
      goto 1000
c
c-------------------------------------------------------------------------
c     VCHAR
c-------------------------------------------------------------------------
357   continue
c
      if (sp .lt. 5) call oerror(n112, m1, 'VCHAR')
c
      ispace = nint(v(sp))
      sp = sp - 1
c     Get the spacing designation.
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'VCHAR')
c     Make sure we have a array, string, or literal on the stack
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	jcnt = 4*irealp(stack(sp-2))
      else if (itype .eq. 7) then
	jcnt = 4*stack(sp-2)
      else 
	call oerror(n112, m1, 'VCHAR')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      SP=SP-4
c
      if (sclchar .ne. 1.) call charsize(sclchar)
      if (ispace .gt. 0) then
	CALL VCHAR (c(LOC),jcnt,.true.)
      else
	CALL VCHAR (c(LOC),jcnt,.false.)
      endif
      if (sclchar .ne. 1.) call charsize(1.)
c
      GO TO 1000
c
C---------------------------------------------------------------------
C					D E B U	G  <true/false>.
C---------------------------------------------------------------------
 351  if (sp .lt. 1) call oerror(n112, m1, 'DEBUG')
      if (v(sp) .gt. 0) then
	IDEBUG = 1
      else
	idebug = -1
      endif
      sp = sp - 1
      GO TO 1000
c-------------------------------------------------------------------
c    SETASTACK
c-------------------------------------------------------------------
295   continue
      if (sp .ge. 2) then
	 value = v(sp)
  	 i1 = nint(v(sp-1))
	 sp = sp - 2
         if (i1 .le. 0 .or. i1 .gt. maxstck) then
	    call oerror(n112, m1, 'ASTACK')
c	    Location of  word must be within the correct range
c
         else
            astack(i1) = value
         endif
      else
         call oerror(n112, m1, 'ASTACK: One needed')
      endif
      goto 1000
c-------------------------------------------------------------------
c    SETMD0
c-------------------------------------------------------------------
410   continue
      iptwh2 = 1
      goto 419
c-------------------------------------------------------------------
c    SETMD1
c-------------------------------------------------------------------
411   continue
      iptwh2 = 2
      goto 419
c-------------------------------------------------------------------
c    SETMD2
c-------------------------------------------------------------------
412   continue
      iptwh2 = 3
      goto 419
c-------------------------------------------------------------------
c    SETMD3
c-------------------------------------------------------------------
413   continue
      iptwh2 = 4
c
419   if (sp .ge. 3) then
	 value = v(sp)
  	 i2 = nint(v(sp-1))
  	 i1 = nint(v(sp-2))
         sp = sp - 3
         if (i1 .le. 0 .or. i1 .gt. mhead(mnaxis1,iptwh2)) then
	    call oerror(n245, m2, 'MD0/D9')
         else if (i2 .le. 0 .or. i2 .gt. mhead(mnaxis2,iptwh2)) then
	    call oerror(n245, m2, 'MD0/D9')
	 else
            mdata(ilct(i1,i2,iptwh2)) = value
         endif
      else
         call oerror(n112, m1, 'MD0/D9: Three needed')
      endif
      goto 1000
c
c-------------------------------------------------------------------
c    SETMH0
c-------------------------------------------------------------------
420   continue
      iptwh2 = 1
      goto 429
c-------------------------------------------------------------------
c    SETMH1
c-------------------------------------------------------------------
421   continue
      iptwh2 = 2
      goto 429
c-------------------------------------------------------------------
c    SETMH2
c-------------------------------------------------------------------
422   continue
      iptwh2 = 3
      goto 429
c-------------------------------------------------------------------
c    SETMH3
c-------------------------------------------------------------------
423   continue
      iptwh2 = 4
c
429   if (sp .ge. 5 .and. stack(sp) .eq. 3) then
c
c	String header parametr
c
	itag = stack(sp-1)
	itype = stack(sp-3)
	if  (itype .eq. 14) then
	  isize = irealp(stack(sp-2))
	else if (itype .eq. 7) then
	  isize = stack(sp-2)
 	else 
	  call oerror(n112, m1, 'MH0/H3: Bad arguments')
	endif
c	Get the correct isize which depends if its a literal or variable.
c	Check to make sure that it is a literal or string variables (types
c	7 and 14).
c
  	i1 = nint(v(sp-4))
	sp = sp - 5
c       i1 = the location in DTWH in which the user wants to change the
c       header word
c
	if (i1 .le. 0 .or. i1 .gt. mheadsize) call oerror(n245, m2, 
     .		'MH0/H3')
c
        if (i1 .lt. mstrings) 
     .		call oerror(n245, m2, 'MH0/MH3: String header word')
c	Make sure we are trying to set a string header parameter.
c
	if (i1 .eq. mcomment) then
	  inum4 = 2*min(7,(mheadsize-mcomment+1))
        else if (i1 .eq. mobject) then
          inum4 = 4
	else
	  inum4 = 2
	endif
c	INUM4 = the maximum number of R*2 words which can be stored in 
c	header words
c
        if (i1 .ne. mdate) then
           if (isize .gt. inum4) call oerror(n114, m1, 
     .		'MH0/H3: Too many characters')
c     Header words can only hold inum4 R*4 words
c
           i1 = (i1-1)*4 + 1
           call copy(iwpr(isize), c(itag), imhead(i1, iptwh2) )
           if (inum4 .gt. isize) call fill(iwpr(fshort(inum4-isize)),
     1          blank, imhead(i1+iwpr(isize),iptwh2) )
c     Copy over the header words; fill the rest of the header word
c     with blanks
        else
c                  The DATE header word is handled as a string
c                  but actually stored as a double
           nchars = isize*4
           call todate(isize, c(itag), ddate, l0)
c                  watch for problems, which are signaled by ddate < 0
           if (ddate .lt. 0.0) call oerror(n301, m1, 'MH0/H3')
           mhead(i1, iptwh2) = ddate
        endif
c
      else if (sp .ge. 2) then
c
c	Real value header parametr
c
	value = v(sp)
  	i1 = nint(v(sp-1))
        sp = sp - 2
c       i1 = the location in DTWH in which the user wants to change the
c       header word
c
	if (i1 .le. 0 .or. i1 .gt. mheadsize) call oerror(n245, m1, 
     .		'MH0/H3')
c
        if (i1 .ge. mstrings .and. i1 .le. mheadsize) 
     .	     call oerror(n245, m1, 'MH0/H3: Real-Value header word')
c	Make sure we are trying to set a real-value header parameter.
c
        mhead(i1,iptwh2) = value
c
      else
c
         call oerror(n112, m1, 'MH0/H3: Two needed')
c
      endif
      goto 1000
c
C----------------------------------------------------------------------
C                   NOP   
C----------------------------------------------------------------------
1000  LFLAG=2
      RETURN
      END
c
