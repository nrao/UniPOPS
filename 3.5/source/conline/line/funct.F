      SUBROUTINE  FUNCT	(LFLAG,J)
C-------------------------------------------------------------------------------
C  @(#)funct.F	5.2 05/04/98
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C FUNCT contains frequently used fortran builtin functions
C---------------------------------------------------------------------
c
      integer*2 jj, lflag, j, iptwh2, icnum, istop, i1, loc, isize,
     .		isize2, iwpr, irealp, itype, numi2, numi4,
     .		i, lastblnk, i2, i3, itag, isize1, ierr, ier,  
     .		iwpc, openmode, rwmode, exmode, posmode, SCR, NEW, OLD,
     .		UNK, EOF, REW, RO, WO, RW, iused
      integer*2 m1, m2, m3, n1, n30, n112, n120, n272, n225, n245, 
     .		n28, n352, n370, n299
      real*4 cube, rcompar2(30), rcompar1(30), rchan
      integer*4 ilct, index, ioget, iunit, slen, sused
      logical string
      character*60 compar1, compar2
      character*1023 fullname
      real*8 nrao140, nrao300, proto12m, x1, x2, dinfinity
      character*8 stat
      double precision chantox, xtochan
c
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
      include 'stk.inc'
      include 'core.inc'
      include 'cio.inc'
      INCLUDE 'mform.inc'
      INCLUDE 'mappl.inc'
c
      equivalence (compar1, rcompar1), (compar2, rcompar2)
c
      data m1, m2, m3, n1, n28, n30, n112, n120, n272, n225, n245, n352
     .     /-1, -2, -3, 1, 28, 30, 112, 120, 272, 225, 245, 352/
      data n370, n299 /370, 299/
      data nrao140/'NRAO 43M'/, nrao300/'NRAO 93M'/
      data proto12m /'PROTO12M'/
c
      parameter (UNK = 0)
      parameter (OLD = 1)
      parameter (NEW = 2)
      parameter (SCR = 3)
      parameter (REW = 0)
      parameter (EOF = 1)
      parameter (RW = 0)
      parameter (WO = 1)
      parameter (RO = 2)
c
C=======================================================================
C
      JJ=J
      GO TO (36,58,37,59,38,39,40,41,42,43,44,45,46,47,48,49,60,
     1       61,62,63,64,65,135,111,112,113,114,115,116,117,118,
     .	     119,120,121,122,123,124,125,126,127,128,129,130,200,
     .       201,202,203,210,211,212,213,220,230,140,150,160,240,
     .	     250,260,170,180,190,270, 280, 290, 300, 310), JJ
      call oerror(n120, m3, 'FUNCT')
c
c----------------------------------------
C		   SIN,	COS, TAN, ATAN,	ATAN2	  
c----------------------------------------
  36  CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'SIN : One needed')
      XX=SIN(V(SP))
      GO TO 5001
   37 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'COS : One needed')
      XX=COS(V(SP))
      GO TO 5001
   38 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'TAN : One needed')
      XX=TAN(V(SP))
      GO TO 5001
   39 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'ATAN : One needed')
      XX=ATAN(V(SP))
      GO TO 5001
   40 CONTINUE
      if (sp.lt.2) call oerror(n112, m1, 'ATAN2 : Two needed')
      X1=V(SP)
      X2=V(SP-1)
      XX=ATAN2(X2,X1)
      GO TO 5000
c----------------------------------------
C                      SQRT, LN, LOG10, EXP
c----------------------------------------
   41 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'SQRT : One needed')
      XX=SQRT(V(SP))
      GO TO 5001
   42 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'LOG : One needed')
      XX=LOG(V(SP))
      GO TO 5001
   43 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'LOG10 : One needed')
      XX=LOG10(V(SP))
      GO TO 5001
   44 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'EXP : One needed')
      XX=EXP(V(SP))
      GO TO 5001
c----------------------------------------
C                                 ABS, IFIX, MAX, MIN, MOD
c----------------------------------------
   45 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'ABS : One needed')
      XX=ABS(V(SP))
      GO TO 5001
   46 CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'IFIX : One needed')
      XX=FLOAT(IFIX(V(SP)))
      GO TO 5001
   47 CONTINUE
      if (sp.lt.2) call oerror(n112, m1, 'MAX : Two needed')
      x1 = v(sp)
      x2 = v(sp-1)
      xx = max(x1,x2)
      goto 5000
   48 CONTINUE
      if (sp.lt.2) call oerror(n112, m1, 'MIN : Two needed')
      x1 = v(sp)
      x2 = v(sp-1)
      xx = min(x1,x2)
      goto 5000
   49 CONTINUE
      if (sp.lt.2) call oerror(n112, m1, 'MOD : Two needed')
      x1 = v(sp)
      x2 = v(sp-1)
      xx = mod(x2,x1)
      goto 5000
c----------------------------------------
c                                   asin, acos
c----------------------------------------
 58   continue
      if (sp.lt.1) call oerror(n112, m1, 'ASIN : One needed')
      xx = asin(v(sp))
      goto 5001
 59   continue
      if (sp.lt.1) call oerror(n112, m1, 'ACOS : One needed')
      xx = acos(v(sp))
      goto 5001
c----------------------------------------
c				anint, sign, dim
c----------------------------------------
60    continue
      if (sp.lt.1) call oerror(n112, m1, 'NINT : One needed')
      xx = anint(v(sp))
      goto 5001
61    continue
      if (sp.lt.2) call oerror(n112, m1, 'SIGN : Two needed')
      x1 = v(sp)
      x2 = v(sp-1)
      xx = sign(x2,x1)
      goto 5000
62    continue
      if (sp.lt.2) call oerror(n112, m1, 'DIM : Two needed')
      x1 = v(sp)
      x2 = v(sp-1)
      xx = dim(x2,x1)
      goto 5000
c----------------------------------------
c				sinh, cosh, tanh
c----------------------------------------
63    continue
      if (sp.lt.1) call oerror(n112, m1, 'SINH : One needed')
      xx = sinh(v(sp))
      goto 5001
64    continue
      if (sp.lt.1) call oerror(n112, m1, 'COSH : One needed')
      xx = cosh(v(sp))
      goto 5001
65    continue
      if (sp.lt.1) call oerror(n112, m1, 'TANH : One needed')
      xx = tanh(v(sp))
      goto 5001
c----------------------------------
c     D0
c----------------------------------
  111 iptwh2 = 1
      goto 1119
c----------------------------------
c     D1
c----------------------------------
  112 iptwh2 = 2
      goto 1119
c----------------------------------
c     D2
c----------------------------------
  113 iptwh2 = 3
      goto 1119
c----------------------------------
c     D3
c----------------------------------
  114 iptwh2 = 4
      goto 1119
c----------------------------------
c     D4
c----------------------------------
  115 iptwh2 = 5
      goto 1119
c----------------------------------
c     D5
c----------------------------------
  116 iptwh2 = 6
      goto 1119
c----------------------------------
c     D6
c----------------------------------
  117 iptwh2 = 7
      goto 1119
c----------------------------------
c     D7
c----------------------------------
  118 iptwh2 = 8
      goto 1119
c----------------------------------
c     D8
c----------------------------------
  119 iptwh2 = 9
      goto 1119
c----------------------------------
c     D9
c----------------------------------
  120 iptwh2 = 10
 1119 if (sp .ge. 1) then
         icnum = nint(v(sp))
         istop = nint(dtwh(c12spn,iptwh2) + dtwh(c12ni,iptwh2)) - 1
         if (icnum .gt. istop .or. icnum .le. 0) then
	    call oerror(n245, m2, 'D0/D9')
	 else
            xx = twh(idatoff+icnum,iptwh2)
	 endif
      else
         call oerror(n112, m1, 'D0/D9: One needed')
      endif
      goto 5001
c----------------------------------
c     H0
c----------------------------------
  121 iptwh2 = 1
      goto 1149
c----------------------------------
c     H1
c----------------------------------
  122 iptwh2 = 2
      goto 1149
c----------------------------------
c     H2
c----------------------------------
  123 iptwh2 = 3
      goto 1149
c----------------------------------
c     H3
c----------------------------------
  124 iptwh2 = 4
      goto 1149
c----------------------------------
c     H4
c----------------------------------
  125 iptwh2 = 5
      goto 1149
c----------------------------------
c     H5
c----------------------------------
  126 iptwh2 = 6
      goto 1149
c----------------------------------
c     H6
c----------------------------------
  127 iptwh2 = 7
      goto 1149
c----------------------------------
c     H7
c----------------------------------
  128 iptwh2 = 8
      goto 1149
c----------------------------------
c     H8
c----------------------------------
  129 iptwh2 = 9
      goto 1149
c----------------------------------
c     H9
c----------------------------------
  130 iptwh2 = 10
c
 1149 if (sp .ge. 1) then
c
  	 i1 = nint(v(sp))
c
   	 string = .false.
	 do 1150 i = 1, numastrings
	    if (i1 .eq. astrings(i)) string = .true.
1150	    continue
c	 Find out if it is a string header word
c	 Deal with class 9 and class 11 special cases
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
         if (string) then
c
c	    Strings
c
            sp = sp - 1
	    if (i1 .eq. c1ona .or. i1 .eq. c1sna .or.
     .          i1 .eq. c12rxi) then
		numi2 = 8
	    else if (i1 .eq. c9cff) then
		numi2 = 12
	    else
		numi2 = 4
                do 1242 i = 1, 10
                   if (i1 .eq. c11ppt(i)) numi2 = 16
 1242           continue
	    endif
	    call runtag(numi2, itag)
	    call copy(numi2, dtwh(i1,iptwh2), c(itag))
c	    Store contents of header word into K
c
c	    Update stack pointers so that proper K location will be
c	    on the stack
	    sp = sp + 4
	    stack(sp-3) = 14
	    stack(sp-2) = numi2
	    stack(sp-1) = itag
	    stack(sp) = 3
	    v(sp-3) = c(7)
	    v(sp-2) = c(4)
	    v(sp-1) = c(itag)
	    v(sp) = c(2)
	    goto 1000
c
	 else if (i1 .gt. 0 .and. i1 .lt. idatoff/2) then
c
c	    Reals
c
            xx = dtwh(i1,iptwh2)
            goto 5001
c
	 else
	    call oerror(n245, m2, 'H0/H9')
	 endif
      else
         call oerror(n112, m1, 'H0/H9: One needed')
      endif
c
c----------------------------------------
c                                   astack
c----------------------------------------
135   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'ASTACK')
c
      i1 = nint(v(sp))
      if (i1 .lt. 1 .or. i1 .gt. maxstck) 
     .		call oerror(n112, m2, 'ASTACK')
c
      xx = astack(i1)
c
      goto 5001
c
c----------------------------------------
c                                   compar
c----------------------------------------
140   continue
c
      if (sp .lt. 8) 
     .		call oerror(n112, m1, 'COMPARE : Two strings needed')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'COMPARE')
      if (stack(sp-4) .ne. 3) call oerror(n112, m1, 'COMPARE')
c
c     First string first
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize1 = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize1 = stack(sp-2)
      else 
	call oerror(n112, m1, 'COMPARE')
      endif
c     Get the correct size which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      compar1 = ' '
      do 141 i = 1, isize1
	rcompar1(i) = c(loc+i-1)
141	continue
c
c     Now for 2nd string
c
      LOC=STACK(SP-5)
      itype = stack(sp-7)
      if  (itype .eq. 14) then
	isize2 = irealp(stack(sp-6))
      else if (itype .eq. 7) then
	isize2 = stack(sp-6)
      else 
	call oerror(n112, m1, 'COMPARE')
      endif
      compar2 = ' '
      do 142 i = 1, isize2
	rcompar2(i) = c(loc+i-1)
142	continue
c
      sp = sp - 7
      if (compar1 .eq. compar2) then
	xx = 1.0
      else
	xx = -1.0
      endif
      goto 5001
c
c----------------------------------------
c                                   lnblnk
c----------------------------------------
150   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'LNBLNK')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'LNBLNK')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'LNBLNK')
      endif
c     Get the correct size which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      compar1 = ' '
      do 151 i = 1, isize
	rcompar1(i) = c(loc+i-1)
151	continue
c
      sp = sp - 3
      xx = lastblnk(compar1)
c
      goto 5001
c
c----------------------------------------
c                                   length
c----------------------------------------
160   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'LENGTH')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'LENGTH')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	xx = 2*stack(sp-2)
      else if (itype .eq. 7) then
	xx = 4*stack(sp-2)
      else 
	call oerror(n112, m1, 'LENGTH')
      endif
c     Get the correct size which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      sp = sp - 3
      goto 5001
c
C-----------------------------------------------------------------------
C					CHANTOX   
C-----------------------------------------------------------------------
170   CONTINUE
c
      if (dtwh(c12ni,1) .eq. 0) call oerror(n225, m2, 'CHANTOX')
c
      rchan=V(SP)
c
      xx = chantox(lxa, dtwh(1,1), rchan, ierr)
      if (ierr .ne. 0) call oerror(n272, m2, 'CHANTOX')

      goto 5001
c
C-----------------------------------------------------------------------
C					XTOCHAN   
C-----------------------------------------------------------------------
180   CONTINUE
c
      if (dtwh(c12ni,1) .eq. 0) call oerror(n272, m2, 'CHANTOX')
c
      rchan=V(SP)
c
      xx = xtochan(lxa, dtwh(1,1), rchan, ierr)
      if (ierr .ne. 0) call oerror(n272, m2, 'XTOCHAN')

      goto 5001
c
C-----------------------------------------------------------------------
C					FOPEN   
C-----------------------------------------------------------------------
190   CONTINUE
c
      if (sp .lt. 5) call oerror(n112, m1, 
     .				'FOPEN: Two needed')
c
      if (stack(sp-1) .ne. 3) call oerror(n112, m1, 'FOPEN')
c
c     mode first
      if (v(sp) .eq. dinfinity() ) then
	openmode = 0
      else
        openmode = nint(v(sp))
      endif
      if (openmode .lt. 0) 
     .	   call oerror(n112, m1, 'FOPEN: Bad open parameter')
c
c     Filename next
      LOC=STACK(SP-2)
      itype = stack(sp-4)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-3))
      else if (itype .eq. 7) then
	isize = stack(sp-3)
      else 
	call oerror(n112, m1, 'FOPEN')
      endif
c     Get the correct size which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      compar1 = ' '
      do 191 i = 1, isize
	rcompar1(i) = c(loc+i-1)
191	continue
c
      sp = sp - 4
c
      do 192 i = 1, nfiles
	if (ffile(i,1) .eq. 0) goto 193
192	continue
      call oerror(n28, m1, 'FOPEN')
c
193   rwmode = mod(openmode,10)
      posmode = mod(openmode/10, 10)
      exmode = mod(openmode/100, 10)
      if (rwmode .gt. 2 .or. posmode .gt. 1 .or. exmode .gt. 3)
     .		call oerror(n112, m1, 'FOPEN: Bad open parameter')
c
      if (exmode .eq. SCR) then
	if (lastblnk(compar1) .ne. 0) call oerror(n352, m1, 
     .		'FOPEN: Scratch file cannot have a name')
	if (rwmode .eq. RO) call oerror(n352, m1, 
     .		'FOPEN: Scratch file cannot be opened for reading')
        iunit = ioget()
        if (iunit .lt. 0) call oerror(n28, m1, 'FOPEN')
	open(unit=iunit, access='sequential', form='print', iostat=ier,
     .		status='scratch')
c
      else if (exmode .eq. NEW) then
	if (lastblnk(compar1) .eq. 0) call oerror(n352, m1, 
     .		'FOPEN: New file must have a name')
	if (rwmode .eq. RO) call oerror(n352, m1, 
     .		'FOPEN: New file cannot be opened for reading')
        iunit = ioget()
        if (iunit .lt. 0) call oerror(n28, m1, 'FOPEN')
        call filecomp(compar1, fullname)
	open(unit=iunit, access='sequential', form='print', iostat=ier,
     .		status='new', file = fullname)
c
      else if (exmode .eq. OLD .or. exmode .eq. UNK) then
	if (lastblnk(compar1) .eq. 0) call oerror(n352, m1, 
     .		'FOPEN: Must supply a file name')
	if (exmode .eq. OLD) then
	   stat = 'old'
	else
	   stat = 'unknown'
	endif
        iunit = ioget()
        if (iunit .lt. 0) call oerror(n28, m1, 'FOPEN')
        call filecomp(compar1, fullname)
	if (posmode .eq. EOF) then
	   open(unit=iunit, access='sequential', form='print', iostat=ier,
     .		status= stat, file = fullname, fileopt='eof')
	else
	   open(unit=iunit, access='sequential', form='print', iostat=ier,
     .		status= stat, file = fullname)
	endif
c
      else
        call oerror(n112, m1, 'FOPEN: Bad open parameter')
      endif
c
      if (ier .ne. 0) then
	call ioput(iunit)
	call oerror(n352, m1, 'FOPEN')
      endif
c
      ffile(i,1) = iunit
      ffile(i,2) = rwmode
      ffile(i,3) = posmode
      ffile(i,4) = exmode
      cfile(i) = compar1
      xx = iunit
c
      goto 5001
c
c----------------------------------
c     MD0
c----------------------------------
  200 iptwh2 = 1
      goto 209
c----------------------------------
c     MD1
c----------------------------------
  201 iptwh2 = 2
      goto 209
c----------------------------------
c     MD2
c----------------------------------
  202 iptwh2 = 3
      goto 209
c----------------------------------
c     MD3
c----------------------------------
  203 iptwh2 = 4
c
 209  if (sp .ge. 2) then
  	 i2 = nint(v(sp))
  	 i1 = nint(v(sp-1))
         sp = sp - 1
         if (i1 .le. 0 .or. i1 .gt. mhead(mnaxis1,iptwh2)) then
	    call oerror(n245, m2, 'MD0/D9')
         else if (i2 .le. 0 .or. i2 .gt. mhead(mnaxis2,iptwh2)) then
	    call oerror(n245, m2, 'MD0/D9')
	 else
            xx = mdata(ilct(i1,i2,iptwh2))
         endif
      else
         call oerror(n112, m1, 'MD0/D9: Two needed')
      endif
      goto 5001
c
c----------------------------------
c     MH0
c----------------------------------
  210 iptwh2 = 1
      goto 219
c----------------------------------
c     MH1
c----------------------------------
  211 iptwh2 = 2
      goto 219
c----------------------------------
c     MH2
c----------------------------------
  212 iptwh2 = 3
      goto 219
c----------------------------------
c     MH3
c----------------------------------
  213 iptwh2 = 4
c
219   if (sp .ge. 1) then
  	 i1 = nint(v(sp))
c
         if (i1 .ge. mstrings .and. i1 .le. mheadsize) then
c
c	    Strings
c
            sp = sp - 1
	    if (i1 .eq. mcomment) then
		numi2 = 4*min(7,(mheadsize-mcomment+1))
            else if (i1 .eq. mobject) then
                numi2 = 8
	    else
		numi2 = 4
	    endif
	    call runtag(numi2, itag)
c                    date is stored as a double but it behaves as a string
            if (i1 .ne. mdate) then
               call copy(numi2, mhead(i1,iptwh2), c(itag))
            else
               slen = 24
               call fromdate(slen, sused, mhead(i1, iptwh2), c(itag))
c
               iused = sused
               numi2 = iwpc(iused)
            endif
c           Store contents of header word into K
c
c	    Update stack pointers so that proper K location will be
c	    on the stack
	    sp = sp + 4
	    stack(sp-3) = 14
	    stack(sp-2) = numi2
	    stack(sp-1) = itag
	    stack(sp) = 3
	    v(sp-3) = c(7)
	    v(sp-2) = c(4)
	    v(sp-1) = c(itag)
	    v(sp) = c(2)
	    goto 1000
c
	 else if (i1 .gt. 0 .and. i1. lt. mstrings) then
c
c	    Reals
c
            xx = mhead(i1, iptwh2)
            goto 5001
c
	 else
	    call oerror(n245, m2, 'MH0/MH9')
	 endif
      else
         call oerror(n112, m1, 'MH0/MH9: One needed')
      endif
c
c----------------------------------
c     CD0
c----------------------------------
 220  continue
c
      if (sp .ge. 3) then
  	 i3 = nint(v(sp))
  	 i2 = nint(v(sp-1))
  	 i1 = nint(v(sp-2))
         sp = sp - 2
         if (i1 .le. 0 .or. i1 .gt. chead(cnaxis1)) then
	    call oerror(n245, m2, 'CD0')
         else if (i2 .le. 0 .or. i2 .gt. chead(cnaxis2)) then
	    call oerror(n245, m2, 'CD0')
         else if (i3 .le. 0 .or. i3 .gt. chead(cnaxis3)) then
	    call oerror(n245, m2, 'CD0')
	 else
            xx = cube(i1,i2,i3)
         endif
      else
         call oerror(n112, m1, 'CD0: Three needed')
      endif
      goto 5001
c
c----------------------------------
c     CH0
c----------------------------------
 230  continue
c
      if (sp .ge. 1) then
  	 i1 = nint(v(sp))
c
         if (i1 .ge. cstrings .and. i1 .le. cheadsize) then
c
c	    Strings
c
            sp = sp - 1
	    if (i1 .eq. ccomment) then
		numi2 = 4*min(7,(cheadsize-ccomment+1))
            else if (i1 .eq. cobject) then
                numi2 = 8
	    else
		numi2 = 4
	    endif
	    call runtag(numi2, itag)
c                    date is stored as a double but it behaves as a string
            if (i1 .ne. mdate) then
               call copy(numi2, chead(i1), c(itag))
            else
               call fromdate(slen, sused, chead(i1), c(itag))
c
               iused = sused
               numi2 = iwpc(iused)
            endif
c	    Store contents of header word into K
c
c	    Update stack pointers so that proper K location will be
c	    on the stack
	    sp = sp + 4
	    stack(sp-3) = 14
	    stack(sp-2) = numi2
	    stack(sp-1) = itag
	    stack(sp) = 3
	    v(sp-3) = c(7)
	    v(sp-2) = c(4)
	    v(sp-1) = c(itag)
	    v(sp) = c(2)
	    goto 1000
c
	 else if (i1 .gt. 0 .and. i1 .lt. cstrings) then
c
c	    Reals
c
            xx = chead(i1)
            goto 5001
c
	 else
	    call oerror(n245, m2, 'CH0')
	 endif
      else
         call oerror(n112, m1, 'CH0: One needed')
      endif
c
c----------------------------------------
c                                   concat
c----------------------------------------
240   continue
c
      if (sp .lt. 8) call oerror(n112, m1, 
     .				'CONCAT: Two strings needed')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'CONCAT')
      if (stack(sp-4) .ne. 3) call oerror(n112, m1, 'CONCAT')
c
c     First string first
c
      LOC=STACK(SP-5)
      itype = stack(sp-7)
      if  (itype .eq. 14) then
	isize1 = irealp(stack(sp-6))
      else if (itype .eq. 7) then
	isize1 = stack(sp-6)
      else 
	call oerror(n112, m1, 'CONCAT')
      endif
c     Get the correct size which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      compar1 = ' '
      do 241 i = 1, isize1
	rcompar1(i) = c(loc+i-1)
241	continue
c
c     Now for 2nd string
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize2 = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize2 = stack(sp-2)
      else 
	call oerror(n112, m1, 'CONCAT')
      endif
c
      numi4 = min(15, isize1 + isize2)
      do 242 i = isize1+1, numi4
	rcompar1(i) = c(loc+i-isize1-1)
242	continue
c
      numi2 = iwpr(numi4)
      call runtag(numi2, itag)
      call copy(numi2, rcompar1(1), c(itag))
c     Store contents of header word into K
c
c     Update stack pointers so that proper K location will be
c     on the stack
      sp = sp - 4
      stack(sp-3) = 14
      stack(sp-2) = numi2
      stack(sp-1) = itag
      stack(sp) = 3
      v(sp-3) = c(7)
      v(sp-2) = c(4)
      v(sp-1) = c(itag)
      v(sp) = c(2)
c
      goto 1000
c
c----------------------------------------
c                                   substr
c----------------------------------------
250   continue
c
      if (sp .lt. 6) call oerror(n112, m1, 
     .				'SUBSTR: Three needed')
c
      if (stack(sp-2) .ne. 3) call oerror(n112, m1, 'SUBSTR')
c
c     String indices first
      i2 = nint(v(sp))
      i1 = nint(v(sp-1))
c
c     String next
      LOC=STACK(SP-3)
      itype = stack(sp-5)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-4))
      else if (itype .eq. 7) then
	isize = stack(sp-4)
      else 
	call oerror(n112, m1, 'SUBSTR')
      endif
c     Get the correct size which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      compar1 = ' '
      do 251 i = 1, isize
	rcompar1(i) = c(loc+i-1)
251	continue
c
      if (i1 .le. 0 .or. i1 .gt. i2) call oerror(n112, m2, 'SUBSTR')
      if (i2 .gt. 4*isize) call oerror(n112, m2, 'SUBSTR')
c 
      compar2 = compar1(i1:i2)
      numi2 = min(n30, iwpc(i2-i1+n1))
c
      call runtag(numi2, itag)
      call copy(numi2, rcompar2(1), c(itag))
c     Store contents of header word into K
c
c     Update stack pointers so that proper K location will be
c     on the stack
      sp = sp - 2
      stack(sp-3) = 14
      stack(sp-2) = numi2
      stack(sp-1) = itag
      stack(sp) = 3
      v(sp-3) = c(7)
      v(sp-2) = c(4)
      v(sp-1) = c(itag)
      v(sp) = c(2)
c
      goto 1000
c
c----------------------------------------
c                                   index
c----------------------------------------
260   continue
c
      if (sp .lt. 8) call oerror(n112, m1, 'INDEX: Two strings needed')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'INDEX')
      if (stack(sp-4) .ne. 3) call oerror(n112, m1, 'INDEX')
c
c     First string first
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize1 = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize1 = stack(sp-2)
      else 
	call oerror(n112, m1, 'INDEX')
      endif
c     Get the correct size which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      compar1 = ' '
      do 261 i = 1, isize1
	rcompar1(i) = c(loc+i-1)
261	continue
c
c     Now for 2nd string
c
      LOC=STACK(SP-5)
      itype = stack(sp-7)
      if  (itype .eq. 14) then
	isize2 = irealp(stack(sp-6))
      else if (itype .eq. 7) then
	isize2 = stack(sp-6)
      else 
	call oerror(n112, m1, 'INDEX')
      endif
      compar2 = ' '
      do 262 i = 1, isize2
	rcompar2(i) = c(loc+i-1)
262	continue
c
      sp = sp - 7
c
      xx = index(compar2, compar1(1:max(1,lastblnk(compar1))))
c
      goto 5001
c---------------------------------------------------------------------
c              FNAME
c---------------------------------------------------------------------
270   continue
      if (sp .ge. 1) then
         i1 = nint(v(sp))
         sp = sp - 1
c			value is file type (<= 12) or unit # >= 50
         compar1 = ' '
         if (i1 .gt. 0 .and. i1 .le. 12) then
            if (i1 .le. 10) then
               if (iounit(i1) .gt. 0) compar1 = ciounit(i1)
            else if (i1 .eq. 11) then
               if (iprtout .gt. 0) compar1 = cprtout
            else
               if (ilogout .gt. 0) compar1 = clogout
            endif
         else if (i1 .ge. 50) then
c			see if this unit number is open
            do 271 i2 = 1, 10
               if (ffile(i2,1) .eq. i1) compar1 = cfile(i2)
 271        continue
         endif
c			how long is the string, result on stack must be mult of 4
         isize1 = lastblnk(compar1)
         if (isize1 .eq. 0) call oerror(n370, m2, 'FNAME')
         if (mod(isize1, 4)  .ne. 0) isize1 = (ifix(isize1/4) + 1) * 4
c			store contents of string into K array
         numi2 = min(n30, iwpc(isize1))
         call runtag(numi2, itag)
         call copy(numi2, rcompar1, c(itag))
c			update the stack pointers so that proper K loc 
c			will be on the stack
         sp = sp + 4
         stack(sp-3) = 14
         stack(sp-2) = numi2
         stack(sp-1) = itag
         stack(sp) = 3
         v(sp-3) = c(7)
         v(sp-2) = c(4)
         v(sp-1) = c(itag)
         v(sp) = c(2)
         goto 1000
      else
         call oerror(n112, m1, 'FNAME: One needed')
      endif
c
c--------------------------------------------------------
C		   XPIX2C, YPIX2T, C2XPIX, T2YPIX	  
c--------------------------------------------------------
 280  CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'XPIX2C : One needed')
      if (numplots .le. 0) call oerror(n299, m2, 'XPIX2C')
c
      xx = (v(sp)-bx(numplots))/ax(numplots)
      if (showplot(numplots) .eq. 1 .and. xlog .gt. 0.5) xx = 10**xx
c
      GOTO 5001
c-----------------
 290  CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'YPIX2T : One needed')
      if (numplots .le. 0) call oerror(n299, m2, 'YPIX2T')
c
      xx = (v(sp)-by(numplots))/ay(numplots)
      if (showplot(numplots) .eq. 1 .and. ylog .gt. 0.5) xx = 10**xx
c
      GOTO 5001
c-----------------
 300  CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'C2XPIX : One needed')
      if (numplots .le. 0) call oerror(n299, m2, 'C2XPIX')
c
      if (showplot(numplots) .eq. 1 .and. xlog .gt. 0.5) then
	xx = log10(v(sp))*ax(numplots) + bx(numplots)
      else
        xx = v(sp)*ax(numplots) + bx(numplots)
      endif
c
      GOTO 5001
c-----------------
 310  CONTINUE
      if (sp.lt.1) call oerror(n112, m1, 'T2YPIX : One needed')
      if (numplots .le. 0) call oerror(n299, m2, 'T2YPIX')
c
      if (showplot(numplots) .eq. 1 .and. ylog .gt. 0.5) then
	xx = log10(v(sp))*ay(numplots) + by(numplots)
      else
        xx = v(sp)*ay(numplots) + by(numplots)
      endif
c
      GOTO 5001
C---------------------------------------------------------------------
C	       E    X	 I    T
C---------------------------------------------------------------------
1000  LFLAG=2
      GO TO 10000
5000  lflag=4
      goto 10000
5001  LFLAG=5
      GO TO 10000
c
10000 CONTINUE
      RETURN
      END

