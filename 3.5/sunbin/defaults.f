      program defaults
c
c     @(#)defaults.f	5.1 06/22/94
c
c     Uses the POPSDAT.ADVERBS file and the CORE.INC file to create an
c     include file for setting default values to adverbs
c
      character*80 input
      character*10 name(200), name1
      character*19 value(200), infinite
      integer*2 numoffset, islash, icomma, lparen, rparen, numindex, 
     .		num, ncomma, nextindex, ierr, offsets(200), number,
     .		numin, i
      integer*4 long
      real*8 d_infinity
c
      data value/200*'           '/
c
      numoffset = 0
      number = 0
      numin = 1
c
      write(infinite,5) d_infinity()
5     format('X''',16r,i16,'''')
c
10    continue

     	 read(5,11,end=9999) input
11	 format(a)
	 if (index(input,'COMMON') .eq. 0) then
		goto 10
	 else
		goto 12
	 endif
c	 Skip over all input lines until you get to COMMON line
c
12	islash = index(input,'/')
	islash = index(input(islash+1:),'/') + islash
c	Skip over common name definition
c
15	icomma = index(input(islash+1:),',') + islash
c
	if (icomma .eq. islash) then
c		We are at the end of the current line
		read(5,11,end=9999) input
		if (input(6:6) .ne. '.') goto 99
c		Stop if the common block definition is over with
c
		islash = 6
c		First field will start in column 6
		goto 15
	endif
c
	lparen = index(input(islash+1:icomma-1),'(') + islash
c	If a '(' is present, then we have an array; else we have a scalar
c
	if (lparen .eq. islash) then
c		We have a scalar
c
		numindex = 1
		name1 = input(islash+1:icomma-1)
	else
c		We have an array
c
		numindex = 1
		name1 = input(islash+1:lparen-1)
		rparen = index(input(lparen+1:),')') + lparen
c		Array must take up at least 1 R*4 word in size; find
c		location of ')'.
c
c		Now start parsing the dimensions
		nextindex = lparen
		numindex = 1
c
20		ncomma = index(input(nextindex+1:rparen-1),',') + nextindex
c		Any commas?
c
		if (ncomma .eq. nextindex) then
			read(input(nextindex+1:rparen-1),21,iostat=ierr) num
21			format(i10)
			if (ierr .ne. 0) write(0,*) input(islash+1:icomma-1),
     .							'Bad input?'
			numindex = numindex * num
			icomma = index(input(rparen+1:),',') + rparen

		else
			read(input(nextindex+1:ncomma-1),21,iostat=ierr) num
			if (ierr .ne. 0) write(0,*) input(islash+1:icomma-1),
     .							'Bad input line?'
			numindex = numindex * num
			nextindex = ncomma
			goto 20
		endif
	endif
c
	if (name1 .eq. 'ALPHABET') then
c	    Doing single-letter adverbs
c
	    do 80 i = 1, numindex
	    	number = number + 1
	    	if (number .gt. 200) goto 9999
	    	numoffset = numoffset + 1
	    	offsets(number) = numoffset
		name(number) = name1
80	    	continue
	else
	   number = number + 1
	   if (number .gt. 200) goto 9999
	   numoffset = numoffset + numindex
	   offsets(number) = numoffset
	   name(number) = name1
	endif
c
	islash = icomma
	goto 15
c
99    open(unit=10, file='POPSDAT.ADVERBS',status='old',
     .		form='formatted')
      rewind(10)
c
101     read(10,11,end=9999) input
	if (input(1:2) .eq. 'C-' .or. input(1:2) .eq. '  ') goto 101
c	Skip comment/blank lines
c
	numin = numin + 1
	if (numin .gt. number) goto 200
	if (input(18:18) .eq. '8' .or. input(18:18) .eq. '7') goto 101
	if (input(50:59) .ne. '          ') then
	    value(numin) = input(50:59)
	    if (value(numin) .eq. 'INFINITY   ') value(numin) = infinite
	else 
	    value(numin) = input(31:40)
	endif
	if (ierr .ne. 0) write(0,*) input(1:40), 'Bad input?'
      	goto 101
c
200   continue
c
c     All done... Now proceed with creating initialization lines.
c
      write(6,*) '      INTEGER*2 OFFSETS(',number+1,'), NUMTAGS'
      write(6,*) '      REAL*8 DEFVALUES(',number+1,')'
      write(6,*) '      DATA NUMTAGS/',number+1,'/'
c
      do 210 i = 1, number
	if (value(i) .eq. '           ') then
	  write(6,205)  i, offsets(i-1)+1, i, infinite
205	  format('      DATA OFFSETS(', i3, ') /', i6, '/, DEFVALUES(',
     .				      i3, ') /', a, '/' )
	else
	  write(6,205)  i, offsets(i-1)+1, i, value(i)
	endif
210	continue
      write(6,205)  number+1, offsets(number)+1, number+1, infinite
c
      write(0,*) 'DEFAULTS.EXE: Normal termination'
      call exit(long(0))
      stop
9999  write(0,*) 'DEFAULTS.EXE: Abnormal termination'
      call exit(long(1))
      stop
      end
