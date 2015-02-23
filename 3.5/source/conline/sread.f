
c
c     @(#)srw.F	5.1 06/22/94
c
      subroutine sread(infmt, string)
c
c     Takes the stack of adverbs and reads values for them from STRING 
c     using the format statement INFMT
c

c
      character*(*) infmt
c

c
      character*(*) string
c

c
      integer*2 stkptr, i0, i1, i2, i3, numchar, ioutst, lastblnk,
     .		numstring, nsize, tag, type, i, ier, itmpstring(30),
     .		nfield, inum, iwpr, fdefault, idefault, ldefault,
     .		stck(40), maxstck, top, ssize, irpt, m1,
     .		i0level0, inumlevel0, n373, n374 
      integer*4 ivalue
      character*60 tmpstring, frmt
      character*8 pms
      character*6 frmt2, snum
      character*4 bms
      character*1 lc1, c1, c2
      logical test, pushcmp, popcmp, left, right
      real value
c
      include 'stk.inc'
      include 'core.inc'
c
      equivalence (itmpstring, tmpstring)
c
      parameter (fdefault = 15)
      parameter (idefault = 12)
      parameter (ldefault = 2)
c     Default field widths for F, I, and L formats.
c
      parameter (maxstck = 20)
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      if (lastblnk(infmt) .eq. 0) call oerror(n373, m1, 'Empty format')
c
      stkptr = sp0
      i0 = 1
      numchar = len(infmt)
      numstring = len(string)
      ioutst = 1
      inum = 0
      pms = '    '
      bms = '    '
      frmt2 = '(a)'
      nsize = 0
      top = 0
      i0level0 = 1
      inumlevel0 = 0
      do 1 i1 = 1, maxstck
	stck(i1) = 0
1	continue
c

c
10    if (i0 .gt. numchar .and. stkptr .le. sp) then
c
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')
c

	call oerror(n373, m1, 'Format exhausted')

c
      else if (i0 .gt. numchar) then
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')

	return
      else
        lc1 = infmt(i0:i0)
	call uppercase(lc1, c1)
      endif
c
c------
      if (ichar(c1).le.32 .or. ichar(c1).ge.128 .or. c1.eq.',') then
	i0 = i0 + 1
c	Skip all blanks, commas, non-printable characters between fields
c
c------
      else if (lle(c1,'9') .and. lge(c1,'0') .or. c1 .eq. '-' ) then
c	We have a number begining a field; parse the number until you get
c	to a non-number and then set up INUM.  Probably a
c	repeat count or a scale factor for 'P'.
c
	i1 = i0
	i2 = i0 + 1
20	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 20
	  endif
	else
	  call oerror(n373, m1, 'Format exhausted')
        endif
	if (c1 .eq. '-' .and. i1+1 .eq. i2) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
c	Must have a digit after the minus sign.
c
	read(infmt(i1:i2-1),'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i0 = i2
c
c------
      else if (c1 .eq. 'P') then
c	We have a P scale factor; set up PMS and reset the repeat-count
c
	write(snum,'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	pms = snum // 'P' // ','
	inum = 0
	i0 = i0 + 1
c
c------

      else if (c1 .eq. 'B') then
c	We have a B scale factor; set up PMS
c
	if (inum .ne. 0) 
     .	   call oerror(n373, m1, 'Extra numerical field in format')
	i2 = i0 + 1
	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if (c2 .eq. 'Z' .or. c2 .eq. 'z' .or.
     .	      c2 .eq. 'N' .or. c2 .eq. 'n') then
	     i2 = i2 + 1
	  else
	     c2 = ' '
	  endif
        endif
	bms = 'B' // c2 // ', '
	i0 = i2
c
c------

      else if (c1 .eq. '(' ) then
c	We have an open parenthesis; push the location of the ( in INFMT and
c	the current repeat count onto the stack.
c
	if (top .eq. 0) then
	   i0level0 = i0
	   inumlevel0 = inum
	endif
c	Save level zero repeat count
c
     	if (.not. pushcmp(stck, top, maxstck, i0) .or.
     .	    .not. pushcmp(stck, top, maxstck, max(1,inum) ) )
     .	    call oerror(n373, m1, 'Too many ( in format')
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. ')' ) then
c	We have a close parenthesis; get the repeat count for the (..) and
c	the location in INFMT of the (.
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
     	if (.not. popcmp(stck, top, irpt) .or. .not. popcmp(stck, top, i1) ) 
     .		call oerror(n373, m1, 'Too many )''s ')
	if (irpt .gt. 1) then
     	   if (.not. pushcmp(stck, top, maxstck, i1) .or.
     .	       .not. pushcmp(stck, top, maxstck, max(1,irpt-1) ) )
     .	       call oerror(n373, m1, 'Too many )''s or (''s')
	   i0 = i1
c	   We have NOT completed the repeat count for the opening (
c	   so we may have to again repeat this (...). Pop I0 and push it 
c	   back onto the stack; push a decremented count back onto the stack.
c 
	endif
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'X') then
c	We have an 'X' format.  If no repeat count, assume 1 and then
c	add blanks to the output string.
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	if (inum .eq. 0) inum = 1
        if (ioutst + inum - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	ioutst = ioutst + inum
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'T') then	
c	We have a 'T' format.  
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	left = .false.
	right = .false.
	i1 = i0 + 1
	i2 = i1
30	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 30
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'L' .or. c2 .eq. 'l') .and. i2 .eq. i1) then
	     left = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'R' .or. c2 .eq. 'r') .and. i2 .eq. i1) then
	     right = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  endif
        endif
	if (i2 .eq. i1) call oerror(n373, m1, 'Incomplete T format')
	read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield  
	if (ier .ne. 0 .or. nfield .le. 0 .or. nfield .gt. numstring) 
     .		call oerror(n373, m1, 'Bad number field in format')
c	Parse column number
c
	if (left) then
	   ioutst = max(1,ioutst - nfield)
	else if (right) then
	   ioutst = ioutst + nfield
	else
	   ioutst = nfield
	endif
      	if (ioutst .gt. numstring) call oerror(n374, m1, 'End of record')
	i0 = i2
c
c------
      else if (c1 .eq. ':') then
c	We have a termination control :
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')

        if (stkptr .gt. sp) return

	i0 = i0 + 1
c
c------

c
c------

      else if (c1 .eq. 'A') then
c	We have an 'A' format.  Check for and parse field width, if
c	any.  Check that next thing to be printed is a string or
c	string adverb.  If so, check its size to that of the optional
c	filed width.  Then place value of string/adverb
c	into STRING and increment IOUT.  Do this for the specified repeat
c	count.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
50	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 50
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 50
	  endif
        endif
	if (i2 .ne. i1) then
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
	else
	   nfield = 0
	endif
	do 55 i = 1, max(1,inum)

           if (stkptr .gt. sp) return

	   if ( stkptr+3 .gt. sp .or. ( stack(stkptr+3) .ne. 3 .or. 
     .         (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) 
     .	       	call oerror(n373, m1, 'Incompatable format')
c
	   type = stack(stkptr)

           if (type .eq. 7) then
	      ssize=iwpr(stack(stkptr+1))
	   else
	      call oerror(n373, m1, 'Incompatable format')
	   endif

           tag=stack(stkptr+2)
           stkptr=stkptr+4
	   if (nfield .eq. 0) nfield = 2*ssize
      	   if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	   tmpstring = string(ioutst:ioutst+nfield-1)
	   call copy( ssize, itmpstring, c(tag))

	   ioutst = ioutst + nfield
55	   continue
	i0 = i2
	inum = 0
c
c------
      else if (c1 .eq. 'F' .or. c1 .eq. 'G' .or. c1 .eq. 'E' .or. 
     .	       c1 .eq. 'L' .or. c1 .eq. 'I' .or. c1 .eq. 'D') then	
c	We have an 'E, F, I, L, or G' format.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
60	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 60
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 60
	  endif
        endif
c	Parse field width
c
	if (i2 .eq. i1) then
	   if (c1 .eq. 'I') then
	     nfield = idefault
	   else if (c1 .eq. 'L') then
	     nfield = ldefault
	   else
	     nfield = fdefault
	   endif
	   i3 = i2
c	   Set default field width
c
	else 
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
c
	   if (c1 .eq. 'L' .or. c2 .ne. '.') then
	      i3 = i2
	   else
	      i3 = i2 + 1
62	      if (i3 .le. numchar) then
	  	c2 = infmt(i3:i3)
	  	if ( (lle(c2,'9') .and. lge(c2,'0')) .or.
     .		     c2 .eq. '.'  .or. c2 .eq. 'e' .or. c2 .eq. 'E') then
	     	  i3 = i3 + 1
	     	  goto 62
	  	endif
	      endif
	      if (i3 .eq. i2 + 1) 
     .		call oerror(n373, m1, 'Bad format specification')
	   endif
	endif
c	Parse field width and # sig. digits.
c  
	frmt = '(' // pms // bms // infmt(i0:i3-1) // ')'
c	Create format statement out of the above pieces
c
	do 65 i = 1, max(1,inum)

            if (stkptr .gt. sp) return

      	    if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	    tmpstring = string(ioutst:ioutst+nfield-1)
	    if (c1 .eq. 'I') then
		read(tmpstring,fmt=frmt,iostat=ier) ivalue
		value = ivalue
	    else if (c1 .eq. 'L') then
		read(tmpstring,fmt=frmt,iostat=ier) test
		if (test) then
		  value = c(true)
		else
		  value = c(false)
		endif
	    else
		read(tmpstring,fmt=frmt,iostat=ier) value
	    endif
	    if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad format specification')
c
	    if ((stkptr+3 .gt. sp) .or. 
     .    	    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     	    (stack(stkptr+3) .ne. 3 .or. 
     .        	    (stack(stkptr) .ne. 7 .and. 
     .		     stack(stkptr) .ne. 14)) ) ) then
c		   We have a scalar!
c
	   	tag = stack(stkptr)
		c(tag) = value
         	stkptr=stkptr+1
	    else
c		We have an array!!
c
		if (nsize .le. 0) then
c		   We aren't trying to complete an array
c
	 	   type = stack(stkptr)
        	   nsize=stack(stkptr+1)
        	   tag=stack(stkptr+2)
        	   if (type.ne. 2) call oerror(n373, m1, 'Incompatable format')
	        endif
		c(tag) = value
		tag = tag + 1
		nsize = nsize - 1
		if (nsize .eq. 0) stkptr = stkptr + 4
	    endif
c

	    ioutst = ioutst + nfield
65	    continue
	i0 = i3
	inum = 0
c
c------
      else
	call oerror(n373, m1, 'Bad format specification')
      endif
c
      goto 10
c

800   call oerror(n374,m1,'End of file')
900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c




c
      subroutine usread(string)
c
c     Takes the stack of adverbs and reads values for them from STRING 
c     using list-directed IO
c

c

c
      character*(*) string
c

c
      integer*2 stkptr, ioutst, numstring, tag, type, i1, lastblnk,
     .		itmpstring(60), nfield, iwpr, nsize, m1, ssize,
     .		i2, i3, i4, n373, n374
      character*120 tmpstring
      character*1 c1
      real value
c
      include 'stk.inc'
      include 'core.inc'
c
      equivalence (itmpstring, tmpstring)
c
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      stkptr = sp0
      numstring = len(string)
      nsize = 0
c

c
      ioutst = 1
c
2400  continue
c
      if (stkptr .gt. sp) return
c
      tmpstring = string(ioutst:)
c
      if ((stkptr+3 .gt. sp) .or. 
     .    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     (stack(stkptr+3) .ne. 3 .or. 
     .     (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) ) then
c	we have a scalar!
c
	type = 1
	nsize = 0
	tag = stack(stkptr)
	nfield = 1
	call getnum(tmpstring,nfield,value)

	if (nfield .ge. len(tmpstring)) call oerror(n374, m1, 'Empty record')

 	nfield = nfield - 1
       if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	c(tag) = value
	stkptr = stkptr + 1
	ioutst = ioutst + nfield
c
      else
c
c       an array or string
c
        if (nsize .le. 0) then
c	   we aren't trying to complete an array
c
	   type=stack(stkptr)
           nsize=stack(stkptr+1)
           tag=stack(stkptr+2)
	endif
c
        if (type.eq.2) then
c
c       We have an array
c
	   nfield = 1
	   call getnum(tmpstring,nfield,value)

	   if (nfield .ge. len(tmpstring)) call oerror(n374, m1, 'Empty record')

	   nfield = nfield - 1
      	   if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   c(tag) = value
	   tag = tag + 1
	   nsize = nsize - 1
	   if (nsize .eq. 0) stkptr = stkptr + 4
	   ioutst = ioutst + nfield

	else 
c	We have a string or literal constant
c
	   i3 = lastblnk(tmpstring)

	   if (i3 .eq. 0) call oerror(n374, m1, 'Empty record') 

c
	   do 2401 i1 = 1, i3
		if (tmpstring(i1:i1) .ne. ' ') goto 2402
2401		continue
	   i1 = i3
2402	   tmpstring = tmpstring(i1:)
	   i1 = i1 - 1
	   if (tmpstring(1:1) .eq. '''' .or. tmpstring(1:1) .eq. '"') then
		c1 = tmpstring(1:1)
		tmpstring = tmpstring(2:)
		i3 = 2
		i4 = 1
2403		i2 = index(tmpstring(i4:), c1) + i4 - 1
		if (i2 .eq. i4 - 1) call oerror(n374, m1, 'End of record')
		if (tmpstring(i2+1:i2+1) .eq. c1) then
			tmpstring(i2+1:) = tmpstring(i2+2:)
			i3 = i3 + 1
			i4 = i2 + 1
			goto 2403
		endif
		tmpstring(i2:) = ' '
c		Take care of strings surrounded by quotes; be wary of 
c		double quotes
c
	   else
		i3 = 0
	        i2 = index(tmpstring, ' ')
	        if (i2 .ne. 0) tmpstring(i2:) = ' '
c		Takes care of strings not begun with quotes; the next blank
c		signals end of string.
c
	   endif
c	   i1 = # blanks precceding string, i2 = # chars in string; i3 =
c		number of quotation marks that didn't get into final string.
c
	   if (type .eq. 7) then
	     ssize = iwpr(nsize)
	   else
	     call oerror(n374, m1, ' ')
	   endif
	   nfield = i2 + i1 + i3
           if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   call copy( ssize, itmpstring, c(tag))
	   nsize = 0
	   stkptr = stkptr + 4
	   ioutst = ioutst + nfield
c
	endif
      endif
c
      goto 2400
c

800   call oerror(n374,m1,'End of file')
900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c


c
      subroutine swrite(infmt, string)
c
c     Takes the stack of adverbs and prints them out into STRING 
c     using the format statement INFMT
c

c
      character*(*) infmt
c

c
      character*(*) string
c

c
      integer*2 stkptr, i0, i1, i2, i3, numchar, ioutst, lastblnk,
     .		numstring, nsize, tag, type, i, ier, itmpstring(30),
     .		nfield, inum, iwpr, fdefault, idefault, ldefault,
     .		stck(40), maxstck, top, ssize, irpt, m1,
     .		i0level0, inumlevel0, n373, n374 
      integer*4 ivalue
      character*60 tmpstring, frmt
      character*8 pms
      character*6 frmt2, snum
      character*4 bms
      character*1 lc1, c1, c2
      logical test, pushcmp, popcmp, left, right
      real value
c
      include 'stk.inc'
      include 'core.inc'
c
      equivalence (itmpstring, tmpstring)
c
      parameter (fdefault = 15)
      parameter (idefault = 12)
      parameter (ldefault = 2)
c     Default field widths for F, I, and L formats.
c
      parameter (maxstck = 20)
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      if (lastblnk(infmt) .eq. 0) call oerror(n373, m1, 'Empty format')
c
      stkptr = sp0
      i0 = 1
      numchar = len(infmt)
      numstring = len(string)
      ioutst = 1
      inum = 0
      pms = '    '
      bms = '    '
      frmt2 = '(a)'
      nsize = 0
      top = 0
      i0level0 = 1
      inumlevel0 = 0
      do 1 i1 = 1, maxstck
	stck(i1) = 0
1	continue
c

      string = ' '

c
10    if (i0 .gt. numchar .and. stkptr .le. sp) then
c
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')
c

	call oerror(n373, m1, 'Format exhausted')

c
      else if (i0 .gt. numchar) then
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')

	return
      else
        lc1 = infmt(i0:i0)
	call uppercase(lc1, c1)
      endif
c
c------
      if (ichar(c1).le.32 .or. ichar(c1).ge.128 .or. c1.eq.',') then
	i0 = i0 + 1
c	Skip all blanks, commas, non-printable characters between fields
c
c------
      else if (lle(c1,'9') .and. lge(c1,'0') .or. c1 .eq. '-' ) then
c	We have a number begining a field; parse the number until you get
c	to a non-number and then set up INUM.  Probably a
c	repeat count or a scale factor for 'P'.
c
	i1 = i0
	i2 = i0 + 1
20	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 20
	  endif
	else
	  call oerror(n373, m1, 'Format exhausted')
        endif
	if (c1 .eq. '-' .and. i1+1 .eq. i2) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
c	Must have a digit after the minus sign.
c
	read(infmt(i1:i2-1),'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i0 = i2
c
c------
      else if (c1 .eq. 'P') then
c	We have a P scale factor; set up PMS and reset the repeat-count
c
	write(snum,'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	pms = snum // 'P' // ','
	inum = 0
	i0 = i0 + 1
c
c------

      else if (c1 .eq. 'S') then
c	We have a S scale factor; set up PMS
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	i2 = i0 + 1
	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if (c2 .eq. 'P' .or. c2 .eq. 'p' .or.
     .	      c2 .eq. 'S' .or. c2 .eq. 's') then
	     i2 = i2 + 1
	  else
	     c2 = ' '
	  endif
        endif
	bms = 'S' // c2 // ', '
	i0 = i2
c
c------

      else if (c1 .eq. '(' ) then
c	We have an open parenthesis; push the location of the ( in INFMT and
c	the current repeat count onto the stack.
c
	if (top .eq. 0) then
	   i0level0 = i0
	   inumlevel0 = inum
	endif
c	Save level zero repeat count
c
     	if (.not. pushcmp(stck, top, maxstck, i0) .or.
     .	    .not. pushcmp(stck, top, maxstck, max(1,inum) ) )
     .	    call oerror(n373, m1, 'Too many ( in format')
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. ')' ) then
c	We have a close parenthesis; get the repeat count for the (..) and
c	the location in INFMT of the (.
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
     	if (.not. popcmp(stck, top, irpt) .or. .not. popcmp(stck, top, i1) ) 
     .		call oerror(n373, m1, 'Too many )''s ')
	if (irpt .gt. 1) then
     	   if (.not. pushcmp(stck, top, maxstck, i1) .or.
     .	       .not. pushcmp(stck, top, maxstck, max(1,irpt-1) ) )
     .	       call oerror(n373, m1, 'Too many )''s or (''s')
	   i0 = i1
c	   We have NOT completed the repeat count for the opening (
c	   so we may have to again repeat this (...). Pop I0 and push it 
c	   back onto the stack; push a decremented count back onto the stack.
c 
	endif
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'X') then
c	We have an 'X' format.  If no repeat count, assume 1 and then
c	add blanks to the output string.
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	if (inum .eq. 0) inum = 1
        if (ioutst + inum - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	string(ioutst:ioutst+inum-1) = ' '

	ioutst = ioutst + inum
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'T') then	
c	We have a 'T' format.  
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	left = .false.
	right = .false.
	i1 = i0 + 1
	i2 = i1
30	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 30
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'L' .or. c2 .eq. 'l') .and. i2 .eq. i1) then
	     left = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'R' .or. c2 .eq. 'r') .and. i2 .eq. i1) then
	     right = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  endif
        endif
	if (i2 .eq. i1) call oerror(n373, m1, 'Incomplete T format')
	read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield  
	if (ier .ne. 0 .or. nfield .le. 0 .or. nfield .gt. numstring) 
     .		call oerror(n373, m1, 'Bad number field in format')
c	Parse column number
c
	if (left) then
	   ioutst = max(1,ioutst - nfield)
	else if (right) then
	   ioutst = ioutst + nfield
	else
	   ioutst = nfield
	endif
      	if (ioutst .gt. numstring) call oerror(n374, m1, 'End of record')
	i0 = i2
c
c------
      else if (c1 .eq. ':') then
c	We have a termination control :
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')

        if (stkptr .gt. sp) return

	i0 = i0 + 1
c
c------

c
c------

      else if (c1 .eq. 'H') then
c	We have a holerith field.  Look for closing ", making sure
c	to skip all double quotes.  Add to STRING the contents of the
c	holerith (skiping over start/end ").  Error if no closing quote.
c
	if (inum .le. 0) call oerror(n373, m1, 'Bad H format')
	i1 = i0 + 1
	i2 = i1 + inum
	if (i2 .gt. numchar) call oerror(n373, m1, 'Format exhausted')
        if (ioutst + i2 - i1 - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	string(ioutst:ioutst+i2-i1-1) = infmt(i1:i2-1)
	ioutst = ioutst + i2 - i1
	i0 = i2 + 1
c
c------
      else if (c1 .eq. '"' .or. c1 .eq. '''') then
c	We have a " or ' holerith field.  Look for closing ", making sure
c	to skip all double quotes.  Add to STRING the contents of the
c	holerith (skiping over start/end ").  Error if no closing quote.
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	i1 = i0 + 1
	i2 = i1
	i3 = 0
	tmpstring = infmt(i1:)
40	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if (c2 .ne. c1) then
	     i2 = i2 + 1
	     goto 40
	  else if (c2 .eq. c1 .and. i2+1 .lt. numchar .and. 
     .             infmt(i2+1:i2+1) .eq. c1 ) then
	     tmpstring(i2 - i1 + 1:) = tmpstring(i2 - i1 + 2:)
	     i3 = i3 + 1
	     i2 = i2 + 2
	     goto 40
	  endif
	else
	     call oerror(n373, m1, 'No closing quote')
	endif
        if (ioutst + i2 - i1 - 1 - i3 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	string(ioutst:ioutst+i2-i1-1-i3) = tmpstring
	ioutst = ioutst + i2 - i1
	i0 = i2 + 1
c
c------

      else if (c1 .eq. 'A') then
c	We have an 'A' format.  Check for and parse field width, if
c	any.  Check that next thing to be printed is a string or
c	string adverb.  If so, check its size to that of the optional
c	filed width.  Then place value of string/adverb
c	into STRING and increment IOUT.  Do this for the specified repeat
c	count.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
50	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 50
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 50
	  endif
        endif
	if (i2 .ne. i1) then
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
	else
	   nfield = 0
	endif
	do 55 i = 1, max(1,inum)

           if (stkptr .gt. sp) return

	   if ( stkptr+3 .gt. sp .or. ( stack(stkptr+3) .ne. 3 .or. 
     .         (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) 
     .	       	call oerror(n373, m1, 'Incompatable format')
c
	   type = stack(stkptr)

           if (type .eq. 7) then
	      ssize=iwpr(stack(stkptr+1))
	   else
	      ssize = stack(stkptr+1)
	   endif

           tag=stack(stkptr+2)
           stkptr=stkptr+4
	   if (nfield .eq. 0) nfield = 2*ssize
      	   if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	   call copy( ssize, c(tag), itmpstring)
	   string(ioutst:ioutst+nfield-1) = tmpstring(1:2*ssize)

	   ioutst = ioutst + nfield
55	   continue
	i0 = i2
	inum = 0
c
c------
      else if (c1 .eq. 'F' .or. c1 .eq. 'G' .or. c1 .eq. 'E' .or. 
     .	       c1 .eq. 'L' .or. c1 .eq. 'I' .or. c1 .eq. 'D') then	
c	We have an 'E, F, I, L, or G' format.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
60	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 60
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 60
	  endif
        endif
c	Parse field width
c
	if (i2 .eq. i1) then
	   if (c1 .eq. 'I') then
	     nfield = idefault
	   else if (c1 .eq. 'L') then
	     nfield = ldefault
	   else
	     nfield = fdefault
	   endif
	   i3 = i2
c	   Set default field width
c
	else 
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
c
	   if (c1 .eq. 'L' .or. c2 .ne. '.') then
	      i3 = i2
	   else
	      i3 = i2 + 1
62	      if (i3 .le. numchar) then
	  	c2 = infmt(i3:i3)
	  	if ( (lle(c2,'9') .and. lge(c2,'0')) .or.
     .		     c2 .eq. '.'  .or. c2 .eq. 'e' .or. c2 .eq. 'E') then
	     	  i3 = i3 + 1
	     	  goto 62
	  	endif
	      endif
	      if (i3 .eq. i2 + 1) 
     .		call oerror(n373, m1, 'Bad format specification')
	   endif
	endif
c	Parse field width and # sig. digits.
c  
	frmt = '(' // pms // bms // infmt(i0:i3-1) // ')'
c	Create format statement out of the above pieces
c
	do 65 i = 1, max(1,inum)

            if (stkptr .gt. sp) return

      	    if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	    if ((stkptr+3 .gt. sp) .or. 
     .    	    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     	    (stack(stkptr+3) .ne. 3 .or. 
     .        	    (stack(stkptr) .ne. 7 .and. 
     .		     stack(stkptr) .ne. 14)) ) ) then
c		   We have a scalar!
c
		value = sngl(v(stkptr))
         	stkptr=stkptr+1
	    else
c		We have an array!!
c
		if (nsize .le. 0) then
c		   We aren't trying to complete an array
c
	 	   type = stack(stkptr)
        	   nsize=stack(stkptr+1)
        	   tag=stack(stkptr+2)
        	   if (type.ne. 2) call oerror(n373, m1, 'Incompatable format')
	        endif
		value = c(tag)
		tag = tag + 1
		nsize = nsize - 1
		if (nsize .eq. 0) stkptr = stkptr + 4
	    endif
c
	    if (c1 .eq. 'I') then
		ivalue = value
		write(tmpstring,fmt=frmt,iostat=ier) ivalue
	    else if (c1 .eq. 'L') then
		if (value .gt. 0.) then
		  test = .true.
		else
		  test = .false.
		endif
		write(tmpstring,fmt=frmt,iostat=ier) test
	    else
		write(tmpstring,fmt=frmt,iostat=ier) value
	    endif
c
	    if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad format specification')
	    string(ioutst:ioutst+nfield-1) = tmpstring

	    ioutst = ioutst + nfield
65	    continue
	i0 = i3
	inum = 0
c
c------
      else
	call oerror(n373, m1, 'Bad format specification')
      endif
c
      goto 10
c

900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c


c
      subroutine uswrite(string)
c
c     Takes the stack of adverbs and prints them out into STRING 
c     using list-directed IO
c

c

c
      character*(*) string
c

c
      integer*2 stkptr, ioutst, lastblnk, numstring, tag, type, ier,
     .		itmpstring(30), nfield, iwpr, nsize, ssize,  m1, n373,
     .		 n374 
      character*60 tmpstring
      character*16 dfltfmt
      character*8 frmt2
      real value
c
      include 'stk.inc'
      include 'core.inc'
      include 'cio.inc'
c
      equivalence (itmpstring, tmpstring)

c
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      stkptr = sp0
      numstring = len(string)
      ioutst = 1
      dfltfmt = prntfmt
      string = ' '
      frmt2 = '(1x,a)'
      nsize = 0
c
2400  continue
c

      if (stkptr .gt. sp) return

c
      if ((stkptr+3 .gt. sp) .or. 
     .    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     (stack(stkptr+3) .ne. 3 .or. 
     .     (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) ) then
c	we have a scalar!
c
	type = 1
	nsize = 0
	tag = stack(stkptr)
	value = v(stkptr)
	write(tmpstring,dfltfmt,iostat=ier) value
	if (ier .ne. 0) call oerror(n373, m1, 'Bad Format specification')
	nfield = lastblnk(tmpstring)+1

        if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	string(ioutst:ioutst+nfield-1) = tmpstring(1:nfield)
	ioutst = ioutst + nfield
	stkptr = stkptr + 1
c
      else
c
c       an array or string
c
        if (nsize .le. 0) then
c	   we aren't trying to complete an array
c
	   type=stack(stkptr)
           nsize=stack(stkptr+1)
           tag=stack(stkptr+2)
	endif
c
        if (type.eq.2) then
c
c       We have an array
c
	   value = c(tag)
	   write(tmpstring,dfltfmt,iostat=ier) value
	   if (ier .ne. 0) call oerror(n373, m1, 'Bad Format specification')
	   nfield = lastblnk(tmpstring)+1

           if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   string(ioutst:ioutst+nfield-1) = tmpstring(1:nfield)
	   ioutst = ioutst + nfield
	   tag = tag + 1
	   nsize = nsize - 1
	   if (nsize .eq. 0) stkptr = stkptr + 4

	else 
c	We have a string or literal constant
c
	   if (type .eq. 7) then
	     ssize = iwpr(nsize)
	   else
	     ssize = nsize
	   endif
	   nfield = 2*ssize+1

	   nsize = 0
	   stkptr = stkptr + 4
           if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   tmpstring = ' '
	   call copy( ssize, c(tag), itmpstring)
	   string(ioutst:ioutst+nfield-1) = tmpstring(1:nfield)
	   ioutst = ioutst + nfield
c
	endif
      endif
c
c
      goto 2400
c

900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c




c
      subroutine fread(infmt, unit)
c
c     Takes the stack of adverbs and reads values for them from UNIT 
c     using the format statement INFMT
c

c
      character*(*) infmt
c

c
      integer*2 unit
      character*5120 string
c

c
      integer*2 stkptr, i0, i1, i2, i3, numchar, ioutst, lastblnk,
     .		numstring, nsize, tag, type, i, ier, itmpstring(30),
     .		nfield, inum, iwpr, fdefault, idefault, ldefault,
     .		stck(40), maxstck, top, ssize, irpt, m1,
     .		i0level0, inumlevel0, n373, n374 
      integer*4 ivalue
      character*60 tmpstring, frmt
      character*8 pms
      character*6 frmt2, snum
      character*4 bms
      character*1 lc1, c1, c2
      logical test, pushcmp, popcmp, left, right
      real value
c
      include 'stk.inc'
      include 'core.inc'
c
      equivalence (itmpstring, tmpstring)
c
      parameter (fdefault = 15)
      parameter (idefault = 12)
      parameter (ldefault = 2)
c     Default field widths for F, I, and L formats.
c
      parameter (maxstck = 20)
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      if (lastblnk(infmt) .eq. 0) call oerror(n373, m1, 'Empty format')
c
      stkptr = sp0
      i0 = 1
      numchar = len(infmt)
      numstring = len(string)
      ioutst = 1
      inum = 0
      pms = '    '
      bms = '    '
      frmt2 = '(a)'
      nsize = 0
      top = 0
      i0level0 = 1
      inumlevel0 = 0
      do 1 i1 = 1, maxstck
	stck(i1) = 0
1	continue
c

      read(unit,frmt2,err=900,end=800) string

c
10    if (i0 .gt. numchar .and. stkptr .le. sp) then
c
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')
c


        read(unit,frmt2,err=900,end=800) string

	i0 = i0level0
	inum = inumlevel0
c	Find rescan point from last level zero '(' or from start
c	of format.
c
	ioutst = 1
	goto 10
c

c
      else if (i0 .gt. numchar) then
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')

	return
      else
        lc1 = infmt(i0:i0)
	call uppercase(lc1, c1)
      endif
c
c------
      if (ichar(c1).le.32 .or. ichar(c1).ge.128 .or. c1.eq.',') then
	i0 = i0 + 1
c	Skip all blanks, commas, non-printable characters between fields
c
c------
      else if (lle(c1,'9') .and. lge(c1,'0') .or. c1 .eq. '-' ) then
c	We have a number begining a field; parse the number until you get
c	to a non-number and then set up INUM.  Probably a
c	repeat count or a scale factor for 'P'.
c
	i1 = i0
	i2 = i0 + 1
20	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 20
	  endif
	else
	  call oerror(n373, m1, 'Format exhausted')
        endif
	if (c1 .eq. '-' .and. i1+1 .eq. i2) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
c	Must have a digit after the minus sign.
c
	read(infmt(i1:i2-1),'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i0 = i2
c
c------
      else if (c1 .eq. 'P') then
c	We have a P scale factor; set up PMS and reset the repeat-count
c
	write(snum,'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	pms = snum // 'P' // ','
	inum = 0
	i0 = i0 + 1
c
c------

      else if (c1 .eq. 'B') then
c	We have a B scale factor; set up PMS
c
	if (inum .ne. 0) 
     .	   call oerror(n373, m1, 'Extra numerical field in format')
	i2 = i0 + 1
	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if (c2 .eq. 'Z' .or. c2 .eq. 'z' .or.
     .	      c2 .eq. 'N' .or. c2 .eq. 'n') then
	     i2 = i2 + 1
	  else
	     c2 = ' '
	  endif
        endif
	bms = 'B' // c2 // ', '
	i0 = i2
c
c------

      else if (c1 .eq. '(' ) then
c	We have an open parenthesis; push the location of the ( in INFMT and
c	the current repeat count onto the stack.
c
	if (top .eq. 0) then
	   i0level0 = i0
	   inumlevel0 = inum
	endif
c	Save level zero repeat count
c
     	if (.not. pushcmp(stck, top, maxstck, i0) .or.
     .	    .not. pushcmp(stck, top, maxstck, max(1,inum) ) )
     .	    call oerror(n373, m1, 'Too many ( in format')
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. ')' ) then
c	We have a close parenthesis; get the repeat count for the (..) and
c	the location in INFMT of the (.
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
     	if (.not. popcmp(stck, top, irpt) .or. .not. popcmp(stck, top, i1) ) 
     .		call oerror(n373, m1, 'Too many )''s ')
	if (irpt .gt. 1) then
     	   if (.not. pushcmp(stck, top, maxstck, i1) .or.
     .	       .not. pushcmp(stck, top, maxstck, max(1,irpt-1) ) )
     .	       call oerror(n373, m1, 'Too many )''s or (''s')
	   i0 = i1
c	   We have NOT completed the repeat count for the opening (
c	   so we may have to again repeat this (...). Pop I0 and push it 
c	   back onto the stack; push a decremented count back onto the stack.
c 
	endif
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'X') then
c	We have an 'X' format.  If no repeat count, assume 1 and then
c	add blanks to the output string.
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	if (inum .eq. 0) inum = 1
        if (ioutst + inum - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	ioutst = ioutst + inum
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'T') then	
c	We have a 'T' format.  
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	left = .false.
	right = .false.
	i1 = i0 + 1
	i2 = i1
30	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 30
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'L' .or. c2 .eq. 'l') .and. i2 .eq. i1) then
	     left = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'R' .or. c2 .eq. 'r') .and. i2 .eq. i1) then
	     right = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  endif
        endif
	if (i2 .eq. i1) call oerror(n373, m1, 'Incomplete T format')
	read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield  
	if (ier .ne. 0 .or. nfield .le. 0 .or. nfield .gt. numstring) 
     .		call oerror(n373, m1, 'Bad number field in format')
c	Parse column number
c
	if (left) then
	   ioutst = max(1,ioutst - nfield)
	else if (right) then
	   ioutst = ioutst + nfield
	else
	   ioutst = nfield
	endif
      	if (ioutst .gt. numstring) call oerror(n374, m1, 'End of record')
	i0 = i2
c
c------
      else if (c1 .eq. ':') then
c	We have a termination control :
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')

        if (stkptr .gt. sp) return

	i0 = i0 + 1
c
c------

      else if (c1 .eq. '/') then
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')

        read(unit,'(a)',err=900,end=800) string

	ioutst = 1
	i0 = i0 + 1
c
c------
      else if (c1 .eq. '$') then
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	frmt2 = '(a,$)'
	i0 = i0 + 1

c
c------

      else if (c1 .eq. 'A') then
c	We have an 'A' format.  Check for and parse field width, if
c	any.  Check that next thing to be printed is a string or
c	string adverb.  If so, check its size to that of the optional
c	filed width.  Then place value of string/adverb
c	into STRING and increment IOUT.  Do this for the specified repeat
c	count.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
50	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 50
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 50
	  endif
        endif
	if (i2 .ne. i1) then
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
	else
	   nfield = 0
	endif
	do 55 i = 1, max(1,inum)

           if (stkptr .gt. sp) return

	   if ( stkptr+3 .gt. sp .or. ( stack(stkptr+3) .ne. 3 .or. 
     .         (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) 
     .	       	call oerror(n373, m1, 'Incompatable format')
c
	   type = stack(stkptr)

           if (type .eq. 7) then
	      ssize=iwpr(stack(stkptr+1))
	   else
	      call oerror(n373, m1, 'Incompatable format')
	   endif

           tag=stack(stkptr+2)
           stkptr=stkptr+4
	   if (nfield .eq. 0) nfield = 2*ssize
      	   if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	   tmpstring = string(ioutst:ioutst+nfield-1)
	   call copy( ssize, itmpstring, c(tag))

	   ioutst = ioutst + nfield
55	   continue
	i0 = i2
	inum = 0
c
c------
      else if (c1 .eq. 'F' .or. c1 .eq. 'G' .or. c1 .eq. 'E' .or. 
     .	       c1 .eq. 'L' .or. c1 .eq. 'I' .or. c1 .eq. 'D') then	
c	We have an 'E, F, I, L, or G' format.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
60	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 60
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 60
	  endif
        endif
c	Parse field width
c
	if (i2 .eq. i1) then
	   if (c1 .eq. 'I') then
	     nfield = idefault
	   else if (c1 .eq. 'L') then
	     nfield = ldefault
	   else
	     nfield = fdefault
	   endif
	   i3 = i2
c	   Set default field width
c
	else 
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
c
	   if (c1 .eq. 'L' .or. c2 .ne. '.') then
	      i3 = i2
	   else
	      i3 = i2 + 1
62	      if (i3 .le. numchar) then
	  	c2 = infmt(i3:i3)
	  	if ( (lle(c2,'9') .and. lge(c2,'0')) .or.
     .		     c2 .eq. '.'  .or. c2 .eq. 'e' .or. c2 .eq. 'E') then
	     	  i3 = i3 + 1
	     	  goto 62
	  	endif
	      endif
	      if (i3 .eq. i2 + 1) 
     .		call oerror(n373, m1, 'Bad format specification')
	   endif
	endif
c	Parse field width and # sig. digits.
c  
	frmt = '(' // pms // bms // infmt(i0:i3-1) // ')'
c	Create format statement out of the above pieces
c
	do 65 i = 1, max(1,inum)

            if (stkptr .gt. sp) return

      	    if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	    tmpstring = string(ioutst:ioutst+nfield-1)
	    if (c1 .eq. 'I') then
		read(tmpstring,fmt=frmt,iostat=ier) ivalue
		value = ivalue
	    else if (c1 .eq. 'L') then
		read(tmpstring,fmt=frmt,iostat=ier) test
		if (test) then
		  value = c(true)
		else
		  value = c(false)
		endif
	    else
		read(tmpstring,fmt=frmt,iostat=ier) value
	    endif
	    if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad format specification')
c
	    if ((stkptr+3 .gt. sp) .or. 
     .    	    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     	    (stack(stkptr+3) .ne. 3 .or. 
     .        	    (stack(stkptr) .ne. 7 .and. 
     .		     stack(stkptr) .ne. 14)) ) ) then
c		   We have a scalar!
c
	   	tag = stack(stkptr)
		c(tag) = value
         	stkptr=stkptr+1
	    else
c		We have an array!!
c
		if (nsize .le. 0) then
c		   We aren't trying to complete an array
c
	 	   type = stack(stkptr)
        	   nsize=stack(stkptr+1)
        	   tag=stack(stkptr+2)
        	   if (type.ne. 2) call oerror(n373, m1, 'Incompatable format')
	        endif
		c(tag) = value
		tag = tag + 1
		nsize = nsize - 1
		if (nsize .eq. 0) stkptr = stkptr + 4
	    endif
c

	    ioutst = ioutst + nfield
65	    continue
	i0 = i3
	inum = 0
c
c------
      else
	call oerror(n373, m1, 'Bad format specification')
      endif
c
      goto 10
c

800   call oerror(n374,m1,'End of file')
900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c




c
      subroutine ufread(unit)
c
c     Takes the stack of adverbs and reads values for them from UNIT 
c     using list-directed IO
c

c

c
      integer*2 unit, ilen
      character*5120 string
c

c
      integer*2 stkptr, ioutst, numstring, tag, type, i1, lastblnk,
     .		itmpstring(60), nfield, iwpr, nsize, m1, ssize,
     .		i2, i3, i4, n373, n374
      character*120 tmpstring
      character*1 c1
      real value
c
      include 'stk.inc'
      include 'core.inc'
c
      equivalence (itmpstring, tmpstring)
c
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      stkptr = sp0
      numstring = len(string)
      nsize = 0
c

2300  if (unit .lt. 0) then
	call pread2('# ', string, ilen)
      else
	read(unit,'(a)',err=900,end=800) string
      endif

c
      ioutst = 1
c
2400  continue
c
      if (stkptr .gt. sp) return
c
      tmpstring = string(ioutst:)
c
      if ((stkptr+3 .gt. sp) .or. 
     .    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     (stack(stkptr+3) .ne. 3 .or. 
     .     (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) ) then
c	we have a scalar!
c
	type = 1
	nsize = 0
	tag = stack(stkptr)
	nfield = 1
	call getnum(tmpstring,nfield,value)

	if (nfield .ge. len(tmpstring)) goto 2300

 	nfield = nfield - 1
       if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	c(tag) = value
	stkptr = stkptr + 1
	ioutst = ioutst + nfield
c
      else
c
c       an array or string
c
        if (nsize .le. 0) then
c	   we aren't trying to complete an array
c
	   type=stack(stkptr)
           nsize=stack(stkptr+1)
           tag=stack(stkptr+2)
	endif
c
        if (type.eq.2) then
c
c       We have an array
c
	   nfield = 1
	   call getnum(tmpstring,nfield,value)

	   if (nfield .ge. len(tmpstring)) goto 2300

	   nfield = nfield - 1
      	   if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   c(tag) = value
	   tag = tag + 1
	   nsize = nsize - 1
	   if (nsize .eq. 0) stkptr = stkptr + 4
	   ioutst = ioutst + nfield

	else 
c	We have a string or literal constant
c
	   i3 = lastblnk(tmpstring)

	   if (i3 .eq. 0) goto 2300

c
	   do 2401 i1 = 1, i3
		if (tmpstring(i1:i1) .ne. ' ') goto 2402
2401		continue
	   i1 = i3
2402	   tmpstring = tmpstring(i1:)
	   i1 = i1 - 1
	   if (tmpstring(1:1) .eq. '''' .or. tmpstring(1:1) .eq. '"') then
		c1 = tmpstring(1:1)
		tmpstring = tmpstring(2:)
		i3 = 2
		i4 = 1
2403		i2 = index(tmpstring(i4:), c1) + i4 - 1
		if (i2 .eq. i4 - 1) call oerror(n374, m1, 'End of record')
		if (tmpstring(i2+1:i2+1) .eq. c1) then
			tmpstring(i2+1:) = tmpstring(i2+2:)
			i3 = i3 + 1
			i4 = i2 + 1
			goto 2403
		endif
		tmpstring(i2:) = ' '
c		Take care of strings surrounded by quotes; be wary of 
c		double quotes
c
	   else
		i3 = 0
	        i2 = index(tmpstring, ' ')
	        if (i2 .ne. 0) tmpstring(i2:) = ' '
c		Takes care of strings not begun with quotes; the next blank
c		signals end of string.
c
	   endif
c	   i1 = # blanks precceding string, i2 = # chars in string; i3 =
c		number of quotation marks that didn't get into final string.
c
	   if (type .eq. 7) then
	     ssize = iwpr(nsize)
	   else
	     call oerror(n374, m1, ' ')
	   endif
	   nfield = i2 + i1 + i3
           if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   call copy( ssize, itmpstring, c(tag))
	   nsize = 0
	   stkptr = stkptr + 4
	   ioutst = ioutst + nfield
c
	endif
      endif
c
      goto 2400
c

800   call oerror(n374,m1,'End of file')
900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c


c
      subroutine fwrite(infmt, unit)
c
c     Takes the stack of adverbs and prints them out onto UNIT 
c     using the format statement INFMT
c

c
      character*(*) infmt
c

c
      integer*2 unit
      character*5120 string
c

c
      integer*2 stkptr, i0, i1, i2, i3, numchar, ioutst, lastblnk,
     .		numstring, nsize, tag, type, i, ier, itmpstring(30),
     .		nfield, inum, iwpr, fdefault, idefault, ldefault,
     .		stck(40), maxstck, top, ssize, irpt, m1,
     .		i0level0, inumlevel0, n373, n374 
      integer*4 ivalue
      character*60 tmpstring, frmt
      character*8 pms
      character*6 frmt2, snum
      character*4 bms
      character*1 lc1, c1, c2
      logical test, pushcmp, popcmp, left, right
      real value
c
      include 'stk.inc'
      include 'core.inc'
c
      equivalence (itmpstring, tmpstring)
c
      parameter (fdefault = 15)
      parameter (idefault = 12)
      parameter (ldefault = 2)
c     Default field widths for F, I, and L formats.
c
      parameter (maxstck = 20)
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      if (lastblnk(infmt) .eq. 0) call oerror(n373, m1, 'Empty format')
c
      stkptr = sp0
      i0 = 1
      numchar = len(infmt)
      numstring = len(string)
      ioutst = 1
      inum = 0
      pms = '    '
      bms = '    '
      frmt2 = '(a)'
      nsize = 0
      top = 0
      i0level0 = 1
      inumlevel0 = 0
      do 1 i1 = 1, maxstck
	stck(i1) = 0
1	continue
c

      string = ' '

c
10    if (i0 .gt. numchar .and. stkptr .le. sp) then
c
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')
c


        write(unit,frmt2,err=900) string(1:ioutst)
	string = ' '

	i0 = i0level0
	inum = inumlevel0
c	Find rescan point from last level zero '(' or from start
c	of format.
c
	ioutst = 1
	goto 10
c

c
      else if (i0 .gt. numchar) then
	if (top .ne. 0) call oerror(n373, m1, 'Missing closing )')

        write(unit,frmt2,err=900) string(1:ioutst)

	return
      else
        lc1 = infmt(i0:i0)
	call uppercase(lc1, c1)
      endif
c
c------
      if (ichar(c1).le.32 .or. ichar(c1).ge.128 .or. c1.eq.',') then
	i0 = i0 + 1
c	Skip all blanks, commas, non-printable characters between fields
c
c------
      else if (lle(c1,'9') .and. lge(c1,'0') .or. c1 .eq. '-' ) then
c	We have a number begining a field; parse the number until you get
c	to a non-number and then set up INUM.  Probably a
c	repeat count or a scale factor for 'P'.
c
	i1 = i0
	i2 = i0 + 1
20	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 20
	  endif
	else
	  call oerror(n373, m1, 'Format exhausted')
        endif
	if (c1 .eq. '-' .and. i1+1 .eq. i2) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
c	Must have a digit after the minus sign.
c
	read(infmt(i1:i2-1),'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i0 = i2
c
c------
      else if (c1 .eq. 'P') then
c	We have a P scale factor; set up PMS and reset the repeat-count
c
	write(snum,'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	pms = snum // 'P' // ','
	inum = 0
	i0 = i0 + 1
c
c------

      else if (c1 .eq. 'S') then
c	We have a S scale factor; set up PMS
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	i2 = i0 + 1
	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if (c2 .eq. 'P' .or. c2 .eq. 'p' .or.
     .	      c2 .eq. 'S' .or. c2 .eq. 's') then
	     i2 = i2 + 1
	  else
	     c2 = ' '
	  endif
        endif
	bms = 'S' // c2 // ', '
	i0 = i2
c
c------

      else if (c1 .eq. '(' ) then
c	We have an open parenthesis; push the location of the ( in INFMT and
c	the current repeat count onto the stack.
c
	if (top .eq. 0) then
	   i0level0 = i0
	   inumlevel0 = inum
	endif
c	Save level zero repeat count
c
     	if (.not. pushcmp(stck, top, maxstck, i0) .or.
     .	    .not. pushcmp(stck, top, maxstck, max(1,inum) ) )
     .	    call oerror(n373, m1, 'Too many ( in format')
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. ')' ) then
c	We have a close parenthesis; get the repeat count for the (..) and
c	the location in INFMT of the (.
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
     	if (.not. popcmp(stck, top, irpt) .or. .not. popcmp(stck, top, i1) ) 
     .		call oerror(n373, m1, 'Too many )''s ')
	if (irpt .gt. 1) then
     	   if (.not. pushcmp(stck, top, maxstck, i1) .or.
     .	       .not. pushcmp(stck, top, maxstck, max(1,irpt-1) ) )
     .	       call oerror(n373, m1, 'Too many )''s or (''s')
	   i0 = i1
c	   We have NOT completed the repeat count for the opening (
c	   so we may have to again repeat this (...). Pop I0 and push it 
c	   back onto the stack; push a decremented count back onto the stack.
c 
	endif
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'X') then
c	We have an 'X' format.  If no repeat count, assume 1 and then
c	add blanks to the output string.
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	if (inum .eq. 0) inum = 1
        if (ioutst + inum - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	string(ioutst:ioutst+inum-1) = ' '

	ioutst = ioutst + inum
	inum = 0
	i0 = i0 + 1
c
c------
      else if (c1 .eq. 'T') then	
c	We have a 'T' format.  
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	left = .false.
	right = .false.
	i1 = i0 + 1
	i2 = i1
30	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 30
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'L' .or. c2 .eq. 'l') .and. i2 .eq. i1) then
	     left = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  else if ( (c2 .eq. 'R' .or. c2 .eq. 'r') .and. i2 .eq. i1) then
	     right = .true.
	     i1 = i1 + 1
	     i2 = i1
	     goto 30
	  endif
        endif
	if (i2 .eq. i1) call oerror(n373, m1, 'Incomplete T format')
	read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield  
	if (ier .ne. 0 .or. nfield .le. 0 .or. nfield .gt. numstring) 
     .		call oerror(n373, m1, 'Bad number field in format')
c	Parse column number
c
	if (left) then
	   ioutst = max(1,ioutst - nfield)
	else if (right) then
	   ioutst = ioutst + nfield
	else
	   ioutst = nfield
	endif
      	if (ioutst .gt. numstring) call oerror(n374, m1, 'End of record')
	i0 = i2
c
c------
      else if (c1 .eq. ':') then
c	We have a termination control :
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')

        if (stkptr .gt. sp) then
          write(unit,frmt2,err=900) string(1:ioutst)
	  return
	endif

	i0 = i0 + 1
c
c------

      else if (c1 .eq. '/') then
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')

        write(unit,'(a)',err=900) string(1:ioutst)
	string = ' '

	ioutst = 1
	i0 = i0 + 1
c
c------
      else if (c1 .eq. '$') then
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	frmt2 = '(a,$)'
	i0 = i0 + 1

c
c------

      else if (c1 .eq. 'H') then
c	We have a holerith field.  Look for closing ", making sure
c	to skip all double quotes.  Add to STRING the contents of the
c	holerith (skiping over start/end ").  Error if no closing quote.
c
	if (inum .le. 0) call oerror(n373, m1, 'Bad H format')
	i1 = i0 + 1
	i2 = i1 + inum
	if (i2 .gt. numchar) call oerror(n373, m1, 'Format exhausted')
        if (ioutst + i2 - i1 - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	string(ioutst:ioutst+i2-i1-1) = infmt(i1:i2-1)
	ioutst = ioutst + i2 - i1
	i0 = i2 + 1
c
c------
      else if (c1 .eq. '"' .or. c1 .eq. '''') then
c	We have a " or ' holerith field.  Look for closing ", making sure
c	to skip all double quotes.  Add to STRING the contents of the
c	holerith (skiping over start/end ").  Error if no closing quote.
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	i1 = i0 + 1
	i2 = i1
	i3 = 0
	tmpstring = infmt(i1:)
40	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if (c2 .ne. c1) then
	     i2 = i2 + 1
	     goto 40
	  else if (c2 .eq. c1 .and. i2+1 .lt. numchar .and. 
     .             infmt(i2+1:i2+1) .eq. c1 ) then
	     tmpstring(i2 - i1 + 1:) = tmpstring(i2 - i1 + 2:)
	     i3 = i3 + 1
	     i2 = i2 + 2
	     goto 40
	  endif
	else
	     call oerror(n373, m1, 'No closing quote')
	endif
        if (ioutst + i2 - i1 - 1 - i3 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	string(ioutst:ioutst+i2-i1-1-i3) = tmpstring
	ioutst = ioutst + i2 - i1
	i0 = i2 + 1
c
c------

      else if (c1 .eq. 'A') then
c	We have an 'A' format.  Check for and parse field width, if
c	any.  Check that next thing to be printed is a string or
c	string adverb.  If so, check its size to that of the optional
c	filed width.  Then place value of string/adverb
c	into STRING and increment IOUT.  Do this for the specified repeat
c	count.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
50	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 50
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 50
	  endif
        endif
	if (i2 .ne. i1) then
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
	else
	   nfield = 0
	endif
	do 55 i = 1, max(1,inum)

           if (stkptr .gt. sp) then
             write(unit,frmt2,err=900) string(1:ioutst)
	     return
	   endif

	   if ( stkptr+3 .gt. sp .or. ( stack(stkptr+3) .ne. 3 .or. 
     .         (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) 
     .	       	call oerror(n373, m1, 'Incompatable format')
c
	   type = stack(stkptr)

           if (type .eq. 7) then
	      ssize=iwpr(stack(stkptr+1))
	   else
	      ssize = stack(stkptr+1)
	   endif

           tag=stack(stkptr+2)
           stkptr=stkptr+4
	   if (nfield .eq. 0) nfield = 2*ssize
      	   if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	   call copy( ssize, c(tag), itmpstring)
	   string(ioutst:ioutst+nfield-1) = tmpstring(1:2*ssize)

	   ioutst = ioutst + nfield
55	   continue
	i0 = i2
	inum = 0
c
c------
      else if (c1 .eq. 'F' .or. c1 .eq. 'G' .or. c1 .eq. 'E' .or. 
     .	       c1 .eq. 'L' .or. c1 .eq. 'I' .or. c1 .eq. 'D') then	
c	We have an 'E, F, I, L, or G' format.  
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
60	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 60
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 60
	  endif
        endif
c	Parse field width
c
	if (i2 .eq. i1) then
	   if (c1 .eq. 'I') then
	     nfield = idefault
	   else if (c1 .eq. 'L') then
	     nfield = ldefault
	   else
	     nfield = fdefault
	   endif
	   i3 = i2
c	   Set default field width
c
	else 
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
c
	   if (c1 .eq. 'L' .or. c2 .ne. '.') then
	      i3 = i2
	   else
	      i3 = i2 + 1
62	      if (i3 .le. numchar) then
	  	c2 = infmt(i3:i3)
	  	if ( (lle(c2,'9') .and. lge(c2,'0')) .or.
     .		     c2 .eq. '.'  .or. c2 .eq. 'e' .or. c2 .eq. 'E') then
	     	  i3 = i3 + 1
	     	  goto 62
	  	endif
	      endif
	      if (i3 .eq. i2 + 1) 
     .		call oerror(n373, m1, 'Bad format specification')
	   endif
	endif
c	Parse field width and # sig. digits.
c  
	frmt = '(' // pms // bms // infmt(i0:i3-1) // ')'
c	Create format statement out of the above pieces
c
	do 65 i = 1, max(1,inum)

            if (stkptr .gt. sp) then
              write(unit,frmt2,err=900) string(1:ioutst)
	      return
	    endif

      	    if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')

	    if ((stkptr+3 .gt. sp) .or. 
     .    	    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     	    (stack(stkptr+3) .ne. 3 .or. 
     .        	    (stack(stkptr) .ne. 7 .and. 
     .		     stack(stkptr) .ne. 14)) ) ) then
c		   We have a scalar!
c
		value = sngl(v(stkptr))
         	stkptr=stkptr+1
	    else
c		We have an array!!
c
		if (nsize .le. 0) then
c		   We aren't trying to complete an array
c
	 	   type = stack(stkptr)
        	   nsize=stack(stkptr+1)
        	   tag=stack(stkptr+2)
        	   if (type.ne. 2) call oerror(n373, m1, 'Incompatable format')
	        endif
		value = c(tag)
		tag = tag + 1
		nsize = nsize - 1
		if (nsize .eq. 0) stkptr = stkptr + 4
	    endif
c
	    if (c1 .eq. 'I') then
		ivalue = value
		write(tmpstring,fmt=frmt,iostat=ier) ivalue
	    else if (c1 .eq. 'L') then
		if (value .gt. 0.) then
		  test = .true.
		else
		  test = .false.
		endif
		write(tmpstring,fmt=frmt,iostat=ier) test
	    else
		write(tmpstring,fmt=frmt,iostat=ier) value
	    endif
c
	    if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad format specification')
	    string(ioutst:ioutst+nfield-1) = tmpstring

	    ioutst = ioutst + nfield
65	    continue
	i0 = i3
	inum = 0
c
c------
      else
	call oerror(n373, m1, 'Bad format specification')
      endif
c
      goto 10
c

900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c


c
      subroutine ufwrite(unit)
c
c     Takes the stack of adverbs and prints them out onto UNIT 
c     using list-directed IO
c

c

c
      integer*2 unit, istring(2560)
      character*5120 string
c

c
      integer*2 stkptr, ioutst, lastblnk, numstring, tag, type, ier,
     .		itmpstring(30), nfield, iwpr, nsize, ssize,  m1, n373,
     .		 n374 
      character*60 tmpstring
      character*16 dfltfmt
      character*8 frmt2
      real value
c
      include 'stk.inc'
      include 'core.inc'
      include 'cio.inc'
c
      equivalence (itmpstring, tmpstring)

      equivalence (istring, string)

c
      parameter (n373 = 373)
      parameter (n374 = 374)
      parameter (m1 = -1)
c
      stkptr = sp0
      numstring = len(string)
      ioutst = 1
      dfltfmt = prntfmt
      string = ' '
      frmt2 = '(1x,a)'
      nsize = 0
c
2400  continue
c

      if (stkptr .gt. sp) then
	if (unit .lt. 0) then
	   call pwrite(istring, lastblnk(string))
	else
	   write(unit,frmt2,err=900) string(1:ioutst)
	endif
	return
      endif

c
      if ((stkptr+3 .gt. sp) .or. 
     .    ((stack(stkptr+3) .ne. 2 .or. stack(stkptr) .ne. 2) .and.
     .     (stack(stkptr+3) .ne. 3 .or. 
     .     (stack(stkptr) .ne. 7 .and. stack(stkptr) .ne. 14)) ) ) then
c	we have a scalar!
c
	type = 1
	nsize = 0
	tag = stack(stkptr)
	value = v(stkptr)
	write(tmpstring,dfltfmt,iostat=ier) value
	if (ier .ne. 0) call oerror(n373, m1, 'Bad Format specification')
	nfield = lastblnk(tmpstring)+1

        if (ioutst + nfield - 1 .gt. 72) then
	   if (unit .lt. 0) then
	      call pwrite(istring, lastblnk(string))
	   else
	      write(unit,frmt2,err=900) string(1:ioutst)
	   endif
	   ioutst = 1
	   string = ' '
        endif

        if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	string(ioutst:ioutst+nfield-1) = tmpstring(1:nfield)
	ioutst = ioutst + nfield
	stkptr = stkptr + 1
c
      else
c
c       an array or string
c
        if (nsize .le. 0) then
c	   we aren't trying to complete an array
c
	   type=stack(stkptr)
           nsize=stack(stkptr+1)
           tag=stack(stkptr+2)
	endif
c
        if (type.eq.2) then
c
c       We have an array
c
	   value = c(tag)
	   write(tmpstring,dfltfmt,iostat=ier) value
	   if (ier .ne. 0) call oerror(n373, m1, 'Bad Format specification')
	   nfield = lastblnk(tmpstring)+1

           if (ioutst + nfield - 1 .gt. 72) then
	      if (unit .lt. 0) then
	         call pwrite(istring, lastblnk(string))
	      else
	         write(unit,frmt2,err=900) string(1:ioutst)
	      endif
	      ioutst = 1
	      string = ' '
           endif

           if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   string(ioutst:ioutst+nfield-1) = tmpstring(1:nfield)
	   ioutst = ioutst + nfield
	   tag = tag + 1
	   nsize = nsize - 1
	   if (nsize .eq. 0) stkptr = stkptr + 4

	else 
c	We have a string or literal constant
c
	   if (type .eq. 7) then
	     ssize = iwpr(nsize)
	   else
	     ssize = nsize
	   endif
	   nfield = 2*ssize+1

           if (ioutst + nfield - 1 .gt. 72) then
	      if (unit .lt. 0) then
	         call pwrite(istring, lastblnk(string))
	      else
	         write(unit,frmt2,err=900) string(1:ioutst)
	      endif
	      ioutst = 1
	      string = ' '
           endif

	   nsize = 0
	   stkptr = stkptr + 4
           if (ioutst + nfield - 1 .gt. numstring) 
     .		call oerror(n374, m1, 'End of record')
	   tmpstring = ' '
	   call copy( ssize, c(tag), itmpstring)
	   string(ioutst:ioutst+nfield-1) = tmpstring(1:nfield)
	   ioutst = ioutst + nfield
c
	endif
      endif
c
c
      goto 2400
c

900   call oerror(n374,m1,' ')

c
      end
c
c=======================================================================
c



