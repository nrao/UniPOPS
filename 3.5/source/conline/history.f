      subroutine history(strng, ptr, print)
c
c     @(#)history.f	5.1 06/22/94
c
c     Does C-Shell ! history substitution using STRNG and HIST arrays
c
c     STRING = C*(*) String that contains history command
c     PTR = I*2 character position in STRING of ! chracater
c     PRINT = L*2 is true if the line is NOT to be executed but only
c	printed.
c
      character*(*) strng
      integer*2 ptr
      logical print
c
      character*320 sbjct, teststr, worddes, old, new, outbuff 
      integer*2 ilen, i1, irepeat, i0, istat, n27, m1, i3, numwords,
     .		word1, word2, ilenwd, iw1, iw2, i4, i5, maxword,
     .		lastblnk, i6, i, ilentst, ilenold, ilennew, word,
     .		ilenout
      logical global
c
      parameter (n27 = 27)
      parameter (m1 = -1)
      parameter (maxword = 400)
c
      integer*2 wend(maxword), wstart(maxword), wtype(maxword)
c
      include 'cio.inc'
c
      save old, ilenold, new, ilennew
c
      data old/' '/, ilenold/0/, new/' '/, ilennew/0/
c
      global = .false.
      print = .false.
      if (strng(ptr:ptr) .ne. '!') 
     .		call herror(n27, m1, 'Internal error')
c
      sbjct = strng(ptr+1:)
c
      if (sbjct(1:1) .eq. '*' .or. sbjct(1:1) .eq. '^' .or.
     .    sbjct(1:1) .eq. '$') sbjct = '-1:' // sbjct
      if (sbjct(1:1) .eq. ':') sbjct = '-1' // sbjct
c
      ilen = lastblnk(sbjct)
      if (ilen .eq. 0) call herror(n27, m1, 'No event specified')
c
c====================================================
c
c     First, parse event designator
c
      if (sbjct(1:1) .eq. '!') then
	i1 = 2
	irepeat = mod(inhist-2,maxhist) + 1
c	Takes care of !!
c-------
      else if (sbjct(1:1) .eq. '-') then
	i0 = 2
	i1 = 2
10	if ( lle(sbjct(i1:i1),'9') .and. lge(sbjct(i1:i1),'0') ) then
	    i1 = i1 + 1
	    if (i1 .le. ilen) goto 10
	endif
	read(sbjct(i0:i1-1),'(i9)', iostat=istat) irepeat
	if (istat .ne. 0 .or. irepeat .eq. 0) 
     .		call herror(n27, m1, 'Bad specified event')
	irepeat = inhist - abs(irepeat)
	if (irepeat .lt. inhist-maxhist+1 .or. irepeat .gt. inhist) 
     .		call herror(n27, m1, 'Event beyond limits of history list')
	irepeat = mod(irepeat-1,maxhist) + 1
c	Takes care of !-n
c-------
      else if ( lle(sbjct(1:1),'9') .and. lge(sbjct(1:1),'0') ) then
	i0 = 1
	i1 = 1
20 	if ( lle(sbjct(i1:i1),'9') .and. lge(sbjct(i1:i1),'0') ) then
	    i1 = i1 + 1
	    if (i1 .le. ilen) goto 20
	endif
	read(sbjct(i0:i1-1),'(i9)', iostat=istat) irepeat
	if (istat .ne. 0 .or. irepeat .eq. 0) 
     .		call herror(n27, m1, 'Bad specified event')
	if (irepeat .lt. inhist-maxhist+1 .or. irepeat .gt. inhist) 
     .		call herror(n27, m1, 'Event beyond limits of history list')
	irepeat = mod(irepeat-1,maxhist) + 1
c	Takes care of !n
c-------
      else if (sbjct(1:1) .eq. '?') then
	i0 = 2
	i1 = index(sbjct(i0:), '?') + i0 - 1
	if (i1 .le. i0-1) call herror(n27, m1, 'Missing "?"')
	if (i1 .eq. i0) call herror(n27, m1, 'Empty search string')
	old = sbjct(i0:i1-1)
	ilenold = i1 - i0
	i1 = i1 + 1
	do 40 i = inhist, max(1, inhist-maxhist+2), -1
	   irepeat = mod(i-2, maxhist) + 1
	   if (index(hist(irepeat), old(1:ilenold)) .ne. 0) goto 65
40	   continue
	call herror(n27, m1, 'Event not found')
c	Takes care of !?str, !?str?, and !?str:
c-------
      else if (sbjct(1:1) .ne. ' ') then
	i0 = 1
	i1 = 1
50	if (sbjct(i1:i1) .ne. ':' .and. sbjct(i1:i1) .ne. ' ') then
	   i1 = i1 + 1
	   if (i1 .le. ilen) goto 50
	endif
	if (i1 .eq. i0) call herror(n27, m1, 'No specified event')
	teststr = sbjct(i0:i1-1)
	ilentst = i1 - i0
	do 60 i = inhist, max(1, inhist-maxhist+2), -1
	   irepeat = mod(i-2, maxhist) + 1
	   if (hist(irepeat)(1:ilentst) .eq. teststr(1:ilentst)) goto 65
60	   continue
	call herror(n27, m1, 'Event not found')
c	Takes care of !str, !and !str:
c-------
      else
	call herror(n27, m1, 'Improper event syntax')
      endif
c
c====================================================
c
c     Do some housework
c
65    outbuff = hist(irepeat)
      ilenout = lastblnk(outbuff)
c
      if (sbjct(i1:i1) .eq. ' ') goto 300
c     No word designator or modifier so we can quit
c
      if (sbjct(i1:i1) .ne. ':') call herror(n27, m1, 'Missing :')
c
      call findwords(outbuff, maxword, numwords, wstart, wend, wtype)
c     Counts the number of 'words' in OUTBUFF
c
      if (sbjct(i1+1:i1+1) .eq. 'S' .or. 
     .    sbjct(i1+1:i1+1) .eq. 's' .or.
     .    sbjct(i1+1:i1+1) .eq. 'P' .or. 
     .    sbjct(i1+1:i1+1) .eq. 'p' .or.
     .    sbjct(i1+1:i1+1) .eq. 'G' .or. 
     .    sbjct(i1+1:i1+1) .eq. 'g') goto 200
c     Modifier -- no word descripto so we can skip word descriptor
c
c====================================================
c
c     Next, proccess word descriptor
c
      i1 = i1 + 1
      i0 = i1
80    if (sbjct(i1:i1) .ne. ':' .and. sbjct(i1:i1) .ne. ' ') then
	   i1 = i1 + 1
	   if (i1 .le. ilen) goto 80
      endif
      if (i1 .eq. i0) goto 200
      worddes = sbjct(i0:i1-1)
      word1 = 0
      word2 = 0
c     Find the word descriptor; watch out for a null descriptor (::)
c
      if (worddes(1:1) .eq. '-') worddes = '0' // worddes
c     A ':-n' is the same as a ':0-n'.
c
      ilenwd = lastblnk(worddes)
      if (ilenwd .eq. 0) call herror(n27, m1, 'No word specified')
c
      if (ilenwd .eq. 1 .and. worddes(1:1) .eq. '0') then
	   word1 = 1
	   word2 = 1
c	   Takes care of :0
c------
      else if (ilenwd .eq. 1 .and. worddes(1:1) .eq. '^')  then
	   word1 = 2
	   word2 = 2
c	   Takes care of :^
c------
      else if (ilenwd .eq. 1 .and. worddes(1:1) .eq. '*')  then
	   word1 = 2
	   word2 = numwords
	   if (word2 .lt. word1) then
		outbuff = ' '
	        ilenout = 1
		goto 300
	   endif
c	   Takes care of :*
c------
      else if (ilenwd .eq. 1 .and. worddes(1:1) .eq. '%')  then
	   if (ilenold .eq. 0) 
     .		call herror(n27, m1, 'No previous search')
	   iw1 = index(outbuff, old(1:ilenold))
	   do 85 word1 = 1, numwords
	     if (iw1 .ge. wstart(word1) .and. iw1 .le. wend(word1) ) goto 87
85	     continue
	   call herror(n27, m1, 'String not found in event')
87	   word2 = word1
c	   Takes care of :%
c------
      else if (ilenwd .eq. 1 .and. worddes(1:1) .eq. '$')  then
	   word1 = numwords
	   word2 = numwords
c	   Takes care of :$
c------
      else if ( lle(worddes(1:1),'9') .and. lge(worddes(1:1),'0') ) then
	   i0 = 1
	   i3 = i0
90 	   if ( lle(worddes(i3:i3),'9') .and. lge(worddes(i3:i3),'0') ) then
	       i3 = i3 + 1
	       if (i3 .le. ilenwd) goto 90
	   endif
	   read(worddes(i0:i3-1),'(i9)', iostat=istat) word1
	   if (istat .ne. 0) call herror(n27, m1, 'Bad word description')
	   word1 = word1 + 1
c	   Finds starting word number (i.e., :n)
c
	   if (i3 .eq. ilenwd .and. worddes(i3:i3) .eq. '*') then
		word2 = numwords
c		Takes care of :n*
c
	   else if (i3 .eq. ilenwd .and. worddes(i3:i3) .eq. '-') then
		word2 = numwords - 1
c		Takes care of :n-
c
	   else if (i3 .eq. ilenwd-1 .and. worddes(i3:i3+1) .eq. '-$') then
		word2 = numwords
c		Takes care of :n-$
c
	   else if (i3 .gt. ilenwd) then
		word2 = word1
c		Takes care of :n
c
	   else if (i3 .lt. ilenwd .and. worddes(i3:i3) .eq. '-') then
	      i0 = i3+1
	      i3 = i0
95 	      if ( lle(worddes(i3:i3),'9') .and. 
     .		   lge(worddes(i3:i3),'0') ) then
	          i3 = i3 + 1
	          if (i3 .le. ilenwd) goto 95
	      endif
	      if (i3 .eq. i0 .or. i3 .lt. ilenwd) 
     .			call herror(n27, m1, 'Bad word description')
	      read(worddes(i0:i3-1),'(i9)', iostat=istat) word2
	      if (istat .ne. 0) call herror(n27, m1, 'Bad word description')
	      word2 = word2 + 1
c	      Finds ending word number (i.e, the -m')
c
	   else
	      call herror(n27, m1, 'Bad word description')
	   endif
c	   Takes care of :n, :n-m, n*, n-, n-$
c------ 
      else
	   call herror(n27, m1, 'Bad word description')
      endif
c      
c====================================================
c
c     Now, parse the line for the desired words
c
      if (word2 .lt. word1 .or. word2 .gt. numwords) 
     .		call herror(n27, m1, 'Bad word description')
      iw1 = wstart(word1)
      iw2 = wend(word2)
      if (iw2 .lt. iw1) call herror(n27, m1, 'Not enough words')
      outbuff = outbuff(iw1:iw2)
      ilenout = iw2 - iw1 + 1
c
c==================================================================     
c
c     Next, the modifier
c
200   if (sbjct(i1:i1) .eq. ' ') goto 300
c     No modifier so we can quit
c
      if (sbjct(i1:i1) .ne. ':') call herror(n27, m1, 'Missing :')
      i1 = i1 + 1
c
210   if (sbjct(i1:i1) .eq. 'p' .or. sbjct(i1:i1) .eq. 'P') then
	print = .true.
	i1 = i1 + 1
	goto 200
c       Takes care of :p -- must parse the next :, if any
c----------
c
      else if (sbjct(i1:i1) .eq. 'g' .or. sbjct(i1:i1) .eq. 'G') then
	global = .true.
	i1 = i1 + 1
        if (sbjct(i1:i1) .ne. 's' .and. sbjct(i1:i1) .ne. 'S' .and.
     .      sbjct(i1:i1) .ne. '&') call herror(n27, m1, 'Bad modifier')
	goto 210
c       Takes care of :gs and :g& -- must parse the & and s next
c----------
c
      else if (sbjct(i1:i1) .eq. '&') then
	if (ilenold .eq. 0) call herror(n27, m1, 'No previous substitution')
	i1 = i1 + 1
c	Takes care of :g& or :& -- use existing old and new, if any
c----------
c
      else if (sbjct(i1:i1) .eq. 's' .or. sbjct(i1:i1) .eq. 'S') then
	if (sbjct(i1+1:i1+1) .ne. '/') call herror(n27, m1, 'Missing /')
	i3 = i1 + 2
	i4 = index(sbjct(i3:),'/') + i3 - 1
	if (i4 .eq. i3-1) call herror(n27, m1, 'Missing /')
	if (i4 .gt. i3) then
	   ilenold = i4 - i3
	   old = sbjct(i3:i4-1)
	else if (ilenold .eq. 0) then
	   call herror(n27, m1, 'No previous substitution')
	endif
c       If null 'old' string, go back and use the previous substitution, if any.
c	If not null, use the given string as 'old'
c	
	i5 = index(sbjct(i4+1:),'/') + i4
	if (i5 .eq. i4) call herror(n27, m1, 'Missing /')
	ilennew = i5 - i4 - 1
	if (ilennew .gt. 0) then
	    new = sbjct(i4+1:i5-1)
	    i0 = 1
250	    i3 = index(new(i0:), '&') + i0 - 1
	    if (i3 .ne. i0-1) then
		new = new(1:i3-1) // old(1:ilenold) // new(i3+1:)
		ilennew = ilennew - 1 + ilenold
		i0 = i3 + ilenold
		if (i0 .le. ilennew) goto 250
	    endif
c	    Substitute 'old' for '&' metacharacter
        endif 
        i1 = i5 + 1
c	Takes care of :s/old/new/
c----------
      else
	call herror(n27,m1, 'Bad modifier syntax')
      endif
c
c     We must now do the work of the preceeding & or s
c
      i0 = 1
260   i6 = index(outbuff(i0:), old(1:ilenold) ) + i0 - 1
      if (i6 .eq. i0 - 1 .and. i0 .eq. 1) 
     .		call herror(n27, m1, 'String not found')
c     Must have at least one substitution
c
      if (i6 .eq. i0 - 1) then
	global = .false.
	goto 200
      endif
c     We're done with the current substitution
c
      do 270 word = 1, numwords
	if (i6 .ge. wstart(word) .and. i6 .le. wend(word) ) goto 277
270	continue
	word = numwords
c       Find the word number in which the substitution will be mase
c
277   if (ilennew .gt. 0) then
	   outbuff = outbuff(1:i6-1) // new(1:ilennew) // outbuff(i6+ilenold:)
      else
	   outbuff = outbuff(1:i6-1) // outbuff(i6+ilenold:)
      endif
      ilenout = ilenout - ilenold + ilennew
c     Do the substitution -- be carefull of a null 'new'
c
      wend(word) = wend(word) - ilenold + ilennew
      do 280 i = word+1, numwords
	wstart(i) = wstart(i) - ilenold + ilennew
	wend(i) = wend(i) - ilenold + ilennew
280     continue
c     Bump wend and wstart pointers to compensate for changes to outbuff
c
c
      if (global .and. word .lt. numwords) then
	 i0 = wstart(word+1)
	 goto 260
c        Takes care of any desired global substitution.
      else
	 global = .false.
	 goto 200
c	 Now work on next modifier, if any.
      endif
c
c==================================================================     
c
c     Now for some finishing up work
c
300   if (sbjct(i1:i1) .ne. ' ') call herror(n27, m1, 'Bad syntax')
      if (global) call herror(n27, m1, 'Bad syntax')
c
      if (ilenout+ilen+ptr-i1 .gt. len(strng)) 
     .		call herror(n27, m1, 'Resulting string is too long')
c
      strng(ptr:) = outbuff(1:ilenout) // sbjct(i1:)
c
      return
      end
c
c--------------------------------------------------
c
      subroutine findwords(string, maxword, numword, wstart, 
     .			   wend, wtype)
c
c     Finds up to MAXWORDS 'words' in STRING and places the number
c     of words found in NUMWORD and the satrting and ending locations
c     of the words in arrays WSTART and WEND.
c     WTYPE(n) = 0 if common word, 1 if special word, 2 if string
c
      character*(*) string
      integer*2 wstart(*), wend(*), maxword, numword, wtype(*)
c
      integer*2 i, j, numsingle, numdouble, n120, m1, lastblnk, ilen
      logical pushcmp, special
c
      parameter (numsingle=18)
      parameter (numdouble=4)
      parameter (n120=120)
      parameter (m1=-1)
c
      character*1 singles(numsingle), c1
      character*2 doubles(numdouble)
c
      data singles/',',   '(',   ')',   '=',   '+',   '-',   '*',   '/',
     .             '>',   '<',   '~',   '|',   '&',   ';',   '?',   '{',
     .             '}',   '@'/
      data doubles/'**',   '~=',   '>=',   '<='/
c
      ilen = lastblnk(string)
      if (ilen .eq. 0) call herror(n120, m1, 'HISTORY: Internal error')
c
      do 1 j = 1, maxword
	wend(j) = 0
	wtype(j) = 0
	wstart(j) = 0
1	continue
      numword = 0
      special = .false.
c
      i = 1
5     if (i .gt. ilen) return
      if (string(i:i) .eq. ' ') then
	i = i + 1
	goto 5 
      endif
c     Skips any blanks in STRING
c
      if (.not. pushcmp(wstart, numword, maxword, i)) 
     .		call herror(n120, m1, 'HISTORY: Internal error')
c     Pushes start of word onto stack
c
      if (string(i:i) .eq. '''' .or. string(i:i) .eq. '"') then
c	   The word is a string; must find closing quote
c
	   c1 = string(i:i)
	   i = i + 1
15	   if (i .le. ilen) then
	     if (string(i:i) .ne. c1) then
		i = i + 1
		goto 15
	     else if(string(i:i) .eq. c1 .and. i+1 .lt. ilen .and. 
     .		     string(i+1:i+1) .eq. c1) then
		i = i + 2
		goto 15
	     endif
c	     Find final quote; skip double quotes
	  endif
	  wend(numword) = min(i,ilen)
	  wtype(numword) = 2
	  i = i + 1
	  goto 5
      endif
c
c 	Start looking for the end of the current word
c
20	if (i .gt. ilen) then
	   wend(numword) = ilen
	   wtype(numword) = 0
	   return
c
	else if (string(i:i) .eq. ' ') then
c	   We have a space
c
	   wend(numword) = i-1
	   wtype(numword) = 0
	   i = i + 1
	   goto 5
c------
c
	else 
c
c	  Look for double character words
c 
	  if (i .lt. ilen) then
	    do 50 j = 1, numdouble
	     if(string(i:i+1) .eq. doubles(j)) then
		if (special) then
		  wend(numword) = i+1
	   	  wtype(numword) = 1
		else
		  if (wstart(numword) .le. i-1) then
			wend(numword) = i-1
 	   	  	wtype(numword) = 1
     		  	if (.not. pushcmp(wstart, numword, maxword, i)) 
     .			     call herror(n120, m1, 'HISTORY: Internal error')
		  endif
		  wend(numword) = i+1
	   	  wtype(numword) = 1
		endif
		i = i + 2
		special = .true.
		goto 5
	     endif
50	     continue
	  endif
c
c	  Look for single character words
c 
	  do 60 j = 1, numsingle
	     if(string(i:i) .eq. singles(j)) then
		if (special) then
		  wend(numword) = i
 	   	  wtype(numword) = 1
		else
		  if (wstart(numword) .le. i-1) then
			wend(numword) = i-1
  	   	  	wtype(numword) = 1
     		        if (.not. pushcmp(wstart, numword, maxword, i)) 
     .			     call herror(n120, m1, 'HISTORY: Internal error')
		  endif
		  wend(numword) = i
 	   	  wtype(numword) = 1
		endif
		i = i + 1
		special = .true.
		goto 5
	     endif
60	     continue
c------
	endif
c
c	Go to next character in string and see if its the end of a word
	special = .false.
        i = i + 1
	goto 20
c
      end
c
c--------------------------------------
c
      subroutine qhistory(strng, ptr, print)
c
c     Does C-Shell ^ history substitution using STRNG and HIST arrays
c
c     STRING = C*(*) String that contains history command
c     PTR = I*2 character position in STRING of ^ chracater
c     PRINT = L*2 is true if the line is NOT to be executed but only
c	printed.
c
      character*(*) strng
      integer*2 ptr
      logical print
c
      integer*2 i1, i2, n27, m1, lastblnk
c
      parameter (n27 = 27)
      parameter (m1 = -1)
c
      if (strng(ptr:ptr) .ne. '^') 
     .		call herror(n27, m1, 'Internal error')
c
      i1 = index(strng(ptr+1:),'^') + ptr
      if (i1 .eq. 0) call herror(n27, m1, 'Missing ^')
      i2 = index(strng(i1+1:),'^') + i1
      if (i2 .eq. i1) call herror(n27, m1, 'Missing ^')
c
      if (lastblnk(strng)+4 .gt. len(strng)) 
     .		call herror(n27, m1, 'Resulting string is too long')
c
      strng(ptr:ptr) = '/'
      strng(i1:i1) = '/'
      strng(i2:i2) = '/'
      strng = strng(1:ptr-1) // '!!:s' // strng(ptr:)
c
      call history( strng, ptr, print)
c
      return
      end
c
c--------------------------------------
c
      subroutine histparser(kb)
c
c     Parses a line for history substitutions.
c
      integer*2 kb(*)
c
      include 'cio.inc'
c
      logical lhist, lprint, noexec
      character*1 c1
      integer*2 i0, ilen, lastblnk, iwpc
c
      i0 = 1
      lhist = .false.
      lprint = .false.
c
20    if ( cbuff(i0:i0) .eq. '!') then
	call history(cbuff, i0, noexec)
	lprint = noexec .or. lprint
	lhist = .true.
c
      else if (cbuff(i0:i0) .eq. '^') then
	call qhistory(cbuff, i0, noexec)
	lprint = noexec .or. lprint
	lhist = .true.
c
      else if (cbuff(i0:i0) .eq. '''' .or. cbuff(i0:i0) .eq. '"') then
c	   The word is a string; must find closing quote
c
	   c1 = cbuff(i0:i0)
	   i0 = i0 + 1
	   ilen = lastblnk(cbuff)
15	   if (i0 .le. ilen) then
	     if (cbuff(i0:i0) .ne. c1) then
		i0 = i0 + 1
		goto 15
	     else if(cbuff(i0:i0) .eq. c1 .and. i0+1 .lt. ilen .and. 
     .		     cbuff(i0+1:i0+1) .eq. c1) then
		i0 = i0 + 2
		goto 15
	     else
		i0 = i0 + 1
	     endif
c	     Find final quote; skip double quotes
	  endif
c
      else
	i0 = i0 + 1
      endif
c
      if (i0 .lt. lastblnk(cbuff)) goto 20
c
      if (lhist) then
	hist(mod(inhist-1,maxhist)+1) = cbuff
	nbytes = min(lastblnk(cbuff)+1, karlim)
	write(istdout,*) cbuff(1:nbytes)
	if (lprint) call togtline
	call unpack(iwpc(nbytes), jbuff, kb)
      endif
c
      return
      end
c
c-----------------------------------------
c
      subroutine herror(n1, n2, string)
c
c     History error trap
c
      integer*2 n1, n2
      character*(*) string
c
      include 'cio.inc'
c
      inhist = inhist - 1
      call oerror(n1, n2, string)
c
      return
      end
c
