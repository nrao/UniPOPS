      subroutine amathparser (stringin, stringout)
C-------------------------------------------------------------------------------
C  @(#)amathparser.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Parses the input string STRINGIN and replaces Array-Math structures
c     with their equivalent POPS commands.
c
      character*(*) stringin, stringout
      character*160 stringtemp, sbegin, send, sexpand, s1, s2
      character*60 ckpak
      character*1 char
      integer*2 lastblnk, ilenin, i1, i3, i4, i5, iii, ilenbegin, 
     .          ilenend, ilentemp, ilenexpand, iat, charat, charopn,
     .		charclse, snglclose, snglopen, dbleclose, dbleopen,
     .		at, opn, clse, i
      integer*2 n1, n4, n9, n35
c
      include 'cio.inc'
      include 'smstuf.inc'
c
      equivalence (ckpak, kpak)
c
      data n1, n4, n9, n35 /1, 4, 9, 35/
c
      data charat/10/, charopn/11/, charclse/12/
c     ASCII decimal codes for what will be substituted fro @, {, and }
c	if such characters are found within a literal constant in the 
c	input string.
c
      ilenin = lastblnk(stringin)
c
      i1 = 0
      i3 = 0
      i4 = 1
      i5 = 1
      iii = 0
      sexpand = ' '
      s1 = ' '
      s2 = ' '
      send = ' '
      sbegin = ' '
      stringtemp = ' '
      stringout = ' '
      snglclose = 0
      dbleclose = 0
c
5     snglopen = index(stringin(snglclose+1:ilenin),'''') + snglclose
      if (snglopen .gt. snglclose) then
	snglclose =  index(stringin(snglopen+1:ilenin),'''') + snglopen
	if (snglclose .gt. snglopen) then
10	   at = index(stringin(snglopen+1:snglclose-1),'@') + snglopen
	   if (at .gt. snglopen) then
		stringin(at:at) = char(charat)
		goto 10
	   endif
20	   opn = index(stringin(snglopen+1:snglclose-1),'{') + snglopen
	   if (opn .gt. snglopen) then
		stringin(opn:opn) = char(charopn)
		goto 20
	   endif
30	   clse = index(stringin(snglopen+1:snglclose-1),'}') + snglopen
	   if (clse .gt. snglopen) then
		stringin(clse:clse) = char(charclse)
		goto 30
	   endif
	   goto 5
	endif
      endif
c     Finds all strings delineated by single quotes; between the quotes,
c	turms all occurances of @, {, and } to the characters whose
c	ASCII decimal representation is CHARAT, CHAROPN, and CHARCLSE
c
c     Now do the same for strings surrounded by double quotes
c
35    dbleopen = index(stringin(dbleclose+1:ilenin),'"') + dbleclose
      if (dbleopen .gt. dbleclose) then
	dbleclose =  index(stringin(dbleopen+1:ilenin),'"') + dbleopen
	if (dbleclose .gt. dbleopen) then
40	   at = index(stringin(dbleopen+1:dbleclose-1),'@') + dbleopen
	   if (at .gt. dbleopen) then
		stringin(at:at) = char(charat)
		goto 40
	   endif
50	   opn = index(stringin(dbleopen+1:dbleclose-1),'{') + dbleopen
	   if (opn .gt. dbleopen) then
		stringin(opn:opn) = char(charopn)
		goto 50
	   endif
60	   clse = index(stringin(dbleopen+1:dbleclose-1),'}') + dbleopen
	   if (clse .gt. dbleopen) then
		stringin(clse:clse) = char(charclse)
		goto 60
	   endif
	   goto 35
	endif
      endif  
c
      i1 = index(stringin(1:ilenin), '{')
c
      if (i1 .eq. ilenin) then
	ckpak = '{'
	stringout = stringin
	call oerror(n35,n1,'No closing }')
      endif
c     { was the last character in line.. a no-no
c
      if (i1 .eq. 0) then
	  i3 = index(stringin(1:ilenin), '}')
	  if (i3 .ne. 0) call oerror(n35, n1, 'No opening {')
c	  Can't have a } if you don't have  a {
c
	  stringout = stringin
	  goto 99
      endif
c     No array math to do so just return input string
c
      if (i1 .gt. 1) sbegin = stringin(1:i1-1)
c     SBEGIN = input string up to the {
c
      i3 = index(stringin(i1+1:ilenin),'}') + i1
      if (i3 .eq. i1) then
	  ckpak = '}'
	  stringout = stringin
	  call oerror(n35,n1,' ')
	  goto 99
      endif
c     Must have a }
c
      stringtemp = stringin(i1+1:i3-1)
      sexpand = ';amathon;for n_n = 1 to 16384;'
c
      if (i3 .ne. ilenin) send = stringin(i3+1:ilenin)
c     SEND = input string from } to end of line
c
      ilenbegin = lastblnk(sbegin) + 1
      ilenend = lastblnk(send) + 1
      ilentemp = lastblnk(stringtemp)
      ilenexpand = lastblnk(sexpand) + 1
c
31    iat = index(stringtemp(i5:ilentemp), '@') + i5 - 1
c     IAT = index of next @
c
      ilenexpand = lastblnk(sexpand) + 1
      if (iat .ne. i5 - 1) then
        sexpand = sexpand(1:ilenexpand)//stringtemp(i5:iat-1)//'(n_n)'
	i5 = iat + 1   
	if (iat .le. ilentemp) goto 31
      endif
c     Replace all @ with (n_n)
c
      ilenexpand = lastblnk(sexpand) + 1
      if (ilenexpand + ilentemp - i5 + 12 .gt. karlim) then
	   ckpak = '{}'
	   stringout = stringin
	   call oerror(n4,n1,'Array Math statement is too long ')
c          Make sure that the returned line doesn't exceed the maximum line
c          length.
c
      else
c
	   stringout = sbegin(1:ilenbegin) // sexpand(1:ilenexpand) // 
     1              stringtemp(i5:ilentemp) // ';end;amathoff' //
     2		    send(1:ilenend)
      endif
c
99    do 100 i = 1, lastblnk(stringout)
	if (stringout(i:i) .eq. char(charat)) stringout(i:i) = '@'
	if (stringout(i:i) .eq. char(charopn)) stringout(i:i) = '{'
	if (stringout(i:i) .eq. char(charclse)) stringout(i:i) = '}'
100	continue
c	Return @, {, and } into the output string
c
      return
      end
c
