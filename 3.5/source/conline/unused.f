      integer function unused (k, start, tag, names, numnames)
c
c     @(#)unused.f	5.1 06/22/94
c
c     Goes through the K array, starting at START, looking for procedures
c     that make use of the indicated TAG.  Will search for NUMNAMES+1 procedures
c     that use that tag.  If NUMNAMES <= 0, then only the 1st procerdure will
c     be looked for and, if one exists, UNUSED will return a 1 else a 0.  
c     If NUMNAMES > 0 then all procedures will be searched and UNUSED will return
c     the number of procedures found.  The routine also stuffs the names off the
c     procedures found into the array NAMES.  Only NUMNAMES entries will be used
c     in NAMES.
c
      integer*2 tag, numnames, k(*), start
      integer*2 n5, fshort
      character*(*) names(*) 
c     
      integer*2 l, type, ll, lll, num, length, iname(5)
      character*10 name
c
      include 'tags.inc'
c
      equivalence (iname, name)
c
      data n5 /5/
c
      num = 0
c     num = Number of procedures found that use the tag
c
      l = k(start)
c
10	if (l .ne. 0) then
c	   Have we exhausted the list?  If not, check if the item is a 
c	   procedure
c
	   length = k(l+1)/16
	   type = k(l+1) - 16*length
c
	   if (type .eq. 3 .and. k(l+2) .ne. tag) then
c		It is a procedure
c		Its safe to skip it if tag points to the first chunk
c		i.e. we can scratch procedures that reference themselves
c
		ll = k(l+2)
c		LL = index to first/next program chunk; check whether it is
c		the last chunk; remember who this is.
c
20		if (k(ll) .ne. 0) then
c		    It is NOT the last program chunk
c
		    lll = ll+2
c		    LLL = index to first/next command in current program chunk
c
30		    if (k(lll) .eq. iomega) then
			ll = k(ll)
			goto 20
		    endif
c		    You have come to the end of the last command in the
c		    current program chunk.  Go on to the next program chunk
c
		    if (k(lll) .eq. tag) then
c
c		        The current command has the desired tag.
c
		   	num = num + 1
			if (num .le. numnames) then
			   name = ' '
			   call copy(fshort(min(length,n5)),k(l+4),iname)
			   names(num) = name
c			   Store away the proc's name, if room left over.
c
			else if (numnames .le. 0) then
			   unused = num
			   return
c			   We have been asked to give up the search after the
c			   1st proc is found that uses TAG.
c
			endif
c
			l = k(l)
			goto 10
c			No use going thru rest of proc.  Skip to next item in K.
c
		    endif
c
		    lll = lll + 1
		    goto 30
c		    Advance to the next command in the current program chunk
c
		endif
c
c		Finished with the last program chunk in this procedure; 
c		go on from L to find next item in list.  
c
	   endif

	   l = k(l)
	   goto 10
c	   Advance to next item in list
c
	endif
c
      unused = num
c     You have gone through the complete list.
c
      return
      end
c
      integer function unalias (k, start, tag, names, numnames)
c
c     Goes through the K array, starting at START, looking for ALIASes
c     that make use of the indicated TAG.  Will search for NUMNAMES+1 ALIASes
c     that use that tag.  If NUMNAMES <= 0, then only the 1st ALIAS will
c     be looked for and, if one exists, UNUSED will return a 1 else a 0.  
c     If NUMNAMES > 0 then all ALIASes will be searched and UNUSED will return
c     the number of ALIASes found.  The routine also stuffs the names off the
c     procedures found into the array NAMES.  Only NUMNAMES entries will be used
c     in NAMES.
c
      integer*2 tag, numnames, k(*), start
      integer*2 n5, fshort
      character*(*) names(*) 
c     
      integer*2 l, type, num, length, iname(5)
      character*10 name
c
      include 'tags.inc'
c
      equivalence (iname, name)
c
      data n5 /5/
c
      num = 0
c     num = Number of aliases found that use the tag
c
      l = k(start)
c
10	if (l .ne. 0) then
c	   Have we exhausted the list?  If not, check if the item is a 
c	   procedure
c
	   length = k(l+1)/16
	   type = k(l+1) - 16*length
c
	   if (type .eq. 6 .and. k(l+2) .eq. tag) then
c		It is a alias that has the desired tag.
c
		num = num + 1
		if (num .le. numnames) then
		   name = ' '
		   call copy(fshort(min(length,n5)),k(l+4),iname)
		   names(num) = name
c		   Store away the proc's name, if room left over.
c
		else if (numnames .le. 0) then
		   unalias = num
		   return
c		   We have been asked to give up the search after the
c		   1st alias is found that uses TAG.
c
		endif
c
	   endif
c
	   l = k(l)
	   goto 10
c	   Advance to next item in list
c
	endif
c
      unalias = num
c     You have gone through the complete list.
c
      return
      end
c
