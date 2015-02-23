      subroutine compress
c
c     @(#)compress.f	5.1 06/22/94
c
c     Compresses the K, KXORG and LISTF arrays
c
      integer*2 newlistf(32766), newk(32766), newkxorg, length, top2,
     .		type, tag, nk0, nt, newl, newkxl, newtag, isize,  top1,
     .		ndim, nsub, i, oldstack(4096), newstack(4096), maxstck, 
     .		newlistl, ii, iwpc, newnk0, ibyte, llocat, irealp, listl,
     .		finish, irtn, unused, iwpr, newkx(32766), maxlnk,
     .		llast, newllast, lstart, substack(4096), top3
      integer*2 n0, n1, n2, n132, m1, fshort
      logical debug, pushcmp
      character*10 names
      real*4 newc(32766)
c
      include 'core.inc'
      include 'cio.inc'
      include 'lsf.inc'
      include 'stk.inc'
c
      equivalence (newk(1), newc(1)), (newk(8),newkxorg), 
     .		  (newkx(1), newc(16384))
c
      data n0, n1, n2, n132, m1 /0, 1, 2, 132, -1/
c
      parameter (MAXSTCK = 4096)
      parameter (MAXLNK = 32766)
c
      debug = idebug .gt. 0
      top1 = 0
      top2 = 0
      top3 = 0
c     TOP1,2,3 = number of tags that might have to be relinked.
c
      call copy(k(7), k, newk)
      newk(3) = k(7)
      newk(10) = 205
c
      l = 1
2       lstart = l
	l = k(l)
        if (l .gt. 0 .and. l .lt. k(7)) then
		newk(9) = l
		goto 2
	endif
c     Copy over builtin section of K array; reset 'next-free-index' pointers.
c     LFIRST = pointer to first user-defined item in linked list.
c
      call copy(kx(7),kx,newkx)
      newkx(3) = kx(7)
c     Copy over values of builtin adverbs; reset 'next-free-index' pointer.
c
      call copy(listf(7), listf, newlistf)
      newlistf(3) = listf(7)
c     Copy over builtin section of LISTF array; reset 'next-free-index' pointer.
c
      write(istdout,*) 'Compressing Adverbs and Aliases'
c
      l = lstart
c     K(LSTART) = Start of user-defined section of K array's linked list
c
10      l = k(l)
        if (l .ne. 0) then
c
	   length = k(l+1)/16
	   type = k(l+1) - 16*length
	   tag = k(l+2)
	   nk0 = l+3
	   nt = length + 4
c	   LENGTH = number of chars in name of proc/adverb
c	   TYPE = type of operator/operand
c	   TAG = tag of operator/operand
c	   NK0 = link to sub-link-list (if it exists)
c	   NT = Number of I*2 words this entry should take up
c
	   if (type .eq. 1 .or. type .eq. 2 .or. type .eq. 7 .or.
     .		    type .eq. 8) then
c
c		We have found a user-defined ADVERB
c
	        if (debug) write(istderr,11) 'Adverb:',l,(k(l+i-1), i = 1, nt)
c
		newl = llocat(nt,newk,newk(9))
c		Find an empty location in the NEWK array
c
		newkxl = 1
		newtag = irealp(fshort(llocat(n2,newkx,newkxl))) + 
     .                                 + irealp(newkxorg) - n1
c		Find an empty location in the adverb section of the NEWKX array
c
		newk(newl+1) = type + 16*length
		newk(newl+2) = newtag
		newk(newl+3) = 0
		call copy(length,k(l+4),newk(newl+4))
c		Copy over the type, length, name from old K to NEWK; use the 
c		new TAG of the location of the new adverb value.
c
		if (type .eq. 2 .or. type .eq. 7) then
c
c		   You have a string or an array and must create the 
c		   sub-link-list entry.
c
		   nk0 = k(nk0)
		   isize = k(nk0)
		   ndim = k(nk0+1)
		   nsub = 2*ndim+2
c		   NK0 = Location of first element in sub-link-list
c		   ISIZE = total number of R*4 elements in array or string
c		   NDIM = Number of dimensions
c		   NSUB = Number of I*2 this sub-link-list should take up.
c
	           if (debug) write(istderr,22) '    In:',nk0,
     .					(k(nk0+i-1), i = 1, nsub)
c
		   newnk0 = newl + 3
		   newnk0 = llocat(nsub, newk, newnk0)
c		   Create space for array dimension sub-link-list in NEWK
c		   NEWNK0 = Location in NEWK where the link to sub-link-list
c			will be found.
c
		   if (nk0 .ne. newnk0) then
     		      if (.not. pushcmp(oldstack,top1,maxstck,fshort(-nk0)) .or.
     .		       .not. pushcmp(newstack,top2,maxstck,fshort(-newnk0)))
     .			call oerror(n132, m1, 'Too many ADVERBS to compress')
                      if (type .eq. 2) then
			if (.not. pushcmp(substack,top3,maxstck,n1))
     .			 call oerror(n132, m1, 'Too many ADVERBS to compress')
                      else 
			if (.not. pushcmp(substack,top3,maxstck,n0))
     .			 call oerror(n132, m1, 'Too many ADVERBS to compress')
                      endif
		   endif
c		   Store away NK0's (equiv. to: K(LOCSYM+2) ) for relinking 
c		   later on references to array subscripts.
c
		   if (isize .gt. 1) irtn = 
     .			llocat(fshort(iwpr(isize)-n2),newkx,newkxl)
c		   Reserve more space in adverb value section of NEWK (the
c		   KXORG list); two I*2 words were already reserved by 
c		   above call to LLOCAT that found NEWTAG.
c
		   call copy(nsub,k(nk0),newk(newnk0))
c		   Copy over dimensions from K to NEWK
c
	           if (debug) write(istderr,22) '   Out:',newnk0,
     .					(newk(newnk0+i-1), i = 1, nsub)
c
		else if (type .eq. 1 .or. type .eq. 8) then
c
c		   You don't have a string or array so the size is 1 R*4 word
c
		   isize = 1
c
c
		endif
c
	        if (debug) write(istderr,11) '   Out:',newl,
     .					(newk(newl+i-1), i = 1, nt)
		if (debug) write(istderr,17) '               Values'
17		format(1x,a)
c
		do 15 i = 1, isize
		   newc(newtag+i-1) = c(tag+i-1)
		   if (debug .and. type .ne. 7) then
			write(istderr,12) i, newc(newtag+i-1), c(tag+i-1)
12			format(1x,i6,2x,1p2g14.7)
		   else if (debug .and. type .eq. 7) then
			write(istderr,13) i, newc(newtag+i-1), c(tag+i-1)
13			format(1x,i6,2x,a4,2x,a4)
		   endif
15		   continue
c		Copy over old adverb's value from old C to NEWC
c
		if (tag .ne. newtag) then
     		   if (.not. pushcmp(oldstack,top1,maxstck,fshort(-tag)) .or.
     .		       .not. pushcmp(newstack,top2,maxstck,fshort(-newtag)) .or.
     .		       .not. pushcmp(substack,top3,maxstck,n0))
     .			call oerror(n132, m1, 'Too many ADVERBS to compress')
		endif
c		Store away tags for relinking later on
c
	  else if (type .eq. 6) then
c
	        if (debug) write(istderr,11) 'Alias:',l,(k(l+i-1), i = 1, nt)
c
		newl = llocat(nt,newk,newk(9))
c		Find an empty location in the NEWK array
c
		newk(newl+1) = type + 16*length
		newk(newl+2) = k(l+2)
		newk(newl+3) = 0
		call copy(length,k(l+4),newk(newl+4))
c		Now, copy over the type, length, name from old K to NEWK.
c
	        if (debug) write(istderr,11) '  Out:',newl,
     .					(newk(newl+i-1), i = 1, nt)
c
c		NOTE:  No need to store away tags for later relinking!
c
	   else if (type .ne. 3) then
c
c		You shouldn't find anything but Adverbs, Aliases, and Proc's 
c		in the user-defined section of the K array.
c
	        if (debug) write(istderr,11) '    :',l, (k(l+i-1), i = 1, nt)
11		format(1x,a,5i7,(1x,5a2))
		call oerror(n132,m1, 
     .		      'Found something where it doesn''t belong')
c
	   endif
c
	   goto 10
c
   	endif
c
      write(istdout,*) 'Compressing Constants'
c
      l = 4
c     K(4) = Start of K array's linked list of permanent constants
c
30      l = k(l)
        if (l .ne. 0) then
c
	   if (l .lt. k(7)) goto 30
c 	   We can skip over the builtin-stuff
c
	   length = k(l+1)/16
	   type = k(l+1) - 16*length
	   tag = irealp(l) + 1
	   nt = length + 2
c	   LENGTH = number of chars in name of proc/adverb
c	   TYPE = type of operator/operand
c	   TAG = tag of operator/operand
c	   NT = Number of I*2 words this entry should take up
c
	   if (type .ne. 11 .and. type .ne. 14) then
c
c		You shouldn't find anything but literals and real constants
c		in this linked-list.
c
	        if (debug) write(istderr,11) '     ',l, (k(l+i-1), i = 1, nt)
		call oerror(n132,m1, 
     .		      'Found something besides a real/literal constant')
c
	   else 
c
c		We have found a literal/real
c
	        if (debug .and. type .eq. 11) write(istderr,31) 
     .			'   Real Constant:',l,k(l), k(l+1), c(tag)
31		format(1x,a,3i7,1pg14.7)		
	        if (debug .and. type .eq. 14) write(istderr,32) 
     .			'Literal Constant:',l,(k(l+i-1), i = 1, nt)
32		format(1x,a,3i7,(1x,30a2))		
c
		if (unused(k, n1, fshort(-tag), names, n0) .eq. 0) then
		   if (debug) write(istderr,17) 'Constant never used --- skipping'
		   goto 30
		endif
c		Make sure the constant is used by a procedure before you try
c		to move it into NEWK.
c 
		newl = llocat(nt,newk,newk(10))
c		Find an empty location in the NEWK array
c
		newtag = irealp(newl) + 1
c
		newk(newl+1) = type + 16*length
		call copy(length,k(l+2),newk(newl+2))
c		Copy over the type, length, values from old K to NEWK.
c
	        if (debug .and. type .eq. 11) write(istderr,31) 
     .		   '             Out:',newl,newk(newl),newk(newl+1),newc(newtag)
	        if (debug .and. type .eq. 14) write(istderr,32) 
     .		   '             Out:',newl,(newk(newl+i-1), i = 1, nt)
c
		if (tag .ne. newtag) then
     		   if (.not. pushcmp(oldstack,top1,maxstck,fshort(-tag)) .or.
     .		       .not. pushcmp(newstack,top2,maxstck,fshort(-newtag)) .or.
     .		       .not. pushcmp(substack,top3,maxstck,n0))
     .			call oerror(n132, m1, 'Too many CONSTANTS to compress')
		endif
c		Store away tags for relinking later on
c
	   endif
c
	   goto 30
c
   	endif
c
      write(istdout,*) 'Compressing Procedures'
c
      l = lstart
c     K(LSTART) = Start of K array's linked list
c
20      l = k(l)
        if (l .ne. 0) then
c
	   length = k(l+1)/16
	   type = k(l+1) - 16*length
	   tag = k(l+2)
	   nk0 = l+2
           llast = l+3
	   nt = length + 4
c	   LENGTH = number of chars in name of proc/adverb
c	   TYPE = type of operator/operand
c	   TAG = tag of operator/operand
c	   NK0 = link to sub-link-list (if it exists)
c	   NT = Number of I*2 words this entry should take up
c          LLAST = Location of next entry in local adverb linked-list
c
	   if (type .eq. 3) then
c
	        if (debug) write(istderr,11) 'Proc:',l,(k(l+i-1), i = 1, nt)
c
		newl = llocat(nt,newk,newk(9))
c		Find an empty location in the NEWK array
c
		newnk0 = newl + 2
	        newllast = newl + 3
c		NEWNK0 = Location in NEWK where the link to next program chunk
c			will be found.
c		NEWLCLLAST = Location where new local adverb list will be start.
c
		newk(newl+1) = type + 16*length
		newk(newl+2) = 0
		newk(newl+3) = 0
		call copy(length,k(l+4),newk(newl+4))
c		Now, copy over the type, length, name from old K to NEWK.
c
21		nk0 = k(nk0)
	  	if (nk0 .ne. 0) then
		  listl = k(nk0+1)
c		  LISTL = Location in LISTF array where procedure text begins
c
		  i = nk0 + 2
25		  if (k(i) .ne. 1) then
		     i = i + 1
		     goto 25
		  endif
		  nsub = i - nk0 + 1
c		  NSUB = Number of I*2 words that this sub-link-list should take up.
c
	          if (debug) write(istderr,22) '  In:',nk0,
     .				(k(nk0+i-1), i = 1, nsub)
22		  format(1x,a,(1x,10i7))
c
		  newnk0 = llocat(nsub, newk, newnk0)
c		  Create space for next program chunk in sub-link-list in NEWK
c
		  call copy(nsub,k(nk0),newk(newnk0))
c		  Copy over program chunk from K to NEWK
c
		  newk(newnk0) = 0
		  newk(newnk0+1) = 0
c		  Zero out link to next program chunk -- will be reset when
c		  next program chunk is handled.  Also reset pointer to LISTF
c		  array -- will fill in in the next stage.
c
		  if ( listl .ne. 0) then
c		     We have to move over the LISTF entry, if one exits.
c
		     ibyte = listf(listl)
		     ii = iwpc(ibyte) + 1
c		     IBYTE = Number of chars in procedure listing
c		     II = Number of I*2 words to hold procedure listing.
c
		     if (debug) write(istderr,23) '  In:',listl,
     .					(listf(listl+i-1),i=1,ii)
23		     format(1x,a,2i7,(1x,36a2))
c
		     newlistl = 1
		     newk(newnk0+1) = llocat(ii, newlistf, newlistl)
c		     Reserve space in LISTF array; update pointer to LISTF in NEWK
c		     array.
c
		     call copy(ii, listf(listl), newlistf(newlistl) )
c		     Copy over contents of LISTF to NEWLISTF
c
		     if (debug) write(istderr,23) ' Out:',newlistl,
     .					(newlistf(newlistl+i-1),i=1,ii)
		  endif
c
c		  We are finished with this program chunk so we can go on to 
c		  the next.
c
	          if (debug) write(istderr,22) ' Out:',newnk0,
     .				(newk(newnk0+i-1), i = 1, nsub)
c
		  goto 21
c
		endif
c
c		Now we can handle the local list of adverbs
c
110             llast = k(llast)
        	if (llast .ne. 0) then
	           length = k(llast+1)/16
	   	   type = k(llast+1) - 16*length
	   	   tag = k(llast+2)
	   	   nk0 = llast+3
	   	   nt = length + 4
	           if (type .eq. 1 .or. type .eq. 2 .or. type .eq. 7 .or.
     .		       type .eq. 8) then
	               if (debug) write(istderr,11) 'Loc Adverb:',
     .				llast,(k(llast+i-1), i = 1, nt)
		       newllast = llocat(nt,newk,newllast)
		       newkxl = 1
		       newtag = irealp(fshort(llocat(n2,newkx,newkxl))) + 
     .                                 + irealp(newkxorg) - n1
		       newk(newllast+1) = type + 16*length
		       newk(newllast+2) = newtag
		       newk(newllast+3) = 0
		       call copy(length,k(llast+4),newk(newllast+4))
		       if (type .eq. 2 .or. type .eq. 7) then
		   	  nk0 = k(nk0)
		   	  isize = k(nk0)
		   	  ndim = k(nk0+1)
		   	  nsub = 2*ndim+2
	           	  if (debug) write(istderr,22) '        In:',nk0,
     .					(k(nk0+i-1), i = 1, nsub)
		   	  newnk0 = newllast + 3
		   	  newnk0 = llocat(nsub, newk, newnk0)
		          if (nk0 .ne. newnk0) then
     		            if (.not. pushcmp(oldstack, top1, maxstck,
     .			  	fshort(-nk0)) .or. .not. pushcmp(newstack,
     .				top2, maxstck, fshort(-newnk0)) .or. .not.
     .				pushcmp(substack, top3, maxstck, n0))
     .			       call oerror(n132, m1, 
     .					'Too many ADVERBS to compress')
		          endif
		          if (isize .gt. 1) irtn = 
     .			       llocat(fshort(iwpr(isize)-n2),newkx,newkxl)
		          call copy(nsub,k(nk0),newk(newnk0))
	                  if (debug) write(istderr,22) '       Out:',newnk0,
     .					(newk(newnk0+i-1), i = 1, nsub)
		       else if (type .eq. 1 .or. type .eq. 8) then
		          isize = 1
		       endif
	               if (debug) write(istderr,11) '       Out:',newllast,
     .					(newk(newllast+i-1), i = 1, nt)
		       if (debug) write(istderr,17) '               Values'
		       do 115 i = 1, isize
		          newc(newtag+i-1) = c(tag+i-1)
		          if (debug .and. type .ne. 7) then
			       write(istderr,12) i, newc(newtag+i-1), c(tag+i-1)
		          else if (debug .and. type .eq. 7) then
			       write(istderr,13) i, newc(newtag+i-1), c(tag+i-1)
		          endif
115		          continue
		       if (tag .ne. newtag) then
     		          if (.not. pushcmp(oldstack, top1, maxstck,
     .			       fshort(-tag)) .or. .not. pushcmp(newstack,
     .			       top2, maxstck, fshort(-newtag)) .or. .not.
     .                         pushcmp(substack, top3, maxstck, n0))
     .			       call oerror(n132, m1, 
     .				    'Too many ADVERBS to compress')
		       endif
c
	 	  else if (type .eq. 6) then
	        	if (debug) write(istderr,11) 'Loc Alias:',
     .			          llast,(k(llast+i-1), i = 1, nt)
			newllast = llocat(nt,newk,newllast)
			newk(newllast+1) = type + 16*length
			newk(newllast+2) = k(llast+2)
			newk(newllast+3) = 0
			call copy(length,k(llast+4),newk(newllast+4))
	        	if (debug) write(istderr,11) '      Out:',newllast,
     .					(newk(newllast+i-1), i = 1, nt)
	  	  else 
	        	if (debug) write(istderr,11) '    :',
     .			        llast, (k(llast+i-1), i = 1, nt)
			call oerror(n132,m1, 
     .		           'Found something where it doesn''t belong')
	          endif
	          goto 110
   		endif
c
c		We are now done with this procedure
c
	        if (debug) write(istderr,11) ' Out:',newl,
     .					(newk(newl+i-1), i = 1, nt)
c
		newtag = newk(newl+2)
	 	tag = k(l+2)
c
		if (tag .ne. newtag) then
     		   if (.not. pushcmp(oldstack, top1, maxstck, tag) .or.
     .		       .not. pushcmp(newstack, top2, maxstck, newtag) .or.
     .		       .not. pushcmp(substack, top3, maxstck, n0))
     .			call oerror(n132, m1, 'Too many PROCEDURES to compress')
		endif
c		Store away tags for relinking later on
c
	   endif
c
	   goto 20
c
   	endif
c
      write(istdout,*) 'Relinking ADVERBS, ALIASES, and PROCEDURES'
c
      call fill(maxlnk, n0, k)
      call fill(maxlnk, n0, listf)
      call fill(maxlnk, n0, kx)
c     Zero out old arrays.
c
      call copy(newk(3), newk, k)
c     Copy back NEWK array on top of K array
c
      call copy(newkx(3),newkx,kx)
c     Copy back variable values in NEWK array on top of K array
c
      call copy(newlistf(3), newlistf, listf)
c     Copy back LISTF array
c
      if (top1 .gt. 0) call relink(oldstack, newstack, substack, top1)
c     Now, do the relinking of old proc/adverb tags with new ones; do for
c     all adverbs and procs.
c
      l = lstart
40      l = k(l)
        if (l .ne. 0) then
	   length = k(l+1)/16
	   type = k(l+1) - 16*length
	   if (type .eq. 3) irtn = finish(k, k(l+2), .false.)
c	   We can ignore any FOR/IF/THEN/ELSE/WHILE errors (i.e., the value
c	   of IRTN) since the erorrs existed in the procedure prior to
c	   compressing.  
c
	   goto 40
	endif
c
      return
c
      end
c
