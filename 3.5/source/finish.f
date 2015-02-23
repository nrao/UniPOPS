      integer*2 function finish(k, l, runorcmp)
c
c     @(#)finish.f	5.1  06/22/94
c
c     Processes and relinks IF, THEN, ELSE, and WHILES in procedures.
c     Returns 0 if all goes well, else a value reflecting what went wrong.
c
c     K = Linked list that is to be relinked
c     L = Index in K where procedure starts
c     RUNORCMP = true if running an input line else must be compiling a line.
c
c     Returns a flag telling what went wrong.  
c		0 = nothing went wrong
c		1 = Too many IF's, FOR's, etc for processing.
c		2 = END, THEN, or ELSE encounterd without a FOR, IF, or WHILE
c			precceding it.
c		3 = IF, FOR, or WHILE without a closing END
c
      integer*2 k(*), l
      logical runorcmp
c
      integer*2 nt, ll, lll, it, reason, tstack(512), maxstck
      logical pushcmp, popcmp
c
      include 'tags.inc'
c
      parameter (MAXSTCK = 512)
c
      NT=0
      finish = 0
c
      ll = l
c     LL = index to first/next program chunk; check whether it is
c		the last chunk
c
20	if (ll .ne. 0) then
c	    It is NOT the last program chunk
c
	    lll = ll+2
c	    LLL = index to first/next command in current program chunk
c
30	    if (k(lll) .eq. iomega) then
		ll = k(ll)
		goto 20
	    endif
c	    You have come to the end of the last command in the
c	    current program chunk.  Go on to the next program chunk
c
            IF (K(lll).eq.iifthen .or. K(lll).eq.iwhile) then
c
	        if (.not. pushcmp(tstack, nt, maxstck, lll) .or. 
     .		    .not. pushcmp (TSTACK, NT, maxstck, K(lll))) then
			finish = 1
			return
		endif
c
            else IF (K(lll).eq.iifelse .or. K(lll).eq.iforend .or.
     .			K(lll).eq.idumend) then
c
	 	if (.not. popcmp (TSTACK, NT, REASON) .or.
     .		    .not. popcmp (TSTACK, NT, IT)) then
			finish = 2
			return
		endif
c
	        IF (REASON.EQ.iifelse .OR. REASON.EQ.iifthen .AND.
     .			K(lll).EQ.iforend) K(lll) = idumend
c		If end operator of an if stmt change to	opcode 96 (a no-op)
c		which won't hurt the editor.
c
c
	        IF (IT.ne .0) then
	          K(IT-2)=-lll-1
                  IF (k(ll) .LE. 0) then
			if (runorcmp) then
			   k(it-1) = -1
			else
			   K(IT-1) = -K(3)
			endif
		  else
	         	K(IT-1)=-k(ll)
		  endif
	          IF (K(lll).EQ.iifelse) then
	       	    if (.not. pushcmp(tstack, nt, maxstck, lll) .or.
     .			.not. pushcmp (TSTACK, NT, maxstck, K(lll))) then
				finish = 1
				return
		    endif
	          endif
	        endif
C
            else IF (K(lll).eq.iloopfor) then
c
	        if (.not. pushcmp(tstack, nt, maxstck, i0) .or.
     .		    .not. pushcmp (TSTACK, NT, maxstck, K(lll))) then
			finish = 1
			return
		endif
c
     	    endif
c
	    lll = lll + 1
c
	    goto 30
c	    Advance to the next command in the current program chunk
c
      endif
c
      if (nt .ne. 0) finish = 3
c
      return
c
      end
c
