      subroutine relink(old,new,subflags,numtags)
c
c     @(#)relink.f	5.1  06/22/94
c
c     Goes through the K array looking for procedures,  When it finds
c     one it checks whether or not that procedure uses the OLD tag and,
c     if so, it replaces the tag with NEW tag.  Does this for NUMTAGS
c     entries in OLD and NEW. SUBFLAGS indicates that the tag is to 
c     an array sub linked list, the subs operator must come next in the
c     procedure chunk.
c
      integer*2 old(*), new(*), subflags(*), numtags 
c     
      integer*2 l, type, ll, lll, i
c
      include 'core.inc'
      include 'tags.inc'
c
      if (numtags .le. 0) return
c     No tags to alter so do nothing.
c
      l = 1
c
10    l = k(l)
c	Advance to next item in list
c
      if (l .ne. 0) then
c	   Have we exhausted the list?  If not, check if the item is a 
c	   procedure
c
         type = k(l+1) - 16*(k(l+1)/16)
         if (type .eq. 3) then
c		It is a procudure
c
            ll = k(l+2)
c		LL = index to first/next program chunk; check whether it is
c		the last chunk
c
 20         if (k(ll) .ne. 0) then
c		    It is NOT the last program chunk
c
               lll = ll+2
c		    LLL = index to first/next command in current program chunk
c
 30            if (k(lll) .eq. iomega) then
                  ll = k(ll)
                  goto 20
               endif
c		    You have come to the end of the last command in the
c		    current program chunk.  Go on to the next program chunk
c
               do 35 i = 1, numtags
                  if (k(lll) .eq. old(i)) then
                     if (subflags(i) .eq. 1 .and.
     .                     k(lll+1) .eq. isubs) then
                        k(lll) = new(i)
                        goto 38
                     else if (subflags(i) .ne. 1 .and. 
     .                          k(lll+1) .ne. isubs) then
                        k(lll) = new(i)
                        goto 38
                     endif
                  endif
 35            continue
c		        If the current command has a desired TAG, change it.
c			Make sure that if its an array type, that it is
c			followed by the subs operator and that if it isn't
c			an array type that it isn't followed by the subs op.
c			Make sure that you don't change the value of the command
c			more than once.
c
 38            lll = lll + 1
               goto 30
c		    Advance to the next command in the current program chunk
c
            endif
c
c		Finished with the last program chunk in this procedure; 
c		go on from L to find next item in list.  
c
         else if (type .eq. 6) then
c		It is an alias
c
            lll = l + 2
            do 45 i = 1, numtags
               if (k(lll) .eq. old(i) .and. subflags(i) .ne. 1) then
                  k(lll) = new(i)
                  goto 10
               endif
 45         continue
c		   If the current Alias has a desired TAG, change it
c
         endif
         goto 10
      endif
c
      return
      end
