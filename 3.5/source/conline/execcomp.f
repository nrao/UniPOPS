      integer*2 function execcomp(string)
c-------------------------------------------------------------------------
c     @(#)execcomp.f	5.1 06/22/94
c-------------------------------------------------------------------------
c     compiles string, copies the result to a safe place in
c     K (runktag) and returns that location for use by execintp
c
      integer*2 lastblnk, ilen, iwpc, ktag, nt, finish, i
      integer*2 n1, n17, n21, np
      character string*(*)
c
      include 'smstuf.inc'
      include 'core.inc'
      include 'stk.inc'
      include 'cio.inc'
c
      data n1, n17, n21 /1, 17, 21/
c-------------------------------------------------------------------------
c		Put string into cbuff and karbuf 
      cbuff = ' '
      ilen = lastblnk(string)
      do 100 i = 1, ilen
         cbuff(i:i) = string(i:i)
 100  continue 
      nbytes = min(ilen+1, karlim)
      call unpack(iwpc(nbytes), cbuff, karbuf)
c		compile it using execpol, special version of polish
      kbptr = 1
      errcode = 0.0
      call execpol
c		double check that mode is 0
      if (mode .ne. 0) call oerror(n17, n1, 'Line Ignored')
c		Do something only if something is there
      ktag = 0
      if (ap .ne. 0) then
c		Use runktag to get someplace safe to copy atmp
         np = ap + 2
         call runktag(np, ktag)
c		runtag is what checks to make sure np I*2 words fit
c		ktag is a location in the C array that is safe
c		copy A array over to c array
         k(ktag) = 0
         k(ktag+1) = 0
         call copy(ap, a(1), k(ktag+2))
      endif
c		use finish to clean up any if, while, for constructs
         nt = finish(k, ktag, .true.)
         if (nt .eq. 1) then
            call oerror(n21,n1,'Too many IF, WHILE, or FOR constructs')
         else if (nt .eq. 2) then
            call oerror(n21,n1,'Too many END, THEN, or ELSE statements')
         else if (nt .eq. 3) then
            call oerror(n21, n1, 'No closing END statement')
         endif
c	
      execcomp = ktag
      return
c
      end
