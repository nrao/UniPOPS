      subroutine updateline(string)
C-------------------------------------------------------------------------------
C  @(#)updateline.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Adds STRING to the internal record of input/output strings
c     Checks whether the text page has overflowed
c*****************************************************
c* 8903  [RJM]
c*****************************************************
c
      character*(*) string
      character*1 c1
      integer*2 lastblnk, ilen, i1, i2
      integer*4 long
c
      include 'cio.inc'
c
      ilen = max(1, lastblnk(string))
      i1 = 1
      i2 = min(ilen, linelngth)
c     Sets limits on what should be stored in INSTRING; Don't want to overflow
c	INSTRING.  Is STRING is very long, then more than one INSTRING will be
c	used to hold STRING
c
9     instring(inline) = string(i1:i2)
      inlast = max(inline, inlast)
      inline = mod(inline,numline) + 1
c
      if (inline .eq. numline .and. pagefull) then
	write(istdout,10) 
10	format(' PAGEFULL: Enter T for tcopy, C to cancel pagefull, ',
     1	       '<cr> for next page:',$)
c
	call flush(long(istdout))
        read(istdin,11) c1
11      format(a1)
c
        if (c1.eq.'t'.or.c1.eq.'T') call tcopy
        if (c1.eq.'c'.or.c1.eq.'C') pagefull = .false. 
        if (igraphtype .ge. 3) call clrpage
      endif
c
      if (i2 .lt. ilen) then
	i1 = i2 + 1
	i2 = min(i1+linelngth-1, ilen)
	goto 9
      endif
c
      return
      end
c

