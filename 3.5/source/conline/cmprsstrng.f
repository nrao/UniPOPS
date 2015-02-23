      subroutine cmprsstrng(in, out)
c---------------------------------------------
c  @(#)cmprsstrng.f	5.1 06/22/94
c---------------------------------------------
c
c     Compresses out all double blanks from IN string and places
c     results back into into OUT string
c
      character*(*) in, out
      integer*2 lastblnk, ilen, i, i1
c
      ilen = lastblnk(in)
      i1 = 1
      out = ' '
      if (ilen .ne. 0) then
	do 100 i = 1, ilen-1
	   if (in(i:i+1) .ne. '  ') then
		out(i1:i1+1) = in(i:i+1)
		i1 = i1 + 1
	   endif
100	   continue
c
      else
	out = in
      endif
c
      return
      end
c
     
