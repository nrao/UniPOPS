      subroutine uppercase(lc,uc)
C-------------------------------------------------------------------------------
C  @(#)uppercase.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Converts any lower case characters in string lc to upper case
c     and stores results in string uc
c
      character*(*) uc, lc
c
      character*2 c1
      integer*2 ic1, izero, lastblnk, ilen, i
c
      equivalence (ic1, c1)
c
      data izero/x'0000'/
c
      ilen = min(100,lastblnk(lc))
c
      ic1 = izero
c
      uc = ' '
      do 100 i = 1, ilen
	c1(2:2) = lc(i:i)
        if (ic1 .ge. 97 .and. ic1 .le. 122) ic1 = ic1 - 32
        uc(i:i) = c1(2:2)
100     continue
c
      return
      end
c
