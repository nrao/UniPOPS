      subroutine updatehist(string)
C-------------------------------------------------------------------------------
C  @(#)updatehist.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Adds STRING to the internal record of input strings
c
      character*(*) string
c
      integer*2 MAXWORDS
      parameter (MAXWORDS=160)
c     MAXWORDS characters maximun can be stored in HIST
c
      include 'cio.inc'
      integer*2 lastblnk, ij, wstart(MAXWORDS), wend(MAXWORDS), i, 
     .		i1, numword, lngth, n104, n0, wtype(MAXWORDS)
      character*160 outstrng
c
      parameter (n104=104)
      parameter (n0=0)
c
      if (lastblnk(string) .eq. 0) return
c
      call findwords(string, MAXWORDS, numword, wstart, wend, wtype)
c
      outstrng = " "
      i1 = 1
      do 100 i = 1, numword
	 lngth = i1 + wend(i) - wstart(i)
         if (lngth .le. MAXWORDS) then
		outstrng(i1:lngth) = string(wstart(i):wend(i))
		if (wtype(i) .eq. 1) then
		   i1 = lngth + 1
		else
		   i1 = lngth + 2
		endif
c		Add a space after regular words or strings; don't after
c		special chracaters.
	 else
		call oerror(n104, n0, 'Line ignored')
	 endif
100	 continue
c     Compress out all unneeded blanks.
c
      inhist = inhist + 1
      ij = mod(inhist-1,maxhist) + 1
      hist(ij) = outstrng
c
      return
      end
c

