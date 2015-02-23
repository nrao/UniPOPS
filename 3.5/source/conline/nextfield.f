      subroutine nextfield(string)
C-------------------------------------------------------------------------------
C  @(#)nextfield.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Extracts the next string from the input line
c     Field begins with next non-blank character and ends with either a
c	semi-colon or another space.
c     Depending upon the input, the returned string could be in either
c     upper or lower case.
c
c     December 1989 [RJM]
c**********************************************************
c
      character*(*) string
      character*2 cm
      integer*2 m, ib, ibstart, iend, n14, n1
c
      include 'cio.inc'
      include 'smstuf.inc'
c
      equivalence (cm, m)
c
      data n1, n14 /1, 14/
c
      ib = kbptr
10    if (ib .le. min(nbytes,karlim) ) then
	m = karbuf(ib)
	if (cm(1:1) .ne. ' ' ) goto 20
	ib = ib + 1
	goto 10
      endif
c     Skips over any initial blanks
c
c
20    ibstart = ib
      do 30 iend = ibstart, min(nbytes,karlim)
	m = karbuf(iend)
	if (cm(1:1) .eq. ' ' .or. cm(1:1) .eq. ';') goto 40
30	continue
      iend = min(nbytes,karlim)
c     looks for next blank or colon to flag end of field
c
40    if (ibstart .eq. iend) then
	string = ' '
      else if (iend - ibstart .gt. 60) then
c		too many characters
        call oerror(n14, n1, ' ')
      else
        string = cbuff(ibstart:iend)
      endif
c     Extracts the next field on the input line
c
      kbptr = iend
c
      return
      end
c
