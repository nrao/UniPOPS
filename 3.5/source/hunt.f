      SUBROUTINE HUNT (K,L,NT,ISTART,KHUNT)
C-------------------------------------------------------------------------------
C  @(#)hunt.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C	 HUNT searches a linked	list usually the symbol	or literal
C     tables.  The array K contains the	table with K(L)	as the first
C     entry.  NT words are matched beginning at	ISTART words from the
C     beginning	of the entry with KHUNT.  The pointer L	is left	pointing
C     at the entry found or equal to zero if no	match is found.
c      Modified 8903  [RJM] See CHANGES.DOC
C----------------------------------------------------------------------
      integer*2 KHUNT(*), K(*), l, nt, istart, i, j

10    continue
      if (l .gt. 0) then
	if (k(l+1)/16 .ne. nt) then
		l = k(l)
		goto 10
	else
      		J=L+ISTART
      		DO 20 I=1,NT
	 	   IF (K(J).NE.KHUNT(I)) then
			l = k(l)
			goto 10
		   else
 			J=J+1
		   endif
20		   continue
	endif
      endif
      RETURN
      END
