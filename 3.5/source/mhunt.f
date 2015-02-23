      SUBROUTINE MHUNT (K,L,NT,ISTART,KHUNT,NHUNT)
C-------------------------------------------------------------------------------
C  @(#)mhunt.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
c     This is the min-match version of HUNT
c
C	 HUNT searches a linked	list usually the symbol	or literal
C     tables.  The array K contains the	table with K(L)	as the first
C     entry.  NT words are matched beginning at	ISTART words from the
C     beginning	of the entry with KHUNT.  The pointer L	is left	pointing
C     at the entry found or equal to zero if no	match is found.
c      Modified 8903 [RJM] See CHANGES.DOC
C----------------------------------------------------------------------
      integer*2	KHUNT(*), K(*), l, nt, istart, nhunt, ib, nmax, newl, i,
     .          length, j, m
      INTEGER*2  KNEW(25), KH(25)
      character*1 BNEW(50), BH(50)
      integer*2 n20, m3
c
      include 'ambig.inc'
c
      EQUIVALENCE (KNEW,BNEW), (KH,BH)
c
      DATA IB/'  '/
      data nmax/100/
      data n20, m3 /20, -3/
c
      NEWL=0
      NAPP=0
   10 IF (L.le.0) GO TO	55
      LENGTH=K(L+1)/16
c			if NT > LENGTH, its an automatic mismatch
      if (nt .gt. length) go to 52
c			if nt != length, check for min-match
      IF (LENGTH.NE.NT)	GO TO 40
      J=L+ISTART
      DO 20 I=1,NT
	     IF (K(J).NE.KHUNT(I)) GO TO 40
         J=J+1
   20    CONTINUE
c     Entered and internal symbols have exactly the same number
c     of characters and match exactly
c
      GO TO 60
   40 J=L+ISTART
      DO 41 M=1,NT
         KH(M)=KHUNT(M)
   41    CONTINUE
      DO 42 M=1,LENGTH
         KNEW(M)=K(J+M-1)
   42    CONTINUE
      DO 50 I=1,NHUNT
         IF (BNEW(I).NE.BH(I)) GO TO 52
   50    CONTINUE
c     Symbols don't have same number of characters but check whether
c     min-match will work on that symbol and that only one symbol
c     min-matches
c     NOTE: this assumes that NHUNT characters can be contained in NT I*2s
c
      NAPP=NAPP+1
c     Bump number of ambiguous symbols
c
c
      if (napp .gt. nmax) goto 55
c
      NEWL=L
      do 43 m = 1, 5
	if (m .le. length) then
	  mname(m,napp) = knew(m)
	else
	  mname(m,napp) = ib
	endif
   43    CONTINUE
   52 L=K(L)
      GO TO 10
c     Go on to next internal symbol
c
   55 IF (NAPP.EQ.1) GO TO 59
      L=0
      IF (NAPP.GT.1) call oerror(n20,m3,' ')
      GO TO 60
   59 L=NEWL
   60 CONTINUE
      RETURN
      END
