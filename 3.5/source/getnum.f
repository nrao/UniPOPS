      SUBROUTINE GETNUM (KB,kbp,x)
C-------------------------------------------------------------------------------
C  @(#)getnum.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C   NUMBER FIELD SCANNER
C   ------ ----- -------
C        CONVERTS ALPHANUMERIC FIELD INTO A SINGLE-PRECISION REAL #.
c Modified 8903 [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
      character*(*) KB
      real x
      integer*2 kbp
      integer*2 lastblnk, kbplim, ifirst, ilast
      integer*2 n1, n15
      data n1, n15 /1, 15/
c
C---------------------------------------------------------------------
c
      kbplim = lastblnk(kb)
      if (kbplim .lt. kbp) then
        kbp = len(kb)
	x = 0.
	return
      endif
c     Returns a zero if all blanks to end of string
c
      do 102 ifirst = kbp, kbplim
         if (kb(ifirst:ifirst) .ne. ' ' .and.
     1       kb(ifirst:ifirst) .ne. ',') goto 105
102      continue
c     Finds first non-blank character
c
105   do 103 ilast = ifirst, kbplim
	if (kb(ilast:ilast) .eq. 'e') kb(ilast:ilast) = 'E'
	if (kb(ilast:ilast) .eq. 'E') goto 103
c       Convert any 'e' for exponent into capital E
c
	if (ilast .gt. ifirst .and.
     1      (kb(ilast-1:ilast) .eq. 'E-' .or.
     2       kb(ilast-1:ilast) .eq. 'E+') ) goto 103
c
        if (ilast .eq. ifirst .and.
     1      (kb(ilast:ilast) .eq. '-' .or.
     2       kb(ilast:ilast) .eq. '+') ) goto 103
c
        if (kb(ilast:ilast) .eq. '.') goto 103
c
        if ( llt(kb(ilast:ilast),'0') .or.
     1       lgt(kb(ilast:ilast),'9') ) goto 110
c       Any non-number character (except E+, E-, E, +, -, .) signals end
c       of field.
c
103     continue
      ilast = kbplim + 1
c     Find next , or space, which indicates end of field
c
110   read(kb(ifirst:ilast-1), 100, err=120) x
100   format(e80.0)
      kbp = ilast + 1
      return
c
120   continue
      call oerror(n15,n1,' ')
      return
c
      END
