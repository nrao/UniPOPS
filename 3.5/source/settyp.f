      SUBROUTINE SETTYP	(KARRAY,LPTR,IVALUE)
C-------------------------------------------------------------------------------
C  @(#)settyp.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C--------------------------------------------------------------------
C	 SETTYP	sets the type code for either a	symbol or literal as
C     defined by the location LPTR, and	the new	type code IVALUE.
c     Modified 8903 [RJM] See CHANGES.DOC
C--------------------------------------------------------------------
      integer*2	KARRAY(*), lptr, ivalue
      integer*2 n120, m3
      data n120, m3 /120, -3/
c
      if (lptr .lt. 2) call oerror(n120,m3,'SETTYP')
c
      KARRAY(LPTR-1)=(KARRAY(LPTR-1)/16)*16+IVALUE
c
      RETURN
      END
