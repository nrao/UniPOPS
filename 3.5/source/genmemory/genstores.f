      SUBROUTINE STORES	(J)
C-------------------------------------------------------------------------------
C  @(#)genstores.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C   STORES-OPERATIONS
C   -----------------
C	 STORES	stores either the procedure source code, procedure
C     object code, or handles the source code.	Note opcode 64 is a
C     internal operator	that stores the	source and object code for
C     procedures.
C
C     LISTF is the storage location for	PROC source code.
C
C---------------------------------------------------------------------
C
      character*160 tempstring
      integer*2 j, llocat, ibyte, lastblnk, ii, listl,
     1          itempstring(80), iwpc 
      integer*2 n120, m3, fshort
c
      INCLUDE 'core.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
      include 'lsf.inc'
      include 'smstuf.inc'
c
      equivalence (itempstring(1), tempstring)
C
      data n120/120/, m3/-3/
c
C=======================================================================
C
C					Get LISTF.
c
      if (j .ne. 1) call oerror(n120, m3, 'GENSTORES')
C
C---------------------------------------------------------------------
C   				   Store PROC source and object	code.
C---------------------------------------------------------------------
C					Store object code.
      L=LLOCAT(fshort(AP+2),K,LPGM)
      LPGM=L
      L=L+1
      LISTL=1
      tempstring = lastline
      ibyte = min( lastblnk(tempstring), kbptr-1)
c     Because of processing by a preparser like AMTHPARSER, must now use the
c     input string stored in HIST as opposed to that in KARBUF or CBUFF.
c     Also, the 'case' of the input string is preserved.
c
      ii = iwpc(ibyte) + 1
      K(L)=LLOCAT(II,LISTF,LISTL)
      L=L+1
      CALL COPY	(AP,A,K(L))
C					Store source code.
      listf(listl) = ibyte
      LISTL=LISTL+1
      call copy(ibyte, itempstring, listf(listl) )
c
      RETURN
      END
c
     
