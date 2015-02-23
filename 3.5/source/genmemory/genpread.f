      SUBROUTINE PREAD (KB,IEOF)
C-------------------------------------------------------------------------------
C  @(#)genpread.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C        PREAD reads input text and unpacks the buffer one byte per
C     word.  The buffer is assumed to be 160 or less characters.  The
C     buffer is padded with blanks.
C-----------------------------------------------------------------------
      integer*2 KB(*), ieof, iblank, lastblnk, iwpc
c
      character*160 cbuff
c
      INCLUDE 'cio.inc'
c
      equivalence (cbuff, jbuff)
c
      DATA IBLANK /'  '/
C
C=======================================================================
C
C                                       All input text is tight buffer.
      IEOF=1
      READ (iinunit,2000,END=30) cbuff
      nbytes = min(lastblnk(cbuff)+1,karlim)
c
      inhist = 1
      maxhist = 1
      hist(1) = cbuff
      lastline = cbuff
c     Stores input string into HIST array in case some other routine may need 
c     it.
c
      CALL UNPACK (iwpc(nbytes),JBUFF,KB)
      GO TO 99
 30   IEOF=2
 99   RETURN
C-----------------------------------------------------------------------
 2000 FORMAT (a160)
      END

