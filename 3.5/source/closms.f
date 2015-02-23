      SUBROUTINE CLOSMS(LUN,IER)
C-------------------------------------------------------------------------------
C  @(#)closms.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c     Changed 8903    [RJM] See CHANGES.DOC
c****************************************************************
      INTEGER*2 LUN, ier, inunit, istat
c
      include 'cio.inc'
c
      inunit = LUN
      CLOSE(UNIT=inunit, IOSTAT=ISTAT)
      ier = istat
      RETURN
      END

