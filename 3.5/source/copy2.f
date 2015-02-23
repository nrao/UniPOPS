      SUBROUTINE COPY2 (N,KFROM,KTO)
C-------------------------------------------------------------------------------
C  @(#)copy2.f	1.1 08/13/98
C-------------------------------------------------------------------------------
C
C          COPY TRANSFERS N INTEGER WORDS FROM KFROM TO KTO.
C
c     Changed 8903 [RJM] See CHANGES.DOC
C     This version uses I*4 indexing, which the other should
C     as well, but that seemed more dangerous than simply
C     making a copy2 for use where necessary.
c************************************************************
      integer*2 KFROM(*), KTO(*)
      integer*4 n, i
c
      DO 10 I=1,N
         KTO(I) = KFROM(I)
   10    CONTINUE
      RETURN
      END
