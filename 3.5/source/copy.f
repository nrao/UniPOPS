      SUBROUTINE COPY (N,KFROM,KTO)
C-------------------------------------------------------------------------------
C  @(#)copy.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C
C          COPY TRANSFERS N INTEGER WORDS FROM KFROM TO KTO.
C
c     Changed 8903 [RJM] See CHANGES.DOC
c************************************************************
      integer*2 KFROM(*), KTO(*)
      integer*2 n, i
c
      DO 10 I=1,N
         KTO(I) = KFROM(I)
   10    CONTINUE
      RETURN
      END
