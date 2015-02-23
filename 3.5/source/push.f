      SUBROUTINE PUSH (STACK,SP,ITEM)
C-------------------------------------------------------------------------------
C  @(#)push.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C--------------------------------------------------------------------
C	 PUSH pushes ITEM onto the pushdown stack STACK	and
C     advances the spack pointer SP.
c      Modified 8903 [RJM] See CHANGES.DOC
C--------------------------------------------------------------------
      INTEGER*2 STACK(*), SP, item
      integer*2 n1, n7
      data n1, n7 /1, 7/
c
c
      SP=SP+1
      IF (SP.gt.399) call oerror(n7,n1,'Too many items in stack')
c
      STACK(SP)=ITEM
c
      RETURN
c
      END
