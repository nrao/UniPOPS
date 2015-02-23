      SUBROUTINE POP (STACK,SP,ITEM)
C-------------------------------------------------------------------------------
C  @(#)pop.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C	 POP takes the last item on the	stack STACK and	places
C     it in ITEM.  The stack pointer SP	is decremented.
c      Modified 8903 [RJM] See CHANGES.DOC
C----------------------------------------------------------------------
      INTEGER*2 STACK(*),SP,ITEM
      integer*2 n1, n7
      data n1, n7 /1, 7/
c
      IF (SP.le.0) call oerror(n7,n1,'Too few items in STACK')
c
      ITEM=STACK(SP)
      SP=SP-1
c
      RETURN
c
      END
