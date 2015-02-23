      SUBROUTINE initcomm
C-------------------------------------------------------------------------------
C  @(#)initcomm.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C	 INIT initializes the K, LISTF,	and commons for	POPS.
C Modified 890105 [PPM] includes to .inc, lowercase
c          8903   [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
      integer*2	ITRUE(2), IFALSE(3), karat, iwpr, irealp, iwpc
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      include 'cio.inc'
c
      integer*2 n0, n1, n2, n4, n5, n40
c
      DATA ITRUE/'TR','UE'/, IFALSE/'FA','LS','E '/
      DATA KARAT /' >'/
c
      data n0, n1, n2, n4, n5, n40 /0, 1, 2, 4, 5, 40/
C
C=======================================================================
C
C					COMMON VARIABLES
c
      slim = 400
      NEXTP=0
      LPGM=2
      KT=51
      IDEBUG = -1
      CALL FILL	(n40,n0,STACK)
      CALL FILL	(n40,n0,CSTACK)
C					Initialize.
C					   One
      L	= K(4)
      CALL HUNT	(K,L,IWPR(n1),n2,1.0)
      TAG = IREALP(L)+1
      ONE=TAG
C					   Zero
      L	= K(4)
      CALL HUNT	(K,L,IWPR(n1),n2,0.0)
      TAG = IREALP(L)+1
      ZERO=TAG
C					   True
      L	= K(1)
      CALL HUNT	(K,L,IWPC(n4),n4,ITRUE)
      TAG = K(L+2)
      TRUE=TAG
C					   False
      L	= K(1)
      CALL HUNT	(K,L,IWPC(n5),n4,IFALSE)
      TAG = K(L+2)
      FALSE=TAG
C
      MODE = 0
      IEDIT = 0
      CALL PROMPT (KARAT,IPT)
c
   99 CONTINUE
      RETURN
      END

