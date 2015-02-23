      SUBROUTINE LTSTOR
C-------------------------------------------------------------------------------
C  @(#)ltstor.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C   Search the literal list.  If matching literal is found, return tag.
C   if not, generate a new one, linking it to the literal list.
C
C   Modified 890105 [PPM] include to .inc, lowercase
c            8903   [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
      integer*2 ix(1), isp, nt, n, llocat, mk, irk, irealp, iinitp
      integer*2 n2
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
c
      equivalence (x(1), ix(1))
C
      DATA ISP/'  '/
      data n2 /2/
C
C=======================================================================
c
C					Literal	look-up	& define.
      NT=LX
      N=LX+2
c
      IF (MODE.le.0) then
C					Temporary Literals.
         L=KKT(4)
C					Leave L	pointing to the	entry.
         CALL HUNT(KKT,L,NT,n2,IX)
         IF (L.eq.0) L=LLOCAT(N,KKT,KKT(10))
C					Make L refer to	the K array.
         L=L+KT-1
  	 MK=L+2
	 K(L+1)=16*NT+TYPE
	 IRK = MK + NT
	 IRK = IREALP(IRK)
	 IF ((MK+NT).NE.IINITP(IRK)) K(MK+NT) =	ISP
	 CALL COPY (NT,IX(1),K(MK))
c
      else
C					Permanent literals.
         L=K(4)
         CALL HUNT(K,L,NT,n2,IX)
         IF (L.eq.0) then
	    L=LLOCAT(N,K,K(10))
  	    MK=L+2
	    K(L+1)=16*NT+TYPE
	    IRK = MK + NT
	    IRK = IREALP(IRK)
	    IF ((MK+NT).NE.IINITP(IRK)) K(MK+NT) = ISP
	    CALL COPY (NT,IX(1),K(MK))
	 endif
c
      endif
c
      TAG=IREALP(L)+1
      LOCSYM=L
c
      RETURN
      END
