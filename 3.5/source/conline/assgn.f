      SUBROUTINE  ASSGN
C-------------------------------------------------------------------------------
C  @(#)assgn.f	5.5 03/23/96
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C Modified 890105 [PPM] includes to .inc, lowercase
c          8903   [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
      integer*2 iblank, nsize, j, m, itag, isize, itype, irealp, iwpr
      integer*2 n28, n107, n126, n127, m1, iwpc, kmin, kmax
      INCLUDE 'smstuf.inc'
      include 'core.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
c
      DATA IBLANK/'  '/
      data n28, n107, n126, n127, m1 /28, 107, 126, 127, -1/
C
C=======================================================================
C
      kmin = iwpc(kxorg) + 6
      kmax = 32766
      if (sp .le. 0 .or. sp .gt. slim) 
     .		call oerror(n107, m1, '= (Assignment)')
      M=STACK(SP)
      IF (M.EQ.2 .or. m .eq. 3) GO TO	4002
      if (sp-1 .le. 0) call oerror(n107, m1, '= (Assignment)')
      M	= STACK(SP-1)
      IF (M.EQ.2) GO TO	4000
c
      if (m .le. kmin .or. m. ge. kmax) 
     .		call oerror(n126, m1, 'Left side is a constant')
c
      call setdefaults(m, v(sp))
c     Revert adverb value back to default value, if needed
c
      C(M)=V(SP)
      SP=SP-2
      if (sp .lt. 0) call oerror(n107, m1, '= (Assignment)')
      goto 99
c
C					Vector = Constant.
 4000 if (sp-4 .le. 0) call oerror(n107, m1, '= (Assignment)')
      TAG=STACK(SP-2)
      NSIZE=STACK(SP-3)
      TYPE=STACK(SP-4)
      IF (TYPE.ne.2) call oerror(n126, m1, ' ')
      M	= TAG +	NSIZE -	1
      if (tag .le. kmin .or. m. ge. kmax) 
     .		call oerror(n126, m1, 'Left side is a constant')
c
      call setdefaults(tag, v(sp))
c     Revert adverb value back to default value, if needed
c
      DO 4009 J	= TAG, M
	   C(J)	= V(SP)
 4009	   CONTINUE
      SP=SP-5
      if (sp .lt. 0) call oerror(n107, m1, '= (Assignment)')
      goto 99
c
C					   VECTOR & STRING ASSIGNMENT
 4002 if (sp-7 .lt. 0) call oerror(n107, m1, '= (Assignment)')
      ITAG = STACK(SP-1)
      ISIZE = STACK(SP-2)
      ITYPE = STACK(SP-3)
      IF (STACK(SP-4).ne.stack(sp)) call oerror(n126, m1, ' ')
      TAG = STACK(SP-5)
      NSIZE = STACK(SP-6)
      TYPE = STACK(SP-7)
      IF ( (TYPE.ne.7.or.ITYPE.ne.14) .and.
     .     (type.ne.7.or.itype.ne.7) ) call oerror(n126, m1, ' ')
      SP = SP -	8
      if (sp .lt. 0) call oerror(n107, m1, '= (Assignment)')
      IF (ITYPE.EQ.14) ISIZE = IREALP(ISIZE)
      IF (ISIZE.gt.NSIZE) call oerror(n127, m1, ' ')
      M	= IWPR(ISIZE)
      if (itag .le. 0 .or. itag+isize-1. ge. kmax) 
     .		call oerror(n126, m1, ' ')
      if (tag .le. kmin .or. tag+isize-1. ge. kmax) 
     .		call oerror(n126, m1, 'Left side is a constant')
      CALL COPY (M,C(ITAG),C(TAG))
C					   Blank fill.
      IF (ISIZE.ne.NSIZE) then
	 M = IBLANK
	 IF (TYPE.EQ.2)	M = 0
	 ITAG =	TAG + ISIZE
	 NSIZE = NSIZE - ISIZE
	 NSIZE = IWPR (NSIZE)
	 if (itag.le.kmin.or.itag+iwpc(nsize)-1.ge.kmax) 
     .		call oerror(n126, m1, 'Left side is a constant')
	 CALL FILL (NSIZE,M,C(ITAG))
      endif
c
   99 CONTINUE
      RETURN
      END

