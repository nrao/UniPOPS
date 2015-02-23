      SUBROUTINE SUBS
C-------------------------------------------------------------------------------
C  @(#)subs.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C Modified 890105 [PPM] includes to .inc, lowercase
c          8903   [RJM] See CHANGES.DOC
C---------------------------------------------------------------------
      integer*2 idata, n, ndim, index, isize, i, ndelta, j
      integer*2 n0, n106, n112, n128, n129, m1
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
c
      data n0, n106, n112, n128, n129, m1 
     .     /0, 106, 112, 128, 129, -1/
C
C=======================================================================
C
C---------------------------------------------------------------------
C		   S U B S   O P E R A T O R			 
C---------------------------------------------------------------------
C
      if (sp .le. 4) call oerror(n112, m1, 'Array subscripts')
      IDATA=STACK(SP)
      TAG=STACK(SP-2)
      TYPE = STACK(SP-4)
      if (idata .le. 0) call oerror(n106, m1, 'Array subscripts')
      N=K(IDATA)
      NDIM=K(IDATA+1)
      SP=SP-5
      INDEX=0
C			     get field length data if character	string
      IF (TYPE.eq.7) then
	 NDIM =	NDIM - 1
	 ISIZE = K(IDATA + 3)
	 IDATA = IDATA + 2
      endif
      I=SP-NDIM+1
c
      if (i .le. 0) call oerror(n106, m1, 'Array subscripts')
c     Error if the wrong number of dimensions are specified
c
      SP=I
      NDELTA=1
      DO 9801 J=1,NDIM
	 IDATA=IDATA+2
         if (nint(v(i)) .lt. k(idata) .or.
     1	     nint(v(i)) .ge. k(idata) + k(idata+1)) 
     2		call oerror(n128, m1, 'Array subscripts')
c	 Traps out-of-bounds on arrays
c
	 INDEX=INDEX+NDELTA*(V(I)-K(IDATA))
	 NDELTA=NDELTA*K(IDATA+1)
C			     put value of array	location on stack
         I=I+1
9801     continue
c
      IF (TYPE.EQ.7) GO TO 9802
      IF (INDEX.ge.N) call oerror(n128, m1, 'Array subscripts')
c
      INDEX=TAG+INDEX
      if (index .le. 0 .or. sp .le. 0) call oerror(n106, m1, 
     .		'Array subscripts')
      STACK(SP)=INDEX
      V(SP)=C(INDEX)
      GO TO 99
C			     put string	subfield parameters onto stack
9802  INDEX = INDEX * ISIZE
      IF (INDEX.ge.N) then
         nspacen = 32767.
         call oerror(n128, m1, 'Array subscripts')
      endif
      INDEX = INDEX + TAG
      if (sp .le. 0) call oerror(n106, m1, 'Array subscripts')
      STACK(SP)	= TYPE
      STACK(SP+1) = ISIZE
      STACK(SP+2) = INDEX
      STACK(SP+3) = 3
      SP = SP +	3
   99 CONTINUE
C---------------------------------------------------------
      END
