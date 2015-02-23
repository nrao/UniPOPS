      SUBROUTINE KDUMP (I,J,K,C)
C-------------------------------------------------------------------------------
C  @(#)kdump.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C------------------------------------------------------------------------
C        KDUMP prints the contents of the K array in integer, character
C     and real format as possible.
c     0389     [RJM] -- see CHANGES.DOC
c------------------------------------------------------------------------
      INTEGER*2 I,J,K(*)
      REAL*4    C(*)
      integer*2 n80
      data n80 /80/
c
      integer*2 n, l, m, irealp
c
      integer*2 itemp(4)
      character*2 ctemp, c1
      double precision dtemp
      character*80 buffer
      integer*2 ibuffer(40)
c
      equivalence (ctemp, itemp(1)), (dtemp, itemp), (ibuffer, buffer)
c
      do 50 n = i, j
	do 40 l = 1, 4
	   itemp(l) = k(n+l-1)
40	   continue
c	   
	m = irealp(n)
	c1 = ctemp
	if (ichar(c1(1:1)).lt.32 .or. ichar(c1(1:1)).gt.126) c1(1:1)=' '
	if (ichar(c1(2:2)).lt.32 .or. ichar(c1(2:2)).gt.126) c1(2:2)=' '
c
	write(buffer,411,err=50) n,(n-i+1),k(n),c(m),dtemp,c1,k(n)
411	format(1x, 3i7, 1x, 1pg15.7e2, 1pg24.16e2, 1x, a2, 1x, 16r, i5)
	call pwrite(ibuffer,n80)
c
50	continue
      return
c
      end
c
