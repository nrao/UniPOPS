      subroutine quichardtcopy
C-------------------------------------------------------------------------------
C  @(#)quichardtcopy.f	4.2 11/24/92
C-------------------------------------------------------------------------------
c
c     Dumps internal array INSTRING to device iouttxt for dumping
c	of text screen -- QUIC code
c
      integer*2 i, ij, ilen2, lastblnk, j
c
      include 'cio.inc'
c
      write(iouttxt,1000)
c
      do 100 i = 1, inlast
           ij = mod(i+inline-2,inlast) + 1
	   ilen2 = min(80,lastblnk(instring(ij)))
           write(iouttxt, 1010)
           write(iouttxt, 1020) (instring(ij)(j:j),j=1,ilen2),'^','X'
 100  continue
c
      write(iouttxt, 1030)
c
      return
c
 1000 format('^A^IGE^T00000^JM00800^X')
 1010 format('^A^T00000^JR00200^X')
 1020 format('^A',82a1)
 1030 format('^A^IGV')
c      
      end
