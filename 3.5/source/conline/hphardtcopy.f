      subroutine hphardtcopy
C-------------------------------------------------------------------------------
C  @(#)hphardtcopy.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Dumps internal array INSTRING to device iouttxt for dumping
c	of text screen -- HPGL code
c******************************
c 8904 [RJM]
c******************************
c
      integer*2 i, ij, lastblnk, ilen2
      integer*2 n16, fshort, n90
      character*100 stch
c
      include 'cio.inc'
c
      data n16/16/, n90/90/
c
      do 1099 i = 1, inlast
         ij = mod(fshort(i+inline-2),inlast) + 1
         write(stch,10) 'PU;PA 10 ',750-(i-1)*20, ';'
10	 format(a9,i6,a1)
	 call putchar(stch, n16, iouttxt)
         ilen2 = min(n90,lastblnk(instring(ij)))
	 write(stch,20) 'LB', instring(ij)(1:ilen2), char(3)
20	 format(a2,a,a1)
	 call putchar(stch, fshort(ilen2+3), iouttxt)
1099     continue
c
      return
c      
      end
c
