      subroutine qmshardtcopy
C-------------------------------------------------------------------------------
C  @(#)qmshardtcopy.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Dumps internal array INSTRING to device iouttxt for dumping
c	of text screen -- QMS (TEK 4010) code
c******************************
c 8904 [RJM]
c******************************
c
      integer*2 i, ij, ilen2, lastblnk
      integer*2 n1, n4, n80, fshort
      character*4 cout
c
      include 'cio.inc'
c
      data n1, n4, n80 /1, 4, 80/
c
      do 1099 i = 1, inlast
           ij = mod(fshort(i+inline-2),inlast) + 1
	   call putchar(char(29),n1,iouttxt)
	   call packer(10, fshort(750-(i-1)*20), cout)
	   call putchar(cout,n4,iouttxt)
	   call putchar(char(31),n1,iouttxt)
	   ilen2 = min(n80,lastblnk(instring(ij)))
	   call putchar(instring(ij), ilen2, iouttxt)
1099     continue
c
      return
c      
      end
