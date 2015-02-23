      subroutine posthardtcopy
C-------------------------------------------------------------------------------
C  @(#)posthtcopy.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Dumps internal array INSTRING to device iouttxt for dumping
c	of text screen -- POSTSCRIPT code
c********************************************
c 8904 [RJM]
c********************************************
c
      integer*2 i, ij, lastblnk, ilen2
      integer*2 n15, fshort
      character*100 stch
c
      include 'cio.inc'
c
      data n15 /15/
c
      do 1099 i = 1, inlast
         ij = mod(i+inline-2,inlast) + 1
	 write(stch,10) 10, 750-(i-1)*20
10       format(2i6, ' m ')
         call putchar(stch,n15,iouttxt)
	 ilen2 = min(90,lastblnk(instring(ij)))
	 stch = '(' // instring(ij)(1:ilen2) // ') show'
         call putchar(stch, fshort(ilen2+7), iouttxt)
1099     continue
c
      return
c      
      end
c
