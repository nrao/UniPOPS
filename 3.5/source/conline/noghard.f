      subroutine noghardvctr(i,j)
C-------------------------------------------------------------------------------
C  @(#)noghard.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Dummy routines for taking care of non-graphics printers
c
      include 'cio.inc'
c
      integer*2 string(*), i, j, k, l, i1, ij, lastblnk, ilen
      integer*4 idx
c
      return
c
      entry noghardplace(i,j)
      return
c
      entry noghardchar(string,i)
      return
c
      entry noghardcsize(i)
      return
c
      entry noghardbox(i,j,k,l)
      return
c
      entry noghardcolor(idx)
      return
c
      entry noghardtcopy
c
c     Dumps internal array INSTRING to device IMUNIT for dumping
c	of text screen -- Non-graphics printer code
c     
	do 1099 i1 = 1, inlast
	   ij = mod(i1+inline-2,inlast) + 1
	   ilen = lastblnk(instring(ij))
	   write(iouttxt,10) instring(ij) (1:ilen)
10	   format(a)
1099	   continue
      return
c
      end
c

