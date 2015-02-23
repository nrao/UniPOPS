      subroutine putchar(string, length, idev)
C-------------------------------------------------------------------------------
C  @(#)putchar.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Puts characters fron STRING onto device IDEV one at
c     a time.  
c************************************************
c 8904 [RJM] 
c************************************************
c
      character*(*) string
      integer*2 length, idev, i
c
      integer*4 fputc, irtn, idev1
c
      idev1 = idev
c
      do 100 i = 1, length
	irtn = fputc(idev1,string(i:i))
100     continue
c
      return
      end
c
