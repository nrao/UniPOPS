      subroutine coreplace (ix, iy)
C-------------------------------------------------------------------------------
C  @(#)coreplace.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Places cursor for next VCTR or PCHAR call -- Uses Graphics shared memory
c****************************************************
c 8904 [RJM]
c****************************************************
c
      include 'cio.inc'
c
      integer*2 ix, iy
      integer*4 l1, lix, liy
      character*80 string
c
      data l1 /1/
c
      lix = ix
      liy = iy
      call writeshm2(l1, lix, liy, string)
c
      return
      end
c
