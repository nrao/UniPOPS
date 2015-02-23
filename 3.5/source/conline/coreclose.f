      subroutine coreclose
C-------------------------------------------------------------------------------
C  @(#)coreclose.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c    
c     Close down graphics Shared Memory and FIFOs
c
      integer*4 long, irtn
c
      include 'cio.inc'
c
      call writefifo(long(igraphout), irtn, 'exit')
      call closefifo(long(igraphout), irtn)
      call closefifo(long(igraphin), irtn)
c     Tell graphics process to close down and close files
c
      call destroyshm2
      igraphtype = 0
c     Destroy shared memory
c
      return
c
      end
c
