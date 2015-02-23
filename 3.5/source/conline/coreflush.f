      subroutine coreflush
c
C-------------------------------------------------------------------------------
C  @(#)coreflush.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Flushes out the graphics shared memory  and FIFO
c
      include 'cio.inc'
c
      character*80 string
      integer*4 long, getnextloc, irtn
c
      if (getnextloc() .ne. 0) then
      	call writefifo(long(igraphout), irtn, 'flush')
10      call readfifo(long(igraphin), irtn, string)
c
      	if (string(1:4) .ne. 'done') goto 10
	if (getnextloc() .ne. 0) goto 10
c	Watch out for a leftover 'cursor' or similar response from the graphics
c	process
c
      endif
c     Only get GRAPHICS process to flush shared memory if anything exists their
c
      return
      end
c
