      SUBROUTINE PWRITE	(KB,N)
C-------------------------------------------------------------------------------
C  @(#)pwrite.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C*	 Control output	devices.  
C*				 
C----------------------------------------------------------------------
      integer*2 kb(*), n, iwpc, lastblnk, ilen2
      character*160 ckb1
      integer*2 kb1(80)
      integer*4 long
c
      INCLUDE 'cio.inc'
      INCLUDE 'appl.inc'
c
      equivalence (ckb1, kb1)
c
      ckb1 = ' '
      call copy(min(iwpc(karlim),iwpc(n)), kb, kb1)
      ilen2 = lastblnk(ckb1)
c
      if (graphon) then
	if (sclchar .ne. 1.) call charsize(sclchar)
	call place(iglinex, igliney)
	call pchar(ckb1, ilen2)
	if (sclchar .ne. 1.) call charsize(1.)
	iglinex = 1
	igliney = igliney - sclchar*25
	if (igliney .lt. 0) igliney = 780
c	Write to the graphics screen and bump pointer to next line to write.
c	Change Char. Size. if needed.
c
      else
        write(iout, 200) ckb1(1:ilen2)
200     format(1x,a)
        if (iout .eq. istdout) call updateline(ckb1)
	call flush(long(iout))
c	Write to either an output file or to the screen.
c
      endif
c
      return
      end
c
