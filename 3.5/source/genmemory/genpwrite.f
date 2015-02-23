      SUBROUTINE PWRITE	(KB,N)
C-------------------------------------------------------------------------------
C  @(#)genpwrite.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C*	 Control output	devices.  
C*				 
c          8903   [RJM] See CHANGES.DOC
C----------------------------------------------------------------------
      integer*2 kb(*), n, iwpc, lastblnk, ilen2
      character*160 ckb1
      integer*2 kb1(80)
c
      INCLUDE 'cio.inc'
c
      equivalence (ckb1, kb1)
c
      if (iout .le. 0) iout = istdout
      ckb1 = ' '
      call copy(min(iwpc(karlim),iwpc(n)), kb, kb1)
      ilen2 = lastblnk(ckb1)
c
      write(iout, 200) ckb1(1:ilen2)
200   format(1x,a)
c
      return
      end
c
