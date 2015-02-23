      subroutine dummy (J)
C-------------------------------------------------------------------------------
C  @(#)gendummy.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c----------------------------------------------
c  Dummy subroutines needed for GENMEMORY.  Should never be called
c  [RJM] 8903
c---------------------------------------------
c
      integer*2 j, stch(40), istat
      integer*2 n80
      character*80 cstch
      data n80 /80/
c
      equivalence (stch, cstch)
c
      entry helps(j)
c
      write(cstch,10,iostat=istat) 
10    format('HELPS:  You should not be here... Get HELP')
      call pwrite(stch, n80)
c
      entry gpflush
c
      return
c
      end
c
