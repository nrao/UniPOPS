      subroutine ctrlcintr()
C-------------------------------------------------------------------------------
C  @(#)ctrlcintr.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Does some clean up operations if a Ctrl-C was hit
c
      include 'cio.inc'
      include 'appl.inc'
c
      if (igraphtype .eq. 2) call resetshm
c
      write(istderr,10) 
10    format(1x,10a)
      if (iintype .eq. istdin) then
       write(istderr,10) 'Ctrl-C Interrupt -- Use EXIT to exit properly'
      else
       write(istderr,10) 'Ctrl-C Interrupt'
       call oerror2
      endif
c
      call revertcolor
c     Revert colors to its currently defined value
c
      return
      end
c
