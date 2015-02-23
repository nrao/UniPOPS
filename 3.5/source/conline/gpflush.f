      subroutine gpflush
c
c     @(#)gpflush.f	5.1 06/22/94
c
c     Flushes out any pending graphics commands
c
      include 'cio.inc'
c
      if (igraphtype .eq. 2) call coreflush
c
      return
      end
c
