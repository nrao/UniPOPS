      integer*2 function lastblnk(string)
C-------------------------------------------------------------------------------
C  @(#)lastblnk.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Finds the position of the last non-blank character in STRING
c*********************************************
c     [RJM] March 89
c*********************************************
c
      character*(*) string
      integer*4 lnblnk
c
c***********************************************************
      lastblnk = lnblnk(string)
c     MASSCOMP/SUN function LNBLNK
c
      return
      end
c
