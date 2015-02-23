c********************************************
c* [RJM] 8903
c********************************************
C-------------------------------------------------------------------------------
C  @(#)ambig.inc	5.1 06/22/94
C-------------------------------------------------------------------------------
      integer*2 napp, mname(5,250)
c
      COMMON /AMBIG/  NAPP, MNAME
c     
c     Set by MHUNT and used by ERRORS for trapping ambiguous
c     Min-matched strings.  
c     NAPP =Number of ambiguous strings
c     MNAME = Array containing the ambiguous strings
c
