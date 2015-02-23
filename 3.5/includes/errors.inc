C****************************************************
c* [RJM] 8903
c****************************************************
C-------------------------------------------------------------------------------
C  @(#)errors.inc	5.1 06/22/94
C-------------------------------------------------------------------------------
      logical erron, amath
c
      integer*2 numdivision, numunderflow, numoverflow, numinvalid,
     .		numothererr, loclasterr

c
      COMMON /ERRORS/ erron, amath, numdivision, numunderflow, 
     .		      numoverflow, numinvalid, numothererr, 
     .		      loclasterr
c
c     amath = Whether or not array math is in affect.
c     erron = Whether or not severe errors will terminate
c	the command being executed.
c     numdivision, num.... = number of floating point exceptions.
c     numothererr = number of other than the above errors
c     loclasterr = type of last error.
c
