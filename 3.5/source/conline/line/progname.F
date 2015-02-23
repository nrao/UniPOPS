      subroutine progname(char2)
C-------------------------------------------------------------------------------
C  @(#)progname.F	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Returns the initial for the program being run (e.g., L, C, or G)'
c
      character*2 char2
c
#ifdef CONTINUUM
      char2 = 'Cc'
#else
      char2 = 'Ll'
#endif
c
      return
      end
c

