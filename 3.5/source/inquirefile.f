      logical*2 function inquirefile(filename)
C-------------------------------------------------------------------------------
C  @(#)inquirefile.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Checks whether file FILENAME exists
c     RJM   March 1989
c
      logical*4 lexist
      character*(*) filename
c
      inquire(file=filename,exist=lexist)
      inquirefile = lexist
c
      return
      end

