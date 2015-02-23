      logical*2 function opened(filename)
C-------------------------------------------------------------------------------
C  @(#)opened.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     SUN version.  Checks whether file FILENAME is opened
c     RJM  March 1989
c
      logical*4 lexist
      character*(*) filename
c
      inquire(file=filename,opened=lexist)
      opened = lexist
c
      return
      end

