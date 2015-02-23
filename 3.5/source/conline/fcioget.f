      integer*4 function fcioget(string)
C
C @(#)fcioget.f	5.2 31 Jan 1995
C
C   reads from istdin (in cio.inc) a string.  Returns the length of the string
C   and null terminates the string
C
      character*120 string
      integer*2 lastblnk, len
c
      include 'cio.inc'
c
      read(istdin, 1000) string
      len = lastblnk(string)
c
      if (len .ge. 0) then
         string(len+1:len+1) = "\0"
      else
         string(1:1) = "\0"
      endif
c
      fcioget = len
      return
c
 1000 format(a)
      end
