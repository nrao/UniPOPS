c
c @(#)fshort.F	5.1 06/22/94
c
      integer*2 function fshort(longint)
c
c	buffer between program and short() function
c
#ifdef I2ON
      integer*2 longint
#else
      integer*4 longint
#endif
c
      integer*4 temp
      integer*2 short
c
      temp = longint
      fshort = short(temp)
c
      return
      end
