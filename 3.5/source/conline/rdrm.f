      subroutine rdrm(junit, nchars, iout)
C-------------------------------------------------------------------------------
C  @(#)rdrm.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c Code used to read from TEKTRONIX like graphics screens when they are in 
c	cursor mode
c***********************************************
c 8904 [RJM]
c***********************************************
c
      character*1 iout(*), ctemp
      integer*2 junit, nchars, i
      integer*4 fgetc, jjunit, irtn
c
      jjunit = junit
      do 100 i = 1, nchars
         irtn = fgetc(jjunit, ctemp)
         iout(i) = ctemp
100      continue
      return
      end
c
