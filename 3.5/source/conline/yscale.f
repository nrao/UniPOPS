      SUBROUTINE YSCALE (Y,LOW,HIGH,ymin,INCR)
C-------------------------------------------------------------------------------
C  @(#)yscale.f	5.1 06/22/94
C-------------------------------------------------------------------------------
      REAL ymin,INCR,Y(*), vmax, vmin
      INTEGER*2 HIGH,LOW, i
      logical okreal4, something
C                                       Find max and min.
      VMAX=-1.e30
      VMIN=1.e30
      something = .false.
      DO 100 I=low,high
         if (okreal4(y(i))) then
      		something = .true.
		vmin = min(vmin, y(i))
		vmax = max(vmax, y(i))
	 endif
100      CONTINUE
c
      if (something) then
	call scal(vmax, vmin, ymin, incr)
      else
	ymin = 1.e30
	incr = 1.e30
      endif
c
      RETURN
      END

