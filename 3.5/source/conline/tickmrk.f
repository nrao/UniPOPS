      subroutine tickmrk(f1, f2, maxtick, x1, x2, ddx, numtick)
c
c----------------------------------------
c  @(#)tickmrk.f	5.1 06/22/94
c----------------------------------------
c
c     Calculates aesthetic tick marks for plots.
c     F1, F2 = (R*4) end points of plots (INPUT)
c     MAXTICK = (I*2) Maximum number of tickmarks to be allowed.
c     X1, X2 = (R*4) first and last tick mark to be used (OUTPUT)
c     DDX = (R*4) increment between tickmarks
c     NUMTICK = (I*2) number of tick marks to be drawn

      real test(13), f1, f2, x1, x2, ddx, r1, r2, a1, a2, a3, adx, 
     1     xtemp, dx
      integer*2 numtick, i, maxtick
c
      data test/.001,.002,.004,.005,.01,.02,.04,.05,.1,.2,.4,.5,1.0/
c
      r1 = min(f1,f2)
      r2 = max(f1,f2)
c
      dx = r2 - r1
      if (dx .eq. 0) then
	x1 = f1
	x2 = f2
	numtick = 0
	ddx = 0.
      else
c
         a1 = log10(dx)
         a2 = aint(a1)
         a3 = 10.**a2   
         adx = dx / a3
c
         do 20 i = 1, 13
	   numtick = adx/test(i)
	   if (numtick .le. maxtick) goto 25
20	   continue
         i = 13
c
25       ddx = test(i)*a3
         x1 = ddx * anint((r1 + ddx) / ddx)
c
26	 continue
         if (x1 - ddx .ge. r1) then
		x1 = x1 - ddx
		goto 26
	 endif
c
         x2 = ddx * anint((r2 - ddx) / ddx)
c
27	 continue
         if (x2 + ddx .le. r2) then
	    	x2 = x2 + ddx
		goto 27
	 endif
c
         numtick = max(1, 1 + nint((x2-x1)/ddx) ) 
c
         if (f1 .gt. f2) then
	   ddx = -ddx
	   xtemp = x2
	   x2 = x1
	   x1 = xtemp
         endif
c
      endif
c
      return
      end
c
