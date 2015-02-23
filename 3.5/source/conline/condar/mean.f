      SUBROUTINE MEAN (ISTART,ISTOP,XDATA,IDBE,TYPSCN,XMEAN,XRMS)
c--------------------------------------------
c   @(#)mean.f	5.4 04/07/95
c
c     This is designed exclusively for 12-m On/Off continuum data.
c     It expects the data to be in groups of 4 values: Off, On, On, Off
c     Each set of 4 from ISTART to ISTOP in XDATA is combined into a
c     single (On - Off) which is then used to calculate the mean (On-Off)
c     and the rms around the mean.
c
c--------------------------------------------
c
      real xdata(*), typscn, xmean, xrms, sumy, sumy2, yvalue, rpt
      integer*2 istart, istop, idbe
      integer*4 i
      logical okreal4
c
      RPT=0
      sumy=0.0
      sumy2=0.0
      DO 122 I = ISTART,ISTOP,4
	 if (okreal4(xdata(i)) .and. okreal4(xdata(i+1)) .and.
     .       okreal4(xdata(i+2)) .and. okreal4(xdata(i+3))) then
            yvalue=(-XDATA(I)+XDATA(I+1)+XDATA(I+2)-XDATA(I+3))/2.0
            sumy = sumy + yvalue
            sumy2 = sumy2 + yvalue*yvalue
	    rpt = rpt + 1.0
	 endif
  122    CONTINUE
      xmean = 0.0
      xrms = 0.0
      if (rpt .gt. 0) then
         XMEAN=sumy/rpt
         if (rpt .gt. 1) then
            xrms = sqrt( (sumy2-sumy*sumy/rpt)/(rpt*(rpt-1)) )
         endif
      endif
c             I don't understand why the following is necessary - rwg
      if (idbe .lt. 0 .and. typscn .gt. 0) then
         xmean = xmean / typscn
         xrms = xrms / typscn
      endif
c
      RETURN
      END
