      subroutine gbmean(xdata, npts, smean, srms)
c
c     @(#)gbmean.f	5.1 06/22/94
c
c     Calculates the mean and rms for a Green Bank on-off scan
c
c     xdata = array containing data points
c     npts = number of data points to use from array
c     smean = returns the mean value
c     srms = return the rms fluctuations
c
      real*4 xdata(*), smean, srms
      integer*2 npts
      logical okreal4
c
      integer*2 i, npairs
      real*4 offs
c
      offs = 0.0
      npairs = 0
      do 111 i = 1, npts-2, 2
	 if (okreal4(xdata(i))) then
	   offs = offs +  xdata(i)
           npairs = npairs + 1
	 endif
111	 continue
      if (npairs .gt. 0) offs = offs / npairs
c
      smean = 0.0
      npairs = 0
      do 112 i = 2, npts-3,  2
	  if (okreal4(xdata(i))) then
		smean = smean + xdata(i)
                npairs = npairs + 1
	  endif
112	  continue
      if (npairs .gt. 0) smean = smean / npairs - offs
c
      srms = 0.0
      npairs = 0
      do 113 i = 2, npts-3,  2
	 if (okreal4(xdata(i))) then
		srms = srms + (xdata(i) - offs - smean)**2
                npairs = npairs + 1
	 endif
113	 continue
      if (npairs .gt. 1) srms = sqrt(srms / (npairs*(npairs-1)))
c
      return
      end
c

