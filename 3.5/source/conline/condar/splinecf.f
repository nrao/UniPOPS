      subroutine splinecf( data, coef, work, num)
c
c     @(#)splinecf.f	5.1 06/22/94
c
c     Calculates spline coefficients for data in DATA and stores results
c     in coef.  work is a temporary array and num is the number of
c     data points. ier = 0 if all goes OK.
c
c     Natural cubic spline using algorithm of "Num. Recipes" as a starting
c     point.  Algorithm suggested by Harvey Liszt. 
c
      real data(*), coef(*), work(*)
      integer*2 num
      logical okreal4
c
      integer*2 ier,n
c
      if (num .lt. 5) then
	ier = -1
	goto 99
      endif
c     Need at least 5 data points for spline fit
c
      work(1) = 0.0
      coef(1) = 0.0
c
      do 100 n = 2, num
	if (okreal4(data(n)) .and. okreal4(data(n+1)) .and.
     .      okreal4(data(n-1))) then
 	 coef(n) = 1. / (coef(n-1) + 4.0)
         work(n) = coef(n)*(work(n-1) - 
     .            6.*(data(n+1)-2.*data(n)+data(n-1)))
	else
	 coef(n) = coef(n-1)
	 work(n) = work(n-1)
	endif
c	This may not be mathematically correct.
c
100	continue
c
      coef(num) = 0.0
      do 110 n = num-1, 1, -1
	coef(n) = coef(n)*coef(n+1)+work(n)
110	continue
c
      ier = 0
c
99    continue
c
      return
      end
c
