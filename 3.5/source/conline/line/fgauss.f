      subroutine fgauss(x, a, y, dyda, na)
c
c     @(#)fgauss.f	5.1 06/22/94
c
c     Calculates sum of (NA-3)/3 gaussians Y using parameters A(NA).
c     Calculates partial derivatives DYDA(NA) of Y with respect to each A(I)
c
c     Equation to be calculated:
c	
c	 N
c	____
c	\      	
c	 \       		        x - x(i) - X0 
c	 /   H0 * h(i) *   exp  { - [  ---------------- ]**2 }
c	/        		       1.41 * s(i) * S0
c	----
c	i = 1
c
c       where:	A(1) = H0
c		A(2) = X0
c		A(3) = S0
c		A[3*i+1] = h(i)
c		A[3*i+2] = x(i)
c		A[3*i+3] = s(i)
c		N = (NA-3)/3
c
c----------------------------------------------------------------
c
      real x, y, a(*), dyda(*)
      integer*4 na
c
      real constant, ex, denom, fact, dy
      integer*4 i
      integer*2 n120, m3
c
      parameter (constant = 1.41421356237)
      parameter (n120 = 120)
      parameter (m3 = -3)
c
      if ( mod(na,3) .ne. 0 .or. na. lt. 3) 
     .		call oerror(n120, m3, 'FGAUSS: Wrong number of coefficients')
c
      dyda(1) = 0.
      dyda(2) = 0.
      dyda(3) = 0.
      y = 0.
c
      do 11 i = 4, na-1, 3
	denom = constant*a(i+2)*a(3)
	fact = ( x-a(i+1)-a(2) ) / denom
	ex = exp( -fact*fact )
	dy = a(i)*a(1)*ex
	y = y + dy
  	dyda(i) = a(1) * ex
	dyda(i+1) = 2. * dy * fact / denom
	dyda(i+2) = 2. * dy * fact*fact / a(i+2) 
  	dyda(1) = dyda(1) + a(i)*ex
	dyda(2) = dyda(2) + 2. * dy * fact / denom  
	dyda(3) = dyda(3) + 2. * dy * fact*fact / a(3)   
11	continue
c
      return
      end
c
c------------------------------------------------------------------
c
      subroutine fgauss0(x, afunc, na)
c
c     Calculates AFUNC(NA) values for NA Gaussians at X.
c
c     Equation to be calculated:
c	      	
c	        	          x - x(i) - X0 
c	  h(i)* H0 * exp  { - [  ---------------- ]**2 }
c	        	         1.41 * s(i) * S0
c
c
c       where:	COEF(1) = H0
c		COEF(2) = X0
c		COEF(3) = S0
c		COEF[3*i+1] = h(i)
c		COEF[3*i+2] = x(i)
c		COEF[3*i+3] = s(i)
c		I = 1,2,...,NA
c
c----------------------------------------------------------------
c
      real x, afunc(*)
      integer*4 na
c
      real constant, xi, si
      integer*4 i
      integer*2 n120, m3
c
      include 'rip.inc'
c
      parameter (constant = 1.41421356237)
      parameter (n120 = 120)
      parameter (m3 = -3)
c
      if (na .gt. ing .or. na .lt. 1) 
     .		call oerror(n120, m3, 'FGAUSS: Wrong number of coefficients')
c
      do 11 i = 1, na
	xi = x - coef(3*i+2) - coef(2)
	si = constant * coef(3*i+3) * coef(3)
	afunc(i) = coef(1) * coef(3*i+1) * exp(- ( xi / si)**2 )
11	continue
c
      return
      end
