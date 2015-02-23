      subroutine mrqmin(x, y, sig, ndata, a, ma, lista, mfit, 
     .	                 covar, alpha, nca, chisq, funcs, alamda)
c
c      @(#)mrqmin.f	5.1 06/22/94
c
c     Levenberg-Marquardt method from Press et al "Numerical Recipes"
c     The following is directly from Press et al:
c     
c	Attempts to reduce the value of chi**2 of a fit between a set
c	of NDATA points X(I), Y(I) woth individual standard deviations
c	SIG(I), and a nonlinear function dependent on MA coeficiaents
c	A.  The array LISTA numbers the parameters A such that the
c	first MFIT elements corespond to values actually being
c	adjusted; the remaining MA-MFIT parameters are held fixed at
c	their input value.  The program returns current best-fit values
c	for the MA fit parameters A and chi**2, CHISQ. The arrays
c	COVAR(NCA,NCA), ALPHA(NCA,NCA) with physical dimensions NCA (>=
c	MFIT) are used as working space during most iterations.  Supply
c	a subroutine FUNCS(X,A,YFIT,DYDA,MA) that evaluates the fitting
c	function YFIT and its derivatives DYDA with respect to the
c	fitting parameters A at X.  On the first call provide an
c	initial guess for the parameters A, and set ALAMDA < 0 for
c	initialization (which then sets ALAMDA to 0.001).  If a step
c	succeeds, CHISQ becomes smaller and ALAMDA decreases by a
c	factor of 10.  If a step fails ALAMDA grows by a factor of 10.
c	You must call this routine repeatedly until convergence is
c	achieved.  Then, make one final call with ALAMDA=0 so that
c	COVAR(I,J) returns the covarience matrix and ALPHA(I,J) the
c	curvature matrix.  
c
      real x(*), y(*), sig(*), a(*), covar(nca, nca), alpha(nca, nca),
     .	   chisq, alamda
      integer*4 nca, ndata, ma, lista(*), mfit
c
      integer*4 mmax, kk, j, ihit, k, n1
      integer*2 n120, m3
c
      parameter (mmax=80)
      parameter (n120=120)
      parameter (m3=-3)
      parameter (n1=1)
c
      real atry(mmax), beta(mmax), da(mmax), ochisq
c
      save atry, beta, da, ochisq
c
      external funcs
c
      if (mmax .lt. ma) 
     .		call oerror(n120, m3, 'MRQMIN: Too many parameters to fit')
c
      if (alamda .lt. 0) then
c	Initialization
c
	kk = mfit + 1
	do 12 j = 1, ma
	    ihit = 0
	    do 11 k = 1, mfit
		if (lista(k) .eq. j) ihit = ihit + 1
11		continue
	    if (ihit .eq. 0) then
		lista(kk) = j
		kk = kk + 1
	    else if (ihit .gt. 1) then
		call oerror(n120, m3, 'MRQMIN: Improper permutation in LISTA')
	    endif
12	    continue
c	    Does LISTA containa proper permutation of the coefficiants.
c
	if (kk .ne. ma+1) call oerror(n120, m3, 'MRQMIN: Improper permutation in LISTA')
	alamda = 0.001
	call mrqcof(x ,y, sig, ndata, a, ma, lista, mfit, alpha, beta,
     .		    nca, chisq, funcs)
	ochisq = chisq
c
	do 13 j = 1, ma
	   atry(j) = a(j)
13	   continue
c
      endif
c
      do 15 j = 1, mfit
	do 14 k = 1, mfit
	   covar(j,k) = alpha(j,k)
14	   continue
	covar(j,j) = alpha(j,j)*(1.+alamda)
	da(j) = beta(j)
15	continue
c	Alter linearized fitting matrix by augmenting diagnol elements
c
      call gaussj(covar, mfit, nca, da, n1, n1)
c     Matrix solution
c
      if (alamda .eq. 0) then
	call covsrt(covar, nca, ma, lista, mfit)
	return
      endif
c     Once converged, evaluate covariance with ALAMDA=0
c
      do 16 j = 1, mfit
	atry(lista(j)) = a(lista(j)) + da(j)
16	continue
c
      call mrqcof(x, y, sig, ndata, atry, ma, lista, mfit, covar, da,
     .	          nca, chisq, funcs)
c
      if (chisq .lt. ochisq) then
c	Trial succedded.. attempt the new solution.
	alamda = 0.1*alamda
	ochisq = chisq
	do 18 j = 1, mfit
	   do 17 k = 1, mfit
		alpha(j,k) = covar(j,k)
17		continue
	   beta(j) = da(j)
	   a(lista(j)) = atry(lista(j))
18	   continue
      else
c	 Failure, increase ALAMDA and return.
	 alamda = 10.*alamda
	 chisq = ochisq
      endif
c
      return
      end
c
c----------------------------------------------------
c
      subroutine mrqcof(x, y, sig, ndata, a, ma, lista, mfit, alpha, 
     .		        beta, nalp, chisq, funcs)
c
c     Used by MRQMIN to evaluate the linearized fitting matrix ALPHA and vector BETA.
c
      integer*4 ndata, ma, lista(*), mfit, nalp
      real x(*), y(*), sig(*), a(*), alpha(nalp,nalp), beta(*)
c
      integer*4 mmax, i, j, k
      integer*2 n120, m3
c
      parameter (mmax=80)
c
      real dyda(mmax), sig2i, dy, wt, ymod, chisq
c
      external funcs
c
      if (mmax .lt. ma) 
     .		call oerror(n120, m3, 'MRQCOF: Too many parameters to fit')
c
      do 12 j = 1, mfit
	do 11 k = 1, j
	   alpha(j,k) = 0.
11	   continue
	beta(j) = 0.
12	continue
      chisq = 0.
c
      do 15 i = 1, ndata
	call funcs(x(i), a, ymod, dyda, ma)
	sig2i = 1/(sig(i)*sig(i))
	dy = y(i)-ymod
	do 14 j = 1, mfit
	   wt = dyda(lista(j))*sig2i
	   do 13 k = 1, j
		alpha(j,k) = alpha(j,k) + wt*dyda(lista(k))
13		continue
	   beta(j) = beta(j) + dy*wt
14	   continue
	chisq = chisq + dy*dy*sig2i
15	continue
c
      do 17 j = 2, mfit
	do 16 k = 1, j-1
	   alpha(k,j) = alpha(j,k)
16	   continue
17	continue
c
      return
      end
c
