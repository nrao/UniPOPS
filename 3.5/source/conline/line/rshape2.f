      subroutine rshape2(ierr)
c
c     @(#)rshape2.f	5.1 06/22/94
c
c     Fits a sinusoid to Array (0).
c
c     IERR = 0 if all goes OK, else it will be an error code.
c
c     If RFIXPER > 0, the period is held fixed to the value in RPERIOD
c		= 0, not allowed
c 		< 0, uses the guess supplied in RPERIOD
c     If RFIXAMP > 0, the amplitude is held fixed to the value in RAMPLTDE
c		= 0, the algorithm will try to find a guess for RAMPLTDE
c 		< 0, uses the guess supplied in RAMPLTDE
c     If RFIXPHA > 0, the phase is held fixed to the value in RPHASE
c		= 0, the algorithm will try to find a guess for RPHASE
c 		< 0, uses the guess supplied in RPHASE
c
      integer*2 ierr
c
      integer*4 nca, ma
      parameter (nca = 3)
      parameter (ma = 3)
c
      real covar(NCA,NCA), alph(NCA,NCA), chisq, alamda, alamda1, 
     .     chisq1, lcoef(MA), twopi
      integer*4 ndata, numfit, numfit1, lista(MA), lista1(MA), i,
     .		linfit
      integer*2  fixper, fixamp, fixpha, kount
c
      external frip, fripa, fripab
c
      include 'cform.inc'
      include 'appl.inc'
      include 'core.inc'
      include 'rip.inc'
c
      parameter (twopi = 6.283185)
c
      ierr = 0
c
      fixper = nint(rfixper)
      fixamp = nint(rfixamp)
      fixpha = nint(rfixpha)
c
c     Let us check for errors in input conditions
c
      if (fixper .gt. 0 .and. fixamp .gt. 0 .and. fixpha .gt. 0) then
	ierr = 284
	goto 99
      endif
c     Cannot fix all three fitted parameters since nothing would be fit.
c
      if (rperiod .le. 0. .or. fixper .eq. 0) then
	ierr = 284
	goto 99
      endif
c     User must always supply an initial guess for rperiod.
c
      if ( abs(fixamp) .gt. 0 .and. rampltde .le. 0.) then
	ierr = 	284
	goto 99
      endif
      if ( abs(fixper) .gt. 0 .and. rperiod .le. 0.) then
	ierr = 	284
	goto 99
      endif
c     Must supply a guess or value if holding a parameter constant (rfix* = 1)
c	or if using the value of the adverb as an initial guess (rfix* = -1).
c       Phase can be < = 0 but amp and period cannot.  The second check should
c	never be reached due to the check above.
c
      CALL PREP (ierr)
      if (ierr .ne. 0) goto 99
c     Store in XDATA and YDATA the channels and data points set by either
c     BBASE,EBASE or by NREGION.
c
      ndata = ibase
      if ( ndata .lt. 4 ) then
	ierr = 	236
	goto 99
      endif
c     Make sure we have enough data points for a fit.
c
      do 173 i = 1, ndata
	work(i) = 1.
	xdata(i) = xdata(i) - idatoff
173	continue
c	Work willbe used to hold SIGMA of the data points all of which are 1.
c	XDATA is created by PREP with respect to start of TWH instead of in
c	channel number -- convert XDATA back to channel number. 
c
      linfit = 0
c     Number of parameters to fit for the two possible linear
c	least-square fits.  If zero, no linear fit is needed.
c
      numfit = 0
      numfit1 = 0
c     Number of parameters to fit for the two possible non-linear
c	least-square fits.  If zero, no non-linear fit is needed.
c
      coef(1) = rampltde
      coef(3) = twopi  / rperiod
      coef(2) = coef(3) * (rphase - 1)
c     The coefficients actually fitted for by the algorithms
c
      lista(1) = 1
      lista(2) = 2
      lista(3) = 3
      lista1(1) = 1
      lista1(2) = 2
      lista1(3) = 3
c     Default order of things to be fitted -- these will change as the user
c	picks various things to hold fixed or to fit.
c
      rpererr = 0.0
      ramperr = 0.0
      rphaerr = 0.0
      covar(1,1) = 0.0
      covar(2,2) = 0.0
      covar(3,3) = 0.0
c     Zero out the errors and the error part of the covariance matrix used by the
c	algorithms.
c
c-----------------------------
c
c     NOW!!!!  Check on the values of rfix* to determine how to go about doing
c	the fit.  If a linear LSQ fit is needed, set LFIT.  If
c	a non-linear fit is needed, prepare arrays and variables and perform
c	the fit below.  The array coef(1-3) corresponds to alpha, beta, and gamma
c	in the fitting function:  YDATA = alpha*cos(XDATA*gamma + beta)
c
c	Either we will have one linear, a linear followed by a non-linear,
c	or two non-linear fits.  No other combinations will be found.
c
c-----------------------------
      if (fixper .eq. 0) then
c	Fit with fixper = 0 is illegal.  This check should never be reached because
c	of the above error checks.
c
	ierr = 284
	goto 99
c
      else if (fixper .gt. 0) then
        if (fixamp .eq. 0) then
c
	  if (fixpha .eq. 0) then
c	     Fit with fixamp, phase, per = 0, 0, 1.
c	     Linear fit for alpha and beta.
c
	     linfit = 2
c
          else if (fixpha .gt. 0) then
c	     Fit with fixamp, phase, per = 0, 1, 1.
c	     Linear fit for alpha.
c
	     linfit = 1
c
	  else if (fixpha .lt. 0) then 
c	     Fit with fixamp, phase, per = 0, -1, 1.
c	     Linear fit for alpha and beta.  This is the same as 
c	     fixamp, phase, per = 0, 0, 1.
c
	     linfit = 2
c 
	  endif
c
        else if (fixamp .gt. 0) then
c
	  if (fixpha .eq. 0) then
c	     Fit with fixamp, phase, per = 1, 0, 1.
c	     Prepare for non-linear fit for beta using zero as an initial guess for
c	     beta.
c
	     numfit = 1
	     lista(1) = 2
	     lista(2) = 1
	     coef(2) = 0.0
c
          else if (fixpha .gt. 0) then
c	     Fit with fixamp, phase, per = 1, 1, 1 is illegal.  This check should 
c	     never be reached because of the above error checks.
c
	     ierr = 284
	     goto 99
c
	  else if (fixpha .lt. 0) then 
c	     Fit with fixamp, phase, per = 1, -1, 1.
c	     Preapre for non-linear fit for beta using the value of RPHASE as an
c	     initial guess.
c
	     numfit = 1
	     lista(1) = 2
	     lista(2) = 1
c
	  endif
c
        else if (fixamp .lt. 0) then
c
	  if (fixpha .eq. 0) then
c	     Fit with fixamp, phase, per = -1, 0, 1.
c	     Linear fit for alpha and beta.  This is the same as 
c	     fixamp, phase, per = 0, 0, 1.
c
	     linfit = 2
c
          else if (fixpha .gt. 0) then
c	     Fit with fixamp, phase, per = -1, 1, 1.
c	     Linear fit for alpha.  This is the same as 
c	     fixamp, phase, per = 0, 1, 1.
c
	     linfit = 1
c
	  else if (fixpha .lt. 0) then 
c	     Fit with fixamp, phase, per = -1, -1, 1.
c	     Linear fit for alpha and beta.  This is the same as 
c	     fixamp, phase, per = 0, 0, 1.
c
	     linfit = 2
c
	  endif
c
	endif
c
      else if (fixper .lt. 0) then
c
        if (fixamp .eq. 0) then
c
	  if (fixpha .eq. 0) then
c	     Fit with fixamp, phase, per = 0, 0, -1.
c	     First, find initial guess for alpha and beta using linear fit. 
c	     Then, prepare for non-linear fit for alpha, beta, and gamma. 
c
	     linfit = 2
	     numfit = 3
c 
          else if (fixpha .gt. 0) then
c	     Fit with fixamp, phase, per = 0, 1, -1.
c	     First, find initial guess for alpha using linear fit. 
c	     Then, prepare for non-linear fit for alpha and gamma. 
c
	     linfit = 1
	     numfit = 2
	     lista(2) = 3
	     lista(3) = 2
c
	  else if (fixpha .lt. 0) then 
c	     Fit with fixamp, phase, per = 0, -1, -1.
c	     First, find initial guess for alpha using linear fit. 
c	     Then, prepare for non-linear fit for alpha, beta, and gamma.
c 
	     linfit = 1
	     numfit = 3
c
	  endif
c
        else if (fixamp .gt. 0) then
c
	  if (fixpha .eq. 0) then
c	     Fit with fixamp, phase, per = 1, 0, -1.
c	     First, prepare first non-linear fit to find initial guess for beta. 
c	     Then, prepare for second non-linear fit for beta and gamma.
c 
	     numfit = 1
	     lista(1) = 2
	     lista(2) = 1
	     lista(3) = 3
	     coef(2) = 0.0
	     numfit1 = 2
	     lista1(1) = 2
	     lista1(2) = 3
	     lista1(3) = 1
c
          else if (fixpha .gt. 0) then
c	     Fit with fixamp, phase, per = 1, 1, -1.
c	     Prepare  for non-linear for for gamma.
c 
	     numfit = 1
	     lista(1) = 3
	     lista(3) = 1
c
	  else if (fixpha .lt. 0) then 
c	     Fit with fixamp, phase, per = 1, -1, -1.
c	     Prepare  for non-linear for for beta and gamma.
c 
	     numfit = 2
	     lista(1) = 2
	     lista(2) = 3
	     lista(3) = 1
c
	  endif
c
        else if (fixamp .lt. 0) then
c
	  if (fixpha .eq. 0) then
c	     Fit with fixamp, phase, per = -1, 0, -1.
c	     First, prepare first non-linear fit to find initial guess for beta. 
c	     Then, prepare for second non-linear fit for alpha, beta, and gamma.
c 
	     numfit = 1
	     lista(1) = 2
	     lista(2) = 1
	     lista(3) = 3
	     coef(2) = 0.0
	     numfit1 = 3
c
          else if (fixpha .gt. 0) then
c	     Fit with fixamp, phase, per = -1, 1, -1.
c	     Prepare  for non-linear for alpha and gamma.
c 
	     numfit = 2
	     lista(2) = 3
	     lista(3) = 2
c
	  else if (fixpha .lt. 0) then 
c	     Fit with fixamp, phase, per = -1, -1, -1.
c	     Prepare  for non-linear for alpha, beta, and gamma.
c 
	     numfit = 3
c
	  endif
c
	endif
      endif
c
c     Perform linear LSQ fit, if called for.
c
      if (linfit .eq. 1) then
	call lfit(xdata, ydata, work, ndata, lcoef, linfit, lista, linfit, 
     .	          covar, NCA, chisq, fripa)
	coef(1) = lcoef(1)
      else if (linfit .eq. 2) then
	call lfit(xdata, ydata, work, ndata, lcoef, linfit, lista, linfit, 
     .	          covar, NCA, chisq, fripab)
	coef(1) = sqrt( lcoef(1)**2 + lcoef(2)**2 )
	coef(2) = -atan2(lcoef(2), lcoef(1) )
      endif
c
c     Perform first non-linear LSQ fit, if called for.
c
      if (numfit .gt. 0) then
	alamda = -1.
        kount = 0
        call mrqmin(xdata, ydata, work, ndata, coef, MA, lista, 
     .		    numfit, covar, alph, NCA, chisq, frip, alamda)
c	
170     chisq1 = chisq
	alamda1 = alamda
        kount = kount + 1
	if (kount .le. niter) then
	   call mrqmin(xdata, ydata, work, ndata, coef, MA, lista, 
     .		       numfit, covar, alph, NCA, chisq, frip, alamda)
	   if ( (alamda .gt. 0. .and. alamda .lt. 1.e10) .and.
     .	       (alamda .gt. alamda1 .or. 
     .         abs(1.-chisq1/chisq) .gt. 0.001) ) goto 170
	else
	   ierr = 282
	endif
        alamda = 0.
        call mrqmin(xdata, ydata, work, ndata, coef, MA, lista, 
     .		    numfit, covar, alph, NCA, chisq, frip, alamda)
      endif
c
c     Perform second non-linear LSQ fit, if called for.
c
      if (numfit1 .gt. 0) then
	ierr = 0
	alamda = -1.
        kount = 0
        call mrqmin(xdata, ydata, work, ndata, coef, MA, lista1, 
     .		    numfit1, covar, alph, NCA, chisq, frip, alamda)
c	
180     chisq1 = chisq
	alamda1 = alamda
        kount = kount + 1
	if (kount .le. niter) then
	   call mrqmin(xdata, ydata, work, ndata, coef, MA, lista1, 
     .		       numfit1, covar, alph, NCA, chisq, frip, alamda)
	   if ( (alamda .gt. 0. .and. alamda .lt. 1.e10) .and.
     .	       (alamda .gt. alamda1 .or. 
     .         abs(1.-chisq1/chisq) .gt. 0.001) ) goto 180
	else
	   ierr = 282
	endif
        alamda = 0.
        call mrqmin(xdata, ydata, work, ndata, coef, MA, lista1, 
     .		    numfit1, covar, alph, NCA, chisq, frip, alamda)
      endif
c
c     Update adverbs and their errors but ONLY if they were fitted for.
c
      if (fixamp .le. 0) then
	rampltde = coef(1)
      	ramperr = sqrt(covar(1,1))
      endif
      if (fixpha .le. 0) then
	rphase = coef(2) / coef(3) + 1
        rphaerr = sqrt(covar(2,2)) / coef(3)
      endif
      if (fixper .le. 0) then
	rperiod = twopi / coef(3)
        rpererr = twopi * sqrt(covar(3,3)) / (coef(3)**2)
      endif
c
99    return
      end
c
c----------------------------------------------
      subroutine fripa(x, y, np)
c
c     Used by LFIT for finding alpha in:
c
c          alpha * cos(gamma*x + beta)
c
      real y(*), x
      integer*4 np
c
      include 'rip.inc'
c
      y(1) = cos( x*coef(3) + coef(2) ) 
      return
      end
c
c
c----------------------------------------------
      subroutine fripab(x, y, np)
c
c     Used by LFIT for finding alpha and beta in:
c
c          alpha * cos(gamma*x + beta), or
c
c	   alpha*cos(gamma*x)*sin(beta) + alpha*sin(gamma*x)*cos(beta)
c
      real y(*), x
      integer*4 np
c
      include 'rip.inc'
c
      y(1) = cos( x*coef(3) ) 
      y(2) = sin( x*coef(3) )
c 
      return
      end
c
c
c----------------------------------------------
      subroutine frip(x, a, y, dyda, na)
c
c     Used by MRQMIN for finding alpha, beta, or gamma in:
c
c          alpha * cos(gamma*x + beta), or
c
      real x, y, a(*), dyda(*)
      integer*4 na
c
      integer*2 n120, m3
c
      parameter (m3 = -3)
      parameter (n120 = 120)
c
      if ( na. ne. 3) 
     .	call oerror(n120, m3, 'FRIP: Wrong number of coefficients')
c
c
      y = a(1) * cos( a(3)*x + a(2) )
c
      dyda(1) = cos( a(3)*x + a(2) )
      dyda(2) = -sin( a(3)*x + a(2) )
      dyda(3) = -x*sin( a(3)*x + a(2) )
c
      return
      end
c
