      subroutine fitfoc(nyqrate, dishdia, focalpt)
c
c     @(#)fitfoc.f	5.2 09/10/98
c
c     Finds the best location of the focus from the phase information
c     Prints out results.  Subtracts fit from the data in Matrix 0 and
c     places the model into Matrix 1.
c
      real nyqrate, dishdia, focalpt
c
      integer*2 m2, n256, mat0x, mat0y, i, j, n120, n0, istch(40),
     .		iblank, niter, n282, kount, lastblnk
      character*80 stch
      character*2 blank
      integer*4 NCA, kin, ndata, MFIT, MA, ilct
c
      parameter (NCA = 7)
c
      integer*4 lista(NCA)
      real coef(NCA), covar(NCA,NCA), dx, dy, theta, thetamax, alamda,
     .     alamda1, chisq, chisq1, alph(NCA,NCA), value, dyda(NCA), xc,
     .	   yc, PRCERR
      character*32 clab(NCA)
c
      external holfunc

      parameter (MA = 7)
      parameter (MFIT = 6)
      parameter (PRCERR = 0.01)
      parameter (M2 = -2)
      parameter (N0 = 0)
      parameter (N256 = 256)
      parameter (N282 = 282)
      parameter (N120 = 120)
      parameter (NITER = 12)
c
      equivalence (blank,iblank), (stch,istch)
c
      INCLUDE 'appl.inc'
      INCLUDE 'mappl.inc'
      INCLUDE 'mform.inc'
      INCLUDE 'cform.inc'
c
      data blank/'  '/
      data clab/'Bias in Phase',
     .          'Y Pointing Offset',
     .          'X Pointing Offset',
     .		'Lateral Focus Offset',
     .		'Angle of Lat. Offset',
     .		'Radial Focus Offset',
     .		'Focal Length'/
c
      mat0x = nint(mhead(mnaxis1,1))
      mat0y = nint(mhead(mnaxis2,1))
      xc = float(mat0x) / 2. + 0.5
      yc = float(mat0y) / 2. + 0.5
c
      if  (mat0x .le. 0 .or. mat0y .le. 0 .or. mat0x*mat0y .gt.
     .     mdatasize/mnumarrays) call oerror(n256, m2, 'HOLFITFOC')
c     Make sure the input matrix has been initialized
c
      do 43 i = 1, mheadsize
	mhead(i,2) = mhead(i,1)
43	continue
c     Copy header
c
      dx = dishdia / (nyqrate*mat0x)
      dy = dishdia / (nyqrate*mat0y)
      thetamax = 2. * atan( dishdia / (4.*focalpt) )
c
      kin = idatoff + 1
c
      do 100 i = 1, mat0x
	do 50 j = 1, mat0y
	   if (mdata(ilct(i,j,1)) .ne. sngl(mhead(mblank,1)) ) then
		twh(kin,1) = dx * (i-xc)
		twh(kin,2) = dy * (j-yc)
		theta = 2. * atan ( sqrt(twh(kin,1)**2 + twh(kin,2)**2 ) /
     .				(2. * focalpt) )
		if (theta .le. thetamax) then
		   work(kin-idatoff) = 1.0
		   xdata(kin-idatoff) = kin
		   ydata(kin-idatoff) = mdata(ilct(i,j,1))
		   kin = kin + 1
		   if (kin .ge. MAX_DATA_POINTS) goto 101
		endif
	   endif
50	   continue
100	continue
c
101   ndata = kin - idatoff - 1
c
      if (ndata .le. MFIT) call oerror(n120, m2, 'HOLFITFOC')
c
      if (ndata .eq. MAX_DATA_POINTS) call oerror(n120, n0, 'HOLFITFOC')
c
      do 102 i = 1, MFIT
	coef(i) = 0
	lista(i) = i
102	continue
      coef(4) = 1.
      coef(NCA) = focalpt
c
      alamda = -1
      kount  = 0
c
      call mrqmin(xdata, ydata, work, ndata, coef, MA, lista, MFIT, 
     .	                 covar, alph, NCA, chisq, holfunc, alamda)
c
180     chisq1 = chisq
	alamda1 = alamda
        kount = kount + 1
	if (kount .le. niter) then
	   call mrqmin(xdata, ydata, work, ndata, coef, MA, lista, MFIT, 
     .	                 covar, alph, NCA, chisq, holfunc, alamda)
	   if (alamda .gt. alamda1 .or. 
     .         abs(1.-chisq1/chisq) .gt. PRCERR) goto 180
	else
	   call oerror(n282, n0, 'HOLFITFOC')
	endif
	alamda = 0.
        call mrqmin(xdata, ydata, work, ndata, coef, MA, lista, MFIT, 
     .	                 covar, alph, NCA, chisq, holfunc, alamda)
c
      stch = 'Covariant Matrix:         1    2    3    4    5    6'
      call pwrite(istch, 80)
c
      do 502 i =1, MFIT
	   write(stch,209) i, (nint(100.*
     .		(covar(i,j)/sqrt(covar(i,i)*covar(j,j)))),j=1,i)
209	   format(17x,i4,':',20i4)
	   call pwrite(istch,80)
502	   continue
      call pwrite(iblank,1)
c
      do 4999 i = 1, MFIT
          write(stch,500) clab(i)(1:lastblnk(clab(i))), coef(i),
     .			  sqrt(covar(i,i))
500	  format(a,':', t32, 1pg15.6, ' +/-', g15.6)
	  call pwrite(istch, 80)
4999	  continue
      call pwrite(iblank,1)
c
      write(stch,410) kount, chisq, ndata
410   format('Iterations:',i4, 5x,  'Chi Square:',1pg12.5, 5x,
     .	     'No. Points:',i6)
      call pwrite(istch, 80)
      call pwrite(iblank,1)
c
      do 600 i = 1, mat0x
	do 550 j = 1, mat0y
		twh(1+idatoff,1) = dx * (i-xc)
		twh(1+idatoff,2) = dy * (j-yc)
		call holfunc(float(idatoff+1),coef, value, dyda, ma)
c
		mdata(ilct(i,j,1)) = mdata(ilct(i,j,1)) - value
		mdata(ilct(i,j,2)) = value
c
550		continue
600	continue
c
      return
      end
c
      subroutine holfunc (xindex, a, value, dyda, na)
c
c     Describes Non-linear equation for phase variations across a
c     dish due to feed position error (From Godwin et al., 1978
c     Electronics Letters, Vol. 14, p. 134,)
c
c     xindex  = index to independent variables
c     a = array of parameters
c	  a(1) = a
c	  a(2) = b
c	  a(3) = c
c	  a(4) = (2*pi/lambda)*deltat
c	  a(6) = (2*pi/lambda)*deltaz
c	  a(5) = phi prime
c	  a(7) = focal length ( a convenient way of passing the focal
c			length ).
c     value = value of function at x
c     dyda = partial derivitives of y with respect to a's
c     na = number of parameters (= 6 here)
c
      integer*4 na
      real value, a(na), dyda(na), xindex
c
      real x, y, theta, sinr, cosr, r
      integer*2 n120, m3
      integer*4 ix
c
      parameter (n120 = 120)
      parameter (m3 = 3)
c
      include 'appl.inc'
c
      if (na .lt. 1 .or. na .gt. 7) call oerror(n120, m3, 'HOLFUNC')
c
      ix = nint(xindex)
      x = twh(ix,1)
      y = twh(ix,2)
      theta = atan2(y,x)
      r = 2 * atan( sqrt(x**2 + y**2)/(2*a(7)) )
      sinr = sin(r)
      cosr = cos(r)
c
      value = a(1) + a(2)*x + a(3)*y + a(4)*sinr*cos(theta-a(5))
     .		 + a(6)*cosr
c
      dyda(1) = 1.
      dyda(2) = x
      dyda(3) = y
      dyda(4) = sinr * cos(theta - a(5))
      dyda(5) = a(4)*sinr*sin(theta-a(5))
      dyda(6) = cosr
      dyda(7) = 0.0
c
      return
c
c
      end
c
c
