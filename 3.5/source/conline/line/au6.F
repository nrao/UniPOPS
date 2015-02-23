      SUBROUTINE AU6 (J)
C-------------------------------------------------------------------------------
C  @(#)au6.F	5.4 04/14/96
C-------------------------------------------------------------------------------
c
c      au6 - Gmodel, Gparts, Residual, Gdisplay, Gauss
c
C---------------------------------------------------------------------
c
      integer*2 mmax
      parameter (mmax=80)
      integer*2 j, iptwh, i, istrt, istp, jj,  sclc,
     .          kount, ierr, ix, iystrt, iy, ier,  ipixel, lxa1
      integer*2 m2, m3, n0, n1, n80, n120, n225, n289, n290, n298
      integer*2 n25, n780, n272, n295, n282, n33
      logical isfirst, okarray, okreal4
      REAL hw, cnt, s, ajj,covar(mmax,mmax),lcoef(mmax),
     .	   alph(mmax,mmax), chisq, alamda, alamda1, chisq1, constant
      real*8 chantox
      integer*4 ndata, ma, mfit, nca, mhold, lista(mmax)
c
      external fgauss
      external fgauss0
c
      parameter (constant = 2.35482004504)
c     Conversion factor from rms width to FWHM
c
      include 'core.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
      INCLUDE 'cio.inc'
      INCLUDE 'rip.inc'
c
      data m2, m3, n0, n1, n80, n120, n225, n272, n289, n290, n295,
     .     n282, n298
     .    /-2, -3, 0, 1, 80, 120, 225, 272, 289, 290, 295, 282, 298/
      data n25, n780, n33 /25, 780, 33/
C
      sclc(ipixel) = max(n1, nint(sclchar*float(ipixel))) 
C
C=======================================================================
c
      IPTWH=1
      iystrt = iym - sclchar*60
c
      ING=nint(NGAUSS)
c
      JJ=J
      GO TO (161,163,164,165,170), JJ
      call oerror(n120, m3, 'AU6')
C--------------------------------------------------------------------
C							 GMODEL
C--------------------------------------------------------------------
 161  CONTINUE
      if (ing.lt.1 .or. ing.gt.2*maxmrk) call oerror(n289,m2,'GMODEL')
      do 179 i = 1, ing
	if(hwidth(i).le.0.) call oerror(n290, m2, 'GMODEL')
179	continue
c
      ISTRT = DTWH(C12SPN,IPTWH)
      ISTP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH) - 1
c
      if (istrt .ge. istp) call oerror(n225, m2, 'GMODEL')
c
      DO 60 I=ISTRT,ISTP
         S=0.0
         DO 61 JJ=1,ING
            S=S+HEIGHT(JJ)*EXP(max(-85., 
     .             -2.772*((float(i)-center(jj))/HWIDTH(JJ))**2))
   61       CONTINUE
         TWH(I+idatoff,IPTWH)=S
   60    CONTINUE
c
      GO TO 99
C------------------------------------------------------------------
C							GPARTS
C--------------------------------------------------------------------
  163 CONTINUE
      if (showplot(numplots) .ne. 0) call oerror(n289, m2, 'GPARTS')
      if (ing.lt.1 .or. ing.gt.2*maxmrk) call oerror(n289,m2,'GPARTS')
      do 209 i = 1, ing
	if(hwidth(i).le.0.)  call oerror(n290, m2, 'GPARTS')
209	continue
c
c
      ISTRT = DTWH(C12SPN,IPTWH)
      ISTP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH) - 1
c
      if (istrt .ge. istp) call oerror(n225, m2, 'GPARTS')
c
      lxa1 = lxa
      if (.not. okarray(lxa, dtwh(1,iptwh)) ) then
         call oerror(n272, n0, 
     .		'GPARTS: Units of X-axis will be channels or samples')
	 lxa1 = 1
      endif
c
      if (slabel .gt. 0.0) then
	 if (sclchar .ne. 1.) call charsize(sclchar)
         DO 1650 I=1,ING
	    cnt = chantox(lxa1, dtwh(1,iptwh), center(i), ierr)
	    hw=dabs(chantox(lxa1,dtwh(1,iptwh),center(i)+hwidth(i)/2,ierr)
     .       - chantox(lxa1,dtwh(1,iptwh),center(i)-hwidth(i)/2,ierr) )
            WRITE (CPUF,9601,IOSTAT=IER) HEIGHT(I), HW,  CNT
	    call place(ix0,iystrt - (i-n1)*sclc(n25))
	    call pchar(ipuf, n80)
 1650       CONTINUE
c
         WRITE (CPUF,9603,IOSTAT=IER) SUMSQ
         call place(ix0,iystrt - ing*sclc(n25))
	 call pchar(ipuf, n80)
	 if (sclchar .ne. 1.) call charsize(1.)
      endif
c
      DO 171 I = 1,ING
         isfirst = .true.
         DO 172 ix = ix0, ixm, 2
	    ajj = (float(ix)-bx(numplots)) / ax(numplots)
            S=HEIGHT(I)*EXP(max(-85.,
     .                -2.772*((ajj-center(i))/HWIDTH(I))**2))
            IY=min(max(iy0,nint(AY(numplots)*s+BY(numplots))), iym)
            if (nint(ajj) .ge. isbg) then
               if (isfirst) then
	          call placewp(ix, iy, n33)
                  isfirst = .false.
               else
	          CALL vctrwp(IX, IY, n33, sclchar, 0.)
               endif
	    endif
  172    continue
  171 CONTINUE
c
      call place(n0,n780 - n25*(inline-n1))
      call pchar('',n0)
c
      GO TO 99
C--------------------------------------------------------------------
C							RESIDUAL
C--------------------------------------------------------------------
  164 CONTINUE
      if (ing .lt. 1 .or. ing .gt. 2*maxmrk) 
     .		call oerror(n289,m2,'RESIDUAL')
      do 189 i = 1, ing
	if(hwidth(i).le.0.) call oerror(n290, m2, 'RESIDUAL')
189	continue
c
      ISTRT = DTWH(C12SPN,IPTWH)
      ISTP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH) - 1
c
      if (istrt .ge. istp) call oerror(n225, m2, 'RESIDUAL')
c
      DO 181 I = ISTRT,ISTP
         S=0.0
         DO 182 JJ = 1,ING
            S=S+HEIGHT(JJ)*EXP(max(-85.,
     .          -2.772*((float(i)-center(jj))/HWIDTH(JJ))**2))
  182       CONTINUE
         if (okreal4(TWH(I+idatoff,IPTWH)))
     .		TWH(I+idatoff,IPTWH)=TWH(I+idatoff,IPTWH)-S
 181     continue
c
      GO TO 99
C-------------------------------------------------------------------
C                                     GDISPLAY
C-------------------------------------------------------------------
  165 CONTINUE
      if (showplot(numplots) .ne. 0) call oerror(n298, m2, 'GDISPLAY')
      if (ing .lt. 1 .or. ing .gt. 2*maxmrk) 
     .		call oerror(n289,m2,'GDISPLAY')
      do 199 i = 1, ing
	if(hwidth(i).le.0.) call oerror(n290, m2, 'GDISPLAY')
199	continue
c
c
      ISTRT = DTWH(C12SPN,IPTWH)
      ISTP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH) - 1
c
      if (istrt .ge. istp) call oerror(n225, m2, 'GDISPLAY')
c
      lxa1 = lxa
      if (.not. okarray(lxa, dtwh(1,iptwh)) ) then
         call oerror(n272, n0, 
     .		'GDISPLAY: Units of X-axis will be channels or samples')
	 lxa1 = 1
      endif
c
      if (slabel .gt. 0.0) then
	 if (sclchar .ne. 1.) call charsize(sclchar)
         DO 1654 I=1,ING
	    cnt = chantox(lxa1, dtwh(1,iptwh), center(i), ierr)
	    hw=dabs(chantox(lxa1,dtwh(1,iptwh),center(i)+hwidth(i)/2,ierr)
     .	     - chantox(lxa1,dtwh(1,iptwh),center(i)-hwidth(i)/2,ierr) )
            WRITE (CPUF,9601,IOSTAT=IER) HEIGHT(I), HW,  CNT
	    call place(ix0,iystrt - (i-n1)*sclc(n25))
	    call pchar(ipuf, n80)
 1654    CONTINUE
c
         WRITE (CPUF,9603,IOSTAT=IER) SUMSQ
         call place(ix0,iystrt - ing*sclc(n25))
	 call pchar(ipuf, n80)
	 if (sclchar .ne. 1.) call charsize(1.)
      endif
c
      isfirst = .true.
      DO 1651 ix = ix0, ixm, 2
	 ajj = (float(ix)-bx(numplots)) / ax(numplots)
         S=0.0
	 DO 1652 JJ=1,ING
	    S=S+HEIGHT(JJ)*EXP(max(-85.,
     .               -2.772*((ajj-center(jj))/HWIDTH(JJ))**2))
 1652       CONTINUE
         IY=min(max(iy0,nint(AY(numplots)*s+BY(numplots))), iym)
         if (nint(ajj) .ge. isbg) then
            if (isfirst) then
	       call placewp(ix, iy, n33)
               isfirst = .false.
            else
	       CALL vctrwp(IX, IY, n33, sclchar, 0.)
            endif
	 endif
 1651 CONTINUE
c
      call place(n0,n780 - n25*(inline-n1))
      call pchar('',n0)
c
      GO TO 99
C
C---------------------------------------------------------------------
C							 GAUSS
C---------------------------------------------------------------------
  170 CONTINUE
c
      if (dtwh(c12ni,iptwh) .le. 0) call oerror(n225, m2, 'GAUSS')
c
      if (ing .lt. 1 .or. ing .gt. 2*maxmrk) 
     .		call oerror(n289,m2,'GAUSS')
c
      if (fixc .gt. 0. .and. fixhw .gt. 0. .and. fixh .gt. 0.) 
     .	call oerror(n295, m2, 'FIXH, FIXC, and FIXHW cannot all be TRUE')
      if (fixc .gt. 0. .and. fixrelc .gt. 0.) 
     .	call oerror(n295, m2, 'FIXC and FIXRELC cannot both be TRUE')
      if (fixh .gt. 0. .and. fixrelh .gt. 0.) 
     .	call oerror(n295, m2, 'FIXH and FIXRELH cannot both be TRUE')
      if (fixhw .gt. 0. .and. fixrelhw .gt. 0.) 
     .	call oerror(n295, m2, 'FIXHW and FIXRELHW cannot both be TRUE')
c
      do 1729 i = 1, ing
	if (hwidth(i) .le. 0.) call oerror(n290, m2, 'GAUSS')
	if (ing .gt. 0 .and. fixrelh .gt. 0 .and. height(i) .eq. 0.) 
     .		call oerror(n295, m2, 'HEIGHT cannot = 0 if FIXRELH is TRUE')
	do 1719 j = i+1, ing
	   if (center(i) .eq. center(j) .and. hwidth(i) .eq. hwidth(j))
     .		call oerror(n295, m2, 
     .			'Two CENTER''s and two HWIDTH''s are equal')
1719	   continue
1729	continue
c	Check input parameters for errors.
c
      call gprep(ierr)
      if (ierr .ne. 0) call oerror(ierr, m2, 'GAUSS')
c
      ndata = ibase
      nca = mmax
      ma = 3*ing + 3
      do 173 i = 1, ndata
	work(i) = 1.
	xdata(i) = xdata(i) - idatoff
173	continue
c	Work willbe used to hold SIGMA of the data points all of whicg are 1.
c	XDATA is created by GPREP with respect to start of TWH instead of in
c	channel number -- convert XDATA back to channel number. 
c
      coef(1) = 1.
      coef(2) = 0.
      coef(3) = 1.
      do 174 i = 1, ing
	if (fixh .gt. 0. .or. fixrelh .gt. 0.) then
	   coef(3*i+1) = height(i)
	else
	   if (center(i) .lt. 1 .or. 
     .         center(i) .gt. (dtwh(c12ni,iptwh)+dtwh(c12spn,iptwh)-1))
     .		call oerror(n295, m3, 'CENTER is out of range of data array')
	   coef(3*i+1) = twh(center(i)+idatoff,iptwh)
	endif
	coef(3*i+2) = center(i)
	coef(3*i+3) = hwidth(i) / constant
174	continue
c     Set up initial guess in COEF; use HEIGHT if FIXH or FIXRELH, else use the 
c	value of the data at the designated channel.  COEF is in RIP.INC
c	COEF(1-3) = H0, X0, S0
c	COEF(4-6) = HEIGHT(1), CENTER(1), HWIDTH(1) {in units of std};
c	COEF(7-9) = HEIGHT(2), CENTER(2), HWIDTH(2) {in units of std}; etc
c
c
c--------------------
      if (ing .gt. 1 .and. fixrelh .gt. 0 .and. fixc .gt. 0. .and. 
     .    fixhw .gt. 0.) then
c	Fitting linear case of just H0
	ma = 1
	mfit = 1
	lista(1) = 1
        if (mfit .ge. ndata) 
     .		call oerror(n295, m3, 'Too few data points')
	call lfit(xdata, ydata, work, ndata, lcoef, ma, lista, mfit, 
     .			 covar, nca, chisq, fgauss0)
	do 177 i = 1, ing
	  height(i) = lcoef(1)*coef(3*i+1)
	  hghterr(i) = coef(3*i+1) * sqrt(covar(1,1))
	  cnterr(i) = 0.0
	  hwerr(i) = 0.0
177	  continue
c--------------------
      else if (fixc .gt. 0. .and. fixhw .gt. 0.) then
c	Fitting height(s) of Gaussians only
	do 178 i = 1, ing
	  lista(i) = i
178	  continue
	ma = ing
	mfit = ing
        if (mfit .ge. ndata) 
     .		call oerror(n295, m3, 'Too few data points')
	call lfit(xdata, ydata, work, ndata, lcoef, ma, lista, mfit, 
     .			 covar, nca, chisq, fgauss0)
	do 1799 i = 1, ing
	  height(i) = lcoef(i)*coef(1)*coef(3*i+1)
	  hghterr(i) = coef(1)*coef(3*i+1) * sqrt(covar(i,i))
	  cnterr(i) = 0.0
	  hwerr(i) = 0.0
1799	  continue
c--------------------
      else
c	Non-linear case
c
c     LISTA indicates which COEF parameters are to be varied and which are to
c     be help constant.  The first MFIT numbers in COEF correspond to the indices
c     in COEF of what is to be varied.  The remaining MA-MFIT values in COEF are
c     those things that do not vary.
c
        mfit = 0
	if (ing .eq. 1) then
	   if (fixh .le. 0) then
	     mfit = mfit + 1
	     lista(mfit) = 4
	   endif
	   if (fixc .le. 0) then
	     mfit = mfit + 1
	     lista(mfit) = 5
	   endif
	   if (fixhw .le. 0) then
	     mfit = mfit + 1
	     lista(mfit) = 6
	   endif
	else
	   do 175 i = 1, ing
              if (fixh .le. 0. .and. fixrelh .le. 0) then
	        mfit = mfit + 1
	        lista(mfit) = 3*i + 1
	      endif
             if (fixc .le. 0. .and. fixrelc .le. 0) then
	        mfit = mfit + 1
	        lista(mfit) = 3*i + 2
	      endif
              if (fixhw .le. 0. .and. fixrelhw .le. 0) then
	        mfit = mfit + 1
	        lista(mfit) = 3*i + 3
	      endif
175	      continue
           if (fixrelh .gt. 0) then
	     mfit = mfit + 1
	     lista(mfit) = 1
           endif
           if (fixrelc .gt. 0) then
	     mfit = mfit + 1
	     lista(mfit) = 2
           endif
           if (fixrelhw .gt. 0) then
	     mfit = mfit + 1
	     lista(mfit) = 3
           endif
	endif
c
        mhold = mfit
	if (ing .eq. 1) then
	   lista(mhold+1) = 1
	   lista(mhold+2) = 2
	   lista(mhold+3) = 3
	   mhold = mhold + 3
	   if (fixh .gt. 0) then
	     mhold = mhold + 1
	     lista(mhold) = 4
	   endif
	   if (fixc .gt. 0) then
	     mhold = mhold + 1
	     lista(mhold) = 5
	   endif
	   if (fixhw .gt. 0) then
	     mhold = mhold + 1
	     lista(mhold) = 6
	   endif
	else
	   do 1759 i = 1, ing
              if (fixh .gt. 0. .or. fixrelh .gt. 0) then
	        mhold = mhold + 1
	        lista(mhold) = 3*i + 1
	      endif
             if (fixc .gt. 0. .or. fixrelc .gt. 0) then
	        mhold = mhold + 1
	        lista(mhold) = 3*i + 2
	      endif
              if (fixhw .gt. 0. .or. fixrelhw .gt. 0) then
	        mhold = mhold + 1
	        lista(mhold) = 3*i + 3
	      endif
1759	      continue
           if (fixrelh .le. 0) then
	     mhold = mhold + 1
	     lista(mhold) = 1
           endif
           if (fixrelc .le. 0) then
	     mhold = mhold + 1
	     lista(mhold) = 2
           endif
           if (fixrelhw .le. 0) then
	     mhold = mhold + 1
	     lista(mhold) = 3
           endif
	endif
c
        if (mhold .ne. ma) call oerror(n120, m3, 'GAUSS')
        if (mfit .ge. ndata) 
     .		call oerror(n295, m3, 'Too few data points')
c
	alamda = -1.
	kount = 0
        call mrqmin(xdata, ydata, work, ndata, coef, ma, lista, mfit, 
     .	                 covar, alph, nca, chisq, fgauss, alamda)
c	
180     chisq1 = chisq
	alamda1 = alamda
        kount = kount + 1
	if (kount .le. niter) then
	   call mrqmin(xdata, ydata, work, ndata, coef, ma, lista, mfit, 
     .	                 covar, alph, nca, chisq, fgauss, alamda)
	   if (alamda .gt. alamda1 .or. 
     .         abs(1.-chisq1/chisq) .gt. 0.001) goto 180
	else
	   call oerror(n282, n0, 'GAUSS')
	endif
	alamda = 0.
        call mrqmin(xdata, ydata, work, ndata, coef, ma, lista, mfit, 
     .	                 covar, alph, nca, chisq, fgauss, alamda)
c
      sumsq=sqrt(chisq/(ibase-mfit))
c
        do 185 i = 1, ing
	    j = 3*i + 1
	    height(i) = coef(j) * coef(1)
	    center(i) = coef(j+1) + coef(2)
	    hwidth(i) = coef(j+2) * coef(3) * constant
c	    calculate results
c
	    hghterr(i) = height(i) * sqrt( covar(1,1)/(coef(1)**2) + 
     .			covar(j,j)/(coef(j)**2) + 
     .			2.*covar(1,j)/(coef(1)*coef(j)) ) * sumsq
c
            cnterr(i) = sqrt( covar(2,2) + covar(j+1,j+1) + 
     .			2.*covar(2,j+1) ) * sumsq
c
	    hwerr(i) = hwidth(i) * sqrt( covar(3,3)/(coef(3)**2) + 
     .			covar(j+2,j+2)/(coef(j+2)**2) + 
     .			2.*covar(3,j+2)/(coef(3)*coef(j+2)) ) * sumsq

c	    Calculates errors
c
185         continue
      endif
c
      GO TO 99
C-----------------------------------------------------------------------
 99   CONTINUE
      RETURN
C-----------------------------------------------------------------------
 9601 FORMAT (F9.4,F9.4,F9.4)
 9603 FORMAT (F14.5)
C-----------------------------------------------------------------------
      END
