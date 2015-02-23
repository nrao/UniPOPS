      Subroutine gmeasure(pr1)
c
c     @(#)gmeasure.f	5.3 07/20/94
c
C
C         Measures areas, velocities and widths of
C         galaxian profiles.
C         It does so in up to 5 different ways, using
C         three different approaches:
C         1 ... width at fraction f of mean flux over (BWIND,EWIND)
C         2 ... width at fraction f of peak flux over (BWIND,EWIND)
C         3 ... width at fraction f of each of two horns,
C               searched from the inside out.
C         4.... width at a fraction f of each horn, located
C               by simple linear interpolation between the 
C               points which bracket the point with flux =
C               f times the peak, the velocity uncertainity
C               is estimated by fitting a polynomial between
C               the f1 and f2 points on each side of the
C               profile
C         5 ... width at a fraction f of each horn, searched
C               by fitting a polynomial between fractions
C               f1 and f2 of the respective horn to each
C               side of the profile
C          
C
C Input : an argument, bwind, ewind, gmfrac, vrms
C
C         where argument = sequence of up to 5 integers 1,2,3 or 4
C                     identifying the mode of computation of
C                     width as listed above, in up to 5
C                     separate calls for measuring; e.g. if
C                     one wants to compute (V,W) first at
C                     level f1 of mean, then at level f3 of
C                     peak, then level f4 of peak, then level
C                     f5 of avg. two horns, then at level f
C                     of each horn located by fitting a
C                     polynomial between levels f_1 and f_2
C                     of the respective peak to each side
C                     of the profile,set argument to 12234;
C                     values f1,f2,f3,f4,f,f_1,f_2 are read
C                     from the GMFRAC(1 - 5) elements. 
c
c		 GMFRAC = GMFRAC(1) = f1; GMFRAC(2) = f2; GMFRAC(3) = f3; etc
c
c		      GMFRAC has to be coded as 0.ff_1f_2 (i.e each
C                     fraction can be specified to one
C                     digit accuracy only. so if you want 
C                     to fit between 0.8 and 0.2 of the
C                     horn and the find the 0.5 level 
C                     by interpolation, set GMFRAC(5) to
C                     0.825
C
C                BWIND, EWIND = boundaries of interval over which
C                     area and Peak search is carried on.
c
c	 	 VRMS = rms of the data (used for methods 4 or 5 only)
C
C         The AREA returned is the one between BWIND and EWIND.
C         If EWIND=BWIND, GMEASURE searches for the
C         first occurrence of nulls from EWIND (or BWIND) out, and
C         it computes the AREA between those two channels.
C
C Output: Computed parameters are displayed on the screen and stored in
c	  array GOUT:
c
C         GOUT(*,1)    : mesuring code
c	  GOUT(*,2)    : velocity
c	  GOUT(*,3)    : width
C	  GOUT(*,4)    : velocity error (methods 4&5 only)
C         GOUT(*,5)    : value of GMFRAC(*)
C         GOUT(6,1)    : maximum within feature
C         GOUT(6,2)    : flux integral
C         GOUT(6,3)    : input rms 
c	  GOUT(6,4)    : BWIND
c	  GOUT(6,5)    : EWIND
C
C
      include 'core.inc'
      include 'appl.inc'
      include 'cform.inc'
c
      Real F(5),SMx,Smax,S,Dex,f
      Real SDV(5),W(5),V(5),pr1,pr2,pr3, dv(5)
      Real RMS,SFAC,wrd(5), velrms
c
      double precision chantox
c     
      Integer*4        I, J, Imax, il, ir, channo, firstch
      Integer*4        gmeasK(5), jl(5), jr(5), ibx
      Integer*4        iostatus, istart, istop, chanil, chanir
c
      character*80 stch
      integer*2 istch(40), short, inn, iptwh, nb, inn2, n80, n292, m1,
     .		iymm10, n112, ierr, n225, m2, n0, n270, lxa1, n268
      logical okarray, okreal4, first
c
      EQUIVALENCE (istch, stch)
C
      DATA gmeasK/5*0/, n80/80/, n292/292/, m1/-1/, n112/112/, n225/225/,
     .     m2/-2/, n0/0/, n270/270/, n268/268/ 
c
      iptwh = 1
c
      firstch = nint(dtwh(c12spn,iptwh))
      istart = firstch + idatoff 
      istop = istart + nint(dtwh(c12ni,iptwh)) - 1
      inn = istop - istart + 1
      if (istart .ge. istop) call oerror(n225, m2, 'GMEASURE')
c     Check that you have data in IPTWH
c
      lxa1 = lxa
      if (.not. okarray(lxa, dtwh(1,iptwh)) ) then
         call oerror(n270, n0, 
     .		'GMEASURE: Units of X-axis will be channels')
	 lxa1 = 1
      endif
c
      inn2 = 1
      first = .true.
      do 209 i = 1, inn
         if (okreal4(twh(istart+i-1,iptwh))) then
            xdata(inn2) = twh(istart+i-1,iptwh)
            channo = i + firstch - 1
            ydata(inn2) = 
     .         chantox(lxa1, dtwh(1,iptwh), float(channo), ierr)
            inn2 = inn2 + 1
         else if (first) then
            call oerror(n268, n0, "GMEASURE: Skipping points")
            first = .false.
         endif
 209  continue
      inn = inn2
c     Copy data and velocities in each channel into XDATA and YDATA respectively.
c
      IF (PR1.le.0.) THEN
229      call pread2( 'Enter string of modes to measure:', stch, nb)
         READ (stch,*,iostat=iostatus) PR1
	 if (iostatus.ne.0 .or. PR1 .eq. '0.') then
		stch = 'Bad input... Try again'
		call pwrite(istch, n80)
		goto 229
	 endif
      END IF
C
      pr3 = nint(ewind) - firstch + 1
      pr2 = nint(bwind) - firstch + 1
c
      If (pr2 .eq. pr3) then
	stch = 'Calculating NULL points -- BMOMENT = EMOMENT'
	call pwrite(istch,n80)
	CALL NULLS(xdata,pr2,pr3, inn)
      endif
c		apparently BMOMENT can not be equal to the first channel
c		and EMOMENT can not be equal to the last channel
      if (pr2 .ge. pr3 .or. pr2 .lt. 2 .or. 
     .    pr3 .gt. (dtwh(c12ni,iptwh)-1))
     .   call oerror(n292,m2,'GMEASURE')
c
      if (showplot(numplots) .eq. 0) then
c       Flag the pr2, pr3 channels that will be used; only flag if last display
c       was a SHOW.
c
        ibx = (pr2+firstch-1)*ax(numplots)+bx(numplots) + 0.5
        if (ibx .ge. ix0 .and. ibx .le. ixm) then
  	  call place(short(ibx), iy0)
	  iymm10 = iym-10
	  call vctr(short(ibx), iymm10)
        endif
        ibx = (pr3+firstch-1)*ax(numplots)+bx(numplots) + 0.5
        if (ibx .ge. ix0 .and. ibx .le. ixm) then
	  call place(short(ibx), iy0)
	  iymm10 = iym-10
	  call vctr(short(ibx), iymm10)
        endif
      endif
C
C     Get the max flux
C
      SMAX = SMx( Xdata, PR2, PR3 )
C
C     Now decode PR1 and decide which routines to call
C
      S = PR1
      J = 1
      DO 14 I=1,5
       wrd(i) = 0.0
       DEX = 10.**(5-I)
       gmeasK(J) = int(S/DEX)
       F(J) = GMFRAC(I)
       S = S - real(gmeasK(J))*DEX
       IF (S.LT.0.5) GO TO 12
       IF (gmeasK(J).ne.0) J = J + 1
   14 CONTINUE
   12 IMAX = J
c
C     Obtain RMS from VRMS
c
      RMS = VRMS
      SFAC = 1.0
c      
      IF (RMS.LE.0.0) THEN
         stch = 
     .     'GMEASURE:RMS of profile = 0, polynomial fiting will fail.'
	 call pwrite(istch, n80)
         stch = 'Have you performed an RMS?'
	 call pwrite(istch, n80)
	 return
      ENDIF
C
C     OK, now go to measure.
C
      IL = PR2
      IR = PR3
c
      DO 21 I=1,IMAX
c
         IF (gmeasK(I).EQ.0) call oerror(n112,m2,
     .		'GMEASURE: Problem with input parameter')
c
         IF (gmeasK(I).EQ.1)
     *    CALL AREA (Xdata,Ydata,IL,IR,F(I),SDV(I),JL(I),JR(I),W(I),
     *                V(I),RMS)
c
         IF (gmeasK(I).EQ.2)
     *    CALL AREAP (Xdata,Ydata,IL,IR,F(I),SDV(I),JL(I),JR(I),W(I),
     *                V(I),RMS)
c
         IF(gmeasK(I).EQ.3)
     *    CALL AREA2P(Xdata,Ydata,IL,IR,F(I),SDV(I),JL(I),JR(I),W(I),
     *                V(I),RMS,inn)        
c
         IF (gmeasK(I).EQ.4)
     *    CALL AREAPE(Xdata,Ydata,IL,IR,F(I),SDV(I),JL(I),JR(I),W(I),
     *                 V(I),RMS,VELRMS)
c
         IF (gmeasK(I).EQ.5)
     *    CALL AREAPF(Xdata,Ydata,IL,IR,F(I),SDV(I),JL(I),JR(I),W(I),
     *                 V(I),RMS,VELRMS)
c
	 chanIL = IL + firstch - 1
         chanIR = IR + firstch - 1
         WRITE (stch,109,iostat=ierr) chanIL,chanIR,SDV(I),V(I),
     *            (JL(I)+firstch-1), (JR(I)+firstch-1),W(I)
  109    FORMAT (" area(",I3,",",I4,") =",E10.4,
     *           " Center =",F8.1,3X," Width(",I3,",",I4,") =",F8.1)
	 call pwrite(istch, n80)
C
C        if the polfit routine has been used then
C        write the uncertainity in velocity
C

         IF((gmeasK(I).EQ.4).OR.(gmeasK(I).EQ.5)) THEN
c
C           increase the uncertainity by the degradation in the
C           velocity resolution caused by smoothing
c           
	    dv(i) = VELRMS*sfac
            WRITE(stch,901,iostat=ierr)  dv(i)
 901        FORMAT (28X,'DeltaV =',F6.2)
	    call pwrite(istch, n80)
	 else
c
	    dv(i) = 0.0
c            
         ENDIF
c
   21 CONTINUE
c
      do 219 i = 1, 6
	do 218 j = 1, 5
	   gout(i,j) = 0.0
218	   continue
219	continue
c
      do 20 i = 1, imax
	gout(i,1) = gmeasK(i)
	gout(i,2) = v(i)
	gout(i,3) = w(i)
	gout(i,4) = dv(i)
	gout(i,5) = f(i)
20	continue
c
      gout(6,1) = SMAX
      gout(6,2) = sdv(1)
      gout(6,3) = vrms
      gout(6,4) = il + firstch - 1
      gout(6,5) = ir + firstch - 1
c
   99 RETURN
c
      END

********************************************************************************
      SUBROUTINE NULLS ( X, PR2, PR3, npts)
********************************************************************************
C
      Real X(*), Pr2, Pr3
      Integer*4        IL, IR, ILL, IRR, JL, JR, npts
C
      IL = PR2
      IR = PR3
c***      ILL = max(1,IL - 15)
c***      IRR = min(IR + 15, npts)
      ill = 1
      irr = npts
C
      DO 11 JL = IL,ILL,-1
         IF (X(JL).LT.0.) GO TO 20
   11 CONTINUE
      jl = ill - 1
C
   20 IL = JL + 1
      DO 21 JR = IR,IRR
         IF (X(JR).LT.0.) GO TO 30
   21 CONTINUE
      jr = irr + 1
C
   30 IR = JR - 1
C
      PR2 = IL
      PR3 = IR
c
      RETURN
      END

********************************************************************************
      Real FUNCTION SMx ( X, X1, X2 )
********************************************************************************
C
C     Finds the maximum between the feature boundaries X1,X2
C
      Real X(*), x1, x2
      Integer*4        J1, J2, J
C
      J1 = X1
      J2 = X2

      SMX = X(J1)
C     SMx = -999.
C
      Do 10 J=J1,J2
         SMx = MAX (X(J),SMx)
 10   Continue
C
      RETURN
      END

********************************************************************************
      SUBROUTINE AREA (X,Y,IFIRST, LAST, F, SDV, JL, JR, W, V,RMS)
********************************************************************************
C
      Real X(*),Y(*),SDV,AVE,BL,FRAC,BR,V,W,FLevel
      Real Sdv, F,RMS,VR,VL
      Integer*4        Ifirst, last, JR, J, JL, I
c
      integer*2 n229, n0
c
      data n229, n0/229, 0/
C
C     compute a rough estiamte of the area, ignore the
C     uneven channel spacing in velocity
C
      SDV = 0.0
C
      Do 10 I=IFIRST,LAST
          SDV = SDV + X(I)
   10 Continue

      AVE = SDV/real(LAST-IFIRST+1)
C
      IF(F.GT.1. .or. F .le. 0.) call oerror(n229, n0, 'GMEASURE')
C
      FLEVEL = F*AVE
C
      Do 20 I=IFIRST,LAST
          IF(X(I).GE.FLEVEL) GO TO 55
   20 Continue
      i = last + 1
C
   55 JL = I
      BL = FRAC(X(JL-1),X(JL),FLEVEL)
      VL = Y(JL-1)+BL*(Y(JL)-Y(JL-1))
c
      Do 30 J = LAST,IFIRST,-1
          IF(X(J).GE.FLEVEL) GO TO 66
   30 Continue
      j = ifirst - 1
C
   66 JR = J
      BR = FRAC(X(JR),X(JR+1),FLEVEL)
      VR = Y(JR)+BR*(Y(JR+1)-Y(JR))
C
      W = VR-VL
      V = (VR+VL)/2.0

C

      SDV = 0.0
C
C     compute area accurately, i.e. using the
C     fact that the chaanels are not evenly 
C     spaced in velocity
C

      Do 40 I=IFIRST,LAST
          SDV = SDV + X(I)*(Y(I)-Y(I-1))
 40    Continue


   99 RETURN
      END

********************************************************************************
      SUBROUTINE AREAP (X,Y,IFIRST, LAST, F, SDV, JL, JR, W, V,RMS)
********************************************************************************
C
      Real X(*),Y(*),SDV, Peak, W, BR,
     *                 BL, V, FLevel, F, Frac,RMS,
     *                 VL,VR
      Integer*4        Ifirst, last, Jl, JR , J, I
c
      integer*2 n0, n229
c
      data n229, n0/229, 0/    
C
      SDV = 0.0
      PEAK = X(IFIRST)
C
C     compute area and peak

      Do 10 I=IFIRST,LAST
          IF(X(I).GT.PEAK) PEAK = X(I)
          SDV = SDV + X(I)*(Y(I)-Y(I-1))
   10 Continue
      
      PEAK = PEAK - RMS
C
      IF(F.GT.1. .or. F .le. 0.) call oerror(n229, n0, 'GMEASURE')
C
      FLEVEL = F*PEAK
C
      Do 20 I=IFIRST,LAST
          IF(X(I).GE.FLEVEL) GO TO 55
   20 Continue
      i = last + 1
C
   55 JL = I
      BL = FRAC(X(JL-1),X(JL),FLEVEL)
      VL = Y(JL-1)+BL*(Y(JL)-Y(JL-1))
c
      Do 30 J = LAST,IFIRST,-1
          IF(X(J).GE.FLEVEL) GO TO 66
   30 Continue
      j = ifirst - 1
c
   66 JR = J
      BR = FRAC(X(JR),X(JR+1),FLEVEL)
      VR = Y(JR) + br*(Y(JR+1)-Y(JR))
C
      W = VR - VL
      V = (VR +VL)/2.0
      RETURN
      END


********************************************************************************
      SUBROUTINE AREA2P (X,Y,IFIRST,LAST,F,SDV,JL,JR,W,V,RMS,npts)
********************************************************************************

      Real X(*),Y(*),Comp,F,Sdv,W,V,FPeak1,Frac,Bl
      Real FPeak2, Br,RMS,VL,VR
      Integer*4        IFirst, Last, k1, k2, Il, I, Jl, JR, IR
      Integer*4        IFlag, IFF, JJ1, ILL, JJ2, npts
c
      integer*2 n229, n0
c
      data n229, n0/229, 0/
C
C     Search for correct channel of two horns (IL,IR)
C
      IF(F.GT.1. .or. F .le. 0.) call oerror(n229, n0, 'GMEASURE')
c
      COMP = 0.
      K1   = max(IFIRST - 10, 1)
      K2   = min(IFIRST + 10, npts)
C
      do 10 I=K1,K2
         IF(X(I).GE.COMP) THEN
           COMP = X(I)
           IL = I
         END IF
   10 Continue

      COMP = 0.
      K1 = max(LAST - 10, 1)
      K2 = min(LAST + 10, npts)
C
      Do 20 I=K1,K2
         IF(X(I).GE.COMP) THEN
           COMP = X(I)
           IR = I
         END IF
  20  Continue
C
C     Find channels at f level of horns and at 1st null
C                                  (J1,J2)    (JJ1,JJ2)
C
      IFLAG = 1
      IFF = max(IL - 100, 1)

C     subtract the rms from the peak value

      FPEAK1 = F*(X(IL)-RMS)

C
      Do 30 I=IL,IFF,-1
       IF(I.LT.2) GO TO 201
       IF(X(I).LE.FPEAK1.AND.IFLAG.LT.2) THEN
        JL = I
        IFLAG = 2
       END IF
       IF(X(I).LE.0.) THEN
        JJ1 = I + 1
        GO TO 201
       END IF
   30 Continue
C
  201 IFLAG = 1
      ILL = min(IR + 100, npts)

C     subtract the rms from the peak value

      FPEAK2 = F*(X(IR)-RMS)
C
      Do 40 I=IR,ILL
         IF(I.GT. npts-1) GO TO 301
         IF(X(I).LE.FPEAK2.AND.IFLAG.LT.2) THEN
           JR = I
           IFLAG = 2
         END IF
         IF(X(I).LE.0.) THEN
            JJ2 = I - 1
            GO TO 301
         END IF
   40 Continue
C
C     COMPUTE AREA AND WIDTH
  301 SDV = 0.0
      Do 50 I=JJ1,JJ2
          SDV = SDV + X(I)*(Y(I)-Y(I-1))
   50 Continue
C
      BL = FRAC(X(JL),X(JL+1),FPEAK1)
      VL = Y(JL) + BL*(Y(JL+1)-Y(JL))
      BR = FRAC(X(JR-1),X(JR),FPEAK2)
      VR = Y(JR-1) + BR*(Y(JR)-Y(JR-1))
      W =  VR -VL
      V = (VR + VL)/2.0
C
      RETURN
      END


***************************************************************************
      SUBROUTINE AREAPF(X,Y,IFIRST,ILAST,F,SDV,IFRL,IFRR,W,V,RMS,VRms)

***************************************************************************
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C              this subroutine calculates the width and average
C   velocity of a galaxy spectrum. The two halves of the double
C   horned profile are treated separately. For each half, the 
C   peak is determined (it is assumed that the two peaks are on either
C   side of the center of the user specified limits IFIRST and LAST).
C   Then each on each side of the profile  a polynomial (by default
C   1st order) is fit to a user specified portion of the profile.
C   If the fit is poor, (i.e. (chisqr - deg.of.freedom) > 3 times
C   sqrt(2*deg.of.freedom)), the user is given the option of fitting
C   a 2nd degree polynomial and/or changing the levels between
C   which to fit. Finaly using the fitted polynomial, the FR points
C   (where FR is a user specified fraction of the peak flux) on
C   either side of the profile is located. The interpolaton
C   is done by channel number and then the velocity corresponding
C   to the interpolated channel number is computed. The average velocity
C   is taken to be the mean of these two velocities and the
C   width is taken to be the difference bwteen these two velocities.
C   The uncertainity in the velocity measurment is then calculated
C   from the covariance matrix of the coefficients of the fitted
C   polynomial. Finally the area of the profile between the limits
C   IFIRST and ILAST is computed as summation (X(I)*dV(I)), where
C   dV(I) is the velocity difference between channels I and I-1.
C
C      the following subroutines are required:
C     
C     pksearch : locates the profile horns
C     plfit    : sets up input functions for lfit
C     lfit     : sets up the normal equations matrix,computes the
C                coefficients of the best fit polynomial
C     gaussj   : does the matrix inversion
C     covsrt   : calculates the covariance matrix of the coefficients
C     funcs    : defines the fitting functions (i.e. polynomials)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                         INPUT VARIABLES
C
C   X       is the array containg the spectrum which is to be measured
C   Y       is the array containing the velocity information for X 
C   IFIRST  is the (user set) channel number of the left edge of the spectrum
C   ILAST   is the (user set) channel number of the right edge of the spectrum
C   RMS     rms noise in the spectrum (as estimated by base.f)
C   F       specifies the levels between which to do the polynomial
C           fitting and the level at which to measure. A double prec
C           number of the form 0.abc, where 0.a and 0.b are the upper
C           and lower fractions of the peak flux between which to 
C           do the fitting and 0.c is the fraction of the peak flux
C           at which to measure
C             
C                         OUTPUT VARIABLES
C
C   SDV     is the area under the spectrum within the IFIRST and ILAST
C   W       is the width of the profile
C   V       is the average velocity of the profile
C   VRMS    is the rms error associated with the velocity measurement
C
C
C                           INTERNAL VARIABLES
C   
C    A(I)   parameters of the POLYNOMIAL fit x = A(1) + A(2)*Y + ..
C    B(I)   DUMMY VECTOR USED TO CONDENSE RMS CALCULATION CODE
C    COVAR  MATRIX CONTAINING COVARIANCE OF THE COEFFICIENTS
C           OF THE FITTED POLYNOMIALS
C    dV     VELOCITY INTERVAL BETWEEN TWO NEIGHBOURING CHANNELS
C    FPL    LEFT HALF PEAK FLUX LEVEL IN THE SPECTRUM
C    FPR    RIGHT HALF PEAK FLUX LEVEL IN THE SPECTRUM
C    FR1    FRACTION OF PEAK FLUX AT WHICH TO STOP LINEAR FIT TO PROFILE
C    FR2    FRACTION OF PEAK FLUX AT WHICH TO START LINEAR FIT TO PROFILE
C    FR     FRACTION OF PEAK FLUX AT WHICH TO CALCULATE WIDTH
C    FRMSL  RMS ERROR OF THE FLUX AT THE INTERPOLATED LEVEL FR OF
C           THE PEAK FLUX
C    I,J    DUMMY INTEGERS FOR LOOPING CONTROL
C    IFR1L  LEFT FR1 CHANNEL
C    IFR2L  LEFT FR2 CHANNEL
C    IFR1R  RIGHT FR1 CHANNEL
C    IFR2R  RIGHT FR1 CHANNEL
C    IFRL   LEFT FR CHANNEL
C    IFRR   RIGHT FR CHANNEL
C    IOS    STATUS FLAG FOR IO
C    JUNK   VARIABLE IN WHICH TO TRAP WRONG TERMINAL INPUT
C    RESET  FLAGS WETHER USER HAS RESET PARAMETERS
C    TEMP   TEMPORARY VARIABLE USED TO DECODE F
C    VRMSL  RMS ERROR ASSOCIATED WITH THE VELOCITY ESTIMATE
C           AT FLUX LEVEL FR OF THE LEFT HALF OF THE PROFILE
C    VRMSR  RMS ERROR ASSOCIATED WITH THE VELOCITY ESTIMATE
C           AT FLUX LEVEL FR OF THE RIGHT HALF OF THE PROFILE
C    YFRL   LEFT FR POINT
C    YFRR   RIGHT FR POINT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      Real X(*),Y(*),SDV,V,W
      Real FTST1,FTST2,RMS ,F

      INTEGER*4 IFIRST,ILAST,NORDER,
     *          IFR1L,IFR2L,IFRL,IFR1R,IFR2R,
     *          IFRR,I,IPL,IPR,J
      Real VFRL,VFRR,YFRL,YFRR,dV,A(3),
     *                 FR1,FR2,FR,FPL,FPR,FPL,B(3),
     *                 FRMS,VRMSL,VRMSR,VRMS,
     *                 COVAR(3,3),TEMP
      character*80 stch
      integer*2 istch(40), nb, n80, IOS
c
      equivalence (istch, stch)

      LOGICAL RETRY
c
      data n80/80/


C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C            DECODE F INTO THE APPROPRIATE FRACTONS
C
C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         

      RETRY = .FALSE.

 90   IF((.NOT.RETRY).AND.((F.GE.1.0).OR.
     *     F.LE.0.0)) THEN

         IOS = 0
 	 stch = ' '
	 call pwrite(istch,n80)
	 stch = 'Specify profile levels for fit; check help file for format'
  	 call pwrite(istch, n80)
	 call pread2('#', stch, nb)
         READ(stch,*,IOSTAT=IOS) F

         IF(IOS.NE.0) THEN
            
            IOS = 0
	    f = 0.0
            stch = 'Bad input... Try again...'
	    call pwrite(istch, n80)
            GOTO 90

         ENDIF

         IF ((F.GE.1.0).OR.(F.LE.0.0)) THEN

            stch = 'Bad input... Try again...'
	    call pwrite(istch, n80)
            GOTO 90

         ENDIF

            
      ENDIF

      IF (F.NE.0.0) THEN

         TEMP = F
         FR1 = (INT(TEMP*10.0)/10.0)
         TEMP = TEMP -FR1

         FR2 = (INT(TEMP*100.0)/10.0)
         TEMP = TEMP - FR2*0.1

         FR = (NINT(TEMP*1000.0)/10.0)
         
         IF((FR.GT.FR1).OR.(FR.LT.FR2).OR.
     *       (FR1.EQ.0.0).OR.(FR2.EQ.0.0).OR.
     *        (FR1.LE.FR2)) THEN
            stch = 'Error in polynomial fitting; ' // 
     .		'you need to reset the fitting levels'
	    call pwrite(istch,n80)
            GO TO 90

         ENDIF

      ELSE
            
         F = FR1+0.1*FR2+0.01*FR

      ENDIF



      
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      
C                       LOCATE THE PROFILE HORNS
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      CALL PKSEARCH(X,IFIRST,ILAST,IPL,IPR,FPL,FPR)

      
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C             locate the FR1 and FR2 points
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      DO 102 I=IPL,IFIRST,-1

         IF(X(I).LT.(FR1*FPL)) GOTO 201

 102  CONTINUE
      i = ifirst - 1
         
 201  IFR1L = I

      
      DO 103 I= IPL,IFIRST,-1

         IF(X(I).LT.FR2*FPL) GOTO 202

 103  CONTINUE
      i = ifirst - 1

 202  IFR2L = I

      IF(IFR1L.LE.IFR2L) THEN
         
         IFR1L = IFR2L + 1
         stch = 'Warning: upper and lower profile points coincide'
	 call pwrite(istch, n80)

      ENDIF

      
      DO 104 I=IPR, ILAST

         IF(X(I).LT.FR1*FPR) GOTO 203

 104  CONTINUE
      i = ilast + 1

 203  IFR1R = I

      
      DO 105 I= IPR,ILAST

         IF(X(I).LT.FR2*FPR) GOTO 204

 105  CONTINUE
      i = ilast + 1

 204  IFR2R = I

      IF(IFR1R.GE.IFR2R) THEN

         IFR2R = IFR1R + 1
         stch = 'Warning: upper and lower profile points coincide'
	 call pwrite(istch, n80)

      ENDIF


      IF(.NOT.RETRY)  NORDER = 1


C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          reduce the peak flux levels by the RMS
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      FPL = FPL - RMS
      FPR = FPR - RMS


C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   
C         fit a polynomial of order NORDER to the left half
C         of the profile and locate the FR point 
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      CALL PLFIT (X,IFR2L,IFR1L,RMS,NORDER,A,FTST1,COVAR)

      IF (NORDER.EQ.1) THEN

         YFRL = (FPL*FR -A(1))/A(2)
         IFRL = INT(YFRL)

      ENDIF

      IF(NORDER.EQ.2) THEN

         YFRL = (-A(2) + SQRT(A(2)*A(2) -4.0*(A(1)-FR*FPL)*A(3)))
         YFRL = YFRL /(2.0*A(3))
         IFRL = INT(YFRL)

         IF((IFRL.LT.IFR2L).OR.(IFRL.GT.IFR1L)) THEN
            
            YFRL = (-A(2)- SQRT(A(2)*A(2)-4.0*(A(1)-FR*FPL)*A(3)))
            YFRL = YFRL /(2.0*A(3))
            
         ENDIF

      ENDIF



C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     calculate the uncertainity in the velocity measurement
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      
      B(1) = 1.0
      B(2) = YFRL
      B(3) = YFRL*YFRL

      FRMS = 0.0

      DO 301 I = 1, NORDER + 1
         
         DO 302 J = 1, NORDER + 1

            FRMS = FRMS + B(I)*B(J)*COVAR(I,J)

 302     CONTINUE

 301  CONTINUE

      IF (NORDER.EQ.1)  VRMSL = FRMS/A(2)*
     *                          (Y(IFRL)- Y(IFRL-1))
      
      IF(NORDER.EQ.2)   VRMSL = FRMS/(2.0*A(3)*YFRL+A(2))*
     *                          (Y(IFRL)-Y(IFRL-1))

      

C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   
C         fit a polynomial of order NORDER to the right half
C         of the profile and locate the FR point 
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      CALL PLFIT (X,IFR1R,IFR2R,RMS,NORDER,A,FTST2,COVAR)
      

      IF (NORDER.EQ.1) THEN

         YFRR = (FPR*FR -A(1))/A(2)
         IFRR = INT(YFRR)

      ENDIF

      IF(NORDER.EQ.2) THEN

         YFRR = (-A(2) + SQRT(A(2)*A(2) -4.0*(A(1)-FR*FPR)*A(3)))
         YFRR = YFRR /(2.0*A(3))
         IFRR = INT(YFRR)

         IF((IFRR.LT.IFR1R).OR.(IFRR.GT.IFR2R)) THEN
            
            YFRR =(-A(2)-SQRT(A(2)*A(2)-4.0*(A(1)-FR*FPR)*A(3)))
            YFRR = YFRR /(2.0*A(3))
            
         ENDIF


      ENDIF

C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     calculate the uncertainity in the velocity measurement
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      
      B(1) = 1.0
      B(2) = YFRR
      B(3) = YFRR*YFRR

      FRMS = 0.0

      DO 303 I = 1, NORDER + 1
         
         DO 304 J = 1, NORDER + 1

            FRMS = FRMS + B(I)*B(J)*COVAR(I,J)

 304     CONTINUE

 303  CONTINUE

      IF (NORDER.EQ.1)  VRMSR = FRMS/A(2)*
     *                          (Y(IFRR)-Y(IFRR-1))
      
      IF(NORDER.EQ.2)   VRMSR = FRMS/(2.0*A(3)*YFRR+A(2))*
     *                          (Y(IFRR)-Y(IFRR-1))
      
      

C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       if the fit is poor let the user reset the parameters
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      IF ((ABS(FTST1).GT.3.0).OR.(ABS(FTST2).GT.3.0)) THEN

	 stch = 'Poor fit to profile; the normalized chisqr are: '
	 call pwrite(istch, n80)
         WRITE(stch,'(2f6.1)',iostat=ios) FTST1,FTST2
         call pwrite(istch,n80)
         write(stch,10,iostat=ios) IFR1L-IFR2L+1,IFR2R-IFR1R+1
 10      format('# of points used to fit l & r halves are',x,i3,x,i3 )
	 call pwrite(istch, n80)

	 retry = .true.
         call pread2(
     .	   'Do you want to fit a higher degree polynomial (y/n [y])? ',
     .	   stch, nb)
         if (stch(1:1) .eq. 'n' .or. stch(1:1) .eq. 'N') retry = .false.

         IF (RETRY) THEN
            
 22         stch = ' '
	    call pwrite(istch,n80)
	    stch = 'Specify polynomial order (<= 2) & profile levels between which to fit.'
	    call pwrite(istch,n80)
	    stch = 'Levels coded as before, 0 0 => use 2nd order & orignal profile levels.'
	    call pwrite(istch, n80)
	    call pread2('Enter ORDER:', stch, nb)
            READ(stch,*,IOSTAT=IOS) NORDER
	    if (ios .eq. 0) then
		call pread2('Enter Levels:', stch, nb)
            	READ(stch,*,IOSTAT=IOS) f
	    endif
            IF(IOS.NE.0) THEN
               
               IOS = 0
               stch = 'Try again'
	       call pwrite(istch, n80)
               GOTO 22
               
            ENDIF

               IF ((F.GE.1.0).OR.(F.LT.0.0).OR.
     *             (NORDER.LT.0).OR.(NORDER.GT.2)) THEN
            
                  stch = 'Try again'
	          call pwrite(istch, n80)
                  GOTO 22
                  
               ENDIF

            IF(NORDER.LE.0) NORDER = 2
            
	    GOTO 90	    

         ENDIF
	
      ENDIF
     
C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        calculate the velocity width and average velocity
C
C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IFRL = INT(YFRL)
      VFRL = Y(IFRL) + (YFRL -float(IFRL))*(Y(IFRL+1)-Y(IFRL))

       
      IFRR = INT(YFRR)
      VFRR = Y(IFRR) + (YFRR -float(IFRR))*(Y(IFRR+1)-Y(IFRR))

      V = (VFRL + VFRR)/2.0

      W = (VFRR - VFRL)

      VRMS = SQRT((VRMSL**2 +VRMSR**2)/2.0)



C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C        calculate the integrated flux in bounded region

C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      SDV = 0.0

      DO 106 I = IFIRST, ILAST

         dV = Y(I) - Y(I-1)
         SDV = SDV +X(I)*dV

 106  CONTINUE


      RETURN

      END


***************************************************************************
      SUBROUTINE AREAPE(X,Y,IFIRST,ILAST,F,SDV,IFRL,IFRR,W,V,RMS,VRms)
***************************************************************************
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C              this subroutine calculates the width and average
C   velocity of a galaxy spectrum. The two halves of the double
C   horned profile are treated separately. For each half, the 
C   peak is determined (it is assumed that the two peaks are on either
C   side of the center of the user specified limits IFIRST and LAST).
C   Then each on each side of the profile  a polynomial (by default
C   1st order) is fit to a user specified portion of the profile.
C   If the fit is poor, (i.e. (chisqr - deg.of.freedom) > 3 times
C   sqrt(2*deg.of.freedom)), the user is given the option of fitting
C   a 2nd degree polynomial and/or changing the levels between
C   which to fit. The FR points (where FR is a user specified fraction
C   of the peak flux) on either side of the profile are located by 
C   interpolation between the two data points that bracket the
C   FR point. The interpolaton is done by channel number and then 
C   the velocity corresponding to the interpolated channel number 
C   is computed. The average velocity is taken to be the mean of
C   these two velocities and the width is taken to be the difference
C    between these two velocities.
C   The uncertainity in the velocity measurment is then calculated
C   from the rms in the baseline and the coefficients of the 
C   fitted polynomial. Finally the area bewteen the channels
C   IFIRST and ILAST is computed as summation (X(I)*dV(I)), where
C   dV(I) is the velocity difference between channels I and I-1.
C
C      the following subroutines are required:
C     
C     pksearch : locates the profile horns
C     plfit    : sets up input functions for lfit
C     lfit     : sets up the normal equations matrix,computes the
C                coefficients of the best fit polynomial
C     gaussj   : does the matrix inversion
C     covsrt   : calculates the covariance matrix of the coefficients
C     funcs    : defines the fitting functions (i.e. polynomials)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                         INPUT VARIABLES
C
C   X       is the array containg the spectrum which is to be measured
C   Y       is the array containing the velocity information for X 
C   IFIRST  is the (user set) channel number of the left edge of the spectrum
C   ILAST   is the (user set) channel number of the right edge of the spectrum
C   RMS     rms noise in the spectrum (as estimated by base.f)
C   F       specifies the levels between which to do the polynomial
C           fitting and the level at which to measure. A double prec
C           number of the form 0.abc, where 0.a and 0.b are the upper
C           and lower fractions of the peak flux between which to 
C           do the fitting and 0.c is the fraction of the peak flux
C           at which to measure
C             
C                         OUTPUT VARIABLES
C
C   SDV     is the area under the spectrum within the IFIRST and ILAST
C   W       is the width of the profile
C   V       is the average velocity of the profile
C   VRMS    is the rms error associated with the velocity measurement
C
C
C                           INTERNAL VARIABLES
C   
C    A(I)   parameters of the POLYNOMIAL fit x = A(1) + A(2)*Y + ..
C    COVAR  MATRIX CONTAINING COVARIANCE OF THE COEFFICIENTS
C           OF THE FITTED POLYNOMIALS
C    dV     VELOCITY INTERVAL BETWEEN TWO NEIGHBOURING CHANNELS
C    FPL    LEFT HALF PEAK FLUX LEVEL IN THE SPECTRUM
C    FPR    RIGHT HALF PEAK FLUX LEVEL IN THE SPECTRUM
C    FR1    FRACTION OF PEAK FLUX AT WHICH TO STOP LINEAR FIT TO PROFILE
C    FR2    FRACTION OF PEAK FLUX AT WHICH TO START LINEAR FIT TO PROFILE
C    FR     FRACTION OF PEAK FLUX AT WHICH TO CALCULATE WIDTH
C    I      DUMMY INTEGERS FOR LOOPING CONTROL
C    IFR1L  LEFT FR1 CHANNEL
C    IFR2L  LEFT FR2 CHANNEL
C    IFR1R  RIGHT FR1 CHANNEL
C    IFR2R  RIGHT FR1 CHANNEL
C    IFRL   LEFT FR CHANNEL
C    IFRR   RIGHT FR CHANNEL
C    IOS    STATUS FLAG FOR IO
C    JUNK   VARIABLE IN WHICH TO TRAP WRONG TERMINAL INPUT
C    RESET  FLAGS WETHER USER HAS RESET PARAMETERS
C    TEMP   TEMPORARY VARIABLE USED TO DECODE F
C    VRMSL  RMS ERROR ASSOCIATED WITH THE VELOCITY ESTIMATE
C           AT FLUX LEVEL FR OF THE LEFT HALF OF THE PROFILE
C    VRMSR  RMS ERROR ASSOCIATED WITH THE VELOCITY ESTIMATE
C           AT FLUX LEVEL FR OF THE RIGHT HALF OF THE PROFILE
C    YFRL   LEFT FR POINT
C    YFRR   RIGHT FR POINT
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      Real X(*),Y(*),SDV,V,W
      Real FTST1,FTST2,RMS ,F

      INTEGER*4 IFIRST,ILAST,NORDER,
     *          IFR1L,IFR2L,IFRL,IFR1R,IFR2R,
     *          IFRR,I,IPL,IPR, ifrp1, ifrm1, n1,
     *          IFRR1,IFRR2,IFRL1,IFRL2
      Real VFRL,VFRR,YFRL,YFRR,dV,A(3),
     *                 FR1,FR2,FR,FPL,FPR,FPL,
     *                 VRMSL,VRMSR,VRMS,COVAR(3,3),
     *                 YFRL1,YFRL2,YFRR1,YFRR2,TEMP
      character*80 stch
      integer*2 istch(40), nb, n80, IOS
c
      equivalence (istch, stch)

      LOGICAL RETRY
c
      data n80/80/, n1/1/


C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C            DECODE F INTO THE APPROPRIATE FRACTONS
C
C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

         

      RETRY = .FALSE.

 90   IF((.NOT.RETRY).AND.((F.GE.1.0).OR.
     *     F.LE.0.0)) THEN

         IOS = 0
 	 stch = ' '
	 call pwrite(istch,n80)
	 stch = 'Specify profile levels for fit; check help file for format'
  	 call pwrite(istch, n80)
	 call pread2('#', stch, nb)
         READ(stch,*,IOSTAT=IOS) F

         IF(IOS.NE.0) THEN
            
            IOS = 0
	    f = 0.0
            stch = 'Bad input... Try again...'
	    call pwrite(istch, n80)
            GOTO 90

         ENDIF

         IF ((F.GE.1.0).OR.(F.LE.0.0)) THEN

            stch = 'Bad input... Try again...'
	    call pwrite(istch, n80)
            GOTO 90

         ENDIF

            
      ENDIF

      IF (F.NE.0.0) THEN

         TEMP = F
         FR1 = (INT(TEMP*10.0)/10.0)
         TEMP = TEMP -FR1

         FR2 = (INT(TEMP*100.0)/10.0)
         TEMP = TEMP - FR2*0.1

         FR = (NINT(TEMP*1000.0)/10.0)
         
         IF((FR.GT.FR1).OR.(FR.LT.FR2).OR.
     *       (FR1.EQ.0.0).OR.(FR2.EQ.0.0).OR.
     *        (FR1.LE.FR2)) THEN
           
            stch = 'Error in polynomial fitting; ' // 
     .		'you need to reset the fitting levels'
	    call pwrite(istch,n80)
            GO TO 90

         ENDIF

      ELSE

         F = FR1 + 0.1*FR2 +0.01*FR

      ENDIF



      
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      
C                       LOCATE THE PROFILE HORNS
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      CALL PKSEARCH(X,IFIRST,ILAST,IPL,IPR,FPL,FPR)

      
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C             locate the FR1 and FR2 points
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      DO 102 I=IPL,IFIRST,-1

         IF(X(I).LT.(FR1*FPL)) GOTO 201

 102  CONTINUE
      i = ifirst - 1
         
 201  IFR1L = I
      
      DO 103 I=  IPL,IFIRST,-1

         IF(X(I).LT.FR2*FPL) GOTO 202

 103  CONTINUE
      i = ifirst - 1

 202  IFR2L = I

      IF(IFR1L.LE.IFR2L) THEN
         
         IFR1L = IFR2L + 1
         stch = 'Warning: upper and lower profile points coincide'
	 call pwrite(istch, n80)

      ENDIF

      
      DO 104 I=IPR, ILAST

         IF(X(I).LT.FR1*FPR) GOTO 203

 104  CONTINUE
      i = ilast + 1

 203  IFR1R = I

      
      DO 105 I=IPR,ILAST

         IF(X(I).LT.FR2*FPR) GOTO 204

 105  CONTINUE
      i = ilast + 1

 204  IFR2R = I

      IF(IFR1R.GE.IFR2R) THEN

         IFR2R = IFR1R + 1
         stch = 'Warning: upper and lower profile points coincide'
	 call pwrite(istch, n80)

      ENDIF

      IF(.NOT.RETRY)  NORDER = 1


C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          reduce the peak flux levels by the RMS
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      FPL = FPL - RMS
      FPR = FPR - RMS


C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C                 locate the FR points
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      IFRL1 = IFIRST
      DO 601 I = IPL, IFIRST,-1
         
         IF(X(I).LT.FR*FPL) GOTO 602

 601  CONTINUE
      i = ifirst - 1

 602  IFRL1 = I


      ifrp1 = IFRL1+1
      CALL PLFIT(X,IFRL1,ifrp1,RMS,n1,A,FTST1,COVAR)
      YFRL1 = (FPL*FR -A(1))/A(2)

      IFRL2 = IPL
      DO 603 I = IFIRST,IPL
         
         IF(X(I).GT.FR*FPL) GOTO 604

 603  CONTINUE
      i = ipl + 1

 604  IFRL2 = I


      ifrm1 = IFRL2-1
      CALL PLFIT(X,ifrm1,IFRL2,RMS,n1,A,FTST1,COVAR)
      YFRL2 = (FPL*FR -A(1))/A(2)
      
      YFRL = (YFRL1+YFRL2)/2.0
      IFRL = INT(YFRL)


      IFRR1 = IPR
      DO 605 I = IPR, ILAST
         
         IF(X(I).LT.FR*FPR) GOTO 606
 605  CONTINUE
      i = ilast + 1

 606  IFRR1 = I


      ifrm1 = IFRR1-1
      CALL PLFIT (X,ifrm1,IFRR1,RMS,n1,A,FTST2,COVAR)
      YFRR1 = (FPR*FR -A(1))/A(2)

      IFRR2 = IPR
      DO 607 I = ILAST,IPR,-1
         
         IF(X(I).GT.FR*FPR) GOTO 608

 607  CONTINUE
      i = ipr - 1

 608  IFRR2 = I


      ifrp1 = IFRR2+1
      CALL PLFIT (X,IFRR2,ifrp1,RMS,n1,A,FTST2,COVAR)
      YFRR2 = (FPR*FR -A(1))/A(2)

      YFRR = (YFRR1 + YFRR2)/2.0
      IFRR =INT(YFRR)

      
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   
C         fit a polynomial of order NORDER to the sides
C         of the profile and calculate the velocity
C         uncertainity
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      
      CALL PLFIT (X,IFR2L,IFR1L,RMS,NORDER,A,FTST1,COVAR)

      IF (NORDER.EQ.1)  VRMSL = RMS/A(2)*
     *                          (Y(IFRL)- Y(IFRL-1))
      
      IF(NORDER.EQ.2)   VRMSL = RMS/(2.0*A(3)*YFRL+A(2))*
     *                          (Y(IFRL)-Y(IFRL-1))

      
      CALL PLFIT (X,IFR1R,IFR2R,RMS,NORDER,A,FTST2,COVAR)

      IF (NORDER.EQ.1)  VRMSR = RMS/A(2)*
     *                          (Y(IFRR)-Y(IFRR-1))
      
      IF(NORDER.EQ.2)   VRMSR = RMS/(2.0*A(3)*YFRR+A(2))*
     *                          (Y(IFRR)-Y(IFRR-1))
      
      

C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       if the fit is poor let the user reset the parameters
C
C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



      IF ((ABS(FTST1).GT.3.0).OR.(ABS(FTST2).GT.3.0)) THEN

	 stch = 'Poor fit to profile; the normalized chisqr are: '
	 call pwrite(istch, n80)
         WRITE(stch,'(2f6.1)',iostat=ios) FTST1,FTST2
         call pwrite(istch,n80)
         write(stch,10,iostat=ios) IFR1L-IFR2L+1,IFR2R-IFR1R+1
 10      format('# of points used to fit l & r halves are',x,i3,x,i3 )
         call pwrite(istch,n80)

	 retry = .true.
         call pread2(
     .	   'Do you want to fit a higher degree polynomial (y/n [y])? ',
     .	   stch, nb)
         if (stch(1:1) .eq. 'n' .or. stch(1:1) .eq. 'N') retry = .false.

         IF (RETRY) THEN
            
 22         stch = ' '
	    call pwrite(istch,n80)
	    stch = 'Specify polynomial order (<= 2) & profile levels between which to fit.'
	    call pwrite(istch,n80)
	    stch = 'Levels coded as before, 0 0 => use 2nd order & orignal profile levels.'
	    call pwrite(istch, n80)
	    call pread2('Enter ORDER:', stch, nb)
            READ(stch,*,IOSTAT=IOS) NORDER
	    if (ios .eq. 0) then
		call pread2('Enter Levels:', stch, nb)
            	READ(stch,*,IOSTAT=IOS) f
	    endif
            IF(IOS.NE.0) THEN
               
               IOS = 0
               stch = 'Try again'
	       call pwrite(istch, n80)
               GOTO 22
               
            ENDIF

               IF ((F.GE.1.0).OR.(F.LT.0.0).OR.
     *             (NORDER.LT.0).OR.(NORDER.GT.2)) THEN
            
                  stch = 'Try again'
	          call pwrite(istch, n80)
                  GOTO 22
                  
               ENDIF

            IF(NORDER.LE.0) NORDER = 2

	    GOTO 90

            
         ENDIF

      ENDIF
     
C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C        calculate the velocity width and average velocity
C
C    CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      IFRL = INT(YFRL)
      VFRL = Y(IFRL) + (YFRL - float(IFRL))*(Y(IFRL+1)-Y(IFRL))

       
      IFRR = INT(YFRR)
      VFRR = Y(IFRR) + (YFRR - float(IFRR))*(Y(IFRR+1)-Y(IFRR))

      V = (VFRL + VFRR)/2.0

      W = (VFRR - VFRL)

      VRMS = SQRT((VRMSL**2 +VRMSR**2)/2.0)



C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C        calculate the integrated flux in bounded region

C     CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      SDV = 0.0

      DO 106 I = IFIRST, ILAST

         dV = Y(I) - Y(I-1)
         SDV = SDV +X(I)*dV

 106  CONTINUE


      RETURN

      END



*************************************************************************
      subroutine pksearch(X,IFIRST,ILAST,IPL,IPR,FPL,FPR)
*************************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     this subroutine is a first step at automating the velocity
C     measurement for double horned profiles. The subroutine
C     will return the fluxes and channel numbers of the two 
C     horns. First the entire profile between the user flaged
C     limits IFIRST and ILAST (IFIRST < ILAST), is searched
C     for peaks, a peak being defined as a channel which has
C     more flux than NISO channels on either side of it. NISO
C     is scaled linearly with the width of the profile (i.e.
C     IFIRST - ILAST) and is 5 when (IFIRST - ILAST) = 40).
C     If the profile has only one peak, then it is treated
C     as a double horned porfile in  which the peaks are 
C     coincident. If there are more than two peaks then
C     the outer two peaks are regarded as the horns of 
C     profile UNLESS one of the inner peaks is more than
C     a factor of FACT times larger than an exterior peak.
C     in that case the closest exterior peak is replaced
C     by the large interior peak. In case there is more than
C     one interior peak that is a factor of FACT larger than
C     the exterior peak, the outer most large peak is
C     used.
C
C                      INPUT VARIABLES
C
C     X        array containg the profile
C     IFIRST   user defined left end of the profile
C     ILAST    user defined right end of the profile
C     
C     
C                     OUTPUT VARIABLES
C     
C     IPL      channel number of left peak
C     IPR      channel number of right peak
C     FPL      Flux of the left peak
C     FPR      Flux of the right peak
C     
C                    INTERNAL VARIABLES
C     
C     PMAX     max number of peaks allowed
C     NPEAK    number of peaks in the profile
C     PKCHAN   array containing channel numbers of the peaks
C     NISO     a peak is defined as a channel with more
C              flux than  NISO channels on either side
C     ICNTR    the midpoint of the profile
C     PCNTR    the channel number of the central peak
C     I,J,K    dummy integers for  looping control
C     FACT     an internal peak has to be greater than
C              FACT times an exterior peak to be considered
C              the profile horn
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      INTEGER*4 PMAX
      PARAMETER(PMAX=20)
      
      Real X(*),FPL,FPR
      INTEGER*4  IPR,IPL,IFIRST,ILAST

      INTEGER*4 NISO,NPEAK,PKCHAN(PMAX),PCNTR,ICNTR,
     *          I,J,K
      Real FACT



      IPL = IFIRST
      FPL = X(IFIRST)
      IPR = ILAST
      FPR = X(ILAST)
      FACT = 2.0


C
C     A CHANNEL HAS TO BE LARGER IN VALUE THAN NISO
C     CHNNELS ON EITHER SIDE TO BE CONSIDERED A
C     PEAK
C


      NISO = INT(5.0*(ILAST - IFIRST)/40.0)
      IF(NISO.LT.3) NISO = 3
      if(niso.gt.7) niso = 7





      
C     FIND ALL PEAKS BETWEEN THE FLAGED LIMITS


      K  = 0
      DO 400 I =IFIRST+NISO, ILAST-NISO

         DO 401 J = 1,NISO
            
            IF((X(I).LT.X(I+J)).OR.(X(I).LT.X(I-J))) GOTO 402

 401     CONTINUE

         K = K + 1
         PKCHAN(K) = I

 402     CONTINUE 

 400  CONTINUE

      NPEAK = K 
C
C     IF THERE IS ONLY ONE PEAK, THE LEFT AND RIGHT
C     SIDE HORNS ARE COINCIDENT
C        

      
      IF (NPEAK.EQ.1) THEN
         
         IPL = PKCHAN(1)
         FPL = X(IPL)
         IPR = IPL
         FPR = FPL
         
      ELSE

C
C        IF THERE ARE ONLY TWO PEAKS, THEN STORE THE
C        PEAK FLUXES AND CHANNEL NUMBERS
C

         IF(NPEAK.EQ.2) THEN
            
            IPL = PKCHAN(1)
            IPR = PKCHAN(2)
            FPL = X(IPL)
            FPR = X(IPR)

         ELSE



C
C           IF THERE ARE MORE THAN TWO PEAKS TREAT THE 
C           OUTERMOST TWO PEAKS AS THE PROFILE HORN,
C           UNLESS ONE OF THE INTERIOR PEAKS IS MORE
C           THAN FACT TIMES LARGER THAN THE EXTERIOR
C           PEAKS (SEE EXPLANATION AT START)
C

            IPL = PKCHAN(1)
            FPL = X(IPL)
            IPR = PKCHAN(NPEAK)
            FPR = X(IPR)

            ICNTR = (PKCHAN(NPEAK) -PKCHAN(1))/2
            
            PCNTR =  2
            
            DO 403, J = 2,NPEAK-1
               
               IF(PKCHAN(J).LT.ICNTR) PCNTR = J

 403        CONTINUE


            J = PCNTR
            
 404        IF(J.GE.2) THEN
               
               IF(X(PKCHAN(J)).GT.FACT*X(PKCHAN(1))) THEN

                  IPL = PKCHAN(J)
                  FPL = X(IPL)
         

               ENDIF

               J = J -1
               GOTO 404

            ENDIF



            J = PCNTR + 1
            
 405        IF(J.LE.NPEAK -1) THEN
               
               IF(X(PKCHAN(J)).GT.FACT*X(PKCHAN(NPEAK))) THEN

                  IPR = PKCHAN(J)
                  FPR = X(IPR)
         
               ENDIF

               J = J + 1
               GOTO 405

            ENDIF


         ENDIF

      
      ENDIF

      RETURN
         
      END

      

*******************************************************************************

      SUBROUTINE PLFIT(X,I1,I2,RMS,NORDER,A,FITRMS,COVAR)

****************************************************************************

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     sets up the input parameters for lfit, computes goodness of 
C     fit
C
C                      INPUT PARAMETERS
C     
C     X      array of values for fitting the polynomial
C     I1,I2, limits between which to fit
C     RMS    rms error in the data points X
C     NORDER fitting polynomial order
C     COVAR  covaraince matrix of coefficients A(I)
C
C                      OUTPUT PARAMETERS
C   
C     A      vector containing coeficients of the fit polynomial
C            X = A(1) +A(2)*Y +..
C     FITRMS goodness of fit normalised to 0 mean, unit variance
C     
C                     INTERNAL VARIABLES
C
C     XA     X shifted left by I1
C     YA     array containg the channel numbers from I1 to I2,
C            this is the independent variable for the fitting
C            routine
C     NMAX   max number of data points  allowed
C     NCVM   max number of fitting coefficients allowed
C     SIG    array containg the rms of each data point 
C             i.e. SIG(I) = RMS for all I
C     CHISQR chisqr of the fit
C     MFIT   number of coeficients to fit
C     MA     number of coeficients (i.e. MA - MFIT coefficients
C            are held fixed
C     NDATA  number of data points
C     NFREE  number of degrees of freedom
C     I      dummy integer for looping control
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 

      integer*4 NMAX,NCVM
      PARAMETER (NMAX = 50)
      PARAMETER (NCVM = 3)

      Real X(*),A(NCVM),FITRMS,RMS
      INTEGER*4 I1,I2,NORDER
      Real XA(NMAX),YA(NMAX),SIG(NMAX)
      Real COVAR(NCVM,NCVM),CHISQR
      INTEGER*4 MFIT,MA,NDATA,LISTA(NCVM),I,NFREE
      character*80 stch
      integer*2 istch(40), ios, n80
c
      external funcs
c
      equivalence (stch, istch)
c
      data n80/80/
c
      NDATA = min(NMAX, I2 -I1 + 1)

      DO 100 I = 1,NDATA

         XA(I) = X(I+I1 -1)
         YA(I) = float(I+I1 -1)
         SIG(I) = RMS

 100  CONTINUE


      MFIT = NORDER + 1
      MA = MFIT
      
      DO 101 I =1, MA
         
         LISTA(I) = 0
         IF(I.LE.(NORDER+1)) LISTA(I) = I

 101  CONTINUE

      CALL LFIT(YA,XA,SIG,NDATA,A,MA,LISTA,MFIT,COVAR,NCVM,CHISQR,funcs)
      

      NFREE = NDATA - MFIT
      
      IF (NFREE.GT.0) THEN

         FITRMS = (CHISQR - NFREE)/SQRT(2.0*NFREE)

      ELSE

         FITRMS = 0.0
         IF(NFREE.LT.0) THEN

            WRITE(stch,20,iostat=ios)NORDER,NDATA
 20         FORMAT('Warning:',i5,'order polynomial fit to',
     *           i5,'points')
            call pwrite(istch,n80)
         ENDIF

      ENDIF

      RETURN 
      
      END

******************************************************************************

      SUBROUTINE  FUNCS (X,AFUNC,MA)
      
******************************************************************************

	
      INTEGER*4 MA
      Real AFUNC(MA),X
      INTEGER*4 I      


      AFUNC(1) = 1.0
 
      DO 100 I = 2 ,MA
      
         AFUNC(I) = AFUNC(I-1)*X

 100  CONTINUE

   

      RETURN

      END


********************************************************************************
      Real FUNCTION FRAC ( X1, X2, F)
********************************************************************************
C
C     Interpolates to find proper velocity width boundaries
C
      Real X1, X2, F
C
      FRAC = (F-X1)/(X2-X1)
c
      RETURN
      END
