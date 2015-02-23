      SUBROUTINE initappl
C---------------------------------------------------------------------
C  @(#)initappl.f	5.2 09/10/98
C
C	 INITAPPL initializes appl.inc and mappl.inc common areas.
C
C---------------------------------------------------------------------
      INCLUDE 'appl.inc'
      INCLUDE 'mappl.inc'
      INCLUDE 'mform.inc'
C
C
      integer*4 inf, iiappl
      integer*2 i
      logical*2 first
      real*4 rinf, rinfinity
      real*8 dinf, dinfinity
c
      equivalence (rinf, inf)
c
      data first/.true./
c
      rinf = rinfinity()
      dinf = dinfinity()
      if (first) then
	numappl = inf
	iiappl = 1
10	if (Rappl(iiappl) .eq. inf) then
	   numappl = iiappl
	else
	   iiappl = iiappl + 1
	   goto 10
	endif
	first = .false.
      endif
c     Finds out the number of I*4 words in APPL common block
c
      call resetcur
c
      XORG0=1
      YORG0=1
      IX0=XORG(1) + 140
      IY0=YORG(1) + 150
      IXM=XMAX(1) - 40
      IYM=YMAX(1) - 60
      XMAX0=1024
      YMAX0=800
      NACCUM = 0
      ibase = 0
      ires = 0
      lit = 0
      isbg = 0
      LXA=2
      UXA=3
      SPLT=1
      RPLT=0
      limx = 0
      limy = 0
      maxstck = 5120
      maxregn = 32
      maxmrk = 12
      ichnext = 1
      ichnum = 0
C
      do 911 i = 1, maxregn
	rixt(i) = 0
	riyt(i) = 0
	breg(i) = 0 
911	continue
      DO 11 I = 1,maxstck
         astack(I)=0.
         chshifts(i,1) = 0.
         chshifts(i,2) = 0.
   11    CONTINUE
c
      sumsq = 0.0
      sclfctx = 1.0
      sclfcty = 1.0
      sclchar = 1.0
c
      call resetcolor
c
      do 901 i = 1, 10
	call raz(dtwh(1,i))
901	continue
      call raz(daccum)
c
      do 902 i = 1, MAX_DATA_POINTS
	xdata(i) = 0.0
	ydata(i) = 0.0
	work(i) = 0.0
902	continue
c
      do 903 i = 1, 101
	daindex(i) = 0
903	continue
c
      mheadsize = 40
      cheadsize = 40
      mdatasize = 409600
      mnumarrays = 4
      imin = 0
      imax = 0
      jmin = 0
      jmax = 0
      do 904 i = 1, mnumarrays
	call mraz(i)
904	continue
      call craz
c
      RETURN
      END
