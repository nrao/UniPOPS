C-------------------------------------------------------------------------------
C  @(#)mappl.inc	5.2 05/04/98
C-------------------------------------------------------------------------------
C
C			Include	MAPPL.
C
      real*4 mdata(409600), fcblank
      double precision mhead(40, 4), chead(40)
      integer*4 cheadsize, mheadsize, mdatasize, mnumarrays, 
     .		imin, imax, jmin, jmax, cnumhead
      integer*2 imhead(160, 4), ichead(160)
      character*8 cmhead(40,4), cchead(40)
C
      COMMON /MAPPL/ chead, mhead, mdata, mheadsize, mdatasize,
     .		     mnumarrays, cheadsize, imin, imax, jmin, jmax,
     .		     cnumhead, fcblank
c
c     MAPPL contain variables which the various algorithms need to
c   	pass to one another for MATRIX and CUBE data reduction.  
c
c     mdata = Matrix data arrays -- actually will be used as if it were
c	a 102400 x 4 array but I*2 numbers dictate that we dimension it
c	this way
c     mhead = Matrix header parameter arrays
c     chead = Cube header parameter arrays
c     cheadsize = number of R*8 words in chead
c     mheadsize = number of R*8 words in mhead
c     mdatasize = number of R*4 words in mdata
c     mnumarrays = number of sub-arrays in mhead and mdata.
c     imin, imax, jmin, jmax = limits of mdata overwhich processing is to
c	be performed; set by MRANGE.
c     cnumhead = number of header records in cube FITS file
c     fcblank = the value of any BLANK keyword in the initial CUBE
c              - if readcube did not find one, this will be a NaN
c
      equivalence (imhead, mhead), (cmhead, mhead)
      equivalence (ichead, chead), (cchead, chead)
