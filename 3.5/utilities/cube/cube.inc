c
c     @(#)cube.inc	5.3 03/01/99
c
      include 'params.inc'
c
      real*8 dtwh(HDU_FLOAT_SIZE/2)
      real*4 twh(HDU_FLOAT_SIZE), a0, d0, v0, dac, ddc, dvc, ap, dp, vp, 
     .	     erra, errd, errv, tzero, tmin, tscale, tmax, equinox
      integer*2 lout(1440), itwh(HDU_FLOAT_SIZE*2)
      integer na, nd, nv, isys, ibad, maxrec,inreclngth, outreclngth,
     .	      jrec, mleft, irec, idigit, numhead, numdata, irange, indev,
     .	      ioutdev, iheadmask
      character*8 ctwh(HDU_FLOAT_SIZE/2), testcoord(14)
      character*24 date
      character*70 headerfile
      character*60 label(10)
      character*8 coorda,coordd,coordv,object(2),appepch,origin,
     .            units
      character*4 caxis(2,18)
      logical cosv, debug, afirst
c
      equivalence (dtwh,ctwh),(dtwh,twh),(dtwh,itwh)
c
      common /header/a0,d0,v0,dac,ddc,dvc,ap,dp,vp,erra,errd,
     1               errv,coorda,coordd,coordv,tzero,tscale,tmin,
     2		     tmax,date,object,appepch,origin,units,na,nd,nv,
     3		     ibad,label,equinox
      common /scan/twh,maxrec,mleft,irec,idigit,numhead,jrec,
     .		   inreclngth,isys,cosv, debug, afirst,
     .             headerfile, lout
c
c     A0 = x-axis start or reference position
c     D0 = y-axis start or reference position
c     V0 = velocity reference value
c     NA, ND, NV = number of pixels in the 3 directions
c     DAC, DDC, DVC = step size in the 3 directions
c     AP,DP,VP = reference pixel in cube... Usually  = 1.
c     TZERO, TSCALE = scaling representation of data
c           (i.e. tape value*TSCALE + TZERO = Ta)
c     COORDA, COORDD, COORDV = strings containing x,y,z axis labels.
c     LABEL = string arrays containing user comments
c     DATE = date stored in header of cube
c     OBJECT = name of the object in cube 
c     ORIGIN = place of cube's origin
c     UNITS = units of pixel values in cube    
c     TMAX, TMIN = maximum, minimum data values to be represented in cube 
c     IBAD = value given to bad data points or points with no data
c     ERRA, ERRD, ERRV = if a data point is within these errors in the 3 
c          directions from a cube position, then that data point overwrites
c          the previous data at that position in cube
c     APPEPCH = More descriptive then COORD, COORDA in recording coordinate
c   	   system
c     EQUINOX = equinox of coordinate system, only used if the Y axis is
c               "DEC-"
c
c     DTWH = array containing present scan; DTWH, TWH, ITWH, and CTWH
c     LOUT has dimension OUTRECLENGTH/2 or NUMDATA; contains the
c	 present output record
c     HEADERFILE = name of header file on disk.
c     COSV = TRUE if cosine y-coord is to be corrected for (GLS projection).
c     AFIRST = TRUE if x-axis in data corresponds to 2nd axis in cube
c              i.e. the FIRST spatial axis in the cube is also the first
c              spatial axis in the data.
c     INRECLNGTH = input file record lengths.
c     JREC = Record number of scan about to be read in input file  
c     MAXREC = number of records in cube file
c     MLEFT = number of data items stored in last record of cube.  The
c	remaining items are blank.
c     IREC = output record currently in use
c     IDIGIT = position within output record where data is to go
c     NUMHEAD = number of header records (usually 1).
c     ISYS = coordinate system code: Follows that given by TESTCOORD.
c
      data outreclngth/2880/,numdata/1440/
c     OUTRECLNGTH = output record lengths.  
c     NUMDATAY = # of data elements in each output record 
c	   (= outeclngth/2 for 16 bit data representation)
c
      data irange/32767/
c     output records will have data between -irange and + irange.  
c
      data caxis/'RA--','DEC-',
     .		 'RA--','DEC-',
     .		 'RA--','DEC-',
     .		 'RA--','DEC-',
     .		 'RA--','DEC-',
     .		 'RA--','DEC-',
     .		 'HA--','DEC-',
     .		 'ELON','ELAT',
     .		 'ELON','ELAT',
     .		 'ELON','ELAT',
     .		 'ELON','ELAT',
     .		 'GLON','GLAT',
     .		 'AZ--','EL--',
     .		 'HUSR','VUSR',
     .		 'RA--','DEC-',
     .		 'GLON','GLAT',
     .		 'AZ--','EL--',
     .		 'INDX','INDY'/
      data testcoord/'1950RADC','EPOCRADC','MEANRADC','APPRADC ',
     .		     '2000RADC','INDRADC ','APPHADC ',
     .		     '1950ECL ','EPOCECL ','MEANECL ','APPECL  ',
     .		     'GALACTIC','AZEL    ','USERDEF '/
c     Possible x/y axis labels and coordinate systems. The 1st 14 values
c     of CAXIS refer to the coordinates in TESTCOORD.  The last 4 values
c     of CAXIS are special cases and refer to the always present entries in 
c     the SDD#1 header.  These last four are equivalent to EPOCRADC, GALACTIC,
c     AZEL, and INDICATD.
c
      data indev/25/, ioutdev/27/, iheadmask/29/
c     Input/output device numbers of scan and cube file.  Device number
c     where head.mask file will be found
c
