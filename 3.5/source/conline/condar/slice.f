      subroutine slice(face, x1, x2)
c
c     @(#)slice.f	5.4 05/04/98
c
c     Slices a 3-d FITS image file (cube) to produce a matrix; stores results
c	in Matrix (0).  
c
c     FACE = face of the cube perpendicular to which the cibe will be
c	integrated and parralled to whic the slice will be made.  It's
c	value must be 1, 2, or 3.
c     X1, X2 = lower and upper pixel numbers away from the specified face
c	overwhich integration is to occur.  Must be within the
c	bounds of the cube
c
      integer*4 x1, x2, r1, r2, d1, d2, v1, v2, mloc, i, j, numtrig,
     .		iloc, ii, jj, numcells, numdone, istch(40),
     .		iarray(3), time0, time1
      integer*2 face, ilen, lastblnk, r, v, d, ierr
      integer*4 l1, l2, l6, l15, l24, l60 
      integer*2 m2, n112, n80
      real*4 cube, cval, factor
      real*8 dinfinity
      character*80 stch
c
      include 'mappl.inc'
      include 'mform.inc'
c
      equivalence (istch, stch)
c
      data m2, n112, n80
     .    /-2, 112, 80/
      data l1, l2, l6, l15, l24, l60
     .   /1, 2, 6, 15, 24, 60/
c
      mloc(ii,jj) = ii + mhead(mnaxis1,1)*(jj-1)
c     Function which return the location in matrix of a specified pixel.
c
      if (face .lt. 1 .or. face .gt. 3) 
     .	call oerror (n112, m2, 'SLICE:  Bad FACE argument')
c     Gets the face and checks up on it
c
      if (face .eq. 1) then
	if (x1 .lt. 1 .or. x2 .lt. x1 .or. x2 .gt. chead(cnaxis1)  )
     .		call oerror(n112, m2, 'SLICE: Bad X1 or X2 argument')
	v1 = x1
	v2 = x2
	r1 = 1
	r2 = chead(cnaxis2)
	d1 = 1
	d2 = chead(cnaxis3)
	mhead(mnaxis1,1) = chead(cnaxis2)
	mhead(mnaxis2,1) = chead(cnaxis3)
	mhead(mrval1,1) = chead(crval2)
	mhead(mrval2,1) = chead(crval3)
	mhead(mdelt1,1) = chead(cdelt2)
 	mhead(mdelt2,1) = chead(cdelt3)
	mhead(mpix1,1) = chead(cpix2)
	mhead(mpix2,1) = chead(cpix3)
	cmhead(mtype1,1) = cchead(ctype2)
	cmhead(mtype2,1) = cchead(ctype3)
      	ilen = lastblnk(cchead(cbunit))
     	cmhead(mbunit,1) = cchead(cbunit)(1:ilen) // '*' // cchead(ctype1)
	factor = chead(cdelt1)
      else if (face .eq. 2) then
	if (x1 .lt. 1 .or. x2 .lt. x1 .or. x2 .gt. chead(cnaxis2) )
     .		call oerror(n112, m2, 'SLICE: Bad X1 or X2 argument')
	v1 = 1
	v2 = chead(cnaxis1)
	r1 = x1
	r2 = x2
	d1 = 1
	d2 = chead(cnaxis3)
	mhead(mnaxis1,1) = chead(cnaxis1)
	mhead(mnaxis2,1) = chead(cnaxis3)
	mhead(mrval1,1) = chead(crval1)
	mhead(mrval2,1) = chead(crval3)
	mhead(mdelt1,1) = chead(cdelt1)
 	mhead(mdelt2,1) = chead(cdelt3)
	mhead(mpix1,1) = chead(cpix1)
	mhead(mpix2,1) = chead(cpix3)
	cmhead(mtype1,1) = cchead(ctype1)
	cmhead(mtype2,1) = cchead(ctype3)
      	ilen = lastblnk(cchead(cbunit))
     	cmhead(mbunit,1) = cchead(cbunit)(1:ilen) // '*' // cchead(ctype2)
	factor = chead(cdelt2)
      else if (face .eq. 3) then
	if (x1 .lt. 1 .or. x2 .lt. x1 .or. x2 .gt. chead(cnaxis3) ) 
     .		call oerror(n112, m2, 'SLICE: Bad X1 or X2 argument')
	v1 = 1
	v2 = chead(cnaxis1)
	r1 = 1
	r2 = chead(cnaxis2)
	d1 = x1
	d2 = x2
	mhead(mnaxis1,1) = chead(cnaxis1)
	mhead(mnaxis2,1) = chead(cnaxis2)
	mhead(mrval1,1) = chead(crval1)
	mhead(mrval2,1) = chead(crval2)
	mhead(mdelt1,1) = chead(cdelt1)
 	mhead(mdelt2,1) = chead(cdelt2)
	mhead(mpix1,1) = chead(cpix1)
	mhead(mpix2,1) = chead(cpix2)
	cmhead(mtype1,1) = cchead(ctype1)
	cmhead(mtype2,1) = cchead(ctype2)
      	ilen = lastblnk(cchead(cbunit))
     	cmhead(mbunit,1) = cchead(cbunit)(1:ilen) // '*' // cchead(ctype3)
	factor = chead(cdelt3)
      endif
c     Set up loop indices; check that input params do not go beyond
c     edge of cube; set up some matrix header parameters
c
      if (mhead(mnaxis1,1)*mhead(mnaxis2,1) .lt. 1 .or. 
     .    mhead(mnaxis1,1)*mhead(mnaxis2,1) .gt. mdatasize/mnumarrays) 
     .	call oerror(n112, m2, 'SLICE:  Resulting matrix is too big')
c
      mhead(mdate,1) = chead(cdate)
      cmhead(mobject,1) = cchead(cobject)
      cmhead(mobject+1,1) = cchead(cobject+1)
      cmhead(morigin,1) = cchead(corigin)
      do 101 i = 1, min(7,cheadsize-ccomment+1,mheadsize-mcomment+1)
	cmhead(i-1+mcomment,1) = cchead(i-1+ccomment)
101	continue
      mhead(mequinox,1) = chead(cequinox)
      mhead(mblank,1) = dinfinity()
      mhead(mbitpix,1) = chead(cbitpix)
c     Fill in rest of matrix header
c
      do 110 j = 1, mhead(mnaxis1,1)
	do 100 i = 1, mhead(mnaxis2,1)
	   mdata(mloc(j,i)) = 0.0
100	   continue
110	continue
c      Initialize matrix
c
      if (x1 .eq. x2) then
	factor = 1
	cmhead(mbunit,1) = cchead(cbunit)
      endif
c     Alter the conversion factor from pixel to units of intergration in case
c	no integration is desired.
c 
      numcells = (d2 - d1 +1)*(r2 - r1 + 1)*(v2 - v1 + 1)
      numdone = 0
      numtrig = 32767
      call itime(iarray)
      time0 = iarray(3) + l60*(iarray(2) + l60*iarray(1))
c
      do 220 d = d1, d2
	do 210 r = r1, r2
	   do 200 v = v1, v2
c
c	   	Step thru the CUBE, most rapidly varying axis first
c
		cval = cube(v,r,d)
c		Assign to CVAL the value of the cube at pixel (V,R,D)
c
		if (face .eq. 1) then
		   iloc = mloc(r,d)
		else if (face .eq. 2) then
		   iloc = mloc(v,d)
		else if (face .eq. 3) then
		   iloc = mloc(v,r)
		endif
c		Finds the location in the matrix of the current pixel.
c
		if (cval .ne. chead(cblank)) then
c
c		Assign values to the MATRIX if the CUBE has a data value
c		at the current pixel and the MATRIX also has.
c
		   if (mdata(iloc) .ne. mhead(mblank,1)) then
			mdata(iloc) = mdata(iloc) + cval*factor
c		        Scale data by delta of intergration axis.
c
		   endif
c		   Performs integration along proper face if the matrix
c		   already has a data value at the current pixel; else
c		   don't put any value there.
c
		else
c
	   	   mdata(iloc) = mhead(mblank,1)
c		   If any CUBE pixel along the integration is blank, then
c			the matrix must have a blank value as well
c
		endif
c
		numdone = numdone + l1
		if ( mod(numdone, numtrig) .eq. 0 ) then
		   write(stch,205,iostat=ierr) 100.*float(numdone)/float(numcells)
205		   format('SLICE: ' f5.1, '% completed.')
		   call pwrite(istch, n80)
		   call itime(iarray)
      		   time1 = iarray(3) + l60*(iarray(2) + l60*iarray(1))
		   if (time1 .lt. time0) time1 = time1 + l24*l60*l60
c		   Take care of going tru midnight
c
		   if (time1-time0 .gt. l15) then
			numtrig = max(l1,numtrig / l2)
		   else if (time1-time0 .lt. l6) then
			numtrig = l2*numtrig
		   endif
		   time0 = time1
c		   Update NUMTRIG depending upon how long it has been since
c		   the last time a notice was posted.
c
	 	endif
c		Assures user how much more needs to be done for the slice.
c
200		continue
210	     continue
220	continue
c	We are now finished updating the matrix and should now write it to
c	disk.
c
      return
c
      end
c
