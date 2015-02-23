      double precision function xtochan(unittype, array, v1, ierr)
c
c     @(#)xtochan.F	5.2 07/20/94
c
c     Converts X1 from units specified by UNITTYPE to channel number
c	for array ARRAY.
c
c     UNITTYPE = (I*2) Units of V1.
c  
c		Continuum     	   	   Spectral Line
c		-------------------------------------------
c		-2 = matrix -x	   	    -2 = matrix -x
c		-1 = matrix -y	   	    -1 = matrix -y
c		1 = channel	   	    1 = channel
c		2 = time offset	   	    2 = frequency offset
c		3 = pos. offset (header)    3 = velocity (header)
c		4 = pos. offset-x   	    4 = velocity (Radio)
c		5 = pos. offset-y  	    5 = velocity (Optical)
c		6 = position x	  	    6 = velocity (Relativity)
c		7 = position y	  	    7 = velocity (Linear)
c		8 = LST			    8 = rest frequency  (sig)
c		9 = UT	 	    	    9 = sky frequency (sig)
c			  		    *10 = rest frequency  (imag)
c					    *11 = sky frequency (imag) 
c
c     Those items preceeded with a * been implemented for 12m data only
c
c     IERR = (I*2) If conversion is not possible, ierr will be 1, else
c	it will be 0.
c     V1 = value to be converted.         
c	
      integer*2 unittype, ierr
      real*8 array(*), lstut
      real*4 v1
      logical okreal8
c
#ifdef CONTINUUM
#else
      integer isb
      double precision veltochan, dveldef, fshift, nrao12m
      character*8 veldef, cnrao12m
      equivalence (dveldef, veldef), (nrao12m, cnrao12m)
c
      data cnrao12m /'NRAO 12M'/
c
#endif
c
      include 'cform.inc'
      include 'mform.inc'
c
      data lstut/1.002737908d0/
c
      ierr = 0
c
      if (unittype .eq. -2  .and. 
     .	       array(mdelt1) .ne. 0 .and. okreal8(array(mrval1)) .and. 
     .	       okreal8(array(mdelt1)) .and. okreal8(array(mpix1)) ) then
	xtochan = (v1-array(mrval1))/array(mdelt1) + array(mpix1)
c-----------------------
c
      else if (unittype .eq. -1  .and. array(mdelt2) .ne. 0 .and. 
     .	       okreal8(array(mrval2)) .and. okreal8(array(mdelt2)) .and. 
     .         okreal8(array(mpix2)) ) then
	xtochan = (v1-array(mrval2))/array(mdelt2) + array(mpix2)
c-----------------------
c
      else if (unittype .eq. 1) then
	xtochan = v1
c-----------------------
c
#ifdef CONTINUUM
c
      else if (unittype .eq. 2 .and. array(c3srt) .ne. 0. .and. 
     .	    okreal8(array(c3srt))  .and. okreal8(array(c12rp)) ) then 
     	xtochan = v1/array(c3srt) + array(c12rp)
c-----------------------
c
      else if (unittype .eq. 3 .and. array(c12dx) .ne. 0 .and.
     .	       okreal8(array(c12dx)) .and. okreal8(array(c12x0)) .and.
     .         okreal8(array(c12rp)) ) then
     	xtochan = array(c12rp) + (v1-array(c12x0))/array(c12dx)
c-----------------------
c
      else if (unittype .eq. 4 .and. array(c6dx) .ne. 0 .and.
     .	       okreal8(array(c6dx)) .and. okreal8(array(c12rp)) ) then
     	xtochan = array(c12rp) + 3600.*v1/(array(c6dx)*array(c3srt))
c-----------------------
c
      else if (unittype .eq. 5 .and. array(c6dy) .ne. 0 .and.
     .	       okreal8(array(c6dy)) .and. okreal8(array(c12rp)) ) then
     	xtochan = array(c12rp) + 3600.*v1/(array(c6dy)*array(c3srt))
c-----------------------
c
      else if (unittype .eq. 6 .and. array(c6dx) .ne. 0 .and.
     .	       okreal8(array(c6dx)) .and. okreal8(array(c4sx)) .and.
     .         okreal8(array(c12rp)) ) then
     	xtochan = array(c12rp) + 3600.*(v1-array(c4sx))/(array(c6dx)*array(c3srt))
c-----------------------
c
      else if (unittype .eq. 7 .and. array(c6dy) .ne. 0 .and.
     .	       okreal8(array(c6dy)) .and. okreal8(array(c4sy)) .and.
     .         okreal8(array(c12rp)) ) then
     	xtochan = array(c12rp) + 3600.*(v1-array(c4sy))/(array(c6dy)*array(c3srt))
c-----------------------
c
      else if (unittype .eq. 8 .and. array(c3srt) .ne. 0. .and. 
     .	    okreal8(array(c3srt))  .and. 
     .	    okreal8(array(c12rp)) .and. okreal8(array(c3lst)) ) then 
     	xtochan = 3600.*(v1 - array(c3lst))/(lstut*array(c3srt)) + 
     .		array(c12rp)
c-----------------------
c
      else if (unittype .eq. 9 .and. array(c3srt) .ne. 0. .and. 
     .	    okreal8(array(c3srt))  .and. 
     .	    okreal8(array(c12rp)) .and. okreal8(array(c3ut)) ) then 
     	xtochan = 3600.*(v1 - array(c3ut))/array(c3srt) + array(c12rp)
c-----------------------
c
#else
c 
      else if (unittype .eq. 2  .and. array(c12fr) .ne. 0 .and. 
     .	       okreal8(array(c12fr)) .and. okreal8(array(c12rp)) ) then
	xtochan = array(c12rp) + v1/array(c12fr)
c-----------------------
c
      else if (unittype .eq. 3) then
	dveldef = array(c7vrd)
	xtochan = veltochan(v1,array,veldef(1:4),ierr)
c-----------------------
c
      else if (unittype .eq. 4) then
	xtochan = veltochan(v1,array,'RADI',ierr)
c-----------------------
c
      else if (unittype .eq. 5) then
	xtochan = veltochan(v1,array,'OPTL',ierr)
c-----------------------
c
      else if (unittype .eq. 6) then
	xtochan = veltochan(v1,array,'RELV',ierr)
c-----------------------
c
      else if (unittype .eq. 7) then
	xtochan = veltochan(v1, array, '    ',ierr)
c-----------------------
c
      else if (unittype .eq. 8 .and. array(c12fr) .ne. 0 .and.
     .	       okreal8(array(c12rf)) .and. 
     .	       okreal8(array(c12fr)) .and. okreal8(array(c12rp)) ) then
	xtochan = array(c12rp) + (v1 - array(c12rf))/array(c12fr)
c-----------------------
c
      else if (unittype .eq. 9 .and. array(c12fr) .ne. 0 .and.
     .	       okreal8(array(c12cf)) .and. 
     .	       okreal8(array(c12fr)) .and. okreal8(array(c12rp)) ) then
	xtochan = array(c12rp) + (v1 - array(c12cf))/array(c12fr)
c-----------------------
c
      else if ((array(c1tel) .eq. nrao12m) .and.
     .        ((unittype .eq. 10) .or. (unittype .eq. 11))) then
         if (okreal8(array(c9fif)) .and. okreal8(array(c9sb)) .and.
     .       okreal8(array(c12fr)) .and. okreal8(array(c12rp)) .and.
     .       okreal8(array(c12cf)) .and. array(c12fr) .ne. 0.0) then
c
            isb = ifix(array(c9sb))
            if ((isb .eq. 0) .or. (isb .eq. 2)) then
               fshift = -2.0 * array(c9fif) / 1000.0
            else
               fshift = 2.0 * array(c9fif) / 1000.0
            endif
c
            if (unittype .eq. 10 .and. okreal8(array(c12rf)) .and.
     .          array(c12cf) .ne. 0.0) then
               xtochan = array(c12rp) + (v1 - array(c12rf)*
     .            (1.0 + fshift / array(c12cf))) /(-1.0*array(c12fr))
            else if (unittype. eq. 11) then
               xtochan = array(c12rp) + 
     .            (v1 - array(c12cf) - fshift) / (-1.0*array(c12fr))
            else
               ierr = 1
               xtochan = v1
            endif
         endif
#endif
c
      else
	ierr = 1
	xtochan = v1
c
      endif
c
      return
      end
c
      logical function okarray(unittype, array)
c
c     Tests whether a following conversion with XTOCHAN or CHANTOX will
c	produce an error.
c
      integer*2 unittype
      double precision array(*)
c
      double precision blah, xtochan, chantox
      integer*2 ind1, ind2, ierr
c
      include 'cform.inc'
      include 'mform.inc'
c
      if (unittype .eq. -2) then
	ind1 = mrval1
	ind2 = mpix1
      else if (unittype .eq. -1) then
	ind1 = mrval2
 	ind2 = mpix2
      else 
	ind1 = c12x0
	ind2 = c12rp
      endif
c
      blah = xtochan(unittype, array, sngl(array(ind1)), ierr)
      if (ierr .ne. 0) then
	   okarray = .false.
      else
	   blah = chantox(unittype, array, sngl(array(ind2)), ierr)
	   if (ierr .ne. 0) then
	   	okarray = .false.
	   else
	        okarray = .true.
	   endif
      endif
c
      return
      end

