      double precision function chantox(unittype, array, v1, ierr)
c
c     @(#)chantox.F	5.1 06/22/94
c
c     Converts X1 from channel number to units specified by UNITTYPE.
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
c     Those items preceeded with a * have been implemented for 12m data only
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
      double precision chantovel, dveldef, fshift, nrao12m
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
      if (unittype .eq. -2  .and. okreal8(array(mrval1)) .and. 
     .	       okreal8(array(mdelt1)) .and. okreal8(array(mpix1)) ) then
	chantox = (v1-array(mpix1))*array(mdelt1) + array(mrval1)
c-----------------------
c
      else if (unittype .eq. -1  .and. okreal8(array(mrval2)) .and. 
     .	       okreal8(array(mdelt2)) .and. okreal8(array(mpix2)) ) then
	chantox = (v1-array(mpix2))*array(mdelt2) + array(mrval2)
c-----------------------
c
      else if (unittype .eq. 1) then
	chantox = v1
c-----------------------
c
#ifdef CONTINUUM
      else if (unittype .eq. 2 .and. okreal8(array(c3srt)) .and. 
     .         okreal8(array(c12rp)) ) then
	chantox = (v1-array(c12rp))*array(c3srt)
c-----------------------
c
      else if (unittype .eq. 3 .and. okreal8(array(c12dx)) .and.
     .      okreal8(array(c12x0)) .and.
     .      okreal8(array(c12rp)) )  then
        chantox = (v1-array(c12rp))*array(c12dx) + array(c12x0)
c-----------------------
c
      else if (unittype .eq. 4 .and. 
     .	       okreal8(array(c6dx)) .and. okreal8(array(c12rp)) ) then
     	chantox = (v1 - array(c12rp)) * array(c6dx) * array(c3srt)/ 3600.
c-----------------------
c
      else if (unittype .eq. 5 .and. 
     .	       okreal8(array(c6dy)) .and. okreal8(array(c12rp)) ) then
     	chantox = (v1 - array(c12rp)) * array(c6dy) * array(c3srt)/ 3600.
c-----------------------
c
      else if (unittype .eq. 6 .and. 
     .	       okreal8(array(c6dx)) .and. okreal8(array(c4sx)) .and.
     .         okreal8(array(c12rp)) ) then
     	chantox = (v1 - array(c12rp)) * array(c6dx) * array(c3srt)/ 3600.
     .		 + array(c4sx)
c-----------------------
c
      else if (unittype .eq. 7 .and.
     .	       okreal8(array(c6dy)) .and. okreal8(array(c4sy)) .and.
     .         okreal8(array(c12rp)) ) then
     	chantox = (v1 - array(c12rp))*array(c6dy) * array(c3srt) / 3600.
     .		+ array(c4sy)
c-----------------------
c
      else if (unittype .eq. 8 .and. okreal8(array(c3srt))  .and. 
     .	    okreal8(array(c12rp)) .and. okreal8(array(c3lst)) ) then 
     	chantox = (v1 - array(c12rp))*array(c3srt)*lstut / 3600. +
     .			array(c3lst)
c-----------------------
c
      else if (unittype .eq. 9 .and. okreal8(array(c3srt))  .and. 
     .	    okreal8(array(c12rp)) .and. okreal8(array(c3ut)) ) then 
     	chantox = (v1 - array(c12rp))*array(c3srt)/ 3600. +
     .			  + array(c3ut)
c-----------------------
c
#else
      else if (unittype .eq. 2 .and. okreal8(array(c12fr)) .and.
     .	  okreal8(array(c12rp)) ) then
	  chantox = (v1-array(c12rp))*array(c12fr)
c-----------------------
c
      else if (unittype .eq. 3) then
	  dveldef = array(c7vrd)
 	  chantox = chantovel(v1, array, veldef(1:4), ierr)
c-----------------------
c
      else if (unittype .eq. 4) then
	chantox = chantovel(v1, array, 'RADI',ierr)
c----------------------- 
c
      else if (unittype .eq. 5) then
	chantox = chantovel(v1, array, 'OPTL',ierr)
c-----------------------
c
      else if (unittype .eq. 6) then
	chantox = chantovel(v1, array, 'RELV',ierr)
c-----------------------
c
      else if (unittype .eq. 7) then
	chantox = chantovel(v1, array, '    ',ierr)
c-----------------------
c
      else if (unittype .eq. 8 .and. okreal8(array(c12rf)) .and. 
     .	       okreal8(array(c12fr)) .and. okreal8(array(c12rp)) ) then
	chantox = (v1 - array(c12rp))*array(c12fr) + array(c12rf)
c-----------------------
c
      else if (unittype .eq. 9 .and. okreal8(array(c12cf)) .and. 
     .	       okreal8(array(c12fr)) .and. okreal8(array(c12rp)) ) then
	chantox = (v1 - array(c12rp))*array(c12fr) + array(c12cf)
c-----------------------
c
      else if ((array(c1tel) .eq. nrao12m) .and. 
     .        ((unittype .eq. 10) .or. (unittype .eq. 11))) then
         if (okreal8(array(c9fif)) .and. okreal8(array(c9sb)) .and.
     .       okreal8(array(c12fr)) .and. okreal8(array(c12rp)) .and.
     .       okreal8(array(c12cf))) then
C
            isb = ifix(array(c9sb))
            if ((isb .eq. 0) .or. (isb .eq. 2)) then
               fshift = -2.0 * array(c9fif) / 1000.0
            else
               fshift = 2.0 * array(c9fif) / 1000.0
            endif
c
            if (unittype .eq. 10 .and. okreal8(array(c12rf)) .and.
     .          array(c12cf) .ne. 0.0) then
               chantox = (v1 - array(c12rp))*(-1.0*array(c12fr)) +
     .                   (array(c12rf) * (1 + fshift/array(c12cf)))
            else if (unittype .eq. 11) then
               chantox = (v1 - array(c12rp)) * (-1.0 * array(c12fr)) +
     .                   (array(c12cf) + fshift)
            else
               ierr = 1
               chantox = v1
            endif
         endif
#endif
c
      else
	ierr = 1
	chantox = v1
c
      endif
c
      return
      end
c
