      double precision function chantovel( chan, array, veldef, ierr)
c
c----------------------------------------------------------
c @(#)chantovel.f	5.1 06/22/94
c----------------------------------------------------------
c
c     Converts from channel number (CHAN) to velocity for array IARRAY
c     IERR = 0 if all goes OK; VELDEF = RADI, OPTL, RELV, or blanks.
c
      double precision restf, freqres, vsys, cee, ct, freq,
     1		       centerf, vrel, g, array(*)
      integer*2 ierr
      real chan
      character*(*) veldef
      logical okreal8
c
      include 'cform.inc'
c
      data cee/299792.5d0/
c
      restf = array(c12rf)
      centerf = array(c12cf)
      freqres = array(c12fr)
      ct = array(c12rp)
      vsys = array(c7vc)
c
      ierr = 0
c
c--------------------------
      if (veldef .eq. 'RADI' .and. okreal8(restf) .and.
     .	  okreal8(freqres) .and. okreal8(vsys) .and. 
     .	  okreal8(centerf) .and. okreal8(ct) .and. 
     .    centerf .gt. 0.d0 .and. freqres .ne. 0.d0 .and. 
     .	  restf .gt. 0.d0) then
        freq = centerf + (chan-ct)*freqres
	vrel = cee * (restf - freq) / restf
	chantovel =  (vrel - vsys) / (1.d0 - vsys/cee)
c
c--------------------------
      else if (veldef .eq. 'OPTL' .and. okreal8(restf) .and. 
     .     okreal8(freqres) .and. okreal8(vsys) .and. 
     .	   okreal8(centerf) .and. okreal8(ct) .and. 
     .	   centerf .gt. 0.d0 .and. freqres .ne. 0.d0 .and. 
     .	   restf .gt. 0.d0) then
        freq = centerf + (chan-ct)*freqres
	vrel = cee * (restf - freq) / freq
	chantovel =  (vrel - vsys) / (1.d0 + vsys/cee)
c
c--------------------------
      else if (veldef .eq. 'RELV' .and. okreal8(restf) .and. 
     .     okreal8(freqres) .and. okreal8(vsys) .and. 
     .	   okreal8(centerf) .and. okreal8(ct) .and. 
     .     centerf .gt. 0.d0 .and. freqres .ne. 0.d0 .and. 
     .	   restf .gt. 0.d0) then
        freq = centerf + (chan-ct)*freqres
	g = (freq / restf)**2
	vrel = cee * (1.d0 - g) / (1.d0 + g)
	chantovel =  (vrel - vsys) / (1.d0 - vrel*vsys/(cee**2))
c
c
c--------------------------
      else if (veldef .eq. '    ' .and. okreal8(array(c12dx)) .and. 
     .      okreal8(array(c12x0)) .and. okreal8(ct) .and.
     .      array(c12dx) .ne. 0.d0) then
        chantovel = (chan-ct)*array(c12dx) + array(c12x0)
c
c--------------------------
      else
	  chantovel = chan
	  ierr = 1
      endif
c
      return
      end
c
