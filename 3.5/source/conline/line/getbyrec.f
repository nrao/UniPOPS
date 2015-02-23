      subroutine getbyrec(array, scan, record, phase, imagic, iunit, 
     .			  filename, ierr)
C-------------------------------------------------------------------------------
C  @(#)getbyrec.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Retrieves the array ARRAY from the ILOC location in file FILENAME
c     which is attached to IO unit IUNIT.  
c
c     All of the work is done in getarec.
c     The filename is ignored.
c
      include 'params.inc'
c
      integer*4 lunit, lrtn, ph, i, n2, nrecs, maxsize
      integer*2 phase, record, fshort
c
      parameter (maxsize = HDU_FLOAT_SIZE)
      character*(*) filename
      character*80 stch
      double precision array(*), sig(maxsize), ref(maxsize),
     .		xdata(maxsize), fqsw, bmsw
      integer*2 iunit, ierr, imagic, ier, totrecs, istart, istop, j
      integer*2 n80, n120, n360, n0, m3, recmin, recmax
      real scan, location, rsig(maxsize*2), rref(maxsize*2), data(1024),
     .     weight, rweight, ffactor, totwght, effint 
c
      equivalence (rsig, sig), (rref, ref)
c
      include 'cform.inc'
c
      data n80, n120, m3, n2, n0, n360 /80, 120, -3, 2, 0, 360/
      data bmsw/"LINEBMSW"/, fqsw/"LINEFQSW"/
c
      ierr = 0
      lunit = iunit
      location = scan

      if (record .eq. 0) then
	recmin = 1
	recmax = 32767
      else
	recmin = record
	recmax = record
      endif
      if (phase .eq. 0) then
	ph = 1
      else
	ph = phase
      endif
      totrecs = 0
c
      do 100 i = recmin, recmax
c
         call getarec(lunit, location, ph, i, xdata, maxsize, lrtn)
         if (lrtn .ne. 0) goto 99
         call raz(sig)
         call conversion2(xdata, sig, ier)
         if (ier .ne. 0) then
		ierr = 360
		goto 999
	 endif
c
	 if (i . eq. recmin) then
	    call copy2(maxsize, sig, array)
	    istart = sig(c12spn) + idatoff
	    istop = istart + sig(c12ni) - 1
	    nrecs = nint(sig(c12it) / sig(c3srt))
	    if (i .eq. 1 .and. nrecs .ne. 1) call oerror(n360, n0, 
     .			"Some records are possibly missing in file")
	    effint = sig(c12eit) / nrecs
	    ffactor = sig(c12rms)*sqrt(effint)/sig(c12sst)
      	    totwght = 0
      	    rweight = 0
	    totrecs = 1
	    array(c12it) = sig(c3srt)
	    array(c12eit) = effint
	    do 30 j = 1, istop - istart + 1
		data(j) = 0.0
30		continue
	 else
	    totrecs = totrecs + 1
	    array(c12it) = array(c12it) + sig(c3srt)
      	    array(c12eit) = array(c12eit) + effint
	 endif
c
	 if (phase .eq. 0 .and. (sig(c1stc) .eq. fqsw .or.
     .				 sig(c1stc) .eq. bmsw) ) then
            call getarec(lunit, location, n2, i, xdata, maxsize, lrtn)
            if (lrtn .ne. 0) goto 991
            call raz(ref)
            call conversion2(xdata, ref, ier)
            if (ier .ne. 0) then
		ierr = 360
		goto 999
	    endif
	    weight = effint /
     .		 ( (sig(c12sst)**2 + ref(c12rst)**2) /2.)
	    totwght = totwght + weight
	    rweight = rweight + effint / (ref(c12rst)**2)
	    do 50 j = istart, istop
		data(j-istart+1) = data(j-istart+1) + weight*ref(c12sst)*
     .				(rsig(j)-rref(j))/rref(j)
50		continue
c
	  else
	    if (phase .eq. 2) then
	       weight = effint / (sig(c12rst)**2)
	    else 
	       weight = effint / (sig(c12sst)**2)
	    endif
	    totwght = totwght + weight
	    rweight = rweight + effint / (sig(c12rst)**2)
	    do 60 j = istart, istop
		data(j-istart+1) = data(j-istart+1) + weight*rsig(j)
60		continue
	  endif
c
100	  continue
	
99    if (lrtn .ne. 0. .and. (lrtn .ne. 4 .or. totrecs .eq. 0)) goto 991
      imagic = 2
      do 200 j = 1, istop - istart + 1
	data(j) = data(j) / totwght
200	continue
      call copy(fshort(2*(istop-istart+1)), data, array((istart+1)/2) )
      array(c12sst) = sqrt(array(c12eit)/totwght)
      array(c12rst) = sqrt(array(c12eit)/rweight)
      array(c12rms) = ffactor*array(c12sst)/sqrt(array(c12eit))
      array(c1recid) = record
      array(c3phsid) = phase
      array(c1norec) = totrecs
      if (phase .ge. 1 .and.  (sig(c1stc) .eq. fqsw .or.
     .				 sig(c1stc) .eq. bmsw) ) then
	    array(c12it) = array(c12it) * 4.
	    array(c12eit) = array(c12eit) * 4.
      endif
c
999   return
c
991   imagic = -1
      if (lrtn .eq. 1) then
            ierr = -370
      else if (lrtn .eq. 2) then
            ierr = -357
      else if (lrtn .eq. 3) then
            ierr = -359
      else if (lrtn .eq. 4) then
            ierr = 362
      else if (lrtn .eq. 5) then
            ierr = 371
      else
            write(stch, 20) lrtn
 20   	    format('Unknown disk error code ', i)
            call oerror(n120, m3, stch)
      endif
      return
c
      end
c
