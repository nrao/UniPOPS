      logical function toilet2()
c
c     @(#)toilet2.f	5.2 05/04/98
c
c     Reads the header for an existing cube file
c
      character*80 head(36), hmask(36), stch
      character*1 quote 
      character*4 oldyaxis(2)
      logical lend
      integer ierr, j, jmax, i, k, n2, n3, oldfits, nchars, n24, lnblnk
      real*8 ddate
c
      include 'cube.inc'
c
      data quote /''''/
      data oldyaxis /'DEC ','EL  '/
      data n24 /24/
c
      write(6,*) 'Reading in Header record(s) in CUBE file' 
      write(6,*) ' '
      toilet2 = .false.
c
      lend = .false.
      cosv = .false.
      afirst = .true.
      isys = 0
      a0 = 1.
      d0 = 1.
      v0 = 1
      n2 = 0
      n3 = 0
      nv = 0
      dac = 1.
      ddc = 1.
      dvc = 1.
      erra = 1.e30
      errd = 1.e30
      errv = 1.e30
      vp = 1.
      dp = 1.
      ap = 1.
      tzero = 0.
      tscale = 1.
      equinox = 0.0
      coorda = '        '
      coordd = '        '
      coordv = '        '
      appepch = '        '
      label(1) = ' '
      label(2) = ' '
      label(3) = ' '
      date = '        '
      object(1) = '        '
      object(2) = '        '
      origin = '        '
      units = '        '
      tmin = -irange
      tmax = irange
      ibad = -irange-1
c     Sets up defaults if the header doesn't contain the necessary
c     information.  No gaurentees if one of the above is not in the header
c
      open(unit=iheadmask,file=headerfile,
     1     access='sequential',form='formatted',status='old',
     2     iostat=ierr)
c
      if (ierr .ne. 0) then
	write(0,*) 'Major error!  Template header-file not found...'
	goto 99
      endif
c
      read(iheadmask,10,iostat=ierr) hmask
10    format(a80)
      close (iheadmask,iostat=ierr)
c
      if (ierr .ne. 0) then
	write(0,*) 'Major error!  Template header-file cannot be read...'
	goto 99
      endif
c     Reads in FITS header template.  Program must know of any changes to
c     template.
c
      close (iheadmask,iostat=ierr)
c
      irec = 0
c
1       irec = irec + 1
	write(6,*) 'Reading the ', irec, ' header record from cube'
      	read(ioutdev,rec=irec,iostat=ierr) head
	if (ierr .ne. 0) then
	   write(0,*) 'Cannot read cube header...'
	   goto 99
        endif
c
	write(6,*) ' '
 	write(6,*) head
	write(6,*) ' '
c
	if (irec .eq. 1) then
c
	if (head(1)(1:30) .ne. hmask(1)(1:30)) then
	   write(0,*) 'FITS header is not in the required format...'
	   write(0,*) head(1)
	   goto 99
	endif
	if (head(2)(1:30) .ne. hmask(2)(1:30)) then
	   write(0,*) 'FITS header is not in the required format...'
	   write(0,*) head(2)
	   goto 99
	endif
	if (head(3)(1:30) .ne. hmask(3)(1:30)) then
	   write(0,*) 'FITS header is not in the required format...'
	   write(0,*) head(3)
	   goto 99
	endif
c       Checks for SIMPLE, BITPIX, NAXIS=3
c
        if (hmask(4) (1:10) .ne. head(4) (1:10) ) then
	   write(0,*) 'FITS header is not in the required format...'
	   write(0,*) head(4)
	   goto 99
	else
	   read(unit=head(4) (11:30),fmt=20) nv
20    	   format(i20)
	endif
        if (hmask(5) (1:10) .ne. head(5) (1:10) ) then
	   write(0,*) 'FITS header is not in the required format...'
	   write(0,*) head(5)
	   goto 99
	else
	   read(unit=head(5) (11:30),fmt=20) n2
	endif
        if (hmask(6) (1:10) .ne. head(6) (1:10) ) then
	   write(0,*) 'FITS header is not in the required format...'
	   write(0,*) head(6)
	   goto 99
	else
	   read(unit=head(6) (11:30),fmt=20) n3
	endif
c       Checks NAXIS1,NAXIS2,NAXIS3, NAXIS1 is nv, the others depend
c	 and get sorted out later
c
        endif
c
	do 50 j = 36, 1, -1
	   if (hmask(36)(1:10) .eq. head(j)(1:10) ) then
		write(6,*) 'Last header record found'
		numhead = irec
		jmax = j - 1
		lend = .true.
		goto 60
	   endif
50	   continue
        jmax = 36
c       Checks if this is the last header record
c
60      continue
c           try and see if this is an old style unipops header
c           this had the DATE keyword with the order of mm/dd reversed
c           and its signaled by the following heuristic
        oldfits = 0
        if (head(2)(32:32) .eq. '/' .and.
     +      head(2)(53:59) .eq. 'UniPops') then
           oldfits = 1
        endif
c
        do 200 i = 1, 36
	   do 100 j = 1, jmax
	      if (hmask(i) (1:10) .eq. head(j) (1:10) ) then
                if (i .eq. 7) then
		  if (head(j) (12:15) .ne. 'VELO' .and.
     .		      head(j) (12:15) .ne. 'FELO') then
	   	    write(0,*) 'FITS header is not in the required format...',
     .			'  Velocity axis must be first...'
	   	    write(0,*) head(j)
	   	    goto 99
		  else
	   	    coordv = head(j) (12:19)
		  endif
		endif
c		Checks CTYPE1 = VELO or FELO
c
		if (i .eq. 12) then
c			make sure this is an "X" coord
                   afirst = .true.
                   do 210 k = 1, 18
                      if (head(j) (12:15) .eq. caxis(2,k)) 
     .                    afirst = .false.
 210               continue
c			these were some old Y coord labels we used to use
                   do 215 k = 1, 2
                      if (head(j) (12:15) .eq. oldyaxis(k))
     .                    afirst = .false.
 215               continue
                endif
c			after i = 12, it is assumed that afirst is set correctly
                if (i .ge. 12 .and. i .le. 21) then
                   if (afirst) then
                      if (i .eq. 12) coorda = head(j) (12:19)
      		      if (i .eq. 17) coordd = head(j) (12:19) 
      		      if (i .eq. 13) 
     .                      read(unit=head(j) (11:30),fmt=40) a0
      		      if (i .eq. 14) 
     .                      read(unit=head(j) (11:30),fmt=40) dac
		      if (i .eq. 15) 
     .                      read(unit=head(j) (11:30),fmt=40) ap
		      if (i .eq. 16) 
     .                      read(unit=head(j) (11:30),fmt=40) erra
      		      if (i .eq. 18) 
     .                      read(unit=head(j) (11:30),fmt=40) d0
      		      if (i .eq. 19) 
     .                      read(unit=head(j) (11:30),fmt=40) ddc
		      if (i .eq. 20) 
     .                      read(unit=head(j) (11:30),fmt=40) dp
		      if (i .eq. 21) 
     .                      read(unit=head(j) (11:30),fmt=40) errd
                   else
                      if (i .eq. 12) coordd = head(j) (12:19)
      		      if (i .eq. 17) coorda = head(j) (12:19) 
      		      if (i .eq. 13) 
     .                      read(unit=head(j) (11:30),fmt=40) d0
      		      if (i .eq. 14) 
     .                      read(unit=head(j) (11:30),fmt=40) ddc
		      if (i .eq. 15) 
     .                      read(unit=head(j) (11:30),fmt=40) dp
		      if (i .eq. 16) 
     .                      read(unit=head(j) (11:30),fmt=40) errd
      		      if (i .eq. 18) 
     .                      read(unit=head(j) (11:30),fmt=40) a0
      		      if (i .eq. 19) 
     .                      read(unit=head(j) (11:30),fmt=40) dac
		      if (i .eq. 20) 
     .                      read(unit=head(j) (11:30),fmt=40) ap
		      if (i .eq. 21) 
     .                      read(unit=head(j) (11:30),fmt=40) erra
                   endif
                endif
c
      		if (i .eq. 22) appepch = head(j) (12:19) 
		if (i .eq. 23) then
                   object(1) = head(j) (12:19) 
                   if (head(j) (20:20) .ne. quote) 
     .                object(2) = head(j) (20:27)
                endif
		if (i .eq. 24) origin = head(j) (12:19) 
		if (i .eq. 25) then
                   call fitskeyvalue(nchars, head(j), stch)
                   call todate(nchars, stch, ddate, oldfits)
                   call fromdate(n24, nchars, ddate, date)
                   if (oldfits) then
c                        we need to rewrite this record to fix the problem
c                        if its oldfits, then it must have 8 chars currently
c                        occupied by the data
c                        The new string will either be 8 chars or 10 chars
c                        depending on whether the y2k format is being used
                       nchars = lnblnk(date)
                       if (nchars .eq. 8) then
c                           a simple overwrite
                          head(j)(12:19) = date(1:8)
                       else if (nchars .eq. 10) then
c                           overwrite
                          head(j)(12:21) = date(1:10)
c                           and put the closing quote in the right place
                          head(j)(22:22) = quote
                       else
c                           this should never happen
                          write(0,*) 'Impossible DATE string found...'
                          write(0,*) 'Please report this error'
                          goto 99
                       endif
c                           now update this record
                       write(ioutdev,rec=irec,iostat=ierr) head
                       if (ierr .ne. 0) then
                          write(0,*) 'Cannot update cube header...'
                          goto 99
                       endif
                   endif
                endif
		if (i .eq. 29) units = head(j) (12:19) 
c
      		if (i.eq.8) read(unit=head(j)(11:30),fmt=40) v0
      		if (i.eq.9) read(unit=head(j)(11:30),fmt=40) dvc
		if (i.eq.10) read(unit=head(j)(11:30),fmt=40) vp
		if (i.eq.11) read(unit=head(j)(11:30),fmt=40) errv
      		if (i .eq. 27) read(unit=head(j)(11:30),fmt=40)tscale
      		if (i .eq. 28) read(unit=head(j) (11:30),fmt=40)tzero
      		if (i .eq. 30) read(unit=head(j) (11:30),fmt=40) tmin
      		if (i .eq. 31) read(unit=head(j) (11:30),fmt=40) tmax
      		if (i .eq. 32) read(unit=head(j) (11:30),fmt=40) equinox
40    		format(1g20.0)
c
      		if (i .eq. 33) label(1) = head(j) (11:70) 
      		if (i .eq. 34) label(2) = head(j) (11:70) 
      		if (i .eq. 35) label(3) = head(j) (11:70)
c
		if (i .eq. 26) read(unit=head(j) (11:30),fmt=20) ibad
c
	      endif
100           continue
200	   continue
c
        if ( .not. lend) goto 1
c
      write(6,*) 'Checking input FITS header parameters'
      write(6,*) ' '
c
      v0 = v0 / 1.e03
      dvc = dvc / 1.e03
      errv = errv / 1.e03
c     Convert from FITS m/sec to internal km/sec.
c		Assign n2, n3 to na, nd as appropriate
      if (afirst) then
         na = n2
         nd = n3
      else
         nd = n2
         na = n3
      endif
c
      if (nv*na*nd .gt. 47.e6 .or. nv*na .lt. 0 .or. na*nd .lt. 0 .or.
     1    nv*na*nd .le. 0) then
	write(0,*) 'Bad NAXIS#:: ',na,nd,nv
	goto 99
      endif
c			set equinox if not already set
      if (coordd(1:4) .eq. 'DEC-' .and. equinox .eq. 0) equinox=1950.0
c
      maxrec = (na*nd*nv) / numdata + numhead
      mleft = mod(na*nd*nv, numdata)
      if (mleft .ne. 0) maxrec = maxrec + 1
c
      if (tscale .eq. 0.) then
	write(0,*) 'Bad BSCALE: ',tscale
	goto 99
      endif
c
      if (a0 .lt. 0. .or. a0 .gt. 360. .or. d0 .lt .-90. .or.
     1    d0 .gt. 90.) write(6,*) 'Possibly bad CRVAL1 : ',a0, d0, v0
c     Corrects the starting coords if the header refeerences it to a
c     new point.
c
      if (erra .lt. 0. .or. errd .lt. 0. .or. errv .lt. 0.) 
     .		write(6,*) 'Possibly bad COMMENT ERR# : ',erra, errd, errv
c
      erra = min(abs(erra), abs(dac) / 2.)
      errd = min(abs(errd), abs(ddc) / 2.)
      errv = min(errv, abs(dvc) / 2.)
c
      if (ibad .ne. -32768) write(6,*) 'Possibly bad BLANK : ',ibad
c
      do 101 isys = 1, 14
	if (appepch .eq. testcoord(isys)) goto 120
101     continue
      do 102 isys = 15, 18
	if (coorda(1:4) .eq. caxis(1,isys) .and. 
     .	    coordd(1:4) .eq. caxis(2,isys)) goto 120
102	continue
c
      write(0,*) 'Bad CTYPE# :', coorda, coordd, appepch
      goto 99
c
120   if (coorda(5:8) .eq. '-GLS') then
	if (coordd(5:8) .ne. '-GLS') then
	   write(0,*) 'Cannot have X and Y in different coordinate systems...'
	   goto 99
	else
	   cosv = .true.
	endif
      endif
c
      tmax = float(irange)*tscale + tzero
      tmin = float(-irange)*tscale + tzero
c     Recalculates tmax and tmin for 16 bit representation.  Ignores Header
c     values.
c
      if (debug) then
      	write(6,*) 'Internal variable values (for debugging only) :'
      	write(6,*) 'a0,d0,v0', a0,d0,v0
      	write(6,*) 'na,nd,nv', na,nd,nv
      	write(6,*) 'dac,ddc,dvc', dac,ddc,dvc
      	write(6,*) 'ap,dp,vp',ap,dp,vp
      	write(6,*) 'erra,errd,errv',erra,errd,errv
      	write(6,*) 'isys,coorda,coordd,coordv', isys,' ',coorda, ' ',
     1	          coordd,' ',coordv
      	write(6,*) 'tzero,tscale', tzero,tscale
      	write(6,*) 'tmin,tmax,ibad',tmin,tmax,ibad
      	write(6,*) 'date,object,appepch ', date,' ',object,' ',appepch
      	write(6,*) 'origin,units,cosv ', origin,' ',units,' ',cosv
        write(6,*) 'equinox ',equinox
      	write(6,*) 'label ',label(1)
      	write(6,*) 'label ',label(2)
      	write(6,*) 'label ',label(3)
      	write(6,*) 'maxrec,mleft,numhead',maxrec,mleft,numhead
        write(6,*) 'afirst ', afirst
	write(6,*) ' '
      endif 
c
      toilet2 = .true.     
c
99    return
c
      end
c
