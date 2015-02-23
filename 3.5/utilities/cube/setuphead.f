      subroutine setuphead(objectname)
c
c     @(#)setuphead.f	5.2 05/04/98
c
c     Asks the user for values in order tp setup the FITS cube header
c
      integer iarray(3),ilen,iobj,istart,istop,i,id,im,lnblnk,
     .	      hostnm, ierr, irtn, isys1
      character*1 answr
      character*(*) objectname
      character*16 object1
      character*24 date1, date2
      character*8 hostname
      character*72 input
      real tmin1, tmax1, sec, aref, xask, yask, vask
      logical lchkdate, hourdeg, inuse
      
      include 'cube.inc'
      include 'dform.inc'
c
c
      ilen = max(16,lnblnk(objectname))
      object1 = objectname(ilen-15:ilen)
19    ilen = index(object1,'/')
      if (ilen .ne. 0) then
	object1 = object1(ilen+1:)
	goto 19
      endif
c     Packs into OBJECT1 the last sixteen chars in objectname.  Watch out for
c	slashes (i.e., directories).
c
1 	write(6,*) 'What OBJECT name do you want in the CUBE header?'
	write(6,*) '1: ', object1   
	write(6,*) '2: ', ctwh(c1ona) // ctwh(c1ona+1)
	write(6,*) '3: Other'
	write(6,101) 'Enter 1, 2, or 3'
101     format(1x,a,' -> ',$)
	read(5,991) input
991     format(a)
	read(input,*,iostat=ierr) iobj
	write(6,*) ' '
	if (ierr .gt. 0) then
	   write(6,*) 'Bad choice... Try again...'
	   goto 1
	endif
	if (iobj .eq. 1) then
	    object(1) = object1(1:8)
            object(2) = object1(9:16)
	else if (iobj .eq. 2) then
	    object(1) = ctwh(c1ona)
	    object(2) = ctwh(c1ona+1)
	else if (iobj. eq. 3) then
	    write(6,101) 'Enter OBJECT (16 chars. max.)'
	    read(5,991) object1
	    write(6,*) ' '
            object(1) = object1(1:8)
            object(2) = object1(9:16)
	else
	    write(6,*) 'Bad choice... Try again...'
	    goto 1
	endif
c	Takes care of OBJECT
c
      call idate( iarray)
      call makedate(24, date1, iarray)
      iarray(3) = int(dtwh(c3dat))
      iarray(2) = 100.d00*mod(dtwh(c3dat),1.d00)
      iarray(1) = 100.d00*mod(dtwh(c3dat)*100.,1.d00)
      call makedate(24, date2, iarray)
c     Gets the system date and that from the present scan
c
2	write(6,*) 'What DATE do you want in the header?'
	write(6,*) '1: ', date1
	write(6,*) '2: ', date2
	write(6,*) '3: Other'
	write(6,101) 'Enter 1, 2, or 3'
	read(5,991) input
	read(input,*,iostat=ierr) iobj
	write(6,*) ' '
	if (ierr .gt. 0) then
	   write(6,*) 'Bad choice... Try again...'
	   goto 2
	endif
	if (iobj .eq. 1) then
	    date = date1
	else if (iobj .eq. 2) then
	    date = date2
	else if (iobj .eq. 3) then
	    write(6,101) 'Enter date in the format of YYYY-MM-DD'
	    read(5,102,iostat=ierr) iarray(3), iarray(2), iarray(1)
102	    format(i4,1x,i2,1x,i2)
	     write(6,*) ' '
	    if (ierr .ne. 0 .or. 
     .		.not. lchkdate(iarray(3),iarray(2), iarray(1))) then
		write(6,*) 'Bad entry... Try again...'
		goto 2
	    endif
            call makedate(24, date, iarray)
	else
	    write(6,*) 'Bad choice... Try again...'
	    goto 2
	endif
c	Takes care of DATE
c
      ibad = -(irange + 1)
c     BAD values are assigned the value of the negative range of data
c     representation.
c
      irtn = hostnm(hostname)
3	write(6,*) 'What PLACE OF ORIGIN do you want in the header?'
	write(6,*) '1: ', hostname
	write(6,*) '2: ',ctwh(c1tel)
	write(6,*) '3: Other'
	write(6,101) 'Enter 1, 2, or 3'
	read(5,991) input
	read(input,*,iostat=ierr) iobj
	write(6,*) ' '
	if (ierr .gt. 0) then
	   write(6,*) 'Bad choice... Try again...'
	   goto 3
	endif
	if (iobj .eq. 1) then
	    origin = hostname
	else if (iobj .eq. 2) then
	    origin = ctwh(c1tel)
	else if (iobj. eq. 3) then
	    write(6,101) 'Enter ORIGIN (8 chars. max)'
	    read(5,991) origin
	    write(6,*) ' '
	else
	    write(6,*) 'Bad choice... Try again...'
	    goto 3
	endif
c	Takes care of ORIGIN
c
4	write(6,*) 'What UNITS are the data in the SDD file?'
	write(6,*) '1: K'
	write(6,*) '2: Jy'
	write(6,*) '3: Other'
	write(6,101) 'Enter 1, 2, or 3'
	read(5,991) input
	read(input,*,iostat=ierr) iobj
	write(6,*) ' '
	if (ierr .gt. 0) then
	   write(6,*) 'Bad choice... Try again...'
	   goto 4
	endif
	if (iobj .eq. 1) then
	    units = 'K'
	else if (iobj .eq. 2) then
	    units = 'JY'
	else if (iobj. eq. 3) then
	    write(6,101) 'Enter UNITS (8 chars. max.)'
	    read(5,991) units
	    write(6,*) ' '
	else
	    write(6,*) 'Bad choice... Try again...'
	    goto 4
	endif
c	Takes care of UNITS
c

      istart = dtwh(c12spn)+idatoff      
      istop = istart + dtwh(c12ni) - 1
      tmin1 = 1.e30
      tmax1 = -1.e30
      do 6 i = istart, istop
 	   tmin1 = min(tmin1, twh(i))
	   tmax1 = max(tmax1, twh(i))
6	   continue
c     Finds guesses for TMIN and TMAX
c
7       write(6,*) 'The first scan has min. and max. data values of',
     .			tmin1, tmax1,'.'
701	write(6,101) 'What min. value do you expect to find in the data?'
	read(5,991) input
        read(input,*,iostat=ierr) tmin
	write(6,*) ' '
	if (ierr .gt. 0) then
  	  write(6,*) 'Bad entry.... Try again...'
	  go to 701
        endif
702	write(6,101) 'What max. value do you expect to find in the data?'
	read(5,991) input
        read(input,*,iostat=ierr) tmax
	write(6,*) ' '
	if (ierr .gt. 0) then
  	  write(6,*) 'Bad entry.... Try again...'
	  go to 702
        endif
        if (tmin .ge. tmax) then
  	  write(6,*) 'Bad entry.... Try again...'
	  go to 7
        endif
c
      tscale = (tmax-tmin) / float(2*irange)
      tzero = tmin + float(irange)*tscale
c     Calculates the MAX/MIN/SCALE factors for the data to be stored.
c
      if (ctwh(c4csc).eq.'EPOCRADC' .or. 
     .	    ctwh(c4csc).eq.'GALACTIC' .or.
     .	    ctwh(c4csc).eq.'AZEL    ' .or. 
     .	    ctwh(c4csc).eq.'INDICATD' .or.
     .      ctwh(c4csc).eq.'        ' ) then
	    inuse = .true.
      else
	    inuse = .false.
      endif
c     INUSE will be true if the coordinate system that the the observation
c     was taken in corresponds to those always existing in the header.
c
8       write(6,*) 'What coordinate system do you want to use?'
        if (.not.inuse) write(6,*) '1: ', ctwh(c4csc)
	write(6,*) '2: EPOCRADC'
	write(6,*) '3: GALACTIC'
        write(6,*) '4: AZEL'
        write(6,*) '5: INDICATD'
        write(6,*) '6: Other'
	if (inuse) then
	  write(6,101) 'Enter 2 - 6'
	else
	  write(6,101) 'Enter 1 - 6'
	endif
	read(5,991) input
        read(input,*,iostat=ierr) isys1
	write(6,*) ' '
	if (ierr .gt. 0) then
	    write(6,*) 'Bad choice... Try again...'
	    goto 8
	endif
c
	if (isys1 .eq. 1 .and. .not. inuse) then
	   do 61 i = 1, 14
		if (ctwh(c4csc) .eq. testcoord(i)) then
		   isys = i
		   goto 69
		endif
61		continue
	   write(6,*) 'Apparently the header in the scan has an improper',
     .		      ' coordinate code. Pick another system...'
	   goto 8
c
	else if (isys1 .gt. 1 .and. isys1 .lt. 6) then
	    isys = isys1 + 13
	else if (isys1 .eq. 6) then
	    write(6,*) 'Program will pick out only those observations',
     .		       ' which were made in one of the'
	    write(6,*) ' systems listed below.  Any observation not made',
     .		       ' in the given system will be ignored.'
	    write(6,*) ' '
	    write(6,*) ' Which of the following systems do you want to use?'
	    write(6,64) (i, ':', testcoord(i),i=1,14)
64	    format(1x,i2, 1x, a1, 1x, a8)
	    write(6,101) 'Enter 1 - 14'
	    read(5,991) input
            read(input,*,iostat=ierr) isys
            write(6,*) ' '
	    if (isys .lt. 1 .or. isys .gt. 14 .or. ierr .ne. 0) then
		write(6,*) 'Bad choice... Try again...'
		goto 8
	    endif
	else
	    write(6,*) 'Bad choice... Try again...'
	    goto 8
	endif
c
69    if (isys .le. 14) then
	  appepch = testcoord(isys)
      else
	  appepch = ' '
      endif
c     Assigns value to APPEPCH header word for storing more descriptive
c     name for coord system
c
      coorda = caxis(1, isys)
      coordd = caxis(2, isys)
      coordv = 'VELO    '
c     Assign values to coordinate codes for header.
c		try and get Equinox right
      if (coordd .eq. 'DEC-') then
         equinox = 1950.0
         if (appepch(1:4) .eq. '2000') then
            equinox = 2000.0
         else
            equinox = dtwh(c4epo)
         endif
      endif
c
20	write(6,101) 'Do you want the map projection to be GLS ' //
     .               '(COSV correction) ?  Answer Y or N'
	read(5,991) answr
	write(6,*) ' '
        if (answr .eq. 'y' .or. answr .eq. 'Y') then
	   cosv = .true.
	else if (answr .eq. 'n' .or. answr  .eq. 'N') then
	   cosv = .false.
	else
	   write(6,*) 'Bad choice... Try again...'
	   goto 20
        endif
c	Set up COSV flag
c
      write(6,*) 'The first scan in the input file has the following',
     .		   ' parameters:'
      if (isys .eq. 15) then
	   call ddtoddmmss(dtwh(c4era)/15.d00, id, im, sec)
	   write(6,71) coorda, ' = ', id, ':', im, ':', sec
71	   format(1x,a,a,i3,a,i2,a,f4.1)
	   call ddtoddmmss(dtwh(c4edc), id, im, sec)
	   write(6,71) coordd, ' = ', id, ':', im, ':', sec
	   hourdeg = .true.
      else if (isys .eq. 16) then
	   write(6,74) coorda, ' = ', dtwh(c4gl)
74	   format(1x,a,a,f10.5)
	   write(6,74) coordd, ' = ', dtwh(c4gb)
	   hourdeg = .false.
      else if (isys .eq. 17) then
	   write(6,74) coorda, ' = ', dtwh(c4az)
	   write(6,74) coordd, ' = ', dtwh(c4el)
	   hourdeg = .false.
      else if (isys .eq. 18) then
	   write(6,74) coorda, ' = ', dtwh(c4ix)
	   write(6,74) coordd, ' = ', dtwh(c4iy)
	   hourdeg = .false.
      else if (isys .le. 7) then
	   call ddtoddmmss(dtwh(c4sx)/15.d00, id, im, sec)
	   write(6,71) coorda, ' = ', id, ':', im, ':', sec
	   call ddtoddmmss(dtwh(c4sy), id, im, sec)
	   write(6,71) coordd, ' = ', id, ':', im, ':', sec
	   hourdeg = .true.
      else
	   write(6,74) coorda, ' = ', dtwh(c4sx)
	   write(6,74) coordd, ' = ', dtwh(c4sy)
	   hourdeg = .false.
      endif
      write(6,72) 'Velocity in channel one = ', 
     .			dtwh(c12x0)+dtwh(c12dx)*(1.-dtwh(c12rp))
72    format(1x,a,1pg16.7)
      write(6,72) 'Delta velocity = ', dtwh(c12dx)
      write(6,*) 'Number of channels = ', nint(dtwh(c12ni))
      write(6,*) ' '
c     Prints out info from scan in memory that may help observer choose some
c     cube parameters.
c     HOURDEG = true if the coordinate system uses hours for the x-axis.
c
c
      write(6,*) 'Enter the corner position in ',coorda(1:4),
     1		   ', ', coordd(1:4),', and ',coordv(1:4),' for the CUBE.'
      if (cosv) write(6,*) 'COSV ON: Position should be specified as',
     .		       ' if the data were taken at equator.'
      a0 = xask(hourdeg,.false.)
      d0 = yask(hourdeg,.true.)
      v0 = vask(.true.)
c
21      write(6,*) 'Enter step size in ',coorda(1:4),', ',coordd(1:4),
     .		   ', and ',coordv(1:4),'.'
	if (cosv) then
	    write(6,*) 'COSV ON: Step should be specified as if the data',
     .		       ' were taken at equator.'
	endif
	dac = xask(hourdeg,.true.)
	ddc = yask(hourdeg,.true.)
	dvc = vask(.true.)
	if (dac .eq. 0. .or. ddc .eq. 0. .or. dvc .eq. 0) then
	  write(6,*) 'Bad step sizes... Try again...'
	  goto 21
        endif
c
      write(6,*) 'Enter errors in ',coorda(1:4),', ',coordd(1:4),
     .		   ', and ',coordv(1:4),'.  Any data point which is'
      write(6,*) 'further than these errors from a position',
     .	 	   ' of the center of a pixel in the CUBE '
      write(6,*) 'will NOT be used.  (Must be > 0 ; if >',
     .		  ' 1/2, then 1/2 step size will be used.'
c
      erra = xask(hourdeg,.false.)
      errd = yask(hourdeg,.false.)
      errv = vask(.false.)
      if (2*erra .gt. abs(dac) .or. 2*errd .gt. abs(ddc) .or. 
     .	    2*errv .gt. abs(dvc)) then
            write(6,*) 'Error is greater than 1/2 stepsize... Setting',
     .                 ' the error to default of'
	    write(6,*) '1/2 the step size. ' 
	    write(6,*) ' '
      endif     
      if (2*erra .gt. abs(dac)) erra = abs(dac)/2.
      if (2*errd .gt. abs(ddc)) errd = abs(ddc)/2.
      if (2*errv .gt. abs(dvc)) errv = abs(dvc)/2.
c  
30      write(6,*) 'Enter number of points in ',coorda(1:4),', ',
     .		   coordd(1:4),', and ',coordv(1:4),' directions (>= 1)'
301	write(6,101) 'Enter number of ' // coorda(1:4) // ' pixels'
	read(5,991) input
        read(input,*,iostat=ierr) na
        write(6,*) ' '
	if (ierr .gt. 0) then
	   write(6,*) 'Bad entry... Try again...'
	   goto 301
	endif
302	write(6,101) 'Enter number of ' // coordd(1:4) // ' pixels'
	read(5,991) input
        read(input,*,iostat=ierr) nd
        write(6,*) ' '
	if (ierr .gt. 0) then
	   write(6,*) 'Bad entry... Try again...'
	   goto 302
	endif
303	write(6,101) 'Enter number of ' // coordv(1:4) // ' pixels'
	read(5,991) input
        read(input,*,iostat=ierr) nv
        write(6,*) ' '
	if (ierr .gt. 0) then
	   write(6,*) 'Bad entry... Try again...'
	   goto 303
	endif
	if (na .lt. 1 .or. nd .lt. 1 .or. nv .lt. 1) then
	   write(6,*) 'Bad number of points... Try again...'
	   goto 30
        endif
	if (na*nd*nv .gt. 47.e6 ) then
	   write(6,*) 'Too many points (47 x 10^6 max)... Try again...'
	   goto 30
        endif
c
      if (cosv) then
	   write(6,*) 'COSV ON: Enter Reference position in ', 
     .			coorda(1:4),' for COS V affect.'
	   aref = xask(hourdeg,.false.)
	   ap = (aref-a0)/dac + 1
	   a0 = aref
c			Also set d ref to be 0 for GLS projection
           dp = -d0/ddc + 1
           d0 = 0.0
c
	   coorda(5:8) = '-GLS'
	   coordd(5:8) = '-GLS'
c	   Get reference coordinate of map.  Update coordinate codes
c
      else
	   ap = 1.
           dp = 1.
      endif
      vp = 1.
c     Also, assign values to reference pixels.
c
      write(6,*) 'Enter the 3 comments you may want to add to the',
     1             ' header in the cube file (60 x 3  characters)'
      write(6,*) '                                                  ',
     1             '           |'
      read (5,991) label(1)
      write(6,*) '                                                  ',
     1             '           |'
      read (5,991) label(2)
      write(6,*) '                                                  ',
     1             '           |'
      read (5,991) label(3)
      write(6,*) ' '
c
      return
c
      end
c
c
