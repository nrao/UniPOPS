      subroutine minit(iptwh, reset)
c
c     @(#)minit.f	5.2 05/04/98
c
c     Asks the user for input values for the header of the IPTWH matrix
c     RESET = true if the IPTWH matrix is to be initialized first
c
      integer*2 iptwh
      integer*2 m2, n112, n1900, n6
      logical reset
c
      include 'cio.inc'
      include 'mappl.inc'
      include 'mform.inc'
c
      integer*2 i,lastblnk,ierr, nx, ny, j1
      integer*4 n0, n24, n80, nchars, datestyle
      character*80 input, fmts, fmtf, fmtd1, fmtd2
      character*24 sdate
      real*8 ddate
      real value
c
      data m2, n6, n24, n80, n112, n1900 / -2, 6, 24, 80, 112, 1900/
      data n0 /0/
c
      if (iptwh .lt. 1 .or. iptwh .gt. mnumarrays) 
     .		call oerror(n112, m2, 'MINIT')
c     Check that the user has specified a legitimate matrix
c
      write(istdout,*) ' '
      if (reset) then
	call mraz(iptwh)
	fmtf = '(1x,a,'' [default='',1pg15.6,''] -> '',$)'
	fmts = '(1x,a,'' [default='',a,''] -> '',$)'
        fmtd1= '(1x,a)'
        fmtd2= '('' [default='',a,''] -> '',$)'
	write(istdout,*) '[Enter carriage returns to get default values]'
      else
	fmtf = '(1x,a,'' [current value='',1pg15.6,''] -> '',$)'
	fmts = '(1x,a,'' [current value='',a,''] -> '',$)'
        fmtd1= '(1x,a)'
        fmtd2= ''' [current value='',a,''] -> '',$)'
	write(istdout,*) '[Enter carriage returns to get current values]'
      endif
      write(istdout,*) ' '
c
1	write(istdout,fmtf) 'Enter number of x-axis pixels', mhead(mnaxis1,iptwh)
	read(istdin,991) input
991     format(a)
        if (lastblnk(input) .gt. 0) then
	  read(input,*,iostat=ierr) nx
	  if (ierr .gt. 0 .or. nx .lt. 1) then
	     write(istdout,*) 'Bad entry... Try again...'
	     goto 1
	  else
	     mhead(mnaxis1,iptwh) = nx
	  endif
	endif
2	write(istdout,fmtf) 'Enter number of y-axis pixels', mhead(mnaxis2,iptwh)
	read(istdin,991) input
        if (lastblnk(input) .gt. 0) then
          read(input,*,iostat=ierr) ny
	  if (ierr .gt. 0 .or. ny .lt. 1) then
	     write(istdout,*) 'Bad entry... Try again...'
	     goto 2
	  else
	     mhead(mnaxis2,iptwh) = ny
	  endif
	endif
	if (mhead(mnaxis1,iptwh)*mhead(mnaxis2,iptwh) 
     .	        .gt. mdatasize/mnumarrays ) then
	   write(istdout,*) 'Too many pixels... Try again...'
	   goto 1
        endif
        write(istdout,*) ' '
c     That's it for MNAXIS1 and MNAXIS2
c
      write(istdout,*) 'What is the coordinate system of the x-axis?',
     .		       ' (e.g., VELO, RA, GLONG, TIME, etc.)'
      write(istdout,fmts) 'Enter SYSTEM (8 chars max)', 
     .				cmhead(mtype1,iptwh)
      read(istdin,991) input
      if (lastblnk(input) .gt. 0) cmhead(mtype1,iptwh) = input
      write(istdout,*) 'What is the coordinate system of the y-axis?',
     .		       ' (e.g., VELO, DEC, GLAT, TIME, etc.)'
      write(istdout,fmts) 'Enter SYSTEM (8 chars max)',
     .				cmhead(mtype2,iptwh)
      read(istdin,991) input
      if (lastblnk(input).gt. 0) cmhead(mtype2,iptwh) = input
      write(istdout,*) ' '
c     Takes care of CTYPE1  and CTYPE2
c
      write(istdout,*) 'Enter an x and y reference pixel and the',
     .		      ' values of the x and y axis at these'
      write(istdout,*) 'pixels.  The specified pixel need not be in',
     .		        ' the matrix nor need it be an'
      write(istdout,*) 'integer.'
      write(istdout,*) ' '
c
5       write(istdout,fmtf) 'Enter x-axis ref. pixel number',
     .				mhead(mpix1,iptwh)
        read(istdin,991) input
        if (lastblnk(input) .gt. 0) then
	  read(input,*,iostat=ierr) value
          if (ierr .ne. 0) then
	    write(istdout,*) 'Bad entry... Try again...'
	    goto 5
	  else
	    mhead(mpix1,iptwh) = value
          endif
        endif
6       write(istdout,fmtf) 'Enter y-axis ref. pixel number', 
     .				mhead(mpix2,iptwh)
        read(istdin,991) input
        if (lastblnk(input) .gt. 0) then 
	  read(input,*,iostat=ierr) value
          if (ierr .ne. 0) then
	    write(istdout,*) 'Bad entry... Try again...'
	    goto 6
	  else
	    mhead(mpix2,iptwh) = value
          endif
        endif
        write(istdout,*) ' '
c       Takes care of MPIX1 and MPIX2
c
7       write(istdout,103) 'What is the x-axis value at pixel ',
     .			mhead(mpix1,iptwh), '?'
103	format(1x,a,1pg12.4,a)
        write(istdout,fmtf) 'Enter x-axis value', mhead(mrval1,iptwh)
        read(istdin,991) input
        if (lastblnk(input) .gt. 0) then
	  read(input,*,iostat=ierr) value
          if (ierr .ne. 0) then
	    write(istdout,*) 'Bad entry... Try again...'
	    goto 7
	  else
	    mhead(mrval1,iptwh) = value
          endif
        endif
8       write(istdout,103) 'What is the y-axis value at pixel ',
     .			mhead(mpix2,iptwh), '?'
        write(istdout,fmtf) 'Enter y-axis value', mhead(mrval2,iptwh)
        read(istdin,991) input
        if (lastblnk(input) .gt. 0) then
	  read(input,*,iostat=ierr) value
          if (ierr .ne. 0) then
	    write(istdout,*) 'Bad entry... Try again...'
	    goto 8
	  else
	    mhead(mrval2,iptwh) = value
          endif
        endif
        write(istdout,*) ' '
c       Takes care of MRVAL1 and MRVAL2
c
9        write(istdout,*) 'What is the resolution of x and y axis ',
     .			' (i.e., distance between pixels)?'
        write(istdout,fmtf) 'Enter x-axis resolution', 
     .			mhead(mdelt1,iptwh)
        read(istdin,991) input
        if (lastblnk(input) .gt. 0) then
	  read(input,*,iostat=ierr) value
          if (ierr .ne. 0) then
	    write(istdout,*) 'Bad entry... Try again...'
	    goto 9
	  else
	    mhead(mdelt1,iptwh) = value
          endif
        endif
10      write(istdout,fmtf) 'Enter y-axis resolution',
     .			 mhead(mdelt2,iptwh)
        read(istdin,991) input
        if (lastblnk(input) .gt. 0) then 
	  read(input,*,iostat=ierr) value
          if (ierr .ne. 0) then
	    write(istdout,*) 'Bad entry... Try again...'
	    goto 10
	  else
	    mhead(mdelt2,iptwh) = value
          endif
        endif
        write(istdout,*) ' '
c       Takes care of MDELT1 and MDELT2
      if (cmhead(mtype1,iptwh)(1:4) .eq. 'RA--' .or.
     .    cmhead(mtype2,iptwh)(1:4) .eq. 'RA--' .or.
     .    cmhead(mtype1,iptwh)(1:4) .eq. 'HA--' .or.
     .    cmhead(mtype2,iptwh)(1:4) .eq. 'HA--' .or.
     .    cmhead(mtype1,iptwh)(1:4) .eq. 'DEC-' .or.
     .    cmhead(mtype2,iptwh)(1:4) .eq. 'DEC-') then
         if (reset .or. mhead(mequinox,iptwh) .eq. 0) 
     .           mhead(mequinox,iptwh) = 1950.0
 11      write(istdout,fmtf) 
     .       'Enter the EQUINOX of the coordinate system', 
     .        mhead(mequinox,iptwh)
         read(istdin,991) input
         if (lastblnk(input) .gt. 0) then
            read(input,*,iostat=ierr) value
            if (ierr .ne. 0) then
               write(istdout,*) 'Bad entry... Try again...'
               goto 11
            else
               mhead(mequinox,iptwh) = value
            endif
         endif
         write(istdout,*) ' '
      endif
c			Takes care of EQUINOX for standard RA-- DEC-- coords
      write(istdout,*)
c
      write(istdout,*) 'What is the OBJECT name that describes the',
     .			' data?'
      write(istdout,fmts) 'Enter name (16 char max)',
     .		cmhead(mobject,iptwh) // cmhead(mobject+1,iptwh)
      read(istdin,991) input
      if (lastblnk(input) .gt. 0) then
         cmhead(mobject,iptwh) = input(1:8)
         cmhead(mobject+1,iptwh) = input(9:16)
      endif
      write(istdout,*) ' '
c     Takes care of OBJECT
c
12    write(istdout,*) 'What DATE describes the data?'
      call fromdate(n24, nchars, mhead(mdate,iptwh), sdate)
      if (datestyle(n0) .eq. 1) then
         write(istdout,fmtd1) 
     .        'Enter date (YYYY-MM-DD[Thh:mm:ss[.sss]] format)'
         write(istdout,fmtd2) sdate(1:lastblnk(sdate))
      else
         write(istdout, fmts)
     .        'Enter date (DD/MM/YY format)', sdate(1:lastblnk(sdate))
      endif
      read(istdin,991) input
      if (lastblnk(input) .gt. 0) then
         call todate(n24, input, ddate, n0)
         if (ddate .lt. 0.0) then
            write(istdout,*) 'Bad entry... Try again...'
            goto 12
         endif
         mhead(mdate,iptwh) = ddate
      endif
      write(istdout,*) ' '
c     Takes care of DATE
c
      write(istdout,*) 'What PLACE OF ORIGIN describes the data?'
      write(istdout,fmts) 'Enter ORIGIN (8 chars max)', 
     .				cmhead(morigin,iptwh)
      read(istdin,991) input
      if (lastblnk(input) .gt. 0) cmhead(morigin,iptwh) = input
      write(istdout,*) ' '
c     Takes care of ORIGIN
c
      write(istdout,*) 'What UNITS are the data in? (M/SEC, K, JY/PIX,',
     .		       ' DEGREES,... -- use SI units)'
      write(istdout,fmts) 'Enter UNITS (8 chars max)', 
     .			cmhead(mbunit,iptwh)
      read(istdin,991) input
      if (lastblnk(input) .gt. 0) cmhead(mbunit,iptwh) = input
      write(istdout,*) ' '
c     Takes care of UNITS
c
c       MBLANK is always NaN, there is no reason to set it here
c
      write(istdout,*) 'Enter any comment you may want to add to',
     .			' the header (56 chars max)'
      
      if (reset) then
	write(istdout,*) 'Default = blanks'
      else
	write(istdout,*) 'Current value = ', 
     .      (cmhead(i,iptwh),i=mcomment,min(mheadsize,mcomment+7))
      endif
      read (istdin,991) input
      if (lastblnk(input) .gt. 0) then
        do 100 i = mcomment, min(mheadsize,mcomment+7)
	  j1 = 8*(i-mcomment) + 1
	  cmhead(i,iptwh) = input(j1:j1+8)
100       continue
      endif
      write(istdout,*) ' '
c     And that's the comment
c
      return
c
      end
c
c
