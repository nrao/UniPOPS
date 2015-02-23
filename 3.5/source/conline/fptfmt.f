      logical function fptfmt(infmt)
c
c     @(#)fptfmt.f	5.1 06/22/94
c
c     Checks that the input format specification (INFMT) can be used
c     as a default format specification.
c
      character*(*) infmt
c
      integer*2 i0, numchar, inum, i1, i2, fdefault, nfield, n373, m1,
     .		ier, i3, lastblnk
      character*1 lc1, c1, c2
      character*6 snum
c
      parameter (fdefault = 15)
      parameter (n373 = 373)
      parameter (m1 = -1)
c
      fptfmt = .false.
      i0 = 1
      numchar = lastblnk(infmt)
      inum = 0
c
10    if (i0 .gt. numchar) then
	call oerror(n373, m1, 'Format exhausted')
      else
        lc1 = infmt(i0:i0)
	call uppercase(lc1, c1)
      endif
c------
      if (c1 .eq. ' ' .or. c1 .eq. ',') then
	i0 = i0 + 1
	goto 10
c	Skip all blanks and commas between fields
c
c------
      else if (lle(c1,'9') .and. lge(c1,'0') .or. c1 .eq. '-' ) then
c	We have a number begining a field; parse the number until you get
c	to a non-number and then set up INUM.  Probably a
c	repeat count or a scale factor for 'P'.
c
	i1 = i0
	i2 = i0 + 1
20	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 20
	  endif
	else
	  call oerror(n373, m1, 'Format exhausted')
        endif
	if (c1 .eq. '-' .and. i1+1 .eq. i2) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
c	Must have a digit after the minus sign.
c
	read(infmt(i1:i2-1),'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i0 = i2
	goto 10
c
c------
      else if (c1 .eq. 'P') then
c	We have a P scale factor
c
	write(snum,'(i6)',iostat=ier) inum
	if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	inum = 0
	i0 = i0 + 1
	goto 10
c
c------
      else if (c1 .eq. 'S') then
c	We have a S scale factor
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Extra numerical field in format')
	i2 = i0 + 1
	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if (c2 .eq. 'P' .or. c2 .eq. 'p' .or.
     .	      c2 .eq. 'S' .or. c2 .eq. 's') then
	     i2 = i2 + 1
	  endif
        endif
	i0 = i2
	goto 10
c
c------
      else if (c1 .eq. 'X') then
c	We have an 'X' format.  If no repeat count, assume 1 and then
c	add blanks to the output string.
c
	if (inum .lt. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	inum = 0
	i0 = i0 + 1
	goto 10
c
c------
      else if (c1 .eq. 'F' .or. c1 .eq. 'G' .or. c1 .eq. 'E' .or. 
     .	       c1 .eq. 'D') then	
c	We have an 'E, F, or G' format.  
c
	if (inum .ne. 0) 
     .		call oerror(n373, m1, 'Bad numerical field in format')
	i1 = i0 + 1
	i2 = i1
60	if (i2 .le. numchar) then
	  c2 = infmt(i2:i2)
	  if ( lle(c2,'9') .and. lge(c2,'0') ) then
	     i2 = i2 + 1
	     goto 60
	  else if (c2 .eq. ' ' .and. i2 .eq. i1) then
	     i1 = i1 + 1
	     i2 = i1
	     goto 60
	  endif
        endif
c	Parse field width
c
	if (i2 .eq. i1) then
	   nfield = fdefault
	   i3 = i2
c	   Set default field width
	else 
	   read(infmt(i1:i2-1),'(i6)',iostat=ier) nfield
	   if (ier .ne. 0) 
     .		call oerror(n373, m1, 'Bad number field in format')
	   if (c2 .ne. '.') then
	      i3 = i2
	   else
	      i3 = i2 + 1
62	      if (i3 .le. numchar) then
	  	c2 = infmt(i3:i3)
	  	if ( (lle(c2,'9') .and. lge(c2,'0')) .or.
     .		     c2 .eq. '.'  .or. c2 .eq. 'e' .or. c2 .eq. 'E') then
	     	  i3 = i3 + 1
	     	  goto 62
	  	endif
	      endif
	      if (i3 .eq. i2 + 1) 
     .		call oerror(n373, m1, 'Bad format specification')
	   endif
	endif
c	Parse field width and # sig. digits.
c  
	i0 = i3
	inum = 0
        if (i3 .le. numchar) 
     .		call oerror(n373, m1, 'Bad format specification')
      else
	call oerror(n373, m1, 'Bad format specification')
      endif
c
      fptfmt = .true.
c
      return
      end
c
