      logical function cvrt(cstring,value)
c
c     @(#)cvrt.f	5.1 06/22/94
c
c     Converts a character string which contains coordinates in the 
c     format SDD MM SS.SS to a fractional degree (DD.ddddd).
c     If the decimal point is not implicately typed in the seconds portion of 
c     the input field, it is assumed to lie between the second and third 
c     character in that field (i.e., '1234' and '12.34' are equivalent; 
c     ' 123' and '1.23' are equivalent).
c
c     CVRT is set to false if there is an error in CSTRING
c
c     cstring = (c*13) Character string to be converted
c
      character*13 cstring
      real value, sgn
c
      logical lperiod
      character*13 string
      integer i, idd, imm
      real ss
c
      string = cstring
      cvrt = .false.
c
      if (string(5:5) .ne. ' ' .or. string(8:8) .ne. ' ') then
	write(6,*) 'Bad field in input line'
	return
      endif
c
      if (string(1:1) .ne. ' ' .and. string(1:1) .ne. '+' .and.
     1    string(1:1) .ne. '-') then
	write(6,*) 'First character must be blank, +, or -'
	return
      endif
c
c
      lperiod = .true.
c
      do 100 i = 2, 13
c
       if( string(i:i) .eq. ' ') string(i:i) = '0'
	if ( llt(string(i:i),'0') .or. lgt(string(i:i),'9') ) then
	   if(i .ge. 9 .and. string(i:i) .eq. '.' .and. lperiod) then
		lperiod = .false.
	   else
		write(6,*) 'Bad character in input line=> ',string(i:i)
		return
	   endif
	endif
c
100	continue
c
      sgn = +1.
      if (string(1:1) .eq. '-') sgn = -1.
c
      read(unit=string,fmt=110) idd,imm,ss
110   format(1x,i3,1x,i2,1x,f5.3)
c
c***      write(6,*) idd, imm, ss
c
      if (imm .gt. 60 .or. ss .gt. 60.) then
	write(6,*) 'Either minutes or seconds are > 60'
	return
      endif
c
c
      value = sgn*(float(idd) + (float(imm) + ss/60.)/60.)
      cvrt = .true.
c
      return
c
      end
c
