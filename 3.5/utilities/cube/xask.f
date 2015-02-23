      real function xask (hord, sign)
c     
c     @(#)xask.f	5.1 06/22/94
c
c     Routine for asking for and returning Horizontal positions.
c
c     If HORD = TRUE, then will ask for input in HH MM SS.SS format and
c     input will be multiplied by 15 to return degrees; 
c     If HORD = FALSE, will ask for input in DDD.DDDD format. 
c 
c     If SIGN = TRUE, will ask for an optional preceeding sign.  
c
      logical hord, sign
c
      logical cvrt
      character*13 coord
      real value
      integer ierr
c
5     if (hord) then 
	coord = ' '
	if (sign) then
	   write(6,*) 'SHH MM SS.SS (Enter number using displayed format.)'
      	   read (5,11) coord(1:1),coord(3:13)
11         format(a,a)
	else
	   write(6,*) 'HH MM SS.SS (Enter number using displayed format.)'
      	   read (5,11) coord(3:13)
 	endif
	write(6,*) ' '
	if (.not. cvrt(coord,value) ) then
	   write(6,*) 'Bad input value... Try again'
	   goto 5
	endif
	xask = 15. * value
      else
	if (sign) then
	   write(6,*) 'SDDD.DDDDD (Enter number using displayed format.)'
	   read(5,20,iostat=ierr) value
20	   format(f10.0)
	else
	   write(6,*) 'DDD.DDDDD (Enter number using displayed format.)'
	   read(5,20,iostat=ierr) value
	endif
	write(6,*) ' '
	if (ierr .ne. 0) then
	  write(6,*) 'Bad input value... Try again'
	  goto 5
	endif
	xask = value
      endif
c
      if (value .lt. 0. .and. .not. sign) then
	   write(6,*) 'Negative value not allowed... Try again...'
	   goto 5
      endif
c
      return
c
      end
c
