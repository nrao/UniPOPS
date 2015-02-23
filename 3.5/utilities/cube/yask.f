      real function yask (hord, sign)
c     
c     @(#)yask.f	5.1 06/22/94
c
c     Routine for asking for and returning Vertical positions.
c
c     If HORD = TRUE, then will ask for input in DD MM SS.SS format 
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
	   write(6,*) 'SDD MM SS.SS (Enter number using displayed format.)'
      	   read (5,11) coord(1:1),coord(3:13)
11         format(a,a)
	else
	   write(6,*) 'DD MM SS.SS (Enter number using displayed format.)'
      	   read (5,11) coord(3:13)
 	endif
	write(6,*) ' '
	if (.not. cvrt(coord,value) ) then
	   write(6,*) 'Bad input value... Try again'
	   goto 5
	endif
	yask = value
      else
	if (sign) then
	   write(6,*) 'SDD.DDDDD (Enter number using displayed format.)'
	   read(5,20,iostat=ierr) value
20	   format(f10.0)
	else
	   write(6,*) 'DD.DDDDD (Enter number using displayed format.)'
	   read(5,20,iostat=ierr) value
	endif
	write(6,*) ' '
	if (ierr .ne. 0) then
	  write(6,*) 'Bad input value... Try again'
	  goto 5
	endif
	yask = value
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
