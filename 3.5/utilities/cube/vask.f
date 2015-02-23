      real function vask (sign)
c     
c     @(#)vask.f	5.1 06/22/94
c
c     Routine for asking for and returning velocities.
c 
c     If SIGN = TRUE, will ask for an optional preceeding sign.  
c
      logical sign
c
      real value
      integer ierr
c     
5     if (sign) then
	write(6,*) 'SVVVVV.VVV km/sec (Enter number using displayed format.)'
        read (5,10,iostat=ierr) value
      else
	write(6,*) 'VVVVV.VVV km/sec (Enter number using displayed format.)'
        read (5,10,iostat=ierr) value
10	format(f10.0)
      endif
      write(6,*) ' '
      if (ierr .ne. 0) then
	write(6,*) 'Bad input value... Try again...'
	goto 5  
      endif
      vask = value
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
