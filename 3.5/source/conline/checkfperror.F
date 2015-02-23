      subroutine checkfperror
c
c     @(#)checkfperror.F	5.1 06/22/94
c
c     Prints out a list of Floating Point errors which have accrued since
c     the last time routine was called.
c
      include 'errors.inc'
      include 'cio.inc'
c
      character*16 out, mode, in
      integer*4 ieeer, ieee_flags
c
#ifdef sun3
      ieeer = ieee_flags('clear', 'exception', 'inexact', out)
      call cleanup()
      call ieee_retrospective()
      ieeer = ieee_flags('clearall', mode, in, out)
#else
      if (numdivision .gt. 0 .or. numunderflow .gt. 0 .or.
     .    numoverflow .gt. 0 .or. numinvalid .gt. 0 .or. 
     .	  numothererr .gt. 0) then
	write(istderr,*) 'Warning:  the following IEEE floating-point',
     .			' arithmetic'
	write(istderr,*) 'exceptions occurred during execution of the',
     .			' last command:'
	if (numdivision .gt. 0) write(istderr,10) numdivision, 'Division by zero;'
10	format(1x,i4,1x,a,$) 
	if (numunderflow .gt. 0) write(istderr,10) numunderflow, 'Underflow;'
	if (numoverflow .gt. 0) write(istderr,10) numoverflow, 'Overflow;'
	if (numinvalid .gt. 0) write(istderr,10) numinvalid, 'Invalid Op.;'
	if (numothererr .gt. 0) write(istderr,12) numothererr, 
     .		'FPA type:', loclasterr, ';'
12	format(1x,i4,1x,a,i3,a,$) 
	write(istderr,*) ' '
        numdivision = 0
        numunderflow = 0
        numoverflow = 0
        numinvalid = 0
        numothererr = 0
        loclasterr = 0
        ieeer = ieee_flags('clearall', mode, in, out)
      endif
#endif
c
      amath = .false.
      erron = .true.
c
      return
      end
