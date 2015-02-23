      subroutine memusage
c
c     @(#)memusage.f	5.1 06/22/94
c
c     Reports on CORE usage.
c
      include 'core.inc'
      include 'lsf.inc'
      include 'cio.inc'
c
      integer*2 iprog, isource, ivar, ier
      integer*2 n80
      data n80 / 80/
c
      write (outbuf, 4998)
      call pwrite(ioutbuf, n80)
      write (outbuf, 4999)
      call pwrite(ioutbuf, n80)
      write (outbuf,5000,IOSTAT=IER) K(3),K(5),Kx(3),Kx(5),LISTF(3),
     .                          LISTF(5)
      call pwrite(ioutbuf, n80)
c
      iprog = nint(100.*float(k(3))/float(k(5)))
      ivar = nint(100.*float(kx(3))/float(kx(5)))
      isource = nint(100.*float(listf(3))/float(listf(5)))
      write(outbuf,5001,iostat=ier) iprog, ivar, isource
      call pwrite(ioutbuf, n80)
c
      if (iprog .ge. 90) then
	write(outbuf,5002, iostat=ier)  'WARNING:  Program storage',
     .					' >= 90 full'
        call pwrite(ioutbuf, n80)
      endif
      if (ivar .ge. 90) then
	write(outbuf,5002, iostat=ier)  'WARNING:  Variable storage',
     .					' >= 90 full'
        call pwrite(ioutbuf, n80)
      endif
      if (isource .ge. 90) then
	write(outbuf,5002, iostat=ier)  'WARNING:  Procedure source',
     .					' code storage >= 90 full'
        call pwrite(ioutbuf, n80)
      endif
      if (iprog .ge. 90 .or. ivar .ge. 90 .or. isource .ge. 90) then
	write(outbuf,5002, iostat=ier)  '(Use COMPRESS to regain some',
     .				 	' storage space.)'
        call pwrite(ioutbuf, n80)
      endif
c
 4998 format(8X,'SPACE USED/AVAILABLE FOR:')
 4999 format('     PROGRAMS   VARIABLES     SOURCE')
 5000 format(1X,3(x,i5,'/',i5))
5001  format(2x,'(',i5,'%',6x,i5,'%',6x,i5,'%)')
5002  format(5a)
c
      return
      end
