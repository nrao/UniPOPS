      subroutine corecursor(irtn, ix, iy)
C-------------------------------------------------------------------------------
C  @(#)corecursor.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Returns cursor position via graphics Fifos
c*************************************************************************
c 8904 [RJM]
c*************************************************************************
c
c
      include 'cio.inc'
c
      integer*4 long, irtn4
      character*80 stch 
      integer*2 irtn, ix, iy, ier
c
      if (irtn .eq. 1) then
         write(istdout,*) 'Hit Button 3 to get coordinates; ',
     1		'Button 1 to exit cursor mode'
      else if(irtn.eq.0) then
	 write(istdout,*) 'Hit Button 1 or 3 to get coordinates'
      endif
      call flush(long(istdout))
c
      stch = ' '
      call coreflush
      call writefifo(long(igraphout), irtn4, 'cursor')
10    call readfifo(long(igraphin), irtn4, stch)
c
      if (stch(1:4) .eq. 'done') goto 10
c     Watch out for a 'lingering' done from the graphics process.
c
      read(stch,20,iostat=ier) irtn, ix, iy 
20    format(3i5)
c
      return
      end
c
