      subroutine tekcursor(irtn, ix, iy)
C-------------------------------------------------------------------------------
C  @(#)tekcursor.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Returns cursor position in TEKTRONIX
c*************************************************************************
c 8904 [RJM]
c*************************************************************************
c
      include 'cio.inc'
c
      real xscale, yscale
      integer*2 iix, iiy
c			make all TEK-like devices 15/16th full size 15/16 = 0.9375
      parameter (yscale=0.9375)
      parameter (xscale=0.9375)
c
      integer*2 icross, icrhex, irtn, ix, iy
      integer*2 n2, n6
      character*1 cciout(6)
      integer*4 long
c
      data icrhex /6938/
      data n2, n6 /2, 6/
c     icrhex = 1b1a = ESC + SUB
c
      if (irtn .eq. 1) then
        write(istdout,*) 'Hit space bar for coordinates; ',
     1	            'Anything else to exit'
	call updateline(' ')
      else if(irtn.eq.0) then
	write(istdout,*) 'Hit space bar for coordinates'
	call updateline(' ')
      endif
      call flush(long(istdout))
      icross = icrhex
      call tekchar(icross, n2)
      call rdrm(istdin, n6, cciout)
      call upack(iix, iiy, irtn, cciout)
c			scale the numbers back
      ix = nint(float(iix) / xscale)
      iy = nint(float(iiy) / yscale)
c
      if (irtn .eq. 32) then
        irtn = -1
      else
        irtn = -2
      endif
c
      return
c
      end
c
