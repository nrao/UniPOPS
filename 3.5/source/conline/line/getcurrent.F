      subroutine getcurrent(ifeed, iptwh, ier2)
C-------------------------------------------------------------------------------
C  @(#)getcurrent.F	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Gets the mostcurrent scan and places it in TWH(1,iptwh)
c
      include 'appl.inc'
      integer*4 irtn, if4, deffeed
      integer*2 ifeed, iptwh, ier2
      real*8 xtemp(HDU_FLOAT_SIZE/2)
c
#ifdef CONTINUUM
      integer*4 iactual, if5
#else
#endif
c
      include 'cio.inc'
c
      if4 = ifeed
      ier2 = 0
      if(if4.eq.0 .or. if4.gt.99) if4 = deffeed()
c
      if (online) then
#ifdef CONTINUUM
	iactual = MAX_DATA_POINTS
	if (if4 .eq. 99) then
	   if5 = -1
	else if (if4 .eq. 98) then
	   if5 = 0
	else
	   if5 = if4
	endif
	call glcont(if5, iactual, MAX_DATA_POINTS, xtemp, irtn)
#else
	call gcscan(xtemp, if4, irtn)
#endif
        if (irtn .ne. 0) then
	  ier2 = 361
        else
	  call raz(dtwh(1,iptwh))
          call conversion2(xtemp, dtwh(1,iptwh), ier2)
          if (ier2 .ne. 0) ier2 = 360
        endif
      else
	ier2 = 361
      endif
c
      return
      end
c
