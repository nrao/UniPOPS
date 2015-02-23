      subroutine craz
c
c     @(#)craz.f	5.3 05/04/98
c
c     Initializes the cube header
c
      integer*4 i, i12, iarray(3), n0
      real*4 rmaxnorm, rmaxnormal
      real*8 dinfinity
      character*12 date1
c
      include 'mappl.inc'
      include 'mform.inc'
c
      data n0, i12 /0, 12/
c
      rmaxnorm = rmaxnormal()
      call idate( iarray)
      call makedate(i12, date1, iarray)
c     Gets the system date and the value of infinity
c
      do 10 i = 1, mstrings - 1
	chead(i) = rmaxnorm
10	continue
      do 20 i = cstrings, cheadsize
	cchead(i) = '  '
20	continue
c     Initialize real and strings in cube header
c
c     The following sets up default header values
c
      chead(cblank) = dinfinity()
      chead(cnaxis1) = 1.0
      chead(cnaxis2) = 1.0
      chead(cnaxis3) = 1.0
      chead(cpix1) = 1.0
      chead(cpix2) = 1.0
      chead(cpix3) = 1.0
      chead(crval1) = 1.0
      chead(crval2) = 1.0
      chead(crval3) = 1.0
      chead(cdelt1) = 1.0
      chead(cdelt2) = 1.0
      chead(cdelt3) = 1.0
      chead(cequinox) = 0.0
      chead(cbitpix) = 16.0
      cchead(ctype1) = 'PIXEL'
      cchead(ctype2) = 'PIXEL'
      cchead(ctype3) = 'PIXEL'
      call todate(i12, date1, chead(cdate), n0)
c
      return
      end
c
c
