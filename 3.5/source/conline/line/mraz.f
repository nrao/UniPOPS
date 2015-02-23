      subroutine mraz(iptwh)
c
c     @(#)mraz.f	5.3 05/04/98
c
c     Initializes the IPTWH matrix
c
      integer*2 iptwh
      integer*2 m2, n112, i1
      integer*4 l1, long, l0
      integer*4 i, n1, n2, n3, iarray(3), i12
      real*4 rmaxnorm, rmaxnormal
      real*8 dinfinity
      character*12 date1
c
      include 'mappl.inc'
      include 'mform.inc'
c
      data m2, i1, i12, n112 /-2, 1, 12, 112/
      data l0, l1 /0, 1/
c
      if (iptwh .lt. 1 .or. iptwh .gt. mnumarrays) 
     .		call oerror(n112, m2, ' ')
c     Check that the user has specified a legitimate matrix
c
      rmaxnorm = rmaxnormal()
      call idate( iarray)
      call makedate(i12, date1, iarray)
c     Gets the system date and the value of infinity
c
      do 10 i = 1, mstrings - 1
	mhead(i,iptwh) = rmaxnorm
10	continue
      do 20 i = mstrings, mheadsize
	cmhead(i,iptwh) = '  '
20	continue
c     Initialize real and strings in matrix header
c
c     The following sets up default header values
c
      mhead(mblank,iptwh) = dinfinity()
      mhead(mnaxis1,iptwh) = 1.0
      mhead(mnaxis2,iptwh) = 1.0
      mhead(mpix1,iptwh) = 1.0
      mhead(mpix2,iptwh) = 1.0
      mhead(mrval1,iptwh) = 1.0
      mhead(mrval2,iptwh) = 1.0
      mhead(mdelt1,iptwh) = 1.0
      mhead(mdelt2,iptwh) = 1.0
      mhead(mequinox,iptwh) = 0.0
      mhead(mbitpix,iptwh) = 16.0
      cmhead(mtype1,iptwh) = 'PIXEL'
      cmhead(mtype2,iptwh) = 'PIXEL'
      call todate(i12, date1, mhead(mdate, iptwh), l0)
c
      n1 = mdatasize/mnumarrays
      n2 = long(iptwh-i1)*n1 + l1
      n3 = n2 + n1 - l1
      do 30 i = n2, n3
	mdata(i) = mhead(mblank,iptwh)
30	continue
c     Initialize data section for IPTWH matrix
c
      return
      end
c
c
