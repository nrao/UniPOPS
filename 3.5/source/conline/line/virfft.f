      subroutine virfft (isgn)
c
c     @(#)virfft.f	5.2 12/15/94
c
c     Performs 2-dim FFT on Matrices 0 and 1 and places result in Matrices 0
c     and 1.  ISGN determines whether the forward (false) or Inverse (true) 
c     transforms are to be used.  
c
      logical isgn
c
      integer*2 i1, i2,  n256, m2
      integer*4 mat0x, mat0y, mat1x, mat1y, ilct, icnt, jcnt
      logical*4 invers
c
      parameter (m2 = -2)
      parameter (n256 = 256)
c
      INCLUDE 'appl.inc'
      INCLUDE 'mappl.inc'
      INCLUDE 'mform.inc'
c
      invers = isgn
c
      mat0x = nint(mhead(mnaxis1,1))
      mat0y = nint(mhead(mnaxis2,1))
      mat1x = nint(mhead(mnaxis1,2))
      mat1y = nint(mhead(mnaxis2,2))
c
      if  (mat0x .le. 0 .or. mat0y .le. 0 .or. mat0x .ne. mat1x .or.
     .     mat0y .ne. mat1y .or. mat0x*mat0y .gt. mdatasize/mnumarrays) 
     .		call oerror(n256, m2, 'MFFT/MIFFT')
c     Make sure the input matrices have been initialized
c
      icnt = 0
16	icnt = icnt + 1
	if (2**icnt .lt. mat0x) goto 16
      jcnt = 0
17	jcnt = jcnt + 1
	if (2**jcnt .lt. mat0y) goto 17
c     ICNT and JCNT will help determine if the matrix sizes are a multiple
c	of two.
c

      do 20 i2 = 1, mat0y
c
	do 10 i1 = 1, mat0x
		xdata (i1) = mdata(ilct(i1,i2,1))
		ydata (i1) = mdata(ilct(i1,i2,2))
10		continue
c
	if (2**icnt .eq. mat0x) then
	   call fft (xdata, ydata, icnt, invers)
	else
	   call dft (xdata, ydata, mat0x, invers)
	endif
c
	do 19 i1 = 1, mat0x
		mdata(ilct(i1,i2,1)) = xdata (i1)
		mdata(ilct(i1,i2,2)) = ydata (i1)
19		continue
c
20	continue
c
c
      do 200 i1 = 1, mat0x
c
	do 100 i2 = 1, mat0y
		xdata (i2) = mdata(ilct(i1,i2,1))
		ydata (i2) = mdata(ilct(i1,i2,2))
100		continue
c
	if (2**jcnt .eq. mat0y) then
	   call fft (xdata, ydata, jcnt, invers)
	else
	   call dft (xdata, ydata, mat0y, invers)
	endif
c
	do 190 i2 = 1, mat0y
		mdata(ilct(i1,i2,1)) = xdata (i2)
		mdata(ilct(i1,i2,2)) = ydata (i2)
190		continue
c
200	continue
c
c
      return
c
      end
c
