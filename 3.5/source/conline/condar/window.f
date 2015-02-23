      subroutine window (iw)
c
c     @(#)window.f	5.1 06/22/94
c
c     2-Dimensional windowing subroutine (holography package)
c       iw     = window number:
c                  1 = rectangular
c                  2 = bartlett
c                  3 = hanning
c                  4 = hamming
c                  5 = blackman
c                (formulas for these functions may be obtained from
c                "Digital Signal Processing", by Oppenheim & Shafer)
c
c
c     User defined functions:
c
c       fnbar(u,v)    = iw .eq. 2.  Function for the 2 dimensional Bartlett
c                       window.  u is the radius and v is the total diameter
c       fnhan(u,v)    = iw .eq. 3.  Function for the 2 dimensional Hanning
c                       window.  u and v are defined in fnbar
c       fnham(u,v)    = iw .eq. 4.  Function for the 2 dimensional Hamming
c                       window.  u and v are defined in fnbar
c       fnbla(u,v)    = iw .eq. 5.  Function for the 2 dimensional Blackman
c                       window.  u and v are defined in fnbar
c
c
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c
c
      integer*2 iw
c
      real fnbar, fnhan, fnham, fnbla, pi, u, v, rho, dia, fn, xc, yc
      integer*2 i, j, n120, m2, mat0x, mat0y, mat1x, mat1y, n256
      integer*4 ilct
c
      parameter (pi=3.1415927)
      parameter (m2 = -2)
      parameter (n256 = 256)
      parameter (n120 = 120)
c
      include 'mappl.inc'
      include 'mform.inc'
c
      fnbar(u,v)=2.0*(u-1.0)/(v-1.0)
      fnhan(u,v)=.5*(1.0-cos(pi*fnbar(u,v)))
      fnham(u,v)=.54-(.46*cos(pi*fnbar(u,v)))
      fnbla(u,v)=.42-(.5*cos(pi*fnbar(u,v))) + 
     1                (.08*cos(2.*pi*fnbar(u,v)))
c
      mat0x = nint(mhead(mnaxis1,1))
      mat0y = nint(mhead(mnaxis2,1))
      mat1x = nint(mhead(mnaxis1,2))
      mat1y = nint(mhead(mnaxis2,2))
c
      if  (mat0x .le. 0 .or. mat0y .le. 0 .or. mat0x .ne. mat1x .or.
     .     mat0y .ne. mat1y .or. mat0x*mat0y .gt. mdatasize/mnumarrays) 
     .		call oerror(n256, m2, 'HOLWINDOW')
c     Make sure the input matrices have been initialized
c
      if (iw .lt. 1 .or. iw .gt. 5) call oerror(n120, m2, 'HOLWINDOW')
c
      if (iw .eq. 1) return
c
      xc = float(mat0x)/2 + 0.5
      yc = float(mat0y)/2 + 0.5
      dia = mat0x
c
      do 20 i=1, mat0x
          do 10 j=1, mat0y
c
            rho = xc-sqrt((i-xc)**2+((j-yc)*mat0x/mat0y)**2 )/sqrt(2.)
c
            if (iw .eq. 2) then
		fn = fnbar (rho, dia)
	    else if (iw .eq. 3) then
		fn = fnhan (rho, dia)
	    else if (iw .eq. 4) then
		fn = fnham (rho, dia)
	    else if (iw .eq. 5) then
		fn = fnbla (rho, dia)
	    else
		call oerror(n120, m2, 'HOLWINDOW')
	    endif
c
	    mdata(ilct(i,j,1)) = mdata(ilct(i,j,1))*fn
	    mdata(ilct(i,j,2)) = mdata(ilct(i,j,2))*fn
c
   10       continue
20       continue
c
      return
      end
c
