      subroutine dft(re,im,n,invers)
C-------------------------------------------------------------------------------
C  @(#)dft.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c  Fourier Transforms an array of arbitrary length n.
c   Normalizes by n**(-1/2) so that forward followed by inverse
c   transform yields original array
c
c  External variables
c
      real re(*),im(*)
      integer*4 n
      integer*2 n120, m3
      logical*4 invers
c
      include 'cio.inc'
      include 'params.inc'
c
c  Internal variables
c
      integer*4 j,k,indx
      real*8 clut(0:MAX_DATA_POINTS),slut(0:MAX_DATA_POINTS)
      real*8 zr(MAX_DATA_POINTS),zi(MAX_DATA_POINTS)
      real*8 dang,ang,div
c
      data n120, m3 /120, -3/
c
      if (n.gt.MAX_DATA_POINTS) call oerror(n120, m3, 'DFT')
c
      dang=2.0d0*3.1415926536d0/dble(n)
      do 100 k = 1, n
       zr(k)=0.d0
       zi(k)=0.d0
       ang=dang*dble(k)
       clut(k)=dcos(ang)
       slut(k)=dsin(ang)
100    continue
c
      if (invers) then
      do 200 k = 1, n
        slut(k)=-slut(k)
200     continue
      endif
c
      clut(0)=clut(n)
      slut(0)=slut(n)
c
      do 400 k = 1, n
       do 300 j = 1, n
        indx=mod(j*k,n)
        zr(k)=zr(k)+dble(re(j))*clut(indx)+dble(im(j))*slut(indx)
        zi(k)=zi(k)+dble(im(j))*clut(indx)-dble(re(j))*slut(indx)
300     continue
400   continue
c
      div=dsqrt(dble(n))
      do 500 k = 1, n
       re(k)=sngl(zr(k)/div)
       im(k)=sngl(zi(k)/div)
 500   continue
c
      return
      end
