      subroutine fft(re,im,ln,invers)
C-------------------------------------------------------------------------------
C  @(#)fft.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c  Fast Fourier Transform for array of n = 2**ln complex values.
c   Normalized by n**(-1/2) so that forward followed by inverse
c   transform yields original array.
c
c  External variables
c
      include 'params.inc'
      real re(*),im(*)
      integer*4 ln
      logical*4 invers
c
      include 'cio.inc'
c
c  Parameter settings
c
      real*8 pi
      parameter (pi=3.1415926536)
c
c  Internal variables
c
      complex*16 data(MAX_DATA_POINTS)
      integer*4 n,nv2,nm1
      complex*16 u,w,keep
      integer*4 l,le,le2
      integer*4 i,j,k,ip
      real rootn
c
      integer*2 n120, m3
      data n120, m3 /120, -3/
c
      n=2**ln
      if (n.gt.MAX_DATA_POINTS) call oerror(n120, m3, 'FFT')
      nv2=n/2
      nm1=n-1
      j=1
c
c  Load internal buffers
c
      do 100 i = 1, n
       data(i)=dcmplx(re(i),im(i))
100    continue
c
c  Apply bit-reversal
c
      do 200 i = 1, nm1
       if (i.lt.j) then
        keep=data(j)
        data(j)=data(i)
        data(i)=keep
       endif
       k=nv2
10     if (k.lt.j) then
        j=j-k
        k=k/2
        goto 10
       endif
       j=j+k
200    continue
c
c  Apply in-place FFT
c
      do 300 l = 1, ln 
       le=2**l
       le2=le/2
       u=dcmplx(1.0,0.0)
       w=dcmplx(dcos(pi/le2),-dsin(pi/le2))
       if (invers) w=dconjg(w)
       do 500 j = 1, le2
        do 400 i = j, n, le
         ip=i+le2
         keep=data(ip)*u
         data(ip)=data(i)-keep
         data(i)=data(i)+keep
400      continue
        u=u*w
500     continue
300   continue
c
c  Unload the internal buffers and normalize
c
      rootn=sqrt(float(n))
      do 600 i = 1, n
       re(i)=dreal(data(i))/rootn
       im(i)=sngl(dimag(data(i)))/rootn
600    continue
c
      return
      end
