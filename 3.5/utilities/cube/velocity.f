      subroutine velocity 
c
c     @(#)velocity.f	5.1 06/22/94
c
c     Reads in the next cube record which is to be processed (if it is
c     not already in memory).  Goes through a scan and sees what channels
c     fall within the limits of the cube and if they are within the
c     user specified error of a cube location.  Writes out that record
c     when the scan is completed.
c
      integer ib, ioldrec, lmin, lmax, kmin, kmax, irecmin, irecmax, 
     .		idigitmax,istart, istop, i1a, i2a, i2, k, i1,
     .		l, idigitmin, idir, itmp
      real cvel, dvel, cc, vlowest, vhighest, v, vk, v1, v2
      logical incube
c
      include 'cube.inc'
      include 'dform.inc'
c
      save ioldrec
c
      data ioldrec/0/
c     record number of last read record from cube file
c
      lmin = 32767
      lmax = -32767
      kmin = 32767
      kmax = -32767
      irecmin = 32767
      irecmax = -32767
      idigitmin = 32767
      idigitmax = -32767
c     Useful for debugging purposes
c
      istart = dtwh (c12spn) 
      istop = istart + dtwh(c12ni) - 1
      cvel = dtwh (c12x0)
      dvel = dtwh (c12dx)
      cc = dtwh(c12rp)
      v1 = v0 + dvc*(1. - vp)
      v2 = v0 + dvc*(float(nv) - vp)
      vlowest = min(v1,v2) - errv
      vhighest = max(v1,v2) + errv
c
c     Calculates start/stop channel numbers, center velocity, velocity
c     width and center channel.  Calculates
c     the minimum/maximum velocities which can go into the cube.
c
      i1a = nint ( (vlowest-cvel) / dvel + cc)
      i2a = nint ( (vhighest-cvel) / dvel + cc)
      i1 = max(min(i1a, i2a) - 1, istart)
      i2 = min(max(i1a, i2a) + 1, istop)
      idir = 1
      if (dvel * dvc .lt. 0) then
         idir = -1
         itmp = i1
         i1 = i2
         i2 = itmp
      endif
      if (debug) write(6,*) 'Looking at channels : ',i1,' to ',i2,
     .                     ' idir ', idir
c     Calculates the range of channels numbers which should be looked at.
c     The +/- 1 are to make sure that some edge channels aren't skipped
c     Make sure we step through the vector in the same direction that
c     the FITS cube is ordered.
c
      if (irec .ne. ioldrec) read(ioutdev,rec=irec,err=500) lout
      ioldrec = irec
c     Reads in a record only if desired one is not already in memory
c
      do 200 k = i1, i2, idir
c
         v = (float(k) - cc) * dvel + cvel
         l = nint( (v - v0)/dvc + vp)
         vk = v0 + (float (l)-vp)*dvc
	 incube = v .ge. vlowest .and. v .le. vhighest .and. 
     .		  abs( vk - v ) .lt. errv .and. l .gt. 0 .and. l .le. nv
c
c        K = channel number of interest
c        V = velocity of center of channel K
c	 L = closest output bin number to velocity V
c	 VK = velocity of center of output bin L
c	 IDIGIT = location-1 in LOUT of output record corresponding to 
c		VLOWEST. 
c        INCUBE = true if the channel K falls within errv of a pixel in the cube 
c
110	 if (l+idigit .lt. 1 .and. k .ne. i1 .and. incube) then
		write(6,*) 'Somethings wrong... in VELOCITY',
     .				       l,idigit,k,v,vk,irec
		goto 200
	 else if (l + idigit. gt. numdata) then
   		write(ioutdev,rec=irec,err=600) lout
		idigit = idigit - numdata
		irec = irec + 1
 		read(ioutdev,rec=irec,err=500) lout
		ioldrec = irec
	        goto 110
c		Scan stared in one record and extends into another; write
c		first rec. out and read in second.  
c
	 endif
c
         if ( incube ) then
c
            ib = nint( 2.*float(irange) * (twh(k + idatoff) - tmin)
     1		     / (tmax-tmin) )
            if (ib .lt. 0 .or. ib .gt. 2*irange) then
		lout(l+idigit) = ibad
	    else
                lout(l+idigit) = ib - irange
	    endif
c	    Input channel falss within the cube pixel; set output buffer to
c	    scaled data value in that channel.
c
	    kmin = min(kmin, k)
	    kmax = max(kmax, k)
	    lmin = min(lmin, l)
	    lmax = max(lmax, l)
	    irecmin = min(irecmin, irec)
	    irecmax = max(irecmax, irec)
	    idigitmin = min(idigitmin, idigit) 
	    idigitmax = max(idigitmax, idigit)
c	    Update debugging variables
c
         endif
c
200   continue
c
      write(ioutdev,rec=irec,err=600) lout
c     Scan is done so write buffer out just in case program dies elswhere.
c
      if (debug) then
	write(6,*) 'Max/Min k, l, irec, idigit ',  kmin, kmax, 
     .		   lmin, lmax, irecmin, irecmax, idigitmin, idigitmax
      endif
c
      if (kmin .eq. 32767) write(6,*) 'NO channels added...',
     .				      '  Wrong velocities'
	   
      return
c
500   write(0,*) 'Error in reading from cube file'
      return
c
600   write(0,*) 'Error in writing to cube file'
      return
c
      end
c
