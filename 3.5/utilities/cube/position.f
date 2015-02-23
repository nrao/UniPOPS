      logical function position ()
c
c     @(#)position.f	5.2 10/14/94
c
c     Gets coordinates of the present scan.  Correctly handles going through
c     360 deg in the X directions.  Calculates where in the cube the position
c     would fall.  If the position is not in the cube or if the distance
c     between the cube position and the actual position is larger than the user
c     supplied errors, that scan is skipped over.  POSITION is false if the
c     scan is to be skipped. 
c
      real rad, xposs, xposc, cosy, yposs, yposc
      integer ixc, iyc, startcube
c
      include 'cube.inc'
      include 'dform.inc'
c
      data rad /57.29577951/
c
      position = .false.
c
      if ( isys .le. 14) then
        xposs = dtwh(c4sx)
        yposs = dtwh(c4sy)
      else if ( isys .eq. 15) then
        xposs = dtwh(c4era)
        yposs = dtwh(c4edc)
      else if ( isys .eq. 16) then
        xposs = dtwh(c4gl)
        yposs = dtwh(c4gb)
      else if ( isys .eq. 17) then
        xposs = dtwh(c4az)
        yposs = dtwh(c4el)
      else if ( isys .eq. 18) then
        xposs = dtwh(c4ix)
        yposs = dtwh(c4iy)
      endif
c     Gets the desired X and Y coordinates 
c
      cosy = 1.
      if (cosv) cosy = cos(yposs/rad)
      xposc = cosy*(xposs - a0)/dac + ap
      if (nint(xposc) .lt. 1) xposc = xposc + abs(360.*cosy/dac)
      if (nint(xposc) .gt. na) xposc = xposc - abs(360.*cosy/dac)
      ixc = nint ( xposc)
c     Turns X position into PIXEL values; corrects, if desired, for COSV.
c     Watches out for going thru 360 degrees.
c
      yposc = (yposs-d0)/ddc + dp
      iyc = nint ( yposc)
c     Turns Y position into PIXEL values.
c
      if (debug) write(6,*) 'Pos:',xposs,yposs,' Cube:',xposc,yposc,
     .				ixc,iyc
c
      if (ixc.gt.na .or. iyc.gt.nd .or. ixc.lt.1 .or. iyc.lt.1) then
        write (6,*) ' Position is NOT in the data cube... Skipping...'
c
      else
c
	if ( abs((xposc - float(ixc))*dac) .gt. erra .or. 
     1       abs((yposc - float(iyc))*ddc) .gt. errd )  then
          write(6,*) 'Error in position is too large... Skipping....'
	  if (debug) write(6,*) abs((xposc - float(ixc))*dac), erra,
     1			        abs((yposc - float(iyc))*ddc), errd

c
        else 
c
          if (afirst) then
             startcube = nv*(na*(iyc -1) + ixc -1)
          else
             startcube = nv*(nd*(ixc -1) + iyc -1)
          endif
c
          irec = startcube/numdata + numhead + 1
          idigit = mod (startcube,numdata)
c	  STARTCUBE = pixel -1 where scan will start in cube
c	  IREC = record number corresponding to that pixel
c	  IDIGIT = location - 1 in that record where scan is to start
c 
          if (irec.le.numhead .or. irec .gt. maxrec .or.
     1        idigit.lt.0 .or. idigit.ge.numdata) then
		write(0,*) 'POSITION: INTERNAL ERROR -- Continuing'
	  else
                position = .true.
	  endif
c
          if (debug) write(6,*) 'Output Record no. ',irec,
     .			         ' Location within record ', idigit
c
        endif
      endif
c
      return
c
      end
c
