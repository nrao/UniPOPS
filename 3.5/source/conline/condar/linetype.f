      subroutine linetype(ltype, lindex)
c
c     @(#)linetype.f	5.1 06/22/94
c
c     Changes the line type specified by LTYPE for lines of index
c     LINDEX.  
c
c     LTYPE is specified as a 3 digit number: ADT
c		A = Arrow type
c		D = Dot-Dash type
c		T = Thickness
c
c     If LTYPE = 000, use default line type; up to 9 types of
c	arrows, dot-dash, thickness lines can be supported.  The
c	abs. value of LTYPE is used.
c
c     LINDEX = 33 for PLOT and SHOW types of graphs while it
c     is 1-32 for contour plots.
c
      integer*2 ltype, lindex
      integer*2 m2, n10, n112, n262
      real*4 direc, scl, xold, yold
      integer*2 ix, iy, index
      logical lflip
c
      logical logtick(33), logdash(33)
      integer*2 dash0(33,10),tick0(33,10),noangl0(33),notick0(33),
     .		deltaline(33,10), numline(33), nodash0(33)
      real delta10(33,10),delta20(33,10), sinangle0(33,10),
     .	   cosangle0(33,10)
c
      integer*2 i, ltype2, itick, idash, x3, y3, x4, y4, x5, y5, 
     .		xc, yc, xd, yd, iangle, ix0, iy0, ix1, iy1
      real olddirec, dlength, tlength, theta, cost, sint, dist,
     .	   rtlft, dt, rad, alength, sinta, costa
c
      integer*2 multi, maxmulti, Mdeltaline(5,6), Mnumline(6)
      integer*2 dashi, maxdashi, Mdash0(6,10), Mnumdash(10)
      integer*2 ticki, maxticki, Mtick0(2,10), Mnumtick(10),
     .		Mnoangle(10)
      real angle(2,10),delta1(2,10),delta2(2,10)
c
      save dash0, tick0, sinangle0, delta10, delta20, nodash0, tlength,
     1	   noangl0, logtick, logdash, notick0, dlength,
     2     cosangle0, deltaline, numline, xold, yold
c
      save ix1, iy1, ix0, iy0, lflip, itick, idash, olddirec,
     .	   rtlft, dt 
c
      data logdash/33*.false./, logtick/33*.false./, lflip /.true./,
     .     numline/33*1/, deltaline/330*0/, rad /57.29577951/
c
      data maxmulti/6/,Mnumline/1,1,2,3,4,5/
      data Mdeltaline/5*0,
     .		      5*0,
     .		      0,1,3*0,
     .		      0,1,-1,2*0,
     .		      0,1,-1,2,0,
     .		      0,1,-1,2,-2/
c     Specifications for the multiple lines to draw of each type
c
      data maxdashi/10/,Mnumdash/1,2,2,2,4,6,2,2,2,4/
      data Mdash0/6*0,
     .		 2*60,4*0,
     .		 2*30,4*0,
     .		 40,20,4*0,
     .		 35,5,5,5,2*0,
     .		 35,5,5,5,5,5,
     .		 5,5,4*0,
     .		 5,25,4*0,
     .		 5,55,4*0,
     .		 1,21,80,21,2*0/
c     Specifications for the dot-dashed lines to draw of each type
c
      data maxticki/10/,Mnumtick/1,1,1,1,1,1,1,1,2,1/
      data Mtick0/2*0,
     .		  60,0,
     .		  30,0,
     .		  60,0,
     .		  30,0,
     .		  60,0,
     .		  30,0,
     .		  60,0,
     .		  50,10,
     .		  123,0/
      data Mnoangle/0,1,1,1,1,2,2,2,2,2/
      data angle /2*0.,
     .		  -90.,0.,
     .		  -90.,0.,
     .		  90.,0.,
     .		  90.,0.,
     .		  -135.,-225.,
     .		  -135.,-225.,
     .		  -45.,-315.,
     .		  -135.,-225.,
     .		  45.,135./
      data delta1/2*0.,
     .		  2*0.,
     .		  2*0.,
     .		  2*0.,
     .		  2*0.,
     .		  2*0.,
     .		  2*0.,
     .		  2*0.,
     .		  2*0.,
     .		  -6.,-6./
      data delta2/2*0.,
     .		  20.,0.,
     .		  10.,0.,
     .		  20.,0.,
     .		  10.,0.,
     .		  20.,20.,
     .		  10.,10.,
     .		  10.,10.,
     .		  10.,10.,
     .		  14.1,14.1/
c
      data m2, n10, n112, n262 /-2, 10, 112, 262/
c     Specifications for the tick lines to draw of each type
c
      ltype2 = abs(ltype)
c
      if (ltype .gt. 999) call oerror(n262, m2, '')
c
      multi = mod(ltype2,n10)+1
      if (multi .gt. maxmulti) 
     .		call oerror(n112,m2,'Line Thickness specification')
      numline(lindex) = Mnumline(multi)
      do 10 i = 1, numline(lindex)
	deltaline(lindex,i) = Mdeltaline(i,multi)
10	continue
c
      dashi = mod(ltype2,100)/10 + 1
      if (dashi .gt. maxdashi) 
     .		call oerror(n262,m2,'Dot-Dash specification')
      if (dashi .gt. 1) then
	logdash(lindex) = .true.
	nodash0(lindex) = Mnumdash(dashi)
	do 20 i = 1, nodash0(lindex)
	   dash0(lindex,i) = Mdash0(i,dashi)
20	   continue
      else
	logdash(lindex) = .false.
      endif
c
      ticki = mod(ltype2,1000)/100 + 1
      if (ticki .gt. maxticki) 
     .		call oerror(n262,m2,'Tick specification')
      if (ticki .gt. 1) then
	logtick(lindex) = .true.
	notick0(lindex) = Mnumtick(ticki)
	do 30 i = 1, notick0(lindex)
	   tick0(lindex,i) = Mtick0(i,ticki)
30	   continue
	noangl0(lindex) = Mnoangle(ticki)
	do 40 i = 1, noangl0(lindex)
	   cosangle0(lindex,i) = cos (angle(i,ticki) / rad)
	   sinangle0(lindex,i) = sin (angle(i,ticki) / rad)
           delta10(lindex,i) = delta1(i,ticki)
	   delta20(lindex,i) = delta2(i,ticki)
40	   continue
      else
	logtick(lindex) = .false.
      endif
c
      return
c
      entry placewp(ix, iy, index)
c
c     Places the pen for the next VCTRWP call.
c
c     (IX,IY) = coords to move penb to
c     INDEX = what index line type to use
c     DIREC = direction to increasing contour level.
c
      ix0 = ix
      iy0 = iy
c
      olddirec = 1.e30
      lflip = .not. lflip
c
      dlength = 1.e30
      tlength = 1.e30
      if (logdash(index) ) then
        if (lflip) then 
      		idash = nodash0(index) 
	else
      		idash = 1 
	endif
	dlength = dash0(index,idash)
      endif
      if (logtick(index) ) then
        if (lflip) then 
      		itick = notick0(index) 
		tlength = tick0(index,itick)-1
	else
      		itick = 1 
		tlength = tick0(index,itick)
	endif
      endif
c
      call place(ix0,iy0)
c
      return
c
      entry vctrwp(ix, iy, index, scl, direc)
c
c     Actually does the drawing of a vector with a certain line type.
c
c     (IX,IY) = coords to move penb to
c     INDEX = what index line type to use
c     SCL = scale factor to multiply distances by.
c
      ix1 = ix
      iy1 = iy
      xold = 1.e34
      yold = 1.e34
c
      alength = sqrt ( float(ix1-ix0)**2 + float(iy1-iy0)**2)
      if (alength .eq. 0.) then
	return
      else
	theta = atan2 (float(iy1 - iy0), float(ix1 - ix0))
	cost = float(ix1-ix0) / alength
	sint = float(iy1-iy0) / alength
      endif
c
1 	dist = min (alength, scl*dlength, scl*tlength) + 0.1
c
	if (dist .lt. alength) then
	   x3 = ix0 + nint(dist*cost)
	   y3 = iy0 + nint(dist*sint)
        else
	   x3 = ix1
	   y3 = iy1
        endif
c
	if ( (dist .gt. 0. .and. .not. logdash(index)) .or. 
     .	     (logdash(index) .and. mod(idash,2).ne.0 .and. 
     2	      dist.gt.0.) )  then
	    do 60 i = 1, numline(index)
	      xc = ix0 + nint(float(deltaline(index,i)) * sint)
	      yc = iy0 - nint(float(deltaline(index,i)) * cost)
	      xd = x3 + nint(float(deltaline(index,i)) * sint)
	      yd = y3 - nint(float(deltaline(index,i)) * cost)
	      if (xc .ne. xold .or. yc .ne. yold) call place (xc, yc)
	      call vctr(xd, yd)
	      xold = xd
	      yold = yd
60	      continue
	endif
c	And thats's dashes
c
	if ( logtick(index) .and. dist .ge. scl*tlength) then
c
	   if (olddirec .ne. direc) then
		rtlft = 1.
		dt = (direc - theta) * rad
		if ( (dt .gt. 0. .and. dt .le. 180.) .or.
     1	   	     (dt .lt. -180.) ) rtlft = -1.
		olddirec = direc
	   endif
c
	   do 50 iangle = 1, noangl0(index)
		sinta = sinangle0(index,iangle) * cost + 
     1			   cosangle0(index,iangle) * sint
		costa = cosangle0(index,iangle) * cost -
     1			   sinangle0(index,iangle) * sint
		x4 = x3 - nint(rtlft*scl*delta10(index,iangle)*sint)
		y4 = y3 + nint(rtlft*scl*delta10(index,iangle)*cost)
		x5 = x4 + nint(rtlft*scl*delta20(index,iangle)*costa)
		y5 = y4 + nint(rtlft*scl*delta20(index,iangle)*sinta)
	        do 45 i = 1, numline(index)
	          xc = x4 + nint(float(deltaline(index,i)) * sinta)
	          yc = y4 - nint(float(deltaline(index,i)) * costa)
	          xd = x5 + nint(float(deltaline(index,i)) * sinta)
	          yd = y5 - nint(float(deltaline(index,i)) * costa)
	          if (xc .ne. xold .or. yc .ne. yold) call place (xc, yc)
	          call vctr(xd, yd)
		  xold = xd
		  yold = yd
45	          continue
50		continue
c
	endif
c	That takes care of tick marks
c
         dlength = dlength - dist/scl
	 alength = alength - dist
	 tlength = tlength - dist/scl
	 ix0 = x3
	 iy0 = y3
c
	 if (logtick(index) .and. tlength.le.0. .and. .not.lflip) then
		itick = itick + 1
		if (itick .gt. notick0(index)) itick = 1
		tlength = tick0(index,itick)
   	 endif
c
	 if (logtick(index) .and. tlength .le. 0. .and. lflip) then
		itick = itick - 1
		if (itick .lt. 0) itick = notick0(index)
		tlength = tick0(index,itick) - 1
   	 endif
c
	 if (logdash(index) .and. dlength.le.0. .and. .not.lflip) then
		idash = idash + 1
		if (idash .gt. nodash0(index) ) idash = 1
		dlength = dash0(index,idash)
	 endif
c
	 if (logdash(index) .and. dlength .le. 0. .and. lflip) then
		idash = idash - 1
		if (idash .lt. 0) idash = nodash0(index)
		dlength = dash0(index,idash)
	 endif
c
	 if (alength .gt. 0.) goto 1
c
      olddirec = 1.e30
      return
c
c
      end
c
