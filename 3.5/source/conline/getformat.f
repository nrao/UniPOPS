      character*10 function getformat(f1, f2, nchar)
c
c------------------------------------------------
c @(#)getformat.f	5.1 06/22/94
c------------------------------------------------
c  
c     Calculates a suggested format for tick mark labels.
c     F1, F2 = (R*4) limits of the plot
c     NCHAR = (I*2) Maximum number of chars for format
c
      real f1, f2, r2, r1, g1
      integer*2 ifrac, nchar, ileft, ierr, iman, iman1, iman2
      character*10 fmtout
c
      r1 = min(f1,f2)
      r2 = max(f1,f2)
c
      if (r1 .eq. r2) then
	if (r1 .gt. 0.0) then
	   r2 = 1.1 * r1
	else if (r1 .lt. 0.0) then
	   r1 = 1.1 * r2
	else
	   r1 = -1.0
	   r2 = 1.0
	endif
      endif
c
      if (r1 .eq. 0.0) r1 = -0.5*r2
      if (r2 .eq. 0.0) r2 = -0.5*r1
c
      ileft = nchar - 1
      if (r1 .lt. 0.) ileft = ileft - 1
c     Reserve a spot for decimal point and negative sign, if needed.
c
      iman1 = int(alog10(abs(r1)))
      iman2 = int(alog10(abs(r2)))
      if (abs(iman1) .gt. abs(iman2) ) then
	iman = iman1
      else
	iman = iman2
      endif
      if (iman .lt. 0) then
	iman = iman - 1
      else
	iman = iman + 1
      endif
c
      if (iman .gt. ileft .or. iman .le. -ileft) then
c       The number can only be represented by an E format
c
	if (r2+r1 .ne. 0. .and. r2-r1 .ne. 0.) then
	   ifrac = max(0, min(ileft-5,
     1              int(1+abs(alog10(abs( (r2+r1)/(r2-r1) ) ) ) ) ) )
	else
	   ifrac = 0
	endif
	if (nchar .lt. 10 .and. ifrac .lt. 10) then
	   write(fmtout,20,iostat=ierr) nchar, ifrac
20	   format( '(1PE', i1, '.', i1, ')' )
	else if (nchar .ge. 10 .and. ifrac .lt. 10) then
	   write(fmtout,21,iostat=ierr) nchar, ifrac
21	   format( '(1PE', i2, '.', i1, ')' )
	else
	   write(fmtout,22,iostat=ierr) nchar, ifrac
22	   format( '(1PE', i2, '.', i2, ')' )
        endif
c
      else
c	The number can be represented with an F format
c
	g1 = mod(abs(r2-r1),1.)
	if (g1 .ne. 0. .and. g1 .gt. 10.**(-7+iman) ) then
	   ifrac = min(ileft-iman, max(0, int(abs(alog10(g1))+2.) ) )
	else
	   ifrac = 0
	endif
	if (nchar .lt. 10 .and. ifrac .lt. 10) then
	   write(fmtout,30,iostat=ierr) nchar, ifrac
30	   format( '(F', i1, '.', i1, ')' )
	else if (nchar .ge. 10 .and. ifrac .lt. 10) then
	   write(fmtout,31,iostat=ierr) nchar, ifrac
31	   format( '(F', i2, '.', i1, ')' )
	else
	   write(fmtout,32,iostat=ierr) nchar, ifrac
32	   format( '(F', i2, '.', i2, ')' )
        endif
      endif        
c
      getformat = fmtout
c
      return
c
      end
c
      
