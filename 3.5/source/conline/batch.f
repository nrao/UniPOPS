      subroutine batch(cnfile)
C-------------------------------------------------------------------------------
C  @(#)batch.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Redirects POPS input from a specified file until EOF
c     or ERROR 
c
c     March 1989 [RJM]
c**********************************************************
c
      integer*2 i, lastblnk, ilen, ilen2, ier, istat
      integer*2 n28, n364, m1
      integer*4 tmpunit, ioget
      character*60 cbatchin
      character*1023 cnfile2
      character*(*) cnfile
c
      include 'cio.inc'
      data n28, n364, m1 /28, 364, -1/
c
      tmpunit = -1
      if (iinptr .lt. 50) then
         tmpunit = ioget()
      endif
      if (tmpunit .eq. -1) then
         call oerror(n28, m1, ' ')
      endif
c
c     Makes sure there is an available unit and space in iinlist
c
      ilen = min(lastblnk(cnfile), 60)
      if (ilen .eq. 0) goto 210
c     Extracts the next field on the input line and stores it in cnfile
c
      do 200 i = 0, 2, 1
	if (i.eq.0) then
	   cbatchin = cnfile(1:ilen)
           call filecomp(cbatchin, cnfile2)
c		only attempt a file completion the first time, if this isn't
c		found, then look for it as is in the other 2 places
	else
	   ilen2 = lastblnk(dirprefix(i+3))
	   if (ilen2 .eq. 0) goto 200
	   cnfile2 = dirprefix(i+3)(1:ilen2)// '/' //cnfile(1:ilen)
	endif
	open(unit=tmpunit,file=cnfile2,status='old',iostat=ier)
c
	if (ier .eq. 0) rewind(tmpunit,iostat=istat)
	if (ier .eq. 0 .and. istat .eq. 0) then
           iinptr = iinptr + 1
           iinlist(iinptr, 1) = tmpunit
           iinlist(iinptr, 2) = ibatchin
           cinlist(iinptr) = cbatchin
           iinunit = tmpunit
	   iintype = ibatchin
           if (.not. lsetup) then
              if (i.eq.0) then
                 ilen2 = lastblnk(cnfile2)
                 write(iout,1200) cnfile2(1:ilen2)
              else
                 ilen2 = lastblnk(cbatchin)
                 write(iout,1210) i, cbatchin(1:ilen2)
              endif
           endif
	   return
	endif
200	continue
c     Looks for file in present directory and then in pop's system 
c     directory
c
210   call ioput(tmpunit)
c				free up the unused tmpunit if error
      call oerror(n364, m1, ' ' // cnfile(1:ilen) )
      return
c
 1200 format(x,'Batch File: ',a)
 1210 format(x,'Batch File: $popsproc',i1,'/',a)
c
      end
c
