      logical function readindex(filename, idev, numbytes, firstrec  )
c
c     @(#)readindex.f	5.1 06/22/94
c
c     Reads in the bootstrap block of an SDD#1 file.  Attaches the
c     file to device IDEV.  Returns the record length (NUMBYTES) and the 
c     first record number of the first scan in the data section of the file
c     (FIRSTREC).
c
c     READINDEX is true if the reading is successful, otherwise it is false.
c
      character*70 filename
      integer idev, numbytes, firstrec
c
      integer*2 i2nhead, i2ndata, i2ibprec, i2ibphead, i2lastind,
     .          i2counter, i2type
      integer*4 numheadrecs, numdatarecs, ibytperrec, ibytperheaditem,
     .          counter, lastindexitem, typesdd, verssdd
      integer itrybyte, ierr
c
      data itrybyte/512/
c
      readindex = .false.
c
      open(unit=idev,file=filename,status='old',access='direct',
     1		recl=itrybyte,form='unformatted',iostat=ierr)
      if (ierr .ne. 0) then
	write(0,*) 'Problem opening SDD file...'
	goto 999
      endif
c
c     Read in the bootstrap record
      read(idev,rec=1,iostat=ierr) numheadrecs, numdatarecs, ibytperrec, 
     1		      ibytperheaditem, lastindexitem, counter, typesdd,
     2                verssdd
      if (ierr .ne. 0) then
	write(0,*) 'Problem reading SDD bootstrap record...'
	goto 999
      endif
c
      if (verssdd .ne. 1) then
         rewind(10)
         read(idev,rec=1,iostat=ierr) i2nhead, i2ndata, i2ibprec, 
     1		      i2ibphead, i2lastind, i2counter, i2type
         if (ierr .ne. 0) then
	   write(0,*) 'Problem reading SDD bootstrap record...'
	   goto 999
         endif
         numheadrecs = i2nhead
         numdatarecs = i2ndata
         ibytperrec = i2ibprec
         ibytperheaditem = i2ibphead
         lastindexitem = i2lastind
         counter = i2counter
         typesdd = i2type
         verssdd = 0
      endif
c
      if (numheadrecs.le.1 .or. ibytperrec.le.10 .or. ibytperheaditem
     1    .gt.ibytperrec .or. ibytperrec.gt.2048) then
	write(0,*)  'SDD file is in an improper format...'
	goto 999
      endif
c
      if (typesdd .ne. 0) then
	write(0,*)  'SDD file is in RECORDS format...'
	goto 999
      endif
c
      if (ibytperrec.ne.itrybyte) then
	close(idev,iostat=ierr)
        if (ierr .ne. 0) open(unit=idev,file=filename,status='old',
     1		access='direct',recl=ibytperrec,form='unformatted',
     2	        iostat=ierr)
        if (ierr .ne. 0) then
	  write(0,*) 'Problem opening SDD file...'
	  goto 999
        endif
      endif
c     Reopens file if the guessed record size doesn't correspond to the
c	record size stored in the first record of the file
c
      readindex = .true.
      numbytes = ibytperrec
      firstrec = numheadrecs + 1
      return
c
999   return
      end
c
