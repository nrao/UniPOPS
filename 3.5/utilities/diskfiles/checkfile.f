      program checkfile
c
c @(#)checkfile.f	5.1 06/22/94
c
c     Reads in the INDEX part of a SAVE/KEEP/DATA file and produces a 
c     summary. See MAKEFILE.F for a description of these files.
c
c     Will correctly summarize either the original I*2 index or the I*4 index
c
      character*50 filename
      integer*2 i2dat(1024)
      integer*4 i4dat(512), counter, typesdd, numphase, numrec
      real*4 rdat(512), coordh, coordv, scanno, freqresol, siderialtime,
     .       universaldate
      character*16 cdat(128), sname
      real*8 ddat(256), restfreq
      integer*4 iargc, ii1, ierr, i4pc, i8pc, i16pc, itrybyte, numarg,
     .          numheadrecs, numdatarecs, ibytperrec, ibytperheaditem,
     .          lastindexitem, counter, typesdd, verssdd, istart, istop,
     .          iposcode, numphase, numrec, numindexperrec,
     .          maxheaditem, irec, i, ind, ii, j, i1
      integer*2 i2nhead, i2ndata, i2ibprec, i2ibphead, i2lastind, 
     .          i2counter, i2type
      integer*2 irtn, obs, mode, iobsmode
      character*4 obsmode(2)
      logical isnew
c
      equivalence (cdat,i2dat,i4dat,rdat,ddat)
c
      i4pc(ii) = (ii-1)/2 + 1
      i8pc(ii) = (ii-1)/4 + 1
      i16pc(ii) = (ii-1)/8 + 1
c
      data itrybyte/512/, ii1/1/
c
      numarg = iargc()
      if (numarg .le. 0) then
	write(0,*) 'Must specify filename'
	call exit(ii1)
      else
	call getarg(ii1, filename)
      endif
c
      open(unit=10,file=filename,status='old',access='direct',
     1		recl=itrybyte,form='unformatted',iostat=ierr)
      if (ierr .ne. 0) then
	write(0,*) 'Cannot open file: ', filename
	call exit(ii1)
      endif
      rewind(10)
c
      read(10,rec=1) numheadrecs, numdatarecs, ibytperrec, 
     1		      ibytperheaditem, lastindexitem, counter, typesdd,
     2                verssdd
c
      if (verssdd .eq. 1) then
         isnew = .true.
      else
         isnew = .false.
         rewind(10)
         read(10,rec=1)i2nhead, i2ndata, i2ibprec, i2ibphead, 
     .                 i2lastind, i2counter, i2type
         numheadrecs = i2nhead
         numdatarecs = i2ndata
         ibytperrec = i2ibprec
         ibytperheaditem = i2ibphead
         lastindexitem = i2lastind
         counter = i2counter
         typesdd = i2type
      endif
c
      write(6,*) 'Number of index records', numheadrecs
      write(6,*) 'Number of data records', numdatarecs
      write(6,*) 'Number of bytes per record', ibytperrec
      write(6,*) 'Number of bytes per index item', ibytperheaditem
      write(6,*) 'Number of scans in file', lastindexitem
      write(6,*) 'Counter currently set at ', counter
      write(6,*) 'Type SDD ', typesdd
      if (.not. isnew) then
         write(6,*) 'Old I*2 index, consider using makeindex.exe to'
         write(6,*) '  convert this to the new I*4 index'
      endif
c
      if (numheadrecs.le.1 .or. ibytperrec.le.10 .or. ibytperheaditem
     1    .gt.ibytperrec .or. ibytperrec.gt.2048) then
	write(6,*)  'File is in an improper format!!!'
	write(6,*) 'Cannot proceed'
	stop
      endif
c
      if (typesdd .ne. 0 .and. typesdd .ne. 1) then
	write(6,*)  'File is in an improper format!!!'
	write(6,*) 'Cannot proceed'
	stop
      endif
c
      numindexperrec = ibytperrec / ibytperheaditem
      maxheaditem = (numheadrecs-1)*numindexperrec
c
      write(6,*) 'Maximum number of scans which could be stored',
     1		  maxheaditem
      write(6,*) 'Number of index items per record', numindexperrec
c
      if (ibytperrec.ne.itrybyte) then
	close(10)
        open(unit=10,file=filename,status='old',access='direct',
     1		recl=ibytperrec,form='unformatted')
        rewind(10)
      endif
c     Reopens file if the guessed record size doesn't correspond to the
c	record size stored in the first record of the file
c
      if(lastindexitem.gt.0.and.numheadrecs.ge.2) then
         if (typesdd .eq. 0) then
		write(6,11)
	 else if (typesdd .eq. 1) then
		write(6,12)
	 endif
         do 100 irec=2, numheadrecs
            read(10,rec=irec) (i4dat(j),j=1,ibytperrec/4)
            do 50 i = 1, numindexperrec
               ind = i + numindexperrec*(irec-2)
               if (ind .le. lastindexitem) then
                  i1 = (i-1)*ibytperheaditem/2 + 1
                  if (isnew) then
                     istart = i4dat( i4pc(i1) )
                     istop = i4dat( i4pc(i1+2) )
                     iposcode = i2dat(i1+30)
                  else
                     istart = i2dat(i1)
                     istop = i2dat(i1+1)
c	magic is here (i1+2) but was never used so don't bother with it
                     iposcode = i2dat(i1+3)
                  endif
                  coordh = rdat( i4pc(i1+4) )
                  coordv = rdat( i4pc(i1+6) )
                  sname = cdat( i16pc(i1+8) )
                  scanno = rdat( i4pc(i1+16) )
                  freqresol = rdat( i4pc(i1+18) )
                  restfreq = ddat( i8pc(i1+20) )
                  siderialtime = rdat( i4pc(i1+24))
                  universaldate = rdat( i4pc(i1+26))
                  iobsmode = i2dat(i1+28)
                  obs = iobsmode / 256
                  mode = iobsmode - obs * 256
                  if (obs .eq. 1) then
                     obsmode(1) = 'CONT'
                  else if (obs .eq. 2) then
                     obsmode(1) = 'LINE'
                  else
                     obsmode(1) = '    '
                  endif
                  call modefield(mode, obsmode(2), irtn)
	          if(istart.gt.0) then
		    if (typesdd .eq. 0) then
                      write(6,51) ind,istart, istop, iposcode, 
     .                coordh, coordv, sname, scanno, freqresol,restfreq,
     .                siderialtime, universaldate, obsmode
		    else if (typesdd .eq. 1) then
		      numphase = mod( i2dat(i1+29), 64)
		      numrec = i2dat(i1+29) / 64
                      write(6,51) ind,istart, istop, iposcode, 
     .                coordh, coordv, sname, scanno, freqresol,restfreq,
     .                siderialtime, universaldate, obsmode, numphase,
     .		      numrec
		    endif
		  endif
                  if (irtn .ne. 0) write(6, 52) scanno, mode
               end if
 50         continue
100      continue
      endif
c
      close (10)
      stop
11    format(//,t6,'Item',t13,'Start',t22,'Stop',t27,'PC',
     .		  t32,'H Coord',t42,'V Coord',t50,'Source Name',
     .            t66,'Scan No',t77,'Del F',t87,'Rest Freq',t104,'LST',
     .            t111,'UT Date',t120,'OBSMODE',/,
     .		  t75,'Smple Rate',t87,'Slew Rate',/)
12    format(//,t6,'Item',t13,'Start',t22,'Stop',t27,'Poscd',
     .		  t32,'H Coord',t42,'V Coord',t50,'Source Name',
     .            t66,'Scan No',t77,'Del F',t87,'Rest Freq',t104,'LST',
     .            t111,'UT Date',t120,'OBSMODE',t128,'Phse Rcrd',/,
     .		  t75,'Smple Rate',t87,'Slew Rate',/)
 51   format(1x,3i8,i3,2f10.4,x,a16,f8.2,f11.6,g16.9,f8.4,f10.4,x,2a4,
     .	     i4,i4)
 52   format(1x,'Scan ', f8.2,' has an unknown mode code = ', i5)
      end
c
