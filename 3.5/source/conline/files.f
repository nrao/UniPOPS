      subroutine files
C-------------------------------------------------------------------------------
C  @(#)files.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Lists data files and checks for their existence.
c
c     RJM December, 1989
c
      include 'cio.inc'
c
      integer*4 access, liounit, lrtn, onlinesite
      integer*2 istch(40), i, istat, GAINS, RECORDS
      integer*2 n80
      logical*2 opened
      character*4 modes(12)
      character*32 notavail, crshfle
      character*15 types(12), oldata
      character*18 verbs(12)
      character*80 stch
      character*1023 fullname, fullname1
c
      equivalence (istch, stch)
c
      data n80 /80/
      data notavail/'Not Available'/
      data modes/'r ','rw ','rw ','r ','r ',5*' ','w ','w '/
      data types/'Data (offline)','Keep','Save',
     1           'Gains','Records', 5*' ',
     2           'Printer','Command logging'/
      data oldata/'Data (online)'/
      data verbs/
     1  'GET, ON, OFF',
     2  'KEEP, KGET  ',
     3  'SAVE, RECALL',
     4  'GGET',
     5  'GETIR',
     6  5*' ',
     7  'PRINTER,CRT,OUTPUT',
     8  'LOGON, LOGOFF'/
c
      parameter (GAINS=4)
      parameter (RECORDS=5)
c
c    modes = array containing neccessary privelages for various files;
c	r = read, w = write, blank = exists
c    types = array containing the 'type' of the various files
c    verbs = array containing the name of verbs which use these files as
c       well as miscellaneos notes
c    The first ten entries in these arrays refer to to files described
c       printout, and memory files, the 11th entry to the printout
c	file, and the 12th to the log file  (see cio.inc for details)
c
c    Files ALWAYS closes all data files via closeall() first.
c
      call closeall()
      write(stch,1) ' '
1     format(1x,10a)
      call pwrite(istch, n80)
c
      write(stch,1) ' '
      call pwrite(istch, n80)
      write(stch,1) 'Table of files'
      call pwrite(istch, n80)
      write(stch,10)
      call pwrite(istch, n80)
10    format(1x,71('-'))
      write(stch,1) 
     1   '  | File type     | Filename                       ',
     2   '| Accessed by'
      call pwrite(istch, n80)
      write(stch,10)
      call pwrite(istch, n80)
c
      if (online) then 
	write(stch,19) oldata, 'Project: ' // projcode, verbs(1)
19      format(3x,'|',a15,'|',a17,15x,'|',a18)
      else
	write(stch,19) oldata, notavail(1:17), verbs(1)
      endif
      call pwrite(istch, n80)
c
      do 100 i = 1,10 
c
         liounit = iounit(i)
	 if (i .eq. GAINS .and. onlinesite() .eq. 0) goto 100
	 if (i .eq. RECORDS .and. onlinesite() .gt. 0) goto 100
c	 Skip a file type that can't be used with a particular program
c
         lrtn = 0
         if (iounit(i).ne.-32767) then
            if (iounit(i) .gt. 0) then
               call filecomp(ciounit(i), fullname)
               call sddopen(liounit, fullname, modes(i), lrtn)
            else
               lrtn = 1
            endif
            if ( (lrtn .eq. 0 .and. i .ne. RECORDS) .or. 
     .		 (lrtn .eq. -1 .and. i .eq. RECORDS) ) then
               write(stch,20) i,types(i),crshfle(fullname),verbs(i)
            else
	       call sddclose(liounit) 
               write(stch,20) i, types(i),notavail,verbs(i)
	       iounit(i) = -abs(iounit(i))
            endif
            call pwrite(istch, n80)
         endif
 100  continue
 20   format(1x,i2,'|',a15,'|',a32,'|',a18)
c       Checks whether general data files exist and have the proper
c       protections.
c
c
      if (iprtout .ne. -32767) then
        call filecomp(cprtout, fullname)
	if(iprtout.gt.0 .and. access(fullname,modes(11)).eq.0) then
	  write(stch,20) 11, types(11),crshfle(fullname),verbs(11)
	else
	  write(stch,20) 11,types(11),notavail,verbs(11)
	  if (iprtout.gt.0 .and. opened(fullname)) 
     1		close(iprtout,iostat=istat)
	  iprtout = -abs(iprtout)
	endif
      endif
      call pwrite(istch, n80)
c
      if (ilogout .ne. -32767) then
        call filecomp(clogout, fullname1)
	if(ilogout.gt.0 .and. access(fullname1,modes(12)).eq.0) then
	  write(stch,20) 12, types(12),crshfle(fullname1),verbs(12)
	else
	  write(stch,20) 12,types(12),notavail,verbs(12)
	  if (ilogout.gt.0 .and. opened(fullname1)) 
     1		close(ilogout,iostat=istat)
	  ilogout = -abs(ilogout)
	endif
      endif
      call pwrite(istch, n80)
c
      if (iprtout .gt. 0 .or. ilogout .gt. 0) then
	write(stch,10)
        call pwrite(istch, n80)
      endif
      if (iprtout .gt. 0) then
	write(stch,1) '  Output will be appended to end of ',
     1			   crshfle(fullname),'         '
        call pwrite(istch, n80)
      endif
      if (ilogout .gt. 0) then
	write(stch,1) '  Output will be appended to end of ',
     1			   crshfle(fullname1),'         '
        call pwrite(istch, n80)
      endif
c
      write(stch,10)
      call pwrite(istch, n80)
      write(stch,1) 'It is YOUR responsibility to ensure the',
     1		       ' files are in the correct format'
      call pwrite(istch, n80)
      write(stch,1) ' '
      call pwrite(istch, n80)
c
      return
      end	
c
      character*32 function crshfle(filename)
c
c     Takes the input filename string and returns the last 32 characters
c     in the name.  If more than 32 chars in name, the first 3 chars
c     will be '...'.  For example:
c     if 'BLAH' is the filename than 'BLAH' is returned.  If the filename 
c     is '/23456789/123456789/123456789/12345'  (i.e., 35 chars long), then
c     '...789/123456789/123456789/12345' is returned.
c
      character*(*) filename
c
      integer*2 lastblnk, ilen
c
      ilen = lastblnk(filename)
c
      if (ilen .le. 32) then
	crshfle = filename
      else
	crshfle = '...' // filename(ilen-28:ilen)
      endif
c
      return
      end
