      program makeempty
c
c @(#)makeempty.f	5.1 06/22/94
c
c   Generates an empty file.
c     usage: makeempty filename [filesize]
c
c     where filesize is an optional argument indicating the number of
c     index entries that filename will hold.  1024 is assumed if no
c     value is give for filesize.
c
c     This version only generates the new I*4 index.
c 
c     Format for a KEEP/SAVE/DATA file is as follows:
c     The file consists of 512 byte records and is a DADS.
c     The minimum file size is 129 records and the maximum is, in
c	principal, unlimited.  
c     The first 129 records contain an index to the data records which
c	immediately follow these index records.
c     The index has room for 1024 entries so the maximum number of 
c	storable scans is 1024.
c     The first index record (i.e., record 1) contains information about the
c	index.  This information follows the following structure:
c	      Integer*4:  Number of Records in Index including the first
c		  	  record (default 129).
c	      Integer*4:  Number of data records which follow index records;
c			  initially set to zero
c	      Integer*4:  Bytes per record (512)
c	      Integer*4:  Number of bytes per entry in index (64)
c	      Integer*4:  Largest index entry number in use, initially zero.
c             Integer*4:  An incremental counter used by the disk access
c                         routines, initially set to zero
c             Integer*4:  The type of SDD file, 0 => normal, 1 => records;
c                         initially set to zero.
c             Integer*4:  SDD version number = 1 for this version
c	      Integer*4:  120 zeros to pad out the record to its 512 byte size;
c			  future entries can be placed there.
c
c     One index entry describes one scan in the data section of the file.
c     These index entries, as specified above, take up 64 bytes and start in
c	record 2 of the file and go through the end of record 129.  8 index
c	entries are in each of these 128 records for a maximum of 1024 entries;
c     Each entry in the index has the following format (sdd version = 1):
c		Integer*4:    Starting record number for the scan about to be
c			         described in the index.
c		Integer*4:    Last record number for that scan.
c	        Real*4:	      Horizontal Coordinate in degrees
c		Real*4:       Vertical Coordinate in degrees
c		Character*16: Source name
c		Real*4:	      Scan number
c		Real*4:       Frequency resolution in MHz
c		Real*8:	      Rest Frequency of the observation in MHz
c		Real*4:       LST in hours
c		Real*4:       UT date in YYYY.MMDD format
c		Integer*2:    OBSMODE coded (see cookbook and modes.h)
c		Integer*2:    Phase & record information
c		Integer*2:    Position Code (1 through 14, see cdcodes.h)
c		Integer*2:    1 zero for padding out index item to 64 bytes;
c				a future index item could be stored here.
c
c	All of the above are 0s at file creation.
c
c     The data records, when a file is created, do not exist but are added
c	either sequentially or by its position in the index.  For example, in
c	the case of the later, the application program could store a scan in
c	records 130-135 but put the index information for those records in
c	the 134 index entry (i.e., record 134/8+2 = 17, bytes 64*mod(134,8)+1
c	=384 to 384+63=447 within that record)
c     The number of records per scan is arbitrary in this system.  The format
c	of the data records is also arbitrary.
c
      character*50 filename
      character*10 cnumindex
      integer*4 idat(128) 
      integer*4 iargc, i1, i2, lastindexitem, numheadrecs,
     .          numdatarecs, ibytperrec, ibytperheaditem, numarg,
     .          icounter, itypesdd, isddversion, i, ierr, 
     .          numindex, irec
c
      data lastindexitem/0/, i1/1/, i2/2/
      data numheadrecs/129/
      data numdatarecs/0/
      data idat/128*0/
      data ibytperrec/512/
      data ibytperheaditem/64/
      data icounter /0/
      data itypesdd /0/
      data isddversion /1/
c
      numarg = iargc()
      if (numarg .le. 0) then
	write(0,*) 'Must specify filename and an optional file size'
	call exit(i1)
      else
	call getarg(i1, filename)
	if (numarg .gt. 1) then
	  call getarg(i2, cnumindex)
	  read(cnumindex,10,iostat=ierr) numindex
10	  format(i)
	  if (ierr .ne. 0 .or. numindex .le. 2) then
		write(0,*) 'Bad number of index items:', cnumindex
		call exit(i1)
	  else
		numheadrecs = (numindex * ibytperheaditem) / ibytperrec + 1
		if (mod(numindex*ibytperheaditem,ibytperrec) .ne. 0)
     .		   numheadrecs = numheadrecs + 1
	  endif
	endif
      endif
c
      open(unit=10,file=filename,status='new',access='direct',
     1		recl=ibytperrec,form='unformatted',iostat=ierr)
      if (ierr .ne. 0) then
	write(0,*) 'Cannot create new file: ', filename
	call exit(i1)
      endif
c
      rewind(10)
c
      write(10,rec=1) numheadrecs, numdatarecs, ibytperrec, 
     1		      ibytperheaditem, lastindexitem, icounter,
     2                itypesdd, isddversion,
     3		      (idat(i),i=1,120)
      do 100 irec=2, numheadrecs
	write(10,rec=irec) idat
100	continue
c
      close (10)
      stop
      end
c
