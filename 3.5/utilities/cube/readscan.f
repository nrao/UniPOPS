      logical function readscan(idev,numbytes,recnumber,maxbytes,idat)
c
c     @(#)readscan.f	5.1 06/22/94
c
c     Reads in the next scan from the SDD#1 file attached to IDEV.
c     NUMBYTES = number of bytes in each record of the file
c     MAXBYTES = maximum number of bytes in scan that can be returned
c     RECNUMBER is a suggetsed record number at which to look for data in
c		file.  Returns the last record + 1 read in so as to
c		be prepared for a future call to READSCAN.
c     IDAT = Array in which scan is returned
c  
c     Returns FALSE if an error on read or EOF; else it returns TRUE.
c
      integer*2 idat(*)
      integer numbytes, recnumber, maxbytes 
c
      integer irec, idev, numclass, j, i, bytesinscan,
     .		numtoread, j1, j2, ierr
      integer*2 idat1(28)
      real*8 ddat1(7)
c
      equivalence (ddat1, idat1)
c
      readscan = .false.
c
      irec = recnumber - 1
c
100     irec = irec + 1
	read(idev,rec=irec,iostat=ierr) (idat(j),j=1,numbytes/2)
c       Read in the first possible data record of the scan
c 
 	if (ierr .lt. 0) then
	   write(0,*) 'End of SDD File....'
	   goto 999
	else if (ierr .gt. 0) then
	   write(0,*) 'Error reading from SDD file...'
	   goto 999
	endif
c
	numclass = idat(1)
        if (numclass .le. 0 .or. numclass .gt. 14) goto 100
	do 40 i = 2, numclass+1
	   if (idat(i) .lt. 0) goto 100
40	   continue
c       Skip over any unused or bad scans/records by checking PREAMBLE
c
 	do 101 i = 1, 28
	   idat1(i) = idat(i)
101	   continue
c       Store first few words in scan into IDAT1/DDAT1
c
        bytesinscan = ddat1(5)+ddat1(6)
        if (bytesinscan .le. 0) goto 100
c	    
	if (mod(bytesinscan, numbytes) .ne. 0) then
	   numtoread = bytesinscan/numbytes + 1
	else
	   numtoread = bytesinscan/numbytes
	endif
c       Number of records in scan
c
	if (bytesinscan .gt. maxbytes) then
	    write(0,*) 'Too many bytes in scan', ddat1(7),
     .		       '.... Skipping.....'
            do 45 i = 2, numtoread
	        irec = irec + 1   
	        read(idev,rec=irec,iostat=ierr) (idat(j),j=1,numbytes)
 		if (ierr .lt. 0) then
	   		write(0,*) 'End of SDD File....'
	   		goto 999
		else if (ierr .gt. 0) then
	   		write(0,*) 'Error reading from SDD file...'
	   		goto 999
		endif
45	        continue
	    goto 100
c           Skip the rest of the scan since it is TOO big
c
	else    
           do 50 i = 2, numtoread
	        j1 = (i-1)*numbytes/2 + 1
	        j2 = i*numbytes/2
	        irec = irec + 1   
	        read(idev,rec=irec,iostat=ierr) (idat(j),j=j1,j2)
 		if (ierr .lt. 0) then
	   		write(0,*) 'End of SDD File....'
	   		goto 999
		else if (ierr .gt. 0) then
	   		write(0,*) 'Error reading from SDD file...'
	   		goto 999
		endif
50	        continue
c           Read in the rest of the scan
c
	endif
c
      recnumber = irec + 1
c     Prepare RECNUMBER for next call to READSCAN.
c
      readscan = .true.
c
999   return
c
      end
c
