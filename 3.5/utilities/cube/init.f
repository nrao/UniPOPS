      subroutine init 
c
c     @(#)init.f	5.1 06/22/94
c
c     Initializes the cube file.  Header records are filled with blanks
c     Data records are filled with IBADs.  Determines maxrec and mleft
c     (the # records in Cube and # of positions used in last record)
c
      integer*2 lzero
      integer j, i
      character*2 spaces
c
      include 'cube.inc'
c
      data spaces/'  '/,lzero/0/
c
      write(6,*) 'Initializing data cube...'
      write(6,*)
c
      do 50 j=1, numdata
        lout(j) = ibad
50    continue
c
      maxrec = (na * nd * nv) / numdata + 1
      mleft = mod(na*nd*nv, numdata)
      if (mleft .ne. 0) maxrec = maxrec + 1
c
      write(6,*) 'Max record number :', maxrec
      write(6,*) 'Number of items used in last record :',mleft
      write(6,*) ' '
c
      write(ioutdev,rec=1) (spaces,i=1,numdata)
      if (maxrec .gt. 2) then
         if (debug) write(6,89) 'Rec:     0'
89       format(1x,a,1x,$)
	 do 100 irec = 2, maxrec
	    if (debug .and. mod(irec-2,20) .eq. 0 .and. irec .ne. 2) 
     .			write(6,90) irec-2
90	    format(1x,i5,1x,$)
	    if (debug .and. mod(irec-2,240) .eq. 0 .and. irec .ne. 2)
     .			write(6,*) ' '
            write(ioutdev,rec=irec) lout
100         continue
         if (debug .and. (mod(maxrec-2,240) .ne. 0 .or. maxrec .eq. 2))
     .			write(6,*) ' '
      endif
c
      if (mleft .ne. 0) then
	write(ioutdev,rec=maxrec) (lout(i),i=1,mleft),
     1			     (lzero,j=mleft+1,numdata)
      endif
c
      return
c
      end
c
