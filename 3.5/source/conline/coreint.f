      subroutine coreint
C-------------------------------------------------------------------------------
C  @(#)coreint.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Initializes Graphics Shared memory, FIFOs, and process
c*********************************************
c 8904 [RJM]
c 8911 [RJM]  
c*********************************************
c
      integer*2 lastblnk
      integer*4 irtn, system, ilen, i, j
      character*18 blank
      character*1 wintype
      character*5 key
c
      include 'cio.inc'
c
      data blank/'                  '/
c				only 4 characters can be used for the
c				shared memory key (its a long int)
c				So, use the last 4 digits of procid
c				substitute 0's if less than 4 digits
      ilen = lastblnk(procid)
      j = 1
      do 100 i = ilen-3, ilen
         if (i .le. 0 .or. procid(i:i) .lt. '0' .or.
     .            procid(i:i) .gt. '9') then
            key(j:j) = '0'
         else
            key(j:j) = procid(i:i)
         endif
         j = j + 1
 100  continue
c				null terminate it
      key(5:5) = char(0)
c
      call createshm2(key)
      wintype = 'S'
      if (termtype .eq. 'xwindow') wintype = 'X'
      irtn = system('makegraph.exe ' // cgraphin // ' ' // cgraphout //
     .  	     ' ' // procid // ' ' // program(1:1) // 
     .               ' ' // wintype)
c     Create shared memory and start up graphics process
c
      if (irtn .ne. 0) then
	write(istderr,*) 'Cannot open graphics screen.... Continuing!'
	igraphtype = 0
	goto 99
      endif
c
      call openread(irtn, cgraphin(1:lastblnk(cgraphin)) // '\0' )
      if (irtn .lt. 0) then
	write(istderr,*) 'Cannot open graphics screen.... Continuing!'
	igraphtype = 0
	goto 99
      endif
      igraphin = irtn
c
      call openwrite(irtn, cgraphout(1:lastblnk(cgraphout)) // '\0' )
      if (irtn .lt. 0) then
	write(istderr,*) 'Cannot open graphics screen.... Continuing!'
	igraphtype = 0
	goto 99
      endif
      igraphout = irtn
c     Open graphics FIFO's
c
99    return
      end
c
