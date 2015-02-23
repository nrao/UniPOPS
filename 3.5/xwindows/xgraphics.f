      program graphics
c
c     %@% 12/08/98
c
c     Accepts graphics commands from the Fifo specified as the second arguement
c     on the command line and does the graphics command using Xlib.  If the
c     command is Flush, then it will read graphics commands from the
c     shared memory (which has the same name as this Fifo).  When a
c     cursor is asked for, the position and button pushed are returned to the
c     Fifo specified as the first command-line arguement. 
c
c     Starts up the 'CHECKUP.EXE' script to make sure that both this process
c     and its parrent process (whose ID is given as the third arguement)
c     remain running
c
c     other arguments, in any order after the first 3 are :
c         -iconic 		start iconic
c         -name  NAME		give NAME to window and icon
c         -geometry STRING	STRING = std X geometry
c
c			dither matrix size = MATSIZE
      integer*4 MATSIZE
      parameter (MATSIZE = 16384)
      character*80 string
      character*16 fifoin, fifoout, shove, popsprocid, procid,
     .		   stch
      character*18 blank  
      integer*2 ilen, lastblnk
      integer*4 iargc, inumarg, irtn, readshm2, cmnd, pos1, pos2,
     .          getpid, system, ifpread, ifpwrite, unlink, icp, icpx,
     .          icpy, iconic, iarg, i, j, n0, n1, n2, n3, n6
      character*80 geometry, name, arg
      character*5 memname
c				the following are used in the dither routine
      integer*4 levmax, ixdt4, iydt4
      integer*4 nx, ny, ix, iy, nxold, nyold, ixdt(MATSIZE),
     .          iydt(MATSIZE), matset(MATSIZE), ii3, ierr
      real percent, rand, boxx(4), boxy(4)
      logical something, first
c
      data blank/'                  '/
      data name /'PopsTool'/
      data iconic /0/
      data n0, n1, n2, n3, n6 /0, 1, 2, 3, 6/
c
      name(lastblnk(name)+1:lastblnk(name)+1) = char(0)
      geometry(1:1) = char(0)
c
      inumarg = iargc()
c
      if (inumarg .lt. 3) then
	write(0,*) 'GRAPHICS: bad parameter list'
	call exit(n6)
      else
	call getarg(n1,fifoin)
	call getarg(n2,fifoout)
	call getarg(n3,popsprocid)
      endif
c     Gets the shared memory and Fifo names from the command line
c
c			get the rest of the arguments
      iarg = 4
 100  continue
         if (iarg .le. inumarg) then
            call getarg(iarg, arg)
            iarg = iarg + 1
            if (arg(1:lastblnk(arg)) .eq. '-iconic') then
               iconic = 1
            else if (arg(1:lastblnk(arg)) .eq. '-name') then
               call getarg(iarg, name)
               name(lastblnk(name)+1:lastblnk(name)+1) = char(0)
               iarg = iarg + 1
            else if (arg(1:lastblnk(arg)) .eq. '-geometry') then
               call getarg(iarg, geometry)
               geometry(lastblnk(geometry)+1:lastblnk(geometry)+1) = 
     .                  char(0)
               iarg = iarg + 1
            else
               write(0,*) 'GRAPHICS: bad argument ' // arg
            endif
            goto 100
         endif
c
      irtn = getpid()
      write(procid, 1) irtn
1     format(i8)
      irtn = system('checkup.exe ' // procid // ' ' // 
     .              popsprocid // ' & ')
c     Get this processes PID and start up 'CHECKUP.EXE'
c
      call openwrite(ifpwrite, fifoin(1:lastblnk(fifoin)) // '\0')
      call openread(ifpread, fifoout(1:lastblnk(fifoout)) // '\0')
c     open FIFO's
c
c                               only 4 characters can be used for the
c                               shared memory name (its a long int)
c                               So, we use the last 4 digits of procid
c                               substitute 0's if less than 4 digits
      ilen = lastblnk(popsprocid)
      j = 1
      do 105 i = ilen-3, ilen
         if (i .le. 0) then
            memname(j:j) = '0'
         else
            memname(j:j) = popsprocid(i:i)
         endif
         j = j + 1
 105  continue
c                               null terminate it
      memname(5:5) = char(0)
c
      call accessshm(memname)
c     Start up shared memory
c
      call sig_init()
      call xinit(iconic, name, geometry)
      call alarm_on()
c     Start up an X window, turn on alarm clock
c
10    call readfifo(ifpread, irtn, shove)
c     Read from FIFO
c
      call alarm_off()
      if (shove(1:4) .eq. 'exit') then
	call xclose()
	call closefifo(ifpread, irtn)
	call closefifo(ifpwrite, irtn)
	irtn = unlink(fifoin)
	irtn = unlink(fifoout)
	stop
c	Close down program
c
      else if (shove(1:6) .eq. 'cursor') then
        call xfront()
	call xcursor(icp, icpx, icpy)
	write(stch,15) icp, icpx, icpy
15	format(3i5)
        call writefifo(ifpwrite, irtn, stch // '\0')
c       Start up a CURSOR session and return position and button number
c
      else if (shove(1:5) .eq. 'clear') then
 	call xclrpage
c       Clear the screen
c
      else if (shove(1:5) .eq. 'flush') then
c       We have something in shared memory that needs to be processed
c
	first = .true.
	something = .false.
20      irtn = readshm2(cmnd, pos1, pos2, string)
	if (irtn .gt. 0) then
c	   We haven't gotten to the end of shared memory yet
c
	   if (cmnd .eq. 1) then
	      call xplace(pos1, pos2)
c	      Place command
c
	   else if (cmnd .eq. 2) then
	      call xvctr(pos1, pos2)
	      something = .true.
c	      Vctr command
c
	   else if (cmnd .eq. 4) then
	      call xcolor(pos1)
c	      Command for updating or changing the color table.
c
	   else if (cmnd .eq. 15) then
              read(string,1010) boxx, boxy
 1010         format(8f10.0)
	      call xbox(boxx, boxy)
	      something = .true.
c	      Command for drawing a filled box.
c
	   else if (cmnd .eq. 16) then
              ix = pos1
              iy = pos2
              read(string, 1000, iostat=ierr) nx, ny, percent
 1000         format(i5, i5, f10.0)
              levmax = nx * ny
              levmax = min(MATSIZE,levmax)
c
              if (nx .ne. nxold .or. ny .ne. nyold) then
                 do 110 i = 1, levmax
                    matset(i) = 0
 110             continue
c			clear out matrix
c
                 do 130 i = 1, levmax
 120                ii3 = nint(rand(n0) * float(levmax-1) + 1.0)
                    if (matset(ii3) .eq. 1) goto 120
                    matset(ii3) = 1
                    ixdt(i) = ifix(ii3/ny) + 1
                    iydt(i) = ii3 - (ixdt(i)-1)*ny
 130             continue
              endif
c			fill in pixel arrays and set matrix
c
              ii3 = nint(rand(n0)*float(levmax-1)+1.0)
              do 140 i = 1, nint(percent*float(levmax))
                 ii3 = ii3 + 1
                 if (ii3 .gt. levmax) ii3 = 1
                 ixdt4 = ix + ixdt(ii3) - 1
                 iydt4 = iy + iydt(ii3) - 1
                 call xpoint(ixdt4, iydt4)
 140          continue
c			draw pixels
              nxold = nx
              nyold = ny
c			all the work for dither is done here
	      something = .true.
c***           else
c***	      write(0,*) 'Bad command:', cmnd
	   endif
c
           if (first .and. something) then
		call xfront()
		first = .false.
		something = .false.
	   endif
c
	   goto 20
c
	else
c
	   call writefifo(ifpwrite, irtn, 'done')
c	   Shared memory is exausted and we should ask for more graphics commands
c
	endif
c***      else
c***	write(0,*) 'Bad shove'
      endif
c
      call xflush()
      call alarm_on()
      goto 10
c
99    write(0,*) 'GRAPHICS: Problem opening file'
      stop
      end
c
      integer*2 function lastblnk(string)
c
c     Finds the position of the last non-blank character in STRING
c
      character*(*) string
      integer*4 lnblnk
c
      lastblnk = lnblnk(string)
c     MASSCOMP/SUN function LNBLNK
c
      return
      end
