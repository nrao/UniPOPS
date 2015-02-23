      subroutine initsetup()
C-------------------------------------------------------------------------------
C  @(#)initsetup.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Prepares POPS so that it will read from the SETUP file
c     if one exists at start up.
c
      include 'cio.inc'
c
      logical*2 inquirefile
      integer*4 long
c
      lsetup = .false.
c
      if (inquirefile(csetupin)) then
            open(unit=isetupin,file=csetupin,status='old',err=50)
            rewind(isetupin,err=50)
            write(istdout,*) ' '
            write(istdout,*) 'Initializing program using SETUP file' 
            write(istdout,*) ' '
            call flush(long(istdout))
            if (iinptr .ge. 50) then
c				no room in iinlist, this should NEVER
c				happen, so we're going to check it anyway
               write(istderr, *) ' '
               write(istderr, *) 'SETUP file can not be added ',
     .                           'to list of open input devices'
               write(istderr, *) 
     .             'This should not happen, report this problem.'
               write(istderr, *) 'Using default initialization'
               close(unit=isetupin)
            else
               iinptr = iinptr + 1
               iinlist(iinptr, 1) = isetupin
               iinlist(iinptr, 2) = isetupin
               cinlist(iinptr) = csetupin
               iinunit = iinlist(iinptr, 1)
               iintype = iinlist(iinptr, 2)
               lsetup = .true.
            endif
	    return
      endif
c
50       write(istderr,*) ' '
         write(istderr,*) 'SETUP file doesn''t exist... ',
     1	                  'Using default initialization'
         write(istderr,*) ' '
c
c
      return
      end
c
