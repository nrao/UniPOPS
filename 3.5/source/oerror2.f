      subroutine oerror2
C-------------------------------------------------------------------------------
C  @(#)oerror2.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Traps an error if it occurs when 'reading' input from other
c     than the terminal.  Asks whether or not to continue reading 
c     from a file at that point.
c
      real*4 answr
      integer*4 ioput, lunit, i
      integer*2 istat, lastblnk
      character*64 cbatchin
c
      include 'cio.inc'
c
      if (iintype .ne. istdin .and. iintype .ne. imenuin) then
         write(istderr,*) ' '
         if (iintype .eq. ibatchin) then
            cbatchin = cinlist(iinptr)
            write(istderr,10) 'Error in batch file ', 
     .         cbatchin(1:lastblnk(cbatchin))
            do 100 i = iinptr, 2, -1
               if (iinlist(i,2) .ne. ibatchin) goto 110
               close(iinlist(i,1),iostat=istat)
               lunit = iinlist(i,1)
               lunit = ioput(lunit)
c			ignore any errors from ioput, we don't really care
 100        continue
c			back out to last menu or stdin input
 110        iinptr = i
            iinunit = iinlist(iinptr, 1)
            iintype = iinlist(iinptr, 2)
            if (iintype .ne. isetupin) then
               if (iintype .eq. ishelpin) then
                  write(istderr,10) 'Internal ERROR in help file ... ',
     1	                         ' Report problem'
               else
                  write(istderr, 11) 'Control returns to '
                  if (iintype .eq. imenuin) then
                     write(istderr, 10) 'MENUs input.'
                  else
                     write(istderr, 10) 'standard input.'
                  endif
               endif
	    endif
	 endif
c
         if (iintype .eq. isetupin) then
            write(istderr,10) 'Error in SETUP file'
            write(istderr,*) ' '
            write(istderr,10) 'Terminate reading from file or skip to ',
     1                        'next line in file'
            write(istderr,11) 'Terminate (Y or N)? (Default = N) '
            read(istdin,12) answr
            if (answr .eq. 'y' .or. answr .eq. 'Y') then
               close(iinunit,iostat=istat)
               iinptr = max(1, iinptr - 1)
               iinunit = iinlist(iinptr, 1)
               iintype = iinlist(iinptr, 2)
            endif
         else if (iintype .eq. ishelpin) then
            write(istderr,10) 'Internal ERROR in help file ... ',
     1	                      ' Report problem'
         endif
 99      write(istderr,*) ' '
      endif
c
      return
c
 10   format(1x,10a)
 11   format(1x,a,$)
 12   format(a1)
C
      end
c
