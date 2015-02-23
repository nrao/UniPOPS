      subroutine initlog
c-------------------------------------------------------------------------------
c @(#)initlog.f	5.1 06/22/94
c
c     Sets up logfile
c
C-------------------------------------------------------------------------------
c
      integer*4 istat, n8
      integer*2 lastblnk
      logical*2 opened
      character*160 line
      character*1023 fullname
      character*24 zdatetime
c
      include 'cio.inc' 
c
      data n8 / 8/
c
      call filecomp(clogout, fullname)
      if (opened(fullname)) return
      open(ilogout,file=fullname,status='unknown',form='formatted',
     1     iostat = istat)
      if (istat .ne. 0) then
         write(istderr,*) ' Can not open command log file ',
     .			  clogout(1:lastblnk(clogout))
         call exitpops(n8)
      end if
c				find end of file
 10   continue
         read(ilogout, 1010, end = 20, iostat = istat) line
         if (istat .ne. 0) then
            write(istderr, 1020) clogout(1:lastblnk(clogout))
            call exitpops(n8)
         end if
      goto 10
c
 20   continue
c
      if (dolog) then
	  write(ilogout, 1030, iostat=istat)'# Command logging ON -- ', 
     .				zdatetime()
          if (istat .ne. 0) then
            write(istderr, 1030) 'Error while trying to write to ', 
     .				clogout(1:lastblnk(clogout))
            call exitpops(n8)
          endif
      endif

      return
c
 1010 format(a10)
 1020 format(' Error while trying to find end of log file ',a)
 1030 format(3a)
c
      end
c
