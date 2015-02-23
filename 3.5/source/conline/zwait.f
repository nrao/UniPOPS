      subroutine zwait(time)
C-------------------------------------------------------------------------------
C  @(#)zwait.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     System call for putting the program to sleep for
c     a specified TIME (in sec).
c     RJM   1989 March
c
      real*4 time
      integer*4 isec
c
      isec = max(1, int(time+0.5))
      call sleep(isec)
c
      return
      end
c
c-----------------------------
c
      character*24 function zdatetime()
c
c     Returns the system date and time
c	Format: Day-of-week, Month, Day, Hour:Min:Sec Year.
c
      character*24 fdate
c
      zdatetime = fdate()
c
      return
      end
c
c-----------------------------
c
      character*16 function zdate()
c
c     Returns the system date
c
      character*24 fdate, temp
c
      temp = fdate()
      zdate = temp(1:11) // temp(21:24)
c
      return
      end
c
c-----------------------------
c
      character*8 function ztime()
c
c     Returns the system time
c
      character*24 fdate, temp
c
      temp = fdate()
      ztime = temp(12:19)
c
      return
      end

