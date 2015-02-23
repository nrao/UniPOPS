      subroutine tellout (scans, numscans, ldoblank, prefix,
     +                    istype, numout)
c-------------------------------------------------------------------------
c @(#)tellout.f	5.1 06/22/94
c
c    This is the subroutine that actually formats the output for the tell
c    command.
c
c   scans(*) real*4  the array holding the list of scan numbers 
c   numscans integer*4 the number of scans in the array scans(*)
c   ldoblank logical*2 true if scan numbers of 0 are to be represented
c               in the output as blank space.  Used on the SAVE file.
c		Usefull for any file where the location of the scan within
c		the file needs to be known.
c   prefix character*1 the initial character to put on the line 
c   istype integer*4 if = 1 than the scans are really integers and should
c		   be treated as such, with 10 per line
c   numout integer*4 Return value: number of scans written out (numout)
c--------------------------------------------------------------------------
c
      integer*4 numscans, numout, ifirst, ilast, i, istype,
     1          nline, nsize, i1
      integer*2 n72
      logical*2 ldoblank, ldoint
      real*4 scans(*)
      character*1 prefix
c
      include 'cio.inc'
c
      data n72 /72/
c			initialize things
      numout = 0
c			nline = number of scans per line of output
c			nsize = size of each field
      nline = 7
      nsize = 9
      ldoint = .false.
      if (istype .eq. 1) then
         nline = 10
         nsize = 6
         ldoint = .true.
      end if
      do 100 i = 1, numscans
         if (scans(i) .ne. 0) then
            numout = numout + 1
            ifirst = mod(numout-1,nline) * nsize + 10
            ilast = ifirst + nsize - 1
            if (ldoint) then
               write(cpuf(ifirst:ilast), 1005) int(scans(i))
            else
               write(cpuf(ifirst:ilast), 1000) scans(i)
            end if
         else if (ldoblank) then
            numout = numout + 1
            ifirst = (mod(numout-1,nline) + 1) * nsize + 1
            ilast = ifirst + nsize - 1
            do 110 i1 = ifirst, ilast
               write(cpuf(i1:i1), 1010)
 110        continue
         end if
         if (mod(numout,nline) .eq. 0) then
            write(cpuf(1:9),1020) prefix, numout - nline + 1
            call pwrite(cpuf,n72)
         end if
 100  continue
c
      if (mod(numout,nline) .ne. 0) then
         ifirst = mod(numout,nline) * nsize + 10
         cpuf(ifirst:72) = ' '
         write(cpuf(1:9),1020) prefix, numout - mod(numout,nline) + 1
         call pwrite(cpuf,n72)
      end if
c
      return
 1000 format(f9.2)
 1005 format(i6)
 1010 format(' ')
 1020 format(a1,x,i6,':')
      end
             

      
