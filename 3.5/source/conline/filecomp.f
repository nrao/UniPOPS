      subroutine filecomp(filename, fullname)
c------------------------------------------------------------
c
c @(#)filecomp.f	5.1 4
c
c   filename is what the user sees/enters and
c   fullname is what the program should use
c
c------------------------------------------------------------
c
      character*20 tmpname
      character*(*) filename
      character*1023 fullname
      character*4 pidtag
      integer*4 lenpid, i, ii, irtn, system, unlink
      integer*2 lastblnk, m1, n376
      logical lexpand
c
      include 'cio.inc'
c
      data pidtag /'----'/
      data m1, n376 /-1, 376/
c		scan filename for ~, * or $, expand only if found
      lexpand = .false.
      if (index(filename,'~') .eq. 1) lexpand = .true.
c			~ only special to csh if it is first
      if (index(filename,'*') .ne. 0) lexpand = .true.
      if (index(filename,'$') .ne. 0) lexpand = .true.
      if (.not. lexpand) then
         write(fullname,fmt="(a)") filename
         return
      endif
c			if we get here, then we should expand filename
      if (pidtag .eq. '----') then
        lenpid = lastblnk(procid)
        do 10 i = 1, 4
          ii = lenpid - 4 + i
          pidtag(i:i) = '0'
          if (ii .gt. 0) then
            if (procid(ii:ii) .ge. '0' .and. procid(i:i) .le. '9') then
               pidtag(i:i) = procid(ii:ii)
            endif
          endif
 10     continue
      endif
c		make sure that pidtag is set to last 4 chars of procid
      tmpname = "/tmp/filecomp" // pidtag
      irtn = system("filecomp.exe " // filename // " " // tmpname)
      if (irtn .eq. 0) then
         open(file=tmpname, unit=iiotmp)
         read(iiotmp,1000) fullname
         close(iiotmp)
         irtn = unlink(tmpname)
      else
         call oerror(n376, m1, filename)
      endif
c
      return
c
 1000 format(A)
c
      end
