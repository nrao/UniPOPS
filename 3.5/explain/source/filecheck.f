      program filecheck
c
c        @(#)filecheck.f	5.1 06/22/94
c
c   Verify that the indicated files in the linked list exist
c
      include 'explain.inc'
c
      character*64 infofile
      integer*4 ISTDOUT, ISTDERR
      integer*4 iargc, lnblnk, long, loc, access
c
      parameter (ISTDERR = 0)
      parameter (ISTDOUT = 6)
c
      open(unit=ISTDOUT, form="print")
c
      if (iargc() .lt. 1) then
         write(ISTDERR,*) 'Syntax: filecheck info_file'
         call exit(long(1))
      endif
c			get the info file and open it
      call getarg(long(1), infofile)
      call readinfo(infofile)
c			start at start and make sure each file can be accessed
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) 'Explain file : ',infofile(1:lnblnk(infofile))
      write(ISTDOUT,*) 'The following files can not be accessed:'
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) '  NODE: filename'
      write(ISTDOUT,*) ' '
      loc = start
c
      do 10 while (loc .gt. 0)
         if (access(filenames(loc),' ') .ne. 0) then
            write(ISTDOUT,*) '  ',nodes(loc)(1:lnblnk(nodes(loc))),
     .             ': ',filenames(loc)(1:lnblnk(filenames(loc)))
         endif
         loc = next(loc)
 10   continue
c
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) 'filecheck ends successfully.'
c
      stop
      end
