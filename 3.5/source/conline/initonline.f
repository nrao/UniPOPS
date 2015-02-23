      subroutine initonline
C-------------------------------------------------------------------------------
C  @(#)initonline.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Initializes ONLINE data file.   
c
c***************************************
c 8904 [RJM]
c***************************************
c
c
      include 'cio.inc'
c
      integer*2 itry
      integer*4 ithree, long, useprojcode
      character*8 project, tucproj
      logical*2 changeproj
c
      data ithree/3/
      data tucproj /"tucson "/
c
      if (useprojcode() .ne. 0) then
         itry = 0
1390     write(istdout,13909) ' '
13909    format(1x,10a)
c
         write(istdout,13910)'Enter Project Code ',
     .		'(3 CHARS MIN, 8 MAX, Default: NULL): '
13910    format(1x,2a,$)
         call flush(long(istdout))
         read(istdin,13911) project 
13911    format(a)
	 if (project .eq. "        ") project = "NULL"
         if (.not. changeproj(project) ) then
            if (itry .le. 10) then
	       itry = itry + 1
	       goto 1390
            else
	       write(istderr,13909) 
     1               'You have had ten tries... TERMINATING!!'
               call exitpops(ithree)
            endif
         endif
      else
         itry = changeproj(tucproj)
      endif
c
      return
      end
c
