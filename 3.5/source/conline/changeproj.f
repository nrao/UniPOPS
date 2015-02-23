      logical*2 function changeproj(project)
C-------------------------------------------------------------------------------
C  @(#)changeproj.f	5.1 06/22/94
C-------------------------------------------------------------------------------
cc
c     Checks and changes the project code to PROJCODE
c     PROJCODE = C*8 string containing the project code. 
c 
c     Returns TRUE if the user has permission to access the data with the
c     specified project code EVENTHOUGH online data maybe unavailable
c
      include 'cio.inc'
      include 'core.inc'
c
      integer*2 lastblnk
      character*8 project, projin, coobs, csite
      real*8 dprojin, doobs, dsite
      integer*4 irtn, onlinesite, qvers, hctype, fbtype, contype, 
     .          sddfmt
      logical first
c
      equivalence (projin, dprojin), (coobs, doobs), (csite, dsite)
c
      save projin, first
c
      data first/.true./
      data hctype, fbtype, contype /1,2,3/
      data sddfmt /0/
c
      changeproj = .false.
      csite = site
      coobs = oobs

      if (onlinesite() .eq. -1) then
c			no on-line data is available
         online = .false.
         first = .false.
         changeproj = .true.
         goto 99
      endif
c        
      if (project .ne. 'tucson  ') then
         if (lastblnk(project) .lt. 3) then
	    write(istderr,*) 'Project code is less than 3 characters!'
	    goto 99
         endif
c
         projin = project      
         if ((projin .ne. 'operatio') .and. (onlinesite() .ne. 1)) 
     .            call uppercase(project, projin)
c        Makes sure project code is in uppercase but only if not 'operation'
c        and not tucson online data.
      else
	projin = project
      endif
c
      if (projin .ne. 'NULL    ') then
	if (first) then
      	  call openaccess(dprojin, doobs, dsite, irtn)
        else
	  call chgprj(dprojin, doobs, dsite, irtn)
        endif
      endif
c     IRTN would zero if the user is in the securefile and has
c     authorization to access that project's data and if call(s) are succesfull.  
c     IRTN < 0 if not in securefile, > 0 if can't access on-line data for any
c     other reason.  Must call them using R*8 instead of C*8.
c
      if (projin .eq. 'NULL    ' .or. projin .eq. 'tucson  ') then
	 online=.false.
      else if (irtn .lt. 0) then
         write(istderr,*) 'Data with project code ', project,
     1                        ' cannot be accessed by ', oobs
	 write(istderr,*) 'Try again ....'
         online = .false.
	 goto 99
      else if (irtn .ge. 80000)  then 
         write(istderr,*) 'Security File is not available!! ',
     .			  'Contact the telescope''s computer staff'
         online = .false.
      else if (irtn .eq. 0) then
         first = .false.
	 online = .true.
      else
         write(istderr,109) '***** WARNING: Online data is not ',
     1			     'available.  IRTN = ', irtn,' *****'
109      format(1x,a,a,i6,a)
         online = .false.
      endif
c
c			set format to SDD, anything else turn online off
      if (online) then
         call setformat(sddfmt, irtn)
         if (irtn .ne. 0) then
            call closeaccess
            online = .false.
            goto 99
         endif
      endif
c
      projcode = projin
      changeproj = .true.
      hcver = qvers(hctype)
      fbver = qvers(fbtype)
      conver = qvers(contype)
c
c     Even if ONLINE data is not available, the user has typed in an authorized
c     project code so we should let him/her go on with the program.  Hence,
c     changeproj should be set to true.
c
99    return
      end
c
