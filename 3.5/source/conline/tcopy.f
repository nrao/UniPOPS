      subroutine tcopy
C-------------------------------------------------------------------------------
C  @(#)tcopy.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Makes a hard copy of the text screen
c
      character*90 output
      integer*4 unlink, irtn, system, istat, istat1
      logical*2 inquirefile
      integer*2 n0, n129
c
      data n0, n129 /0, 129/
c
      include 'cio.inc'
c
      if (iprinttype .le. 0) then
	call oerror(n129, n0, 'TCOPY')
	return
      endif
c
      if (inquirefile(couttxt)) irtn = unlink(couttxt)
      open(unit=iouttxt,file=couttxt,status='new',iostat=istat)
      rewind(iouttxt,iostat=istat1)
c
      if (istat .ne. 0 .or. istat1 .ne. 0) then
	write(istderr,*) 'Cannot perform TCOPY... Bad file designation'
	return
      endif
c
      if (iprinttype .eq. 5) then
        call quichardtcopy
      else if (iprinttype .eq. 4) then
	call hphardtcopy
      else if(iprinttype .eq. 3) then
	call posthardtcopy
      else if(iprinttype .eq. 2) then
	call qmshardtcopy
      else if(iprinttype .eq. 1) then
	call noghardtcopy
      endif
c
      output = 'printit ' // printer // ' ' // 
     .            printtype // ' ' // couttxt
c
      close(iouttxt,status='keep',iostat=istat)
      irtn = system(output)
      if (irtn .ne. 0) write(istderr,*) 'Internal problem with TCOPY'
      irtn = unlink(couttxt)
      if (irtn .ne. 0) write(istderr,*) 'Internal problem with TCOPY'
      return
      end
c
