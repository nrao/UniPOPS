      SUBROUTINE BSHAPE (ierr)
C-------------------------------------------------------------------------------
C  @(#)bshape.f	5.2 03/06/95
C-------------------------------------------------------------------------------
C-------------------------------------------------------------------
C Changed includes to .inc and lowercase 890105 [PPM]
c Modified 8904 [RJM] See CHANGES.DOC
C-------------------------------------------------------------------
      integer*2 ip, ierr, iop, iposn, i, istch(40), ier
      integer*2 n80
      real*4 xd, x0, eps, eta
      character*80 stch
c
      INCLUDE 'appl.inc'
      include 'core.inc'
c
      equivalence (istch, stch)
c
      data n80 /80/
c
      ierr = 0
      IP=IFIX(NFIT)+1
      CALL APCH (XDATA,YDATA,IBASE,IP,XD,X0,WORK,IER)
c
      if (ier .ne. 0) then
	ierr = -1
	return
      endif
c
      EPS=1.0E-6
      eta=0.0
      IOP=-1
      CALL APFS (WORK,IP,IRES,IOP,EPS,ETA,IER)
c
      if (ip .ne. ires) then
	write(stch,9000) "You are losing numerical significance through"
9000	format(a)
	call pwrite(istch,n80)
	write(stch,9001) "using a value of NFIT larger than", ires-1
9001	format(a,i4)
	call pwrite(istch,n80)
      endif
c
      IPOSN=((IRES-1)*IRES)/2
      DO 10 I=1,IRES
  10     BPARM(I)=WORK(I+IPOSN)
      BPARM(14)=X0
      BPARM(15)=XD
      RETURN
      END
