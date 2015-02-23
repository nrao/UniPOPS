      SUBROUTINE OPENMS(LUN,NAME,IRDO,IER)
C-------------------------------------------------------------------------------
C  @(#)openms.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C        IRDO=1     OPEN SHARED, WRITE ACCESS
C        IRDO=0     OPEN NON SHARED, WRITE ACCESS
C        IRDO=-1     OPEN SHARED, READONLY
C                                               Modified 13 Feb 1987, DTE
c     Modified 8903 [RJM] See CHANGES.DOC
c*****************************************************
C
      INTEGER*2 LUN, ier, inunit, IRECLWORD, name(*), i, istat
      logical*2 opened
      INTEGER*2  IRDO
      character*64 cname
      integer*2 iname(32)
c
      include 'cio.inc'
c
      equivalence (iname(1), cname)
c
      IRECLWORD=IRECSIZ/2
      inunit = LUN
c
      do 10 i = 1, 32
	iname(i) = name(i)
10	continue
c
      if (opened(cname)) return
      open(unit=inunit,file=cname,recl=4*ireclword,form='unformatted',
     1     status='old',access='direct',iostat=istat)
      ier = istat
      RETURN
      END

