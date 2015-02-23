      subroutine table
C-------------------------------------------------------------------------------
C  @(#)table.F	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c    Prints out a table of data values.
c
C-------------------------------------------------------------------------------
c
      character*80 cpuf
      real*4 scan 
      integer*2 iptwh, ipuf(40), ier, ierr, lxa1
      integer*2 m2, n80, n288, n0, n272
      integer*4 istart, istop, i, lj, lm
      double precision chantox
      logical okarray, okreal8
c
#ifdef CONTINUUM
#else
      real*8 fib, dosb, nrao12m
      integer*4 iosb(2)
      equivalence (dosb, iosb(1))
      data nrao12m /'NRAO 12M'/
#endif
c
      include 'cform.inc'
      include 'appl.inc'
      include 'core.inc'
c
      equivalence (ipuf, cpuf)
c
      data m2, n80, n288, n0, n272 /-2, 80, 288, 0, 272/
c
c*****************************************
c
      iptwh = 1
c
c
      ISTART=DTWH(C12SPN,IPTWH)+BDROP+IDATOFF
      ISTOP = DTWH(C12SPN,IPTWH) + DTWH(C12NI,IPTWH) + IDATOFF - 
     1        1 - EDROP
      if (istart .ge. istop) call oerror(n288, m2, 'TABLE')
c
      if (okreal8(dtwh(c1sno,iptwh))) then
         SCAN=DTWH(C1SNO,IPTWH)
c
#ifdef CONTINUUM
         WRITE(CPUF,209,IOSTAT=IER) SCAN
209      format(10x,f8.2)
         CALL PWRITE(IPUF,n80)
#else
         fib=abs(DTWH(C12FR,IPTWH))
         if (dtwh(c1tel,iptwh) .eq. nrao12m) then
            dosb = dtwh(c10pv, iptwh)
            write(cpuf,2091,iostat=ier) scan, fib, iosb
2091        format(10x,f8.2,5x,'Freq. Res.',1pg18.10,' MHz',x,a4)
         else
            WRITE(CPUF,209,IOSTAT=IER) SCAN,fib
209         format(10x,f8.2,5x,'Freq. Res.',1pg18.10, ' MHz')
         endif
         CALL PWRITE(IPUF,n80)
#endif
c
      endif
c     Write out header if the array contains telescope data
c
      lxa1 = lxa
      if (.not. okarray(lxa, dtwh(1,iptwh)) ) then
         call oerror(n272, n0,
     .		 'TABLE: Units of X-axis will be channels or samples')
	 lxa1 = 1
      endif
c
#ifdef CONTINUUM
      if (lxa1 .eq. 1) then
	 WRITE(CPUF,210,IOSTAT=IER) 
210      format(6x, 3('Samples  Temperature ') )
      else if (lxa1 .eq. 2 .or. lxa1 .ge. 8) then
	 WRITE(CPUF,212,IOSTAT=IER) 
212      format(6x, 3('  Time   Temperature ') )
      else
	 WRITE(CPUF,211,IOSTAT=IER) 
211      format(6x, 3('  Pos.   Temperature ') )
      endif
#else
      if (lxa1 .eq. 1) then
	 WRITE(CPUF,210,IOSTAT=IER) 
210      format(6x, 3('Channels Temperature ') )
      else if (lxa1 .eq. 2 .or. lxa1 .ge. 8) then
	 WRITE(CPUF,211,IOSTAT=IER) 
211      format(6x, 3('  Freq.  Temperature ') )
      else
	 WRITE(CPUF,212,IOSTAT=IER) 
212      format(6x, 3('  Vel.   Temperature ') )
      endif
#endif
c
      CALL PWRITE(IPUF,n80)
c
c
      ierr = 0
      do 30 i = istart, istop, 3
	  lj = min(istop, i+2)
c
          if (lxa1 .eq. 1) then
	     WRITE(CPUF,3000,IOSTAT=IER) (float(lm-idatoff), 
     .				twh(lm, iptwh), lm = i, lj)
3000         format(5x,':',1p6g11.3)
	  else
	     WRITE(CPUF,3001,IOSTAT=IER) i-idatoff, 
     1         (chantox(lxa1, dtwh(1,iptwh), float(lm-idatoff), ierr), 
     2         twh(lm, iptwh), lm = i, lj)
3001         format(i5,':',1p6g11.3)
	  endif
c
          CALL PWRITE(IPUF,n80)
   30     CONTINUE
c	  Write out data values and an appropriate x-axis value.
c
99    continue
      return
c
      end
c
