      subroutine ptbyte (data,iword,nbyte)
C-------------------------------------------------------------------------------
C  @(#)ptbyte.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C	 PTBYTE	put a character	(byte) into IWORD in either the	right
C     or left byte (NBYTE = 0 => left,	= 1 => right, from DATA
C     that is right justified.
c Modified 8903 [RJM] See CHANGES.DOC
C-----------------------------------------------------------------------
C     For byte machines (PDP 11 series) NBYTE = 1 => left;  0 => right.
C-----------------------------------------------------------------------
c
      integer*2 nbyte
      integer*2 data,iword
c
      character*2 cout, cin
      integer*2 iout, iin
c
      equivalence (cout, iout), (cin, iin)
c
      iin = data
      iout = iword
c
      if(nbyte.ne.0) cout(2:2) = cin(2:2)
      if(nbyte.eq.0) cout(1:1) = cin(2:2)
c
      iword = iout
c
      return
      end
