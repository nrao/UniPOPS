      subroutine gtbyte (data,iword,nbyte)
C-------------------------------------------------------------------------------
C  @(#)gtbyte.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C	 GTBYTE	extracts a character (byte) from IWORD and returns the
C     character	in DATA	right justified	with zero fill.
C     NBYTE = 0	=> left	char,  = 1 => right char.
c Modified 8903 [RJM] See CHANGES.DOC
C-----------------------------------------------------------------------
C     For byte machines (PDP 11 series) NBYTE = 1 =>left;  0 => right.
C-----------------------------------------------------------------------
c
      integer*2 data,iword
      integer*2 nbyte
c
      if(nbyte.eq.1) data = mod(iword, 256)
      if(nbyte.eq.0) data = iword / 256
c
      if (data.ge.97 .and. data.le.122) data = data - 32
c     Changes Lower Case to Upper
c
      if (data .le. 31 .or. data .ge. 127) data = 32
c     Replaces all non alpha-numeric character to a space.  Eliminates
c     tabs, nulls, esc, etc.  
c
      return
      end
