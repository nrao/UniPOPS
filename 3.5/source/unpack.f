      SUBROUTINE UNPACK	(N,KF,KT)
C-------------------------------------------------------------------------------
C  @(#)unpack.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C------------------------------------------------------------------------
C	 UNPACK	unpacks	a tight	to a loose character buffer.  The
C     operation	is defined as X/Y = X/b, Y/b.
c     Modified 8903 [RJM] See CHANGES.DOC
C------------------------------------------------------------------------
      integer*2	KF(*), KT(*), n, iblank, ktp, i, it
      integer*2 n0, n1
      DATA IBLANK /'  '/
      data n0, n1 /0, 1/
      KTP = 1
      DO 10 I =	1, N
	 KT(KTP) = IBLANK
	 CALL GTBYTE(IT,KF(I),n0)
	 CALL PTBYTE(IT,KT(KTP),n0)
	 KTP = KTP + 1
	 KT(KTP) = IBLANK
	 CALL GTBYTE(IT,KF(I),n1)
	 CALL PTBYTE(IT,KT(KTP),n0)
	 KTP = KTP + 1
 10	 CONTINUE
      RETURN
      END
