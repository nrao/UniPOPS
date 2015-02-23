      SUBROUTINE PROMPT	(ICHAR,IPT)
C-------------------------------------------------------------------------------
C  @(#)prompt.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C	 PROMPT	stores the right character of ICHAR into the left byte
C     of the prompt variable IPT.
C--------------------------------------------------------------- Modcomp
      integer*2 ichar, ipt, it
      integer*2 n0, n1
      data n0, n1 /0, 1/
c
      CALL GTBYTE (IT,ICHAR,n1)
      CALL PTBYTE (IT,IPT,n0)
      RETURN
      END
