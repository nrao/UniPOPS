      SUBROUTINE TIPSLV (NPTS, TAMB, TCAL, T, secz, tau, trcvr)
C-----------------------------------------------------------------------
C @(#)tipslv.f	5.1 06/22/94
C-----------------------------------------------------------------------
C
C        This program uses telescope tipping scan data to determine
C        zenith extinction and receiver temperature.
C-----------------------------------------------------------------------
C
      character*80 stch
      integer*2 NPTS, NANGLE, istch(40), j, nptsm1, n80
      DOUBLE PRECISION SECZ(*), T(*), TAMB, TCAL, flnpts, sumon,
     .		       sumoff, calfct, tau, trcvr
C
      equivalence (istch, stch)
c
      data n80 /80/
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
      NANGLE = NPTS / 3
      NPTSM1 = NPTS - 1
      FLNPTS = DBLE (FLOAT (NPTS))
c
C                   Find counts-to-temperature conversion factor
      SUMON = 0.D0
      DO 2000 J = 2, NPTSM1, 3
         SUMON = SUMON + T(J)
 2000    CONTINUE
      SUMOFF = 0.D0
      DO 3000 J = 3, NPTS, 3
         SUMOFF = SUMOFF + T(J - 2) + T(J)
 3000    CONTINUE
C
C                   Convert input data to temperature scale
      CALFCT = (2.D0 / 3.D0 * FLNPTS) * TCAL / (2.D0 * SUMON - SUMOFF)
      DO 4000 J = 1, NPTS
         T(J) = CALFCT * T(J)
 4000    CONTINUE
C
C                   Solve for extinction and rcvr temp
C                   using linearized least squares routine
      CALL SOLVE (NPTS, SECZ, T, TAMB, TCAL, tau, trcvr)
C
99999 RETURN
      END
