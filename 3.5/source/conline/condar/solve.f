      SUBROUTINE SOLVE (NPTS, SECZ, T, TAMB, TCAL, tau, trcvr)
C-----------------------------------------------------------------------
C @(#)solve.f	5.1 06/22/94
C-----------------------------------------------------------------------
c
c     Used by TIPSLV for fitting a simple extinction curve; used for
c     Green Bank data alone
C-----------------------------------------------------------------------
c
      real*8 tau, trcvr, tamb, tcal, eps, xtamb, xtau, xsecz, der,
     .       flnpts, sumfnc, sumcrx, dfdtau, res1, fnc, dftau, dftrc, 
     .	     ftrcvr, deltau, deltrc, sumder, sumdsq, sumrsq, sumres,
     .	     fexp, res2, res, det, dtau, dtrcvr, ftau, SECZ(*), T(*)
      character*80 stch
      integer*2 istch(40), iter, j, npts, nptsm1, n80
C
      equivalence (istch, stch)
c
      DATA EPS /1.D-6/
      data n80 /80/
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
      FEXP (XTAU, XSECZ) = DEXP (-XTAU * XSECZ)
      DFDTAU (XTAMB, XTAU, XSECZ) = XSECZ * XTAMB * FEXP (XTAU, XSECZ)
C
      NPTSM1 = NPTS - 1
      FLNPTS = DBLE (FLOAT (NPTS))
      TAU = 0.D0
      TRCVR = T(1)
C
      do 100 iter = 1, 60
c
         SUMFNC = 0.D0
         SUMCRX = 0.D0
         SUMDER = 0.D0
         SUMDSQ = 0.D0
         SUMRSQ = 0.D0
C
         DO 10 J = 3, NPTS, 3
            FNC = TAMB * (1.D0 - FEXP (TAU, SECZ(J / 3))) + TRCVR
            DER = DFDTAU (TAMB, TAU, SECZ(J / 3))
C
            RES1 = T(J) - FNC
            RES2 = T(J - 2) - FNC
            SUMRES = RES1 + RES2
            SUMFNC = SUMFNC + SUMRES
            SUMRSQ = SUMRSQ + RES1 ** 2 + RES2 ** 2
            SUMCRX = SUMCRX + SUMRES * DER
            SUMDER = SUMDER + DER + DER
            SUMDSQ = SUMDSQ + 2.D0 * DER ** 2
 10         CONTINUE
C
         DO 20 J = 2, NPTSM1, 3
            FNC = TAMB*(1.D0 - FEXP (TAU, SECZ(J / 3 + 1)))+TRCVR+TCAL
            DER = DFDTAU (TAMB, TAU, SECZ(J / 3 + 1))
C
            RES = T(J) - FNC
            SUMFNC = SUMFNC + RES
            SUMRSQ = SUMRSQ + RES ** 2
            SUMCRX = SUMCRX + RES * DER
            SUMDER = SUMDER + DER
            SUMDSQ = SUMDSQ + DER ** 2
 20         CONTINUE
C
         DET = FLNPTS * SUMDSQ - SUMDER ** 2
         DTAU = (FLNPTS * SUMCRX - SUMDER * SUMFNC) / DET
         DTRCVR = (SUMFNC * SUMDSQ - SUMDER * SUMCRX) / DET
C
         TAU = TAU + DTAU
         TRCVR = TRCVR + DTRCVR
C
         FTAU = DTAU / TAU
         FTRCVR = DTRCVR / TRCVR
         DFTAU = DABS (FTAU)
         DFTRC = DABS (FTRCVR)
C
         IF (DFTAU .lt. EPS .and. DFTRC .lt. EPS) GO TO 200
c
 100     CONTINUE
c
      WRITE (unit=stch,fmt=800)
800   FORMAT ('*** Iteration limit exceeded ***')
      call pwrite(istch,n80)
C
200   continue
      DELTAU = .75D0 * DSQRT (2.D0 * SUMRSQ / DET)
      DELTRC = .25D0 * DSQRT (SUMDSQ * SUMRSQ / DET)
c
      WRITE (unit=stch,fmt=900)  TAU, DELTAU
900   FORMAT ('Zenith extinction:   ', f10.5, ' +/- ', f10.5)
      call pwrite(istch,n80)
c
      write (unit=stch,fmt=901) trcvr, deltrc
901   FORMAT ('Receiver temperature:', f10.2, ' +/- ', f10.2,' K')
      call pwrite(istch,n80)
c
      RETURN
      END
