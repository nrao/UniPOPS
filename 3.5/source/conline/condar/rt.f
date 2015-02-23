      real FUNCTION RT(FREQ,T)
C
C-----------------------------------------------------------------------
C @(#)rt.f	5.1 06/22/94
C-----------------------------------------------------------------------
C  THIS FUNCTION SUBPROGRAM CALCULATES THE EFFECTIVE RADIATION TEMP.
C  OF A BLACKBODY AT TEMP. T AND FREQUENCY FREQ.
c
      real freq, t, hkghz, xx
c
      HKGHZ = 0.04799275
      XX = HKGHZ*FREQ
      RT = XX/(EXP(XX/T) - 1.)
      RETURN
      END


