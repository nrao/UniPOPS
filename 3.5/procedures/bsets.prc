PROCEDURE BSETS(NRX)
#    Defines baseline parameters, and stores into BLINESEGS array
global ARRAY BLINESEGS(8,32)
#    Enough for 8 receivers, 32 parameters (=16 baseline pair segments per rx)
#    NRX is the number of receiver being fitted.
#    Global variable NSEG defines how many baseline segments for this rx,
#    carried over from the BSET call
global scalar nseg
scalar i
BSET
FOR I = 1 TO NSEG*2
  BLINESEGS(NRX,I)=NREGION(I)
END
if (nseg >= 16) then; return; end
FOR I = NSEG*2+1 TO 32
  BLINESEGS(NRX,I)=0.
END
RETURN
FINISH
