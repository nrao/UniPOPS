c     CVTELE.INC
c
c     @(#)cvttele.inc	5.1 06/22/94
c
c     Used by the routines for converting between a telescope tape and
c     the IEEE KEEP format.
c
      character*9600 cinbuff
      character*9600 ccvtbuff
      character*5120 coutbuff(18)
c
      integer*2 ioutbuff(2560,18), iinbuff(4800), icvtbuff(4608)
      real routbuff(1280,18), rinbuff(2400), rcvtbuff(2304)
      double precision doutbuff(640,18), dinbuff(1200), dcvtbuff(1152)
      integer*2 lsno, ktos, kstc, kmd, kir, klr, kin, kst, ksp, lstm, 
     .		lsrt, lstp, lint, ltrf, lss, lrs, ktosin, lrho, lthe, 
     .		lnt, lscle, lstm2, kdb, kstart, kstop, kmpst, kmpstp, 
     .		kfeed, lrhoout, ltheout, lntout, lscleout, lstm2out, 
     .		ldbout, lrms
c
c     *inbuff = input record (Modcomp binary)
c     *cvtbuff = binary converted input record.  Some items are
c		 skipped over in inbuff to create cvtbuff.
c     *outbuff = averaged records.  Sig and Ref are combined in outbuff for
c		 Spectral line scans or data are added together for continuum
c		 scans.  It is the output buffer
c     Four versions of the these arrays exist: Char, Int, Real., D Prec.
c
      integer*2 outbuffsize, cinbuffsize, linbuffsize
c
      common /editor/ coutbuff, ccvtbuff, cinbuff
c
      equivalence (cinbuff, iinbuff), (cinbuff, rinbuff)
      equivalence (rinbuff, dinbuff)
      equivalence (ccvtbuff, icvtbuff), (ccvtbuff, rcvtbuff)
      equivalence (rcvtbuff, dcvtbuff)
      equivalence (coutbuff, ioutbuff), (coutbuff, routbuff)
      equivalence (routbuff, doutbuff)
c
      data outbuffsize/5120/, linbuffsize/9600/, cinbuffsize/4800/
c     Record sizes of the two types of input arrays and the output arrays
c
c     Below are the locations in the various buffers of the necessary header
c     words.
c     k... = integer words, l... = real words.
c     Items described with a star after their names are one less than
c	the actual values.
c
      data lsno/ 1/, ktos/34/, kstc/35/, kmd/36/, kir/37/, klr/38/,
     1     kin /39/
c     Locations in cvtbuff and outbuff where necessary items are contained. 
c     These are common to both line and continuum records.
c     lsno = scan no.      ktos = obs. program     kstc = scan type
c     kmd = MODE           kir = IREC              klr = LREC
c     kin = INCR (spectral line)  or Samples/record (cont)
c 
      data kst /39/, ksp /43/, lstm/27/, lsrt/29/, lstp/97/,
     1     lint/113/, ltrf/232/, lss/256/, lrs/1280/, lrms/121/
c     Location in cvtbuff of items specific to line records.  Except for
c     LRS, these are common to line outbuff too.
c     kst* = ISTART(4)	   ksp* = ISTOP(4)
c     lstm = scan duration lsrt = int. time	   lstp* = Sig.Sys.temp.(4)
c     lint* = int time(4)  tref* = Ref.Sys.temp(4) lss* = Sig. Spec values
c     lrs* = Ref. Spec values  lrms* = rms values
c
      data ktosin/34/
c     Locations in inbuff.   ktosin = obs. program
c
      data lrho/63/,lthe/71/, lnt/79/, lscle/87/, lstm2/91/, kdb/198/,
     1     kstart/40/, kstop/41/, kmpst/42/, kmpstp/43/, kfeed/53/
c     Locations in cvtbuff specific to continuum records.  
c     lrho* = RHO value       lthe*, = THETA     lnt* = Noise Tube 
c     lscle* = scale factor   lstm2* = DCR Sys temp.
c     kdb* = start data point  kstart = START    kstop = STOP
c     kmpst = map start       kmpstp = map stop
c     kfeed = feed number 
c
      data lrhoout/64/,ltheout/65/,lntout/66/,lscleout/67/,lstm2out/68/,
     1     ldbout/87/
c     lrhoout = RHO        ltheout = THETA    lntout = Noise Tube
c     lstm2out = Sys Temp  lscleout = Scale Factor 
c     ldbout* = starting data point
