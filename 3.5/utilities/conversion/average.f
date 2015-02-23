      logical function average(lnew)
c
c     @(#)average.f	5.1 06/22/94
c
c     Creates (Sig-Ref)*Tref/Ref for switched data in cvtbuff.
c     For both TPOWER and Switched data, averages cvtbuff into
c     outbuff.
c
      real rweight(4), sweight(4), ffactor(4)
      logical lnew
      integer*2 j, i
      real*4 effint(4), swnext, rwnext, swnew, rwnew
c
      save rweight, sweight, ffactor
c
      include 'cvttele.inc'
c
      average = .false.
c
      if (icvtbuff(ktos) .eq. 1) then
c     (i.e., Switched power scan)
c
        do 200 j = icvtbuff(kir), min(4,icvtbuff(klr)), icvtbuff(kin)
	  do 100 i = icvtbuff(kst+j), icvtbuff(ksp+j)
	    if (rcvtbuff(i+lrs) .ne. 0. ) then
		rcvtbuff(i+lss) = (rcvtbuff(i+lss) - rcvtbuff(i+lrs)) * 
     1				   rcvtbuff(j+lstp) / rcvtbuff(i+lrs)
	    else
		rcvtbuff(i+lss) = 1.e30
	    endif
100	    continue
c
c         Creates (Sig-Ref)*Tref / Ref if Switched Powers scan and stores
c         results over the old Sig values.  Prevents possible overflow.
c
c         NOTE: Sig and Ref part of Total power scans are already averaged 
c         when they hit the telescope tape.
c
200	  continue
c
      endif
c
      if (lnew) then
c	(i.e., a new scan so we can overwrite previous outbuff and skip
c	any averaging.)
c
	do 50 i = 1, outbuffsize/4
		routbuff(i,1) = rcvtbuff(i)
50      	continue
c
        do 60 j = icvtbuff(kir), min(4,icvtbuff(klr)), icvtbuff(kin)
	  effint(j) = rcvtbuff(lint+j)
	  if (rcvtbuff(lstp+j).le.0. .or. rcvtbuff(ltrf+j).le.0.) then
	     ffactor(j) = 1.
	     return
	  else
	     ffactor(j) = rcvtbuff(lrms+j) * sqrt(rcvtbuff(lint+j)) /
     .			rcvtbuff(lstp+j)
             if (icvtbuff(ktos) .eq. 1) then
	         sweight(j) = effint(j) / 
     .		     ( (rcvtbuff(j+lstp)**2 + rcvtbuff(j+ltrf)**2)/2.)
	     else
	         sweight(j) = effint(j) / (rcvtbuff(lstp+j) ** 2)
	     endif
	     rweight(j) = effint(j) / (rcvtbuff(ltrf+j) ** 2)
	  endif
60        continue
	routbuff(lstm,1) = rcvtbuff(lsrt)
c       Resets outbuff and the weights if a new scan has just been read in and
c	if system temps make sense.  Skip record if Sys temp do not make sense.
c	Clear out Duration Time.
c
      else
c	(i.e., there is data for the same scan in outbuff then we must average)
c
        do 400 j = icvtbuff(kir), min(4,icvtbuff(klr)), icvtbuff(kin)
c
	  if (rcvtbuff(lstp+j).le.0. .or. rcvtbuff(ltrf+j).le.0.) then
	     return
c	     Skip this record if sys. temp make no sense
	  else
             if (icvtbuff(ktos) .eq. 1) then
	         swnext = effint(j) / 
     .		     ( (rcvtbuff(j+lstp)**2 + rcvtbuff(j+ltrf)**2)/2.)
	     else
	         swnext = effint(j) / (rcvtbuff(lstp+j) ** 2)
	     endif
	     swnew = sweight(j) + swnext
	     rwnext = effint(j) / (rcvtbuff(ltrf+j) ** 2)
	     rwnew = rweight(j) + rwnext
c	     Calculate necessary weight factors
c	     *WNEW = sum of previous weight factors + present wieght factors
c            *WNEXT = weight factor for present record
c            *WIEGHT = sum of previous weights.  The r and s signify reference
c	              and signal
c
	     routbuff(lint+j,1) = routbuff(lint+j,1) + effint(j) 
c	     Update effective integration time
c
	     do 300 i = icvtbuff(kst+j), icvtbuff(ksp+j)
	        routbuff(lss+i,1) = ( routbuff(lss+i,1)*sweight(j) + 
     1		                     rcvtbuff(lss+i)*swnext ) / swnew
300             continue
c	     Average data with Int/Tsys**2 weighing
c
             routbuff(lstp+j,1) = sqrt( routbuff(lint+j,1) / swnew)
	     routbuff(ltrf+j,1) = sqrt( routbuff(lint+j,1) / rwnew)
c	     Average sys temps [see memo by RJM dated 1/22/92].
c
	     routbuff(lrms+j,1) = ffactor(j)* routbuff(lstp+j,1) / 
     .					sqrt(routbuff(lint+j,1) )
c	     Update rms
c
	     sweight(j) = swnew
	     rweight(j) = rwnew
c	     Update weight factors for the next record in scan, if one exists
	  endif
c
400       continue
	  routbuff(lstm,1) = routbuff(lstm,1) + rcvtbuff(lsrt)
c	  Update Duration time.
c
      endif
c
      average = .true.
c
      return
c
      end
c
