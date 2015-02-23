proc testscn2
# Lists the complete header information in Arrays (0) and (1).
#
scalar totswvar, i_loop, offset
string*60 fmt1, fmt2, fmt3
? '********** ';? 'Class 1'
? 'headlen', h0(headlen), h1(headlen)
? 'datalen', h0(datalen), h1(datalen)
? 'scan', h0(scan), h1(scan)
? 'obsid' h0(obsid), h1(obsid)
? 'observer'  h0(observer), h1(observer)
? 'telescop'  h0(telescop), h1(telescop)
? 'projid'  h0(projid), h1(projid)
? 'object'  h0(object), h1(object)
? 'obsmode'  h0(obsmode), h1(obsmode)
? 'frontend'  h0(frontend), h1(frontend)
? 'backend'  h0(backend), h1(backend)
? 'precis'  h0(precis), h1(precis)
? 'savenum', h0(savenum), h1(savenum)
? 'norecord', h0(norecord), h1(norecord)
? 'recordid', h0(recordid), h1(recordid)
? '********** ';? 'Class 2'
? 'xpoint', h0(xpoint), h1(xpoint)
? 'ypoint', h0(ypoint), h1(ypoint)
? 'uxpnt', h0(uxpnt), h1(uxpnt)
? 'uypnt', h0(uypnt), h1(uypnt)
? 'ptcon', h0(ptcon), h1(ptcon)
? '     ', h0(ptcon+1), h1(ptcon+1)
? '     ', h0(ptcon+2), h1(ptcon+2)
? '     ', h0(ptcon+3), h1(ptcon+3)
? 'orient', h0(orient), h1(orient)
? 'focusr', h0(focusr), h1(focusr)
? 'focusv', h0(focusv), h1(focusv)
? 'focush', h0(focush), h1(focush)
? 'pt_model', h0(pt_model), h1(pt_model)
? '********** ';? 'Class 3'
? 'utdate', h0(utdate), h1(utdate)
? 'ut', h0(ut), h1(ut)
? 'lst', h0(lst), h1(lst)
? 'norchan', h0(norchan), h1(norchan)
? 'noswvar', h0(noswvar), h1(noswvar)
? 'nophase', h0(nophase), h1(nophase)
? 'cycllen', h0(cycllen), h1(cycllen)
? 'samprat', h0(samprat), h1(samprat)
? 'cl11type',h0(cl11type), h1(cl11type)
? 'phaseid', h0(phaseid), h1(phaseid)
? '********** ';? 'Class 4'
? 'epoch', h0(epoch), h1(epoch)
? 'xsource', h0(xsource), h1(xsource)
? 'ysource', h0(ysource), h1(ysource)
? 'xref', h0(xref), h1(xref)
? 'yref', h0(yref), h1(yref)
? 'epocra', h0(epocra), h1(epocra)
? 'epocdec', h0(epocdec), h1(epocdec)
? 'gallong', h0(gallong), h1(gallong)
? 'gallat', h0(gallat), h1(gallat)
? 'az', h0(az), h1(az)
? 'el', h0(el), h1(el)
? 'indx', h0(indx), h1(indx)
? 'indy', h0(indy), h1(indy)
? 'desorg', h0(desorg), h1(desorg)
? '      ', h0(desorg+1), h1(desorg+1)
? '      ', h0(desorg+2), h1(desorg+2)
? 'coordcd' h0(coordcd),  h1(coordcd)
? '********** ';? 'Class 5'
? 'tamb', h0(tamb), h1(tamb)
? 'pressure', h0(pressure), h1(pressure)
? 'humidity', h0(humidity), h1(humidity)
? 'refrac', h0(refrac), h1(refrac)
? 'dewpt', h0(dewpt), h1(dewpt)
? 'mmh2o', h0(mmh2o), h1(mmh2o)
? '********** ';? 'Class 6'
? 'scanang', h0(scanang), h1(scanang)
? 'xzero', h0(xzero), h1(xzero)
? 'yzero', h0(yzero), h1(yzero)
? 'deltaxr', h0(deltaxr), h1(deltaxr)
? 'deltayr', h0(deltayr), h1(deltayr)
? 'nopts', h0(nopts), h1(nopts)
? 'noxpts', h0(noxpts), h1(noxpts)
? 'noypts', h0(noypts), h1(noypts)
? 'xcell0', h0(xcell0), h1(xcell0)
? 'ycell0', h0(ycell0), h1(ycell0)
? 'frame'  h0(frame), h1(frame)
? '********** ';? 'Class 7'
? 'bfwhm', h0(bfwhm), h1(bfwhm)
? 'offscan', h0(offscan), h1(offscan)
? 'badchv', h0(badchv), h1(badchv)
? 'rvsys', h0(rvsys), h1(rvsys)
? 'velocity', h0(velocity), h1(velocity)
? 'veldef'  h0(veldef), h1(veldef)
? 'typecal'  h0(typecal), h1(typecal)
? '********** ';? 'Class 8'
? 'appeff', h0(appeff), h1(appeff)
? 'beameff', h0(beameff), h1(beameff)
? 'antgain', h0(antgain), h1(antgain)
? 'etal', h0(etal), h1(etal)
? 'etafss', h0(etafss), h1(etafss)
if compare(h0(telescop),'NRAO 43M') then
   ? '********** ';? 'Class 9 -- Green Bank'
   ? 'l1' h0(l1) h1(l1)
   ? 'l1f1' h0(l1f1) h1(l1f1)
   ? 'l1f2' h0(l1f2) h1(l1f2)
   ? 'l2' h0(l2)  h1(l2)
   ? 'l2f1' h0(l2f1) h1(l2f1)
   ? 'l2f2' h0(l2f2) h1(l2f2)
   ? 'la' h0(la) h1(la)
   ? 'lb' h0(lb) h1(lb)
   ? 'lc' h0(lc) h1(lc)
   ? 'ld' h0(ld) h1(ld)
   ? 'levcorr' h0(levcorr) h1(levcorr)
   ? 'ptfudge' h0(ptfudge) h1(ptfudge)
   ? '       ' h0(ptfudge+1) h1(ptfudge+1)
   ? 'rho' h0(rho) h1(rho)
   ? 'theta' h0(theta) h1(theta)
   ? 'cfform'  h0(cfform), h1(cfform)
end
if compare(h0(telescop),'NRAO 12M') then 
   ? '********** ';? 'Class 9 -- 12-m'
   ? 'synfreq ', h0(synfreq), h1(synfreq)
   ? 'lofact', h0(lofact), h1(lofact)
   ? 'harmonic', h0(harmonic), h1(harmonic)
   ? 'loif', h0(loif), h1(loif)
   ? 'firstif', h0(firstif), h1(firstif)
   ? 'razoff', h0(razoff), h1(razoff)
   ? 'reloff', h0(reloff), h1(reloff)
   ? 'bmthrow', h0(bmthrow), h1(bmthrow)
   ? 'baseoff', h0(baseoff), h1(baseoff)
   ? 'obstol', h0(obstol), h1(obstol)
   ? 'sideband', h0(sideband), h1(sideband)
   ? 'wl', h0(wl), h1(wl)
   ? 'gains', h0(gains), h1(gains)
   ? 'pbeam', h0(pbeam), h1(pbeam)
   ? '     ', h0(pbeam+1), h1(pbeam+1)
   ? 'mbeam', h0(mbeam), h1(mbeam)
   ? '     ', h0(mbeam+1), h1(mbeam+1)
   ? 'sroff', h0(sroff), h1(sroff)
   ? '     ', h0(sroff+1), h1(sroff+1)
   ? '     ', h0(sroff+2), h1(sroff+2)
   ? '     ', h0(sroff+3), h1(sroff+3)
   ? 'foffsig',h0(foffsig), h1(foffsig)
   ? 'foffref1',h0(foffref1), h1(foffref1)
   ? 'foffref2',h0(foffref2), h1(foffref2)
end
? '********** ';? 'Class 10'
? 'openpar  '  h0(openpar), h1(openpar)
? 'openpar+1'  h0(openpar+1), h1(openpar+1)
? 'openpar+2'  h0(openpar+2), h1(openpar+2)
? 'openpar+3'  h0(openpar+3), h1(openpar+3)
? 'openpar+4'  h0(openpar+4), h1(openpar+4)
? 'openpar+5'  h0(openpar+5), h1(openpar+5)
? 'openpar+6'  h0(openpar+6), h1(openpar+6)
? 'openpar+7'  h0(openpar+7), h1(openpar+7)
? 'openpar+8'  h0(openpar+8), h1(openpar+8)
? 'openpar+9'  h0(openpar+9), h1(openpar+9)
if compare(h0(cl11type),'PROTO12M') then
   ? '********** ';? 'Prototype 12m Class 11'
   ? 'noswvarf', h0(noswvarf), h1(noswvarf)
   ? 'numcyc', h0(numcyc), h1(numcyc)
   ? 'numcycf', h0(numcycf), h1(numcycf)
   ? 'nophasef', h0(nophasef), h1(nophasef)
   ? 'cycllenf', h0(cycllenf), h1(cycllenf)
   ? 'samptimf', h0(samptimf), h1(samptimf)
   totswvar = min(h0(noswvar)+h0(noswvarf),10)
   ? '-------- Displaying ',totswvar,' switching var. info'
   if (totswvar > 0) then
      fmt1 = "(x,a6,i2.2,a2,1pg15.7.2,2x,g15.7.2)"
      fmt2 = "(x,a6,i2.2,a2,a8,2x,a8)"
      fmt3 = "(x,a6,i2.2,a2,a32,2x,a32)"
      for i_loop = 1 to totswvar
         offset = (i_loop - 1) * 6
         fprint(-1,fmt1) "varval",i_loop," ",h0(varval01+offset),h1(varval01+offset)
         fprint(-1,fmt2) "vardes",i_loop," ",h0(vardes01+offset),h1(vardes01+offset)
         fprint(-1,fmt3) "phastb",i_loop," ",h0(phastb01+offset),h1(phastb01+offset)
      end
   end
else
   ? '********** ';? 'Original Class 11'
   totswvar = min(h0(noswvar),22)
   ? '-------- Displaying ',totswvar,' switching var. info'
   if (totswvar > 0) then
      fmt1 = "(x,a10,i2,a2,1pg15.7.2,g15.7.2)"
      fmt2 = "(x,a10,i2,a2,a8,2x,a8)"
      for i_loop = 0 to (totswvar-1)
         offset = i_loop * 3
         fprint(-1,fmt1) " varval + ",i_loop," ",h0(varval+offset),h1(varval+offset)
         fprint(-1,fmt2) " vardes + ",i_loop," ",h0(vardes+offset),h1(vardes+offset)
         fprint(-1,fmt2) " phastb + ",i_loop," ",h0(phastb+offset),h1(phastb+offset)
      end
   end
end
? '********** ';? 'Class 12'
? 'obsfreq', h0(obsfreq), h1(obsfreq)
? 'restfreq', h0(restfreq), h1(restfreq)
? 'freqres', h0(freqres), h1(freqres)
? 'bw', h0(bw), h1(bw)
? 'trx', h0(trx), h1(trx)
? 'tcal', h0(tcal), h1(tcal)
? 'stsys', h0(stsys), h1(stsys)
? 'rtsys', h0(rtsys), h1(rtsys)
? 'tsource', h0(tsource), h1(tsource)
? 'trms', h0(trms), h1(trms)
? 'refpt', h0(refpt), h1(refpt)
? 'x0', h0(x0), h1(x0)
? 'deltax', h0(deltax), h1(deltax)
? 'inttime', h0(inttime), h1(inttime)
? 'noint', h0(noint), h1(noint)
? 'spn', h0(spn), h1(spn)
? 'tauh2o', h0(tauh2o), h1(tauh2o)
? 'th2o', h0(th2o), h1(th2o)
? 'tauo2', h0(tauo2), h1(tauo2)
? 'to2', h0(to2), h1(to2)
? 'polariz' h0(polariz),  h1(polariz)
? 'effint', h0(effint), h1(effint)
? 'rx_info', h0(rx_info), h1(rx_info)
? '********** ';? 'Class 13'
? 'nostac', h0(nostac), h1(nostac)
? 'fscan', h0(fscan), h1(fscan)
? 'lscan', h0(lscan), h1(lscan)
? 'lamp', h0(lamp), h1(lamp)
? 'lwid', h0(lwid), h1(lwid)
? 'ili', h0(ili), h1(ili)
? 'drms', h0(drms), h1(drms)
return
finish
