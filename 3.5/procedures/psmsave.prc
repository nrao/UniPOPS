procedure psmsave(xscan,yscan)
#  Puts a PSM map set into the NSAVE area
#  Assumes PSM mapping.  xscan,yscan the first and last scan #
#  Starts at NSAVE=1.
#                          Darrel Emerson, Jan 1992.
scalar i
if (sprotect = TRUE) then
   print ' SPROTECT must be FALSE'
   return
end
nsave=1
for i = xscan to yscan
  get i
    replace
    baseline
    save
    nsave=nsave+1
end
return
finish
