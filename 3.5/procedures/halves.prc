procedure halves
#   Average the two polarization channels of a parallel FB scan.
#   The two scans are presumed to be in data arrays D3 and D4.
scalar scan1, scan2, nscan
sclear
scan1 = min(h3(fscan), h4(fscan))
scan2 = max(h3(lscan), h4(lscan))
nscan = h3(nostac) + h4(nostac)
copy(3,0)
accum
copy(4,0)
accum ave 
h0(fscan) = scan1
h0(lscan) = scan2
h0(nostac) = nscan
page show
return
finish
