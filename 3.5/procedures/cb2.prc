procedure cb2
#  Averages both polarizations of parallel mode data for 2nd FB.
sclear
# The first argument to pstack is the FB number, the 2nd the polz. no.
pstack(2,1)
pstack(2,2)
ave all page show
# print '       CB2:'; tell stack
return
finish
