procedure cb1
#  Averages both polarizations of parallel mode data for 1st FB.
sclear
# The first argument to pstack is the FB number, the 2nd the polz. no.
pstack(1,1)
pstack(1,2)
ave all page show
# print '       CB1:'; tell stack
return
finish
