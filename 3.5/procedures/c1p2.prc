procedure c1p2
#  Averages the first filter bank, second polarization.
sclear
# The first argument to pstack is the FB number, the 2nd the polz. no.
pstack(1,2)
ave all page show
print '       C1P2:'; tell stack
return
finish
