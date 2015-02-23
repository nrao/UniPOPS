procedure c1p1
#  Averages the first filter bank, first polarization.
sclear
# The first argument to pstack is the FB number, the 2nd the polz. no.
pstack(1,1)
ave all page show
print '       C1P1:'; tell stack
return
finish
