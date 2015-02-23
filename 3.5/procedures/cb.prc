procedure cb
#  Averages both IFs.
sclear
#  The first argument to pstack is the Spectrometer number, 
#  the 2nd the polarization number.
pstack(1,1)
if (spectype = hctype) then
  pstack(1,2)
else
  pstack(2,1)
end
ave all page show
# print '       CB:'; tell stack
return
finish
