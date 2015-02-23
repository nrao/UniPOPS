proc nrset(nrset_i)
# Sets up NREGION parameters for NRSET_I baseline regions
#
scalar nrset_j
#
nregion=0.0
for nrset_j = 1 to 2*nrset_i by 2
  nregion(nrset_j)=ccur
  nregion(nrset_j+1)=ccur
  end
return
finish

