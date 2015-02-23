proc grand
# Returns a normaly distributed random number with zero mean and 
# unit variance,
#
scalar v1, v2, r, fac, gset, iset
#
if iset = 0 then
  v1 = rand; v2 = rand
  r = v1**2 + v2**2
  while r >= 1
  	v1 = rand; v2 = rand
	r = v1**2 + v2**2
	end
  fac = sqrt(-2.*log(r)/r)
  gset = v1*fac
  iset = 1
  return v2*fac
else
  iset = 0
  return gset
end
finish

	
