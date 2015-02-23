procedure con_pstack (oset)
#  Condar pstack;  Accumulates scans.
scalar i, oset
for i = 1 to acount
   get (newfeed(astack(i),(1 + oset)))
#	All the work happens in prcstack, found in con_prcstack.prc
   prcstack
end
return
finish
