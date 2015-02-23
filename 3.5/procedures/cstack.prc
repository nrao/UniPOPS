procedure cstack
#	averages the scans in the stack and shows the result
scalar i
sclear
for i = 1 to acount
   get astack(i)
   prcstack
end
ave; all; page; show
return
finish
