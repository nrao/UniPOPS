# Functions for performing basic Math operations on Matrices 0 and 1
#

proc mplus
# Adds Matrix 0 and 1 and places results in 0
scalar i, j, undef0, undef1
undef0=mh0(mundef)
undef1=mh1(mundef)
for i = 1 to mh0(mnaxis1)
   for j = 1 to mh0(mnaxis2)
     if md0(i,j) ~= undef0 & md1(i,j) ~= undef1 then
	md0(i,j) = md0(i,j) + md1(i,j)
 	end
     end
   end
return
finish

proc mminus
# Subtracts Matrix 1 from 0 placing results in 0
scalar i, j, undef0, undef1
undef0=mh0(mundef)
undef1=mh1(mundef)
for i = 1 to mh0(mnaxis1)
   for j = 1 to mh0(mnaxis2)
     if md0(i,j) ~= undef0 & md1(i,j) ~= undef1 then
	md0(i,j) = md0(i,j) - md1(i,j)
 	end
     end
   end
return
finish

proc mmultiply
# Multiplies Matrix 0 by 1 and places results in 0
scalar i, j, undef0, undef1
undef0=mh0(mundef)
undef1=mh1(mundef)
for i = 1 to mh0(mnaxis1)
   for j = 1 to mh0(mnaxis2)
     if md0(i,j) ~= undef0 & md1(i,j) ~= undef1 then
	md0(i,j) = md0(i,j) * md1(i,j)
 	end
     end
   end
return
finish

proc mdivide
# Divides Matrix 0 by 1 and places results in 0
scalar i, j, undef0, undef1
undef0=mh0(mundef)
undef1=mh1(mundef)
for i = 1 to mh0(mnaxis1)
   for j = 1 to mh0(mnaxis2)
     if md0(i,j) ~= undef0 & md1(i,j) ~= undef1 then
	md0(i,j) = md0(i,j) / md1(i,j)
 	end
     end
   end
return
finish

proc mscale(a1)
# Multiplies Matrix 0 by A1
scalar i, j, undef
undef=mh0(mundef)
for i = 1 to mh0(mnaxis1)
   for j = 1 to mh0(mnaxis2)
     if md0(i,j) ~= undef then
	md0(i,j) = a1*md0(i,j)
 	end
     end
   end
return
finish

proc mbias(a1)
# Removes a DC offset A1 from Matrix 0
scalar i, j, undef
undef=mh0(mundef)
for i = 1 to mh0(mnaxis1)
   for j = 1 to mh0(mnaxis2)
     if md0(i,j) ~= undef then
	md0(i,j) = md0(i,j) + a1
 	end
     end
   end
return
finish

