proc getbits(a)
array bits(0 to 31)
scalar i
for i = 31 to 0 by -1
    if a/(2**i) >= 1 then
    	print a, i
    	bits(i) = 1
    	a = a - (2**i)
    else
    	bits(i) = 0
    end; end   
return
finish
