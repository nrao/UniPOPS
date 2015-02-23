proc dbref
# Takes the contents of Array zero, which is assumed to contain
# Tsys(SIG) * [ SIG - REF ] / REF and converts the data to
# Tsys(REF) * [ REF - SIG ] / SIG.  Alters the value of FACT.
#
# Useful for processing double-beam-switched or frequency-switched 
# data
#
fact = 1/h0(stsys); scale
{ d0@ = (1/(d0@+1))-1 }
fact=h0(rtsys); scale
h0(rtsys) = h0(stsys)
h0(stsys) = fact
#
return
finish

