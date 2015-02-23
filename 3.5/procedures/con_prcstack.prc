procedure prcstack
#	this does the work for pstack and cstack, changes in how a scan is
#	processed during an ACCUM should be placed here, probably
#	before the FHIFT is used to calculate the shift applied by
#	SHIFT, e.g. you want to apply OSB to each scan during an ACCUM.
switched; avgd; accum
return
finish
