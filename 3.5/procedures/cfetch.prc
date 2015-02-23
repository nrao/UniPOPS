proc cfetch(cfetch_fd)
# Gets the most current scan and, if it is a Total Power scan
# gets the off scan and does a TEMP.  CFETCH_FD is the feed number
# to get
#
scalar cfetch_off
#
cget cfetch_fd
cfetch_off = h0(offscan)
if cfetch_off > 1 then; off(cfetch_off+cfetch_fd/100); temp; end
return
finish
