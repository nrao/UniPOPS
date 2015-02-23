proc getscan(gets_on)
# Places the GETS_ON scan into Array(0); if it is a total power scan, then
# places the off scan into Array (1) and does a TEMP.
#
scalar gets_fd
scalar gets_off
#
on gets_on
gets_fd = mod(gets_on, 1.)
gets_off = ifix(h0(offscan)) + gets_fd
if gets_off > 1 then; off(gets_off); temp; end
return
finish
