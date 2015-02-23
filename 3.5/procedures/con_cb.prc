procedure con_cb
#  Condar CB  Combine Channel 1 & 2 data
sclear slabel=1; con_pstack(0); con_pstack(1) 
ave page show 
slabel=0; 
place(150,675); graphics avg crt
print 'Scans in the Average:'; tell stack
return
finish
