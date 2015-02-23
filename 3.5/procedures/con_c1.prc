procedure con_c1
#  Condar C1  Combine Channel 1 data
sclear slabel=1; con_pstack(0); 
ave page show 
slabel=0; 
place(150,675); graphics avg crt
print 'Scans in the Channel 1 Average:'; tell stack
return
finish
