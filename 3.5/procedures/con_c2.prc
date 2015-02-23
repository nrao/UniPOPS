procedure con_c2
#  Condar C2  Combine Channel 2 data
sclear slabel=1; con_pstack(1); 
ave page show 
slabel=0; 
place(150,675); graphics avg crt
print 'Scans in the Channel 2 Average:'; tell stack
return
finish
