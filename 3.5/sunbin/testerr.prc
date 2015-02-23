proc testerr
for nsave = 1 to 20
   erroff
   recall
   if errcode = 0 then
       baseline
   else
       print 'ERROR-- code:', errcode
   end; end
erron return
finish
