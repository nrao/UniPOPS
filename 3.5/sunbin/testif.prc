proc testif(a)
if a > 10 then
   print 'a>10'; end
if a < 10 then
   print 'a<10'; end
if a=10 then
   print 'a=10'; end
if a ~= 10 then
   print 'a~=10'; end
if a >= 10 then
   print 'a>=10'; end
if a <= 10 then
   print 'a<=10'; end
if a>0 & a<10 then
   print 'a>0 & a<10'
else
   print 'not a>0 & a,10';end
if a>10 | a<0 then
   print 'a>10 | a<0'; end
if ~(a>10 | a<0) then
   print '~(a>10 | a<0)'; end
if ~(a>10) & a<0 then
  print '~(a>10) & a<0'; end
if a~=10 & a=5 then
  print 'a~=10 & a=5'; end
return
finish
