proc testwhile(b)
scalar a
a=b
while a>= 10
   print a; a=a-1; end
a=b
while a<=10
   print a; a=a+1; end
a=b
while a~=10
   print a; a=10; end
a=b
while a~=10 & a~=20
   print a; a=10; end
return a
finish
