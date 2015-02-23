procedure stip (xscan)
#   fit a tipping scan using model 3 of solvetip
   scalar i, j
   typetip = 3
   slabel = 1
   get xscan
   totalpwr
   j = 1
   for i = 1 to 21 by 2
      d0(j) = d0(i) - d0(i+1)
      j = j + 1
   end
   h0(noint) = 11
   setpage(1,1024,1,720)
   cc page show
   graphics ; place(0,620)
   solvetip rline reshow
   place(75,750)
   print 'TYPE:STIP    TVANE=',tvane,'FTM=',ftm,'FTSBR=',ftsbr
   crt
   rstpage
return
finish
