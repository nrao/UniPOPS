procedure sptip (xscan)
#  Fit a log-linearized sky tipping scan.
   scalar i, j
   typetip = 2
   slabel = 1
   get xscan
   totalpwr
   j = 1
   for i = 1 to 21 by 2
      d0(j) = log ( d0(i) - d0(i+1) )
      j = j + 1
   end
   h0(noint) = 11
   setpage(1,1024,1,720)
   cc page show
   graphics ; place(0,620)
   solvetip rline reshow
   place(75,750)
   print 'TYPE:SPTIP'
   crt
return
finish
