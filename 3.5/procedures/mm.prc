procedure mm(xscan,yscan,scstep)
#  Displays a shadow-plot map of mapping scans.
scalar i scrange1 scrange2
scalar xscan yscan scstep
line
page
scrange1=-2*scstep
scrange2=(yscan-xscan+2)*scstep
yrange (scrange1,scrange2) 
slabel=6
get xscan
switched
show
slabel=0
for i = xscan+1 to yscan ; get i ; switched ;
  fact=(i-xscan)*scstep
  bias
  reshow
  print i
end
histogram
freey
return
finish
