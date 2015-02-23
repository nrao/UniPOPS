procedure mm1(xscan,yscan,scstep)
#  condar continuum map shadow plot, polarization 1
scalar xxs yys
scalar xscan yscan scstep
xxs=newfeed(xscan,1)
yys=newfeed(yscan,1)
mm(xxs,yys,scstep)
return
finish
