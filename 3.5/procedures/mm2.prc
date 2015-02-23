procedure mm2(xscan,yscan,scstep)
#  condar continuum map shadow plot, polarization 1
scalar xxs yys
scalar xscan yscan scstep
xxs=newfeed(xscan,2)
yys=newfeed(yscan,2)
mm(xxs,yys,scstep)
return
finish
