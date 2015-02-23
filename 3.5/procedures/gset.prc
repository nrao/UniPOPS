procedure gset
#  Enters Gaussian fit parameters with crosshairs.  PRJ.
scalar i, x1, x2, x3, curx, cury
PRINT 'Enter # of Gaussians to fit'
read ngauss
hwidth=0; height=0; center=0
bgauss=1; egauss=H0(noint)
for i = 1 to ngauss
#  remember current graphics position
  curx = status(31);cury = status(32)
  place (1,800-120*i)
  print '** PARMS for Gaussian ',I
  print 'Mark LEFT HP POINT, PEAK, then RIGHT HP POINT'
#  return cursor to old position
  place(curx,cury)
  x1=ccur
  x2=tcur
  x3=ccur
  height(i)=x2
  center(i)=(x1+x3)/2.
  hwidth(i)=abs(x3-x1)
end
return
finish
