proc gaussfit(gauss_num)
# Sets up parameters for fitting GAUSS_NUM gaussians to the contents
# of array (0)
#
scalar gauss_i, gauss_a, gauss_b
#
if gauss_num < 1 | gauss_num > 12 then
    print 'GAUSSFIT: Bad argument'
    return
end
ngauss= gauss_num
#
print ' '
print 'Define leftmost and rightmost channels overwhich Gaussians.'
print 'will be fitted'
print ' '
gauss_a=ccur; gauss_b=ccur
bgauss=min(gauss_a,gauss_b); egauss=max(gauss_a,gauss_b)
#
for gauss_i=1 to ngauss
   print ' '
   print 'Define center of Gaussian #', gauss_i
   print ' '
   center(gauss_i)=ccur
   print ' '
   print 'Define FWHM of Gaussian #', gauss_i
   print ' '
   gauss_a=ccur; gauss_b=ccur; hwidth(gauss_i)=abs(gauss_a - gauss_b)
   end
print ' '
return
finish
