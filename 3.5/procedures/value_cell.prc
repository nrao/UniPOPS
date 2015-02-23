# Conversion function procedures for cubes and matrices.
# @(#)value_cell.prc	5.1 06/22/94

proc cval2cell(axis, value)
# Converts value for axis AXIS of a cube to its corresponding cell number
if axis=1 then
   return   (value-ch0(crval1) ) / ch0(cdelt1) + ch0(cpix1)
end
if axis=2 then
   return   (value-ch0(crval2) ) / ch0(cdelt2) + ch0(cpix2)
end
if axis=3 then
   return   (value-ch0(crval3) ) / ch0(cdelt3) + ch0(cpix3)
end
return default
finish

proc ccell2val(axis, pix)
# Converts cell number for axis AXIS of a cube to its corresponding value
if axis=1 then
   return ((pix-ch0(cpix1))*ch0(cdelt1) + ch0(crval1))
end
if axis=2 then
   return ((pix-ch0(cpix2))*ch0(cdelt2) + ch0(crval2))
end
if axis=3 then
   return ((pix-ch0(cpix3))*ch0(cdelt3) + ch0(crval3))
end
return default
finish

proc mval2cell(axis, value)
# Converts value for axis AXIS of a matrix to its corresponding cell number
if axis=1 then
   return   (value-mh0(mrval1) ) / mh0(mdelt1) + mh0(mpix1)
end
if axis=2 then
   return   (value-mh0(mrval2) ) / mh0(mdelt2) + mh0(mpix2)
end
return default
finish

proc mcell2val(axis, pix)
# Converts cell number for axis AXIS of a cube to its corresponding value
if axis=1 then
   return ((pix-mh0(mpix1))*mh0(mdelt1) + mh0(mrval1))
end
if axis=2 then
   return ((pix-mh0(mpix2))*mh0(mdelt2) + mh0(mrval2))
end
return default
finish

