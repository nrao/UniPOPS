procedure dms(angle, sgn, ideg, imin, secs)
#  Decomposes angle into deg, mins, and seconds
#  The sign (+/-) is returned via sgn, all other returned values are >= 0.
#  ideg and imin are guaranteed to be integers.
#  angle is not changed
scalar angtmp
sgn = 1.0
if (angle < 0) then; sgn = -1.0; end
angtmp = angle * sgn
ideg = ifix(angtmp)
imin = ifix((angtmp- ideg)*60)
secs = ((angtmp - ideg)*60 - imin) * 60
#
return
finish
