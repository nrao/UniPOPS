# Trigonometric function procedures that accept/give
# angles in radians.

proc sind(x)
pointer rad 57.2957795
return sin(x/rad)
finish

proc cosd(x)
pointer rad 57.2957795
return cos(x/rad)
finish

proc tand(x)
pointer rad 57.2957795
return tan(x/rad)
finish

proc atand(x)
pointer rad 57.2957795
return rad*atan(x)
finish

proc asind(x)
pointer rad 57.2957795
return rad*asin(x)
finish

proc acosd(x)
pointer rad 57.2957795
return rad*acos(x)
finish

proc atan2d(x,y)
pointer rad 57.2957795
return rad*atan2(x,y)
finish

