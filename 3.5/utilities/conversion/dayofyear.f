      subroutine dayofyear(iyear, imth, iday, idoy)
c
c     @(#)dayofyear.f	5.1 06/22/94
c
c     Calculates day of the year from month (IMTH) and day (IDAY)
c
c     iyear = (I*4) Year (needed to check for leap year (bissextile year)
c     imth = (I*4) Month number
c     iday = (I*4) Day number
c     idoy = (I*4) day of year number generated
c
      integer*4 imth, iday, idoy, iyear, i1
      double precision  jdate, jnewyear, tjd
c 
      data i1/1/
c
      call checkdate(iyear,imth,iday,0,0,0) 
c 
      call jd(iyear, imth, iday, tjd) 
      jdate = tjd 
      call jd(iyear, i1, i1,tjd)  
      jnewyear = tjd
c
      idoy = jdate - jnewyear + 1 
c
      return
      end
c
