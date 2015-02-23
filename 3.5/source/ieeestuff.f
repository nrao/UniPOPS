      double precision function dmaxnormal()
c
c     @(#)ieeestuff.f	5.1 06/22/94
c
      double precision d_max_normal
c
      dmaxnormal = d_max_normal()
      return
      end
c
c-----------------------------
c
      double precision function dminnormal()
c
      double precision d_min_normal
c
      dminnormal = d_min_normal()
      return
      end
c
c-----------------------------
c
      real function rmaxnormal()
c
      real r_max_normal
c
      rmaxnormal = r_max_normal()
      return
      end
c
c-----------------------------
c
      real function rminnormal()
c
      real r_min_normal
c
      rminnormal = r_min_normal()
      return
      end
c
c-----------------------------
c
      real function rinfinity()
c
      real r_infinity
c
      rinfinity = r_infinity()
      return
      end
c
c-----------------------------
c
      double precision function dinfinity()
c
      double precision d_infinity
c
      dinfinity = d_infinity()
      return
      end
c
c-----------------------------
c
      logical function okreal8(value)
c
c     Checks that VALUE is not +/- infinity and NaN.
c
      double precision value, dmaxnormal
c
      if(abs(value) .le. dmaxnormal()) then
	okreal8 = .true.
      else
	okreal8 = .false.
      endif
c
      return
      end
c
c-----------------------------
c
      logical function okreal4(value)
c
c     Checks that VALUE is not +/- infinity and NaN.
c
      real value, rmaxnormal
c
      if(abs(value) .le. rmaxnormal()) then
	okreal4 = .true.
      else
	okreal4 = .false.
      endif
c
      return
      end
c
