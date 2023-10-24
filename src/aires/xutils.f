c
c     FILE: xutils.f                        Creation date: 14/APR/1999.
c                                       LAST MODIFICATION: 09/JUN/2003.
c
c     This file contains several routines related with atmospheric
c     depth calculations.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function xslant(xvertical, x0, cozenith, zcground)
c
c     Evaluating the slanted path (in g/cm2) along an axis starting at
c     altitude zcground, and inclination given by cozenith (cosine of
c     the zenith angle). The path corresponds to the "segment" whose
c     ends possess vertical depths xvertical and x0, respectively. The
c     entire segment must lie above the ground surface. If x0 = 0 then
c     the path up to the top of the atmosphere is evaluated. The
c     earth's curvature is taken into account to perform the
c     calculations.
c
c     The slanted path is defined via
c
c                    point2
c          Xslant = INTEGRAL'  rho(zv) dl
c                    point1
c
c     where rho(zv) is the density of the medium, which is supposed to
c     depend only on zv, the vertical altitude. The prime in the
c     integral indicates that it is done along the specified axis.
c
c     The atmospheric model must be properly initialized before
c     invoking this routine.
c
c     Written by: S. J. Sciutto, La Plata 1998, 2001, 2003.
c
c
c     Parameters:
c     ==========
c
c     xvertical....... (input, double precision) Vertical depth of
c                      the path's end point 1 (g/cm2).
c     x0.............. (input, double precision) Vertical depth of
c                      the path's end point 2 (g/cm2). If x0 is zero
c                      then the path is evaluated up to the top of the
c                      atmosphere.
c     cozenith........ (input, double precision) Cosine of the axis'
c                      zenith angle. Must be in the range (0, 1].
c     zcground........ (input, double precision) Central altitude of
c                      the ground level, which defines the intersection
c                      between the slanted axis and the z-axis.
c
c     Return value: (double precision) The corresponding slanted path,
c     ============  in g/cm2. The returned value is zero on error.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  xslant
      double precision  xvertical, x0
      double precision  cozenith, zcground
c
c     Declaration of internal variables and arrays.
c
      double precision  rearth2
      parameter         (rearth2 = 2 * rearth)
c
c     The following parameters were set to optimize calculations whilst
c     ensuring that the error in Xslant is approximately less than
c     0.1 g/cm2 (Xslant > 300 g/cm2).
c
      double precision  coplanemin
      parameter         (coplanemin = 0.996d0)
      double precision  deltax0
      parameter         (deltax0 = 0.40d0)
c
      double precision  xv, xvp, zv, zvp, xvmax
      logical           xsign
      double precision  cozen2, sizprim2, zgrsz2
      double precision  xvrfact, xvrfact0, xvl0, xvqx, xpath
      integer           i
      double precision  zfromdepth
c
c     FIRST EXECUTABLE STATEMENT
c
      xslant = 0
c
c     Checking input arguments.
c
      if (cozenith .le. 0) return
c
c     Analysing the inclination of the axis.
c
      if (cozenith .lt. coplanemin) then
c
c       Oblique axes must be chopped.
c
        xsign = (x0 .gt. xvertical)
c
        if (xsign) then
          xv    = xvertical
          xvmax = x0
        else
          xv    = x0
          xvmax = xvertical
        endif
c
        cozen2   = cozenith ** 2
        xvrfact0 = deltax0 * cozen2
        xvrfact  = 1 + xvrfact0
        xvqx     = max(0.0001d0 * xvmax, xquantum)
        xvl0     = xvqx / xvrfact0
        zgrsz2   = 4 * (1 - cozen2) * (zcground + rearth) ** 2
c
        zv       = zfromdepth(xv, i)
        xpath    = 0
c
 1010   continue
c
          xvp = xv
c
          if (xv .gt. xvl0) then
            xv = xv * xvrfact
          else
            xv = xv + xvqx
          endif
          if (xv .gt. xvmax) xv = xvmax
c
          zvp = zv
          zv  = zfromdepth(xv, i)
c
          sizprim2 = zgrsz2 / (zv + zvp + rearth2) ** 2
          xpath    = xpath + (xv - xvp) / sqrt(1 - sizprim2)
c
        if (xv .lt. xvmax) goto 1010
c
        if (xsign) xpath = - xpath
c
        xslant = xpath
c
      else
c
c       Cuasi vertical axes are processed with a plane Earth model.
c
        xslant = (xvertical - x0) / cozenith
c
      endif
c
      return
      end
c     --- End of routine xslant.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function xvfromxs(xslant0, xs0, cozenith, zcground)
c
c     Evaluating the vertical depth corresponding to the end point of a
c     slanted path (in g/cm2) along an axis starting at altitude
c     zcground, and inclination given by cozenith (cosine of the zenith
c     angle). The path corresponds to the "segment" whose
c     ends possess slant depths xslant and xs0, respectively. If
c     xs0 = 0 then the path up to the top of the atmosphere is
c     evaluated. The earth's curvature is taken into account to perform
c     the calculations.
c
c     The slanted path is defined via
c
c                    point2
c          Xslant = INTEGRAL'  rho(zv) dl
c                    point1
c
c     where rho(zv) is the density of the medium, which is supposed to
c     depend only on zv, the vertical altitude. The prime in the
c     integral indicates that it is done along the specified axis.
c
c     The atmospheric model must be properly initialized before
c     invoking this routine.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Parameters:
c     ==========
c
c     xslant0......... (input, double precision) Slant depth of
c                      the path's end point 1 (g/cm2).
c     xs0............. (input, double precision) Slant depth of
c                      the path's end point 2 (g/cm2). If xs0 is zero
c                      then the path is evaluated up to the top of the
c                      atmosphere.
c     cozenith........ (input, double precision) Cosine of the axis'
c                      zenith angle. Must be in the range (0, 1].
c     zcground........ (input, double precision) Central altitude of
c                      the ground level, which defines the intersection
c                      between the slanted axis and the z-axis.
c
c     Return value: (double precision) The vertical depth of point 1,
c     ============  in g/cm2. The returned value is zero on error.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  xvfromxs
      double precision  xslant0, xs0
      double precision  cozenith, zcground
c
c     Declaration of internal variables and arrays.
c
      double precision  xstot, xs, xv, dxv
      double precision  xslant
c
c     FIRST EXECUTABLE STATEMENT
c
      xvfromxs = 0
c
c     Checking input arguments.
c
      if (cozenith .le. 0) return
c
c     Slant path from top of atmosphere.
c
      xstot = xslant0 + xs0
      if (xstot .le. 0) return
c
c     First approximation taken from plane Earth model.
c
      xv = xstot * cozenith
c
c     Newton-Raphson iteration.
c
 1010 continue
      xs  = xslant(xv, 0.d0, cozenith, zcground)
      dxv = (xstot - xs) * cozenith
      xv  = xv + dxv
      if (abs(dxv) .ge. abs(xv * 1.d-8)) goto 1010
c
      xvfromxs = xv
c
      return
      end
c     --- End of routine xvfromxs.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine olv2slant(nobslev, olxv, x0, zendis, zen1, zen2,
     +                     zcground, olxs)
c
c     Evaluating the slant depth of a set of observing levels. The slant
c     depths are calculated along an axis starting at altitude zcground,
c     for the "segment" that ends at vertical depth x0 (x0 = 0 is the
c     top of the atmosphere). The integer variable "zendis" allows to
c     select among fixed, sin and sin-cos zenith angle distributions.
c
c     Written by: S. J. Sciutto, Fermilab 1999.
c
c
c     Arguments:
c     =========
c
c     nobslev......... (input, integer) The number of observing
c                      levels.
c     olxv............ (input, double precision, array(nobslev))
c                      Vertical depths (g/cm^2) of the corresponding
c                      observing levels.
c     x0.............. (input, double precision) Vertical depth of
c                      the paths' end points (g/cm2). If x0 is zero
c                      then the paths are evaluated up to the top of
c                      the atmosphere.
c     zendis.......... (input, integer) Zenith angle distribution
c                      switch: 0 - fixed zenith angle, 1 - sin
c                      distribution, 2 - sin cos distribution.
c     zen1, zen2...... (input, double precision) Minimum and maximum
c                      zenith angles (degrees). If zendis is 0, then
c                      zen2 is not used and zen1 gives the
c                      corresponding fixed zenith angle.
c     zcground........ (input, double precision) Central altitude of
c                      the ground level, which defines the intersection
c                      between the slanted axis and the z-axis.
c     olxv............ (output, double precision, array(nobslev))
c                      Slant depths (g/cm^2) of the corresponding
c                      observing levels.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           nobslev, zendis
      double precision  olxv(nobslev)
      double precision  x0, zen1, zen2, zcground
      double precision  olxs(nobslev)
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  cozen, ftmp1, ftmp2
      double precision  xslant
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating cos(zenith) factor.
c
      if ((zendis .le. 0) .or. (zen1 .eq. zen2)) then
        cozen = cos(pi180 * zen1)
      else if (zendis .eq. 1) then
        ftmp1 = cos(pi180 * zen1)
        ftmp2 = cos(pi180 * zen2)
        cozen = (ftmp1 - ftmp2) / log(abs(ftmp1 / ftmp2))
      else
        ftmp1 = pi180 * zen1
        ftmp2 = pi180 * zen2
        cozen = (cos(2 * ftmp1) - cos(2 * ftmp2)) /
     +          (4 * (cos(ftmp1) - cos(ftmp2)))
      endif
c
c     Calculating the slant depths.
c
      ftmp1 = x0
      ftmp2 = 0
c
      do i = 1, nobslev
        ftmp2   = ftmp2 + xslant(olxv(i), ftmp1, cozen, zcground)
        ftmp1   = olxv(i)
        olxs(i) = ftmp2
      enddo
c
      return
      end
c     --- End of routine olv2slant
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'xutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
