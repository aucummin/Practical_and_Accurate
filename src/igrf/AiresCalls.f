c
c     FILE: AiresCalls.f                    Creation date: 01/OCT/1997.
c                                       LAST MODIFICATION: 21/ABR/2005.
c
c     This file contains the AIRES-IGRF10 interface routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine geomagnetic(latitude, longitude, altitude, fdate,
     +                       bfield, inclination, declination)
c
c     Calculation of the geomagnetic field of a given location
c     using the IGRF10 model.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000, 2005.
c
c
c     Arguments:
c     =========
c
c     latitude........ (input, double precision) Geographical latitude
c                      (deg). Must be in the proper range.
c     longitude....... (input, double precision) Geographical
c                      longitude (deg). Must be in the proper range.
c     altitude........ (input, double precision) Site altitude in
c                      meters above sea level.
c     fdate........... (input, double precision) Date, encoded as a
c                      single real number. If date is positive, then
c                      it is interpreted as a floating point year
c                      (months and days are fractions of years). If
c                      negative, then abs(date) = day + 32 * month +
c                      512 * year.
c     bfield.......... (output, double precision) Magnetic field
c                      intensity (nT).
c     inclination..... (output, double precision) Inclination of the
c                      magnetic field with respect to the local
c                      horizontal plane (deg).
c     declination..... (output, double precision) Angle between the
c                      local meridian and the horizontal component of
c                      the magnetic field.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'igrfpar.f'
c
c     Declaration of arguments.
c
      double precision  latitude, longitude, altitude, fdate
      double precision  bfield, inclination, declination
c
c     Declaration of internal variables and arrays.
c
      double precision  fyear
      integer           ymd(3)
c
      double precision  altitkm, colat
      double precision  bx, by, bz, bf
c
      double precision  cvtfltdate
c
c     FIRST EXECUTABLE STATEMENT
c
c     Floating point date.
c
      fyear = cvtfltdate(fdate, ymd)
c
c     Calling the IGRF10 geomagnetic field syntesis routine.
c
      altitkm = 1.d-3 * altitude
      colat   = 90 - latitude
c
      call igrf10syn (0, fyear, 1, altitkm, colat, longitude,
     +                bx, by, bz, bf)
c
      bfield = bf
c
      if (bf .gt. 0) then
        inclination = upi180 * asin(bz / bf)
        declination = upi180 * atan2(by, bx)
      endif
c
      return
c
      end
c     --- End of routine geomagnetic
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'AiresCalls.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
