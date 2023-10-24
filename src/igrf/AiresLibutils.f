c
c     FILE: AiresLibutils.f                 Creation date: 02/APR/2003.
c                                       LAST MODIFICATION: 21/APR/2005.
c
c     This file contains some AIRES-GEOMAG interface utility routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine geomagneticr(latitude, longitude, altitude, fdate,
     +                        bnorth, bwest, bup)
c
c     Calculation of the cartesian components of the geomagnetic field
c     of a given location using the IGRF model.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000, 2003, 2005.
c
c
c     Arguments:
c     =========
c
c     latitude........ (input, double precision) Geographical latitude
c                      (deg).
c     longitude....... (input, double precision) Geographical
c                      longitude (deg).
c     altitude........ (input, double precision) Site altitude in
c                      meters above sea level.
c     fdate........... (input, double precision) Date, encoded as a
c                      single real number. If date is positive, then
c                      it is interpreted as a floating point year
c                      (months and days are fractions of years). If
c                      negative, then abs(date) = day + 32 * month +
c                      512 * year.
c     bnorth.......... (output, double precision) Northward component
c                      of the geomagnetic field (nT).
c     bwest........... (output, double precision) Westward component
c                      of the geomagnetic field (nT).
c     bup............. (output, double precision) Vertically-upward
c                      component of the geomagnetic field (nT).
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
      double precision  bnorth, bwest, bup
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
      bnorth =   bx
      bwest  = - by
      bup    = - bz
c
      return
c
      end
c     --- End of routine geomagneticr
c     
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'AiresLibutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
