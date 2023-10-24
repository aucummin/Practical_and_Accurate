c
c     FILE: xfitcomm.f                      Creation date: 13/SEP/1999.
c                                       LAST MODIFICATION: 13/SEP/1999.
c
c     This file contains the shared data used by the Xmax and Nmax
c     fitting routines.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Internal arrays for the depths of observing levels, number of
c     charged particles and weights.
c
      integer           nxfitdata
      double precision  xfitx(mxlongidata)
      double precision  xfitnch(mxlongidata)
      double precision  xfitwt(mxlongidata)
c
      common            /xfit_c0/ xfitx, xfitnch, xfitwt,
     +                            nxfitdata
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'xfitcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
