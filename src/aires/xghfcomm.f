c
c     FILE: xghfcomm.f                      Creation date: 14/SEP/1999.
c                                       LAST MODIFICATION: 27/OCT/2000.
c
c     This file contains the shared data used by the Gaisser-Hillas
c     function related routines.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2000.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Current parameter set and related internal quantities.
c
      double precision  ghf_nmax, ghf_xmax, ghf_x0, ghf_lambda
      double precision  ghf_xmaxx0, ghf_xmaxx0l
      double precision  ghf_x1i0, ghf_x1i1
c
      common            /xghf_c0/ ghf_nmax, ghf_xmax,
     +                            ghf_x0, ghf_lambda,
     +                            ghf_xmaxx0, ghf_xmaxx0l,
     +                            ghf_x1i0, ghf_x1i1
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'xghfcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
