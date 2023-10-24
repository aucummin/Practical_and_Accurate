c
c     FILE: showercomm.f                    Creation date: 16/AUG/1996.
c                                       LAST MODIFICATION: 12/MAY/2000.
c
c     Auxiliary shower parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Primary code, direction of the shower axis, global time shift and
c     central injection altitude.
c     Real fields of the primary header.
c
      integer           primcode
      double precision  initshdir(3), currshdir(3)
      double precision  cinjz, showert0
      double precision  primfdata(15)
c
      common            /auxshower_c0/ cinjz, showert0, primcode
      common            /auxshower_c1/ initshdir, currshdir,
     +                                 primfdata
c
c     Coordinates of the intersections of the shower axis with the
c     different observing levels, and related data.
c
      double precision  obslevcore(5, mxobslevelsp1)
      double precision  obslevt0(mxobslevelsp1)
c
      common            /auxshower_c2/ obslevcore, obslevt0
c
c     Auxiliary data to evaluate intersection with shower axis.
c
      double precision  zsfact(0:3)
      double precision  obslevsa(0:mxobslevelsp1)
c
      common            /zs_c/ zsfact, obslevsa
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'showercomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
