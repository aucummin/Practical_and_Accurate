c
c     FILE: thincomm.f                      Creation date: 13/MAR/1998.
c                                       LAST MODIFICATION: 24/SEP/2001.
c
c     This file contains definition of variables related with
c     Hillas thinning algorithm.
c
c     Written by: S. J. Sciutto, La Plata 1998, 2001.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Auxiliary variables for standard thinning.
c
      double precision  thinmarker
      double precision  thinmaxw(npstacks)
      double precision  thinmaxwp(npstacks)
      double precision  thinmaxwg(npstacks)
      double precision  thinmaxdel(npstacks)
      double precision  currethin, thinmaxwfactor, emtohwfratio
c
      common            /auxthin_c0/ thinmarker, thinmaxw,
     +                               thinmaxwp, thinmaxwg, thinmaxdel,
     +                               currethin, thinmaxwfactor,
     +                               emtohwfratio
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'thincomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
