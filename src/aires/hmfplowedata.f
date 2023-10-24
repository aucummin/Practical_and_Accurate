c
c     FILE: hmfplowedata.f                  Creation date: 02/JUL/2000.
c                                       LAST MODIFICATION: 02/JUL/2000.
c
c     This file contains the definitions of the internal arrays needed
c     for the parameterizations of the low energy hadronic mean free
c     paths.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
c
c     Arrays for the parameterization in the asymptotic zone.
c
      double precision  mfplasym1(totmfplsets)
      double precision  mfplasya(totmfplsets)
      double precision  mfplasyb(totmfplsets)
      double precision  mfplasyc(totmfplsets)
c
c     Arrays for the parameterization in the zone of resonances.
c
      integer           mfplnseg(totmfplsets)
      double precision  mfplglim0(0:mxmfplseg, totmfplsets)
      double precision  mfpla(mxmfplseg, totmfplsets)
      double precision  mfplb(mxmfplseg, totmfplsets)
c
c     Actual values of the parameterization variables are defined in
c     file 'hmfplowedata0.f'
c
      include 'hmfplowedata0.f'
c
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
c
c     End of file 'hmfplowedata.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
