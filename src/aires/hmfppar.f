c
c     FILE: hmfppar.f                       Creation date: 02/JUL/2000.
c                                       LAST MODIFICATION: 03/JUL/2000.
c
c     Parameters related with hadronic mean free paths.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     Sizes of arrays used to parameterize high energy mean free paths.
c
      integer           totmfphsets
c
      parameter         (totmfphsets = 3)
c
c     Parameter defining the high energy sets.
c
      integer           pionhset, kaonhset, nucleonhset
c
      parameter         (nucleonhset = 1)
      parameter         (pionhset    = 2)
      parameter         (kaonhset    = 3)
c
c     Sizes of arrays used to parameterize low energy mean free paths.
c
      integer           totmfplsets, mxmfplseg
c
      parameter         (totmfplsets = 9)
      parameter         (mxmfplseg   = 9)
c
c     Number of different low energy hadronic mfp sets, and parameters
c     to store the sets associated with each hadronic particle.
c
      integer           nmfpsets, pi0set, piminusset, piplusset
      integer           k0sset, k0lset, kminusset, kplusset, etaset
      integer           nset, nbarset, pset, pbarset
c
c     Actual values of these parameters are given in file
c     'hmfplowepar0.f'
c
      include 'hmfplowepar0.f'
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'hmfppar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
