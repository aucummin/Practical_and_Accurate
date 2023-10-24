c
c     FILE: siblinkcomm.f                   Creation date: 16/FEB/2002.
c                                       LAST MODIFICATION: 16/FEB/2002.
c
c     This file contains the definitions of some arrays needed for the
c     AIRES-SIBYLL interface.
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     List of SIBYLL particle codes that are relevant for the
c     AIRES-SIBYLL interface.
c
c        1    gamma            13    proton         -13    pbar
c        2    e+               14    neutron        -14    nbar
c        3    e-               15    nu_e
c        4    mu+              16    nu_ebar
c        5    mu-              17    nu_mu
c        6    pi0              18    nu_mubar
c        7    pi+                 ...
c        8    pi-              23    eta
c        9    K+                  ...
c       10    K-               39    Lambda         -39    Lambda-bar
c       11    K0L
c       12    K0S
c
c     Allowed projectile particles (SIBYLL codes):
c
c     6,   7, 8, 9, 10, 11, 12, 13, 14,  -13, -14
c     pi0, pi+-, K+-,   KL, KS,  p,  n, pbar, nbar
c
c     SIBYLL nuclei codes: 1000 + A, where A is mass number
c                          (1 < A < 60).
c
c     SIBYLL particle codes conversion arrays.
c
      integer           sibcode(-31:31)
      integer           airescode(-39:39)
c
      common            /asibcodes_c/ sibcode, airescode
c
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'siblinkcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
