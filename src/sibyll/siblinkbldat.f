c
c     FILE: siblinkbldat.f                  Creation date: 16/FEB/2002.
c                                       LAST MODIFICATION: 16/FEB/2002.
c
c     This file contains initializations for some arrays needed for the
c     AIRES-SIBYLL interface.
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
cb---bb---bb---bb---bb---bb---bb---b*b---bb---bb---bb---bb---bb---bb---b
cb---bb---bb---bb---bb---bb---bb---b*b---bb---bb---bb---bb---bb---bb---b
c
      block data siblinkbldat
c
c     SIBYLL particle codes conversion arrays.
c
c     Declaration of shared variables.
c
      include 'siblinkcomm.f'
c
c     Data initialization.
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
c     AIRES -> SIBYLL code transformations: etas are mapped onto K0S,
c     and Lambdas onto neutrons.
c
      data              sibcode
     +
     +   /                                           -13, -14,
     +       -99, -99, -99, -99, -99, -99, -99, -99, -99, -14,
     +       -99, -99, -99, -99,  12,  10,  11,  12,   8,   6,
     +       -99, -99,  18,  16, -99, -99,   5,   3, -99, -99,
     +         1,   2,   4,  99,  99,  15,  17,  99,  99,   6,
     +         7,  12,  11,   9,  12,  99,  99,  99,  99,  14,
     +        99,  99,  99,  99,  99,  99,  99,  99,  99,  14,
     +        13                                                /
c
      data              airescode 
     +
     +   /   -20, -99, -99, -99, -99, -99, -99, -99, -99, -99,
     +       -99, -99, -99, -99, -99, -99,  15, -99, -99, -99,
     +       -99,   7,  -7,   6,  -6, -30, -31,  12,  13,  14,
     +       -14,  11, -11,  10,   3,  -3,   2,  -2,   1, -99,
     +         1,   2,  -2,   3,  -3,  10,  11, -11,  14, -14,
     +        13,  12,  31,  30,   6,  -6,   7,  -7,  99,  99,
     +        99,  99,  15,  99,  99,  99,  99,  99,  99,  99,
     +        99,  99,  99,  99,  99,  99,  99,  99,  20        /
c
      end
c     --- End of block data siblinkbldat
c
cb---bb---bb---bb---bb---bb---bb---b*b---bb---bb---bb---bb---bb---bb---b
cb---bb---bb---bb---bb---bb---bb---b*b---bb---bb---bb---bb---bb---bb---b
c
c     End of file 'siblinkbldat.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
