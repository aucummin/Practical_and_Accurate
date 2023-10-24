c
c     FILE: randomdata.f                    Creation date: 17/JUL/1996.
c                                       LAST MODIFICATION: 21/MAR/2001.
c
c     Parameters and shared data for the random number generators.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999, 2001.
c
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
c
c     Parameters.
c
      double precision  a0, a1, c1, z1, z2
      parameter         (a0 = 123.456784373478d0, a1 = 1.23577d0)
      parameter         (c1 = 0.2719D0, z1 = 10100.d0, z2 = 10000.d0)
c
c     Shared data.
c
      double precision  xran, xran2, original_seed
      common            /random0_c/ xran, xran2, original_seed
c
      double precision  auxxran, auxxran2, auxoriginal_seed
      common            /random0_auxc/ auxxran, auxxran2,
     +                                 auxoriginal_seed
c
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
c
c     End of file 'randomdata.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
