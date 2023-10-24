c
c     FILE: splitmodeldata.f                Creation date: 29/JUN/2001.
c                                       LAST MODIFICATION: 15/OCT/2001.
c
c     This file contains static parameters and shared variables used in
c     the Hillas Splitting algorithm.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     General parameters.
c
      double precision  qqspmes, qqspnuc, prob2pi
      double precision  ptopih, ptokah, ptonuh
c
      parameter         (qqspmes  = 0.650d0)
      parameter         (qqspnuc  = 0.650d0)
      parameter         (prob2pi  = 0.060d0)
c
      parameter         (ptopih   = 0.295d0)
      parameter         (ptokah   = 0.305d0)
      parameter         (ptonuh   = 0.475d0)
c
c     Types of energy packets.
c
      integer           pion, kaon, baryon, particle, glueball, leader
      parameter         (pion     = 0)
      parameter         (kaon     = 1)
      parameter         (baryon   = 2)
      parameter         (particle = 3)
      parameter         (leader   = 4)
      parameter         (glueball = 5)
c
c     Shared variables and arrays.
c
      double precision  qqsp
      double precision  prnucd, prnucdrf
      double precision  prpion, prbaryon, prkaon
      double precision  prqfam(0:3, pion:baryon)
      double precision  pofam(pion:baryon)
c
      common            /hspa_c1/ qqsp, prnucd, prnucdrf,
     +                            prpion, prbaryon, prkaon,
     +                            prqfam, pofam
c
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'splitmodeldata.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
