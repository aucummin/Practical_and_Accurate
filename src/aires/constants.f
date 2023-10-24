c
c     FILE: constants.f                     Creation date: 19/JUL/1996.
c                                       LAST MODIFICATION: 11/JUL/2003.
c
c     This file contains some frequently used mathematical and physical
c     constants.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2000, 2002,
c                                         2003.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     Mathematical constants.
c
      double precision  pi
      parameter         (pi = 3.1415926535897932385d0)
c
      double precision  twopi, halfpi, qterpi, pi180, upi180
      parameter         (twopi  = 2 * pi)
      parameter         (halfpi = pi / 2)
      parameter         (qterpi = pi / 4)
      parameter         (pi180  = pi / 180)
      parameter         (upi180 = 180 / pi)
c
      double precision  ln10, uln10, sqrtl10
      parameter         (uln10  = 0.434294481903251828d0)
      parameter         (ln10   = 1.d0 / uln10)
      parameter         (sqrtl10 = uln10 / 2)
c
c     Fundamental physical constants.
c
      double precision  cspeed, cspeedns
c
      parameter         (cspeed   = 299792458.d0)
      parameter         (cspeedns = cspeed / 1.d9)
c
      double precision  ucspeed, ucspeedns
c
      parameter         (ucspeed   = 1.d0 / cspeed)
      parameter         (ucspeedns = 1.d9 / cspeed)
c
      double precision  hbargev, avogadro, epsilon0
c
      parameter         (hbargev  = 6.58212200d-25)
      parameter         (avogadro = 6.02214199d+23)
      parameter         (epsilon0 = 1.d7 / (4 * pi * cspeed ** 2))
c
c     Rest energies of elementary particles (in GeV).
c     Taken from: C. Caso et al. (Particle Data Group), European
c                 Physical Journal, C3 (1998) 1; and
c                 K. Hagiwara et al. (Particle Data Group),
c                 Phys. Rev. D 66, 010001 (2002).
c
      double precision  electronmass, muonmass, taumass
      double precision  neutronmass, protonmass
      double precision  chpimass, pizeromass
      double precision  chkmass, k0mass, etamass, lambdamass
      double precision  sigma0mass, sigmaplusmass, sigmaminusmass
      double precision  xi0mass, ximinusmass
      double precision  omegaminusmass
c
      parameter         (electronmass    = 0.51099907d-3)
      parameter         (muonmass        = 0.105658389d0)
      parameter         (taumass         = 1.77705d0)
      parameter         (neutronmass     = 0.93956563d0)
      parameter         (protonmass      = 0.93827231d0)
      parameter         (chpimass        = 0.13956995d0)
      parameter         (pizeromass      = 0.1349764d0)
      parameter         (chkmass         = 0.493677d0)
      parameter         (k0mass          = 0.497672d0)
      parameter         (etamass         = 0.54730d0)
      parameter         (lambdamass      = 1.115684d0)
      parameter         (sigma0mass      = 1.192642d0)
      parameter         (sigmaplusmass   = 1.18937d0)
      parameter         (sigmaminusmass  = 1.197449d0)
      parameter         (xi0mass         = 1.31483d0)
      parameter         (ximinusmass     = 1.32131d0)
      parameter         (omegaminusmass  = 1.67245d0)
c
      double precision  sqremass, halfemass, twoemass
      double precision  sqrmumass
      double precision  avnucmass
c
      parameter         (sqremass  = electronmass ** 2)
      parameter         (halfemass = electronmass / 2)
      parameter         (twoemass  = electronmass * 2)
      parameter         (sqrmumass = muonmass ** 2)
      parameter         (avnucmass = (neutronmass + protonmass) / 2)
c
      double precision  protoncharge
c
      parameter         (protoncharge = 1.60217733d-19)
c
c     Constants related to the Earth's sphericity, and the propagation
c     of particles through the Earth's atmosphere.
c
      double precision  rearth
c
      parameter         (rearth = 6370949.d0)
c
      double precision  hplanelim0, hplanelim, zplane2
      double precision  cosalphamax, xquantum
c
      parameter         (hplanelim   = 0.15d0 * pi180 * rearth)
      parameter         (zplane2     = 60000.d0)
      parameter         (hplanelim0  = 0.25d0 * hplanelim)
      parameter         (cosalphamax = 0.50d0)
      parameter         (xquantum    = 2.00d-12)
c
      double precision  hplanelim1, zcglobmin 
c
      parameter         (hplanelim1 = (hplanelim - hplanelim0)
     +                                / zplane2)
      parameter         (zcglobmin  = rearth * (cosalphamax - 1))
c
c     Miscellaneous constants.
c
c     Factor to evaluate plasma frequencies.
c
      double precision  plasmaf0
c
      parameter         (plasmaf0 = 1.d-3 *
     +                              ((hbargev / electronmass) ** 2) *
     +                              avogadro * protoncharge *
     +                              (cspeed ** 2) /
     +                              (epsilon0 * electronmass))
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'constants.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
