c
c     FILE: pgndbuffcomm.f                  Creation date: 03/DEC/1996.
c                                       LAST MODIFICATION: 06/MAR/2002.
c
c     This file contains the definitions of the arrays used to store
c     ground particle data.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997; Fermilab 1999;
c                                La Plata 2002.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Main ground buffer
c
      integer           npgndbuff
      integer           gndbuffsta(gndbuffsize)
      double precision  fgndbuff(maxstalen, gndbuffsize)
c
      common            /gndbuff_c1/ fgndbuff,
     +                               gndbuffsta, npgndbuff
c
c     Equivalencing with other variables to improve stack
c     management.
c
      integer           igndbuff(maxstalen4, gndbuffsize)
      logical           lgndbuff(maxstalen4, gndbuffsize)
      character*(mxbch) gndbuffrec(gndbuffsize)
c
      equivalence       (fgndbuff, igndbuff, lgndbuff, gndbuffrec)
c
c     Particles not saved in the ground particle file.
c
      double precision  ngndlowp(5, 2)
      double precision  ngndhighp(5)
      double precision  egndlowp(5, 2)
      double precision  egndhighp(5)
c
      common            /pclests_c2/ ngndlowp, ngndhighp,
     +                               egndlowp, egndhighp
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'pgndbuffcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
