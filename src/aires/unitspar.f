c
c     FILE: unitspar.f                      Creation date: 25/JUN/1996.
c                                       LAST MODIFICATION: 15/OCT/2001.
c
c     This file contains the physical units related parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     neunits is the number of energy units accepted by the program.
c     nlunits idem for length units.
c     ntunits idem for time units.
c     naunits idem for angle units.
c     nbunits idem for angle units (nbunits0 is for "nice" units).
c
      integer           neunits, neunits0
      integer           nlunits, nlunits0
      integer           ntunits, naunits
      integer           nbunits, nbunits0
      parameter         (neunits  = 11)
      parameter         (neunits0 =  9)
      parameter         (nlunits  =  7)
      parameter         (nlunits0 =  3)
      parameter         (ntunits  =  4)
      parameter         (naunits  =  2)
      parameter         (nbunits  =  8)
      parameter         (nbunits0 =  4)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'unitspar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
