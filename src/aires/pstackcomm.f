c
c     FILE: pstackcomm.f                    Creation date: 12/JUN/1996.
c                                       LAST MODIFICATION: 27/JAN/1999.
c
c     This file contains the particle stack (buffer) common data
c     definitions.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Main stack arrays.
c
      integer           npsta, supstasize, nmidlev, nmidlev2, midstalev
      integer           stasize(0:maxnsta), maxstasize(maxnsta)
      integer           totstasize(0:maxnsta)
      integer           nstaentr(maxnsta)
      integer           stabeg(maxnsta + 1)
      integer           staend(0:maxnsta)
      integer           last0staentr(maxnsta)
      integer           laststaentr(maxnsta)
      integer           freestaentr(2, maxnsta)
      integer           middlestaentr(maxnsta, 0:mxmidlev)
      integer           q3staentr(maxnsta, 0:mxmidlev)
c
      double precision  fpsta(maxstalen, maxstaentries)
c
      common            /pstack_c1/ npsta, supstasize,
     +                              nmidlev, nmidlev2,
     +                              midstalev, stasize, maxstasize,
     +                              totstasize, nstaentr,
     +                              stabeg, staend,
     +                              last0staentr, laststaentr,
     +                              freestaentr,
     +                              middlestaentr, q3staentr
c
      common            /pstack_c2/ fpsta
c
c     Equivalencing with other variables to improve stack
c     management.
c
      integer           stabeg0(maxnsta + 1)
      equivalence       (staend(0), stabeg0(1))
c
      integer           ipsta(maxstalen4, maxstaentries)
      logical           lpsta(maxstalen4, maxstaentries)
      character*(mxbch) chpsta(maxstaentries)
c
      equivalence       (fpsta, ipsta, lpsta, chpsta)
c
c     Data for hard extensions of the stacks.
c
      integer           nhstaentr(maxnsta)
c
      common            /phstack_c/nhstaentr
c
c     Size limiting semaphores and related variables.
c
      integer           stalevkey, maxhentries
      logical           greenlight(maxnsta)
      integer           ngreencalls, nredcalls
      integer           bigstasize(maxnsta)
      integer           littlestasize(maxnsta)
      integer           stacateg(maxnsta)
      integer           stafirst(maxnsta), nstafirstcat
c
      common            /pstack_sem/ stalevkey, maxhentries,
     +                               greenlight,
     +                               ngreencalls, nredcalls,
     +                               bigstasize, littlestasize,
     +                               stacateg, stafirst, nstafirstcat
c
c     Statistical counters.
c
      integer           totalloc(maxnsta)
      integer           nrhoper(maxnsta)
      integer           nwhoper(maxnsta)
c
      common            /pstacksts_c/ totalloc, nrhoper, nwhoper
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'pstackcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
