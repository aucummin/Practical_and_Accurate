c
c     FILE: kernelmergecomm.f               Creation date: 18/AUG/2003.
c                                       LAST MODIFICATION: 24/AUG/2003.
c
c     This file contains the definitions of the kernel shared
c     variables, that are needed in the AiresMerge system.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Number of stacks (a parameter in the main system).
c
      integer           amnpstacks
c
      common            /amstacksta_c0/ amnpstacks
c
c     Stack statistics and related variables.
c
      double precision  amavgtotsize(npstacks)
      double precision  amprocentries(3, npstacks)
      integer           ampeakstsize(3, npstacks)
      integer           amhardswrite(3, npstacks)
      integer           amhardsread(3, npstacks)
      integer           amcallcounter(2, 0:npstacks)
c
      common            /amstacksta_c/ amavgtotsize, amprocentries,
     +                                 amhardswrite, amhardsread,
     +                                 ampeakstsize, amcallcounter
c
c     Particle and energy counters.
c
      double precision  amnplost(5, 0:npstacks), amelost(5, 0:npstacks)
      double precision  amnplowe(5, 0:npstacks), amelowe(5, 0:npstacks)
      double precision  amnprgnd(5, 0:npstacks)
      double precision  ameprgnd(5, 0:npstacks)
      double precision  amtotpcles(5, 0:npstacks)
      double precision  ameloss(5, 0:npstacks)
      double precision  amnnotap(5), amenotap(5)
      double precision  amnneutrino(5), ameneutrino(5)
      double precision  amaveprim(5)
c
      common            /ampclests_c1/ amnplost, amelost,
     +                                 amnplowe, amelowe,
     +                                 amnprgnd, ameprgnd,
     +                                 amtotpcles,
     +                                 ameloss, amnnotap, amenotap,
     +                                 amnneutrino, ameneutrino,
     +                                 amaveprim
c
c     Miscellaneous.
c
c     fstintdp contains statistical data related with the first
c     interaction depth.
c
      double precision  amfstintdp(5, 2)
c
      common            /amkernelmisc_c/ amfstintdp
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'kernelmergecomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
