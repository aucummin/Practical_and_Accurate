c
c     FILE: initlib.f                       Creation date: 07/JUN/2003.
c                                       LAST MODIFICATION: 07/JUN/2003.
c
c     This file contains auxiliary routines for initialization that
c     are also needed in the AIRES library.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine init4s
c
c     Basic initializations to be performed every run after task
c     initialization or idf file reading.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
      include 'ciopar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'hdatacomm.f'
      include 'ciocomm.f'
      include 'constants.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      double precision  zfromdepth
c
c     FIRST EXECUTABLE STATEMENT
c
c     Array of positions (depths and altitudes) of the different
c     observing levels, and related data.
c
      do i = 0, nobslevels - 1
        j = i + 1
        obslevdepth(j) = obslevmind +
     +                   i * ((obslevmaxd - obslevmind) /
     +                        (nobslevels - 1))
        obslevz(j)     = zfromdepth(obslevdepth(j), k)
        obslevz00sq(j) = (rearth + obslevz(j)) ** 2
        if (obslevdepth(j) .le. groundepth) obslevlmax = j
      enddo
      obslevdepth(nobslevelsp1) = groundepth
      obslevz(nobslevelsp1)     = groundz
      if ((obslevlmax .eq. nobslevels) .and.
     +    (obslevdepth(nobslevels) .lt. groundepth))
     +  obslevlmax = nobslevelsp1
c
c     Histogram scaling factors and related data.
c
      rhn0      = - 2 * log(rminhis)
      factrthn  = nttabins / (2 * log(rmaxhis) + rhn0)
      rhn0      = rhn0 + 1.d0 / factrthn
      rhni0     = - 0.5d0 * rhn0
      factrthni =  0.5d0 / factrthn
c
      rhe0      = - log(eminhis)
      factrthe  = nttabins / (log(emaxhis) + rhe0)
      rhe0      = rhe0 + 1.d0 / factrthe
      rhei0     = - rhe0
      factrthei = 1.d0 / factrthe
c
      return
      end
c     --- End of routine init4s
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setdynfromidf
c
c     Setting dynamical variables from their values obtained
c     from the idf file.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      tasknameset    = xtasknameset
      mtotshowers    = xtotshowers
      runshowers     = xrunshowers
      cpuperrun      = xcpuperrun
      processjobs    = xprocessjobs
      totshwset      = xtotshwset
      runshwset      = xrunshwset
      cpurunset      = xcpurunset
      processjobsset = xprocessjobsset
c
      return
      end
c     --- End of routine setdynfromidf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initlib.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
