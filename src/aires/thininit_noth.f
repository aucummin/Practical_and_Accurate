c
c     FILE: thininit_noth.f                 Creation date: 01/MAY/2003.
c                                       LAST MODIFICATION: 01/MAY/2003.
c
c     This file contains initialization routines for the AIRES
c     thinning algorithm, for the case when this feature is disabled.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine thininit0(thinningison)
c
c     Initializations for no thinning to be performed before scanning
c     the input file.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     thinningison.... (input, logical) True to enable thinning. False
c                      otherwise.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           thinningison
c
c     FIRST EXECUTABLE STATEMENT
c
      thinningison = .false.
c
      return
      end
c     --- End of routine thininit0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine thininit4
c
c     Standard thinning method initializations to be performed before
c     entering the simulation process.
c     Version to use when thinning is disabled.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'kernelpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'thincomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, npstacks
        thinmaxw(i)   = 1
        thinmaxwp(i)  = 1
        thinmaxwg(i)  = 1
        thinmaxdel(i) = 0
      enddo
c
c     Minimum and current thinning energy are evaluated for
c     compatibility.
c
      minethin  = 1.001d0 * min(nucutenergy,
     +                          cutenergy(pipluscode, 1),
     +                          cutenergy(mupluscode, 1),
     +                          cutenergy(electroncode, 1),
     +                          cutenergy(gammacode, 1))
      currethin = minethin
c
      return
      end
c     --- End of routine thininit4
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine showerthinit(primenergy)
c
c     Standard thinning method initializations to be performed before
c     starting a new shower.
c
c     WITH THINNING DISABLED, THIS IS JUST A PLACEHOLDER.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     primenergy...... (input, double precision) The primary energy
c                      in GeV.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  primenergy
c
c     FIRST EXECUTABLE STATEMENT
c
      return
      end
c     --- End of routine showerthinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'thininit_noth.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
