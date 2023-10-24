c
c     FILE: thininit.f                      Creation date: 07/APR/1998.
c                                       LAST MODIFICATION: 01/MAY/2003.
c
c     This file contains initialization routines for the AIRES
c     thinning algorithm.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine thininit0(thinningison)
c
c     Standard thinning method initializations to be performed before
c     scanning the input file.
c
c     Written by: S. J. Sciutto, La Plata 1998, 2001, 2002, 2003.
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
      thinningison = .true.
c
c     Creating IDL directives to set the maximum weight parameters.
c
      call newinpreal(1, 'ThinningWFactor', 10,
     +                'Max. stat. weight factor', 0,
     +                12.0d0, 1, 1.0d-10, 0.d0)
c
      call newinpreal(1, 'EMtoHadronWFRatio', 12,
     +                'EM to Hadron WF ratio', 0,
     +                88.0d0, 3, 1.0d0, 1.d10)
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
c
c     Written by: S. J. Sciutto, La Plata 1998, 2001.
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
      double precision  getinpreal
c
c     FIRST EXECUTABLE STATEMENT
c
      thinmaxwfactor = 14.0d0 * getinpreal('ThinningWFactor')
      emtohwfratio   = getinpreal('EMtoHadronWFRatio')
c
      do i = 1, npstacks
        thinmaxwp(i)  = 8 * thinmaxw(i)
        thinmaxwg(i)  = thinmaxw(i) / 4
        thinmaxdel(i) = thinmaxw(i) - thinmaxwg(i)
      enddo
c
c     Minimum thinning energy.
c
      minethin = 1.001d0 * max(nucutenergy,
     +                         cutenergy(pipluscode, 1),
     +                         cutenergy(mupluscode, 1),
     +                         cutenergy(electroncode, 1),
     +                         cutenergy(gammacode, 1))
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
c     Written by: S. J. Sciutto, La Plata 1998, 1999, 2001.
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
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'schedulerpar.f'
c
c     Declaration of arguments.
c
      double precision  primenergy
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'thincomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  eth(2)
      double precision  thinmaxtmp
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the absolute thinning energy.
c
      if (ethinpar .lt. 0) then
        currethin = primenergy * abs(ethinpar)
      else
        currethin = ethinpar
      endif
c
c     Checking consistency of thinning energy and cut energies.
c
      if (currethin .lt. minethin) then
        eth(1) = currethin
        eth(2) = minethin
        call errprint(2, '$A43', 2, 'showerthinit', ' ',
     +                0, 0, 2, eth, ' ')
        currethin = minethin
      endif
c
c     Setting the maximum weight associated variables.
c
      thinmaxtmp = max(thinmaxwfactor * currethin, 80.d0)
      do i = 1, npstacks
        thinmaxwp(i) = thinmaxtmp
      enddo
c
      thinmaxtmp = max(thinmaxtmp / emtohwfratio, 80.d0)
      thinmaxwp(heavynt_sta) = thinmaxtmp
      thinmaxwp(heavych_sta) = thinmaxtmp
c
      do i = 1, npstacks
        thinmaxw(i)   = thinmaxwp(i) / 8
        thinmaxwg(i)  = thinmaxw(i)  / 4
        thinmaxdel(i) = thinmaxw(i) - thinmaxwg(i)
      enddo
c
c     Setting the global thinning marker to a random value.
c
      thinmarker = urandom() * currethin
c
      return
      end
c     --- End of routine showerthinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'thininit.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
