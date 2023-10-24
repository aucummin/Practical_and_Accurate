c
c     FILE: scheduler.f                     Creation date: 13/NOV/1996.
c                                       LAST MODIFICATION: 28/JAN/1999.
c
c     This file contains the routine which scans the different particle
c     stacks.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine scheduler0(currpsize, notempty)
c
c     Scanning the particle stacks.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c     Parameters:
c     ==========
c
c     currpsize....... (input-output, integer) Accumulated number of
c                      processed records.
c     notempty........ (output, logical) True if a not empty stack was
c                      processed. False if every stack is empty.
c
c
c      use iso_c_binding
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'       (included by pstackpar.f)
      include 'pstackpar.f'
      include 'schedulerpar.f'
c
c     Declaration of arguments.
c
      integer           currpsize
      logical           notempty
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'pstackcomm.f'
c
c     Declaration of stack processing routines.
c
      include 'schedulerfun.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      notempty = .false.
c
c     Processing the stacks.
c
c     The contents of this section must be set manually accordingly
c     with the defined stack structure. Notice the inclusion of the
c     "greenlight" switchs for all stacks.
c
      if (greenlight(gamma_sta) .and.
     +    (stasize(gamma_sta) .gt. 0)) then
        notempty  = .true.
        currpsize = currpsize + nstaentr(gamma_sta)
        callcounter(2, gamma_sta) = callcounter(2, gamma_sta) + 1
        call adscansta(gammaadv, gammadecay, gamma_sta)
      endif
c
      if (greenlight(eplumin_sta) .and.
     +    (stasize(eplumin_sta) .gt. 0)) then
        notempty  = .true.
        currpsize = currpsize + nstaentr(eplumin_sta)
        callcounter(2, eplumin_sta) = callcounter(2, eplumin_sta) + 1
        call adscansta0(epmadv, epmdecay, eplumin_sta)
      endif
c
      if (greenlight(heavynt_sta) .and.
     +    (stasize(heavynt_sta) .gt. 0)) then
        notempty  = .true.
        currpsize = currpsize + nstaentr(heavynt_sta)
        callcounter(2, heavynt_sta) = callcounter(2, heavynt_sta) + 1
        call adscansta(heavyntadv, heavyntdecay, heavynt_sta)
      endif
c
      if (greenlight(heavych_sta) .and.
     +    (stasize(heavych_sta) .gt. 0)) then
        notempty  = .true.
        currpsize = currpsize + nstaentr(heavych_sta)
        callcounter(2, heavych_sta) = callcounter(2, heavych_sta) + 1
        call adscansta0(heavychadv, heavychdecay, heavych_sta)
      endif
c
      return
c
      end
c     --- End of routine scheduler0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'scheduler.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
