c
c     FILE: inicheck.f                      Creation date: 11/JUL/1996.
c                                       LAST MODIFICATION: 18/MAY/2004.
c
c     Checking the input data after scanning the input file.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine idatacheck(ouflag)
c
c     Checking the data obtained from the input file, setting
c     the corresponding internal variables and printing
c     summary information.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2003,
c                                         2004.
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           ouflag
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      logical           mandatoryfail, multipleset
      logical           notokinjgrd, notokehisto, notgoodobslev
      integer           i, j
      integer           thisdefi
      logical           thisdefl, magdef
      double precision  thisdef1, thisdef2
      double precision  tmpv(4)
      integer           itmpv(6)
      double precision  zfromdepth, depthfromz
c
c     FIRST EXECUTABLE STATEMENT
c
c
      multipleset   = .false.
      usedefaultd   = .false.
      usedefaults   = .false.
c
c     Checking the mandatory parameters (and related ones).
c
      mandatoryfail = .false.
c
      if (totshwset .le. 0) then
        mandatoryfail = .true.
        call errprint(ouflag, '$A27', 3, 'idatacheck',
     +                '"Total number of showers"',
     +                0, 0, 0, 0.d0, ' ')
      else if (totshwset .gt. 1) then
        multipleset = .true.
      endif
c
      if (firstshowerset .le. 0) then
        usedefaultd = .true.
      else if (firstshowerset .gt. 1) then
        multipleset = .true.
      endif
c
      if (runshwset .le. 0) then
        usedefaultd = .true.
      else if (runshwset .gt. 1) then
        multipleset = .true.
      endif
c
      if (cpurunset .le. 0) then
        usedefaultd = .true.
      else if (cpurunset .gt. 1) then
        multipleset = .true.
      endif
c
      if (processjobsset .le. 0) then
        usedefaultd = .true.
      else if (processjobsset .gt. 1) then
        multipleset = .true.
      endif
c
      if (nshprimary .le. 0) then
        mandatoryfail = .true.
        call errprint(ouflag, '$A27', 3, 'idatacheck',
     +                '"Primary particle type"',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
      if (pryeminset .le. 0) then
        mandatoryfail = .true.
        call errprint(ouflag, '$A27', 3, 'idatacheck',
     +                '"Energy of primary particle"',
     +                0, 0, 0, 0.d0, ' ')
      else if (pryeminset .gt. 1) then
        multipleset = .true.
      endif
c
c     Checking the task name.
c
      if (tasknameset .le. 0) then
        usedefaultd = .true.
      else if (tasknameset .gt. 1) then
        multipleset = .true.
      endif
c
c     Setting the atmospheric model and calling the corresponding
c     initializing routine.
c
      if (atmoslset .le. 0) then
        usedefaults = .true.
      else if (atmoslset .gt. 1) then
        multipleset = .true.
      endif
c
      call atmosinit(max(1, atmoslabel), atmosmodel)
c
c     Checking the remaining parameters and setting default values.
c
c     Setting the event date.
c
      if (fidatawaset(39) .le. 0) then
c
c       Using current date as default date.
c
        usedefaults  = .true.
        call intdati(itmpv)
        thisdef1  = itmpv(3) + 32 * itmpv(2) + 512 * itmpv(1)
        eventdate = -thisdef1
c
      else
        eventdate = fidata(1, 39)
        if (fidatawaset(39) .gt. 1) multipleset = .true.
      endif
c
c     Setting the site (default site is Site00).
c
      magdef = (iidatawaset(3) .le. 0)
      if (magdef) then
        usedefaults  = .true.
        isite = 0
      else
        isite = iidata(1, 3)
        if (iidatawaset(3) .gt. 1) multipleset = .true.
      endif
c
      if (siteground(isite) .lt. 0) then
        siteground(isite) = zfromdepth(abs(siteground(isite)), i)
      endif
c
c     Completing the parameters related to the Primary pcles.
c
      if (fidatawaset(1) .eq. pryeminset) then
        pryenergymax = fidata(1, 1)
      else
        pryenergymax = pryenergymin
      endif
c
c     Spectrum slope. Default is gamma = 1.7
c
      thisdef2 = 1.7d0
c
      if (fidatawaset(2) .eq. pryeminset) then
        pryenergyslp = fidata(1, 2)
      else
        pryenergyslp = thisdef2
      endif
c
c     Zenith defaults to zero.
c
      thisdef2 = 0.0d0
c
      if (fidatawaset(3) .le. 0) then
        usedefaults  = .true.
        pryzenithmin = thisdef2
        pryzenithmax = thisdef2
        varzendis    = 1
      else
        pryzenithmin = fidata(1, 3)
        pryzenithmax = fidata(1, 4)
        varzendis    = iidata(1, 1)
        if (fidatawaset(3) .gt. 1) multipleset = .true.
      endif
c
c     Azimuth defaults to zero if zenith is non-varying. Otherwise
c     the default is [0, 360].
c     Azimuth origin defaults to local magnetic north.
c
      thisdef1 = 0.0d0
      if (pryzenithmin .ge. pryzenithmax) then
        thisdef2 = 0.0d0
      else
        thisdef2 = 360.d0
      endif
      thisdefl = .false.
c
      if (fidatawaset(5) .le. 0) then
        usedefaults = .true.
        pryazimmin  = thisdef1
        pryazimmax  = thisdef2
      else
        pryazimmin  = fidata(1, 5)
        pryazimmax  = fidata(1, 6)
        if (fidatawaset(5) .gt. 1) multipleset = .true.
      endif
      if (lidatawaset(1) .le. 0) then
        usedefaults = .true.
        geognorth   = thisdefl
      else
        geognorth   = lidata(1, 1)
        if (lidatawaset(1) .gt. 1) multipleset = .true.
      endif
c
      if (thinningon) then
c
c       Thinning energy defaults to 10^{-4} Relative.
c
        thisdef2 = -1.0d-4
c
        if (fidatawaset(7) .le. 0) then
          usedefaults = .true.
          ethinpar    = thisdef2
        else
          ethinpar    = fidata(1, 7)
          if (fidatawaset(7) .gt. 1) multipleset = .true.
        endif
c
      else
        ethinpar = 0
      endif
c
c     Injection altitude defaults to 100 km.
c     (Notice that this is the "local" injection altitude, not the
c     "central" one. They are different for nonvertical showers in a
c     curved earth).
c
      thisdef2 = 100.d3
c
      if (fidatawaset(8) .le. 0) then
        usedefaults = .true.
        injz        = thisdef2
        injdepth    = depthfromz(thisdef2, atmlayer0)
      else
        if (fidata(1, 9) .lt. -1) then
          fidata(1, 9) = depthfromz(fidata(1, 8), atmlayer0)
        else
          fidata(1, 8) = zfromdepth(fidata(1, 9), atmlayer0)
        endif
        injz       = fidata(1, 8)
        injdepth   = fidata(1, 9)
        if (fidatawaset(8) .gt. 1) multipleset = .true.
      endif
c
c     Ground altitude defaults to the Site altitude.
c
      thisdef2 = siteground(isite)
c
      if (fidatawaset(10) .le. 0) then
        usedefaults = .true.
        groundz     = thisdef2
        groundepth  = depthfromz(thisdef2, i)
      else
        if (fidata(1, 11) .lt. -1) then
          fidata(1, 11) = depthfromz(fidata(1, 10), i)
        else
          fidata(1, 10) = zfromdepth(fidata(1, 11), i)
        endif
        groundz    = fidata(1, 10)
        groundepth = fidata(1, 11)
        if (fidatawaset(10) .gt. 1) multipleset = .true.
      endif
c
c     Consistency check of injection depth and ground depth.
c     Notice that this check cannot be performed earlier.
c
      notokinjgrd = (groundepth .le. injdepth) .or.
     +              (injdepth .le. 0) .or. (groundepth .le. 0)
c
c     Observing levels data.
c     Default: 19 observing levels from ground to top.
c
      thisdefi = 19
      thisdef1 = injdepth
      thisdef2 = groundepth
c
      notgoodobslev = .false.
      if (obslevset .le. 0) then
        usedefaults = .true.
        nobslevels  = thisdefi
        obslevstep  = (thisdef2 - thisdef1) / (nobslevels + 1)
        obslevmind  = thisdef1 + obslevstep
        obslevmaxd  = thisdef2 - obslevstep
      else
        if (iobslevdat(1) .lt. 0) then
c
c         Only the number of observing levels was specified.
c
          nobslevels = - iobslevdat(1)
          obslevstep = (thisdef2 - thisdef1) / (nobslevels + 1)
          obslevmind = thisdef1 + obslevstep
          obslevmaxd = thisdef2 - obslevstep
c
        else
c
c         Full specification.
c
          do i = 1, 2
            if (obszset(i)) then
              fobslevdat(1, i) = depthfromz(fobslevdat(1, i), j)
            endif
            tmpv(i) = fobslevdat(1, i)
          enddo
c
c         Checking minimum and maximum.
c
          notgoodobslev = (tmpv(1) .eq. tmpv(2))
          if (.not. notgoodobslev) then
c
c           Everything is OK. Completing assignment.
c
            fobslevdat(1, 1) = min(tmpv(1), tmpv(2))
            fobslevdat(1, 2) = max(tmpv(1), tmpv(2))
c
            nobslevels = iobslevdat(1)
            obslevmind = fobslevdat(1, 1)
            obslevmaxd = fobslevdat(1, 2)
            obslevstep = (obslevmaxd - obslevmind) / (nobslevels - 1)
          endif
        endif
        if (obslevset .gt. 1) multipleset = .true.
      endif
c
c     Observing levels derived quantities.
c     Notice that the value of obslevl0 can change for non vertical
c     showers if a curved earth is taken into account: This variable
c     can be reset in the shower initializing routine.
c
      nobslevelsp1 = nobslevels + 1
      obslevmaxz   = zfromdepth(obslevmind, i)
      obslevminz   = zfromdepth(obslevmaxd, i)
      if (obslevstep .ne. 0) then
        obslevca = 1.d0 / obslevstep
        obslevcb = 1.d0 - obslevca * obslevmind
        obslevl0 = obslevca * injdepth + obslevcb
        obslevl0 = max(0, obslevl0)
      endif
c
c     Geomagnetic field.
c
c     If enabled, the geomagnetic field defaults to the local
c     geomagnetic field (with no fluctuations) which is evaluated by
c     the corresponding model. Notice that the geomagnetic field is
c     always evaluated since it is needed to define the direction of
c     the x-axis.
c
      call geomagnetic(sitelat(isite), sitelong(isite),
     +         zfromdepth(0.6d0 * groundepth + 0.4 * injdepth, i),
     +         eventdate, geob, geobi, geobd)
      geobfluc = 0
c
      if (iidatawaset(4) .le. 0) then
        usedefaults = .true.
        if (magdef) then
          geobswitch  = 0
        else
          geobswitch  = 1
        endif
      else
        geobswitch  = iidata(1, 4)
        if (fidatawaset(35) .eq. iidatawaset(4)) then
          geob = fidata(1, 35)
        endif
        do i = 36, 38
          if (fidatawaset(i) .eq. iidatawaset(4)) then
            fidata(0, i) = fidata(1, i)
          endif
        enddo
        if (iidatawaset(4) .gt. 1) multipleset = .true.
      endif
c
c     Histogram energy limits default to 10 Mev and
c     max(75% of pryenergymin, 10 TeV)
c
      thisdef1 = 10.0d-3
      thisdef2 = max(0.75d0 * pryenergymin, 10.0d3)
c
      if (fidatawaset(12) .le. 0) then
        usedefaults = .true.
        eminhis     = thisdef1
        emaxhis     = thisdef2
      else
        if (fidata(1, 12) .ge. 0) then
          eminhis = fidata(1, 12)
        else
          eminhis = abs(fidata(1, 12)) * pryenergymin
        endif
        if (fidata(1, 13) .ge. 0) then
          emaxhis = fidata(1, 13)
        else
          emaxhis = abs(fidata(1, 13)) * pryenergymin
        endif
        if (fidatawaset(12) .gt. 1) multipleset = .true.
      endif
c
c     Consistency check of minimum and maximum histogram
c     energies.
c     Notice that this check cannot be performed earlier.
c
      notokehisto = (emaxhis .le. eminhis)
c
c     Histogram radial limits default to 50 m and 2 km
c     File radial limits default to 250 m and 12 km (9 different
c     files supported by default).
c
      thisdef1 = 50.0d0
      thisdef2 =  2.0d3
c
      do i = 14, 32, 2
        j = i + 1
c
        if (fidatawaset(i) .le. 0) then
          usedefaults  = .true.
          fidata(0, i) = thisdef1
          fidata(0, j) = thisdef2
        else
          fidata(0, i) = fidata(1, i)
          fidata(0, j) = fidata(1, j)
          if (fidatawaset(i) .gt. 1) multipleset = .true.
        endif
c
        thisdef1 = 250.0d0
        thisdef2 =  12.0d3
c
      enddo
c
c     The separate showers index defaults to -1 which means that
c     compressed output files are not split.
c
      if (iidatawaset(2) .le. 0) then
        usedefaults  = .true.
        iidata(0, 2) = -1
      else
        iidata(0, 2) = iidata(1, 2)
        if (iidatawaset(i) .gt. 1) multipleset = .true.
      endif
c
c     Random number generator seed.
c
      if (fidatawaset(50) .le. 0) then
        usedefaults = .true.
        inputrseed  = 0
      else
        inputrseed  = fidata(1, 50)
        if (fidatawaset(50) .gt. 1) multipleset = .true.
      endif
c
c     Setting defaults for the non-defined "not-hardwired" parameters.
c
      do i = nfidata0, nfidata
        if (fidatawaset(i) .le. 0) then
          usedefaults = .true.
        else
          fidata(0, i) = fidata(1, i)
          if (fidatawaset(i) .gt. 1) multipleset = .true.
        endif
      enddo
c
      do i = niidata0, niidata
        if (iidatawaset(i) .le. 0) then
          usedefaults = .true.
        else
          iidata(0, i) = iidata(1, i)
          if (iidatawaset(i) .gt. 1) multipleset = .true.
        endif
      enddo
c
      do i = nlidata0, nlidata
        if (lidatawaset(i) .le. 0) then
          usedefaults = .true.
        else
          lidata(0, i) = lidata(1, i)
          if (lidatawaset(i) .gt. 1) multipleset = .true.
        endif
      enddo
c
      do i = nsidata0, nsidata
        if (sidatawaset(i) .le. 0) then
          usedefaults = .true.
        else
          sidata(0, i) = sidata(1, i)
          if (sidatawaset(i) .gt. 1) multipleset = .true.
        endif
      enddo
c
      usedefault = (usedefaultd .or. usedefaults)
c
c     Reporting on the parameters in effect.
c
      call putlog(ouflag, .true.,
     +            'Displaying a summary of the input directives:')
c
      call inpsry(ouflag)
c
c     Printing out additional messages.
c
      if (ninierrors .gt. 0) call errprint(ouflag, '$A72', 2,
     +                            'idatacheck', ' ',
     +                            1, ninierrors, 0, 0.d0, ' ')
c
      if (multipleset) call errprint(ouflag, '$A28', 2, 'idatacheck',
     +                               ' ', 0, 0, 0, 0.d0, ' ')
c
c     Mandatory parameters were not set. Stopping.
c
      if (mandatoryfail) call errprint(ouflag, '$A29', 4,
     +                                 'idatacheck', ' ',
     +                                 0, 0, 0, 0.d0, ' ')
c
c     Inconsistent injection depth and ground depth. Stopping.
c
      if (notokinjgrd) then
        tmpv(1) = injdepth
        tmpv(2) = groundepth
        tmpv(3) = injz
        tmpv(4) = groundz
        call errprint(ouflag, '$A41', 4, 'idatacheck', ' ',
     +                0, 0, 4, tmpv, ' ')
      endif
c
c     Inconsistent minimum and maximum altitude of obs. levels.
c
      if (notgoodobslev) then
        tmpv(1) = fobslevdat(1, 1)
        tmpv(2) = fobslevdat(1, 2)
        call errprint(ouflag, '$A39', 4, 'idatacheck', ' ',
     +                0, 0, 2, tmpv, ' ')
      endif
c
c     Inconsistent energies for histograms. Stopping.
c
      if (notokehisto) then
        tmpv(1) = eminhis
        tmpv(2) = emaxhis
        call errprint(ouflag, '$A42', 4, 'idatacheck', ' ',
     +                0, 0, 2, tmpv, ' ')
      endif
c
c     Checking consistency between additional parameters, generally
c     "not-hardwired" ones. (These checks cannot be made earlier).
c
      call ispecialcheck(ouflag)
c
c     Checking consistency of external model. Directive ForceModelName
c     activates this feature with forces that a determined external
c     model must be used to start the simulations.
c
      if (nforcextset .gt. 1) then
        call errprint(ouflag, '$A45', 2, 'idatacheck',
     +   'ForceModelName directive used multiple times.',
     +   0, 0, 0, 0.d0, ' ')
      endif
      if (notforcext) then
        call extmodelname(auxline, i)
        call errprint(ouflag, '$A45', 4, 'idatacheck',
     +   'Parameter of ForceModelName directive does not match ' //
     +   'the current one:',
     +   0, 0, 0, 0.d0, auxline(1:i))
      endif
c
c     Checking consistence of compressed file parameters.
c
      call checkciopars
c
      return
      end
c     --- End of routine idatacheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ispecialcheck(ouflag)
c
c     Checking the data obtained from the input file: Special checks
c     to do after the basic tests.
c     It is assumed that the input file has already been scanned,
c     and all the basic variables have been set.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of arguments.
c
      integer           ouflag
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  maxmine(2)
      double precision  getinpreal
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking that the primary energy is higher than all the cut
c     energies.
c
      maxmine(2) = max(getinpreal('GammaCutEnergy'),
     +                 getinpreal('ElectronCutEnerg'),
     +                 getinpreal('MuonCutEnergy'),
     +                 getinpreal('MesonCutEnergy'),
     +                 getinpreal('NuclCutEnergy'))

      if (pryenergymin .le. maxmine(2)) then
        maxmine(1) = pryenergymin
        call errprint(ouflag, '$A79', 4, 'ispecialcheck', ' ',
     +                0, 0, 2, maxmine, ' ')
      endif
c
      return
      end
c     --- End of routine ispecialcheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine checkciopars
c
c     Consistency checks of compressed files parameters.
c
c     Written by: S. J. Sciutto, La Plata 2004.
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
c     Declaration of internal variables and arrays.
c
      integer           j
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking rlims for ground particle file.
c
      if (gpcleciofile .gt. 0) then
        j = 14 + 2 * gpcleciofile
        if (fidata(0, j) .le. 0) then
          call errprint(0, '*', 4, 'checkciopars',
     +      'Minimum lateral limit for ground particle file ' //
     +      'is not positive.',
     +      0, 0, 1, fidata(0, j), ' ')
        endif
      endif
c
      return
      end
c     --- End of routine checkciopars
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'inicheck.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
