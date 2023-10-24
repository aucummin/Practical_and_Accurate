c
c     FILE: stackprimary.f                  Creation date: 12/OCT/1999.
c                                       LAST MODIFICATION: 18/JUL/2003.
c
c     Routines related with the stacking of the primary particle(s).
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stackprimary(primcode, primener,
     +                        injpos, injectdepth, showert0,
     +                        uprim, obslev0, atlayer0)
c
c     Stacking the primary particle(s).
c
c     Written by: S. J. Sciutto, La Plata 1999, 2000, 2003.
c
c
c     Arguments:
c     =========
c
c     primcode........ (input, integer) Code of primary particle.
c     primener........ (input, double precision) Primary energy.
c     injpos.......... (input, double precision, array(4)) Position
c                      of injection point.
c     injectdepth..... (input, double precision) Atmospheric depth of
c                      injection point.
c     showert0........ (input, double precision) Shower global time
c                      shift.
c     uprim........... (input, double precision, array(3)) Unitary
c                      vector pointing the direction of motion of the
c                      primary particle.
c     obslev0(2),
c     atlayer0........ (input, integer) Internal information about
c                      observing levels and atmospheric layers.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'pclepar.f'
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           primcode
      double precision  primener
      double precision  injpos(4)
      double precision  injectdepth, showert0
      double precision  uprim(3)
      integer           obslev0(2)
      integer           atlayer0
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'pclecomm.f'
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           l, spmagic, isp, npstok, irc
      logical           logsw1
      double precision  clockrandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking particle code.
c
      specialprim = ((primcode .ge. pescode1) .and.
     +               (primcode .le. lastescpcle))
c
      if (specialprim) then
c
c       The primary code is a "special" particle (escape code).
c       This means that the primary (or primaries) will be evaluated
c       via a call to an external module (spawned process).
c
c       The following number is to uniquely identify this call.
c
        spmagic = 1000000000 * clockrandom()
c
c       Initializing the interface.
c
        call spinit(primcode, primener, injpos, injectdepth,
     +              showert0, uprim, spmagic)
c
        isp            = primcode - pescode1 + 1
        specialprimlab = isp
        logsw1         = (sprimlog .ge. 1)
c
c       Spawing the external process to generate the special
c       primary(ies).
c
        if (logsw1) then
          auxline = 'Invoking module for special primary ' //
     +              escpclename(isp)
          call strim(176, auxline, l)
          call putlog(2, .true., auxline(1:l))
        endif
c
        call sysspawn(escpclemacro(escmacropos(2, isp) + 1:
     +                             escmacropos(3, isp)),
     +                ' ', ' ', irc)
c
        if (irc .ne. 0) then
          call errprint(2, '*', 2, 'stackprimary',
     +         'Nonzero return code from external macro.',
     +         1, irc, 0, 0.d0,
     +         escpclemacro(escmacropos(2, isp) + 1:
     +                      escmacropos(3, isp)))
        endif
c
        if (logsw1) call putlog(2, .true.,
     +                   'Control returned from external module.')
c
c       Processing information coming from the external module
c       and stacking the "special" primary(ies).
c       When using special primaries, the first interaction is
c       set manually within "spafter"
c
        call spafter(isp, primener, injpos, injectdepth, showert0,
     +               uprim, obslev0, atlayer0, spmagic,
     +               nspecialprim)
c
c       The first interaction is labelled as "already set".
c
        fstintnotset = .false.
c
c       At this point, the number of stacked particles could be very
c       large. It is therefore necessary to check stack sizes and
c       set stack green lights before continuing with the simulation.
c
        call stacklights
c
      else
c
c       The primary is a normal partice or nucleus. Stacking.
c
c       Notice that weight is 1 and time is -showert0. The last hadronic
c       process depth is set to zero (top of the atmosphere).
c       Notice also the inclusion of parameters obslevl0 and atlayer
c       which define the position of the primary in the
c       atmosphere and with respect to the observing levels.
c
        call stacknpcleu(1, 1, primcode, primener, 1.d0,
     +                   fstposdp, injectdepth, -showert0,
     +                   0.d0, 3, uprim, obslev0, atlayer0, npstok)
c
        if (npstok .ne. 1) then
          call errprint(2, '*', 4, 'stackprimary',
     +                  'No primary particles to process.',
     +                  0, 0, 0, 0.d0, ' ')
        endif
c
c       Setting the switch to determine the first interaction depth,
c       and related variables.
c       fstintauto is currently set to "true" to allow the corresponding
c       routine to set the first interaction depth accordingly with
c       data obtained from the particle stack.
c
        fstintnotset = .true.
        fstintauto   = .true.
        fstdpmanual  = injectdepth
c
      endif
c
      return
c
      end
c     --- End of routine stackprimary
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine spinit(primcode, primener, injpos, injectdepth,
     +                  showert0, uprim, spmagic)
c
c     Initializing the interface for the "special" primary particle
c     external process.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     primcode........ (input, integer) Code of primary particle. It
c                      belongs to the range of "special" (escape)
c                      particle codes.
c     primener........ (input, double precision) Primary energy.
c     injpos.......... (input, double precision, array(4)) Position
c                      of injection point.
c     injectdepth..... (input, double precision) Atmospheric depth of
c                      injection point.
c     showert0........ (input, double precision) Shower global time
c                      shift.
c     uprim........... (input, double precision, array(3)) Unitary
c                      vector pointing the direction of motion of the
c                      primary particle.
c     spmagic......... (input, integer) Integer number uniquely
c                      identifying the current call.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
      include 'initpar.f'
      include 'kernelpar.f'
      include 'pclepar.f'
      include 'spipar.f'
c
c     Declaration of arguments.
c
      integer           primcode
      double precision  primener
      double precision  injpos(4)
      double precision  injectdepth, showert0
      double precision  uprim(3)
      integer           spmagic
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
      include 'atmosdata.f'
      include 'randomdata.f'
c
c     Declaration of internal variables and arrays.
c
      character*24      version
      integer           i, j, i1, i2, i3, isp
c
c     FIRST EXECUTABLE STATEMENT
c
c     Opening the interface file.
c
      open(55, file = spiifile, status = 'UNKNOWN',
     +         form = 'UNFORMATTED', err = 3010)
c
c     Writing the header.
c
      version = aires_version
c
      write(55, err = 3010) spiifileheader0, spiintheader0,
     +                      version, aires_version_no, spmagic
c
c     Writing task and shower information.
c
      write(55, err = 3010) tasknamever, tasknamelen
      if (tasknamelen .gt. 0) then
        write(55, err = 3010) taskname(1:tasknamelen)
      endif
      write(55, err = 3010) cruser, crhost
c
      write(55, err = 3010) currshower, mtotshowers,
     +                      firstshowernon1
c
c     Physical shower variables.
c
      write(55, err = 3010) primcode, primener, injectdepth, showert0,
     +                      groundz, groundepth
      write(55, err = 3010) (injpos(i), i = 1, 4)
      write(55, err = 3010) (uprim(i), i = 1, 3)
c
c     Variables related with the "special primary particle".
c
      isp = primcode - pescode1 + 1
c
      write(55, err = 3010) escpclename(isp)
      write(55, err = 3010) escmacrover(isp)
c
      i3 = escmacropos(4, isp - 1)
      i1 = escmacropos(2, isp) - i3
      i2 = escmacropos(3, isp) - i3
      i3 = escmacropos(4, isp) - i3
c
      write(55, err = 3010) i1, i2, i3
      write(55, err = 3010)
     +          escpclemacro(escmacropos(1, isp):escmacropos(4, isp))
c
c     Writing the special internal data arrays.
c
      write(55, err = 3010) nintintvar, nintfltvar, recordspvar0
      write(55, err = 3010) mxintintvar, mxintfltvar
c
      if (nintintvar .gt. 0) then
        write(55, err = 3010) (spintvar(i), i = 1, nintintvar)
      endif
      if (nintfltvar .gt. 0) then
        write(55, err = 3010) (spfltvar(i), i = 1, nintfltvar)
      endif
c
c     Writing some additional spare fields for future use.
c
      i1 = 0
      write(55, err = 3010) (i1, i = 0, 9)
c
c     Transferring global variables.
c
      write(55, err = 3010) thereareglobars, nglobar(1), nglobar(2)
c
      do j = 1, 2
        if (nglobar(j) .gt. 0) then
          write(55, err = 3010) (globnam(i, j), i = 1, nglobar(j)),
     +                          (globlen(i, j), i = 1, nglobar(j))
          write(55, err = 3010) globstrlen(j),
     +                          (globdfend(i, j), i = 0, nglobar(j))
          if (globstrlen(j) .gt. 0) then
            write(55, err = 3010) globarstring(j)(1:globstrlen(j))
          endif
        endif
      enddo
c
c     Transferring the data corresponding to the atmospheric model.
c
      write(55, err = 3010) nlayers, nlayers1, nlayersp1
      write(55, err = 3010) (alaylimx(i), i = 0, nlayers),
     +                      (alaylimz(i), i = 0, nlayers)
      write(55, err = 3010) (alaya1(i), i = 0, nlayers),
     +                      (alaya2(i), i = 0, nlayers)
      write(55, err = 3010) (alayb1(i), i = 0, nlayers),
     +                      (alayb2(i), i = 0, nlayers)
      write(55, err = 3010) (alayd1(i), i = 0, nlayers),
     +                      (alayd2(i), i = 0, nlayers)
c
c     Transferring the current status of the random number generator.
c     (Random generator identification is set to 1 in this version,
c     0 corresponds to previous versions where the original seed was
c     not saved).
c
      i = 1
      write(55, err = 3010) i
      write(55, err = 3010) xran, xran2, original_seed
c
c     Writing some additional spare fields for future use.
c
      i1 = 0
      write(55, err = 3010) (i1, i = 0, 9)
c
c     Transferring additional data needed in some particular cases.
c
c     Shower primary general data.
c
      write(55, err = 3010) nshprimary, pryenergymin, pryeminset
      if (nshprimary .gt. 0)
     +  write(55, err = 3010) (shprimary(i), i = 1, nshprimary),
     +                        (shprimarywt(i), i = 1, nshprimary),
     +                        (shprimarywt0(i), i = 1, nshprimary)
c
c     Main input data arrays, and site library.
c
      i = 0
      write(55, err = 3010) nfidata, niidata, nlidata,
     +                      nsidata, sidatalen, nlibsites,
     +                      (i, j = 1, 5)
      write(55, err = 3010) nfidata0, niidata0, nlidata0,
     +                      nsidata0, sidatalen0,
     +                      (i, j = 1, 6)
c
      if (nfidata .gt. 0) then
        write(55, err = 3010)
     +            ((fidata(j, i), j = 0, 3), i = 1, nfidata),
     +            (fidatawaset(i), i = 1, nfidata),
     +            (fdbdry(i), i = 1, nfidata)
      endif
c
      if (niidata .gt. 0) then
        write(55, err = 3010)
     +            ((iidata(j, i), j = 0, 3), i = 1, niidata),
     +            (iidatawaset(i), i = 1, niidata),
     +            (idbdry(i), i = 1, niidata)
      endif
c
      if (nlidata .gt. 0) then
        write(55, err = 3010)
     +            ((lidata(j, i), j = 0, 2), i = 1, nlidata),
     +            (lidatawaset(i), i = 1, nlidata)
      endif
c
      if (nsidata .gt. 0) then
        write(55, err = 3010)
     +            ((sidata(j, i), j = 0, 6), i = 1, nsidata),
     +            (sidatawaset(i), i = 1, nsidata)
        write(55, err = 3010) sidatastring(1:sidatalen)
      endif
c
c     Saving the site library.
c
      if (nlibsites .gt. 0) then
        write(55, err = 3010) (sitename(i), i = 0, nlibsites)
        write(55, err = 3010) (sitenlen(i), i = 0, nlibsites)
        write(55, err = 3010) (sitelat(i), i = 0, nlibsites)
        write(55, err = 3010) (sitelong(i), i = 0, nlibsites)
        write(55, err = 3010) (siteground(i), i = 0, nlibsites)
      endif
c
c     Writing some additional spare fields for future use.
c
      i1 = 0
      write(55, err = 3010) (i1, i = 0, 9)
c
      close(55)
      return
c
c     Error messages.
c
 3010 continue
      call errprint(2, '$A09', 4, 'spinit',
     +     '(Corresponds to external module interchange file)',
     +     1, 55, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine spinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine spafter(isp, primener, injpos, injectdepth,
     +                   showert0, uprim, obslev0, atlayer0, spmagic,
     +                   nprimp)
c
c     Processing the data generated by the special particles external
c     module.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2000, 2003.
c
c
c     Arguments:
c     =========
c
c     isp............. (input, integer) "Special" particle label.
c     primener........ (input, double precision) Primary energy.
c     injpos.......... (input, double precision, array(4)) Position
c                      of injection point.
c     injectdepth..... (input, double precision) Atmospheric depth of
c                      injection point.
c     showert0........ (input, double precision) Shower global time
c                      shift.
c     uprim........... (input, double precision, array(3)) Unitary
c                      vector pointing the direction of motion of the
c                      primary particle.
c     obslev0(2),
c     atlayer0........ (input, integer) Internal information about
c                      observing levels and atmospheric layers.
c     spmagic......... (input, integer) Integer number uniquely
c                      identifying the current call.
c     nprimp.......... (output, integer) The number of primary
c                      particles generated by the external module.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
      include 'initpar.f'
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'kernelpar.f'
      include 'spipar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           isp
      double precision  primener
      double precision  injectdepth, showert0
      double precision  uprim(3)
      double precision  injpos(4)
      integer           obslev0(2)
      integer           atlayer0, spmagic, nprimp
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'pclecomm.f'
      include 'kernelcomm.f'
      include 'randomdata.f'
c
c     Declaration of internal variables and arrays.
c
      logical           logsw1, logsw2
      character*24      version, header0
      integer           iversion, iheader0, spmagic0
      integer           ut, i, i1, i2, irc, thisshower
      integer           usrmacrover
      integer           nprimp0, nprimps, nprimpnull
      double precision  eprimp, eprimp0, totprimp
      integer           ppcode, atlayer1
      integer           obslev1(2)
      double precision  pptime, ppdepth
      double precision  pppos(4)
      double precision  ftmp1, rdata(5), codata(15)
      integer           cidata(15), ciws(15)
      logical           injbad
      integer           set1sti, npstok
      double precision  r1sti(3)
      double precision  depthfromz, xvfromxs
c
c     FIRST EXECUTABLE STATEMENT
c
      logsw1 = (sprimlog .ge. 1)
      logsw2 = (sprimlog .ge. 2)
c
c     Opening the interface files.
c
      open(56, file = spiifile, status = 'OLD',
     +         form = 'UNFORMATTED', err = 3010)
c
      open(55, file = spiofile, status = 'OLD',
     +         form = 'UNFORMATTED', err = 3010)
c
      do ut = 55, 56
c
c       Reading and checking the header.
c
        read(ut, end = 3010, err = 3010) header0, iheader0,
     +                                   version, iversion, spmagic0
c
        if ((header0 .ne. spiofileheader0) .or.
     +      (spmagic0 .ne. spmagic)) then
          call errprint(2, '$P30', 4, 'spafter',
     +                  'Invalid file header.',
     +                  1, ut, 0, 0.d0, ' ')
        endif
c
c       Recovering relevant task and shower information.
c
        read(ut, end = 3010, err = 3010) thisshower
c
        if (thisshower .ne. currshower) then
          call errprint(2, '$P30', 4, 'spafter',
     +                  'Invalid recovered shower number.',
     +                  1, ut, 0, 0.d0, ' ')
        endif
c
      enddo
c
      if (iversion .ne. aires_version_no) then
        call errprint(2, '*', 2, 'spafter',
     +    'External module does not use the current AIRES version.',
     +    0, 0, 0, 0.d0,
     +    escpclemacro(escmacropos(1, isp):escmacropos(2, isp)))
      endif
c
c     Reading module version and return code information.
c
      read(56, end = 3010, err = 3010) usrmacrover, irc
c
      if (irc .ne. 0) then
        i = 1 + abs(irc) / 10
        call errprint(2, '*', i, 'spafter',
     +       'Nonzero return code from external macro.',
     +       1, irc, 0, 0.d0,
     +       escpclemacro(escmacropos(2, isp) + 1:
     +                    escmacropos(3, isp)))
      endif
c
      if ((escmacrouse(isp) .gt. 0) .and.
     +    (escmacrover(isp) .ne. usrmacrover)) then
        call errprint(2, '*', 2, 'spafter',
     +  'Current external macro version differs from the one used$' //
     +  'for the previous shower. Current version is listed',
     +  1, usrmacrover, 0, 0.d0,
     +  escpclemacro(escmacropos(1, isp):escmacropos(2, isp)))
      endif
c
      escmacrover(isp) = usrmacrover
c
c     Recovering the special internal data arrays.
c
      read(56, end = 3010, err = 3010)
     +          nintintvar, nintfltvar, recordspvar0
      read(56, end = 3010, err = 3010) i1, i2
c
      if (nintintvar .gt. 0) then
        if (nintintvar .gt. mxintintvar) goto 3010
        read(56, end = 3010, err = 3010)
     +            (spintvar(i), i = 1, nintintvar)
      endif
      if (nintfltvar .gt. 0) then
        if (nintfltvar .gt. mxintfltvar) goto 3010
        read(56, end = 3010, err = 3010)
     +            (spfltvar(i), i = 1, nintfltvar)
      endif
c
c     Recovering dynamic global variables.
c
      read(56, end = 3010, err = 3010) nglobar(1)
c
      thereareglobars = ((nglobar(1) + nglobar(2)) .gt. 0)
c
      if (nglobar(1) .gt. 0) then
        read(56, end = 3010, err = 3010)
     +            (globnam(i, 1), i = 1, nglobar(1)),
     +            (globlen(i, 1), i = 1, nglobar(1))
        read(56, end = 3010, err = 3010)
     +            globstrlen(1),
     +            (globdfend(i, 1), i = 0, nglobar(1))
        if (globstrlen(1) .gt. 0) then
          read(56, end = 3010, err = 3010)
     +              globarstring(1)(1:globstrlen(1))
        endif
      endif
c
c     Recovering the current status of the random number generator.
c     (Notice that the original seed is not recovered).
c
      read(56, end = 3010, err = 3010) i
      read(56, end = 3010, err = 3010) xran, xran2, ftmp1
c
c     Reading primary particle data..
c
      read(56, end = 3010, err = 3010) nprimp0, eprimp0
c
c     Reading 1st interaction information.
c
      read(56, end = 3010, err = 3010) set1sti,
     +                                 (r1sti(i), i = 1, 3)
c
c     Setting the first interaction depth and related variables, and
c     processing the "beginning of shower" related data.
c
      if (set1sti .eq. 1) then
        fstdpmanual = depthfromz(sqrt(r1sti(1) ** 2 + r1sti(2) ** 2 +
     +                                (rearth + r1sti(3)) ** 2)
     +                           - rearth, i)
      else if (set1sti .eq. 2) then
        fstdpmanual = r1sti(3)
      else if (set1sti .eq. 3) then
        fstdpmanual = xvfromxs(r1sti(3), 0.d0, abs(uprim(3)), groundz)
      else
        fstdpmanual = injectdepth
      endif
c
      fstintauto = .false.
      call setfirstint(0, 0, 0)
c
c     Processing the primary particle file.
c
      totprimp   = 0
      nprimp     = 0
      nprimps    = 0
      nprimpnull = 0
      eprimp     = 0
c
      if (logsw2) then
        call putlog(2, .false., 'Summary of special primary data')
        call putlog(2, .false.,
     +       '(Lengths in m, times in ns, energies in GeV):')
      endif
c
c     Initial injection position is the injection point.
c
      do i = 1, 4
        pppos(i)      = injpos(i)
        codata(i + 4) = 0
      enddo
      ppdepth    = injectdepth
      codata(8)  = injectdepth
      pptime     = -showert0
      codata(9)  = 0
      atlayer1   = atlayer0
      obslev1(1) = obslev0(1)
      obslev1(2) = obslev0(2)
      injbad     = .false.
c
 1010 continue
c
      read(55, end = 3010, err = 3010)
     +         ppcode, (rdata(i), i = 1, 5)
c
c     The "rdata" fields have different meanings, accordingly with
c     the value of ppcode.
c
      if (ppcode .eq. -999999) goto 1020
c
      if (ppcode .eq. injpointcode) then
c
c       Changing the injection point.
c
c       The "rdata" fields give the new injection position and time.
c
        do i = 1, 3
          pppos(i)      = rdata(i)
          codata(i + 4) = rdata(i) - injpos(i)
        enddo
        pppos(4)   = sqrt(rdata(1) ** 2 + rdata(2) ** 2 +
     +                    (rdata(3) + rearth) ** 2)
     +               - rearth
        ppdepth    = depthfromz(pppos(4), atlayer1)
        codata(8)  = ppdepth
        pptime     = rdata(4)
        codata(9)  = pptime + showert0
        obslev1(1) = obslevca * ppdepth + obslevcb
        obslev1(1) = max(0, min(obslev1(1), totobslev))
        injbad     = (ppdepth .gt. groundepth)
c
        call salastol(pppos, obslev1(1), obslev1(2))
c
        if (logsw2 .or. injbad) then
          write(auxline, 2010) 'Injection point:',
     +            pppos(1), pppos(2), pppos(3),
     +            (pptime + showert0) / cspeedns
          call putlog(2, .false., auxline(1:72))
        endif
 2010   format(a, 1p, 4g14.6)
c
        if (injbad) then
          call errprint(2, '*', 2, 'spafter',
     +     'Specified injection point is located below ground level.',
     +     0, 0, 0, 0.d0, ' ')
        endif
c
      else if (ppcode .eq. nullcode) then
c
c       Null (unphysical) particle.
c
c       The "rdata(1)" field gives the particle kinetic energy.
c
        call notapcle(rdata(1), rdata(5))
c
        nprimpnull = nprimpnull + 1
        eprimp     = eprimp + rdata(1) * rdata(5)
c
        if (logsw2) then
          write(auxline, 2010)
     +            'Unphysical primary: Energy and weight',
     +            rdata(1), rdata(5)
          call putlog(2, .false., auxline(1:65))
        endif
c
      else
c
c       Normal particle.
c
c       The "rdata" fields give the particle kinetic energy (GeV), the
c       direction of motion with respect to the AIRES coordinate
c       system, and the particle weight.
c
c       Notice that the weight is given externally and that the time is
c       already corrected by subtracting showert0. The last hadronic
c       process depth is set to zero (top of the atmosphere).
c       Notice also that the parameters obslevl1 and atlayer1 are
c       calculated for each particle.
c
        nprimp   = nprimp + 1
c
        if (logsw2 .or. injbad) then
          write(auxline, 2020)
     +            'Primary: Code, energy, weight',
     +            ppcode, rdata(1), rdata(5)
          call putlog(2, .false., auxline(1:71))
        endif
 2020   format(a, i5, 1p, 2g14.6)
c
        if (injbad) then
          call errprint(2, '*', 3, 'spafter',
     +     'Bad injection point. Primary particle ignored.',
     +     0, 0, 0, 0.d0, ' ')
        else
c
c         Stacking.
c
          call stacknpcleu(1, 1, ppcode, rdata(1), rdata(5),
     +                     pppos, ppdepth, pptime, 0.d0,
     +                     3, rdata(2), obslev1, atlayer1, npstok)
c
          totprimp = totprimp + rdata(5)
          eprimp   = eprimp + rdata(1) * rdata(5)
c
c         Writing the particle to the compressed files.
c         (Special primary record type is 3).
c
          if (npstok .eq. 1) then
c
            nprimps   = nprimps + 1
c
            if (recordspp) then
              cidata(1) = ppcode
              codata(1) = log(rdata(1))
              do i = 2, 4
                codata(i) = rdata(i)
              enddo
              codata(10) = rdata(5)
              call allciosave(0, 0, 3, 1, cidata, codata, ciws)
            endif
c
          endif
        endif
      endif
      goto 1010
c
 1020 continue
c
c     Closing and removing the interchange files.
c
      do ut = 55, 56
        close(ut, status = 'DELETE')
      enddo
c
      if (logsw1) then
        write(auxline, 2030) 'Number of primary particles:',
     +          nprimps
          call putlog(2, .false., auxline(1:36))
        endif
 2030   format(a, i8)
c
c     Checking the data read.
c
      if (nprimps .le. 0) then
        call errprint(2, '$P30', 4, 'spafter',
     +                'No primary particles to process.',
     +                0, 0, 0, 0.d0, ' ')
      else if ((nprimp + nprimpnull) .ne. nprimp0) then
        call errprint(2, '$P30', 4, 'spafter',
     +    'Inconsistency in the number of primary particles.',
     +    1, nprimp, 0, 0.d0, ' ')
      endif
c
c     Accounting for the missing primary energy.
c
      if (eprimp .lt. (1.004d0 * primener)) then
        call notapcle(primener - eprimp, 1.d0)
      else
        call errprint(2, '$P30', 4, 'spafter',
     +       'Total energy of primaries exceeds the specified one.',
     +       0, 0, 0, 0.d0, ' ')
      endif
c
c     Writing the special particle trailer record to the compressed
c     files (Special primary trailing record type is 4).
c
      cidata(1) = usrmacrover
      codata(1) = totprimp
      codata(2) = nprimp
      codata(3) = eprimp
c
      if (recordspvar0 .and. (nintfltvar .gt. 0)) then
        call allciosavewaf(0, 0, 4, 1, cidata, codata,
     +                    nintfltvar, spfltvar, ciws)
      else
        call allciosave(0, 0, 4, 1, cidata, codata, ciws)
      endif
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(2, '$P30', 4, 'spafter',
     +     'Open or read operation failed.',
     +     0, 0, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine spafter
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'stackprimary.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
