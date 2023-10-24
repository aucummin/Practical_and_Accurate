c
c     FILE: speilib.f                       Creation date: 19/OCT/1999.
c                                       LAST MODIFICATION: 20/JUL/2006.
c
c     Library of routines related to the "Special primary"
c     external interface.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speistart(showerno, primener, injpos, xvinj,
     +                     zground, xvground, dgroundinj, uprim)
c
c     Starting the interface for the "special" primary particle
c     external process.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2001, 2003;
c                                Fermilab 2003;
c                                La Plata 2005.
c
c
c     Arguments:
c     =========
c
c     showerno........ (output, integer) Current shower number.
c     primener........ (output, double precision) Primary energy (GeV).
c     injpos.......... (output, double precision, array(3)) Position
c                      of injection point with respect to the AIRES
c                      coordinate system (in meters).
c     xvinj........... (output, double precision) Vertical atmospheric
c                      depth of the injection point (in g/cm2).
c     zground......... (output, double precision) Altitude of ground
c                      level (in m.a.s.l).
c     xvground........ (output, double precision) Vertical atmospheric
c                      depth of the ground surface (in g/cm2).
c     dgroundinj...... (output, double precision) Distance from the
c                      injection point to the intersection between
c                      the shower axis and the ground surface (in m).
c     uprim........... (output, double precision, array(3)) Unitary
c                      vector in the direction of the straight line
c                      going from the injection point towards the
c                      intersection between the shower axis and the
c                      ground plane.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
      include 'initpar.f'
      include 'kernelpar.f'
      include 'spipar.f'
c
c     Declaration of arguments.
c
      integer           showerno
      double precision  primener
      double precision  injpos(3)
      double precision  xvinj, zground, xvground, dgroundinj
      double precision  uprim(3)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'atmosdata.f'
      include 'randomdata.f'
      include 'spicomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*24      version, header0
      integer           iheader0
      integer           i, j, i1
      double precision  sinprimt0, cosprimp0, sinprimp0
c
c     FIRST EXECUTABLE STATEMENT
c
c     Initializing the AIRES error message system. Notice that there
c     are no pre-definitions of messages, so the only available message
c     is the dummy message '*'.
c
      call clearerrmess
c
c     Opening the interface file.
c
      open(55, file = spiifile, status = 'OLD',
     +         form = 'UNFORMATTED', err = 3010)
c
c     Reading and checking the header.
c
      read(55, end = 3010, err = 3010) header0, iheader0,
     +                                 version, airescalliversion,
     +                                 spmagic
c
      if (header0 .ne. spiifileheader0) then
         call errprint(0, '*', 4, 'speistart',
     +     'Error processing external module interchange file.$' //
     +     'Invalid file header.',
     +     0, 0, 0, 0.d0, ' ')
      endif
c
c     Reading task and shower information.
c
      read(55, end = 3010, err = 3010) tasknamever, tasknamelen
      if (tasknamelen .gt. 0) then
        read(55, end = 3010, err = 3010) taskname(1:tasknamelen)
      endif
      read(55, end = 3010, err = 3010) cruser, crhost
c
      read(55, end = 3010, err = 3010) currshower, mtotshowers,
     +                                 firstshowernon1
c
      showerno = currshower + firstshowernon1
c
c     Physical shower variables.
c
      read(55, end = 3010, err = 3010)
     +         primcode0, primener0, injdepth, showert00,
     +         groundz, groundepth
      read(55, end = 3010, err = 3010)
     +         (injpos0(i), i = 1, 4)
      read(55, end = 3010, err = 3010)
     +         (uprim0(i), i = 1, 3)
c
      primener   = primener0
      xvinj      = injdepth
      zground    = groundz
      xvground   = groundepth
      dgroundinj = sqrt(injpos0(1) ** 2 + injpos0(2) ** 2 +
     +                  (injpos0(3) - groundz) ** 2)
c
      do i = 1, 3
        injpos(i) = injpos0(i)
        uprim(i)  = uprim0(i)
      enddo
c
c     Variables related with the "special primary particle".
c
      read(55, end = 3010, err = 3010) escpclename0
      read(55, end = 3010, err = 3010) escmacrover0
c
      read(55, end = 3010, err = 3010) macroi1, macroi2, macroi3
      read(55, end = 3010, err = 3010) escpclemacro0(1:macroi3)
c
c     Reading the special internal data arrays.
c
      read(55, end = 3010, err = 3010)
     +          nintintvar, nintfltvar, recordspvar0
      read(55, end = 3010, err = 3010) mxintintvar0, mxintfltvar0
c
      if (mxintintvar0 .gt. mxintintvar) mxintintvar0 = mxintintvar
      if (mxintfltvar0 .gt. mxintfltvar) mxintfltvar0 = mxintfltvar
c
      if (nintintvar .gt. 0) then
        if (nintintvar .gt. mxintintvar) goto 3010
        read(55, end = 3010, err = 3010)
     +            (spintvar(i), i = 1, nintintvar)
      endif
      if (nintfltvar .gt. 0) then
        if (nintfltvar .gt. mxintfltvar) goto 3010
        read(55, end = 3010, err = 3010)
     +            (spfltvar(i), i = 1, nintfltvar)
      endif
c
c     Reading some additional spare fields for future use.
c
      read(55, end = 3010, err = 3010) (i1, i = 0, 9)
c
c     Reading global variables.
c
      read(55, end = 3010, err = 3010)
     +          thereareglobars, nglobar(1), nglobar(2)
c
      do j = 1, 2
        if (nglobar(j) .gt. 0) then
          read(55, end = 3010, err = 3010)
     +              (globnam(i, j), i = 1, nglobar(j)),
     +              (globlen(i, j), i = 1, nglobar(j))
          read(55, end = 3010, err = 3010)
     +              globstrlen(j),
     +              (globdfend(i, j), i = 0, nglobar(j))
          if (globstrlen(j) .gt. 0) then
            read(55, end = 3010, err = 3010)
     +                globarstring(j)(1:globstrlen(j))
          endif
        endif
      enddo
c
c     Getting data corresponding to the atmospheric model.
c
      read(55, end = 3010, err = 3010) nlayers, nlayers1, nlayersp1
      read(55, end = 3010, err = 3010) (alaylimx(i), i = 0, nlayers),
     +                                 (alaylimz(i), i = 0, nlayers)
      read(55, end = 3010, err = 3010) (alaya1(i), i = 0, nlayers),
     +                                 (alaya2(i), i = 0, nlayers)
      read(55, end = 3010, err = 3010) (alayb1(i), i = 0, nlayers),
     +                                 (alayb2(i), i = 0, nlayers)
      read(55, end = 3010, err = 3010) (alayd1(i), i = 0, nlayers),
     +                                 (alayd2(i), i = 0, nlayers)
c
c     Getting the current status of the random number generator.
c
      read(55, end = 3010, err = 3010) i
      read(55, end = 3010, err = 3010) xran, xran2, original_seed
c
c     Reading some additional spare fields for future use.
c
      read(55, end = 3010, err = 3010) (i1, i = 0, 9)
c
c     Transferring additional data needed in some particular cases.
c
c     Shower primary general data.
c
      read(55, end = 3010, err = 3010)
     +          nshprimary, pryenergymin, pryeminset
      if (nshprimary .gt. 0) then
        read(55, end = 3010, err = 3010)
     +            (shprimary(i), i = 1, nshprimary),
     +            (shprimarywt(i), i = 1, nshprimary),
     +            (shprimarywt0(i), i = 1, nshprimary)
      endif
c
c     Main input data arrays.
c
      read(55, end = 3010, err = 3010)
     +          nfidata, niidata, nlidata,
     +          nsidata, sidatalen, nlibsites,
     +          (i, j = 1, 5)
      read(55, end = 3010, err = 3010)
     +          nfidata0, niidata0, nlidata0,
     +          nsidata0, sidatalen0,
     +          (i, j = 1, 6)
c
      if ((nfidata .gt. mxfidata) .or.
     +    (niidata .gt. mxiidata) .or.
     +    (nlidata .gt. mxlidata) .or.
     +    (nsidata .gt. mxsidata) .or.
     +    (sidatalen .gt. mxsil)       ) goto 3010
c
      if (nfidata .gt. 0) then
        read(55, end = 3010, err = 3010)
     +            ((fidata(j, i), j = 0, 3), i = 1, nfidata),
     +            (fidatawaset(i), i = 1, nfidata),
     +            (fdbdry(i), i = 1, nfidata)
      endif
c
      if (niidata .gt. 0) then
        read(55, end = 3010, err = 3010)
     +            ((iidata(j, i), j = 0, 3), i = 1, niidata),
     +            (iidatawaset(i), i = 1, niidata),
     +            (idbdry(i), i = 1, niidata)
      endif
c
      if (nlidata .gt. 0) then
        read(55, end = 3010, err = 3010)
     +            ((lidata(j, i), j = 0, 2), i = 1, nlidata),
     +            (lidatawaset(i), i = 1, nlidata)
      endif
c
      if (nsidata .gt. 0) then
        read(55, end = 3010, err = 3010)
     +            ((sidata(j, i), j = 0, 6), i = 1, nsidata),
     +            (sidatawaset(i), i = 1, nsidata)
        read(55, end = 3010, err = 3010)
     +            sidatastring(1:sidatalen)
      endif
c
c     Reading the site library.
c
      if (nlibsites .gt. 0) then
        read(55, end = 3010, err = 3010)
     +           (sitename(i), i = 0, nlibsites)
        read(55, end = 3010, err = 3010)
     +           (sitenlen(i), i = 0, nlibsites)
        read(55, end = 3010, err = 3010)
     +           (sitelat(i), i = 0, nlibsites)
        read(55, end = 3010, err = 3010)
     +           (sitelong(i), i = 0, nlibsites)
        read(55, end = 3010, err = 3010)
     +           (siteground(i), i = 0, nlibsites)
      endif
c
c     Reading some additional spare fields for future use.
c
      read(55, end = 3010, err = 3010) (i1, i = 0, 9)
c
      close(55)
c
c     Evaluating the matrix to change coordinates from the injection-
c     shower axis system to the AIRES system.
c     This system (x', y', z') has the origin placed at the original
c     injection point, the z' axis in the direction of vector uprim0
c     (parallel to the shower axis), and the x' axis is always
c     horizontal.
c
      if (abs(uprim0(3)) .lt. 0.999999999999d0) then
        sinprimt0 =   sqrt(1 - uprim0(3) ** 2)
        cosprimp0 =   uprim0(1) / sinprimt0
        sinprimp0 =   uprim0(2) / sinprimt0
        syschia11 =   sinprimp0
        syschia21 = - cosprimp0
        syschia12 =   cosprimp0 * uprim0(3)
        syschia22 =   sinprimp0 * uprim0(3)
        syschia32 = - sinprimt0
      else
        syschia11 =   1
        syschia21 =   0
        syschia12 =   0
        syschia22 =   uprim0(3)
        syschia32 =   0
      endif
c
      syschia13 = uprim0(1)
      syschia23 = uprim0(2)
      syschia33 = uprim0(3)
c
c     The injection point is set to the original injection point.
c
      xinj      = injpos0(1)
      yinj      = injpos0(2)
      zinj      = injpos0(3)
      tinj      = -showert00
c
c     Opening the particle file.
c
      open(55, file = spiofile, status = 'UNKNOWN',
     +         form = 'UNFORMATTED', err = 3020)
c
c     Writing the header.
c
      version = aires_version
c
      write(55, err = 3020) spiofileheader0, spiintheader0,
     +                      version, aires_version_no, spmagic
c
      write(55, err = 3020) currshower
      write(55, err = 3020) injpointcode, xinj, yinj, zinj, tinj, tinj
c
c     Unit 55 remains open to receive particle data.
c
c     Some additional initializations.
c
      nprimp0   = 0
      eprimp0   = 0
      eprimpmax = 1.004d0 * primener0
      set1st    = 0
c
      speidone  = speidonenumber
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '*', 4, 'speistart',
     +     'Error processing external module interchange file.$' //
     +     'Open or read operation failed.',
     +     0, 0, 0, 0.d0, ' ')
      return
c
 3020 continue
      call errprint(0, '*', 4, 'speistart',
     +     'Error opening and/or writing interchange file.',
     +     0, 0, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine speistart
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speiend(retcode)
c
c     Closing the interface for the "special" primary particle
c     external process.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     retcode......... (input, integer) Return code to pass to the
c                      main simulation program. retcode = 0 means
c                      normal return. If retcode is not zero, a
c                      a message will be printed and saved in the log
c                      file (.lgf): 0 < |retcode| < 10,
c                      10 <= |retcode| < 20, 20 <= |retcode| < 30, and
c                      |retcode| >= 30 correspond, respectively, to
c                      information, warning, error and fatal messages.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
      include 'initpar.f'
      include 'kernelpar.f'
      include 'spipar.f'
c
c     Declaration of arguments.
c
      integer           retcode
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'randomdata.f'
      include 'spicomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*24      version
      integer           i, j
      double precision  ftmp1
c
c     FIRST EXECUTABLE STATEMENT
c
c     Closing the particle file.
c
      i     = -999999
      ftmp1 = 0
      write(55, err = 3010) i, (ftmp1, j = 1, 5)
      close(55)
c
c     Rewriting the interface file.
c
      open(55, file = spiifile, status = 'UNKNOWN',
     +         form = 'UNFORMATTED', err = 3010)
c
c     Writing the header.
c
      version = aires_version
c
      write(55, err = 3010) spiofileheader0, spiintheader0,
     +                      version, aires_version_no, spmagic
c
      write(55, err = 3010) currshower
c
c     Writing module version and return code, information.
c
      write(55, err = 3010) escmacrover0, retcode
c
c     Re-writing the special internal data arrays.
c
      write(55, err = 3010) nintintvar, nintfltvar, recordspvar0
      write(55, err = 3010) mxintintvar0, mxintfltvar0
c
      if (nintintvar .gt. 0) then
        write(55, err = 3010) (spintvar(i), i = 1, nintintvar)
      endif
      if (nintfltvar .gt. 0) then
        write(55, err = 3010) (spfltvar(i), i = 1, nintfltvar)
      endif
c
c     Re-transferring dynamic global variables.
c
      write(55, err = 3010) nglobar(1)
c
      if (nglobar(1) .gt. 0) then
        write(55, err = 3010) (globnam(i, 1), i = 1, nglobar(1)),
     +                        (globlen(i, 1), i = 1, nglobar(1))
        write(55, err = 3010) globstrlen(1),
     +                        (globdfend(i, 1), i = 0, nglobar(1))
        if (globstrlen(1) .gt. 0) then
          write(55, err = 3010) globarstring(1)(1:globstrlen(1))
        endif
      endif
c
c     Re-writing the current status of the random number generator.
c
      i = 1
      write(55, err = 3010) i
      write(55, err = 3010) xran, xran2, original_seed
c
c     Writing general data about the primary particles.
c
      write(55, err = 3010) nprimp0, eprimp0
c
c     Writing 1st interaction information.
c
      write(55, err = 3010) set1st, x1st, y1st, z1st
c
c     Closing the interchange file.
c
      close(55)
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '*', 4, 'speiend',
     +     'Error opening and/or writing interchange file.',
     +     0, 0, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine speiend
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine spinjpoint(csys, x0, y0, z0, tsw, t0beta, irc)
c
c     Setting the current injection point for primary particles.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     csys............ (input, integer) Parameter labelling the
c                      coordinate system used. 0 = AIRES coordinate
c                      system. 1 = Injection point-shower axis system.
c     x0, y0, z0...... (input, double precision) Coordinates of the
c                      injection point with respect to the chosen
c                      system (in meters).
c     tsw............. (input, integer) Injection time switch. If
c                      tsw is zero then t0beta is an absolute injection
c                      time; if tsw is 1, then the injection time is
c                      set as the time employed by a particle whose
c                      speed is t0beta * c to go from the original
c                      injection point to the intersection point of
c                      the shower axis with the plane orthogonal to
c                      that axis and containing the point (x0, y0, z0).
c     t0beta.......... (input, double precision) The meaning of this
c                      argument depends on the current value of "tsw".
c                      It can be the absolute injection time (ns)
c                      (time at original injection is taken as zero);
c                      or the relative speed of a particle with
c                      respect to c.
c     irc............. (output, integer) Return code. 0 means normal
c                      return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'spipar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           csys, tsw, irc
      double precision  x0, y0, z0, t0beta
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  dpp
c
      integer           csysipsave, tswipsave
      save              csysipsave, tswipsave
      double precision  x0ipsave, y0ipsave, z0ipsave, t0betaipsave
      save              x0ipsave, y0ipsave, z0ipsave, t0betaipsave
      data              csysipsave, tswipsave  / 999999, 999999 /
      data              x0ipsave, y0ipsave, z0ipsave, t0betaipsave
     +                  / -33.d0,   -33.d0,   -33.d0,   -33.d0 /
c
c     FIRST EXECUTABLE STATEMENT
c
      irc = 0
c
c     Checking if it is actually necessary to change the injection
c     point.
c
      if ( (csys   .eq. csysipsave  ) .and.
     +     (x0     .eq. x0ipsave    ) .and.
     +     (y0     .eq. y0ipsave    ) .and.
     +     (z0     .eq. z0ipsave    ) .and.
     +     (tsw    .eq. tswipsave   ) .and.
     +     (t0beta .eq. t0betaipsave)       ) return
c
      if (csys .eq. 0) then
c
c       Specification using the AIRES coordinate system.
c
        xinj = x0
        yinj = y0
        zinj = z0
c
      else if (csys .eq. 1) then
c
c       Specification using the injection-shower axis system.
c
        call isas2as(x0, y0, z0, xinj, yinj, zinj)
c
      else
        irc = 16
        return
      endif
c
c     Evaluating the injection time.
c
      if (tsw .eq. 0) then
c
c       Absolute specification.
c
        tinj = cspeedns * t0beta - showert00
c
      else if (tsw .eq. 1) then
c
c       Injection time to be calculated using x0, y0, and z0.
c
        if ((t0beta .gt. 0) .and. (t0beta .le. 1)) then
          dpp  = abs(uprim0(1) * (xinj - injpos0(1)) +
     +               uprim0(2) * (yinj - injpos0(2)) +
     +               uprim0(3) * (zinj - injpos0(3)))
          tinj = dpp / t0beta - showert00
        else
          irc = 12
          return
        endif
c
      else
        irc = 14
        return
      endif
c
c     Writing the "change of injection point" record.
c
      write(55, err = 3010) injpointcode, xinj, yinj, zinj, tinj, tinj
c
c     Injection point changed. Remembering the arguments for future
c     calls.
c
      csysipsave   = csys
      x0ipsave     = x0
      y0ipsave     = y0
      z0ipsave     = z0
      tswipsave    = tsw
      t0betaipsave = t0beta
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '*', 4, 'spinjpoint',
     +     'Error writing interchange file.',
     +     0, 0, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine spinjpoint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine spaddp0(pcode, pener, csys, ux, uy, uz, pwt, irc)
c
c     Adding a single primary particle.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     pcode........... (input, integer) Particle code (AIRES coding
c                      system).
c     pener........... (input, double precision) Particle kinetic
c                      energy (GeV).
c     csys............ (input, integer) Parameter labelling the
c                      coordinate system used. 0 = AIRES coordinate
c                      system. 1 = Injection point-shower axis system.
c     ux, uy, uz...... (input, double precision) Direction of motion,
c                      with respect to the coordinate system selected
c                      by csys. The vector (ux, uy, uz) does not need
c                      to be normalized.
c     pwt............. (input, double precision) Particle weight.
c                      Must be equal or greater than one.
c     irc............. (output, integer) Return code. 0 means normal
c                      return.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           pcode, csys, irc
      double precision  pener, ux, uy, uz, pwt
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  uu0(3)
      double precision  un
c
c     FIRST EXECUTABLE STATEMENT
c
      if (pwt .lt. 1) then
        irc = 9
        return
      endif
c
      if (pener .le. 0) then
        irc = 8
        return
      endif
c
      un = ux ** 2 + uy ** 2 + uz ** 2
c
      if (un .le. 0) then
        irc = 5
        return
      endif
c
      un  = 1 / sqrt(un)
      uu0(1) = ux * un
      uu0(2) = uy * un
      uu0(3) = uz * un
c
      if (csys .eq. 1) then
c
c       Direction of motion is expressed in the shower axis system.
c       Transforming to AIRES system.
c
        call mrotate(uprim0, 1, 3, uu0)
c
      else if (csys .ne. 0) then
        irc = 16
        return
      endif
c
      write(55, err = 3010) pcode, pener, uu0(1), uu0(2), uu0(3), pwt
c
      nprimp0 = nprimp0 + 1
      eprimp0 = eprimp0 + pener * pwt
      irc     = 0
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '*', 4, 'spaddp0',
     +     'Error writing interchange file.',
     +     0, 0, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine spaddp0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine spaddpn(n, pcode, pener, csys, ldu, uxyz, pwt, irc)
c
c     Adding a set of n primary particles.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of primary particles
c                      to add.
c     pcode........... (input, integer, array(n)) Particle codes (AIRES
c                      coding system).
c     pener........... (input, double precision, array(n)) Particle
c                      kinetic energies (GeV).
c     csys............ (input, integer) Parameter labelling the
c                      coordinate system used. 0 = AIRES coordinate
c                      system. 1 = Injection point-shower axis system.
c     ldu............. (input, integer) Leading dimension of array
c                      uxyz.
c     uxyz............ (input-output, double precision, array(ldu, n))
c                      Direction of motion of the respective primaries,
c                      with respect to the coordinate system selected
c                      by csys. The vectors uxyz(j, i), j = 1, 2, 3, do
c                      not need to be normalized. This array will
c                      be modified internally (normalization, and
c                      eventually coordinate transformation).
c     pwt............. (input, double precision, array(n)) Particle
c                      weights.
c                      Must be equal or greater than one.
c     irc............. (output, integer) Return code. 0 means normal
c                      return.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           n, csys, ldu, irc
      integer           pcode(n)
      double precision  pener(n)
      double precision  uxyz(ldu, n)
      double precision  pwt(n)
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      double precision  un
c
c     FIRST EXECUTABLE STATEMENT
c
c     Normalizing the directions of motion, and checking particle
c     energies and weights.
c
      do i = 1, n
c
        if (pwt(i) .lt. 1) then
          irc = 9
          return
        endif
c
        if (pener(i) .le. 0) then
          irc = 8
          return
        endif
c
        un = uxyz(1, i) ** 2 + uxyz(2, i) ** 2 + uxyz(3, i) ** 2
c
        if (un .le. 0) then
          irc = 5
          return
        endif
c
        un         = 1 / sqrt(un)
        uxyz(1, i) = uxyz(1, i) * un
        uxyz(2, i) = uxyz(2, i) * un
        uxyz(3, i) = uxyz(3, i) * un
c
      enddo
c
      if (csys .eq. 1) then
c
c       Direction of motion is expressed in the shower axis system.
c       Transforming to AIRES system.
c
        call mrotate(uprim0, n, ldu, uxyz)
c
      else if (csys .ne. 0) then
          irc = 16
          return
      endif
c
      do i = 1, n
        write(55, err = 3010) pcode(i), pener(i),
     +                        (uxyz(j, i), j = 1, 3),
     +                        pwt(i)
        eprimp0 = eprimp0 + pener(i) * pwt(i)
      enddo
c
      nprimp0 = nprimp0 + n
      irc     = 0
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '*', 4, 'spaddpn',
     +     'Error writing interchange file.',
     +     0, 0, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine spaddpn
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine spaddnull(pener, pwt, irc)
c
c     Adding a "null" (unphysical) particle. This particle will not
c     be stacked, but its energy will be added to the unphysical
c     particle counter.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     pener........... (input, double precision) Particle kinetic
c                      energy (GeV).
c     pwt............. (input, double precision) Particle weight.
c                      Must be equal or greater than one.
c     irc............. (output, integer) Return code. 0 means normal
c                      return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      double precision  pener, pwt
      integer           irc
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (pwt .lt. 1) then
        irc = 9
        return
      endif
c
      if (pener .le. 0) then
        irc = 8
        return
      endif
c
      write(55, err = 3010) nullcode, pener,
     +                      uprim0(1), uprim0(2), uprim0(3),
     +                      pwt
c
      nprimp0 = nprimp0 + 1
      eprimp0 = eprimp0 + pener * pwt
      irc     = 0
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '*', 4, 'spaddnull',
     +     'Error writing interchange file.',
     +     0, 0, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine spaddnull
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine sp1stint(csys, x1, y1, z1, irc)
c
c     Setting the position of the first interaction.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     csys............ (input, integer) Parameter labelling the
c                      coordinate system, or depth specification used.
c                      0 = AIRES coordinate system. 1 = Injection
c                      point-shower axis system. 11 = The position
c                      of the first interaction is given as a
c                      vertical depth. 12 = The position of the first
c                      interaction is given as a slant depth.
c     x1, y1, z1...... (input, double precision) Coordinates of the
c                      interaction point with respect to the chosen
c                      system (in meters). If csys is 11 or 12, then
c                      x1 and y1 are not used, and z1 must contain
c                      the corresponding depth in g/cm2 (It is the
c                      user's response to ensure that the atmospheric
c                      model used to estimate the depth is the same
c                      than the one used in the simulation program).
c     irc............. (output, integer) Return code. 0 means normal
c                      return.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           csys, irc
      double precision  x1, y1, z1
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (csys .eq. 0) then
c
c       Specification using the AIRES coordinate system.
c
        x1st   = x1
        y1st   = y1
        z1st   = z1
        set1st = 1
c
      else if (csys .eq. 1) then
c
c       Specification using the injection-shower axis system.
c
        call isas2as(x1, y1, z1, x1st, y1st, z1st)
        set1st = 1
c
      else if (csys .eq. 11) then
c
c       Direct specification as a vertical depth.
c
        z1st   = z1
        set1st = 2
c
      else if (csys .eq. 12) then
c
c       Direct specification as a slant depth.
c
        z1st   = z1
        set1st = 3
c
      else
        irc = 16
        return
      endif
c
      irc = 0
c
      return
      end
c     --- End of routine sp1stint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine isas2as(xprime, yprime, zprime,
     +                   xaires, yaires, zaires)
c
c     Converting coordinates expressed in the injection-shower axis
c     system (isas) (x', y', z') to the AIRES coordinate system (as)
c     (x, y, z).
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     xprime, yprime,
c     zprime.......... (input, double precision) Coordinates in the
c                      "isas" system.
c     xaires, yaires,
c     zaires.......... (output, double precision) Coordinates in the
c                      "as" system.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  xprime, yprime, zprime
      double precision  xaires, yaires, zaires
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      xaires = injpos0(1) + syschia11 * xprime
     +                    + syschia12 * yprime
     +                    + syschia13 * zprime
      yaires = injpos0(2) + syschia21 * xprime
     +                    + syschia22 * yprime
     +                    + syschia23 * zprime
      zaires = injpos0(3) + syschia32 * yprime
     +                    + syschia33 * zprime
c
      return
      end
c     --- End of routine isas2as
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speigetintvar(varn, vval, irc)
c
c     Getting an integer internal variable interchanged with the
c     AIRES kernel.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     varn............ (input, integer) Variable index. Must lie in the
c                      allowed range.
c     vval............ (output, integer) Current value of the
c                      variable.
c     irc............. (output, integer) Return code. Zero corresponds
c                      to normal return. If irc is non zero, then vval
c                      is set to zero.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           varn, irc
      integer           vval
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((varn .ge. 1) .and. (varn .le. nintintvar)) then
        vval = spintvar(varn)
        irc  = 0
      else
        vval = 0
        irc  = 3
      endif
c
      return
      end
c     --- End of routine speigetintvar
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speisetintvar(varn, vval, irc)
c
c     Setting an integer internal variable interchanged with the
c     AIRES kernel.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     varn............ (input, integer) Variable index. Must lie in the
c                      allowed range.
c     vval............ (input, integer) Value to be assigned to the
c                      variable.
c     irc............. (output, integer) Return code. Zero corresponds
c                      to normal return. If irc is non zero, no
c                      variable is set.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           varn, irc
      integer           vval
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((varn .ge. 1) .and. (varn .le. mxintintvar0)) then
        spintvar(varn) = vval
        if (varn .gt. nintintvar) nintintvar = varn
        irc  = 0
      else
        irc  = 8
      endif
c
      return
      end
c     --- End of routine speisetintvar
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speigetrealvar(varn, vval, irc)
c
c     Getting a real internal variable interchanged with the
c     AIRES kernel.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     varn............ (input, integer) Variable index. Must lie in the
c                      allowed range.
c     vval............ (output, double precision) Current value of the
c                      variable.
c     irc............. (output, integer) Return code. Zero corresponds
c                      to normal return. If irc is non zero, then vval
c                      is set to zero.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           varn, irc
      double precision  vval
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((varn .ge. 1) .and. (varn .le. nintfltvar)) then
        vval = spfltvar(varn)
        irc  = 0
      else
        vval = 0
        irc  = 3
      endif
c
      return
      end
c     --- End of routine speigetrealvar
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speisetrealvar(varn, vval, irc)
c
c     Setting a real internal variable interchanged with the
c     AIRES kernel.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     varn............ (input, integer) Variable index. Must lie in the
c                      allowed range.
c     vval............ (input, double precision) Value to be assigned
c                      to the variable.
c     irc............. (output, integer) Return code. Zero corresponds
c                      to normal return. If irc is non zero, no
c                      variable is set.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           varn, irc
      double precision  vval
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((varn .ge. 1) .and. (varn .le. mxintfltvar0)) then
        spfltvar(varn) = vval
        if (varn .gt. nintfltvar) nintfltvar = varn
        irc  = 0
      else
        irc  = 8
      endif
c
      return
      end
c     --- End of routine speisetrealvar
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine sprecordvars(rsw)
c
c     Setting the switch to save or not the kernel interchange
c     variables.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     rsw............. (input, integer) Integer switch. If rsw is
c                      positive, then all defined variables are
c                      recorded in the compressed output files.
c                      If rsw is zero or negative, the variables are
c                      not recorded.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           rsw
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      recordspvar0 = (rsw .gt. 0)
c
      return
      end
c     --- End of routine sprecordvars
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function sprecordvarenabled()
c
c     Returning the current value of the switch to save or not the
c     kernel interchange variables.
c
c     Written by: S. J. Sciutto, La Plata 2003, 2006.
c
c
c     Return value: (logical) True if the varible recording mechanism
c     ============  is enabled. False otherwise.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      logical           sprecordvarenabled
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      sprecordvarenabled = recordspvar0
c
      return
      end
c     --- End of routine sprecordvarenabled
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine sprimname(pname, pnamelen)
c
c     Getting the name of the special primary particle specified in the
c     corresponding IDL instruction.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     pname........... (output, character*(*)) Particle name. The
c                      calling program must provide enough space to
c                      store this string.
c     pnamelen........ (output, integer) Lenght of the particle name
c                      string.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'spipar.f'
c
c     Declaration of arguments.
c
      character*(*)     pname
      integer           pnamelen
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (speidone .ne. speidonenumber) then
        call errprint(0, '*', 3, 'sprimname',
     +  'Cannot retrieve data. Interface not set up properly.',
     +  0, 9, 0, 0.d0, ' ')
        pnamelen = 0
      else
        call strimcopy(escpclename0, 16, pname, pnamelen)
      endif
c
      return
      end
c     --- End of routine sprimname
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine spnshowers(totsh, firstsh, lastsh)
c
c     Getting the current values of first and last shower, and total
c     number of showers.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     totsh........... (output, integer) Total number of showers for
c                      the current task.
c     firstsh......... (output, integer) Number of first shower.
c     lastsh.......... (output, integer) Number of last shower.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'spipar.f'
c
c     Declaration of arguments.
c
      integer           totsh, firstsh, lastsh
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (speidone .ne. speidonenumber) then
        call errprint(0, '*', 3, 'spnshowers',
     +  'Cannot retrieve data. Interface not set up properly.',
     +  0, 9, 0, 0.d0, ' ')
        totsh = 0
      else
        totsh   = mtotshowers
        firstsh = firstshowernon1 + 1
        lastsh  = firstshowernon1 + mtotshowers
      endif
c
      return
      end
c     --- End of routine spnshowers
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speitask(taskn, tasklen, tver)
c
c     Getting the task name and version corresponding to the current
c     shower.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     taskn........... (output, character*(*)) Task name. The
c                      calling program must provide enough space to
c                      store this string.
c     tasklen......... (output, integer) Lenght of the task name.
c     tver............ (output, integer) Task name version.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'spipar.f'
c
c     Declaration of arguments.
c
      character*(*)     taskn
      integer           tasklen, tver
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (speidone .ne. speidonenumber) then
        call errprint(0, '*', 3, 'speitask',
     +  'Cannot retrieve data. Interface not set up properly.',
     +  0, 9, 0, 0.d0, ' ')
        tasklen = 0
        tver    = 0
      else
        taskn   = taskname
        tasklen = tasknamelen
        tver    = tasknamever
      endif
c
      return
      end
c     --- End of routine speitask
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speigetmodname(mn, mnlen, mnfull, mnfullen)
c
c     Getting the name of the module invoked by the simulation
c     program.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     mn.............. (output, character*(*)) Name of external module.
c                      The calling program must provide enough space to
c                      store this string.
c     mnlen........... (output, integer) Lenght of string "mn".
c     mnfull.......... (output, character*(*)) Full name of external
c                      module (Will be different of "mn" if the module
c                      was placed within one of the InputPath
c                      directories). The calling program must provide
c                      enough space to store this string.
c     mnfullen........ (output, integer) Lenght of string "mnfull".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'spipar.f'
c
c     Declaration of arguments.
c
      character*(*)     mn, mnfull
      integer           mnlen, mnfullen
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (speidone .ne. speidonenumber) then
        call errprint(0, '*', 3, 'speigetmodname',
     +    'Cannot retrieve data. Interface not set up properly.',
     +    0, 9, 0, 0.d0, ' ')
        mnlen    = 0
        mnfullen = 0
      else
        mn       = escpclemacro0(1:macroi1)
        mnlen    = macroi1
        mnfull   = escpclemacro0(macroi1+1:macroi2)
        mnfullen = macroi2 - macroi1
      endif
c
      return
      end
c     --- End of routine speigetmodname
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speigetpars(parstring, pstrlen)
c
c     Getting the parameter string specified in the corresponding
c     IDL instruction.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     parstring....... (output, character*(*)) Parameter string. The
c                      calling program must provide enough space to
c                      store this string.
c     pstrlen......... (output, integer) Lenght of the parameter
c                      string. Zero if there are no parameters.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'spipar.f'
c
c     Declaration of arguments.
c
      character*(*)     parstring
      integer           pstrlen
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      pstrlen = 0
      if (speidone .ne. speidonenumber) then
        call errprint(0, '*', 3, 'speigetpars',
     +    'Cannot retrieve data. Interface not set up properly.',
     +    0, 9, 0, 0.d0, ' ')
      else
        if (macroi3 .gt. macroi2) then
          parstring = escpclemacro0(macroi2+1:macroi3)
          pstrlen   = macroi3 - macroi2
        else
          parstring = ' '
        endif
      endif
c
      return
      end
c     --- End of routine speigetpars
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function spsimprogversion()
c
c     Returning the version of the calling main simulation program.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
c
c     Return value: (integer) The corresponding version in integer
c     ============  format (for example 01040200 for version 1.4.2,
c                   01040201 for version 1.4.2a, etc.).
c                   If the routine is invoked out of a special
c                   particle environment it returns 0.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'spipar.f'
c
c     Declaration of arguments.
c
      integer           spsimprogversion
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (speidone .eq. speidonenumber) then
        spsimprogversion = airescalliversion
      else
        spsimprogversion = 0
        call errprint(0, '*', 3, 'spsimprogversion',
     +    'Cannot retrieve data. Interface not set up properly.',
     +    0, 9, 0, 0.d0, ' ')
      endif
c
      return
      end
c     --- End of routine spsimprogversion
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine speimv(mvnew, mvprev)
c
c     Setting/getting the external macro version.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     mvnew........... (input, integer) Macro version number. Must be
c                      an integer in the range [1, 759375]. If mvnew
c                      is zero, the macro version is not set.
c     mvprev.......... (output, integer) Macro version number
c                      effective at the moment of invoking the
c                      routine.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'spipar.f'
c
c     Declaration of arguments.
c
      integer           mvnew, mvprev
c
c     Declaration of shared data.
c
      include 'spicomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (speidone .ne. speidonenumber) goto 3010
c
      mvprev = escmacrover0
c
      if ((mvnew .gt. 0) .and. (mvnew .le. 759375)) then
        escmacrover0 = mvnew
      else if (mvnew .ne. 0) then
        call errprint(0, '*', 3, 'speisetmv',
     +  'Invalid external module version number. Instruction ignored.',
     +  1, mvnew, 0, 0.d0, ' ')
      endif
c
      return
c
c     Error message.
c
 3010 continue
      call errprint(0, '*', 3, 'speimv',
     +  'Cannot retrieve data. Interface not set up properly.',
     +  0, 9, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine speimv
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'speilib.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
