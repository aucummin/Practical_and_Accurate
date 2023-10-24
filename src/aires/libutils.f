c
c     FILE: libutils.f                      Creation date: 17/JUL/1997.
c                                       LAST MODIFICATION: 05/OCT/2006.
c
c     This file contains several routines included in the AIRES library
c     that are useful for analysing simulation data.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine olcoord(nobslev, olzv, groundz, injz, zenith, azimuth,
     +                   xaxis, yaxis, zaxis, tshift, mx, my, irc)
c
c     Calculating (xo, yo, zo), coordinates of the intersections of
c     observing level surfaces with the shower axis, the corresponding
c     time shifts and the coefficients (mx, my) of the plane tangent to
c     the surface at the intersection point:
c
c                 z - zo = mx (x - xo) + my (y - yo)
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     nobslev......... (input, integer) The number of observing
c                      levels.
c     olzv............ (input, double precision, array(nobslev))
c                      Altitudes (m) of the corresponding observing
c                      levels.
c     groundz......... (input, double precision) Ground altitude (m).
c     injz............ (input, double precision) Injection altitude
c                      (m).
c     zenith.......... (input, double precision) Zenith angle (deg).
c     azimuth......... (input, double precision) Azimuth angle (deg).
c     xaxis, yaxis,
c     zaxis........... (output, double precision, array(nobslev))
c                      (xo, yo, zo) coordinates (m) of the intersection
c                      points between the observing level surfaces and
c                      the shower axis.
c     tshift.......... (output, double precision, array(nobslev))
c                      Observing levels time shifts (ns), that is,
c                      the amount of time a light speed particle
c                      needs to go from the injection point to
c                      the intersection point (xo, yo, zo).
c     mx, my.......... (output, double precision, array(nobslev))
c                      Coefficients of the planes tangent to the
c                      observing levels, passing by the corresponding
c                      intersection points.
c     irc............. (output, integer) Return code. Zero means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           nobslev, irc
      double precision  olzv(nobslev)
      double precision  groundz, injz, zenith, azimuth
      double precision  xaxis(nobslev)
      double precision  yaxis(nobslev)
      double precision  zaxis(nobslev)
      double precision  tshift(nobslev)
      double precision  mx(nobslev), my(nobslev)
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  rad, cozenith, sizenith, coazim, siazim, sizen2
      double precision  groundz00, iovgz00sq
      double precision  inja, cinjz, drz, rxy, tfact, ola, olzo
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking input parameters.
c
      irc = 5
      if (groundz .lt. -200.d0) return
      if (injz .le. groundz) return
c
      rad       = pi180 * zenith
      cozenith  = cos(rad)
      sizenith  = sin(rad)
      sizen2    = sizenith ** 2
c
      rad       = pi180 * azimuth
      coazim    = cos(rad)
      siazim    = sin(rad)
c
      groundz00 = groundz + rearth
      iovgz00sq = ((injz + rearth) / groundz00) ** 2
c
c     Evaluating the central injection altitude. To this end we take
c     into account the Earth's curvature.
c
      inja  = groundz00 * (sqrt(iovgz00sq - sizen2) - cozenith)
      drz   = inja * cozenith
      rxy   = inja * sizenith
      cinjz = drz + groundz
c
c     Evaluating time shift factor.
c
      tfact = ucspeedns / cozenith
c
      do i = 1, nobslev
c
c       Evaluating intersection points.
c
        ola      = groundz00 *
     +             (sqrt(((rearth + olzv(i)) / groundz00) ** 2 -
     +                   sizen2) - cozenith)
        olzo     = ola * cozenith + groundz
        ola      = ola * sizenith
        xaxis(i) = ola * coazim
        yaxis(i) = ola * siazim
        zaxis(i) = olzo
c
c       Evaluating time shifts.
c
        tshift(i) = (cinjz - olzo) * tfact
c
c       Evaluating tangent slopes.
c
        olzo  = -(rearth + olzo)
        mx(i) = xaxis(i) / olzo
        my(i) = yaxis(i) / olzo
c
      enddo
c
      irc = 0
      return
      end
c     --- End of routine olcoord
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine olcrossed(olkey, updown, firstol, lastol)
c
c     Given the crossed observing levels key (saved at each particle
c     record in the longitudinal tracking file), the first and last
c     observing levels and the direction of motion are reconstructed.
c     The encoding formula is the following:
c
c         olkey = firstol + 512 * lastol + 512^2 * updown01
c
c     where updown01 is one if the particle goes upwards, zero
c     otherwise. The corresponding variable updown is decoded in a
c     slightly different way: It will be set to 1 if the particle
c     goes upwards, -1 otherwise.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     olkey........... (input, integer) Key with information about the
c                      crossed observing levels.
c     updown.......... (output, integer) Up-down indicator: 1 if the
c                      particle was going upwards, -1 otherwise.
c     firstol......... (output, integer) First observing level crossed
c                      (0 LE firstol LE 511).
c     lastol.......... (output, integer) Last observing level crossed
c                      (0 LE lastol LE 511).
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           olkey, updown, firstol, lastol
c
c     Declaration of internal variables and arrays.
c
      integer           c1, c2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the direction of motion.
c
      if (olkey .lt. 262144) then
        updown = -1
      else
        updown =  1
      endif
c
c     Setting the first and last crossed levels.
c
      c1      = olkey / 512
      firstol = olkey - c1 * 512
      c2      = c1 / 512
      lastol  = c1 - c2 * 512
c
      return
      end
c     --- End of routine olcrossed
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine olcrossedu(olkey, ux, uy, uz, firstol, lastol)
c
c     Given the crossed observing levels key (saved at each particle
c     record in the longitudinal tracking file), and the x and y
c     components of the unitary vector marking the direction of motion,
c     the first and last observing levels and z component of the
c     direction of motion vector are reconstructed.
c     The encoding formula is the following:
c
c         olkey = firstol + 512 * lastol + 512^2 * updown01
c
c     where updown01 is one if the particle goes upwards, zero
c     otherwise.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     olkey........... (input, integer) Key with information about the
c                      crossed observing levels.
c     ux, uy.......... (input, double precision) X and y components of
c                      the unitary vector marking the particle's
c                      direction of motion.
c     uz.............. (output, double precision) Z component of the
c                      direction of motion.
c     firstol......... (output, integer) First observing level crossed
c                      (0 LE firstol LE 511).
c     lastol.......... (output, integer) Last observing level crossed
c                      (0 LE lastol LE 511).
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           olkey, firstol, lastol
      double precision  ux, uy, uz
c
c     Declaration of internal variables and arrays.
c
      integer           c1, c2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the z-component of the direction of motion.
c
      uz = sqrt(abs(1 - ux * ux - uy * uy))
      if (olkey .lt. 262144) uz = - uz
c
c     Setting the first and last crossed levels.
c
      c1      = olkey / 512
      firstol = olkey - c1 * 512
      c2      = c1 / 512
      lastol  = c1 - c2 * 512
c
      return
      end
c     --- End of routine olcrossedu
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getlgtinit(fileid, vrb, irc)
c
c     Initializing internal data needed to process records from
c     compressed logitudinal particle tracking files by means of
c     routine getlgtrecord and related ones.
c     This routine should be called immediately after opening
c     the corresponding compressed file.
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
      include 'cio2par.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           fileid, vrb, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'cio2comm.f'
      include 'libucomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           dtyp
      integer           crofieldindex
c
c     FIRST EXECUTABLE STATEMENT
c
c     The first get operation must be a real read from the file.
c
      mustread   = .true.
      mustmodify = .false.
      needmupd   = .false.
c
c     Field indices needed within this routine.
c
      idxolkey   = crofieldindex(fileid, 0, 'Observing levels',
     +                           vrb, dtyp, irc)
      idxt       = crofieldindex(fileid, 0, 'Crossing time',
     +                           vrb, dtyp, irc)
      idxx       = crofieldindex(fileid, 0, 'X coordinate',
     +                           vrb, dtyp, irc)
      idxy       = crofieldindex(fileid, 0, 'Y coordinate',
     +                           vrb, dtyp, irc)
      idxpcode   = crofieldindex(fileid, 0, 'Particle code',
     +                           vrb, dtyp, irc)
      idxprimzen = crofieldindex(fileid, 1, 'Primary zenith',
     +                           vrb, dtyp, irc)
      idxprimazi = crofieldindex(fileid, 1, 'Primary azimuth',
     +                           vrb, dtyp, irc)
c
      idxux    = crofieldindex(fileid, 0,
     +                         'Direction of motion (x component)',
     +                         0, dtyp, irc)
      uxyfld   = (irc .eq. 0)
      idxuy    = crofieldindex(fileid, 0,
     +                         'Direction of motion (y component)',
     +                         0, dtyp, irc)
c
      idxlegy  = crofieldindex(fileid, 0, 'Energy',
     +                         0, dtyp, irc)
      betafld  = ((irc .eq. 0) .and. (currcodsys .eq. 0))
c
c     Number of fields to keep in default records.
c
      call crorecstruct(fileid, nlgtdata, svintfields(1),
     +                  svintfields(11), irc)
      nlgtdata = max(svintfields(1), svintfields(11))
c
      if (nlgtdata .gt. mxlgtdata) then
        irc = 22
        if (vrb .gt. 0) then
          call errprint(0, '*', max(3, vrb), 'getlgtinit',
     +       'Not enough space to save particle data.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
      irc = 0
c
c     Some general geometric variables required.
c
      svgroundz = groundz
      svinjz    = injz
c
c     General observing levels data.
c
      call crooldata(vrb, svtotol, svolzv, svx0, irc)
c
      return
      end
c     --- End of routine getlgtinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getlgtrecord(fileid, currol, updown,
     +                      intfields, realfields, altrec, vrb, irc)
c
c     Reading records from a compressed logitudinal particle tracking
c     file and returning the read data in a "level per level" basis.
c     The position of the particle and its arrival time are corrected,
c     if needed, to match the correct values corresponding to the
c     current observing level (returned in variable currol).
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     currol.......... (output, integer) Observing level crossed
c                      by the particle.
c     updown.......... (output, integer) Up-down indicator: 1 if the
c                      particle was going upwards, -1 otherwise.
c     intfields....... (output, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). The calling
c                      program must provide enough space for this
c                      array (The minimum dimension is the maximum
c                      number of fields that can appear in a record
c                      plus 1). Positions beyond the last integer
c                      fields are used as scratch working space.
c     realfields...... (output, double precision, array(0:*)) Real
c                      fields of the record. The calling program must
c                      provide enough space for this array.
c                      In the case of particle records, the position
c                      and time information can be corrected
c                      as needed accordingly with the observing
c                      level crossed.
c     altrec.......... (output, logical) True if the record is an
c                      alternative record. False if a default record
c                      was obtained.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means that a
c                      default record was successfully read.
c                      i (i > 0) means that an alternative record of
c                      type i was successfully read. -1 means that an
c                      end-of-file condition was got from the
c                      corresponding file.  Any other value indicates
c                      a reading error (irc equals the system return
c                      code plus 10000).
c
c     Return value: (logical) True if a record was successfully read.
c     ============  False othwerise (EOF or I/O error).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      logical           getlgtrecord
      integer           fileid, currol, updown, vrb, irc
      integer           intfields(1)
      double precision  realfields(1)
      logical           altrec
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
      include 'pclecomm.f'
      include 'libucomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, pcode
      double precision  zen, azi, siz
      double precision  mass, g, xj, yj, deltat
      logical           getlgtrecord0
c
c     FIRST EXECUTABLE STATEMENT
c
c     Getting the next record from the basic i/o lgtfile processing
c     routine.
c
      getlgtrecord = getlgtrecord0(fileid, currol, updown,
     +                             intfields, realfields,
     +                             altrec, vrb, irc)
c
c     Processing the record if needed.
c
      if (mustmodify) then
c
c       Correcting the coordinates and time.
c
        if (needmupd) then
c
c         This is the first time we process this entry; need to
c         calculate some variables.
c
          if (uxyfld) then
            svux = realfields(idxux)
            svuy = realfields(idxuy)
            svuz = updown * sqrt(max(1 - svux ** 2 - svuy ** 2, 0.d0))
          else
            svux = updown * shux
            svuy = updown * shuy
            svuz = updown * shuz
          endif
c
          pcode = intfields(idxpcode)
          if (betafld .and. (pcode .ne. gammacode)) then
            if (pcode .lt. minncode) then
              mass = pclemass(pcode)
            else
              mass = nucmass(pcode)
            endif
            g    = 1 + exp(filelbcf(fileid) * realfields(idxlegy))
     +                 / mass
            svuv = ucspeedns / sqrt(1 + 1 / (g * g))
          else
            svuv = ucspeedns
          endif
c
          svax = realfields(idxx)
          svay = realfields(idxy)
          svzp = svz0(svlastol) + svmx(svlastol) * svax
     +                          + svmy(svlastol) * svay
          svax = svax + svx0(svlastol)
          svay = svay + svy0(svlastol)
          svdt = realfields(idxt) + svt0(svlastol)
c
          needmupd = .false.
c
        endif
c
c       Appliying the correction.
c
        xj     = svax - svx0(currol)
        yj     = svay - svy0(currol)
        deltat = (svz0(currol) - svzp
     +            + svmx(currol) * xj + svmy(currol) * yj) /
     +           (svuz - svux * svmx(currol) - svuy * svmy(currol))
c
        realfields(idxx) = xj + svux * deltat
        realfields(idxy) = yj + svuy * deltat
        realfields(idxt) = svdt + deltat * ucspeedns - svt0(currol)
c
      else if (irc .eq. 1) then
c
c       Got a beginning of shower record.
c
        zen  = realfields(idxprimzen)
        azi  = realfields(idxprimazi)
c
        call olcoord(svtotol, svolzv, svgroundz, svinjz, zen, azi,
     +               svx0, svy0, svz0, svt0, svmx, svmy, i)
c
        zen  = pi180 * zen
        azi  = pi180 * azi
        siz  = sin(zen)
        shux = siz * cos(azi)
        shuy = siz * sin(azi)
        shuz = cos(zen)
c
      endif
c
      return
      end
c     --- End of routine getlgtrecord
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getlgtrecord0(fileid, currol, updown,
     +                       intfields, realfields, altrec, vrb, irc)
c
c     Reading records from a compressed logitudinal particle tracking
c     file and returning the read data in a "level per level" basis.
c     The position of the particle and its arrival time are returned
c     as read from the compressed file, without corrections of any
c     kind.
c
c     Written by: S. J. Sciutto, La Plata 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     currol.......... (output, integer) Observing level crossed
c                      by the particle.
c     updown.......... (output, integer) Up-down indicator: 1 if the
c                      particle was going upwards, -1 otherwise.
c     intfields....... (output, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). The calling
c                      program must provide enough space for this
c                      array (The minimum dimension is the maximum
c                      number of fields that can appear in a record
c                      plus 1). Positions beyond the last integer
c                      fields are used as scratch working space.
c     realfields...... (output, double precision, array(0:*)) Real
c                      fields of the record. The calling program must
c                      provide enough space for this array.
c                      In the case of particle records, the position
c                      and time information are not changed, and
c                      correspond to the last observing level
c                      crossed.
c     altrec.......... (output, logical) True if the record is an
c                      alternative record. False if a default record
c                      was obtained.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means that a
c                      default record was successfully read.
c                      i (i > 0) means that an alternative record of
c                      type i was successfully read. -1 means that an
c                      end-of-file condition was got from the
c                      corresponding file.  Any other value indicates
c                      a reading error (irc equals the system return
c                      code plus 10000).
c
c     Return value: (logical) True if a record was successfully read.
c     ============  False othwerise (EOF or I/O error).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      logical           getlgtrecord0
      integer           fileid, currol, updown, vrb, irc
      integer           intfields(1)
      double precision  realfields(1)
      logical           altrec
c
c     Declaration of shared data.
c
      include 'libucomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
      logical           getcrorecord, olsavemarked
c
c     FIRST EXECUTABLE STATEMENT
c
c     Reading one record from the compressed file.
c
      if (mustread) then
c
        getlgtrecord0 = getcrorecord(fileid, intfields, realfields,
     +                               altrec, vrb, irc)
        if (irc .eq. 0) then
c
c         Got a particle record.
c
          call olcrossed(intfields(idxolkey), svupdown,
     +                   svfirstol, svlastol)
c
          updown = svupdown
c
          if (svfirstol .eq. svlastol) then
c
c           Only one level crossed ==> Single use record.
c
            mustmodify = .false.
            currol     = svfirstol
c
          else
c
c           Multiple use record involving various "particle entries".
c
            svnol = 0
            do i = svfirstol, svlastol, -svupdown
              if (olsavemarked(i, vrb, irc)) then
                svnol          = svnol + 1
                svcrool(svnol) = i
              endif
            enddo
c
            if (svnol .le. 0) then
c
c             Checking quasi-horizontal particles.
c
              do i = svfirstol, svlastol, svupdown
                if (olsavemarked(i, vrb, irc)) then
                  svnol          = svnol + 1
                  svcrool(svnol) = i
                endif
              enddo
c
              if (svnol .le. 0) then
                getlgtrecord0 = .false.
                irc = 108
                if (vrb .gt. 0) then
                  call errprint(0, '*', max(3, vrb), 'getlgtrecord0',
     +               'No saved level associated with particle record.',
     +               0, 0, 0, 0.d0, ' ')
                endif
                return
              endif
c
            endif
c
c           Keeping relevant data.
c
            do i = 1, nlgtdata
              svintfields(i) = intfields(i)
              svfltfields(i) = realfields(i)
            enddo
c
c           Returning the data corresponding to the first crossed
c           level.
c
            svcurrol   = 1
            currol     = svcrool(1)
            mustmodify = (currol .ne. svlastol)
            needmupd   = .true.
            mustread   = (svnol .le. 1)
c
          endif
c
        else
c
c         Shower headers, special primaries, or i/o error.
c
          updown     = -1
          currol     = 0
          mustmodify = .false.
c
        endif
c
      else
c
c       Returning data corresponding to another level of an already
c       read-in record.
c
        svcurrol   = svcurrol + 1
        currol     = svcrool(svcurrol)
        mustmodify = (currol .ne. svlastol)
        mustread   = (svcurrol .ge. svnol)
c
        do i = 1, nlgtdata
          intfields(i)  = svintfields(i)
          realfields(i) = svfltfields(i)
        enddo
c
        getlgtrecord0 = .true.
c
      endif
c
      return
      end
c     --- End of routine getlgtrecord0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function crospcode(pcode, splabel)
c
c     Function returning true if a given particle code corresponds to
c     a special particle, or false otherwise.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2006.
c
c
c     Arguments:
c     =========
c
c     pcode........... (input, integer) The particle code, as returned
c                      by the compressed file routines (any coding
c                      system) or an internal AIRES code.
c     splabel......... (output, integer) Label associated to the
c                      special particle, or zero if the code does not
c                      corresponds to a special particle.
c
c     Return value: (logical) .true. if the code corresponds to a
c     ============  special particle.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      logical           crospcode
      integer           pcode, splabel
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (cioinitlock .eq. 4167) then
c
c       Code coming from the CIO system.
c
        if (currcodsys .le. 1) then
          if ((pcode .ge. pescode1) .and. (pcode .le. lastescpcle))
     +    then
c
c           Special code and AIRES coding system.
c
            crospcode = .true.
            splabel   = pcode - (pescode1 - 1)
c
          else
            crospcode = .false.
            splabel   = 0
          endif
c
        else
c
          if ((pcode .ge. -99999) .and.
     +        (pcode .le. (lastescpcle - (pescode1 + 99999))) ) then
c
c           Special code and other (not AIRES) coding system.
c
            crospcode = .true.
            splabel   = pcode + 100000
c
          else
            crospcode = .false.
            splabel   = 0
          endif
c
        endif
c
      else if ((pcode .ge. pescode1) .and.
     +         (pcode .le. lastescpcle) ) then
c
c       Special code coming from the AIRES internal system.
c
        crospcode = .true.
        splabel   = pcode - pescode1 + 1
c
      else
        crospcode = .false.
        splabel   = 0
      endif
c
      return
      end
c     --- End of routine crospcode
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'libutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
