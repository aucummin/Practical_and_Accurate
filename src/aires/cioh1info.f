c
c     FILE: cioh1info.f                     Creation date: 16/JUL/1997.
c                                       LAST MODIFICATION: 01/MAY/2003.
c
c     Aires compressed i/o system (IIIa): Routines to process already
c     created compressed files: Getting information contained in
c     section 1 of file headers.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine croheader1info(ouflag)
c
c     Getting information from an already read compressed file header.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000.
c
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
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           ouflag
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u, minu, maxu
      integer           aux1
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if there is an already opened file.
c
      if (lastopciofile2 .le. 0) then
        call errprint(ouflag, '$CU1', 3, 'croheader1info',
     +                ' ', 0, 0, 0, 0.d0, ' ')
        return
      endif
c
c     Selecting output units.
c
      call minmaxou(ouflag, minu, maxu)
c
 2010 format(a25, ': ', 3a)
 2011 format(a25, '  ', 3a)
c
c     Printing general information.
c
      call scontain(crhost, crhostlen, 52 - cruserlen, auxline, aux1)
      do u = minu, maxu
        write(u, 2010) 'Created by', cruser(1:cruserlen), '@',
     +                 auxline(1:aux1)
        write(u, 2010) 'Creation date', datistr0(1)
        write(u, *)
      enddo
c
c     Printing a summary of the input data.
c
      call inpsry(ouflag)
c
      return
c
      end
c     --- End of routine croheader1info
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crotaskid(tskname, tsknamelen, tskversion, startdate)
c
c     Getting task name and date from an already read compressed file
c     header.
c
c     Written by: S. J. Sciutto, Fermilab 1999.
c
c
c     Arguments:
c     =========
c
c     tskname......... (output, character*(*)) Task name (maximum
c                      length is 64 characters).
c     tsknamelen...... (output, integer) Length of task name.
c     tskversion...... (output, integer) Task version.
c     startdate....... (output, character*20) Task starting date,
c                      in the format "dd/Mmm/yyyy hh:mm:ss".
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
      character*(*)     tskname
      integer           tsknamelen, tskversion
      character*20      startdate
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      tskname    = taskname
      tsknamelen = tasknamelen
      tskversion = tasknamever
      startdate  = datistr0(1)
c
      return
c
      end
c     --- End of routine crotaskid
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine croinputdata0(intdata, realdata, shprimcode, shprimwt)
c
c     Copying into arrays input data parameters from an already read
c     compressed file header.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2003.
c
c
c     Arguments:
c     =========
c
c     intdata......... (output, integer, array(*)) Integer data array.
c                      The calling program must provide enough space
c                      for it. The following list describes the
c                      different data items:
c                      ( 1) Number of different primary particles.
c                      ( 2) Number of special particles defined.
c                      ( 3-4) Reserved for future use.
c                      ( 5) Primary energy distribution: 0 fixed
c                           energy; 1 varying energy.
c                      ( 6) Zenith angle distribution: 0 fixed angle;
c                           1 cos distribution; 2 sin cos distribution.
c                      ( 7) Azimuth angle distribution: 0 (10) fixed
c                           angle (geographic azimuth); 1 (11) varying
c                           angle (geographic azimuths).
c                      ( 8) Number of observing levels.
c                      ( 9) Atmospheric model label.
c                      (10) Thinning switch: 0 disabled, 1 enabled.
c                      (11-14) Reserved for future use.
c                      (15) First shower number.
c     realdata........ (output, double precision, array(*)) Real data
c                      array. The calling program must provide enough
c                      space for it. The following list describes the
c                      different data items:
c                      ( 1) Minimum primary energy (GeV).
c                      ( 2) Maximum primary energy (GeV).
c                      ( 3) Gamma of energy distribution.
c                      ( 4) Minimum zenith angle (deg).
c                      ( 5) Maximum zenith angle (deg).
c                      ( 6) Minimum azimuth angle (deg).
c                      ( 7) Maximum azimuth angle (deg).
c                      ( 8) Thinning energy parameter.
c                      ( 9) Injection altitude (m).
c                      (10) Injection depth (g/cm2).
c                      (11) Ground altitude (m).
c                      (12) Ground depth (g/cm2).
c                      (13-14) Reserved for future use.
c                      (15) Altitude of first observing level (m).
c                      (16) Depth of first observing level (g/cm2).
c                      (17) Altitude of last observing level (m).
c                      (18) Depth of last observing level (g/cm2).
c                      (19) Distance between consecutive observing
c                           levels in g/cm2.
c                      (20) Site latitude (deg).
c                      (21) Site longitude (deg).
c                      (22) Geomagnetic field strength (nT).
c                      (23) Local geomagnetic inclination (deg).
c                      (24) Local geomagnetic declination (deg).
c                      (25) Amplitude of relative fluctuation of B.
c                      (26) Event date.
c                      (27-29) Reserved for future use.
c                      (30) Minimum lateral distance used for ground
c                           particle histograms (m).
c                      (31) Maximum lateral distance used for ground
c                           particle histograms (m).
c                      (32) Minimum energy used for histograms (GeV).
c                      (33) Maximum energy used for histograms (GeV).
c                      (34-35) Reserved for future use.
c                      (36) Minimum radial distance parameter for the
c                           current compressed file (m).
c                      (37) Maximum radial distance parameter for the
c                           current compressed file (m).
c     shprimcode...... (output, integer, array(*)) For i from 1 to
c                      intdata(1), shprimcode(i) gives the
c                      corresponding primary particle code. The
c                      coding system used is the one defined when
c                      starting the cio system.
c     shprimwt........ (output, double precision, array(*)) For i from
c                      1 to intdata(1), shprimwt(i) gives the
c                      corresponding primary particle weight. This
c                      weight is 1 in the single primary case.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'pclepar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           intdata(99)
      double precision  realdata(99)
      integer           shprimcode(99)
      double precision  shprimwt(99)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'pclecomm.f'
      include 'kernelcomm.f'
      include 'hdatacomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Assigning integer data.
c
      intdata( 1)  = nshprimary
      intdata( 2)  = nescpcles
c
      if (pryenergymin .eq. pryenergymax) then
        intdata( 5) = 0
      else
        intdata( 5) = 1
      endif
c
      if (pryzenithmin .eq. pryzenithmax) then
        intdata( 6) = 0
      else
        intdata( 6) = varzendis
      endif
c
      if (pryazimmin .eq. pryazimmax) then
        intdata( 7) = 0
      else
        intdata( 7) = 1
      endif
      if (geognorth) intdata( 7) = intdata( 7) + 10
c
      intdata( 8)  = nobslevels
      intdata( 9)  = min(1, atmoslabel)
c
      if (thinningon) then
        intdata(10) = 1
      else
        intdata(10) = 0
      endif
c
      intdata(15)  = firstshowernon1 + 1
c
c     Assigning primary particle data.
c
      do i = 1, nshprimary
        shprimcode(i) = cio2pcledecode(maxpcle + shprimary(i))
        shprimwt(i)   = shprimarywt(i)
      enddo
c
c     Assigning real data.
c
      realdata( 1) = pryenergymin
      realdata( 2) = pryenergymax
      realdata( 3) = pryenergyslp
      realdata( 4) = pryzenithmin
      realdata( 5) = pryzenithmax
      realdata( 6) = pryazimmin
      realdata( 7) = pryazimmax
      realdata( 8) = ethinpar
      realdata( 9) = injz
      realdata(10) = injdepth
      realdata(11) = groundz
      realdata(12) = groundepth
      realdata(15) = obslevmaxz
      realdata(16) = obslevmind
      realdata(17) = obslevminz
      realdata(18) = obslevmaxd
      realdata(19) = obslevstep
      realdata(20) = sitelat(isite)
      realdata(21) = sitelong(isite)
      realdata(22) = geob
      realdata(23) = geobi
      realdata(24) = geobd
      realdata(25) = geobfluc
      realdata(26) = eventdate
      realdata(30) = rminhis
      realdata(31) = rmaxhis
      realdata(32) = eminhis
      realdata(33) = emaxhis
c
      if (lastopciofile2 .gt. 0) then
        i            = 14 + 2 * pifiledefno(lastopciofile2)
        realdata(36) = fidata(0, i)
        realdata(37) = fidata(0, i + 1)
      else
        realdata(36) = 0
        realdata(37) = 0
      endif
c
      return
c
      end
c     --- End of routine croinputdata0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crospnames(nspp, spname)
c
c     Retrieving the names of the defined special particles.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     nspp............ (output, integer) The number of special
c                      particles defined.
c     spname.......... (output, character*16, array(nspp)) Array
c                      containing the names of the defined particles.
c                      The calling program must provide enough
c                      space for this array.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           nspp
      character*(*)     spname(11)
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      nspp = nescpcles
c
      do i = 1, nspp
        spname(i) = escpclename(i)
      enddo
c
      return
      end
c     --- End of routine crospnames
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crospmodinfo(spname, spmodu, spml, sppars, sppl, irc)
c
c     Retrieving information about the external module associated to a
c     already defined special particle.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     spname.......... (input, character*16) The name of the special
c                      particle.
c     spmodu.......... (output, character*(*)) The name of the
c                      associated module. The calling program must
c                      provide enough space for this string.
c     spml............ (output, integer) Length of string "spmodu".
c     sppars.......... (output, character*(*)) String containing the
c                      parameters passed to the module. The calling
c                      program must provide enough space for this string.
c     sppl............ (output, integer) Length of string "sppars".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      character*(*)     spname, spmodu, sppars
      integer           spml, sppl, irc
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, nescpcles
        j = i
        if (spname .eq. escpclename(i)) goto 1010
      enddo
c
c     Name was not found.
c
      spml = 0
      sppl = 0
      irc  = 8
      return
c
 1010 continue
c
      spml   = escmacropos(2, j) - escmacropos(1, j - 1)
      spmodu = escpclemacro(escmacropos(1, j):escmacropos(2, j))
      if (escmacropos(4, j) .gt. escmacropos(3, j)) then
        sppl   = escmacropos(4, j) - escmacropos(3, j)
        sppars = escpclemacro(escmacropos(3, j) + 1:escmacropos(4, j))
      else
        sppl   = 0
        sppars = ' '
      endif
c
      return
      end
c     --- End of routine crospmodinfo
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function olsavemarked(obslev, vrb, irc)
c
c     Function returning true if an observing level is marked to be
c     saved into longitudinal files, false otherwise.
c
c     Written by: S. J. Sciutto, La Plata 1998, 1999.
c
c
c     Arguments:
c     =========
c
c     obslev.......... (input, integer) The number of observing level.
c                      If it is out of range, then "false" will be
c                      returned.
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
c     Return value: (logical) .true. if the level is marked.
c     ============
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      logical           olsavemarked
      integer           obslev, vrb, irc
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
      include 'hdatacomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if there is an already opened file.
c
      if (lastopciofile2 .le. 0) then
        if (vrb .gt. 0) then
          call errprint(0, '$CU1', max(3, vrb), 'olsavemarked',
     +       ' ', 0, 0, 0, 0.d0, ' ')
        endif
        irc          = 12
        olsavemarked = .false.
        return
      endif
c
c     Evaluating the marker.
c
      if ((obslev .ge. 1) .and. (obslev .le. nobslevels)) then
c
        if (vrb .eq. 1) then
          call errprint(0, '*', 1, 'olsavemarked',
     +       'Observing level marker successfully set.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        irc = 0
c
        olsavemarked = olfilesave(obslev)
c
      else
c
        if (vrb .gt. 0) then
          call errprint(0, '*', 2, 'olsavemarked',
     +       'Observing level index out of range.',
     +       1, obslev, 0, 0.d0, ' ')
        endif
        irc = 2
c
        olsavemarked = .false.
c
      endif
c
      return
      end
c     --- End of routine olsavemarked
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crooldata(vrb, nobslev, olzv, oldepth, irc)
c
c     Calculating observing levels information from data contained in
c     a compressed data file header.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
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
c     nobslev......... (output, integer) The number of observing
c                      levels.
c     olzv............ (output, double precision, array(*)) Altitudes
c                      (m) of the corresponding observing levels, from
c                      1 to nobslevels. The calling program must ensure
c                      that there is enough space for this array.
c     oldepth......... (output, double precision, array(*)) Atmospheric
c                      depth (g/cm2) of the corresponding observing
c                      levels, from 1 to nobslevels. The calling
c                      program must ensure that there is enough space
c                      for this array.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           vrb, nobslev, irc
      double precision  olzv(1)
      double precision  oldepth(1)
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      double precision  zfromdepth
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if there is an already opened file.
c
      if (lastheadfile2 .lt. 0) then
        irc = 12
        if (vrb .gt. 0) then
          call errprint(0, '$CU1', max(3, vrb), 'crooldata',
     +       ' ', 0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
c     Evaluating the observing levels data.
c
      nobslev = nobslevels
c
      do i = 0, nobslev - 1
        j = i + 1
        oldepth(j) = obslevmind +
     +               i * ((obslevmaxd - obslevmind) /
     +                    (nobslev - 1))
        olzv(j)    = zfromdepth(oldepth(j), k)
      enddo
c
      if (vrb .eq. 1) then
        call errprint(0, '*', 1, 'crooldata',
     +     'Observing level data successfully set.',
     +     0, 0, 0, 0.d0, ' ')
      endif
      irc = 0
c
      return
      end
c     --- End of routine crooldata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cioh1info.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
