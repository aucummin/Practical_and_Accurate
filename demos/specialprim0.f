c
c     File specialprim0.f: A sample FORTRAN program to illustrate the
c                          use of an external module to inject special
c                          primary particles in AIRES simulations.
c
c
c     ------------------------------------------------------------------
c     TO COMPILE THIS FORTRAN PROGRAM:
c
c     This program contains calls to some of the routines included in
c     the AIRES object library (generally named libAires.a). Therefore,
c     the library must be specified as an object library in the link
c     step of the compilation process. In the file "config" you can set
c     then name and placement of this library. If your home directory
c     is, say, "/myhome", the default full path for this library is
c     "/myhome/aires/lib/libAires.a". If "f77" is the command you use
c     to compile FORTRAN programs, then the command:
c
c       f77 -o specialprim0 specialprim0.f -L/myhome/aires/lib -lAires
c
c     will do the work in the default case. In some systems it may be
c     necessary to put some qualifiers to the original "f77" commands.
c     If plain "f77" failed, try one of the following commands using
c     one or more qualifiers:
c
c       f77 -O5  ....                             for DEC Alpha FORTRAN.
c       f77 +U77 +O3  ....                        for HP FORTRAN.
c       f77 -qstrict -qcharlen=10000 -O3  ....    for IBM FORTRAN.
c       f77 -O3  ....                             for SGI FORTRAN.
c       f77 -native -O4  ....                     for SUN FORTRAN.
c     ------------------------------------------------------------------
c
      program specialprim
c
c     Generating primary particles via an external module, an example.
c
      implicit none
c
c     Declaration of internal variables and arrays.
c
      double precision  twopi, chpionmass
      parameter         (twopi = 2 * 3.14159265359d0)
      parameter         (chpionmass = 0.13956995d0)
c
      integer           shower_number
      double precision  primary_energy
      double precision  default_injection_position(3)
      double precision  injection_depth, ground_depth
      double precision  ground_altitude, d_ground_inj
      double precision  shower_axis(3)
c
      character*200     parstring, auxstring
      character*120     task
      character*16      spname
      integer           parstringlen, auxlen, tasklen, splen
      integer           task_version
c
      double precision  epp, epp1, epp2, eremain
      double precision  gamma, angle, phi, cphi, sphi
      double precision  ux, uy, uz, xi, yi, zi, xy
      double precision  shower_zenith, shower_azim
c
      integer           irc, oldmv
      integer           i, nrp
c
      double precision  urandom, urandomt, zfromdepth
c
c     FIRST EXECUTABLE STATEMENT
c
c     Initializing the AIRES-external module (this program) interface.
c
c     Routine "speistart" makes a series of internal initializations
c     and checks, and simoultaneously retrieves some basic information
c     about the characteristics of the current invocation of the
c     present module, namely:
c
c     shower_number          The current shower number.
c
c     primary_energy         The primary energy for this shower
c                            (in GeV). The sum of the energies of all
c                            the primaries created by this module
c                            cannot overpass this value.
c
c     default_injection_position   Array with 3 elements containing the
c                            (x, y, z) coordinates (with respect to
c                            the AIRES coordinate system, in meters) of
c                            the default injection point.
c
c     injection_depth        Vertical atmospheric depth of the
c                            injection point, in g/cm2.
c
c     ground_altitude        Ground_altitude in m.a.s.l.
c
c     ground_depth           Vertical atmospheric depth of the
c                            ground level, in g/cm2.
c
c     d_ground_inj           Distance from the default injection point
c                            to the ground plane, measured along the
c                            shower axis, in meters.
c
c     shower_axis            Array with 3 elements containing the
c                            cartesian components (with respect to the
c                            AIRES coordinate system) of the unitary
c                            vector parallel to the shower axis,
c                            pointing from the injection point towards
c                            the ground:
c                            shower_axis(1) = -sin(zenith) * cos(azim)
c                            shower_axis(2) = -sin(zenith) * sin(azim)
c                            shower_axis(3) = -cos(zenith)
c
      call speistart(shower_number, primary_energy,
     +               default_injection_position, injection_depth,
     +               ground_altitude, ground_depth,
     +               d_ground_inj, shower_axis)
c
c     "speistart" MUST ALWAYS be called at the very beginning of the
c     program, always BEFORE any other call to the AIRES library
c     routines.
c
c
c     Retrieving additional information (These calls are
c     optional).
c
c     Parameters passed from the IDL input file.
c
      call speigetpars(parstring, parstringlen)
c
c     Task name (and version).
c
      call speitask(task, tasklen, task_version)
c
c     Name of the special particle, as specified in the corresponding
c     IDL directive.
c
      call sprimname(spname, splen)
c
c     User defined global variables (The current value of the variable
c     is retrieved into string "auxtring", see input file and the
c     documentation for details).
c
      call getglobal('MYVAR2', i, auxstring, auxlen)
c
c     Etc., etc. (There are other routines to retrieve environmental
c     data (see the documentation).
c
c     Setting the macro version. The version number must be a positive
c     integer in the range [1, 759375]. "oldmv" returns the version
c     number corresponding to the previous invocation of the external
c     module (zero if it was never set).
c
      call speimv(010203, oldmv)
c
c     The preceding call, like other settings described in the
c     documentation, can be placed anywhere after the call to
c     "speistart" and before the call to "speiend".
c
c
c     INJECTING THE PRIMARY PARTICLES.
c
c     The code included here will inject several particles at
c     different altitudes. Its purpose is just to illustrate how to
c     use the external interface. The algorithm does not intend to
c     be physically realistic in any sense.
c
      epp     = primary_energy * urandomt(0.01d0) * urandomt(0.01d0)
      eremain = primary_energy - epp
c
c     "urandom()" and "urandomt(threshold)" are the calls for the AIRES
c     random number generators.
c     Notice that the generator was NOT initialized. Instead, the
c     status of the generator is get from the main program, and
c     this allows having different numbers at each call. Of course,
c     the final status is transferred back to the calling program
c     to ensure proper operation with random numbers. It is
c     STRONGLY RECOMMENDED to use the AIRES generator.
c
c     The energy epp will be split into two parts and a pair of
c     mesons pi+, pi- will be injected. The original injection
c     point is the one returned by "speistart".
c
      epp1 = epp * urandom()
      epp2 = epp - epp1
c
c     The directions of motion of these particles form a (small)
c     angle with the shower axis.
c
      gamma = 1 + epp1 / chpionmass
      angle = 1 / gamma
      uz    = cos(angle)
      phi   = twopi * urandom()
      cphi  = cos(phi)
      sphi  = sin(phi)
      ux    = angle * cphi
      uy    = angle * sphi
c
c     Inserting the first particle in the list of primary particle.
c     Notice the particle code (-11) indicating that the particle is
c     a pi-. The coordinate system to express the direction of motion
c     has its z axis parallel to the shower axis (The integer 1 selects
c     this system, use 0 for the AIRES coordinate system with
c     horizontal xy plane at sea level). The particle weight is 1.
c
      call spaddp0(-11, epp1, 1, ux, uy, uz, 1.d0, irc)
c
c     Inserting the other particle, a pi+.
c
      gamma = 1 + epp2 / chpionmass
      angle = 1 / gamma
      uz    = cos(angle)
      ux    = -angle * cphi
      uy    = -angle * sphi
c
      call spaddp0(+11, epp2, 1, ux, uy, uz, 1.d0, irc)
c
c     Changing the injection point (10 km far from the original one,
c     along the shower axis).
c
      call spinjpoint(1, 0.d0, 0.d0, 1.d4, 1, 1.d0, irc)
c
c     Inserting another two additional particles (protons), in the
c     direction of the shower axis.
c
      epp     = eremain * urandomt(0.01d0) * urandomt(0.01d0)
      eremain = eremain - epp
c
      epp1 = epp * urandom()
      epp2 = epp - epp1
c
      call spaddp0(31, epp1, 1, 0.d0, 0.d0, 1.d0, 1.d0, irc)
      call spaddp0(31, epp2, 1, 0.d0, 0.d0, 1.d0, 1.d0, irc)
c
c     Changing again the injection point. Notice the use of the AIRES
c     coordinate system. The new injection point is out of the shower
c     axis. The injection time is also given absolutely (280000 ns).
c     Notice the use of "zfromdepth" to convert an atmospheric depth
c     (in g/cm2) to altitude above sea level (in meters). The functions
c     "zfromdepth" and "depthfromz" can be used (below 118 km.a.s.l.)
c     normally (anywhere in the program after having called
c     "speistart"), and their results correspond to the atmospheric model
c     selected in the IDL input file.
c
      shower_zenith = acos(- shower_axis(3))
      shower_azim   = atan2(- shower_axis(2), - shower_axis(1))
c
      angle = shower_zenith * 1.1d0
      zi    = zfromdepth(122.d0, i)
      xy    = (zi - ground_altitude) * tan(angle)
      angle = shower_azim * 1.1d0
      xi    = xy * cos(angle)
      yi    = xy * sin(angle)
c
      call spinjpoint(0, xi, yi, zi, 0, 280.d3, irc)
c
c     Injecting the remaining particles, gamma rays, all parallel to
c     the "secondary" axis.
c
      ux  = -ux
      uy  = -uy
      uz  = -uz
c
      nrp = 1 + 5 * urandom()
c
      do i = 2, nrp
        epp     = eremain * urandomt(0.01d0) * urandomt(0.01d0)
        eremain = eremain - epp
        call spaddp0(1, epp, 0, ux, uy, uz, 1.d0, irc)
      enddo
c
      call spaddp0(1, eremain, 0, ux, uy, uz, 1.d0, irc)
c
c     No more particles to be injected as primary particles.
c
c     Completing the main program-external module interchange.
c     The integer argument of routine "speiend" is an integer return
c     code passed to the calling program. 0 means normal return.
c
      call speiend(0)
c
c     "speiend" MUST always be called at the end of the program,
c     always AFTER any other call to the AIRES library
c     routines.
c
      end
c
