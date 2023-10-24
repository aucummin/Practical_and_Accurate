c
c     File uprimary.f: A sample FORTRAN program to illustrate the
c                      use of an external module to inject a
c                      primary particle going upwards.
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
      program uprimary
c
c     Generating primary particles going upwards.
c
      implicit none
c
c     Compilation parameters.
c
      integer           macro_version
c
      parameter         (macro_version = 0100)
c
      double precision  cspeed
c
      parameter         (cspeed = 299792458.d0)
c
c     Declaration of internal variables and arrays.
c
c
      integer           shower_number
      double precision  primary_energy
      double precision  default_injection_position(3)
      double precision  injection_depth, ground_depth
      double precision  ground_altitude, d_ground_inj
      double precision  shower_axis(3)
c
      integer           primary_code, ncodes
      double precision  invaxis(3)
c
      character*200     parstring
      integer           parstringlen
      integer           irc, jrc, oldmv
      integer           i, i1, i2
      double precision  x0, y0, z0, r0
c
c     FIRST EXECUTABLE STATEMENT
c
c     Initializing the AIRES-external module (this program) interface.
c
      call speistart(shower_number, primary_energy,
     +               default_injection_position, injection_depth,
     +               ground_altitude, ground_depth,
     +               d_ground_inj, shower_axis)
c
c     Additional AIRES initializations.
c
      call ciorinit(0, 0, 0, jrc)
c
c     Parameters passed from the IDL input file. In this module
c     it is expected a single parameter with the name of the
c     primary particle.
c
      irc = 80
c
      call speigetpars(parstring, parstringlen) 
c
      if (parstringlen .le. 0) goto 1900
      print *, parstringlen, ' ', parstring(1:parstringlen)
      print *, 'SA', shower_axis
c
      irc = 60
      i2  = 0
      call getpclecode(.true., parstring, parstringlen, i1, i2,
     +                 ncodes, primary_code, jrc)
      print *, jrc, ncodes, primary_code
      if (jrc .ge. 3) goto 1900
c
c     Injecting the particle.
c
c     Inverted axis vector.
c
      do i = 1, 3
        invaxis(i) = - shower_axis(i)
      enddo
c
c     Injection point is set 5 cm above ground level, in the
c     direction of the shower axis.
c
      r0 = 0.05d0
      x0 = r0 * invaxis(1)
      y0 = r0 * invaxis(2)
      z0 = r0 * invaxis(3) + ground_altitude
c
      call spinjpoint(0, x0, y0, z0, 1, 1.d0, jrc)
c
c     Now the particle is injected moving upwards along the shower
c     axis, and with weight one.
c
      call spaddp0(primary_code, primary_energy,
     +             0, invaxis(1), invaxis(2), invaxis(3),
     +             1.d0, jrc)
c
      irc = 0
c
 1900 continue
c
c     Setting the macro version and completing the
c     main program-external module interchange.
c     The integer argument of routine "speiend" is an integer return
c     code passed to the calling program. 0 means normal return.
c
      call speimv(macro_version, oldmv)
      call speiend(irc)
c
c     "speiend" MUST always be called at the end of the program,
c     always AFTER any other call to the AIRES library
c     routines.
c
      end
c
