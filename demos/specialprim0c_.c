/*
    File specialprim0c_.c: A sample C program to illustrate the use of an
                           external module to inject primary particles in
                           AIRES simulations.


    ------------------------------------------------------------------
    TO COMPILE THIS C PROGRAM:

    This program contains calls to some of the routines included in
    the AIRES object library (generally named libAires.a). Therefore,
    the library must be specified as an object library in the link
    step of the compilation process. In the file "config" you can set
    then name and placement of this library. If your home directory
    is, say, "/myhome", the default full path for this library is
    "/myhome/aires/lib/libAires.a". If "cc" is the command you use
    to compile C programs, then the command:

      cc specialprim0c_.c -L/myhome/aires/lib -lAires

    will do the work in the default case. Since some of the library
    modules used are FORTRAN routines, in some systems it may be
    necessary to specify also additional FORTRAN libraries in the C
    compile instruction, as well to put some qualifiers to the original
    "cc" command. If you cannot compile this program using the previous
    command and do not know how to proceed, you should ask a UNIX expert
    to have the demo compiled for you. In some systems the necessary
    FORTRAN libraries can be included using the FORTRAN compile command
    to link the program, like in the following instructions:

      cc -c specialprim0c_.c   # will produce object file
      f77   specialprim0c_.o -L/myhome/aires/lib -lAires

    Some FORTRAN compilers automatically detect the C format in the file
    "specialprim0c_.c" and invoke the C compiler to obtain the corresponding
    object file. If plain "f77" failed as well, it may be necessary to
    use one of the following commands employing one or more qualifiers:

      f77 -O5  ....                             for DEC Alpha FORTRAN.
      f77 +U77 +O3  ....                        for HP FORTRAN.
      f77 -qstrict -qcharlen=10000 -O3  ....    for IBM FORTRAN.
      f77 -O3  ....                             for SGI FORTRAN.
      f77 -native -O4  ....                     for SUN FORTRAN.

    NOTE ABOUT FORTRAN EXTERNAL NAMES: Some FORTRAN compilers append
    one or more underscores ("_") to function and subroutine names. If
    it is the case in your system, then you must accordingly modify
    all the routine names marked with "__" between comment marks. If
    just a single underscore is needed, you can use the file
    "specialprim0c_.c" that comes with the present AIRES distribution.
    ------------------------------------------------------------------
*/
#include <math.h>

int main()
    /*
       Generating primary particles via an external module, an example.
    */
    {

#      define    twopi        (2 * 3.14159265359)
#      define    chpionmass   0.13956995

       int       shower_number;
       double    primary_energy;
       double    default_injection_position[4];
       double    injection_depth, ground_depth;
       double    ground_altitude, d_ground_inj;
       double    shower_axis[4];

       char      parstring[200], auxstring[200], task[120], spname[17];
       int       parstringlen, auxlen, tasklen, splen;
       int       task_version;

       double    th0 = 0.01;
       int       pipluscode = 11, piminuscode = -11;
       int       gammacode  = 1, protoncode = 31;

       double    epp, epp1, epp2, eremain;
       double    gamma, angle, phi, cphi, sphi;
       double    ux, uy, uz, xi, yi, zi, xy;
       double    shower_zenith, shower_azim;

       int       csys, t0beta;
       double    t0, beta, wt;

       int       irc, newmv, oldmv;
       int       i, nrp;

       double    urandom_(), urandomt_(), zfromdepth_();    /*__*/
       void      speistart_(), speiend_();                  /*__*/
       void      speigetparsc(), speitaskc(), sprimnamec(); /*__*/
       void      getglobalc();                              /*__*/
       void      speimv_(),spaddp0_(), spinjpoint_();       /*__*/

       /* Initializing the AIRES-external module (this program) interface.

      Routine "speistart" makes a series of internal initializations
      and checks, and simoultaneously retrieves some basic information
      about the characteristics of the current invocation of the
      present module, namely:

      shower_number          The current shower number.

      primary_energy         The primary energy for this shower
                             (in GeV). The sum of the energies of all
                             the primaries created by this module
                             cannot overpass this value.

      default_injection_position   Array with 3 elements containing the
                             (x, y, z) coordinates (with respect to
                             the AIRES coordinate system, in meters) of
                             the default injection point.

      injection_depth        Vertical atmospheric depth of the
                             injection point, in g/cm2.

      ground_altitude        Ground_altitude in m.a.s.l.

      ground_depth           Vertical atmospheric depth of the
                             ground level, in g/cm2.

      d_ground_inj           Distance from the default injection point
                             to the ground plane, measured along the
                             shower axis, in meters.

      shower_axis            Array with 3 elements containing the
                             cartesian components (with respect to the
                             AIRES coordinate system) of the unitary
                             vector parallel to the shower axis,
                             pointing from the injection point towards
                             the ground:
                               shower_axis(1) = -sin(zenith) cos(azim)
                               shower_axis(2) = -sin(zenith) sin(azim)
                               shower_axis(3) = -cos(zenith)
      */

      speistart_(&shower_number, &primary_energy,                 /*__*/
                 &default_injection_position[1], &injection_depth,
                 &ground_altitude, &ground_depth,
                 &d_ground_inj, &shower_axis[1]);



      /* "speistart" MUST always be called at the very beginning of the
      program, always BEFORE any other call to the AIRES library
      routines.                                                        */


      /* Retrieving additional information (These calls are optional). */

      /* Parameters passed from the IDL input file.                    */

      speigetparsc(parstring, &parstringlen);

      /* Task name (and version).                                      */

      speitaskc(task, &tasklen, &task_version);

      /* Name of the special particle, as specified in the corresponding
         IDL directive.                                                */

      sprimnamec(spname, &splen);

      /* User defined global variables (The current value of the variable
         is retrieved into string "auxtring", see input file and the
         documentation for details).                                   */

      getglobalc("MYVAR2", &i, auxstring, &auxlen);

      /* Etc., etc. (There are other routines to retrieve environmental
         data (see the documentation).                                 */

      /* Setting the macro version. The version number must be a positive
      integer in the range [1, 759375]. "oldmv" returns the version
      number corresponding to the previous invocation of the external
      module (zero if it was never set).                               */

      newmv = 010203;
      speimv_(&newmv, &oldmv);                                     /*__*/

      /* The preceding call, like other settings described in the
      documentation, can be placed anywhere after the call to
      "speistart" and before the call to "speiend".                    */


      /*    INJECTING THE PRIMARY PARTICLES.                           */

      /* The code included here will inject several particles at
      different altitudes. Its purpose is just to illustrate how to
      use the external interface. The algorithm does not intend to
      be physically realistic in any sense.                            */

      epp     = primary_energy * urandomt_(&th0) * urandomt_(&th0); /*__*/
      eremain = primary_energy - epp;

      /* "urandom_()" and "urandomt_(threshold)" are the calls for the AIRES
      random number generators.
      Notice that the generator was NOT initialized. Instead, the
      status of the generator is get from the main program, and
      this allows having different numbers at each call. Of course,
      the final status is transferred back to the calling program
      to ensure proper operation with random numbers. It is
      STRONGLY RECOMMENDED to use the AIRES generator.               */

      /* The energy epp will be split into two parts and a pair of
      mesons pi+, pi- will be injected. The original injection
      point is the one returned by "speistart".                      */

      epp1 = epp * urandom_();                                   /*__*/
      epp2 = epp - epp1;

      /* The directions of motion of these particles form a (small)
         angle with the shower axis.                                 */

      gamma = 1 + epp1 / chpionmass;
      angle = 1 / gamma;
      uz    = cos(angle);
      phi   = twopi * urandom_();                                /*__*/
      cphi  = cos(phi);
      sphi  = sin(phi);
      ux    = angle * cphi;
      uy    = angle * sphi;

      /* Inserting the first particle in the list of primary particle.
      Notice the particle code (-11) indicating that the particle is
      a pi-. The coordinate system to express the direction of motion
      has its z axis parallel to the shower axis (The integer 1 selects
      this system, use 0 for the AIRES coordinate system with
      horizontal xy plane at sea level). The particle weight is 1.    */

      csys = 1;
      wt   = 1;

      spaddp0_(&piminuscode, &epp1, &csys, &ux, &uy, &uz, &wt, &irc); /*__*/

      /* Inserting the other particle, a pi+.                         */

      gamma = 1 + epp2 / chpionmass;
      angle = 1 / gamma;
      uz    = cos(angle);
      ux    = -angle * cphi;
      uy    = -angle * sphi;

      spaddp0_(&pipluscode, &epp2, &csys, &ux, &uy, &uz, &wt, &irc); /*__*/

      /* Changing the injection point (10 km far from the original one,
         along the shower axis).                                      */

      csys   = 1;
      xi     = 0;
      yi     = 0;
      zi     = 10000;
      t0beta = 1;
      beta   = 1;
      spinjpoint_(&csys, &xi, &yi, &zi, &t0beta, &beta, &irc);      /*__*/

      /* Inserting another two additional particles (protons), in the
	 direction of the shower axis.                                  */

      csys = 1;
      ux   = 0;
      uy   = 0;
      uz   = 1;
      wt   = 1;

      epp     = eremain * urandomt_(&th0) * urandomt_(&th0);        /*__*/
      eremain = eremain - epp;

      epp1 = epp * urandom_();                                      /*__*/
      epp2 = epp - epp1;

      spaddp0_(&protoncode, &epp1, &csys, &ux, &uy, &uz, &wt, &irc);  /*__*/
      spaddp0_(&protoncode, &epp2, &csys, &ux, &uy, &uz, &wt, &irc);  /*__*/

      /* Changing again the injection point. Notice the use of the AIRES
      coordinate system. The new injection point is out of the shower
      axis. The injection time is also given absolutely (280000 ns).
      Notice the use of "zfromdepth" to convert an atmospheric depth
      (in g/cm2) to altitude above sea level (in meters). The functions
      "zfromdepth" and "depthfromz" can be used (below 118 km.a.s.l.)
      normally (anywhere in the program after having called
      "speistart"), and their results correspond to the atmospheric model
      selected in the IDL input file.                                  */

      shower_zenith = acos(- shower_axis[3]);
      shower_azim   = atan2(- shower_axis[2], - shower_axis[1]);

      angle  = shower_zenith * 1.1;
      zi     = 122;
      zi     = zfromdepth_(&zi, &i);                                /*__*/
      xy     = (zi - ground_altitude) * tan(angle);
      angle  = shower_azim * 1.1;
      xi     = xy * cos(angle);
      yi     = xy * sin(angle);
      csys   = 0;
      t0beta = 0;
      t0     = 280000;

      spinjpoint_(&csys, &xi, &yi, &zi, &t0beta, &t0, &irc);        /*__*/

      /* Injecting the remaining particles, gamma rays, all parallel to
         the "secondary" axis.                                      */

      ux  = -ux;
      uy  = -uy;
      uz  = -uz;

      nrp = 1 + 5 * urandom_();                                        /*__*/

      for (i = 2; i <= nrp; i++) {
        epp     = eremain * urandomt_(&th0) * urandomt_(&th0);         /*__*/
        eremain = eremain - epp;
        spaddp0_(&gammacode, &epp, &csys, &ux, &uy, &uz, &wt, &irc);   /*__*/
      }

      spaddp0_(&gammacode, &eremain, &csys, &ux, &uy, &uz, &wt, &irc); /*__*/

      /* No more particles to be injected as primary particles.       */

      /* Completing the main program-external module interchange.
      The integer argument of routine "speiend" is an integer return
      code passed to the calling program. 0 means normal return.      */

      irc = 0;
      speiend_(&irc);                                              /*__*/

      /* "speiend" MUST always be called at the end of the program,
      always AFTER any other call to the AIRES library
      routines.                                                       */

      return(0);

    }


