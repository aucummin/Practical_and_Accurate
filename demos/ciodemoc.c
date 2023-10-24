/*
    File ciodemoc.c: A sample C program to illustrate the use of some of
                     the routines to manage AIRES compressed output files.

    The example consists in evaluating a rough estimation of rho600
    for electrons (do not take this example as a serious calculation).

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

      cc ciodemoc.c -L/myhome/aires/lib -lAires

    will do the work in the default case. Since some of the library
    modules used are FORTRAN routines, in some systems it may be
    necessary to specify also additional FORTRAN libraries in the C
    compile instruction, as well to put some qualifiers to the original
    "cc" command. If you cannot compile this program using the previous
    command and do not know how to proceed, you should ask a UNIX expert
    to have the demo compiled for you. In some systems the necessary
    FORTRAN libraries can be included using the FORTRAN compile command
    to link the program, like in the following instructions:

      cc -c ciodemoc.c   # will produce object file
      f77   ciodemoc.o -L/myhome/aires/lib -lAires

    Some FORTRAN compilers automatically detect the C format in the file
    "ciodemoc.c" and invoke the C compiler to obtain the corresponding
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
    "ciodemoc_.c" that comes with the present AIRES distribution.
    ------------------------------------------------------------------
*/
#include <math.h>

    main()
    /*
       Reading binary files created with AIRES cio system, an example.
    */
    {
       char      wdir[80], filename[80];
       int       ciochann;
       int       i1, i2, i3, i4, i5, irc;

       int       dtyp, idxpcode, idxlogr, idxweight;
       double    logr, weight;

       int       altrec;
       int       indata[99];
       double    fldata[99];

       int       nciorec, nshowers, npart, ntrail;
       int       fileversion, libversion;
       double    rho600, rho600_sh, rho600_master;
       double    l550, l650, ringarea;

       void      opencrofilec();
       int       crofieldindexc();

       int       crofileversion(), thisairesversion();      /*__*/
       int       getcrorecord();                            /*__*/
       int       ciorinit(), inpsry(), ciorshutdown();      /*__*/


       /* Initializing the CIO system.

       Argument number 2 (i2) is the particle coding system label.
       This variable permits selecting among different coding systems:

         i2 = 0    AIRES internal coding system.
         i2 = 1    AIRES internal coding system with decimal nuclear
                   codes (code = A + 100 * Z).
         i2 = 4    Particle Data Group coding system (Phys. Rev. D 45
                   (1992) S1) (code = A + 100000 * Z for nuclei).
         i2 = 5    CORSIKA program particle coding system.
         i2 = 6    GEANT particle coding system (with GEANT codes
                   also for nuclei, dependent on Z only).
         i2 = 8    SIBYLL particle coding system (code = A + 100 * Z
                   for nuclei).
         i2 = 9    MOCCA-style particle coding system (1 photon,
                   +-2 e+-, +-3 mu+-, +-4 pi+-, 5 pi0, 6 n, 7 p,
                   -7 pbar, code = A + 100 * Z for nuclei).
         other     Any other value is equivalent to i2 = 1.

       Argument number 3 (i3) controls the "verbosity":

         i3 <= 0    No error/informative messages printed, error
                    conditions are communicated to the calling
                    program using the return code (irc).
         i3 = 1     Print messages even for successful operations.
         i3 = 2     Print only error messages and return.
         i3 = 3     Print only error messages and stop if error
                    is fatal.

       The return code (irc) is 0 for successful return, 1 when the
       "other" option was specified for "i2". */

       /* This calls sets AIRES particle coding system and no messages. */

       i1 = 0;      i2 = 1;     i3 = 0;

       ciorinit(&i1, &i2, &i3, &irc);      /*__*/

       do {

         printf("Enter working directory (. for current dir): ");
         scanf("%s", &wdir[0]);

         printf("Enter file name: ");
         scanf("%s", &filename[0]);

	 /* Opening the cio input file. It will be labelled in variable
         "ciochann" (Integer. This variable should not be set in the
         calling program).

         Argument number 3 (i3) switchs among different modes for
         processing the first part of the header. With the current
         AIRES version, use i3 = 0 for normal operation.

         Argument number 4 (i4) set the base to use for logarithms.
         Use i4 = 0 (10) for natural (decimal) logarithms.

         Argument number 5 (i5) sets the "verbosity" as explained before
         the call to "ciorinit".

         irc is the return code. 0 means successful return. 1 means
         successful return obtained with a file that was written with a
         previous AIRES version. 10 means that the file could be opened
         normally, but that it seems not to be a valid AIRES compressed
         data file, or is a corrupted file; 12 invalid file header; 14
         not enough size in some of the internal arrays; 16 format
         incompatibilities. 20: too many compressed files already opened.
         Any other value indicates an opening / header-reading error (irc
         equals the system return code plus 10000). */

         i3 = 0;     i4 = 10;     i5 = 4;

         opencrofilec(&wdir, &filename,  &i3, &i4, &i5, &ciochann, &irc);

       } while(irc > 1);

       /* Printing some information. */

       i1 = 0;
       inpsry(&i1);      /*__*/

       /* "croheaderinfo" prints a summary of the information contained in
          the file header (Task name, input parameters, etc.).
          "crofileinfo" prints a list of the different records defined for
          the file and the names of the fields within records.
          In both routines the "0" arguments indicate that the output
          channel is the standard output (FORTRAN unit 6). If you want to
          direct the output to a file, use channels 9 to 14 changing the
          "0" by the corresponding number.                */

       i1 = 0;
       i2 = 4;
       croheaderinfo_(&i1, &i2, &irc);                   /*__*/
       crofileinfo_(&ciochann, &i1, &i2, &irc);          /*__*/

       /* Of course, there are many more routines that can help ypu to
          retrieve information stored in the file header. The UsersManual
          contains documentation about them.
       */

       /* Do you need the library and/or compressed file versions?     */

       fileversion = crofileversion(ciochann);
       libversion  = thisairesversion();

       printf("\n Compressed file written with AIRES version %d\n",
              fileversion);
       printf("\n This program compiled with AIRES version   %d\n",
               libversion);

       /* Setting indices for different fields.

         The procedure to obtain the data stored in a record is simple:

         Routine "getcrorecord" the next available record each time it is
         invoked. The return code tells you what is the type of the
         scanned record. Most of the time this parameter will be 0
         indicating that a default record was read, but in some cases
         (for example beginning or end of a shower) the return code will
         be a small positive number indicating that an alternative record
         has been read in. If the return code is negative it means that
         an "end of file" condition was reached, and if it is greater
         than 10000 it means an i/o error.

         For each record, the data is stored in an integer array
         ("indata") array and a double precision floating point array
         ("fldata"). The meaning of each array element varies for each
         different record types and file formats. What remains fixed,
         however, is the set of field names. These names are used to
         set indices like the following ones, to be used in the next
         section to address the actual data items:        */

       i1 = 0;
       i2 = 4;
       idxpcode = crofieldindexc(&ciochann, &i1, "Particle code",
                                 &i2, &dtyp, &irc);

       /* In the previous call we set an integer index that will be useful
         to obtain the particle code corresponding to each default record
         (Argument number 2, "0", refers to the default record).
         It is not necessary to give the complete name of the field:
         Routine crofieldindex interprets this string as a substring of
         the complete name (starting from the rightmost character) and
         looks for matching fields. It is an error if none or more than
         one field match the input specification. Notice that the
         parameter before dtyp, the "4", is the verbosity. Setting it to
         4 ensures that processing will be stopped if the index is not
         properly set.

         The output parameter dtyp gives the data type of the field (1 for
         integer data, 2 for date-time data and 3 for double precision
         data).

         The user must set the needed indices, the following assignments
         are only a fraction of the available data items, and correspond
         to the ground particles file. A complete listing for each file is
         returned from "crofileinfo".                 */

       idxlogr   = crofieldindexc(&ciochann, &i1, "Distance from the core",
                                  &i2, &dtyp, &irc);
       idxweight = crofieldindexc(&ciochann, &i1, "Particle we",
                                  &i2, &dtyp, &irc);

       /*  Processing. */

       printf("\n Processing ...\n");

       rho600_master = 0;
       l550          = log10(550.0);
       l650          = log10(650.0);

       nciorec  = 0;
       nshowers = 0;
       npart    = 0;
       ntrail   = 0;
 
       /* Here i5 is the "verbosity" control (see below). */

       i5 = 0;

       /*  If getcrorecord is false, that means error or end of file. */

       while(getcrorecord(&ciochann, &indata[1], &fldata[1],
                          &altrec, &i5, &irc))                /*__*/
       {

         nciorec++;

         /* If altrec is 1 ("true") that means "non default record"
            (Here: beginning and ending of shower.) */

         if (altrec) {
           /* Alternative record. */

           if (irc == 1) {

             /* Beginning of shower. */

             /* Integer fields:

                  indata[1] ---- Primary code. Coding system is as specified
                                 in the call to the initializing routine.
                         2  ---- Shower number.
                       3-8  ---- Starting date (year, month,...,min,sec).

                Real fields:

                  fldata[1] ---- log(Eprim) (log base as set in the open call)
                         2  ---- Zenith angle (deg)
                         3  ---- Azimuth angle (deg)
                         4  ---- Thinning energy.
                         5  ---- Depth of first interaction (X1).
                         6  ---- Global time shift (t0), in sec.

             NOTE:  Energies in GeV.  */

             /*------------------------------------------------------*/
             /* "BEGINNING OF SHOWER" PROCESSING CODE.               */

             rho600_sh = 0;

	     /*------------------------------------------------------*/
           }
           else if (irc == 2) {

             /* End of shower record. */

             ntrail++;
             nshowers++;

             /* Integer fields

                  indata[1] ---- Shower number.
                         2  ---- Return code of the Xmax fit.
                       3-8  ---- Ending date (year, month,...,min,sec).

                Real fields

                  fldata[1] ---- total particles
                         2  ---- lost particles
                         3  ---- low-e particles
                         4  ---- ground particles
                         5  ---- unphysical particles
                         6  ---- number of neutrinos
                         7  ---- no. of particles too near the core
                                 (were not stored in file).
                         8  ---- Particles in the resampling region.
                         9  ---- idem too far.
                        10  ---- Xmax (g/cm2)
                        11  ---- Nmax (all charged)
                        12  ---- energy of lost particles
                        13  ---- energy of low-e particles
                        14  ---- energy of ground particles
                        15  ---- energy of unphysical particles
                        16  ---- energy of neutrinos
                        17  ---- energy lost in the air
                        18  ---- energy of particles too near the core
                                 (were not stored in file).
                        19  ---- Idem too far.
                        20  ---- CPU time used (sec).

             NOTE:  Energies in GeV.  */

             /*------------------------------------------------*/
             /* "END OF SHOWER" PROCESSING CODE.               */

             rho600_master += rho600_sh;

             /*------------------------------------------------*/

	   }
	 }
         else {

           /* Processing a particle record (default record).

           Fields:

           Integer fields:

             indata[1] ---- Particle code. Coding system is as specified
                            in the call to the initializing routine.

           Real fields:

             fldata[1] ---- log(energy) (log base as set in the open call)
                    2  ---- log(R)      (log base as set in the open call)
                    3  ---- theta
                    4  ---- Ux (direction of motion)
                    5  ---- Uy
                    6  ---- time
                    7  ---- weight

           NOTE:  Lengths in m, energies in GeV, times in ns.
           OTHER: Uz = - sqrt(1 - Ux ** 2 - Uy ** 2).  */

           npart++;

           /*------------------------------------------------*/
           /* PARTICLE PROCESSING CODE.                      */

           /* Selecting electrons (AIRES code -2) and positrons
              (AIRES code 2) */

           if (abs(indata[1]) == 2) {

             /* Selecting log10(R) in [log10(550), log10(650)] */

             if ((fldata[2] >= l550) & (fldata[2] <= l650))
               rho600_sh += fldata[7];
           }

           /*------------------------------------------------*/

	 }

       } /* End of while(getcrorecord....)  */

       /* I/O Error or EOF reached. */

       if (irc >= 0) {
         printf("Error processing cio file.\n");
         printf("Return code = %d\n", irc);
       }

       printf("\n Closing cio file.\n");

       /* This call is to go away in an ordered fashion. */

       ciorshutdown;      /*__*/

       printf("\n");
       printf(" Number of blocks read      : %d\n", nciorec);
       printf(" Number of showers          : %d\n", nshowers);
       printf(" Number of particles        : %d\n", npart);
       printf(" Number of trail records    : %d\n", ntrail);

       /* Evaluating rho600. */

       if (nshowers > 0) {

         ringarea = 3.1415926 * (650. * 650. - 550. * 550.);
         rho600   = rho600_master / (nshowers * ringarea);

         printf("\n Estimated <rho600> = %.10g pcles/m2\n\n", rho600);
       }
    }
