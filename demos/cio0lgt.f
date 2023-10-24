c
c     File cio0lgt.f: A sample FORTRAN program to illustrate the use of
c                     some of the routines to manage AIRES compressed
c                     output files, in the special case of longitudinal
c                     particle tracking files (extension lgtpcles).
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
c       f77 cio0.f -L/myhome/aires/lib -lAires
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
      program cio0lgt
c
c     Reading binary files created with AIRES cio system, an example.
c
      implicit none
c
      character*80      wdir, filename
      integer           ciochann
      integer*4         i2, i3, irc
c
      integer           dtyp, idxpcode, idxlogenergy, idxweight
      integer           idxx, idxy
      integer           idxshwnumber, idxstartdate, idx1stdepth
      integer           idxshwnumbe2, idxenddate, idxxmax
c
      logical           get1, altrec
      integer           currol, updown
      integer           indata(99)
      double precision  fldata(99)
c
      integer           nrecentry, nshowers, npart, nbeg
      integer           libversion, fileversion
c
      integer           crofieldindex
      logical           getlgtrecord
      integer           crofileversion, thisairesversion
c
c
c     Initializing the CIO system.
c
c     Argument number 2 (i2) is the particle coding system label.
c     This variable permits selecting among different coding systems
c     (see cio0.f). When using the special routines for processing
c     lgtpcles compressed files, i2 should be set to 0
c
      i2 = 0
c
c     Argument number 3 (i3) controls the "verbosity":
c
c         i3 .le. 0  No error/informative messages printed, error
c                    conditions are communicated to the calling
c                    program using the return code (irc).
c         i3 = 1     Print messages even for successful operations.
c         i3 = 2     Print only error messages and return.
c         i3 = 3     Print only error messages and stop if error
c                    is fatal.
c
      i3 = 0
c
c     The return code (irc) is 0 for successful return, 1 when the
c     "other" option was specified for "i2".
c
      call ciorinit(0, i2, i3, irc)
c
c     Getting input file name.
c
 1010 continue
      print *, 'Enter working directory (blank for current dir):'
      read 2010, wdir
 2010 format(a)
      print *, 'Enter file name'
      read 2010, filename
c
c     opening the cio input file. It will be labelled in variable
c     "ciochann" (Integer. This variable should not be set in the
c     calling program).
c
c     Argument number 3 (i3) switchs among different modes for
c     processing the first part of the header. With the current
c     AIRES version, use i3 = 0 for normal operation.
c
c     Argument number 4 (i4) set the base to use for logarithms.
c     Use i4 = 0 (10) for natural (decimal) logarithms.
c
c     Argument number 5 (i5) sets the "verbosity" as explained before
c     the call to "ciorinit".
c
c     irc is the return code. 0 means successful return. 1 means
c     successful return obtained with a file that was written with a
c     previous AIRES version. 10 means that the file could be opened
c     normally, but that it seems not to be a valid AIRES compressed
c     data file, or is a corrupted file; 12 invalid file header; 14
c     not enough size in some of the internal arrays; 16 format
c     incompatibilities. 20: too many compressed files already opened.
c     Any other value indicates an opening / header-reading error (irc
c     equals the system return code plus 10000).
c
c                                      i3  i4  i5
      call opencrofile(wdir, filename,  0, 10,  4, ciochann, irc)
      if (irc .gt. 1) goto 1010
c
c     Printing some information.
c
c     "croheaderinfo" prints a summary of the information contained in
c     the file header (Task name, input parameters, etc.).
c     "crofileinfo" prints a list of the different records defined for
c     the file and the names of the fields within records.
c     In both routines the "0" arguments indicate that the output
c     channel is the standard output (FORTRAN unit 6). If you want to
c     direct the output to a file, use channels 9 to 14 changing the
c     "0" by the corresponding number.
c
      call croheaderinfo(0, 4, irc)
      call crofileinfo(ciochann, 0, 4, irc)
c
c     Getting library and compressed file versions:
c
      fileversion = crofileversion(ciochann)
      libversion  = thisairesversion()
c
      print *, ' '
      print *, 'Compressed file written with AIRES version',
     +         fileversion
      print *, 'This program compiled with AIRES version  ',
     +         libversion
      print *, ' '
c
c     Setting indices for different fields.
c
c     The procedure to obtain the data stored in a record is simple:
c
c     Routine "getlgtrecord" returns the next available record each
c     time it is invoked. The return code tells you what is the type of
c     the corresponding record. Most of the time this parameter will be
c     0 indicating that a default record was read, but in some cases
c     (for example beginning or end of a shower) the return code will
c     be a small positive number indicating that an alternative record
c     has been read in. If the return code is negative it means that
c     an "end of file" condition was reached, and if it is greater
c     than 10000 it means an i/o error.
c
c     For each record, the data is stored in an integer array
c     ("indata") array and a double precision floating point array
c     ("fldata"). The meaning of each array element varies for each
c     different record types and file formats. What remains fixed,
c     however, is the set of field names. These names are used to
c     set indices like the following ones, to be used in the next
c     section to address the actual data items:
c
      idxpcode = crofieldindex(ciochann, 0, 'Particle code',
     +                         4, dtyp, irc)
c
c     In the previous call we set an integer index that will be useful
c     to obtain the particle code corresponding to each default record
c     (Argument number 2, "0", refers to the default record).
c     It is not necessary to give the complete name of the field:
c     Routine crofieldindex interprets this string as a substring of
c     the complete name (starting from the rightmost character) and
c     looks for matching fields. It is an error if none or more than
c     one field match the input specification. Notice that the
c     parameter before dtyp, the "4", is the verbosity. Setting it to
c     4 ensures that processing will be stopped if the index is not
c     properly set.
c
c     The output parameter dtyp gives the data type of the field (1 for
c     integer data, 2 for date-time data and 3 for double precision
c     data).
c
c     The user must set the needed indices, the following assignments
c     are only a fraction of the available data items, and correspond
c     to the ground particles file. A complete listing for each file is
c     returned from "crofileinfo".
c
      idxlogenergy = crofieldindex(ciochann, 0, 'Energy',
     +                             4, dtyp, irc)
      idxweight    = crofieldindex(ciochann, 0, 'Particle we',
     +                             4, dtyp, irc)
      idxx         = crofieldindex(ciochann, 0, 'X coordinate',
     +                             4, dtyp, irc)
      idxy         = crofieldindex(ciochann, 0, 'Y coordinate',
     +                             4, dtyp, irc)
c
c     Some indices for the "Beginning of shower" record:
c
      idxshwnumber = crofieldindex(ciochann, 1, 'Shower number',
     +                             4, dtyp, irc)
      idxstartdate = crofieldindex(ciochann, 1, 'Starting date',
     +                             4, dtyp, irc)
      idx1stdepth  = crofieldindex(ciochann, 1, 'First int',
     +                             4, dtyp, irc)
c
c     Some indices for the "End of shower" record:
c
      idxshwnumbe2 = crofieldindex(ciochann, 2, 'Shower number',
     +                             4, dtyp, irc)
      idxenddate   = crofieldindex(ciochann, 2, 'Ending date',
     +                             4, dtyp, irc)
      idxxmax      = crofieldindex(ciochann, 2, 'Shower maximum depth',
     +                             4, dtyp, irc)
c
c     Initializing the system for processing particle records of the
c     lgtfiles.
c
c     The arguments are: i/o channel identification, verbosity (same as
c     already explained), and return code (0 = normal return)

      call getlgtinit(ciochann, 4, irc)
c
c     Processing the data records.
c
      print *, ' '
      print *, 'Processing ...'
c
      nrecentry = 0
      nshowers  = 0
      npart     = 0
      nbeg      = 0
 1020 continue
c
c     Here "0" is the "verbosity" control (see below).
c
      get1 = getlgtrecord(ciochann, currol, updown,
     +                    indata, fldata, altrec, 0, irc)
c
c     If get1 is false, that means error or end of file.
c
      if (.not. get1) goto 1030
      nrecentry = nrecentry + 1
c
c     If altrec is true that means "non default record"
c     (Here: beginning and ending of shower.)
c
      if (altrec) goto 1025
c
c     The routine returned data corresponding to a particle crossing
c     one of the selected observing levels.
c
c     THERE ARE SOME DIFFERENCES WITH THE BASIC ROUTINE "crogetrecord"
c     (compare with the program within cio0.f). The differences apply
c     only to the case of particle records (return code 0).
c
c     1) There are two additional output arguments:
c        currol (integer): The observing level crossed.
c        updown (integer): +1 (-1) if the particle goes upwards
c                          (downwards).
c        These two variables are undefined when the return code is not
c        zero.
c
c     2) The other arguments are exactly as in the basic example
c        cio0.f, but, in the case of particle records, the fields
c        corresponding to position and arrival time are corrected to
c        match exactly with what corresponds to the observing
c        level crossed (variable currol).
c
c     The AIRES library also contaoins a similar routine,
c     "crofltrecord0" (with the same arguments as "crofltrecord"),
c     that returns all records as saved in the compressed file, without
c     applying the corrections mentioned in (2).
c
c     Examples of available fields:
c
c     Integer fields:
c
c         indata(idxpcode) ---- Particle code. Coding system is as
c                               specified in the call to the
c                               initializing routine.
c
c     Real fields:
c
c         fldata(idxlogenergy) ---- log(energy) (log base as set in the
c                                   open call).
c         fldata(idxweight)    ---- weight.
c         fldata(idxx)         ---- X coordinate.
c         fldata(idxy)         ---- Y coordinate.
c
c         NOTE:  Lengths in m, energies in GeV, times in ns.
c
      npart = npart + 1
c
c     -----------------------------------------
c     PLACE HERE YOUR PARTICLE PROCESSING CODE.
c


c
c     -----------------------------------------
c
      goto 1020
 1025 continue
c
c     Alternative record.
c
      if (irc .eq. 1) then
c
c       Beginning of shower.
c
        nbeg = nbeg + 1
c
c       Examples of available fields:
c
c       Integer fields:
c
c         indata(idxshnumber) ---- Shower number.
c         indata(idxstartdate),..., indata(idxstartdate + 5)
c                             ---- Starting date (year, month,...,min,
c                                  sec).
c
c       Real fields:
c
c         fldata(idx1stdepth) ---- Depth of first interaction (X1).
c
c         NOTE:  Energies in GeV.
c
c       ------------------------------------------------------
c       PLACE HERE YOUR "BEGINNING OF SHOWER" PROCESSING CODE.
c


c
c       ------------------------------------------------------
c
      else if (irc .eq. 2) then
c
c       End of shower record.
c
        nshowers = nshowers + 1
c
c       Examples of available fields:
c
c       Integer fields
c
c         indata(idxshnumbe2) ---- Shower number.
c         indata(idxenddate),..., indata(idxenddate + 5)
c                             ---- Ending date (year, month,...,min,
c                                  sec).
c
c       Real fields
c
c         fldata(idxxmax)     ---- Xmax (g/cm2)
c
c       ------------------------------------------------
c       PLACE HERE YOUR "END OF SHOWER" PROCESSING CODE.
c

c
c       ------------------------------------------------
c
      endif
c
      goto 1020
 1030 continue
c
c     I/O Error or EOF reached.
c
      if (irc .ge. 0) then
        print *, 'Error processing cio file.'
        print *, 'Return code =', irc
      endif
c
      print *, ' '
      print *, 'Closing cio file.'
c
c     This call is to go away in an ordered fashion.
c
      call ciorshutdown
c
      print *, ' '
      print *, ' Number of processed entries :', nrecentry
      print *, ' Number of head records      :', nbeg
      print *, ' Number of showers           :', nshowers
      print *, ' Number of particle entries  :', npart
c
      print *, ' '
c
      end
c
