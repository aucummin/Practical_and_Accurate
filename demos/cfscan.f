c
c     File cfscan.f: A sample FORTRAN program to illustrate the use of
c                    some of the routines to manage AIRES compressed
c                    output files.
c
c     This program scans a compressed file and counts the showers saved
c     within it, printing also some additional information.
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
c       f77 ciodemo.f -L/myhome/aires/lib -lAires
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
      program cfscan
c
c     Reading binary files created with AIRES cio system, an example.
c
      implicit none
c
      character*120     wdir, filename
      integer           ciochann
      integer           wdlen, fnlen, i2, i3, irc, jrc
c
      integer           dtyp, idxshno, idxbdate, idxshn2, idxedate
      integer           idxk, idxpcode, idxpegy, idxpzen, idxpazim
      integer           showerno, showern2, prcode
      double precision  pegy
c
      logical           fstbeg, get1, altrec, grdfile
      integer           indata(30)
      double precision  fldata(30)
c
      integer           ncfiles, npart, nbeg, ntrail, nbetween
      integer           shbegr, shendr, pbegr
      double precision  shiftoev
c
      character*20      datistring, primname
      character*80      taskname
      integer           tnlen, taskversion, pnlen
c
      integer           crofieldindex, crorecnumber
      logical           regetcrorecord, crorecfind, crospcode
      integer           crofileversion
c
c
c     Initializing the CIO system.
c
c     Argument number 2 (i2) is the particle coding system label.
c     This variable permits selecting among different coding systems:
c
c         i2 = 0    AIRES internal coding system.
c         i2 = 1    AIRES internal coding system with decimal nuclear
c                   codes (code = A + 100 * Z).
c         i2 = 4    Particle Data Group coding system (Phys. Rev. D 45
c                   (1992) S1) (code = A + 100000 * Z for nuclei).
c         i2 = 5    CORSIKA program particle coding system.
c         i2 = 6    GEANT particle coding system (with GEANT codes
c                   also for nuclei, dependent on Z only).
c         i2 = 8    SIBYLL particle coding system (code = A + 100 * Z
c                   for nuclei).
c         i2 = 9    MOCCA-style particle coding system (1 photon,
c                   +-2 e+-, +-3 mu+-, +-4 pi+-, 5 pi0, 6 n, 7 p,
c                   -7 pbar, code = A + 100 * Z for nuclei).
c         other     Any other value is equivalent to i2 = 1.
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
c     The return code (irc) is 0 for successful return, 1 when the
c     "other" option was specified for "i2".
c
c     This calls sets AIRES particle coding system and no messages.
c
      i2 = 0
      i3 = 0
      call ciorinit(0, i2, i3, irc)
c
c
      print *, 'Enter working directory (blank for current dir):'
 1010 continue
      read(5, 2010, end = 1040) wdir
 2010 format(a)
      call strim(-1, wdir, wdlen)
      if (wdir(1:1) .eq. '#') goto 1010
      wdlen = max(1, wdlen)
c
      ncfiles = 0
c
 1015 continue
      print *, 'Enter file name'
 1016 continue
      read(5, 2010, end = 1040) filename
      call strim(-1, filename, fnlen)
      if (filename .eq. ' ') goto 1016
      if (filename(1:1) .eq. '#') goto 1016
      if (filename(1:fnlen) .eq. '&no_more_files') goto 1040
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
c                                                        i3  i4  i5
      call opencrofile(wdir(1:wdlen), filename(1:fnlen),  0,  0,  4,
     +                 ciochann, irc)
      if (irc .gt. 1) goto 1010
c
c     Setting indices for different fields.
c
      idxshno  = crofieldindex(ciochann, 1, 'Shower number',
     +                         4, dtyp, irc)
      idxbdate = crofieldindex(ciochann, 1, 'Starting date',
     +                         4, dtyp, irc)
      idxpcode = crofieldindex(ciochann, 1, 'Primary particle',
     +                         4, dtyp, irc)
      idxpegy  = crofieldindex(ciochann, 1, 'Primary energy',
     +                         4, dtyp, irc)
      idxpzen  = crofieldindex(ciochann, 1, 'Primary zenith',
     +                         4, dtyp, irc)
      idxpazim = crofieldindex(ciochann, 1, 'Primary azimuth',
     +                         4, dtyp, irc)
c
      idxshn2  = crofieldindex(ciochann, 2, 'Shower number',
     +                         4, dtyp, irc)
      idxedate = crofieldindex(ciochann, 2, 'Ending date',
     +                         4, dtyp, irc)
c
c     Using crofieldindex to determine if the file is a ground particle
c     file or a longitudinal tracking file.
c
      idxk    = crofieldindex(ciochann, 0, 'Observing levels crossed',
     +                        0, dtyp, irc)
      grdfile = (irc .ne. 0)
c
c     Printing some information.
c
c
      call crotaskid(taskname, tnlen, taskversion, datistring)
c
      print *, ' '
      print *, 'File           ', filename(1:fnlen)
      if (taskversion .le. 0) then
        print *, 'Task           ', taskname(1:tnlen)
      else
        print *, 'Task           ', taskname(1:tnlen),
     +           ' (V', taskversion, ')'
      endif
      print *, 'Starting date  ', datistring
c
      if (grdfile) then
        print *,
     +   'Compressed ground particle file written with AIRES version',
     +   crofileversion(ciochann)
      else
        print *,
     +   'Compressed longitudinal file written with AIRES version',
     +   crofileversion(ciochann)
      endif
c
c     Processing
c
      print *, ' '
      print *, 'Scanning the file...'
      print *, ' '
c
      shiftoev = log(1.d9)
c
      ncfiles  = ncfiles + 1
      npart    = 0
      nbeg     = 0
      ntrail   = 0
      fstbeg   = .true.
      shendr   = -99
c
 1020 continue
c
      get1 = crorecfind(ciochann, 1, 0, indata, irc)
      if (.not. get1) goto 1030
      if (irc .ne. 1) goto 1030
c
c     "Beginning" of shower found.
c
      nbeg   = nbeg + 1
      shbegr = crorecnumber(ciochann, 0, irc)
c
      if (fstbeg) then
        if (shbegr .ne. 1) then
          print *, ' '
          print *,
     +    '!  WARNING: File does not begin with a ',
     +    '"Beginning of shower" record.'
          print *,
     +    '            Number of records before first BOS:',
     +    shbegr - 1
          print *, ' '
        endif
        fstbeg = .false.
      else
        nbetween = shbegr - shendr - 1
        if (nbetween .ne. 0) then
          print *, ' '
          print *,
     +    '!  WARNING: "Beginning of Shower" record does not follow',
     +    'after last "End of shower".'
          print *,
     +    '            Number of records between current BOS and ',
     +    'last EOS:', nbetween
          print *, ' '
        endif
      endif
c
      get1 = regetcrorecord(ciochann, indata, fldata, altrec, 0, irc)
c
      showerno = indata(idxshno)
      prcode   = indata(idxpcode)
      pegy     = exp(fldata(idxpegy) + shiftoev)
c
      call pnice(0, prcode, primname, pnlen)
      call datifmt(indata(idxbdate), datistring)
c
      print *, '=====> Beginning of shower', nbeg,
     +         '                    <====='
      print *, '=  Shower number', showerno,
     +         ' started ', datistring
      print *, '=  Primary ', primname(1:pnlen),
     +         '; energy', pegy, ' eV'
      print *, '=  Zenith and azimuth angles:',
     +         fldata(idxpzen), ' deg;', fldata(idxpazim), ' deg.'
c
      if (crospcode(prcode, irc)) then
        get1  = crorecfind(ciochann, 4, 0, indata, irc)
        if (.not. get1) goto 1030
        if (irc .ne. 4) goto 1030
        pbegr = crorecnumber(ciochann, 0, irc)
      else
        pbegr = shbegr
      endif
c
      get1 = crorecfind(ciochann, 2, 0, indata, irc)
c
      if ((.not. get1) .or. (irc .ne. 2)) then
        print *, ' '
        print *,
     +  '!  WARNING: No "End of Shower" record found for shower', nbeg
        print *,
     +  '            Last record read in:',
     +  crorecnumber(ciochann, 0, jrc)
        goto 1030
      endif
c
c     "End" of shower found.
c
      ntrail = ntrail + 1
      shendr = crorecnumber(ciochann, 0, irc)
      npart  = npart + shendr - pbegr - 1
c
      get1 = regetcrorecord(ciochann, indata, fldata, altrec, 0, irc)
c
      showern2 = indata(idxshn2)
c
      call datifmt(indata(idxedate), datistring)
c
      print *, '=  Shower starts at record', shbegr,
     +         ' and ends at record', shendr
c
      if (pbegr .ne. shbegr) then
        print *, '=  Special primary data from record', shbegr + 1,
     +           ' to record', pbegr
      endif
c
      print *, '=  Shower number', showern2,
     +         '  ended ', datistring
c
      if (showerno .ne. showern2) then
        print *, ' '
        print *,
     +  '!  WARNING: Shower number recorder at beginning of shower'
        print *,
     +  '            is not equal to the corresponding one at the end!'
        print *, ' '
      endif
c
      print *, '------ End of shower', nbeg
c
      goto 1020
c
 1030 continue
c
c     I/O Error or EOF reached.
c
      if (irc .ge. 0) then
        print *, 'Error processing cio file.'
        print *, 'Return code =', irc
      endif
c
      call cioclose1(ciochann)
c
      if (ntrail .ne. nbeg) then
        print *, ' '
        print *, '!  WARNING: Number of "beginning of shower" and'
        print *, '            "end of shower" records do not match.'
        print *, '            Probably last shower is incomplete'
        print *, ' '
      endif     
c
      print *, ' '
      print *, ' Beginning of shower records     :', nbeg
      print *, ' End of shower records           :', ntrail
      print *, ' Number of particle records      :', npart
      print *, ' '
c
      goto 1015
 1040 continue
c
c     This call is to go away in an ordered fashion.
c
      call ciorshutdown
c
      print *, ' '
      if (ncfiles .le. 0) then
        print *, 'No files processed.'
        print *, ' '
      else if (ncfiles .gt. 1) then
        print *, ncfiles, ' files scanned.'
        print *, ' '
      endif
c
      end
c
