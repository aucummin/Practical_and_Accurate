c
c     File cfreccount.f: A sample FORTRAN program to illustrate the use
c                        of some of the routines to manage AIRES
c                        compressed output files.
c
c     This program scans a compressed file and counts its records.
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
      program cfreccount
c
c     Reading binary files created with AIRES cio system, an example.
c
      implicit none
c
      character*120     wdir, filename
      integer           ciochann
      integer           wdlen, fnlen, i2, i3, irc, i
c
      integer           idxk, dtyp
      logical           grdfile
c
      integer           ncfiles, nrtype, totrec
      integer           trecno(0:11)
c
      character*20      datistring
      character*80      taskname
      integer           tnlen, taskversion
c
      integer           crofieldindex
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
      ncfiles = ncfiles + 1
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
c     Counting the number of records.
c
      call croreccount(ciochann, 0, nrtype, trecno, irc)
      if (irc .ne. 0) then
        print *, 'Nonzero return code from croreccount:', irc
        goto 1030
      endif
c
      print *, ' '
      print *, 'Record type     N. of records'
      print *, ' '
      totrec = 0
      do i = 0, nrtype
        print 2040, i, trecno(i)
        totrec = totrec + trecno(i)
      enddo
 2040 format(i7, i21)
      print 2050, 'All', totrec
 2050 format(a7, i21)
      print *, ' '
c
 1030 continue
c
c     I/O Error or EOF reached.
c
      if (irc .gt. 0) then
        print *, 'Error processing cio file.'
        print *, 'Return code =', irc
      endif
c
      call cioclose1(ciochann)
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
