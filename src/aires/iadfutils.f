c
c     FILE: iadfutils.f                     Creation date: 26/OCT/2000.
c                                       LAST MODIFICATION: 04/NOV/2004.
c
c     This file contains routines to manage data stored within dump
c     files (adf or idf) from analysis environments.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine loadumpfile(wdir, itaskname, raw, vrb, irc)
c
c     Reading the dump file associated with a given task.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2003.
c
c
c     Arguments:
c     =========
c
c     wdir............ (input, character*(*)) The name of the directory
c                      where the file is placed. It defaults to the
c                      current directory when blank.
c     itaskname....... (input, character*(*)) Task name, or dump file
c                      name.
c     raw............. (input, integer) Integer switch to determine
c                      whether (raw ne 0) or not (raw eq 0) final
c                      statistical calculations are applied to output
c                      observables.
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
c                      successful return. 1 means successful return,
c                      but the dump file was not created using the
c                      same AIRES version. 8 means that no dump file
c                      (in the sequence taskname, taskname.adf,
c                      taskname.idf) exists. 12 means invalid file
c                      name. Other return codes
c                      come from the adf or idf read routines.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
      include 'cio2par.f'
      include 'versionpar.f'
c
c     Declaration of arguments.
c
      character*(*)     wdir, itaskname
      integer           raw, vrb, irc
c
c     Declaration of shared data.
c
      include 'maincomm.f'
      include 'initcomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           fnlen, fnlene
      logical           exists
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting base file name.
c
      call strimcopy(wdir, len(wdir), wdirname, wdirnamelen)
      call strimcopy(itaskname, len(itaskname), taskname, tasknamelen)
      call absfnset(wdirname, wdirnamelen, taskname, tasknamelen,
     +              auxfilestring, fnlen)
c
      if (fnlen .le. 0) then
        irc = 12
        if (vrb .gt. 0) then
          call errprint(0, '*', max(3, vrb), 'loadumpfile',
     +         'Missing or invalid task or file name.',
     +         0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
      adfile = .true.
c
c     Checking for the existence of the input file.
c
      inquire (file = auxfilestring, exist = exists)
c
      if (exists) then
c
c       The input name, as given, points to an existing file.
c
        fnlene = fnlen
c
c       Trying ADF.
c
        call adfread(auxfilestring(1:fnlene), 0, irc)
c
        if (irc .ne. 0) then
c
c         Trying IDF.
c
          adfile = .false.
          call idfread(auxfilestring(1:fnlene), 0, vrb, irc)
c
        endif
c
      else
c
c       Trying the names appending the corresponding suffixes.
c
        auxfilestring(fnlen+1:184) = adfext
        fnlene = fnlen + 4
c
        inquire (file = auxfilestring, exist = exists)
c
        if (exists) then
          call adfread(auxfilestring(1:fnlene), vrb, irc)
        else
          auxfilestring(fnlen+1:184) = idfext
          adfile = .false.
          call idfread(auxfilestring(1:fnlene), 0, vrb, irc)
        endif
c
      endif
c
      if (irc .eq. 0) then
c
c       The dump file was successfully opened and scanned.
c
        if (vrb .eq. 1) then
          call errprint(0, '$I01', 1, 'loadumpfile',
     +         'Dump file successfully read in.',
     +         0, 0, 0, 0.d0, auxfilestring(1:fnlene))
        endif
c
        if (original_version .ne. aires_version) irc = 1
c
c       Labelling the head data as "coming from dump file"
c       (file number 0)
c
        lastheadfile2 = 0
c
c       Additional initializations for idf files.
c
        if (.not. adfile) call setdynfromidf
c
c       Calling the atmospheric model initializing routine.
c
        call atmosinit(max(1, atmoslabel), atmosmodel)
c
c       Other initializations.
c
        call init4s
        call iniprtexpopts
c
c       Statistical analysis of observable data.
c
        if ((raw .ne. 0) .and. (pgmcode .ge. 2000)
     +      .and. (lastshower .gt. 0)) then
          call table2(lastshower)
          call momstxobs(lastshower)
        endif
c
      else
c
c       There was some problem reading the dump files.
c
        lastheadfile2 = -1
c
      endif
c
      return
      end
c     --- End of routine loadumpfile.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine dumpinputdata0(intdata, realdata)
c
c     Copying into arrays global input data parameters stored in an
c     already read dump file, and that are not returned by
c     croinputdata0.
c
c     Written by: S. J. Sciutto, La Plata 2000; Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     intdata......... (output, integer, array(*)) Integer data array.
c                      The calling program must provide enough space
c                      for it. The following list describes the
c                      different data items:
c                      ( 1) Total number of showers
c                      ( 2) Number of completed showers.
c                      ( 3) First shower number.
c                      ( 4) Number of merged data files.
c                      ( 5-9) Reserved for future use.
c                      (10) Separate Showers integer parameter.
c     realdata........ (output, double precision, array(*)) Real data
c                      array. The calling program must provide enough
c                      space for it. The following list describes the
c                      different data items:
c                      ( 1- ) Reserved for future use.
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
      integer           intdata(99)
      double precision  realdata(99)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Assigning integer data.
c
      intdata( 1)  = mtotshowers
      intdata( 2)  = lastshower
      intdata( 3)  = firstshowernon1 + 1
      intdata( 4)  = adfnmergedfiles
c
      intdata(10)  = ciosplitevery
c
c     Assigning real data.
c     (No real data returned currently).
c
      return
      end
c     --- End of routine dumpinputdata0.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function dumpfileversion()
c
c     Returning the AIRES version associated with an already loaded
c     dump file, that is, the version of the AIRES program that
c     created the file.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
c     Return value: (integer) The corresponding version in integer
c     ============  format (for example 01040200 for version 1.4.2,
c                   01040201 for version 1.4.2a, etc.).
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           dumpfileversion
c
c     Declaration of shared data.
c
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           iiver, irc
c
c     FIRST EXECUTABLE STATEMENT
c
      call versioncheck(idfversion(1:idfverlen), iiver, irc)
      dumpfileversion = iiver
c
      return
      end
c     --- End of routine dumpfileversion.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function dumpfileversiono()
c
c     Returning the original version associated with an already loaded
c     dump file, that is, the version of the AIRES program used to
c     simulate the data stored in the file.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2003.
c
c
c     Return value: (integer) The corresponding version in integer
c     ============  format (for example 01040200 for version 1.4.2,
c                   01040201 for version 1.4.2a, etc.).
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
      integer           dumpfileversiono
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ovl, iiver, irc
c
c     FIRST EXECUTABLE STATEMENT
c
      call strim(24, original_version, ovl)
      call versioncheck(original_version(1:ovl), iiver, irc)
      dumpfileversiono = iiver
c
      return
      end
c     --- End of routine dumpfileversiono.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getfirstintdata(vsl, fintdata, nshowers)
c
c     Returning first interaction data.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     vsl............. (input, integer) Integer switch to select
c                      between vertical (vsl = 1) and slant (vsl = 2)
c                      depths. Any other value of vsl is equivalent to
c                      vsl = 1.
c     fintadata....... (output, double precision, array(5)) Array
c                      containing information about the first
c                      interaction:
c                         fintdata(1) --- Average X1.
c                         fintdata(2) --- RMS error of the mean.
c                         fintdata(3) --- Standard deviation.
c                         fintdata(4) --- Minimum X1.
c                         fintdata(5) --- Maximum X1.
c     nshowers........ (output, integer) Number of showers in the data
c                      set. If nshowers is less or equal than 0, then
c                      the array fintdata is undefined.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           vsl
      double precision  fintdata(5)
      integer           nshowers
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
      if (vsl .eq. 2) then
        i = 2
      else
        i = 1
      endif
c
      do j = 1, 5
        fintdata(j) = fstintdp(j, i)
      enddo
c
      nshowers = lastshower
c
      return
      end
c     --- End of routine getfirstintdata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getxmaxdata(vsl, xmaxdata, nmaxdata, nshowers)
c
c     Returning shower maximum and related data.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     vsl............. (input, integer) Integer switch to select
c                      between vertical (vsl = 1) and slant (vsl = 2)
c                      depths. Any other value of vsl is equivalent to
c                      vsl = 1.
c     xmaxdata........ (output, double precision, array(5)) Array
c                      containing information about the depth of shower
c                      maximum:
c                         xmaxdata(1) --- Average Xmax.
c                         xmaxdata(2) --- RMS error of the mean.
c                         xmaxdata(3) --- Standard deviation.
c                         xmaxdata(4) --- Minimum Xmax.
c                         xmaxdata(5) --- Maximum Xmax.
c     nmaxdata........ (output, double precision, array(5)) Array
c                      containing information about the number of
c                      charged particles at maximum:
c                         nmaxdata(1) --- Average Xmax.
c                         nmaxdata(2) --- RMS error of the mean.
c                         nmaxdata(3) --- Standard deviation.
c                         nmaxdata(4) --- Minimum Xmax.
c                         nmaxdata(5) --- Maximum Xmax.
c     nshowers........ (output, integer) Number of showers in the data
c                      set (Showers with converged fits). If nshowers
c                      is less or equal than 0, then the arrays
c                      xmaxdata and nmaxdata are undefined.
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
      integer           vsl
      double precision  xmaxdata(5)
      double precision  nmaxdata(5)
      integer           nshowers
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
      if (vsl .eq. 2) then
        i = 2
      else
        i = 1
      endif
c
      do j = 1, 5
        xmaxdata(j) = shxmax(j, i)
        nmaxdata(j) = shnmax(j)
      enddo
c
      nshowers = shxsamples
c
      return
      end
c     --- End of routine getxmaxdata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getlgodt(tableno, opt, ldt, ndata, xre, otd)
c
c     Returning longitudinal and ground output data tables
c     (Table number in the range 1000 to 2999, and 7000 to 7999).
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     tableno......... (input, integer) AIRES table number.
c     opt............. (input, character*(*)) Options string. The
c                      format of the options string is the same as
c                      in the ExportTables IDL directive.
c     ldt............. (input, integer) Leading dimension of array otd.
c                      ldt must be greater than or equal to 5.
c     ndata........... (output, integer) Number of data items returned.
c                      In case of error ndata is set to zero or
c                      negative.
c     xre............. (output, double precision, array(*)) Array
c                      containing the abscissas corresponding to the
c                      requested table.
c     otd............. (output, double precision, array(ldt, *)) Array
c                      containing the data items corresponding to the
c                      requested table:
c                         odt(1, *) --- Average.
c                         odt(2, *) --- RMS error of the mean.
c                         odt(3, *) --- Standard deviation.
c                         odt(4, *) --- Minimum.
c                         odt(5, *) --- Maximum.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           tableno, ldt
      character*(*)     opt
      integer           ndata
      double precision  xre(ndata)
      double precision  otd(ldt, ndata)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, jtab
      integer           iopt(5)
      double precision  auxtable(5, 0:tdbinsp1)
      double precision  auxtablel(5, mxobslevelsp1)
      double precision  auxtable2(5 * mxobslevelsp1)
      equivalence       (obslevscra0, auxtable, auxtablel, auxtable2)
      logical           thislant, thisnorm
      integer           codopt, itmp1, itmp2, irc
      double precision  fener
      external          logbin
      double precision  logbin
c
c     FIRST EXECUTABLE STATEMENT
c
      ndata = -1
c
      call petopts(2, opt, len(opt), codopt, irc)
      if (irc .ne. 0) return
c
c     Selecting options: iopt(1) --> (0 no header, 1 commented head)
c                        iopt(2) --> (0 no "border" bins,
c                                     1 commented, 2 not commented.)
c                        iopt(3) --> (0 normal (raw) data,
c                                     1 density distributions,
c                                     2 dN/dln distributions,
c                                     3 dN/dlog10 disributions,
c                                     4 slant depths).
c                        iopt(4) --> (0 to 5, KeV, MeV, TeV, PeV,
c                                     EeV)
c                        iopt(5) --> (0 horiz. obs. lev.,
c                                     1 normal obs. planes.)
c
c     iopt(1) and iopt(2) not used here.
c
      itmp1 = codopt
      do i = 1, 5
        itmp2   = itmp1 / 11
        iopt(i) = itmp1 - 11 * itmp2
        itmp1   = itmp2
      enddo
c
c     Selecting the table type.
c
      if ((tableno .lt. 2000) .or.
     +    (tableno .ge. 7000) .and. (tableno .lt. 8000))
     +then
c
c       Longitudinal histograms.
c
        thislant = (iopt(3) .eq. 4)
        thisnorm = (iopt(5) .eq. 1)
c
        if (thislant) then
          call olv2slant(nobslevelsp1, obslevdepth, 0.0d0,
     +                   varzendis, pryzenithmin, pryzenithmax,
     +                   groundz, xre)
        else
          do i = 1, nobslevelsp1
              xre(i) = obslevdepth(i)
          enddo
        endif
c
c       Searching the kind-specific index, and getting data in the
c       cases of normal, unweighted or energy tables.
c
        if (tableno .lt. 1300) then
c
c         Normal tables.
c
          call gettcode(tableno, nlhtables, lhcoden, jtab)
          if (thisnorm) jtab = jtab + mxlhtable
c
          do i = 1, nobslevelsp1
            do j = 1, 5
              otd(j, i) = lhistn(j, i, jtab)
            enddo
          enddo
c
        else if (tableno .lt. 1500) then
c
c         Unweighted tables.
c
          call gettcode(tableno, nlhtables, wlhcoden, jtab)
          if (thisnorm) jtab = jtab + mxlhtable
c
          do i = 1, nobslevelsp1
            do j = 1, 5
              otd(j, i) = wlhistn(j, i, jtab)
            enddo
          enddo
c
        else if (tableno .lt. 2000) then
c
c         Energy tables.
c
          call gettcode(tableno, nlhtables, lhcodee, jtab)
c
c         Notice that option 'p' has no effect here.
c
          fener = 1000.d0 ** (2 - iopt(4))
c
          do i = 1, nobslevelsp1
            do j = 1, 5
              otd(j, i) = fener * lhiste(j, i, jtab)
            enddo
          enddo
c
        else if (tableno .lt. 7300) then
c
c         Number of lowe particles tables.
c
          call gettcode(tableno, nlitables, llcoden, jtab)
          if (thisnorm) jtab = jtab + mxlitable
c
          do i = 1, nobslevelsp1
            do j = 1, 5
              otd(j, i) = llhistn(j, i, jtab)
            enddo
          enddo
c
        else if (tableno .lt. 7500) then
c
c         Unweighted number of lowe particles tables.
c
          call gettcode(tableno, nlitables, wllcoden, jtab)
          if (thisnorm) jtab = jtab + mxlitable
c
          do i = 1, nobslevelsp1
            do j = 1, 5
              otd(j, i) = wllhistn(j, i, jtab)
            enddo
          enddo
c
        else if (tableno .lt. 7800) then
c
c         Energy of low-e particles tables.
c
          call gettcode(tableno, nlitables, llcodee, jtab)
          if (thisnorm) jtab = jtab + mxlitable
c
          fener = 1000.d0 ** (2 - iopt(4))
c
          do i = 1, nobslevelsp1
            do j = 1, 5
              otd(j, i) = fener * llhiste(j, i, jtab)
            enddo
          enddo
c
        else
c
c         Deposited energy tables.
c
          call gettcode(tableno, nlitables, licodee, jtab)
c
          fener = 1000.d0 ** (2 - iopt(4))
c
c         Notice that option 'p' has no effect here.
c
          do i = 1, nobslevelsp1
            do j = 1, 5
              otd(j, i) = fener * lihiste(j, i, jtab)
            enddo
          enddo
c
        endif
c
        ndata = nobslevelsp1
c
      else if (tableno .lt. 2500) then
c
c       Lateral distribution histograms: Radial.
c
c       Searching the kind-specific index, and getting data in the
c       cases of normal or unweighted tables.
c
        if (tableno .lt. 2300) then
c
c         Normal tables.
c
          call gettcode(tableno, nldtables, ldcoden, jtab)
c
          do i = 0, nttabinsp1
            do j = 1, 5
              auxtable(j, i) = rthistn(j, i, jtab)
            enddo
          enddo
c
        else
c
c         Unweighted tables.
c
          call gettcode(tableno, nldtables, wldcoden, jtab)
c
          do i = 0, nttabinsp1
            do j = 1, 5
              auxtable(j, i) = wrthistn(j, i, jtab)
            enddo
          enddo
c
        endif
c
        logbinca = factrthni
        logbincb = rhni0
c
        if (iopt(3) .eq. 1) then
c
c         Density distributions (particles/m2).
c
          call dstransf(5, nttabinsp1, auxtable, auxtable)
c
        else if (iopt(3) .eq. 2) then
c
c         dN/dln(R) distributions.
c
          call lgtransf(5, nttabinsp1, .false., auxtable, auxtable)
c
        else if (iopt(3) .eq. 3) then
c
c         dN/dlog10(R) distributions.
c
          call lgtransf(5, nttabinsp1, .true., auxtable, auxtable)
c
        endif
c
        do i = 1, nttabinsp1
          xre(i) = logbin(i)
          do j = 1, 5
            otd(j, i) = auxtable(j, i)
          enddo
        enddo
        xre(nttabinsp1 + 1) = 0
        do j = 1, 5
          otd(j, nttabinsp1 + 1) = auxtable(j, 0)
        enddo
c
        ndata = nttabinsp1 + 1
c
      else if (tableno .lt. 3000) then
c
c       Energy distributions histograms.
c
c       Searching the kind-specific index, and getting data in the
c       cases of normal or unweighted tables.
c
        if (tableno .lt. 2800) then
c
c         Normal tables.
c
          call gettcode(tableno, nldtables, ldcodee, jtab)
c
          do i = 0, nttabinsp1
            do j = 1, 5
              auxtable(j, i) = rthiste(j, i, jtab)
            enddo
          enddo
c
        else
c
c         Unweighted tables.
c
          call gettcode(tableno, nldtables, wldcodee, jtab)
c
          do i = 0, nttabinsp1
            do j = 1, 5
              auxtable(j, i) = wrthiste(j, i, jtab)
            enddo
          enddo
c
        endif
c
        logbinca = factrthei
        logbincb = rhei0
        fener    = 1000.d0 ** (2 - iopt(4))
c
        if (iopt(3) .eq. 2) then
c
c         dN/dln(E) distributions.
c
          call lgtransf(5, nttabinsp1, .false., auxtable, auxtable)
c
        else if (iopt(3) .eq. 3) then
c
c         dN/dlog10(E) distributions.
c
          call lgtransf(5, nttabinsp1, .true., auxtable, auxtable)
c
        endif
c
        do i = 1, nttabinsp1
          xre(i) = fener * logbin(i)
          do j = 1, 5
            otd(j, i) = auxtable(j, i)
          enddo
        enddo
        xre(nttabinsp1 + 1) = 0
        do j = 1, 5
          otd(j, nttabinsp1 + 1) = auxtable(j, 0)
        enddo
c
        ndata = nttabinsp1 + 1
c
      endif
c
      return
      end
c     --- End of routine getlgodt
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine gettodt(tableno, opt, ldt, ndata, xre, otd, nsamp)
c
c     Returning time output data tables.
c     (Table number in the range 3000 to 3999).
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     tableno......... (input, integer) AIRES table number.
c     opt............. (input, character*(*)) Options string. The
c                      format of the options string is the same as
c                      in the ExportTables IDL directive.
c     ldt............. (input, integer) Leading dimension of array otd.
c                      ldt must be greater than or equal to 5.
c     ndata........... (output, integer) Number of data items returned.
c                      In case of error ndata is set to zero or
c                      negative.
c     xre............. (output, double precision, array(*)) Array
c                      containing the abscissas corresponding to the
c                      requested table.
c     otd............. (output, double precision, array(ldt, *)) Array
c                      containing the data items corresponding to the
c                      requested table:
c                         odt(1, *) --- Average.
c                         odt(2, *) --- RMS error of the mean.
c                         odt(3, *) --- Standard deviation.
c                         odt(4, *) --- Minimum.
c                         odt(5, *) --- Maximum.
c     nsamp........... (output, integer, array(*)) Number of data
c                      samples used to evaluate the respective final
c                      output data items. When nsamp(i) is zero, then
c                      the values stored in otd(*, i) are not
c                      meaningful.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           tableno, ldt
      character*(*)     opt
      integer           ndata
      double precision  xre(ndata)
      double precision  otd(ldt, ndata)
      integer           nsamp(ndata)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, jtab
      integer           iopt(5)
      integer           codopt, itmp1, itmp2, irc
      external          logbin
      double precision  logbin
c
c     FIRST EXECUTABLE STATEMENT
c
      ndata = -1
c
      call petopts(2, opt, len(opt), codopt, irc)
      if (irc .ne. 0) return
c
c     Selecting options: iopt(1) --> (0 no header, 1 commented head)
c                        iopt(2) --> (0 no "border" bins,
c                                     1 commented, 2 not commented.)
c                        iopt(3) --> (0 normal (raw) data,
c                                     1 density distributions,
c                                     2 dN/dln distributions,
c                                     3 dN/dlog10 disributions,
c                                     4 slant depths).
c                        iopt(4) --> (0 to 5, KeV, MeV, TeV, PeV,
c                                     EeV)
c                        iopt(5) --> (0 horiz. obs. lev.,
c                                     1 normal obs. planes.)
c
c     iopt(1) and iopt(2) not used here.
c
      itmp1 = codopt
      do i = 1, 5
        itmp2   = itmp1 / 11
        iopt(i) = itmp1 - 11 * itmp2
        itmp1   = itmp2
      enddo
c
c     Selecting the table type.
c
      if ((tableno .ge. 3000) .and. (tableno .lt. 4000)) then
c
c       Time distribution tables.
c
c       Searching the kind-specific index.
c
        call gettcode(tableno, ntdtables, tdcoden, jtab)
c
        logbinca = factrthni
        logbincb = rhni0
c
        do i = 1, nttabinsp1
          xre(i)    = logbin(i)
          nsamp(i)  = rtsampl(i, jtab)
          do j = 1, 5
            otd(j, i) = rthistt(j, i, jtab)
          enddo
        enddo
        xre(nttabinsp1 + 1)   = 0
        nsamp(nttabinsp1 + 1) = rtsampl(0, jtab)
        do j = 1, 5
          otd(j, nttabinsp1 + 1) = rthistt(j, 0, jtab)
        enddo
c
        ndata = nttabinsp1 + 1
c
      endif
c
      return
      end
c     --- End of routine gettodt
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getpgvshno(tableno, opt, ndata, shno, npcles,
     +                      nuwentries, epcles)
c
c     Returning particles at ground versus shower number data tables.
c     (Table number in the range 1000 to 5299).
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     tableno......... (input, integer) AIRES table number.
c     opt............. (input, character*(*)) Options string. The
c                      format of the options string is the same as
c                      in the ExportTables IDL directive.
c     ndata........... (output, integer) Number of data items returned.
c                      In case of error ndata is set to zero or
c                      negative.
c     shno............ (output, integer, array(*)) Array containing the
c                      shower numbers.
c     npcles.......... (output, double precision, array(*)) Number
c                      of particles at ground, for each shower.
c     nuwentries...... (output, double precision, array(*)) Number
c                      of unweighted particle entries at ground, for
c                      each shower.
c     epcles.......... (output, double precision, array(*)) Energy
c                      of particles at ground, for each shower.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           tableno
      character*(*)     opt
      integer           ndata
      integer           shno(ndata)
      double precision  npcles(ndata)
      double precision  nuwentries(ndata)
      double precision  epcles(ndata)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, l, jtab, jtab1, jtab2, ndp
      integer           iopt(5)
      integer           codopt, itmp1, itmp2, irc
      double precision  auxtable(5, 0:tdbinsp1)
      double precision  auxtablel(5, mxobslevelsp1)
      double precision  auxtable2(5 * mxobslevelsp1)
      equivalence       (obslevscra0, auxtable, auxtablel, auxtable2)
      integer           klast, ngt
      integer           ipars(5)
      integer           fitrc, shprcode
      equivalence       (fitrc, ipars(1))
      equivalence       (shprcode, ipars(2))
      double precision  xpars(5), sumofsq, fener
      integer           kfields, rfields
      external          logbin
      double precision  logbin
c
c     FIRST EXECUTABLE STATEMENT
c
      ndata = 0
c
      if (.not. saveshowerdata) return
c
      ndata = -1
c
      call petopts(2, opt, len(opt), codopt, irc)
      if (irc .ne. 0) return
c
c     Selecting options: iopt(1) --> (0 no header, 1 commented head)
c                        iopt(2) --> (0 no "border" bins,
c                                     1 commented, 2 not commented.)
c                        iopt(3) --> (0 normal (raw) data,
c                                     1 density distributions,
c                                     2 dN/dln distributions,
c                                     3 dN/dlog10 disributions,
c                                     4 slant depths).
c                        iopt(4) --> (0 to 5, KeV, MeV, TeV, PeV,
c                                     EeV)
c                        iopt(5) --> (0 horiz. obs. lev.,
c                                     1 normal obs. planes.)
c
c     iopt(1) and iopt(2) not used here.
c
      itmp1 = codopt
      do i = 1, 5
        itmp2   = itmp1 / 11
        iopt(i) = itmp1 - 11 * itmp2
        itmp1   = itmp2
      enddo
c
c     Selecting the table type.
c
      if ((tableno .ge. 5000) .and. (tableno .lt. 5300)) then
c
c       "Per shower" tables.
c
c       Searching the kind-specific index.
c
        call gettcode(tableno, npshtables, pshcode, jtab)
c
c       Opening the file that contains shower-per-shower data.
c       (ground particles list).
c
        open(shzut, file = shzfn, status = 'OLD',
     +       form = 'UNFORMATTED', err = 3020)
c
        read(shzut, err = 3020, end = 3020) i, klast, ngt,
     +                                      kfields, rfields
        if (jtab .gt. ngt) goto 1150
        if (pershowertables) call intfileskip(shzut, -99966)
c
        rfields = rfields - klast - 1
        i       = firstshowernon1
        jtab1   = jtab + ngt
        jtab2   = jtab1 + ngt
        fener   = 1000.d0 ** (2 - iopt(4))
        ndp     = 0
c
 1100   continue
        read(shzut, err = 3020, end = 1140)
     +              (ipars(l), l = 1, kfields),
     +              (xpars(l), l = 1, klast), sumofsq,
     +              (auxtable2(j), j = 1, rfields)
        if (pershowertables) call intfileskip(shzut, -99966)
        i = i + 1
c
        ndp             = ndp + 1
        shno(ndp)       = i
        npcles(ndp)     = auxtable2(jtab)
        epcles(ndp)     = auxtable2(jtab1) * fener
        nuwentries(ndp) = auxtable2(jtab2)
c
        goto 1100
 1140   continue
c
        ndata = ndp
c
 1150   continue
        close(shzut)
c
      endif
c
      return
c
c     Error processing internal file.
c
 3020 continue
      return
c
      end
c     --- End of routine getpgvshno
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getprxdatvshno(opt, ndata, shno,
     +                          pcode, pegy, pzen, pazi, x1,
     +                          xmax, nmax, x0, lambda, sofsqr, frc)
c
c     Returning primary and Xmax related data versus shower number.
c     This rutine retrieves all the data corresponding to tables
c     5501, 5511, and 5513.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     opt............. (input, character*(*)) Options string. The
c                      format of the options string is the same as
c                      in the ExportTables IDL directive.
c     ndata........... (output, integer) Number of data items returned.
c                      In case of error ndata is set to zero or
c                      negative.
c     shno............ (output, integer, array(*)) Array containing the
c                      shower numbers.
c     pcode........... (output, integer, array(*)) Primary code, for
c                      each shower.
c     pegy............ (output, double precision, array(*)) Primary
c                      energy, for each shower.
c     pzen............ (output, double precision, array(*)) Primary
c                      zenith angle (deg), for each shower.
c     pazi............ (output, double precision, array(*)) Primary
c                      azimuth angle (deg), for each shower.
c     x1.............. (output, double precision, array(*)) Depth of
c                      first interaction (g/cm2), for each shower.
c     xmax............ (output, double precision, array(*)) Depth of
c                      shower maximum (g/cm2), for each shower.
c     nmax............ (output, double precision, array(*)) Number of
c                      particles at maximum (g/cm2), for each shower.
c     x0, lambda...... (output, double precision, array(*)) Remaining
c                      fitted parameters of the Gaisser-Hillas
c                      function, for each shower.
c     sofsqr.......... (output, double precision, array(*)) Normalized
c                      sum of squares of the Gaisser-Hillas function
c                      fit, for each shower.
c     frc............. (output, integer, array(*)) Return code of the
c                      Gaisser-Hillas function fit, for each shower.
c                      Zero means successful fit.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      character*(*)     opt
      integer           ndata
      integer           shno(ndata)
      integer           pcode(ndata)
      double precision  pegy(ndata)
      double precision  pzen(ndata)
      double precision  pazi(ndata)
      double precision  x1(ndata)
      double precision  xmax(ndata)
      double precision  nmax(ndata)
      double precision  x0(ndata)
      double precision  lambda(ndata)
      double precision  sofsqr(ndata)
      integer           frc(ndata)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, l, ndp
      integer           iopt(5)
      integer           codopt, itmp1, itmp2, irc
      double precision  auxtable(5, 0:tdbinsp1)
      double precision  auxtablel(5, mxobslevelsp1)
      double precision  auxtable2(5 * mxobslevelsp1)
      equivalence       (obslevscra0, auxtable, auxtablel, auxtable2)
      integer           klast, knmax, kxmax, kxmaxv, kxmaxs
      integer           kx0, klambda, ngt
      integer           i1, i2, ix1, ipegy, ipzen, ipazi
      integer           ipars(5)
      integer           fitrc, shprcode
      equivalence       (fitrc, ipars(1))
      equivalence       (shprcode, ipars(2))
      double precision  xpars(5), sumofsq, fener, ftmp1
      integer           kfields, rfields
      external          logbin
      double precision  logbin
c
c     FIRST EXECUTABLE STATEMENT
c
      ndata = 0
c
      if (.not. saveshowerdata) return
c
      ndata = -1
c
      call petopts(2, opt, len(opt), codopt, irc)
      if (irc .ne. 0) return
c
c     Selecting options: iopt(1) --> (0 no header, 1 commented head)
c                        iopt(2) --> (0 no "border" bins,
c                                     1 commented, 2 not commented.)
c                        iopt(3) --> (0 normal (raw) data,
c                                     1 density distributions,
c                                     2 dN/dln distributions,
c                                     3 dN/dlog10 disributions,
c                                     4 slant depths).
c                        iopt(4) --> (0 to 5, KeV, MeV, TeV, PeV,
c                                     EeV)
c                        iopt(5) --> (0 horiz. obs. lev.,
c                                     1 normal obs. planes.)
c
c     iopt(1) and iopt(2) not used here.
c
      itmp1 = codopt
      do i = 1, 5
        itmp2   = itmp1 / 11
        iopt(i) = itmp1 - 11 * itmp2
        itmp1   = itmp2
      enddo
c
c     Opening the file that contains shower-per-shower data.
c     (ground particles list).
c
      open(shzut, file = shzfn, status = 'OLD',
     +     form = 'UNFORMATTED', err = 3020)
c
      read(shzut, err = 3020, end = 3020) i, klast, ngt,
     +                                    kfields, rfields
      if (pershowertables) call intfileskip(shzut, -99966)
c
      rfields = rfields - klast - 1
c
      j       = i / 1000000
      i       = i - 1000000 * j
      kx0     = 1
      klambda = 2
      if (i .eq. 114) then
        knmax   = klast - 1
        kxmaxv  = klast - 2
        kxmaxs  = klast
      else
        knmax   = klast
        kxmaxv  = klast - 1
        kxmaxs  = kxmaxv
        if (i .eq. 103) then
          klambda  = 5
          xpars(5) = 70
        endif
      endif
c
      i1 = 3 * ngt + 1
      i2 = i1 + 3
      do j = rfields + 1, i2
        auxtable2(j) = 0
      enddo
      ix1   = i1
      ipegy = i1 + 1
      ipzen = i1 + 3
      ipazi = i1 + 4
c
      if (iopt(3) .eq. 4) then
        kxmax = kxmaxs
        ix1   = ipegy + 1
      else
        kxmax = kxmaxv
      endif
c
      fener    = 1000.d0 ** (2 - iopt(4))
      i        = firstshowernon1
      ndp      = 0
c
 1100 continue
      read(shzut, err = 3020, end = 1140)
     +            (ipars(l), l = 1, kfields),
     +            (xpars(l), l = 1, klast), sumofsq,
     +            (auxtable2(j), j = 1, rfields)
      if (pershowertables) call intfileskip(shzut, -99966)
      i = i + 1
c
      ndp          = ndp + 1
      shno(ndp)    = i
      pcode(ndp)   = shprcode
      pegy(ndp)    = auxtable2(ipegy) * fener
      pzen(ndp)    = auxtable2(ipzen)
      pazi(ndp)    = auxtable2(ipazi)
      x1(ndp)      = auxtable2(ix1)
      xmax(ndp)    = xpars(kxmax)
      nmax(ndp)    = xpars(knmax)
      ftmp1        = xpars(kxmax) / xpars(kxmaxv)
      x0(ndp)      = ftmp1 * xpars(kx0)
      lambda(ndp)  = ftmp1 * xpars(klambda)
      sofsqr(ndp)  = sumofsq
      frc(ndp)     = fitrc
c
      goto 1100
 1140 continue
c
      ndata = ndp
c
 1150 continue
      close(shzut)
c
      return
c
c     Error processing internal file.
c
 3020 continue
      return
c
      end
c     --- End of routine getprxdatvshno
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine lprof0cut(tableno, opt, ldt, nobslevp1,
     +                     xobsl, enpcles, ws)
c
c     Evaluating zero cut energy estimations for logintudinal profiles
c     (table number in the range 1000 to 1299).
c
c     Let N0(X) be the total number of particles as a function of X,
c     N(X) be the number of particles for a given (nonzero) cut energy,
c     Elow(X) the energy of low energy particles as a function of X,
c     and Edep(X) the deposited energy as a function of X. Then the
c     estimation used in this routine is:
c
c                        Elow(X)
c        N0(X) ~ N(X) * ---------
c                        Edep(X)
c
c     Written by: S. J. Sciutto, La Plata 2004.
c
c
c     Arguments:
c     =========
c
c     tableno......... (input, integer) AIRES longitudinal table
c                      number, must be in the range 1000 to 1299.
c     opt............. (input, character*(*)) Options string. The
c                      format of the options string is the same as
c                      in the ExportTables IDL directive.
c     ldt............. (input, integer) Leading dimension of array otd.
c                      ldt must be greater than or equal to 5.
c     nobslevp1....... (output, integer) Number of data items returned,
c                      normally the number of observing levels plus 1.
c                      In case of error nobslevp1 is set to zero or
c                      negative.
c     xobsl........... (output, double precision, array(*)) Array
c                      containing the depths of the observing levels.
c     enpcles......... (output, double precision, array(ldt, *)) Array
c                      containing the estimated number of particles at
c                      each observing level:
c                         enpcles(1, *) --- Average.
c                         enpcles(2, *) --- RMS error of the mean.
c                         enpcles(3, *) --- Standard deviation.
c                         enpcles(4, *) --- Minimum.
c                         enpcles(5, *) --- Maximum.
c     ws.............. (scratch, double precision, array(*)) Working
c                      space of size 10 * nobslevp1.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           tableno, ldt, nobslevp1
      character*(*)     opt
      double precision  xobsl(nobslevp1)
      double precision  enpcles(ldt, nobslevp1)
      double precision  ws(99)
c
c     Declaration of internal variables and arrays.
c
      integer           ndata, i, j, k1, k2, lws1, tableno2
      double precision  efact
c
c     FIRST EXECUTABLE STATEMENT
c
      nobslevp1 = -1
      if ((tableno .le. 1000) .or. (tableno .ge. 1300)) return
c
      call getlgodt(tableno, opt, ldt, ndata, xobsl, enpcles)
      if (ndata .le. 0) return
c
      call getlgodt(tableno + 6500, opt, 5, i, xobsl, ws(1))
      if (ndata .ne. i) return
c
      lws1 = 5 * ndata
c
      if (tableno .lt. 1200) then
        tableno2 = tableno + 6800
      else
        tableno2 = tableno + 6700
      endif
c
      call getlgodt(tableno2, opt, 5, i, xobsl, ws(lws1 + 1))
      if (ndata .ne. i) return
c
      nobslevp1 = ndata
c
      k1 = -4
      do i = 1, ndata
        k1 = k1 + 5
        k2 = k1 + lws1
        if (ws(k2) .gt. 0) then
          ws(i) = ws(k1) / ws(k2)
        else
          ws(i) = 0
        endif
      enddo
c
      efact = 1 + ws(1)
      do j = 1, 5
        enpcles(j, 1) = enpcles(j, 1) * efact
      enddo
      do i = 2, ndata
        efact = 1 + (ws(i) + ws(i - 1)) / 2
        do j = 1, 5
          enpcles(j, i) = enpcles(j, i) * efact
        enddo
      enddo
c
      return
      end
c     --- End of routine lprof0cut
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'iadfutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
