c
c     FILE: idfread.f                       CREATION DATE: 22/JUN/1996.
c                                       LAST MODIFICATION: 25/FEB/2004.
c
c     Routines to restore internal data. Used when restarting task
c     processing.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine idfread(filename, rflag, vrb, irc)
c
c     Reading the internal dump file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2001, 2002, 2003,
c                                2004.
c
c
c     Arguments:
c     =========
c
c
c     filename........ (input, character*(*)) The name of the file to
c                      open. If filename is blank, then the input
c                      filename is set internally accordingly with
c                      the current task name.
c     rflag........... (input, integer) If positive then the
c                      particle stacks are read. Otherwise not.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1, 2 or 3 mean that error
c                      messages will be printed. vrb > 3 is similar to
c                      the previous case, but with the additional
c                      action of stopping the program if a fatal error
c                      takes place.
c     irc............. (output, integer) Return code. zero means
c                      successful return. Other return codes are used
c                      (see source listing)
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
      include 'pclepar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
      include 'versionpar.f'
      include 'idfhead.f'
c
c     Declaration of arguments.
c
      character*(*)     filename
      integer           rflag, vrb, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'thincomm.f'
      include 'maincomm.f'
      include 'kernelcomm.f'
      include 'showercomm.f'
      include 'randomdata.f'
      include 'pclecomm.f'
      include 'ciocomm.f'
      include 'cioauxcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           fnlen, errsev, iiver
      logical           errprt
      integer           i, j, k, l, m, i0, i1, i2, lsh, intut
      logical           sryprog, simprog
      integer           srysev
      integer           nwfstacks1
      integer           itmp0, itmp1, itmp2, itmp3, itmp4, itmp5
      double precision  ftmp1
      integer           infidata, iniidata, inlidata
      integer           insidata, isidatalen
      integer           infidata0, iniidata0, inlidata0
      integer           insidata0, isidatalen0
      integer           infodata, iniodata, inlodata
      integer           infodata0, iniodata0, inlodata0
      integer           incommands, inallodata
      integer           incommands0, inallodata0, imxcdl, xmaxncode
      integer           xnttabins, ingrdprint, inlallcht
      integer           xnlhtables, xnldtables, xntdtables, xnpshtables
      integer           xnlhtable1, xnldtable1, xntdtable1, xnpshtable1
      integer           xnlitables
      integer           xnlitable1
      integer           xnhwciofiles, xnciofiles, xciofiles(mxciofiles)
      integer           xnglobar(2)
      integer           kshz, kfields, rfields, kfnow, rfnow, savedbuf
      integer           xnpstacks
      double precision  stackw(npstacks)
      logical           defsta
c
c     idf header related variables.
c
      character*32      idfheader
      integer           idfintheader
c
c     Previous version compatibility related data.
c
      integer           nprevv
      parameter         (nprevv = 10)
      logical           afterprev(nprevv)
c
      logical           after120, after140, after155, after172
      logical           after201, after215, after232, after23A
      logical           after242, after269
c
      equivalence       (after120, afterprev( 1))
      equivalence       (after140, afterprev( 2))
      equivalence       (after155, afterprev( 3))
      equivalence       (after172, afterprev( 4))
      equivalence       (after201, afterprev( 5))
      equivalence       (after215, afterprev( 6))
      equivalence       (after232, afterprev( 7))
      equivalence       (after23A, afterprev( 8))
      equivalence       (after242, afterprev( 9))
      equivalence       (after269, afterprev(10))
c
c     FIRST EXECUTABLE STATEMENT
c
      if (vrb .le. 0) then
        errsev = 2
        errprt = .false.
      else if (vrb .le. 3) then
        errsev = 3
        errprt = .true.
      else
        errsev = 4
        errprt = .true.
      endif
c
c     Opening the file.
c
      if (filename .eq. ' ') then
        auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // idfext
        fnlen         = leadingfnlen(2) + 4
      else
        call strimcopy(filename, len(filename), auxfilestring, fnlen)
      endif
c
      irc = 115
      close(10, err = 3010)
      open(10, file = auxfilestring, status = 'OLD',
     +        form = 'UNFORMATTED', err = 3010)
      irc = 105
c
      sryprog = (pgmcode .ge. 2000)
      simprog = (pgmcode .lt. 2000)
      if (sryprog) then
        srysev = 2
      else
        if (simprog) then
          srysev  = 4
        else
          srysev  = 1
          sryprog = .true.
        endif
      endif
c
c     READING THE DATA.
c
c     File header.
c
      read(10, end = 3010, err = 3010) idfheader, idfintheader,
     +                                 idfversion
c
c     Checking the validity of the file parameters.
c
      if ((idfheader .ne. idfheader0) .or.
     +    (idfintheader .ne. idfintheader0)) then
        if (errprt) then
          call errprint(0, '*', errsev, 'idfread',
     +      'This file seems not to be a valid AIRES idf file.',
     +      0, 0, 0, 0.d0, auxfilestring(1:fnlen))
        endif
        irc = 206
        return
      endif
c
c     Checking input AIRES version, and backwards compatibility stuff.
c
      call strim(24, idfversion, idfverlen)
      call versioncheck(idfversion(1:idfverlen), iiver, irc)
      idfvnecurrent = (irc .ne. 0)
c
      if (idfvnecurrent) then
c
c       Current and input version are not equal.
c
        if (irc .gt. 100) then
c
c         Invalid version format.
c
          irc = 360
          if (errprt) then
            call errprint(0, '$DF1', errsev, 'idfread',
     +           auxfilestring(1:fnlen),
     +           0, 0, 0, 0.d0, idfversion(1:idfverlen))
          endif
          return
c
        else if (irc .gt. 10) then
c
c         File corresponds to a future version of AIRES.
c
          irc = 350
          if (errprt) then
            call errprint(0, '$DF2', errsev, 'idfread',
     +           auxfilestring(1:fnlen),
     +           0, 0, 0, 0.d0, idfversion(1:idfverlen))
          endif
          return
c
        else
c
c         File was written with a previous AIRES version.
c         Applying backwards compatibility.
c
          if (irc .lt. 0) srysev = min(3, srysev)
          irc = 300 + abs(irc)
c
          if (errprt) then
            call errprint(0, '*', srysev, 'idfread',
     +           'This idf file was not written with the current ' //
     +           'AIRES version$File version   : ' // idfversion //
     +           '$Current version: ' // aires_version,
     +           0, 0, 0, 0.d0, auxfilestring(1:fnlen))
c
            if (iiver .lt. 01010000) then
              call errprint(0, '*', errsev, 'idfread',
     +        'This idf file format is no more supported.',
     +        0, 0, 0, 0.d0, ' ')
              return
            endif
          endif
c
          after120 = (iiver .gt. 01020000)
          after140 = (iiver .gt. 01040000)
          after155 = (iiver .gt. 01050500)
          after172 = (iiver .gt. 01070200)
          after201 = (iiver .gt. 02000102)
          after215 = (iiver .gt. 02010500)
          after232 = (iiver .gt. 02030200)
          after23A = (iiver .gt. 02031000)
          after242 = (iiver .gt. 02040200)
          after269 = (iiver .gt. 02060900)
c
        endif
c
      else
c
c       File was written with the same (current) AIRES version.
c       Applying "null" backwards compatibility.
c
        do i = 1, nprevv
          afterprev(i) = .true.
        enddo
c
      endif
c
c     Reading original version information.
c
      read(10, end = 3010, err = 3010)
     +         original_version, nverchanges, oversionset,
     +         idfcdate, idfcwho, idfcstar
c
      read(10, end = 3010, err = 3010)
     +         idfcruserlen, idfcrhostlen, idfcruser, idfcrhost
c
c     Skipping record of zeros (spare fields for future use).
c     First record is used now to label number of stacks for
c     weight limiting factors.
c     Second field is the number of dump files used to build
c     the merged adf file. It is zero for every idf file.
c
      if (after120) read(10, end = 3010, err = 3010)
     +                        nwfstacks1, adfnmergedfiles,
     +                        (i, j = 3, 10)
c
c     Reading basic data.
c
      read(10, end = 3010, err = 3010) (datistr0(i), i = 1, 3),
     +                                 (cpu0(i), i = 1, 3),
     +                                 shinidati
      read(10, end = 3010, err = 3010) xwdirnamelen, xtasknamelen,
     +                                 xtasknamever, xtasknameset
      if (xwdirnamelen .gt. 0)
     +   read(10, end = 3010, err = 3010) xwdirname(1:xwdirnamelen)
      if (xtasknamelen .gt. 0)
     +   read(10, end = 3010, err = 3010) xtaskname(1:xtasknamelen)
c
      read(10, end = 3010, err = 3010) xtotshowers, xrunshowers,
     +                       xcpuperrun, xprocessjobs,
     +                       xtotshwset, xrunshwset, xcpurunset,
     +                       xprocessjobsset,
     +                       usedefaultd, usedefaults, usedefault,
     +                       lastshower, currshower,
     +                       processnumber, jobnumber, shcomplete,
     +                       stopchsize1, stopchsize2, stopchsize3
c
      read(10, end = 3010, err = 3010) nshprimary,
     +                                 pryenergymin, pryeminset
      if (nshprimary .gt. 0)
     +  read(10, end = 3010, err = 3010)
     +      (shprimary(i), i = 1, nshprimary),
     +      (shprimarywt(i), i = 1, nshprimary),
     +      (shprimarywt0(i), i = 1, nshprimary)
c
      if (after140) then
        read(10, end = 3010, err = 3010)
     +            currethin,
     +            (thinmaxw(i + 1), i = 0, nwfstacks1),
     +            thinmarker
      else
        read(10, end = 3010, err = 3010) currethin, thinmarker
        do i = 1, npstacks
          thinmaxw(i) = 1.d35
        enddo
      endif
c
      read(10, end = 3010, err = 3010) atmoslabel, atmoslset
c
      if (after120) then
c
c       Skipping record of zeros (spare fields for future use),
c       and reading the first shower number and related parameter.
c
        read(10, end = 3010, err = 3010)
     +            firstshowernon1, firstshowerset,
     +            (i, j = 1, 8)
c
      else
c
c       Some basic shower parameters (compatibility with AIRES 1.2.0).
c
        read(10, end = 3010, err = 3010) initshdir, currshdir, showert0
        read(10, end = 3010, err = 3010) primfdata
c
        firstshowernon1 = 0
        firstshowerset  = 0
c
      endif
c
c     Random number generators shared data.
c
      read(10, end = 3010, err = 3010) i
      if (i .gt. 0) then
        read(10, end = 3010, err = 3010) auxxran, auxxran2,
     +                                   auxoriginal_seed
      else
        read(10, end = 3010, err = 3010) auxxran, auxxran2
        auxoriginal_seed = 0
      endif
c
c     Main input data arrays.
c
      if (after155) then
c
c       New format with string variables.
c
        read(10, end = 3010, err = 3010)
     +            infidata, iniidata, inlidata,
     +            insidata, isidatalen,
     +            (i, j = 1, 6)
        read(10, end = 3010, err = 3010)
     +            infidata0, iniidata0, inlidata0,
     +            insidata0, isidatalen0,
     +            (i, j = 1, 6)
c
      else
c
c       Old (AIRES 1.4.2) format.
c
        read(10, end = 3010, err = 3010)
     +            infidata, iniidata, inlidata,
     +            infidata0, iniidata0, inlidata0
c
        insidata    = 0
        insidata0   = 0
        isidatalen  = 0
        isidatalen0 = 0
c
      endif
c
      if ((infidata .gt. mxfidata) .or.
     +    (iniidata .gt. mxiidata) .or.
     +    (inlidata .gt. mxlidata) .or.
     +    (insidata .gt. mxsidata) .or.
     +    (isidatalen .gt. mxsil)       ) goto 3010
c
      if (infidata .gt. 0) then
        read(10, end = 3010, err = 3010)
     +            ((fidata(j, i), j = 0, 3), i = 1, infidata),
     +            (fidatawaset(i), i = 1, infidata),
     +            (fdbdry(i), i = 1, infidata)
      endif
c
      if (iniidata .gt. 0) then
        read(10, end = 3010, err = 3010)
     +            ((iidata(j, i), j = 0, 3), i = 1, iniidata),
     +            (iidatawaset(i), i = 1, iniidata),
     +            (idbdry(i), i = 1, iniidata)
      endif
c
      if (inlidata .gt. 0) then
        read(10, end = 3010, err = 3010)
     +            ((lidata(j, i), j = 0, 2), i = 1, inlidata),
     +            (lidatawaset(i), i = 1, inlidata)
      endif
c
      if (insidata .gt. 0) then
        read(10, end = 3010, err = 3010)
     +            ((sidata(j, i), j = 0, 6), i = 1, insidata),
     +            (sidatawaset(i), i = 1, insidata)
        read(10, end = 3010, err = 3010)
     +            sidatastring(1:isidatalen)
      endif
c
      if (.not. after269) then
        thinningon = .true.
      endif
c
c     Main output data arrays.
c
      read(10, end = 3010, err = 3010) infodata, iniodata, inlodata,
     +                                 infodata0, iniodata0, inlodata0
c
      if ((infodata .gt. mxfodata) .or.
     +    (iniodata .gt. mxiodata) .or.
     +    (inlodata .gt. mxlodata))      goto 3010
c
      if (infodata .gt. 0) then
        read(10, end = 3010, err = 3010)
     +            ((fodata(j, i), j = 1, 5), i = 1, infodata),
     +            (fosamples(i), i = 1, infodata),
     +            (foupdate(i), i = 1, infodata)
      endif
c
      if (iniodata .gt. 0) then
        read(10, end = 3010, err = 3010)
     +            ((iodata(j, i), j = 1, 3), i = 1, iniodata),
     +            (iosamples(i), i = 1, iniodata),
     +            (ioupdate(i), i = 1, iniodata)
      endif
c
      if (inlodata .gt. 0) then
        read(10, end = 3010, err = 3010)
     +            (lodata(i), i = 1, inlodata)
      endif
c
c     Observing levels data.
c
      read(10, end = 3010, err = 3010)
     +         (iobslevdat(i), i = 0, 1), obslevset, nobslevelsp1
      read(10, end = 3010, err = 3010)
     +         ((fobslevdat(i, j), i = 0, 1), j = 1, 2)
      read(10, end = 3010, err = 3010)
     +         obslevstep, obslevminz, obslevmaxz, obslevca,
     +         obslevcb, obslevl0
c
      if (nobslevelsp1 .gt. mxobslevelsp1) goto 3010
c
c     Intersection of the observing levels with the shower axis.
c
      if (after232) then
        read(10, end = 3010, err = 3010)
     +           (obslevsa(i), i = 0, nobslevelsp1)
        read(10, end = 3010, err = 3010)
     +           (zsfact(i), i = 0, 3)
      endif
c
c     Markers for observing levels that are recorded into the
c     longitudinal file(s).
c
      if (nobslevels .gt. 0) then
        if (after140) then
          read(10, end = 3010, err = 3010)
     +             (olfilesave(i), i = 1, nobslevels)
        else
          do i = 1, nobslevels
            olfilesave(i) = .true.
          enddo
        endif
      endif
c
c     Some basic shower parameters.
c
      if (after120) then
c
        read(10, end = 3010, err = 3010)
     +           initshdir, currshdir, cinjz, showert0, primcode
        read(10, end = 3010, err = 3010) primfdata
c
        read(10, end = 3010, err = 3010) dtoplost, hcosinedummy,
     +                                   hrmaxlx, hrmaxly,
     +                                   hrminlx, hrminly
c
        if (nobslevelsp1 .gt. 0) then
          read(10, end = 3010, err = 3010)
     +             ((obslevcore(i, j), i = 1, 5), j = 1, nobslevelsp1)
          read(10, end = 3010, err = 3010)
     +             (obslevt0(j), j = 1, nobslevelsp1)
        endif
c
        read(10, end = 3010, err = 3010) shgbx, shgbz, emagfac, shgbon
c
      else
        cinjz = injz
      endif
c
c     Shower maximum data.
c
      if (after172) then
c
c       Vertical and slant values retrieved.
c
        read(10, end = 3010, err = 3010) shxsamples, itmp1
        if (itmp1 .gt. mxxmaxd) goto 3010
        read(10, end = 3010, err = 3010)
     +           ((shxmax(i, j), i = 1, 5), j = 1, itmp1),
     +           (shnmax(i), i = 1, 5)
      else
c
c       Compatibility with older versions. Slant depths will be
c       evaluated in approximate form (see below, first interaction
c       depth section).
c
        read(10, end = 3010, err = 3010) shxsamples
        read(10, end = 3010, err = 3010) (shxmax(i, 1), i = 1, 5),
     +                                   (shnmax(i), i = 1, 5)
      endif
c
c     Table (histogram) data (including some spare positions).
c
      read(10, end = 3010, err = 3010)
     +         xnttabins, itmp1, maxtabcode,
     +         xnlhtables, xnlhtable1,
     +         xnldtables, xnldtable1,
     +         xntdtables, xntdtable1,
     +         xnpshtables, xnpshtable1,
     +         xnlitables, xnlitable1,
     +         (i, j = 1, 2),
     +         itmp2, itmp3
c
      if (xnttabins   .gt. tdbins)     goto 3010
      if (xnlhtables  .gt. mxlhtable)  goto 3010
      if (xnldtables  .gt. mxldtable)  goto 3010
      if (xntdtables  .gt. mxtdtable)  goto 3010
      if (xnpshtables .gt. mxpshtable) goto 3010
      if (xnlitables  .gt. mxlitable)  goto 3010
c
c     Longitudinal tables.
c
      read(10, end = 3010, err = 3010)
     +     (((lhistn(j, i, k), j = 1, 5), i = 1, nobslevelsp1),
     +     k = 1, xnlhtables)
c
      read(10, end = 3010, err = 3010)
     +     (((lhiste(j, i, k), j = 1, 5), i = 1, nobslevelsp1),
     +      k = 1, xnlhtables)
c
c     Unweighted tables are read conditionally
c
      if (itmp2 .gt. 0) then
        read(10, end = 3010, err = 3010)
     +       (((wlhistn(j, i, k), j = 1, 5), i = 1, nobslevelsp1),
     +       k = 1, xnlhtables)
      endif
c
c     Normal longitudinal tables.
c
      if (after232) then
c
        read(10, end = 3010, err = 3010)
     +       (((lhistn(j, i, k + mxlhtable), j = 1, 5),
     +       i = 1, nobslevelsp1), k = 1, xnlhtables)
c
        read(10, end = 3010, err = 3010)
     +       (((wlhistn(j, i, k + mxlhtable), j = 1, 5),
     +       i = 1, nobslevelsp1), k = 1, xnlhtables)
c
      else
c
        do k = 1, xnlhtables
          do i = 1, nobslevelsp1
            do j = 1, 5
              lhistn(j, i, k + mxlhtable)  = lhistn(j, i, k)
              wlhistn(j, i, k + mxlhtable) = wlhistn(j, i, k)
            enddo
          enddo
        enddo
c
      endif
c
c     Lateral distribution tables.
c
      read(10, end = 3010, err = 3010)
     +     (((rthistn(j, i, k), j = 1, 5), i = 0, itmp1),
     +      k = 1, xnldtables)
c
      read(10, end = 3010, err = 3010)
     +     (((rthiste(j, i, k), j = 1, 5), i = 0, itmp1),
     +      k = 1, xnldtables)
c
c     Unweighted tables are read conditionally
c
      if (itmp3 .gt. 0) then
        read(10, end = 3010, err = 3010)
     +       (((wrthistn(j, i, k), j = 1, 5), i = 0, itmp1),
     +        k = 1, xnldtables)
c
        read(10, end = 3010, err = 3010)
     +       (((wrthiste(j, i, k), j = 1, 5), i = 0, itmp1),
     +        k = 1, xnldtables)
      endif
c
c     Time distribution tables.
c
      read(10, end = 3010, err = 3010)
     +     ((rtsampl(i, k), i = 0, itmp1), k = 1, xntdtables)
      read(10, end = 3010, err = 3010)
     +     (((rthistt(j, i, k), j = 1, 5), i = 0, itmp1),
     +      k = 1, xntdtables)
c
c     Deposited energy and related tables are read conditionally.
c
      if (xnlitables .gt. 0) then
c
        do l = 0, mxlitable, mxlitable
c
          read(10, end = 3010, err = 3010)
     +         (((llhistn(j, i, k + l), j = 1, 5),
     +         i = 1, nobslevelsp1), k = 1, xnlitables)
c
          read(10, end = 3010, err = 3010)
     +         (((llhiste(j, i, k + l), j = 1, 5),
     +         i = 1, nobslevelsp1), k = 1, xnlitables)
c
          read(10, end = 3010, err = 3010)
     +         (((wllhistn(j, i, k + l), j = 1, 5),
     +         i = 1, nobslevelsp1), k = 1, xnlitables)
c
        enddo
c
        read(10, end = 3010, err = 3010)
     +       (((lihiste(j, i, k), j = 1, 5),
     +       i = 1, nobslevelsp1), k = 1, xnlitables)
c
      endif
c
c     Input and output data associated variables (Directive names,
c     etc.)
c
      read(10, end = 3010, err = 3010)
     +         incommands, incommands0, imxcdl,
     +         inallodata, inallodata0, ingrdprint, inlallcht
c
      if ((imxcdl .le. 0) .or. (imxcdl .gt. mxcdl)) goto 3010
c
      if (simprog) then
c
c       Simulation program: Command names must be as already
c                           initialized; cannot read from file.
c
        if ((infidata   .ne. nfidata) .or.
     +      (iniidata   .ne. niidata) .or.
     +      (inlidata   .ne. nlidata) .or.
     +      (insidata   .ne. nsidata) .or.
     +      (isidatalen .ne. sidatalen) .or.
     +      (infodata   .ne. nfodata) .or.
     +      (iniodata   .ne. niodata) .or.
     +      (inlodata   .ne. nlodata)         ) then
c
          if (errprt) then
            call errprint(0, '$A31', 4, 'idfread',
     +        'Read values of "nfidata", "niidata", "nlidata",$' //
     +        '"nsidata", "sidatalen", "nfodata", "niodata"$' //
     +        'and/or "nlodata" are not coincident with the$' //
     +        'respective internal variables.',
     +        0, 0, 0, 0.d0, ' ')
          endif
        endif
c
c       Skipping records.
c
        if (incommands .gt. 0) then
          itmp2 = imxcdl + 47
          do i = 1, incommands
            read(10, end = 3010, err = 3010) auxline(1:itmp2)
          enddo
        endif
c
        if (inallodata .gt. 0) then
          do i = 1, inallodata
            read(10, end = 3010, err = 3010) auxline(1:39)
          enddo
        endif
c
c       Table (histogram) names and related data.
c
        if ((xnttabins   .ne. nttabins)   .or.
     +      (xnlhtables  .ne. nlhtables)  .or.
     +      (xnldtables  .ne. nldtables)  .or.
     +      (xntdtables  .ne. ntdtables)  .or.
     +      (xnpshtables .ne. npshtables) .or.
     +      (xnlitables  .ne. nlitables)      ) then
c
          if (errprt) then
            call errprint(0, '*', 4, 'idfread',
     +        'The numbers of predefined tables are not coincident$' //
     +        'with the respective internal variables.',
     +        0, 0, 0, 0.d0, ' ')
          endif
        endif
c
c       Skipping table names records.
c
        do i = 1, xnlhtables
          read(10, end = 3010, err = 3010) auxline(1:152)
        enddo
c
        if (ingrdprint .gt. 0) then
          read(10, end = 3010, err = 3010)
     +             (j, i = 1, ingrdprint),
     +             (j, i = 1, ingrdprint)
        endif
c
        if (after172) then
          do i = 1, xnlhtables
            read(10, end = 3010, err = 3010) auxline(1:68)
          enddo
        endif
c
        do i = 1, xnldtables
          read(10, end = 3010, err = 3010) auxline(1:136)
        enddo
c
        if (after172) then
          do i = 1, xnldtables
            read(10, end = 3010, err = 3010) auxline(1:136)
          enddo
        endif
c
        do i = 1, xntdtables
          read(10, end = 3010, err = 3010) auxline(1:68)
        enddo
c
        do i = 1, xnpshtables
          read(10, end = 3010, err = 3010) auxline(1:68)
        enddo
c
        if (xnlitables .gt. 0) then
          do i = 1, xnlitables
            read(10, end = 3010, err = 3010) auxline(1:272)
          enddo
        endif
c
      else
c
c       Summary or upgrade programs: Names must be read from file.
c
        if (incommands .gt. 0) then
          do i = 1, incommands
            read(10, end = 3010, err = 3010)
     +               clgname(i), cname(i)(1:imxcdl),
     +               ccode(i), minclen(i), aditem(i),
     +               veryimpdir(i), wngonset(i)
          enddo
        endif
c
        if (inallodata .gt. 0) then
          do i = 1, inallodata
            read(10, end = 3010, err = 3010)
     +               odataname(i),
     +               odatatype(i), odataitem(i),
     +               veryimpodata(i)
          enddo
        endif
c
c       Reading table names and related data.
c
        do i = 1, xnlhtables
          read(10, end = 3010, err = 3010)
     +             lhnamen(i), lhnamee(i), lhpclen(i),
     +             lhcoden(i), lhcodee(i)
        enddo
c
        if (ingrdprint .gt. 0) then
          read(10, end = 3010, err = 3010)
     +             (grdporder(i), i = 1, ingrdprint),
     +             (grdpspa(i), i = 1, ingrdprint)
        endif
c
        if (after172) then
          do i = 1, xnlhtables
            read(10, end = 3010, err = 3010) wlhnamen(i), wlhcoden(i)
          enddo
        endif
c
        do i = 1, xnldtables
          read(10, end = 3010, err = 3010) ldnamen(i), ldnamee(i),
     +                                     ldcoden(i), ldcodee(i)
        enddo
c
        if (after172) then
          do i = 1, xnldtables
            read(10, end = 3010, err = 3010) wldnamen(i), wldnamee(i),
     +                                       wldcoden(i), wldcodee(i)
          enddo
        endif
c
        do i = 1, xntdtables
          read(10, end = 3010, err = 3010) tdnamen(i), tdcoden(i)
        enddo
c
        do i = 1, xnpshtables
          read(10, end = 3010, err = 3010) pshname(i), pshcode(i)
        enddo
c
        if (xnlitables .gt. 0) then
          do i = 1, xnlitables
            read(10, end = 3010, err = 3010) llnamen(i), llnamee(i),
     +                                       linamee(i), wllnamen(i),
     +                                       llcoden(i), llcodee(i),
     +                                       licodee(i), wllcoden(i)
          enddo
        endif
c
        nfidata     = infidata
        niidata     = iniidata
        nlidata     = inlidata
        nsidata     = insidata
        sidatalen   = isidatalen
        nfidata0    = infidata0
        niidata0    = iniidata0
        nlidata0    = inlidata0
        nsidata0    = insidata0
        sidatalen0  = isidatalen0
        nfodata     = infodata
        niodata     = iniodata
        nlodata     = inlodata
        ncommands   = incommands
        ncommands0  = incommands0
        nallodata   = inallodata
c
        nttabins    = xnttabins
        nttabinsp1  = xnttabins + 1
        nttabinsp2  = xnttabins + 2
        nlhtables   = xnlhtables
        nlhtable1   = xnlhtable1
        nldtables   = xnldtables
        nldtable1   = xnldtable1
        ntdtables   = xntdtables
        ntdtable1   = xntdtable1
        npshtables  = xnpshtables
        npshtable1  = xnpshtable1
        nlitables   = xnlitables
        nlitable1   = xnlitable1
        ngrdprint   = ingrdprint
        lallcht     = inlallcht
c
      endif
c
c     Skipping record of zeros (spare fields for future use).
c     First element is now used to store the size of the site library.
c     Second element is now used to store the number of "special"
c     particles that were defined.
c     Third and fourth elements are now used to store the numbers
c     of global variables (dynamic and static, respectively).
c
      read(10, end = 3010, err = 3010)
     +         nlibsites, nescpcles,
     +         xnglobar(1), xnglobar(2),
     +         (i, j = 5, 10)
c
c     Reading the site library.
c
      if (nlibsites .gt. 0) then
        read(10, end = 3010, err = 3010)
     +           (sitename(i), i = 0, nlibsites)
        read(10, end = 3010, err = 3010)
     +           (sitenlen(i), i = 0, nlibsites)
        read(10, end = 3010, err = 3010)
     +           (sitelat(i), i = 0, nlibsites)
        read(10, end = 3010, err = 3010)
     +           (sitelong(i), i = 0, nlibsites)
        read(10, end = 3010, err = 3010)
     +           (siteground(i), i = 0, nlibsites)
      endif
c
c     Reading data related with "Special" particles.
c
      if (nescpcles .gt. 0) then
c
        read(10, end = 3010, err = 3010)
     +           lastescpcle, itmp0, itmp1, itmp2, itmp3
        if (nescpcles .gt. itmp0)   goto 3010
        if (lastescpcle .gt. itmp2) goto 3010
        if (itmp1 .ne. pescode1)    goto 3010
        read(10, end = 3010, err = 3010)
     +           (escpclename(i), i = 1, nescpcles)
        read(10, end = 3010, err = 3010)
     +           ((escmacropos(j, i), j = 1, 4), i = 0, nescpcles)
        if (escmacropos(4, nescpcles) .gt. espms) goto 3010
        read(10, end = 3010, err = 3010)
     +           escpclemacro(1:escmacropos(4, nescpcles))
        read(10, end = 3010, err = 3010)
     +           (escmacrover(i), i = 1, nescpcles)
        read(10, end = 3010, err = 3010)
     +           (escmacrouse(i), i = 1, nescpcles)
        read(10, end = 3010, err = 3010)
     +           ((nsprimpart(j, i), j = 1, 3), i = 1, nescpcles)
c
        read(10, end = 3010, err = 3010)
     +            nintintvar, nintfltvar, recordspvar0, i, i
        read(10, end = 3010, err = 3010) itmp1, itmp2, i, i, i
c
        if (nintintvar .gt. 0) then
          if (nintintvar .gt. mxintintvar) goto 3010
          read(10, end = 3010, err = 3010)
     +             (spintvar(i), i = 1, nintintvar)
        endif
        if (nintfltvar .gt. 0) then
          if (nintfltvar .gt. mxintfltvar) goto 3010
          read(10, end = 3010, err = 3010)
     +             (spfltvar(i), i = 1, nintfltvar)
        endif
c
      endif
c
c     Reading global variables.
c
c     Dynamic variables are restored from the idf file only from a
c     summary program.
c
      if (xnglobar(1) .gt. 0) then
        if (sryprog) then
          if (xnglobar(1) .gt. mxglobar) goto 3010
          nglobar(1) = xnglobar(1)
c
          read(10, end = 3010, err = 3010)
     +             (globnam(i, 1), i = 1, nglobar(1)),
     +             (globlen(i, 1), i = 1, nglobar(1))
          read(10, end = 3010, err = 3010)
     +             globstrlen(1),
     +             (globdfend(i, 1), i = 0, nglobar(1))
          if (globstrlen(1) .gt. 0) then
            if (globstrlen(1) .gt. mxgll) goto 3010
            read(10, end = 3010, err = 3010)
     +               globarstring(1)(1:globstrlen(1))
          endif
        else
          read(10, end = 3010, err = 3010)
     +             (auxline(1:imxcdl), i = 1, xnglobar(1)),
     +             (j, i = 1, xnglobar(1))
          read(10, end = 3010, err = 3010)
     +             itmp1, (j, i = 0, xnglobar(1))
          if (itmp1 .gt. 0) then
            read(10, end = 3010, err = 3010)
     +               (auxline(1:1), i = 1, itmp1)
          endif
        endif
      endif
c
c     Static variables are always read in.
c
      nglobar(2) = xnglobar(2)
      if (nglobar(2) .gt. 0) then
        if (nglobar(2) .gt. mxglobar) goto 3010
c
        read(10, end = 3010, err = 3010)
     +           (globnam(i, 2), i = 1, nglobar(2)),
     +           (globlen(i, 2), i = 1, nglobar(2))
        read(10, end = 3010, err = 3010)
     +           globstrlen(2),
     +           (globdfend(i, 2), i = 0, nglobar(2))
        if (globstrlen(2) .gt. 0) then
          if (globstrlen(2) .gt. mxgll) goto 3010
          read(10, end = 3010, err = 3010)
     +             globarstring(2)(1:globstrlen(2))
        endif
      endif
c
      thereareglobars = ((nglobar(1) + nglobar(2)) .gt. 0)
c
c     Reading model specific variables.
c
      if (simprog) then
c
        call modelidfread(10, irc)
        if (irc .ne. 0) goto 3030
        call idfgetskip(10, i, j, k, irc)
        if (irc .ne. 0) goto 3030
c
      else
c
 1100   continue
        call idfgetskip(10, i, j, k, irc)
        if (irc .ne. 0) goto 3030
        if (i .ne. -99999) goto 1100
c
      endif
c
c     For AIRES versions 1.2.0 and previous, there was a single
c     particle buffer, and was saved right here.
c
      if (.not. after120) then
        call ciobufread(10, 1, irc)
        if (irc .ne. 0) goto 3010
      endif
c
c     Recovering the cio buffers and related variables.
c
      if (simprog) then
c
c       Recovering cio information for simulation programs.
c       (No need for backwards compatibility).
c
        read(10, end = 3010, err = 3010) xnhwciofiles, xnciofiles,
     +                                   nciofilesu, itmp1
        savedbuf = xnhwciofiles
        if (xnhwciofiles .ne. nhwciofiles) goto 3015
        if (xnciofiles .ne. nciofiles) goto 3015
        if (itmp1 .gt. mxciofiles) goto 3010
c
        if (nciofilesu .gt. 0) then
          read(10, end = 3010, err = 3010)
     +      (ciofilesu(i), i = 1, nciofilesu)
          do i = 1, nciofilesu
            j = ciofilesu(i)
            read(10, end = 3010, err = 3010)
     +        auxline(1:12), pofileno(j),
     +        cioreclast(j), lastciopointer(i),
     +        ciorlastcp(j), ciowblocks(j)
            if (auxline(1:12) .ne. pofilext(j)) goto 3015
            if (cioreclast(j) .gt. 0) then
              read(10, end = 3010, err = 3010)
     +          ciorecord1(j)(1:cioreclast(j))
            endif
          enddo
        endif
c
        if (xnciofiles .gt. 0) then
          do i = 1, xnciofiles
            read(10, end = 3010, err = 3010) itmp2
            if (itmp2 .ne. nrectypes(i)) goto 3015
            do j = 0, nrectypes(i)
              if (after269) then
                read(10, end = 3010, err = 3010) itmp3, itmp4, itmp5
              else
                read(10, end = 3010, err = 3010) itmp3
                itmp4 = 0
                itmp5 = itmp3
              endif
              if (itmp3 .ne. totrecfields(j, i)) goto 3015
              ciodynfield(j, i) = itmp4
              ciodynfwcix(j, i) = itmp5
              itmp5 = max(itmp3, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofminv(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofmaxv(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwsc0(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwsc1(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrsc0(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrsc1(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwca(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwcb(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrca(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrcb(k, j, i), k = 1, itmp5)
            enddo
c
c           Reading the specifications on which particles are to be
c           saved in the corresponding cio file.
c
            read(10, end = 3010, err = 3010) anypinfile(i)
            read(10, end = 3010, err = 3010)
     +        (allpclesave(k, i), k = -maxpcle, maxncode)
c
          enddo
        endif
c
        if (xnhwciofiles .gt. 0) then
          read(10, end = 3010, err = 3010)
     +      (xciofiles(i), i = 1, xnhwciofiles)
        endif
        do i = 1, nhwciofiles
          if (xciofiles(i) .ne. ciofiles(i)) goto 3015
        enddo
c
      else
c
c       Entering cio information for non-simulation programs
c       (e.g. summary programs).
c
        if (after120) then
          read(10, end = 3010, err = 3010) nhwciofiles, nciofiles,
     +                                     nciofilesu, itmp1
          savedbuf = nhwciofiles
        else
          read(10, end = 3010, err = 3010) nciofiles,
     +                                     nciofilesu, itmp1
          nhwciofiles = nciofiles
c
c         Reading old statistical variables related with cio
c         ground particle buffer.
c
          call ciostatread(10, 0, after201, irc)
          if (irc .ne. 0) goto 3010
          savedbuf = -1
c
        endif
        if (itmp1 .gt. mxciofiles) goto 3010
c
        if (nciofilesu .gt. 0) then
          read(10, end = 3010, err = 3010)
     +      (ciofilesu(i), i = 1, nciofilesu)
          do i = 1, nciofilesu
            j = ciofilesu(i)
            if (after23A) then
              read(10, end = 3010, err = 3010)
     +          pofilext(j), pofileno(j),
     +          cioreclast(j), lastciopointer(i),
     +          ciorlastcp(j), ciowblocks(j)
            else
              read(10, end = 3010, err = 3010)
     +          pofilext(j), pofileno(j), cioreclast(j)
            endif
            if (cioreclast(j) .gt. 0) then
              read(10, end = 3010, err = 3010)
     +          ciorecord1(j)(1:cioreclast(j))
            endif
          enddo
        endif
c
        if (nciofiles .gt. 0) then
          if (.not. after120)
     +      read(10, end = 3010, err = 3010)
     +        (ciofiles(i), i = 1, nciofiles)
          if (after242) then
            xmaxncode = maxncode
          else
            xmaxncode =  nuccod0 + nuccod1 * 26 + nuccod1 - 1
          endif
          do i = 1, nciofiles
            read(10, end = 3010, err = 3010) nrectypes(i)
            do j = 0, nrectypes(i)
              if (after269) then
                read(10, end = 3010, err = 3010) itmp3, itmp4, itmp5
              else
                read(10, end = 3010, err = 3010) itmp3
                itmp4 = 0
                itmp5 = itmp3
              endif
              totrecfields(j, i) = itmp3
              ciodynfield(j, i)  = itmp4
              ciodynfwcix(j, i)  = itmp5
              itmp5 = max(itmp3, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofminv(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofmaxv(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwsc0(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwsc1(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrsc0(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrsc1(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwca(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofwcb(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrca(k, j, i), k = 1, itmp5)
              read(10, end = 3010, err = 3010)
     +          (ciofrcb(k, j, i), k = 1, itmp5)
            enddo
c
c           Reading the specifications on which particles are to be
c           saved in the corresponding cio file.
c
            read(10, end = 3010, err = 3010) anypinfile(i)
            read(10, end = 3010, err = 3010)
     +        (allpclesave(k, i), k = -maxpcle, xmaxncode)
            do k = xmaxncode + 1, maxncode
              allpclesave(k, i) = .false.
            enddo
c
          enddo
        endif
c
        if (after120) then
          if (nhwciofiles .gt. 0) then
            read(10, end = 3010, err = 3010)
     +        (ciofiles(i), i = 1, nhwciofiles)
          endif

        endif
c
      endif
c
c     Reading statistical variables related with cio particle buffers.
c
      call ciostatread(10, savedbuf, after201, irc)
      if (irc .ne. 0) goto 3010
c
c     Stack names, processing modes, etc.
c
      read(10, end = 3010, err = 3010) xnpstacks, minpstack
c
      if (simprog) then
        if (xnpstacks .ne. npstacks) goto 3010
      else
        if (xnpstacks .gt. npstacks) goto 3010
      endif
c
      read(10, end = 3010, err = 3010)
     +         (psta_mode(i), i = 1, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         (psta_n(i), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         (psta_model_n(i), i = 1, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((psta_rn(j, i), j = 1, 2), i = 1, xnpstacks)
c
c     Stack statistical data and related variables
c
      read(10, end = 3010, err = 3010)
     +         (avgtotsize(i), i = 1, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((peakstsize(j, i), j = 1, 3), i = 1, xnpstacks)
c
c     The following patch ensures compatibility with old integer
c     "procentries".
c
      read(10, end = 3010, err = 3010)
     +         ((hardswrite(j, i), j = 1, 3), i = 1, xnpstacks)
      if (hardswrite(1, 1) .eq. -3456789) then
        read(10, end = 3010, err = 3010)
     +           ((procentries(j, i), j = 1, 3), i = 1, xnpstacks)
      else
        do i = 1, xnpstacks
          do j = 1, 3
            procentries(j, i) = hardswrite(j, i)
          enddo
        enddo
      endif
c
      read(10, end = 3010, err = 3010)
     +         ((hardswrite(j, i), j = 1, 3), i = 1, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((hardsread(j, i), j = 1, 3), i = 1, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((callcounter(j, i), j = 1, 2), i = 0, xnpstacks)
c
      read(10, end = 3010, err = 3010)
     +         ((totpcles(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((eloss(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((nplost(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((elost(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((nplowe(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((elowe(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((nprgnd(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, end = 3010, err = 3010)
     +         ((eprgnd(j, i), j = 1, 5), i = 0, xnpstacks)
c
c     Information about unphysical particles, first interaction, etc.
c
      read(10, end = 3010, err = 3010) (nnotap(j), j = 1, 5)
      read(10, end = 3010, err = 3010) (enotap(j), j = 1, 5)
      read(10, end = 3010, err = 3010) (nneutrino(j), j = 1, 5)
      read(10, end = 3010, err = 3010) (eneutrino(j), j = 1, 5)
      read(10, end = 3010, err = 3010) (aveprim(j), j = 1, 5)
c
      if (after172) then
c
c       Retrieving vertical and slant depths.
c
        read(10, end = 3010, err = 3010)
     +           ((fstintdp(j, i), j = 1, 5), i = 1, 2)
      else
c
c       Compatibility with older versions. Slant depths for both X1 and
c       Xmax are evaluated in approximate form.
c
        read(10, end = 3010, err = 3010)
     +           (fstintdp(j, 1), j = 1, 5)
c
        ftmp1 = 1 / cos(0.01745329252d0 * pryzenithmin)
        do j = 1, 5
          fstintdp(j, 2) = ftmp1 * fstintdp(j, 1)
          shxmax(j, 2)   = ftmp1 * shxmax(j, 1)
        enddo
      endif
c
      if (after120) then
        read(10, end = 3010, err = 3010)
     +           (fstposdp(i), i = 1, 5), fstdpmanual
        if (after215) then
          read(10, end = 3010, err = 3010) fstintnotset, fstintauto,
     +                                     specialprim, specialprimlab,
     +                                     nspecialprim
        else
          read(10, end = 3010, err = 3010) fstintnotset, fstintauto
          specialprim    = .false.
          specialprimlab = 0
          nspecialprim   = 0
        endif
      else
        read(10, end = 3010, err = 3010)
     +           (fstposdp(i), i = 1, 4)
        read(10, end = 3010, err = 3010) fstintnotset
      endif
c
c     Skipping record of zeros (spare fields for future use).
c     First field is now used for shower maximum data file.
c
      read(10, end = 3010, err = 3010) kshz, (i, j = 1, 9)
c
c     READING DATA TO BE STORED IN OTHER INTERNAL FILES.
c
c     Remarks.
c
      read(10, end = 3010, err = 3010) remark
      if (remark) then
c
c       Opening the remarks file.
c
        intut = rmkut
        open(rmkut, file = rmkfn, status = 'UNKNOWN',
     +              form = 'UNFORMATTED', err = 3020)
c
 1010   continue
        read(10, end = 3010, err = 3010) i
        if (i .eq. -99999) goto 1020
        write(rmkut, err = 3020) i
c
        if (i .gt. 0) then
          read(10, end = 3010, err = 3010) auxline(1:i)
          write(rmkut, err = 3020) auxline(1:i)
        endif
        goto 1010
 1020   continue
        close(rmkut)
c
      endif
c
c     Listing of shower maximum data and related quantities (shower per
c     shower).
c
c     Opening the corresponding internal file.
c
      intut = shzut
      open(shzut, file = shzfn, status = 'UNKNOWN',
     +            form = 'UNFORMATTED', err = 3020)
c
      if (kshz .eq. 6) then
c
c       Current format with extended header.
c
        read(10, err = 3010, end = 3010)
     +       i0, i1, lsh, kfields, rfields,
     +       (obslevscrai(j), j = 21, 26)
c
        pershowertables = (i0 .ge. 1000000)
        saveshowerdata  = (rfields .gt. 0)
c
        if (pershowertables) then
          j = 31
          do i = 1, obslevscrai(21)
            read(10, err = 3010, end = 3010)
     +           (obslevscrai(j + l), l = 1, 10)
            j = j + 10
          enddo
          read(10, err = 3010, end = 3010) l
        endif
c
      else if (kshz .eq. 4) then
c
c       Previous format with brief header.
c
        read(10, end = 3010, err = 3010)
     +       i0, i1, lsh, kfields, rfields
c
        pershowertables = .false.
        saveshowerdata  = .true.
        do l = 21, 26
          obslevscrai(l) = 0
        enddo
c
      else
c
c       Very old format with no header.
c
        i0              = 103
        i1              = 3
        lsh             = xnlhtables
        kfields         = 1   
        rfields         = 2 * xnlhtables + 4
        pershowertables = .false.
        saveshowerdata  = .true.
        do l = 21, 26
          obslevscrai(l) = 0
        enddo
c
      endif
c
c     Comparing the stored number of fields with the current one.
c
      kfnow = 2
      rfnow = nshff * xnlhtables + i1 + 6
c
c     The header is now written for every case, and using the
c     corresponding number of fields.
c
      if (saveshowerdata) then
        write(shzut, err = 3020) i0, i1, lsh, kfnow, rfnow,
     +                           (obslevscrai(j), j = 21, 26)
      else
        write(shzut, err = 3020) i0, i1, lsh, kfields, rfields,
     +                           (obslevscrai(j), j = 21, 26)
      endif
c
      if (pershowertables) then
        j = 31
        do i = 1, obslevscrai(21)
          write(shzut, err = 3020)
     +                 (obslevscrai(j + l), l = 1, 10)
          obslevscrai(j + 1) = obslevscrai(j + 1) * obslevscrai(j + 2)
          j = j + 10
        enddo
        i = -99966
        write(shzut, err = 3020) i
      endif
c
      if (saveshowerdata .and. (lastshower .gt. 0)) then
c
        do i = 1, max(kfields, kfnow)
          obslevscraj(i) = 0
        enddo
c
        do i = 1, max(rfields, rfnow)
          obslevscra1(i) = 0
        enddo
c
        if (pershowertables) then
c
c         Copying full table information
c
          do i = 1, lastshower
c
c           Shower header.
c
            read(10, end = 3010, err = 3010)
     +        (obslevscraj(k), k = 1, kfields),
     +        (obslevscra1(k), k = 1, rfields)
            write(shzut, err = 3020)
     +        (obslevscraj(k), k = 1, kfnow),
     +        (obslevscra1(k), k = 1, rfnow)
            read(10, err = 3010, end = 3010)
     +        (obslevscrai(k), k = 1, 10)
            write(shzut, err = 3020) (obslevscrai(k), k = 1, 10)
c
c           Tables.
c
            j = 31
            do m = 1, obslevscrai(21)
              i1 = obslevscrai(j + 3)
              i2 = obslevscrai(j + 4)
              do k = 1, obslevscrai(j + 1)
                read(10, end = 3010, err = 3010)
     +            (obslevscra4(l), l = i1, i2)
                write(shzut, err = 3020) (obslevscra4(l), l = i1, i2)
              enddo
              j = j + 10
            enddo
c
c           End shower mark.
c
            read(10, end = 3010, err = 3010) i1
            write(shzut, err = 3010) i1
c
          enddo
c
        else
c
          do i = 1, lastshower
            read(10, end = 3010, err = 3010)
     +        (obslevscraj(k), k = 1, kfields),
     +        (obslevscra1(k), k = 1, rfields)
            write(shzut, err = 3020)
     +        (obslevscraj(k), k = 1, kfnow),
     +        (obslevscra1(k), k = 1, rfnow)
          enddo
c
        endif
      endif
c
c     Reading trailing record.
c
      if (lastshower .gt. 0) then
c
        read(10, end = 3010, err = 3010) lsh
c
        if (lsh .ne. lastshower) then
          if (errprt) then
            call errprint(0, '*', 2, 'idfread',
     +      'Shower maximum listing valid only up to shower number:',
     +      1, lsh, 0, 0.d0, ' ')
          endif
          irc = 2
        endif
      endif
c
      close(shzut)
c
c     Skipping record of zeros (spare fields for future use).
c     Now the first element is assigned to variable savedbuf
c
      read(10, end = 3010, err = 3010) savedbuf,
     +                                 (i, j = 2, 10)
c
c     The remaining reading opeartions are performed conditionally.
c
      irc = 0
      if (rflag .gt. 0) then
c
c       Reading the cio particle buffers.
c
        if (savedbuf .gt. 0) then
          call ciobufread(10, nhwciofiles, irc)
          if (irc .ne. 0) goto 3010
        endif
c
c       Reading the stacks.
c
c       Evaluating stack weights if there are completed showers;
c       otherwise no action is taken and the weights set in
c       routine "init0" become effective.
c
        if (lastshower .gt. 0) then
          call setstackw(stackw)
          i = 1
        else
          i = 0
        endif
c
c       Opening the particle stacks.
c
        if (.not. defsta(i, npstacks, 0, stackw,
     +            leadingfn(mxwdir)(1:leadingfnlen(mxwdir)))) then
          if (errprt) then
            call errprint(2, '$A34', errsev, 'idfread', ' ',
     +                    0, 0, 0, 0.d0, ' ')
          endif
          irc = 112
          return
        endif
c
        call stacp2d(10, itmp1)
c
        if (itmp1 .eq. 5) goto 3010
      endif
c
c     All done, closing the file.
c
      close(10)
c
      if (irc .eq. 105) irc = 0
      return
c
c     Error messages.
c
 3010 continue
      if (errprt) then
        call errprint(0, '$A30', errsev, 'idfread', ' ',
     +                1, 10, 0, 0.d0,
     +                auxfilestring(1:fnlen))
      endif
      irc = 97
      return
c
 3015 continue
      if (errprt) then
        call errprint(0, '$A31', errsev, 'idfread',
     +                '(Not coincident compressed file definitions).',
     +                0, 0, 0, 0.d0, ' ')
      endif
      irc = 67
      return
c
 3020 continue
c
      if (errprt) then
        call errprint(0, '$A09', errsev, 'idfread',
     +                ' ', 1, intut, 0, 0.d0, ' ')
      endif
      irc = 107
      return
c
 3030 continue
      if (errprt) then
        call errprint(0, '$A09', 4, 'idfread',
     +   '(Error condition enabled during call to "modelidfread")',
     +   1, 10, 0, 0.d0, auxfilestring(1:fnlen))
      endif
      irc = 109
      return
c
      end
c     --- End of routine idfread
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine idfget(iounit, nsw, nflt, fltvar, nint, intvar,
     +                  nlog, logvar, rc)
c
c     Auxiliary routine for restoring internal variables from the IDF
c     file.
c     The routine restores variables written using "idfput"
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     iounit......... (input, integer) The i/o unit number connected
c                     with the IDF file. It points to an already
c                     opened unformatted file. The unit must not be
c                     closed.
c     nsw............ (input, logical) Number of data checking switch.
c                     If true, the read values of "nflt", "nint",
c                     and "nlog" are tested to be equal to the
c                     corresponding given values. If false, then these
c                     numbers are returned as output.
c     nflt........... (input-output, integer) Number of restored
c                     floating point data (See "nsw").
c     fltvar......... (output, double precision, array(nflt)) Floating
c                     point data.
c     nint........... (input-output, integer) Number of restored
c                     integer data (See "nsw").
c     intvar......... (output, integer, array(nint)) Integer data.
c     nlog........... (input-output, integer) Number of restored
c                     logical data (See "nsw").
c     logvar......... (output, logical, array(nlog)) Logical data.
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the restoring operation is completed
c                     successfully. Nonzero otherwise.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iounit, nflt, nint, nlog, rc
      logical           nsw
      double precision  fltvar(nflt)
      integer           intvar(nint)
      logical           logvar(nlog)
c
c     Declaration of internal variables and arrays.
c
      integer           i, inflt, inint, inlog
c
c     FIRST EXECUTABLE STATEMENT
c
c     Restoring array size information.
c
      read(iounit, end = 3010, err = 3010) inflt, inint, inlog
c
c     Restoring data.
c
      if (inflt .gt. 0) then
        read(iounit, end = 3010, err = 3010)
     +               (fltvar(i), i = 1, inflt)
      endif
c
      if (inint .gt. 0) then
        read(iounit, end = 3010, err = 3010)
     +               (intvar(i), i = 1, inint)
      endif
c
      if (inlog .gt. 0) then
        read(iounit, end = 3010, err = 3010)
     +               (logvar(i), i = 1, inlog)
      endif
c
      rc = 0
c
c     Checking the numbers of data items.
c
      if (nsw) then
c
c       Contrasting with given ones.
c
        if ((inflt .ne. nflt) .or.
     +      (inint .ne. nint) .or.
     +      (inlog .ne. nlog)      ) rc = 88
c
      else
c
c       Setting output quantities.
c
        nflt = inflt
        nint = inint
        nlog = inlog
c
      endif
      return
c
c     Error exit
c
 3010 continue
      rc = 56
      return
c
      end
c     --- End of routine idfget
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine idfgetskip(iounit, nflt, nint, nlog, rc)
c
c     Auxiliary routine for restoring internal variables from the IDF
c     file.
c     The routine skips a record written using "idfput"
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     iounit......... (input, integer) The i/o unit number connected
c                     with the IDF file. It points to an already
c                     opened unformatted file. The unit must not be
c                     closed.
c     nflt........... (output, integer) Number of floating point data
c                     in the skipped record.
c     nint........... (output, integer) Number of integer data in the
c                     skipped record.
c     nlog........... (output, integer) Number of logical data in the
c                     skipped record.
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the skipping operation is completed
c                     successfully. Nonzero otherwise.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iounit, nflt, nint, nlog, rc
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  a
      integer           b
      logical           c
c
c     FIRST EXECUTABLE STATEMENT
c
c     Restoring array size information.
c
      read(iounit, end = 3010, err = 3010) nflt, nint, nlog
c
c     Skipping data records
c
      if (nflt .gt. 0) then
        read(iounit, end = 3010, err = 3010)
     +               (a, i = 1, nflt)
      endif
c
      if (nint .gt. 0) then
        read(iounit, end = 3010, err = 3010)
     +               (b, i = 1, nint)
      endif
c
      if (nlog .gt. 0) then
        read(iounit, end = 3010, err = 3010)
     +               (c, i = 1, nlog)
      endif
c
      rc = 0
      return
c
c     Error exit
c
 3010 continue
      rc = 56
      return
c
      end
c     --- End of routine idfgetskip
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciostatread(iounit, nbuf, longlow, rc)
c
c     Reading the statistical counters related with the cio particle
c     buffers.
c
c     Written by: S. J. Sciutto, La Plata 1997; Fermilab 1999.
c
c
c     Arguments:
c     =========
c
c     iounit......... (input, integer) The i/o unit number connected
c                     with the IDF file. It points to an already
c                     opened unformatted file. The unit must not be
c                     closed.
c     nbuf........... (input, integer) The number of buffers whose
c                     related data must be read in.
c     longlow........ (input, logical) True for long "low" particles
c                     counters. False for compatibility with previous
c                     versions.
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the skipping operation is completed
c                     successfully. Nonzero otherwise.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
c     include 'pstackpar.f'      (Included by 'pbuffpar.f')
c     include 'ciopar.f'         (Included by 'pbuffpar.f')
      include 'pbuffpar.f'
c
c     Declaration of arguments.
c
      integer           iounit, nbuf, rc
      logical           longlow
c
c     Declaration of shared data.
c
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, kmax, nsfiles
c
c     FIRST EXECUTABLE STATEMENT
c
      rc = 0
c
      if (nbuf .lt. 0) return
c
      if (longlow) then
        kmax = 2
      else
        kmax = 1
      endif
c
c     Reading the ground particle buffer related data.
c
      read(iounit, end = 3010, err = 3010)
     +              ((ngndlowp(j, k), j = 1, 5), k = 1, kmax)
      read(iounit, end = 3010, err = 3010) ngndhighp
      read(iounit, end = 3010, err = 3010)
     +              ((egndlowp(j, k), j = 1, 5), k = 1, kmax)
      read(iounit, end = 3010, err = 3010) egndhighp
c
      if (nbuf .lt. 1) return
c
c     Reading statistical data associated with the other cio particle
c     buffers.
c
      read(iounit, end = 3010, err = 3010) nsfiles
      if (nsfiles .gt. mxciofiles) goto 3010
      if (nsfiles .gt. 1) then
        read(iounit, end = 3010, err = 3010)
     +                (((noplowp(j, k, i), j = 1, 5), k = 1, kmax),
     +                                     i = 2, nsfiles)
        read(iounit, end = 3010, err = 3010)
     +                ((nophighp(j, i), j = 1, 5), i = 2, nsfiles)
        read(iounit, end = 3010, err = 3010)
     +                (((eoplowp(j, k, i), j = 1, 5), k = 1, kmax),
     +                                  i = 2, nsfiles)
        read(iounit, end = 3010, err = 3010)
     +                ((eophighp(j, i), j = 1, 5), i = 2, nsfiles)
      endif
c
      return
c
c     Error exit
c
 3010 continue
      rc = 66
      return
c
      end
c     --- End of routine ciostatread
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciobufread(iounit, nbuf, rc)
c
c     Reading the particle buffers associated with compressed I/O
c     files.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2002.
c
c
c     Arguments:
c     =========
c
c     iounit......... (input, integer) The i/o unit number connected
c                     with the IDF file. It points to an already
c                     opened unformatted file. The unit must not be
c                     closed.
c     nbuf........... (input, integer) The number of buffers to read.
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the skipping operation is completed
c                     successfully. Nonzero otherwise.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
c     include 'pstackpar.f'      (Included by 'pbuffpar.f')
c     include 'ciopar.f'         (Included by 'pbuffpar.f')
      include 'pbuffpar.f'
c
c     Declaration of arguments.
c
      integer           iounit, nbuf, rc
c
c     Declaration of shared data.
c
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, nsfiles
c
c     FIRST EXECUTABLE STATEMENT
c
      rc = 0
c
      if (nbuf .le. 0) return
c
c     Reading the ground particle buffer.
c
      read(iounit, end = 3010, err = 3010) npgndbuff
      if (npgndbuff .gt. 0) then
        read(iounit, end = 3010, err = 3010)
     +    ((fgndbuff(j, i), j = 1, maxstalen), i = 1, npgndbuff)
        read(iounit, end = 3010, err = 3010)
     +    (gndbuffsta(i), i = 1, npgndbuff)
      endif
c
      if (nbuf .le. 1) return
c
c     Reading the particle buffers associated with other compressed
c     files.
c
      read(iounit, end = 3010, err = 3010) nsfiles
      if (nsfiles .gt. mxciofiles) goto 3010
      if (nsfiles .gt. 1) then
        read(iounit, end = 3010, err = 3010)
     +       (npopbuff(k), k = 2, nsfiles)
        do k = 2, nsfiles
          l = npopbuff(k)
          if (l .gt. 0) then
            read(iounit, end = 3010, err = 3010)
     +           (opbuffsta(i, k), i = 1, l)
            read(iounit, end = 3010, err = 3010)
     +           ((opbuffiaux(j, i, k), j = 1, nopauxi), i = 1, l)
            read(iounit, end = 3010, err = 3010)
     +           ((opbufffaux(j, i, k), j = 1, nopauxf), i = 1, l)
            read(iounit, end = 3010, err = 3010)
     +           ((fopbuff(j, i, k), j = 1, maxstalen), i = 1, l)
          endif
        enddo
      endif
c
      return
c
c     Error exit
c
 3010 continue
      rc = 66
      return
c
      end
c     --- End of routine ciobufread
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'idfread.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
