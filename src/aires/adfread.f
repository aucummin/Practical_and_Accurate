c
c     FILE: adfread.f                       CREATION DATE: 12/APR/1999.
c                                       LAST MODIFICATION: 25/FEB/2004.
c
c     Routines to restore internal data. Can be used only for summary
c     purposes.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine adfread(filename, vrb, irc)
c
c     Reading the ASCII dump file.
c
c     Written by: S. J. Sciutto, Fermilab 1999;
c                                La Plata 1999, 2000, 2001, 2002, 2003,
c                                2004.
c
c
c     Arguments:
c     =========
c
c
c
c     filename........ (input, character*(*)) The name of the file to
c                      open. If filename is blank, then the input
c                      filename is set internally accordingly with
c                      the current task name.
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
      integer           vrb, irc
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
      integer           errsev, iiver, fnlen
      logical           errprt
      integer           i, j, k, l, m, i0, i1, i2, lsh, intut
      logical           sryprog
      integer           srysev
      integer           nwfstacks1
      integer           itmp0, itmp1, itmp2, itmp3, itmp4, itmp5
      integer           imxcdl, xmaxncode
      integer           kshz, kfields, rfields, kfnow, rfnow, savedbuf
      integer           xnpstacks
c
c     adf header related variables.
c
      character*32      adfheader
      integer           idfintheader
c
c     Previous version compatibility related data.
c
      integer           nprevv
      parameter         (nprevv = 6)
      logical           afterprev(nprevv)
c
      logical           after201, after215, after232, after23A
      logical           after242, after269
c
      equivalence       (after201, afterprev( 1))
      equivalence       (after215, afterprev( 2))
      equivalence       (after232, afterprev( 3))
      equivalence       (after23A, afterprev( 4))
      equivalence       (after242, afterprev( 5))
      equivalence       (after269, afterprev( 6))
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
        auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // adfext
        fnlen         = leadingfnlen(2) + 4
      else
        call strimcopy(filename, len(filename), auxfilestring, fnlen)
      endif
c
      irc = 115
      close(10, err = 3010)
      open(10, file = auxfilestring, status = 'OLD',
     +         err = 3010)
      irc = 105
c
      sryprog = ((pgmcode .ge. 2000) .and. (pgmcode .lt. 3000))
      if (sryprog) then
        srysev = 2
      else
        srysev  = 1
      endif
c
c     READING THE DATA.
c
c     File header.
c
      read(10, 2010, end = 3010, err = 3010) adfheader, idfintheader,
     +                                       idfversion
 2010 format(a32, i11, a24)
c
c     Checking the validity of the file parameters.
c
      if ((adfheader .ne. adfheader0) .or.
     +    (idfintheader .ne. idfintheader0)) then
        if (errprt) then
          call errprint(0, '*', errsev, 'adfread',
     +      'This file seems not to be a valid AIRES adf file.',
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
            call errprint(0, '$DF1', errsev, 'adfread',
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
            call errprint(0, '$DF2', errsev, 'adfread',
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
            call errprint(0, '*', srysev, 'adfread',
     +           'This adf file was not written with the current ' //
     +           'AIRES version$File version   : ' // idfversion //
     +           '$Current version: ' // aires_version,
     +           0, 0, 0, 0.d0, auxfilestring(1:fnlen))
c
            if (iiver .lt. 01070200) then
              call errprint(0, '*', errsev, 'adfread',
     +        'File format not supported.',
     +        0, 0, 0, 0.d0, ' ')
              return
            endif
          endif
c
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
      read(10, 2020, end = 3010, err = 3010)
     +         original_version, nverchanges, oversionset
 2020 format(a24, i11, l3)
      read(10, 2022, end = 3010, err = 3010)
     +         idfcdate, idfcwho, idfcstar
 2022 format(a11, a48, a8)
c
      read(10, 2030, end = 3010, err = 3010)
     +         idfcruserlen, idfcrhostlen, idfcruser, idfcrhost
 2030 format(2i11, a24, a64)
c
c     Skipping record of zeros (spare fields for future use).
c     First record is used now to label number of stacks for
c     weight limiting factors.
c     Second field is the number of dump files used to build
c     the merged adf file. It is zero for normal (non merged)
c     files.
c
      read(10, *, end = 3010, err = 3010)
     +             nwfstacks1, adfnmergedfiles,
     +             (i, j = 3, 10)
c
c     Reading basic data.
c
 2120 format(66l2)
c
      read(10, 2150, end = 3010, err = 3010) (datistr0(i), i = 1, 3)
 2150 format(3a20)
      read(10, *, end = 3010, err = 3010) (cpu0(i), i = 1, 3),
     +                                    shinidati
      read(10, *, end = 3010, err = 3010) wdirnamelen(1), tasknamelen,
     +                                    tasknamever, tasknameset
      if (wdirnamelen(1) .gt. 0)
     +   read(10, 2200, end = 3010, err = 3010)
     +                   wdirname(1)(1:wdirnamelen(1))
      if (tasknamelen .gt. 0)
     +   read(10, 2200, end = 3010, err = 3010) taskname(1:tasknamelen)
c
 2200 format(4a)
c
      read(10, *, end = 3010, err = 3010)
     +                mtotshowers, runshowers, processjobs,
     +                lastshower, currshower,
     +                processnumber, jobnumber
      read(10, *, end = 3010, err = 3010)
     +                totshwset, runshwset, cpurunset, processjobsset,
     +                stopchsize1, stopchsize2
      read(10, *, end = 3010, err = 3010) cpuperrun, stopchsize3
      read(10, 2120, end = 3010, err = 3010)
     +                usedefaultd, usedefaults, usedefault,
     +                shcomplete
c
      read(10, *, end = 3010, err = 3010) nshprimary, pryeminset
      if (nshprimary .gt. 0) then
        do i = 1, nshprimary
          read(10, *, end = 3010, err = 3010)
     +                 shprimary(i), shprimarywt(i), shprimarywt0(i)
        enddo
      endif
c
      read(10, *, end = 3010, err = 3010)
     +             pryenergymin, currethin,
     +             (thinmaxw(i + 1), i = 0, nwfstacks1),
     +             thinmarker
c
      read(10, *, end = 3010, err = 3010) atmoslabel, atmoslset
c
c     Skipping record of zeros (spare fields for future use),
c     and reading the first shower number.
c
      read(10, *, end = 3010, err = 3010)
     +             firstshowernon1, firstshowerset,
     +             (i, j = 1, 8)
c
c     Random number generators shared data.
c
      read(10, *, end = 3010, err = 3010) i
      if (i .gt. 0) then
        read(10, *, end = 3010, err = 3010) auxxran, auxxran2,
     +                                      auxoriginal_seed
      else
        read(10, *, end = 3010, err = 3010) auxxran, auxxran2
        auxoriginal_seed = 0
      endif
c
c     Main input data arrays.
c
      read(10, *, end = 3010, err = 3010)
     +             nfidata, niidata, nlidata,
     +             nsidata, sidatalen,
     +            (i, j = 1, 6)
      read(10, *, end = 3010, err = 3010)
     +             nfidata0, niidata0, nlidata0,
     +             nsidata0, sidatalen0,
     +             (i, j = 1, 6)
c
      if ((nfidata .gt. mxfidata) .or.
     +    (niidata .gt. mxiidata) .or.
     +    (nlidata .gt. mxlidata) .or.
     +    (nsidata .gt. mxsidata) .or.
     +    (sidatalen .gt. mxsil)       ) goto 3010
c
      if (nfidata .gt. 0) then
        do i = 1, nfidata
          read(10, *, end = 3010, err = 3010)
     +                 (fidata(j, i), j = 0, 3), 
     +                 fidatawaset(i), fdbdry(i)
        enddo
      endif
c
      if (niidata .gt. 0) then
        do i = 1, niidata
          read(10, *, end = 3010, err = 3010)
     +                 (iidata(j, i), j = 0, 3),
     +                 iidatawaset(i), idbdry(i)
        enddo
      endif
c
      if (nlidata .gt. 0) then
        do i = 1, nlidata
          read(10, 2240, end = 3010, err = 3010)
     +                    (lidata(j, i), j = 0, 2), 
     +                    lidatawaset(i)
        enddo
 2240   format(3l3, i12)
      endif
c
      if (nsidata .gt. 0) then
        do i = 1, nsidata
          read(10, *, end = 3010, err = 3010)
     +                 (sidata(j, i), j = 0, 6),
     +                 sidatawaset(i)
        enddo
        read(10, 2200, end = 3010, err = 3010)
     +                  sidatastring(1:sidatalen)
      endif
c
      if (.not. after269) then
        thinningon = .true.
      endif
c
c     Main output data arrays.
c
      read(10, *, end = 3010, err = 3010) nfodata, niodata, nlodata,
     +                                    nfodata0, niodata0, nlodata0
c
      if ((nfodata .gt. mxfodata) .or.
     +    (niodata .gt. mxiodata) .or.
     +    (nlodata .gt. mxlodata))      goto 3010
c
      if (nfodata .gt. 0) then
        do i = 1, nfodata
          read(10, 2250, end = 3010, err = 3010)
     +              (fodata(j, i), j = 1, 5),
     +              fosamples(i), foupdate(i)
        enddo
 2250   format(5g16.8, i11, l3)
      endif
c
      if (niodata .gt. 0) then
        do i = 1, niodata
          read(10, 2260, end = 3010, err = 3010)
     +              (iodata(j, i), j = 1, 3), 
     +              iosamples(i), ioupdate(i)
        enddo
 2260   format(4i11, l3)
      endif
c
      if (nlodata .gt. 0) then
        read(10, 2120, end = 3010, err = 3010)
     +                  (lodata(i), i = 1, nlodata)
      endif
c
c     Observing levels data.
c
      read(10, *, end = 3010, err = 3010)
     +             (iobslevdat(i), i = 0, 1), obslevset, nobslevelsp1,
     +             obslevl0
      read(10, *, end = 3010, err = 3010)
     +             ((fobslevdat(i, j), i = 0, 1), j = 1, 2)
      read(10, *, end = 3010, err = 3010)
     +             obslevstep, obslevminz, obslevmaxz, obslevca,
     +             obslevcb
c
      if (nobslevelsp1 .gt. mxobslevelsp1) goto 3010
c
c     Markers for observing levels that are recorded into the
c     longitudinal file(s).
c
      if (nobslevels .gt. 0) then
        read(10, 2120, end = 3010, err = 3010)
     +                  (olfilesave(i), i = 1, nobslevels)
      endif
c
c     Some basic shower parameters.
c
      read(10, *, end = 3010, err = 3010) primcode
      read(10, *, end = 3010, err = 3010)
     +             initshdir, currshdir, cinjz, showert0
      read(10, *, end = 3010, err = 3010) primfdata
c
      read(10, *, end = 3010, err = 3010)
     +             dtoplost, hcosinedummy,
     +             hrmaxlx, hrmaxly, hrminlx, hrminly
c
      if (nobslevelsp1 .gt. 0) then
        do j = 1, nobslevelsp1
          read(10, *, end = 3010, err = 3010)
     +                 (obslevcore(i, j), i = 1, 5),
     +                 obslevt0(j)
        enddo
      endif
c
      read(10, 2280, end = 3010, err = 3010)
     +                shgbx, shgbz, emagfac, shgbon
2280  format(3g16.8, l3)
c
c     Shower maximum data.
c
      read(10, *, end = 3010, err = 3010) shxsamples, itmp1
      do j = 1, itmp1
        read(10, *, end = 3010, err = 3010) (shxmax(i, j), i = 1, 5)
      enddo
      read(10, *, end = 3010, err = 3010) (shnmax(i), i = 1, 5)
c
c     Table (histogram) data (including some spare positions).
c
      read(10, *, end = 3010, err = 3010)
     +         nttabins, nttabinsp1, maxtabcode,
     +         nlhtables, nlhtable1,
     +         nldtables, nldtable1,
     +         ntdtables, ntdtable1,
     +         npshtables, npshtable1,
     +         nlitables, nlitable1,
     +         (i, j = 1, 2),
     +         itmp2, itmp3
c
      if (nttabins   .gt. tdbins)     goto 3010
      if (nlhtables  .gt. mxlhtable)  goto 3010
      if (nldtables  .gt. mxldtable)  goto 3010
      if (ntdtables  .gt. mxtdtable)  goto 3010
      if (npshtables .gt. mxpshtable) goto 3010
      if (nlitables  .gt. mxlitable)  goto 3010
c
c     Longitudinal tables.
c
      do k = 1, nlhtables
        do i = 1, nobslevelsp1
          read(10, *, end = 3010, err = 3010)
     +                 (lhistn(j, i, k), j = 1, 5)
        enddo
      enddo
c
      do k = 1, nlhtables
        do i = 1, nobslevelsp1
          read(10, *, end = 3010, err = 3010)
     +                 (lhiste(j, i, k), j = 1, 5)
        enddo
      enddo
c
c     Unweighted longitudinal tables are read conditionally
c
      if (itmp2 .gt. 0) then
        do k = 1, nlhtables
          do i = 1, nobslevelsp1
            read(10, *, end = 3010, err = 3010)
     +                   (wlhistn(j, i, k), j = 1, 5)
          enddo
        enddo
      endif
c
      if (after232) then
c
        do k = 1, nlhtables
          do i = 1, nobslevelsp1
            read(10, *, end = 3010, err = 3010)
     +                   (lhistn(j, i, k + mxlhtable), j = 1, 5)
          enddo
        enddo
c
        do k = 1, nlhtables
          do i = 1, nobslevelsp1
            read(10, *, end = 3010, err = 3010)
     +                   (wlhistn(j, i, k + mxlhtable), j = 1, 5)
          enddo
        enddo
c
      else
c
        do k = 1, nlhtables
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
      do k = 1, nldtables
        do i = 0, nttabinsp1
          read(10, *, end = 3010, err = 3010)
     +                 (rthistn(j, i, k), j = 1, 5)
        enddo
      enddo
c
      do k = 1, nldtables
        do i = 0, nttabinsp1
          read(10, *, end = 3010, err = 3010)
     +                 (rthiste(j, i, k), j = 1, 5)
        enddo
      enddo
c
c     Unweighted tables are read conditionally
c
      if (itmp3 .gt. 0) then
c
        do k = 1, nldtables
          do i = 0, nttabinsp1
            read(10, *, end = 3010, err = 3010)
     +                   (wrthistn(j, i, k), j = 1, 5)
          enddo
        enddo
c
        do k = 1, nldtables
          do i = 0, nttabinsp1
            read(10, *, end = 3010, err = 3010)
     +                   (wrthiste(j, i, k), j = 1, 5)
          enddo
        enddo
c
      endif
c
c     Time distribution tables.
c
      do k = 1, ntdtables
        do i = 0, nttabinsp1
          read(10, *, end = 3010, err = 3010) rtsampl(i, k)
          read(10, *, end = 3010, err = 3010)
     +                 (rthistt(j, i, k), j = 1, 5)
        enddo
      enddo
c
c     Deposited energy and related tables are read conditionally.
c
      if (nlitables .gt. 0) then
c
        do l = 0, mxlitable, mxlitable
          do k = 1, nlitables
            do i = 1, nobslevelsp1
              read(10, *, end = 3010, err = 3010)
     +                     (llhistn(j, i, k + l), j = 1, 5)
            enddo
          enddo
          do k = 1, nlitables
            do i = 1, nobslevelsp1
              read(10, *, end = 3010, err = 3010)
     +                     (llhiste(j, i, k + l), j = 1, 5)
            enddo
          enddo
          do k = 1, nlitables
            do i = 1, nobslevelsp1
              read(10, *, end = 3010, err = 3010)
     +                     (wllhistn(j, i, k + l), j = 1, 5)
            enddo
          enddo
        enddo
c
        do k = 1, nlitables
          do i = 1, nobslevelsp1
            read(10, *, end = 3010, err = 3010)
     +                   (lihiste(j, i, k), j = 1, 5)
          enddo
        enddo
c
      endif
c
c     Input and output data associated variables (Directive names,
c     etc.)
c
      read(10, *, end = 3010, err = 3010)
     +         ncommands, ncommands0, imxcdl,
     +         nallodata, nallodata0, ngrdprint, lallcht
c
      if ((imxcdl .le. 0) .or. (imxcdl .gt. mxcdl)) goto 3010
c
c     "adfwrite" is always used in summary or upgrade programs,
c     therefore names must be read from file.
c
      if (ncommands .gt. 0) then
        do i = 1, ncommands
          read(10, 2310, end = 3010, err = 3010)
     +                    clgname(i), cname(i)(1:imxcdl),
     +                    ccode(i), minclen(i), aditem(i),
     +                    veryimpdir(i), wngonset(i)
 2310     format(a27, a, 3i11, 2l3)
        enddo
      endif
c
      if (nallodata .gt. 0) then
        do i = 1, nallodata
          read(10, 2320, end = 3010, err = 3010)
     +                    odataname(i),
     +                    odatatype(i), odataitem(i),
     +                    veryimpodata(i)
 2320     format(a27, 2i11, l3)
        enddo
      endif
c
c     Reading table names and related data.
c
      do i = 1, nlhtables
        read(10, 2332, end = 3010, err = 3010)
     +                  lhnamen(i), lhpclen(i), lhcoden(i)
        read(10, 2330, end = 3010, err = 3010)
     +                  lhnamee(i), lhcodee(i)
        read(10, 2330, end = 3010, err = 3010)
     +                  wlhnamen(i), wlhcoden(i)
      enddo
 2330 format(a64, i11)
 2332 format(a64, a16, i11)
c
      if (ngrdprint .gt. 0) then
        do i = 1, ngrdprint
          read(10, *, end = 3010, err = 3010)
     +                 grdporder(i), grdpspa(i)
        enddo
      endif
c
      do i = 1, nldtables
        read(10, 2330, end = 3010, err = 3010) ldnamen(i), ldcoden(i)
        read(10, 2330, end = 3010, err = 3010) ldnamee(i), ldcodee(i)
        read(10, 2330, end = 3010, err = 3010) wldnamen(i), wldcoden(i)
        read(10, 2330, end = 3010, err = 3010) wldnamee(i), wldcodee(i)
      enddo
c
      do i = 1, ntdtables
        read(10, 2330, end = 3010, err = 3010) tdnamen(i), tdcoden(i)
      enddo
c
      do i = 1, npshtables
        read(10, 2330, end = 3010, err = 3010) pshname(i), pshcode(i)
      enddo
c
      if (nlitables .gt. 0) then
        do i = 1, nlitables
          read(10, 2330, end = 3010, err = 3010)
     +                    llnamen(i), llcoden(i)
          read(10, 2330, end = 3010, err = 3010)
     +                    llnamee(i), llcodee(i)
          read(10, 2330, end = 3010, err = 3010)
     +                    wllnamen(i), wllcoden(i)
          read(10, 2330, end = 3010, err = 3010)
     +                    linamee(i), licodee(i)
        enddo
      endif
c
      nttabinsp2  = nttabins + 2
c
c     Skipping record of zeros (spare fields for future use).
c     First element is now used to store the size of the site library.
c     Second element is now used to store the number of "special"
c     particles that were defined.
c     Third and fourth elements are now used to store the numbers
c     of global variables (dynamic and static, respectively).
c
      read(10, *, end = 3010, err = 3010)
     +             nlibsites, nescpcles,
     +             nglobar(1), nglobar(2),
     +             (i, j = 5, 10)
c
c     Reading the site library.
c
      if (nlibsites .gt. 0) then
        do i = 0, nlibsites
          read(10, 2340, end = 3010, err = 3010)
     +                    sitename(i), sitenlen(i),
     +                    sitelat(i), sitelong(i), 
     +                    siteground(i)
        enddo
 2340   format(a16, i11, 3g16.8)
      endif
c
c     Reading data related with "Special" particles.
c
      if (nescpcles .gt. 0) then
c
        read(10, *, end = 3010, err = 3010)
     +               lastescpcle, itmp0, itmp1, itmp2, itmp3
        if (nescpcles .gt. itmp0)   goto 3010
        if (lastescpcle .gt. itmp2) goto 3010
        if (itmp1 .ne. pescode1)    goto 3010
        escmacropos(4, 0) = 0
        do i = 1, nescpcles
          read(10, 2350, end = 3010, err = 3010)
     +                    escpclename(i), escmacrover(i),
     +                    escmacrouse(i),
     +                    (escmacropos(j, i), j = 1, 4),
     +                    (nsprimpart(j, i), j = 1, 3)
        enddo
 2350   format(a16, 9i11)
        if (escmacropos(4, nescpcles) .gt. espms) goto 3010
        read(10, 2200, end = 3010, err = 3010)
     +            escpclemacro(1:escmacropos(4, nescpcles))
c
        read(10, *, end = 3010, err = 3010)
     +               nintintvar, nintfltvar, j, i, i
        recordspvar0 = (j .eq. 1)
        read(10, *, end = 3010, err = 3010)
     +               itmp1, itmp2, i, i, i
c
        if (nintintvar .gt. 0) then
          if (nintintvar .gt. mxintintvar) goto 3010
          read(10, *, end = 3010, err = 3010)
     +                 (spintvar(i), i = 1, nintintvar)
        endif
        if (nintfltvar .gt. 0) then
          if (nintfltvar .gt. mxintfltvar) goto 3010
          read(10, *, end = 3010, err = 3010)
     +                 (spfltvar(i), i = 1, nintfltvar)
        endif
c
      endif
c
c     Reading global variables.
c
      do j = 1, 2
        if (nglobar(j) .gt. 0) then
          if (nglobar(j) .gt. mxglobar) goto 3010
          read(10, 2360, end = 3010, err = 3010)
     +                    (globnam(i, j)(1:imxcdl), globlen(i, j),
     +                    i = 1, nglobar(j))
 2360     format(a, i11)
          read(10, *, end = 3010, err = 3010)
     +                 globstrlen(j),
     +                 (globdfend(i, j), i = 0, nglobar(j))
          if (globstrlen(j) .gt. 0) then
            if (globstrlen(j) .gt. mxgll) goto 3010
            read(10, 2200, end = 3010, err = 3010)
     +                      globarstring(j)(1:globstrlen(j))
          endif
        endif
      enddo
c
      thereareglobars = ((nglobar(1) + nglobar(2)) .gt. 0)
c
c     No model specific variables are saved in the ADF file.
c
c     Recovering the cio buffers and related variables.
c     Entering cio information for non-simulation programs
c     (e.g. summary programs).
c
      read(10, *, end = 3010, err = 3010) nhwciofiles, nciofiles,
     +                                    nciofilesu, itmp1
      savedbuf = nhwciofiles
      if (itmp1 .gt. mxciofiles) goto 3010
c
      if (nciofilesu .gt. 0) then
        read(10, *, end = 3010, err = 3010)
     +               (ciofilesu(i), i = 1, nciofilesu)
        do i = 1, nciofilesu
          j = ciofilesu(i)
          if (after23A) then
            read(10, 2410, end = 3010, err = 3010)
     +                      pofilext(j), pofileno(j),
     +                      cioreclast(j), lastciopointer(i),
     +                      ciorlastcp(j), ciowblocks(j)
          else
            read(10, 2410, end = 3010, err = 3010)
     +                      pofilext(j), pofileno(j), cioreclast(j)
          endif
          if (cioreclast(j) .gt. 0) then
            read(10, 2200, end = 3010, err = 3010)
     +                      ciorecord1(j)(1:cioreclast(j))
          endif
        enddo
 2410   format(a12, 5i11)
      endif
c
      if (nciofiles .gt. 0) then
        if (after242) then
          xmaxncode = maxncode
        else
          xmaxncode =  nuccod0 + nuccod1 * 26 + nuccod1 - 1
        endif
        do i = 1, nciofiles
          read(10, *, end = 3010, err = 3010) nrectypes(i)
          do j = 0, nrectypes(i)
            if (after269) then
              read(10, *, end = 3010, err = 3010) itmp3, itmp4, itmp5
            else
              read(10, *, end = 3010, err = 3010) itmp3
              itmp4 = 0
              itmp5 = itmp3
            endif
            totrecfields(j, i) = itmp3
            ciodynfield(j, i)  = itmp4
            ciodynfwcix(j, i)  = itmp5
            itmp5 = max(itmp3, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofminv(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofmaxv(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofwsc0(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofwsc1(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofrsc0(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofrsc1(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofwca(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofwcb(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofrca(k, j, i), k = 1, itmp5)
            read(10, *, end = 3010, err = 3010)
     +                   (ciofrcb(k, j, i), k = 1, itmp5)
          enddo
c
c         Reading the specifications on which particles are to be
c         saved in the corresponding cio file.
c
          read(10, 2120, end = 3010, err = 3010) anypinfile(i)
          read(10, 2120, end = 3010, err = 3010)
     +                    (allpclesave(k, i), k = -maxpcle, xmaxncode)
          do k = xmaxncode + 1, maxncode
            allpclesave(k, i) = .false.
          enddo
c
        enddo
      endif
c
      if (nhwciofiles .gt. 0) then
        read(10, *, end = 3010, err = 3010)
     +    (ciofiles(i), i = 1, nhwciofiles)
      endif
c
c     Reading statistical variables related with cio particle buffers.
c
      call ciostatreadadf(10, savedbuf, after201, irc)
      if (irc .ne. 0) goto 3010
c
c     Stack names, processing modes, etc.
c
      read(10, *, end = 3010, err = 3010) xnpstacks, minpstack
c
      if (xnpstacks .gt. npstacks) goto 3010
c
      read(10, *, end = 3010, err = 3010)
     +             (psta_mode(i), i = 1, xnpstacks)
      read(10, 2200, end = 3010, err = 3010)
     +                (psta_n(i), i = 0, xnpstacks)
      read(10, 2200, end = 3010, err = 3010)
     +                (psta_model_n(i), i = 1, xnpstacks)
      read(10, 2430, end = 3010, err = 3010)
     +                ((psta_rn(j, i), j = 1, 2), i = 1, xnpstacks)
 2430 format(8a)
c
c     Stack statistical data and related variables
c
      read(10, *, end = 3010, err = 3010)
     +             (avgtotsize(i), i = 1, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((peakstsize(j, i), j = 1, 3), i = 1, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((procentries(j, i), j = 1, 3), i = 1, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((hardswrite(j, i), j = 1, 3), i = 1, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((hardsread(j, i), j = 1, 3), i = 1, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((callcounter(j, i), j = 1, 2), i = 0, xnpstacks)
c
      read(10, *, end = 3010, err = 3010)
     +             ((totpcles(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((eloss(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((nplost(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((elost(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((nplowe(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((elowe(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((nprgnd(j, i), j = 1, 5), i = 0, xnpstacks)
      read(10, *, end = 3010, err = 3010)
     +             ((eprgnd(j, i), j = 1, 5), i = 0, xnpstacks)
c
c     Information about unphysical particles, first interaction, etc.
c
      read(10, *, end = 3010, err = 3010) (nnotap(j), j = 1, 5)
      read(10, *, end = 3010, err = 3010) (enotap(j), j = 1, 5)
      read(10, *, end = 3010, err = 3010) (nneutrino(j), j = 1, 5)
      read(10, *, end = 3010, err = 3010) (eneutrino(j), j = 1, 5)
      read(10, *, end = 3010, err = 3010) (aveprim(j), j = 1, 5)
c
      do j = 1, 2
        read(10, *, end = 3010, err = 3010)
     +               (fstintdp(i, j), i = 1, 5)
      enddo
c
      read(10, *, end = 3010, err = 3010)
     +             (fstposdp(i), i = 1, 5), fstdpmanual
      if (after215) then
        read(10, 2120, end = 3010, err = 3010)
     +                  fstintnotset, fstintauto, specialprim
        read(10, *, end = 3010, err = 3010)
     +               specialprimlab, nspecialprim
      else
        read(10, 2120, end = 3010, err = 3010)
     +                  fstintnotset, fstintauto
        specialprim  = .false.
        nspecialprim = 0
      endif
c
c     Skipping record of zeros (spare fields for future use).
c     First field is now used for shower maximum data file.
c
      read(10, *, end = 3010, err = 3010) kshz, (i, j = 1, 9)
c
c     READING DATA TO BE STORED IN OTHER INTERNAL FILES.
c
c     Remarks.
c
      read(10, 2120, end = 3010, err = 3010) remark
      if (remark) then
c
c       Opening the remarks file.
c
        intut = rmkut
        open(rmkut, file = rmkfn, status = 'UNKNOWN',
     +              form = 'UNFORMATTED', err = 3020)
c
 1010   continue
        read(10, *, end = 3010, err = 3010) i
        if (i .eq. -99999) goto 1020
        write(rmkut, err = 3020) i
c
        if (i .gt. 0) then
          read(10, 2200, end = 3010, err = 3010) auxline(1:i)
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
        read(10, *, err = 3010, end = 3010)
     +               i0, i1, lsh, kfields, rfields,
     +               (obslevscrai(j), j = 21, 26)
c
        pershowertables = (i0 .ge. 1000000)
        saveshowerdata  = (rfields .gt. 0)
c
        if (pershowertables) then
          j = 31
          do i = 1, obslevscrai(21)
            read(10, *, err = 3010, end = 3010)
     +                   (obslevscrai(j + l), l = 1, 10)
            j = j + 10
          enddo
          read(10, *, err = 3010, end = 3010) l
        endif
c
      else if (kshz .eq. 4) then
c
c       Previous format with brief header.
c
        read(10, *, end = 3010, err = 3010)
     +               i0, i1, lsh, kfields, rfields
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
        lsh             = nlhtables
        kfields         = 1   
        rfields         = 2 * nlhtables + 4
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
      rfnow = nshff * nlhtables + i1 + 6
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
            read(10, *, end = 3010, err = 3010)
     +                   (obslevscraj(k), k = 1, kfields),
     +                   (obslevscra1(k), k = 1, rfields)
            write(shzut, err = 3020)
     +                   (obslevscraj(k), k = 1, kfnow),
     +                   (obslevscra1(k), k = 1, rfnow)
            read(10, *, err = 3010, end = 3010)
     +                   (obslevscrai(k), k = 1, 10)
            write(shzut, err = 3020) (obslevscrai(k), k = 1, 10)
c
c           Tables.
c
            j = 31
            do m = 1, obslevscrai(21)
              i1 = obslevscrai(j + 3)
              i2 = obslevscrai(j + 4)
              do k = 1, obslevscrai(j + 1)
                read(10, *, end = 3010, err = 3010)
     +                       (obslevscra4(l), l = i1, i2)
                write(shzut, err = 3020) (obslevscra4(l), l = i1, i2)
              enddo
              j = j + 10
            enddo
c
c           End shower mark.
c
            read(10, *, end = 3010, err = 3010) i1
            write(shzut, err = 3010) i1
c
          enddo
c
        else
c
          do i = 1, lastshower
            read(10, *, end = 3010, err = 3010)
     +                   (obslevscraj(k), k = 1, kfields),
     +                   (obslevscra1(k), k = 1, rfields)
            write(shzut, err = 3020)
     +                   (obslevscraj(k), k = 1, kfnow),
     +                   (obslevscra1(k), k = 1, rfnow)
          enddo
c
        endif
      endif
c
c     Reading trailing record.
c
      if (lastshower .gt. 0) then
c
        read(10, *, end = 3010, err = 3010) lsh
c
        if (lsh .ne. lastshower) then
          if (errprt) then
            call errprint(0, '*', 2, 'adfread',
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
c
      read(10, *, end = 3010, err = 3010) (i, j = 1, 10)
c
c     No more data is stored in the ADF file.
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
        call errprint(0, '$A30', errsev, 'adfread', ' ',
     +                1, 10, 0, 0.d0,
     +                auxfilestring(1:fnlen))
      endif
      irc = 97
      return
c
 3020 continue
c
      if (errprt) then
        call errprint(0, '$A09', errsev, 'adfread',
     +                ' ', 1, intut, 0, 0.d0, ' ')
      endif
      irc = 107
      return
c
      end
c     --- End of routine adfread
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciostatreadadf(iounit, nbuf, longlow, rc)
c
c     Saving the statistical counters related with the cio particle
c     buffers.
c
c     Written by: S. J. Sciutto, Fermilab 1999.
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
      read(iounit, *, end = 3010, err = 3010)
     +              ((ngndlowp(j, k), j = 1, 5), k = 1, kmax)
      read(iounit, *, end = 3010, err = 3010) ngndhighp
      read(iounit, *, end = 3010, err = 3010)
     +              ((egndlowp(j, k), j = 1, 5), k = 1, kmax)
      read(iounit, *, end = 3010, err = 3010) egndhighp
c
      if (nbuf .lt. 1) return
c
c     Reading statistical data associated with the other cio particle
c     buffers.
c
      read(iounit, *, end = 3010, err = 3010) nsfiles
      if (nsfiles .gt. mxciofiles) goto 3010
      if (nsfiles .gt. 1) then
        read(iounit, *, end = 3010, err = 3010)
     +                (((noplowp(j, k, i), j = 1, 5), k = 1, kmax),
     +                                     i = 2, nsfiles)
        read(iounit, *, end = 3010, err = 3010)
     +                ((nophighp(j, i), j = 1, 5), i = 2, nsfiles)
        read(iounit, *, end = 3010, err = 3010)
     +                (((eoplowp(j, k, i), j = 1, 5), k = 1, kmax),
     +                                  i = 2, nsfiles)
        read(iounit, *, end = 3010, err = 3010)
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
c     --- End of routine ciostatreadadf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'adfread.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
