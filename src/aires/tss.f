c
c     FILE: tss.f                           Creation date: 23/NOV/2001.
c                                       LAST MODIFICATION: 20/AUG/2003.
c
c     Routines related with the "task summary script" file, that
c     contains a summary of task information in a format suitable
c     for parsing from another program or script.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine tssfile
c
c     Writing a task summary script file.
c
c     Written by: S. J. Sciutto, La Plata 2001, 2002; Fermilab 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      character*20      tssdati
      integer           flen1, slen1
      character*64      tssauxstr
      integer           showerno
      integer           klast, ngt, kfields, rfields
      integer           ipars(5)
      integer           fitrc, shprcode
      equivalence       (fitrc, ipars(1))
      equivalence       (shprcode, ipars(2))
      double precision  xpars(5), sumofsq
      integer           kx0, klambda, knmax, kxmaxv
      integer           iprimx1v, iprimegy, iprimzen, iprimazi
c
c     FIRST EXECUTABLE STATEMENT
c
c     PREPARING THE OUTPUT SUMMARY FILES.
c
      call dati(tssdati)
c
c     Opening the tss file.
c
      auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // tssext
      flen1         = leadingfnlen(2) + slen1 + 4
c
      open(8, file = auxfilestring, status = 'UNKNOWN',
     +        access = 'SEQUENTIAL', err = 3010)
c
c     Writing the file header.
c
      call writetsshead(8)
c
      auxline =  'TSS file for task ' // taskname(1:tasknamelen)
      slen1 = tasknamelen + 18
      if (tasknamever .gt. 0) then
        write(auxline(slen1+1:slen1+4), 2010) '_', tasknamever
 2010   format(a, i3.3)
        slen1 = slen1 + 4
      endif
      call writetsscomm(8, auxline(1:slen1))
c
c     Program version.
c
      call writetsscomm(8, 'Program and compilation parameters.')
      call writetssstr(8, 'AiresVersion', aires_version)
c
      if (sryprog) then
c
        call writetssstr(8, 'DumpFileAuthor',
     +                   idfcruser(1:idfcruserlen) // '@' //
     +                   idfcrhost(1:idfcrhostlen))
c
        if (nverchanges .gt. 0) then
          call writetssstr(8, 'OriginalAiresVersion', original_version)
          if (idfvnecurrent) then
            call writetssstr(8, 'DumpFileVersion', idfversion)
          endif
        else if (idfvnecurrent) then
          call writetssstr(8, 'OriginalAiresVersion', idfversion)
        endif
c
        call writetssint(8, 'NumberOfMergedDumpFiles',
     +                   adfnmergedfiles, 0)
c
      endif
c
      call writetsscomm(8, 'Units')
      call writetssstr(8,         'LengthUnit',      'm')
      call writetssstr(8,           'TimeUnit',    'sec')
      call writetssstr(8,         'EnergyUnit',    'GeV')
      call writetssstr(8,          'DepthUnit',  'g/cm2')
      call writetssstr(8,          'AngleUnit',    'deg')
      call writetssstr(8,  'MagneticFieldUnit',     'nT')
c
c     General data and run control.
c
      call writetsscomm(8, 'General data')
      call writetssstr(8, 'TaskName', taskname(1:tasknamelen))
      call writetssint(8, 'TaskVersion', tasknamever, 0)
c
      call writetsscomm(8, ' ')
      call writetssint(8, 'TotalShowers', mtotshowers, 0)
      call writetssint(8, 'CompletedShowers', lastshower, 0)
      call writetssint(8, 'FirstShowerNumber', firstshowernon1 + 1, 0)
      call writetssstr(8, 'TaskStartDate', datistr0(1))
      call writetssstr(8, 'LastShowerEndDate', datistr0(2))
      if (lastshower .lt. mtotshowers) then
        call writetssstr(8, 'LastRunEndDate', datistr0(3))
      else
        call writetssstr(8, 'TaskEndDate', datistr0(2))
      endif
      call writetssstr(8, 'CurrentDate', tssdati)
c
      call writetssint(8, 'TotalProcesses', processnumber, 0)
      call writetssint(8, 'TotalRuns', jobnumber, 0)
      call writetssflt(8, 'TotalCPUTime', cpu0(1), 0)
      if (lastshower .gt. 0) then
        call writetssflt(8, 'AvgCPUTimePerShower',
     +                      cpu0(2) / lastshower, 0)
      endif
c
c     Input parameters.
c
      call inptss(8)
c
c     Global variables.
c
      if (thereareglobars) then
c
        call writetsscomm(8, 'Global variables.')
c
        do j = 1, 2
          if (nglobar(j) .gt. 0) then
            do i = 1, nglobar(j)
              call getglobal(globnam(i, j)(1:globlen(i, j)), k,
     +                       auxline, slen1)
              if (slen1 .le. 0) then
                auxline = ' '
                slen1   = 1
              endif
              call writetssstr(8, globnam(i, j)(1:globlen(i, j)),
     +                         auxline(1:slen1))
            enddo
          endif
        enddo
c
      endif
c
c     Some results from the simulations.
c
      if (saveshowerdata) then
c
c       Writing the most relevant "shower per shower" output data.
c
        call writetsscomm(8, 'Parameters relative to each shower')
c
        call writetssstr(8, 'ShowerPerShowerKey',
     +                      'PrCode PrEgy Zenith Azim '//
     +                      'X1v Xmaxv Nmax X0v Lambda SofSqr FitRc')
        call writetsscomm(8, ' ')
c
c       Opening the file that contains shower-per-shower data.
c
        open(shzut, file = shzfn, status = 'OLD',
     +       form = 'UNFORMATTED', err = 3020)
c
        read(shzut, err = 3020, end = 3020)
     +              i, klast, ngt, kfields, rfields
c
        rfields = rfields - klast - 1
c
        j       = i / 1000000
        i       = i - 1000000 * j
        kx0     = 1
        klambda = 2
        if (i .eq. 114) then
          knmax  = klast - 1
          kxmaxv = klast - 2
        else
          knmax  = klast
          kxmaxv = klast - 1
          if (i .eq. 103) then
            klambda  = 5
            xpars(5) = 70
          endif
        endif
c
        showerno = firstshowernon1
        iprimx1v = 3 * ngt + 1
        iprimegy = iprimx1v + 1
        iprimzen = iprimegy + 2
        iprimazi = iprimzen + 1
c
 1100   continue
        if (pershowertables) call intfileskip(shzut, -99966)
        read(shzut, err = 3020, end = 1140)
     +              (ipars(k), k = 1, kfields),
     +              (xpars(k), k = 1, klast), sumofsq,
     +              (obslevscra1(j), j = 1, rfields)
        showerno = showerno + 1
c
        write(auxline, 2050) shprcode,
     +                       obslevscra1(iprimegy),
     +                       obslevscra1(iprimzen),
     +                       obslevscra1(iprimazi),
     +                       obslevscra1(iprimx1v),
     +                       xpars(kxmaxv),
     +                       xpars(knmax),
     +                       xpars(kx0),
     +                       xpars(klambda),
     +                       sumofsq, fitrc
 2050   format(i6, 1p, 9g14.6, i6)
c
        write(tssauxstr, 2060) 'DataSh', showerno
 2060   format(a, i6.6)
        call writetssstr(8, tssauxstr(1:12), auxline(1:138))
c
        goto 1100
 1140   continue
        close(shzut)
c
      endif
c
      call writetsscomm(8, 'End of tss file')
      close(8)
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A09', 4, 'tssfile',
     +              auxfilestring(1:flen1),
     +              1, 8, 0, 0.d0, ' ')
      return
c
 3020 continue
c
      call errprint(0, '$A30', 4, 'tssfile',
     +              ' ', 1, shzut, 0, 0.d0, ' ')
c
      end
c     --- End of routine tssfile.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'tss.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
