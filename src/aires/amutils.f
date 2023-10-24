c
c     FILE: amutils.f                       CREATION DATE: 18/AUG/2003.
c                                       LAST MODIFICATION: 20/NOV/2003.
c
c     Routines called from AiresMerge, that process several idf/adf
c     files and then create a single output adf file with merged
c     data.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setiowdir(cdline, llen, i1, i2)
c
c     Setting working i/o directory for AiresMerge.
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Positions of beginning
c                      and end of current working word of cdline.
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
      character*(*)     cdline
      integer           llen, i1, i2
c
c     Declaration of shared data.
c
      include 'initmergecomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      call nextword(cdline, llen, i1, i2)
c
      if (i1 .le. i2) then
        amwdirname    = cdline(i1:i2)
        amwdirnamelen = i2 - i1 + 1
      else
        amwdirname    = ' '
        amwdirnamelen = 0
      endif
c
      return
      end
c     --- End of routine setiowdir
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setotaskname(cdline, llen, i1, i2)
c
c     Setting output task name for AiresMerge.
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Positions of beginning
c                      and end of current working word of cdline.
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
      character*(*)     cdline
      integer           llen, i1, i2
c
c     Declaration of shared data.
c
      include 'initmergecomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      call nextword(cdline, llen, i1, i2)
c
      if (i1 .le. i2) then
        if (cdline(i1:i2) .eq. 'Append') then
          call nextword(cdline, llen, i1, i2)
          if (i1 .le. i2) then
            amtasknamelen = 0
          else
            call errprint(0, '$A12', 3, 'setotaskname',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            return
          endif
        endif
      else
        call errprint(0, '$A12', 3, 'setotaskname',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        return
      endif
c
      if (amtasknamelen .eq. 0) amtasknameset = amtasknameset + 1
c
      amtaskname(amtasknamelen+1:64) = cdline(i1:i2)
      amtasknamelen = amtasknamelen + i2 - i1 + 1
c
      if (amtasknamelen .gt. 64) then
        amtasknamelen = 64
        call errprint(0, '$T15', 2, 'setotaskname', ' ',
     +                0, 0, 0, 0.d0, amtaskname(1:amtasknamelen))
      endif
c
      return
      end
c     --- End of routine setotaskname
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setototshowers(cdline, llen, i1, i2)
c
c     Setting total number of showers for AiresMerge.
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Positions of beginning
c                      and end of current working word of cdline.
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
      character*(*)     cdline
      integer           llen, i1, i2
c
c     Declaration of internal variables and arrays.
c
      integer           itmp1, irc
      double precision  ftmp1
c
c     Declaration of shared data.
c
      include 'initmergecomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c
      call getnumber(.true., cdline, llen, i1, i2, 2, 0.d0,
     +               ftmp1, irc)
c
      if (irc .eq. 0) then
c
        itmp1 = ftmp1
        if ((itmp1 .gt. 0) .and. (itmp1 .le. 759375) 
     +      .and. (itmp1 .eq. ftmp1)                 ) then
          ammtotshowers = itmp1
          amtotshwset   = amtotshwset + 1
        else
          call errprint(0, '$A20', 3, 'setototshowers',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 8
        endif
c
      else
        call errprint(0, '$A06', 3, 'setototshowers',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
      endif
c
      return
      end
c     --- End of routine setototshowers
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine mergefile(iadfile)
c
c     Reading in a idf/adf file and merging its contents to the
c     AiresMerge area
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
c     Arguments:
c     =========
c
c     iadfile......... (input, character*(*)) Task name, or dump file
c                      name.
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
      character*(*)     iadfile
c
c     Declaration of shared data.
c
      include 'initmergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           irc
c
c     FIRST EXECUTABLE STATEMENT
c
c     Opening the dump file.
c
      call loadumpfile(amwdirname(1:amwdirnamelen), iadfile, 0, 4, irc)
c
c     The file has been read in successfully. Merging the new data
c     set into the general one.
c
      if (nmergedfiles .le. 0) call inimerge
c
      call mergedata(iadfile)
c
      nmergedfiles = nmergedfiles + 1
c
      return
      end
c     --- End of routine mergefile
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setmergedout
c
c     Packing already merged data and getting ready for ADF file
c     writing.
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'maincomm.f'
      include 'initcomm.f'
      include 'initmergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Packing merged data.
c
      call pakmergedata
c
c     Setting new taskname
c
      taskname = amtaskname
      tasknamelen = amtasknamelen
c
c     Setting name and directory for output file.
c
      do i = 1, 2
        wdirname(i) = amwdirname
        wdirnamelen(i) = amwdirnamelen
        call absfnset(wdirname(i), wdirnamelen(i),
     +                taskname, tasknamelen,
     +                leadingfn(i), leadingfnlen(i))
      enddo
c
c     Setting the variable adfnmergedfiles equal to the
c     number of merged files. This is to label the output adf file
c     as the result of merging various data sets.
c
      adfnmergedfiles = nmergedfiles
c
      return
      end
c     --- End of routine setmergedout
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine inimerge
c
c     Initializing all the internal arrays used during the merging
c     process.
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'initmergecomm.f'
      include 'kernelmergecomm.f'
      include 'cioauxcomm.f'
      include 'ciomergecomm.f'
      include 'hdatacomm.f'
      include 'hmergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
c
c     FIRST EXECUTABLE STATEMENT
c
c     Date and time data.
c
      do i = 1, 4
        amdatistr0(i) = datistr0(i)
      enddo
c
      do i = 1, 3
        amcpu0(i) = 0
      enddo
c
c     Main input data arrays.
c
      amnfidata = nfidata
      amniidata = niidata
      amnlidata = nlidata
      amnsidata = nsidata
c
c     Main output data arrays.
c
      amnfodata = nfodata
      amniodata = niodata
      amnlodata = nlodata
c
      do i = 1, nfodata
        amfosamples(i) = 0
      enddo
      call statzero(nfodata, 5, amfodata)
c
      do i = 1, niodata
        amiosamples(i) = 0
        amiodata(1, i) = 0
        amiodata(2, i) = 2147483647
        amiodata(3, i) = 0
      enddo
c
c     Observing levels data.
c
      do i = 0, 1
        amiobslevdat(i) = iobslevdat(i)
      enddo
      do i = 0, 1
        do j = 1, 2
          amfobslevdat(i, j) = fobslevdat(i, j)
        enddo
      enddo
c
c     Table (histogram) data.
c
      amnttabins   = nttabins
      amnlhtables  = nlhtables
      amnldtables  = nldtables
      amntdtables  = ntdtables
      amnpshtables = npshtables
      amnlitables  = nlitables
c
c     Logitudinal tables.
c
      do k = 1, nlhtables
        call statzero(nobslevelsp1, 5, amlhistn(1, 1, k))
        call statzero(nobslevelsp1, 5, amlhiste(1, 1, k))
        call statzero(nobslevelsp1, 5, amwlhistn(1, 1, k))
        call statzero(nobslevelsp1, 5,
     +                 amlhistn(1, 1, k + mxlhtable))
        call statzero(nobslevelsp1, 5,
     +                 amwlhistn(1, 1, k + mxlhtable))
      enddo
c
c     Deposited energy and related histograms.
c
      do k = 1, nlitables
        call statzero(nobslevelsp1, 5, amllhistn(1, 1, k))
        call statzero(nobslevelsp1, 5, amllhiste(1, 1, k))
        call statzero(nobslevelsp1, 5, amwllhistn(1, 1, k))
        call statzero(nobslevelsp1, 5,
     +                 amllhistn(1, 1, k + mxlitable))
        call statzero(nobslevelsp1, 5,
     +                 amllhiste(1, 1, k + mxlitable))
        call statzero(nobslevelsp1, 5,
     +                 amwllhistn(1, 1, k + mxlitable))
        call statzero(nobslevelsp1, 5, amlihiste(1, 1, k))
      enddo
c
c     Lateral distribution histograms.
c
      do k = 1, nldtables
        call statzero(nttabinsp2, 5, amrthistn(1, 0, k))
        call statzero(nttabinsp2, 5, amrthiste(1, 0, k))
        call statzero(nttabinsp2, 5, amwrthistn(1, 0, k))
        call statzero(nttabinsp2, 5, amwrthiste(1, 0, k))
      enddo
c
c     Time distribution histograms.
c
      do k = 1, ntdtables
        call statzero(nttabinsp2, 5, amrthistt(1, 0, k))
        do i = 0, nttabinsp1
          amrtsampl(i, k) = 0
        enddo
      enddo
c
c     Input and output data associated variables (Directive names,
c     etc.)
c
      amncommands = ncommands
c
c     Site library.
c
      amnlibsites = nlibsites
c
c     cio buffers and related variables.
c
      amnciofiles  = nciofiles
      amnciofilesu = nciofilesu
c
c     Statistical variables related with cio particle buffers.
c
      call ciostatzero
c
c     Stack names, processing modes, etc.
c
      amnpstacks = npstacks
c
c     Stack statistical data and related variables.
c
      do i = 1, npstacks
c
        amcallcounter(1, i) = 0
        amavgtotsize(i)     = 0
c
        ampeakstsize(1, i) = 0
        ampeakstsize(2, i) = 2147483647
        ampeakstsize(3, i) = 0
c
        amprocentries(1, i) = 0
        amprocentries(2, i) = 2147483647
        amprocentries(3, i) = 0
c
        amhardswrite(1, i) = 0
        amhardswrite(2, i) = 2147483647
        amhardswrite(3, i) = 0
c
        amhardsread(1, i) = 0
        amhardsread(2, i) = 2147483647
        amhardsread(3, i) = 0
c
      enddo
c
      call statzero(npstacksp1, 5, amtotpcles)
      call statzero(npstacksp1, 5, ameloss)
      call statzero(npstacksp1, 5, amnplost)
      call statzero(npstacksp1, 5, amelost)
      call statzero(npstacksp1, 5, amnplowe)
      call statzero(npstacksp1, 5, amelowe)
      call statzero(npstacksp1, 5, amnprgnd)
      call statzero(npstacksp1, 5, ameprgnd)
c
c     Information about unphysical particles, first interaction, etc.
c
      call statzero(1, 5, amnnotap)
      call statzero(1, 5, amenotap)
      call statzero(1, 5, amnneutrino)
      call statzero(1, 5, ameneutrino)
      call statzero(1, 5, amaveprim)
      call statzero(2, 5, amfstintdp)
c
c     Per-shower data.
c
      amshzfn           = randomfnh // '.shwz_AMT'
      ampershowertables = pershowertables
      amsaveshowerdata  = saveshowerdata
      amsavesheader     = .true.
c
c     Cumulative number of showers.
c
      ammtotshowersum = 0
c
c     Merging initialization complete
c
      return
c
      end
c     --- End of routine inimerge
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine mergedata(iadfile)
c
c     Merging data already read in from a single dump file.
c     AiresMerge area
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
c     Arguments:
c     =========
c
c     iadfile......... (input, character*(*)) Task name, or dump file
c                      name.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      character*(*)     iadfile
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'initmergecomm.f'
      include 'kernelcomm.f'
      include 'kernelmergecomm.f'
      include 'cioauxcomm.f'
      include 'ciomergecomm.f'
      include 'hdatacomm.f'
      include 'hmergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, irc
      character*20      auxdatistr(4)
      integer           idasort(4)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Date and time data.
c
      auxdatistr(1) = amdatistr0(1)
      auxdatistr(2) = datistr0(1)
      auxdatistr(3) = datistr0(2)
      auxdatistr(4) = amdatistr0(2)
c
      do i = 1, 4
        idasort(i) = i
      enddo
      call subsort(4, auxdatistr, 1, 20, idasort)
c
      amdatistr0(1) = auxdatistr(idasort(1))
      amdatistr0(2) = auxdatistr(idasort(4))
      amdatistr0(3) = datistr0(3)
      amdatistr0(4) = datistr0(4)
c
      do i = 1, 3
        amcpu0(i) = amcpu0(i) + cpu0(i)
      enddo
c
c     Main input data arrays.
c
      if (nfidata .ne. amnfidata) goto 3010
      if (niidata .ne. amniidata) goto 3010
      if (nlidata .ne. amnlidata) goto 3010
      if (nsidata .ne. amnsidata) goto 3010
c
c     Main output data arrays.
c
      if (nfodata .ne. amnfodata) goto 3010
      if (niodata .ne. amniodata) goto 3010
      if (nlodata .ne. amnlodata) goto 3010
c
      do i = 1, nfodata
        amfosamples(i) = amfosamples(i) + fosamples(i)
      enddo
      call statmerge(nfodata, 5, fodata, amfodata)
c
      do i = 1, niodata
        amiosamples(i) = amiosamples(i) + iosamples(i)
        amiodata(1, i) = amiodata(1, i) + iodata(1, i)
        if (iodata(2, i) .lt. amiodata(2, i))
     +        amiodata(2, i) = iodata(2, i)
        if (iodata(3, i) .gt. amiodata(3, i))
     +        amiodata(3, i) = iodata(3, i)
      enddo
c
c     Observing levels data.
c
      do i = 0, 1
        if (iobslevdat(i) .ne. amiobslevdat(i)) goto 3010
      enddo
      do i = 0, 1
        do j = 1, 2
          if (fobslevdat(i, j) .ne. amfobslevdat(i, j)) goto 3010
        enddo
      enddo
c
c     Table (histogram) data.
c
      if (nttabins   .ne. amnttabins)   goto 3010
      if (nlhtables  .ne. amnlhtables)  goto 3010
      if (nldtables  .ne. amnldtables)  goto 3010
      if (ntdtables  .ne. amntdtables)  goto 3010
      if (npshtables .ne. amnpshtables) goto 3010
      if (nlitables  .ne. amnlitables)  goto 3010
c
c     Logitudinal tables.
c
      do k = 1, nlhtables
        call statmerge(nobslevelsp1, 5,
     +                 lhistn(1, 1, k), amlhistn(1, 1, k))
        call statmerge(nobslevelsp1, 5,
     +                 lhiste(1, 1, k), amlhiste(1, 1, k))
        call statmerge(nobslevelsp1, 5,
     +                 wlhistn(1, 1, k), amwlhistn(1, 1, k))
        call statmerge(nobslevelsp1, 5,
     +                 lhistn(1, 1, k + mxlhtable),
     +                 amlhistn(1, 1, k + mxlhtable))
        call statmerge(nobslevelsp1, 5,
     +                 wlhistn(1, 1, k + mxlhtable),
     +                 amwlhistn(1, 1, k + mxlhtable))
      enddo
c
c     Deposited energy and related histograms.
c
      do k = 1, nlitables
        call statmerge(nobslevelsp1, 5,
     +                 llhistn(1, 1, k), amllhistn(1, 1, k))
        call statmerge(nobslevelsp1, 5,
     +                 llhiste(1, 1, k), amllhiste(1, 1, k))
        call statmerge(nobslevelsp1, 5,
     +                 wllhistn(1, 1, k), amwllhistn(1, 1, k))
        call statmerge(nobslevelsp1, 5,
     +                 llhistn(1, 1, k + mxlitable),
     +                 amllhistn(1, 1, k + mxlitable))
        call statmerge(nobslevelsp1, 5,
     +                 llhiste(1, 1, k + mxlitable),
     +                 amllhiste(1, 1, k + mxlitable))
        call statmerge(nobslevelsp1, 5,
     +                 wllhistn(1, 1, k + mxlitable),
     +                 amwllhistn(1, 1, k + mxlitable))
        call statmerge(nobslevelsp1, 5,
     +                 lihiste(1, 1, k), amlihiste(1, 1, k))
      enddo
c
c     Lateral distribution histograms.
c
      do k = 1, nldtables
        call statmerge(nttabinsp2, 5,
     +                 rthistn(1, 0, k), amrthistn(1, 0, k))
        call statmerge(nttabinsp2, 5,
     +                 rthiste(1, 0, k), amrthiste(1, 0, k))
        call statmerge(nttabinsp2, 5,
     +                 wrthistn(1, 0, k), amwrthistn(1, 0, k))
        call statmerge(nttabinsp2, 5,
     +                 wrthiste(1, 0, k), amwrthiste(1, 0, k))
      enddo
c
c     Time distribution histograms.
c
      do k = 1, ntdtables
        call statmerge(nttabinsp2, 5,
     +                 rthistt(1, 0, k), amrthistt(1, 0, k))
        do i = 0, nttabinsp1
          amrtsampl(i, k) = amrtsampl(i, k) + rtsampl(i, k)
        enddo
      enddo
c
c     Input and output data associated variables (Directive names,
c     etc.)
c
      if (ncommands .ne. amncommands) goto 3010
c
c     Site library.
c
      if (nlibsites .ne. amnlibsites) goto 3010
c
c     cio buffers and related variables.
c
      if (nciofiles  .ne. amnciofiles)  goto 3010
      if (nciofilesu .ne. amnciofilesu) goto 3010
c
c     Statistical variables related with cio particle buffers.
c
      call ciostatmerge(irc)
      if (irc .ne. 0) goto 3010
c
c     Stack names, processing modes, etc.
c
      if (npstacks .ne. amnpstacks) goto 3010
c
c     Stack statistical data and related variables.
c
      do i = 1, npstacks
c
        amcallcounter(1, i) = amcallcounter(1, i) + callcounter(1, i)
        amavgtotsize(i)     = amavgtotsize(i) + avgtotsize(i)
c
        ampeakstsize(1, i) = ampeakstsize(1, i) + peakstsize(1, i)
        if (peakstsize(2, i) .lt. ampeakstsize(2, i))
     +        ampeakstsize(2, i) = peakstsize(2, i)
        if (peakstsize(3, i) .gt. ampeakstsize(3, i))
     +        ampeakstsize(3, i) = peakstsize(3, i)
c
        amprocentries(1, i) = amprocentries(1, i) + procentries(1, i)
        if (procentries(2, i) .lt. amprocentries(2, i))
     +        amprocentries(2, i) = procentries(2, i)
        if (procentries(3, i) .gt. amprocentries(3, i))
     +        amprocentries(3, i) = procentries(3, i)
c
        amhardswrite(1, i) = amhardswrite(1, i) + hardswrite(1, i)
        if (hardswrite(2, i) .lt. amhardswrite(2, i))
     +        amhardswrite(2, i) = hardswrite(2, i)
        if (hardswrite(3, i) .gt. amhardswrite(3, i))
     +        amhardswrite(3, i) = hardswrite(3, i)
c
        amhardsread(1, i) = amhardsread(1, i) + hardsread(1, i)
        if (hardsread(2, i) .lt. amhardsread(2, i))
     +        amhardsread(2, i) = hardsread(2, i)
        if (hardsread(3, i) .gt. amhardsread(3, i))
     +        amhardsread(3, i) = hardsread(3, i)
c
      enddo
c
      call statmerge(npstacksp1, 5, totpcles, amtotpcles)
      call statmerge(npstacksp1, 5, eloss, ameloss)
      call statmerge(npstacksp1, 5, nplost, amnplost)
      call statmerge(npstacksp1, 5, elost, amelost)
      call statmerge(npstacksp1, 5, nplowe, amnplowe)
      call statmerge(npstacksp1, 5, elowe, amelowe)
      call statmerge(npstacksp1, 5, nprgnd, amnprgnd)
      call statmerge(npstacksp1, 5, eprgnd, ameprgnd)
c
c     Information about unphysical particles, first interaction, etc.
c
      call statmerge(1, 5, nnotap, amnnotap)
      call statmerge(1, 5, enotap, amenotap)
      call statmerge(1, 5, nneutrino, amnneutrino)
      call statmerge(1, 5, eneutrino, ameneutrino)
      call statmerge(1, 5, aveprim, amaveprim)
      call statmerge(2, 5, fstintdp, amfstintdp)
c
c     Shower per shower data.
c
      call pershowerdatmerge(irc)
      if (irc .ne. 0) goto 3020
c
c     Cumulative number of showers.
c
      ammtotshowersum = ammtotshowersum + lastshower
c
c     Merging complete
c
      return
c
c     Error messages.
c
 3010 continue
c
      call errprint(0, '$A30', 4, 'mergedata',
     +     'Serious problem when merging dump file data.$' //
     +     'Incompatibility between saved and current parameters.$' //
     +     'Task/dump file name:',
     +     0, 0, 0, 0.d0, iadfile)
      return
c
 3020 continue
c
      call errprint(0, '$A30', 4, 'mergedata',
     +     'Serious problem when merging dump file data.$' //
     +     'Error processing internal data file.$' //
     +     'Task/dump file name:',
     +     0, 0, 0, 0.d0, iadfile)
      return
c
      end
c     --- End of routine mergedata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pakmergedata
c
c     Packing into the original arrays data that was already merged.
c
c     Written by: S. J. Sciutto, Fermilab 2003
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'initmergecomm.f'
      include 'kernelcomm.f'
      include 'kernelmergecomm.f'
      include 'cioauxcomm.f'
      include 'ciomergecomm.f'
      include 'showercomm.f'
      include 'hdatacomm.f'
      include 'hmergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k
      logical           sgshower
      integer           iitem, itmp1, dindex, dcode, irc
      double precision  primazi
      double precision  xmax(2), nmax, x0, lambda, sumofsq
      integer           wordlocate, commandcode
      double precision  xslant
c
c     FIRST EXECUTABLE STATEMENT
c
c     Verifying total number of showers.
c
      if (amtotshwset .gt. 0) then
        mtotshowers = ammtotshowers
      else
        mtotshowers = ammtotshowersum
      endif
c
      if (mtotshowers .le. 0) then
        call errprint(0, '*', 4, 'setmergedata',
     +  'Total number of showers is zero. No data to process.',
     +  0, 0, 0, 0.d0, ' ')
      endif
c
      lastshower = mtotshowers
      currshower = lastshower
      sgshower   = (mtotshowers .le. 1)
c
c     Preparing the output data.
c
c     Date and time data.
c
      do i = 1, 4
        datistr0(i) = amdatistr0(i)
      enddo
c
      do i = 1, 3
        cpu0(i) = amcpu0(i)
      enddo
c
c     Main output data arrays.
c
      do i = 1, nfodata
        fosamples(i) = amfosamples(i)
      enddo
      call statpak(sgshower, nfodata, 5, amfodata, fodata)
c
      if (sgshower) then
        do i = 1, niodata
          iosamples(i) = amiosamples(i)
          iodata(1, i) = amiodata(1, i)
          iodata(2, i) = amiodata(1, i)
          iodata(3, i) = amiodata(1, i)
        enddo
      else
        do i = 1, niodata
          iosamples(i) = amiosamples(i)
          iodata(1, i) = amiodata(1, i)
        enddo
      endif
c
c     Table (histogram) data.
c
c     Logitudinal tables.
c
      do k = 1, nlhtables
        call statpak(sgshower, nobslevelsp1, 5,
     +               amlhistn(1, 1, k), lhistn(1, 1, k))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amlhiste(1, 1, k), lhiste(1, 1, k))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amwlhistn(1, 1, k), wlhistn(1, 1, k))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amlhistn(1, 1, k + mxlhtable),
     +               lhistn(1, 1, k + mxlhtable))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amwlhistn(1, 1, k + mxlhtable),
     +               wlhistn(1, 1, k + mxlhtable))
      enddo
c
c     Deposited energy and related histograms.
c
      do k = 1, nlitables
        call statpak(sgshower, nobslevelsp1, 5,
     +               amllhistn(1, 1, k), llhistn(1, 1, k))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amllhiste(1, 1, k), llhiste(1, 1, k))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amwllhistn(1, 1, k), wllhistn(1, 1, k))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amllhistn(1, 1, k + mxlitable),
     +               llhistn(1, 1, k + mxlitable))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amllhiste(1, 1, k + mxlitable),
     +               llhiste(1, 1, k + mxlitable))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amwllhistn(1, 1, k + mxlitable),
     +               wllhistn(1, 1, k + mxlitable))
        call statpak(sgshower, nobslevelsp1, 5,
     +               amlihiste(1, 1, k), lihiste(1, 1, k))
      enddo
c
c     Lateral distribution histograms.
c
      do k = 1, nldtables
        call statpak(sgshower, nttabinsp2, 5,
     +               amrthistn(1, 0, k), rthistn(1, 0, k))
        call statpak(sgshower, nttabinsp2, 5,
     +               amrthiste(1, 0, k), rthiste(1, 0, k))
        call statpak(sgshower, nttabinsp2, 5,
     +               amwrthistn(1, 0, k), wrthistn(1, 0, k))
        call statpak(sgshower, nttabinsp2, 5,
     +               amwrthiste(1, 0, k), wrthiste(1, 0, k))
      enddo
c
c     Time distribution histograms.
c     The time distribution histograms make sense only for
c     multi-shower merging.
c
      if (sgshower) then
        do k = 1, ntdtables
          call statzero(nttabinsp2, 5, rthistt(1, 0, k))
          do i = 0, nttabinsp1
            rtsampl(i, k) = 0
          enddo
        enddo
      else
        do k = 1, ntdtables
          call statpak(sgshower, nttabinsp2, 5,
     +                 amrthistt(1, 0, k), rthistt(1, 0, k))
          do i = 0, nttabinsp1
            rtsampl(i, k) = amrtsampl(i, k)
          enddo
        enddo
      endif
c
c     Statistical variables related with cio particle buffers.
c
      call ciostatpak(sgshower)
c
c     Stack statistical data and related variables.
c
      do i = 1, npstacks
        callcounter(1, i) = amcallcounter(1, i)
        avgtotsize(i)     = amavgtotsize(i)
      enddo
c
      if (sgshower) then
        do i = 1, npstacks
          peakstsize(1, i)  = ampeakstsize(1, i)
          peakstsize(2, i)  = ampeakstsize(1, i)
          peakstsize(3, i)  = ampeakstsize(1, i)
          procentries(1, i) = amprocentries(1, i)
          procentries(2, i) = amprocentries(1, i)
          procentries(3, i) = amprocentries(1, i)
          hardswrite(1, i)  = amhardswrite(1, i)
          hardswrite(2, i)  = amhardswrite(1, i)
          hardswrite(3, i)  = amhardswrite(1, i)
          hardsread(1, i)   = amhardsread(1, i)
          hardsread(2, i)   = amhardsread(1, i)
          hardsread(3, i)   = amhardsread(1, i)
        enddo
      else
        do i = 1, npstacks
          peakstsize(1, i)  = ampeakstsize(1, i)
          procentries(1, i) = amprocentries(1, i)
          hardswrite(1, i)  = amhardswrite(1, i)
          hardsread(1, i)   = amhardsread(1, i)
        enddo
      endif
c
      call statpak(sgshower, npstacksp1, 5, amtotpcles, totpcles)
      call statpak(sgshower, npstacksp1, 5, ameloss, eloss)
      call statpak(sgshower, npstacksp1, 5, amnplost, nplost)
      call statpak(sgshower, npstacksp1, 5, amelost, elost)
      call statpak(sgshower, npstacksp1, 5, amnplowe, nplowe)
      call statpak(sgshower, npstacksp1, 5, amelowe, elowe)
      call statpak(sgshower, npstacksp1, 5, amnprgnd, nprgnd)
      call statpak(sgshower, npstacksp1, 5, ameprgnd, eprgnd)
c
c     Information about unphysical particles, first interaction, etc.
c
      call statpak(sgshower, 1, 5, amnnotap, nnotap)
      call statpak(sgshower, 1, 5, amenotap, enotap)
      call statpak(sgshower, 1, 5, amnneutrino, nneutrino)
      call statpak(sgshower, 1, 5, ameneutrino, eneutrino)
c
c     Average primary energy and average depth of first interaction
c     need a special treatment in the case of a single shower, being not
c     "cumulative" observables.
c
      if (sgshower) then
        i           = max(ammtotshowersum, 1)
        aveprim(1)  = amaveprim(1) / i
        aveprim(3)  = aveprim(1) ** 2
        do k = 1, 2
          fstintdp(1, k) = amfstintdp(1, k) / i
          fstintdp(3, k) = fstintdp(1, k) ** 2
        enddo
      else
        call statpak(sgshower, 1, 5, amaveprim, aveprim)
        call statpak(sgshower, 2, 5, amfstintdp, fstintdp)
      endif
c
c     Shower per shower data.
c
      if (sgshower) then
c
c       Data represents a single shower. Therefore the accumulated
c       shower per shower data makes no sense. We need to rewrite
c       the internal file, generating again the data for the
c       merged sets. This enforces "Brief" option in PerShowerData
c       directive.
c
        dcode = commandcode('PerShowerData', 13, dindex)
        if ((dcode .eq. 0) .or. (dindex .le. 0)) goto 3010
        iitem = aditem(dindex)
        if (iitem .le. 0) goto 3010
        itmp1 = wordlocate('Brief',
     +                     sidatastring(sidata(4, iitem):
     +                                  sidata(5, iitem)),
     +                     sidatastring(sidata(3, iitem):
     +                                  sidata(3, iitem)),
     +                     sidata(6, iitem))
        sidatawaset(iitem) = 0
        sidata(1, iitem)   = itmp1
c
c       Initializing the internal file.
c
        call inipershowerdata
c
c       Evaluating shower profile for this special case and
c       saving all data into the internal file.
c
        call xmaxfit(1, x0, lambda, xmax(1), nmax, sumofsq, irc)
        xmax(2) = xslant(xmax(1), 0.d0, -initshdir(3), groundz)
c
c       Saving data into the internal file.
c
c       Primary code, related data are taken from general parameters
c       corresponding to the last read in dump file. Azimuths are
c       transformed into geomagnetic ones if necessary.
c
        primazi = pryazimmin
c
        if (geognorth) then
c
          primazi = geobd - primazi
c
c         Reducing to the (-pi, pi] interval.
c
          if (primazi .gt. 180) then
            primazi = primazi - 360
          else if (primazi .le. -180) then
            primazi = primazi + 360
          endif
c
        endif
c
        call addpershowerdata(shprimary(1), pryenergymax,
     +                        pryzenithmin, primazi,
     +                        fstintdp(1, 1), fstintdp(1, 2),
     +                        x0, lambda, xmax, nmax, sumofsq, irc)
c
      else
c
c       Multishower merging. All the data from the
c       merged internal files is kept unchanged.
c
        amauxfn = shzfn
        shzfn   = amshzfn
        amshzfn = amauxfn
c
      endif
c
c     Data packing complete.
c
      return
c
c     Error exits.
c
 3010 continue
      call errprint(0, '*', 3, 'pakmergedata',
     +  'Serious error when processing internal file header',
     +  0, 0, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine pakmergedata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciostatzero
c
c     Initializing the statistical counters related with the merging of
c     cio related data.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
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
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
      include 'ciomergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Ground particle buffer related data.
c
      call statzero(2, 5, amngndlowp)
      call statzero(1, 5, amngndhighp)
      call statzero(2, 5, amegndlowp)
      call statzero(1, 5, amegndhighp)
c
c     Statistical data associated with the other cio particle
c     buffers.
c
      do i = 2, nciofiles
        call statzero(2, 5, amnoplowp(1, 1, i))
        call statzero(1, 5, amnophighp(1, i))
        call statzero(2, 5, ameoplowp(1, 1, i))
        call statzero(1, 5, ameophighp(1, i))
      enddo
c
      return
c
      end
c     --- End of routine ciostatzero
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciostatmerge(rc)
c
c     Merging the statistical counters related with the cio particle
c     buffers.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the merging operation is completed
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
      integer           rc
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
      include 'ciomergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Ground particle buffer related data.
c
      call statmerge(2, 5, ngndlowp, amngndlowp)
      call statmerge(1, 5, ngndhighp, amngndhighp)
      call statmerge(2, 5, egndlowp, amegndlowp)
      call statmerge(1, 5, egndhighp, amegndhighp)
c
c     Statistical data associated with the other cio particle
c     buffers.
c
      do i = 2, nciofiles
        call statmerge(2, 5, noplowp(1, 1, i), amnoplowp(1, 1, i))
        call statmerge(1, 5, nophighp(1, i), amnophighp(1, i))
        call statmerge(2, 5, eoplowp(1, 1, i), ameoplowp(1, 1, i))
        call statmerge(1, 5, eophighp(1, i), ameophighp(1, i))
      enddo
c
      rc = 0
      return
c
      end
c     --- End of routine ciostatmerge
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciostatpak(sgshower)
c
c     Packing for output the statistical counters related with the
c     merging of cio related data.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     sgshower....... (input, logical) TRUE if the data set corresponds
c                     to a single shower. FALSE otherwise.
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
      logical           sgshower
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
      include 'ciomergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Ground particle buffer related data.
c
      call statpak(sgshower, 2, 5, amngndlowp, ngndlowp)
      call statpak(sgshower, 1, 5, amngndhighp, ngndhighp)
      call statpak(sgshower, 2, 5, amegndlowp, egndlowp)
      call statpak(sgshower, 1, 5, amegndhighp, egndhighp)
c
c     Statistical data associated with the other cio particle
c     buffers.
c
      do i = 2, nciofiles
        call statpak(sgshower, 2, 5,
     +               amnoplowp(1, 1, i), noplowp(1, 1, i))
        call statpak(sgshower, 1, 5,
     +               amnophighp(1, i), nophighp(1, i))
        call statpak(sgshower, 2, 5,
     +               ameoplowp(1, 1, i), eoplowp(1, 1, i))
        call statpak(sgshower, 1, 5,
     +               ameophighp(1, i), eophighp(1, i))
      enddo
c
      return
      end
c     --- End of routine ciostatpak
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pershowerdatmerge(rc)
c
c     Merging shower by shower related data.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the merging operation is completed
c                     successfully. Nonzero otherwise.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           rc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'initmergecomm.f'
      include 'kernelmergecomm.f'
      include 'cioauxcomm.f'
      include 'ciomergecomm.f'
      include 'hdatacomm.f'
      include 'hmergecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           amshzut
      parameter         (amshzut = shzut + 1)
c
      integer           i, i0, i1, i2, j, k, l, m, lsh, lshr
      integer           kfields, rfields
c
c     FIRST EXECUTABLE STATEMENT
c
      rc = 32
c
c     Checking related parameters.
c
      if (pershowertables .neqv. ampershowertables) return
      if (saveshowerdata  .neqv. amsaveshowerdata)  return
c
c     Listing of shower maximum data and related quantities (shower per
c     shower).
c
      lsh     = -1
c
c     Opening the general internal file.
c
      if (amsavesheader) then
        rc = 8
        open(amshzut, file = amshzfn, status = 'UNKNOWN',
     +                form = 'UNFORMATTED', err = 1010)
        rc = 0
 1010   continue
      else
        call appendopen(amshzut, amshzfn, 'UNKNOWN', 'UNFORMATTED', rc)
      endif
      if (rc .ne. 0) return
c
c     Opening the current internal file.
c
      open(shzut, file = shzfn, status = 'OLD',
     +            form = 'UNFORMATTED', err = 1030)
c
c     Reading (and eventually copying) the file header
c
      read(shzut, err = 1030, end = 1030)
     +     i0, i, lshr, kfields, rfields, (obslevscrai(j), j = 21, 26)
      if (amsavesheader) write(amshzut, err = 3010)
     +     i0, i, lshr, kfields, rfields, (obslevscrai(j), j = 21, 26)
      if (pershowertables) then
        j = 31
        do i = 1, obslevscrai(21)
          read(shzut, err = 1030, end = 1030)
     +                 (obslevscrai(j + l), l = 1, 10)
          if (amsavesheader) write(amshzut, err = 3010)
     +                 (obslevscrai(j + l), l = 1, 10)
          obslevscrai(j + 1) = obslevscrai(j + 1) * obslevscrai(j + 2)
          j = j + 10
        enddo
        read(shzut, err = 1030, end = 1030) i
        if (amsavesheader) write(amshzut, err = 3010) i
      endif
c
      amsavesheader = .false.
c
      if (saveshowerdata) then
c
        lsh = 0
c
        if (lastshower .gt. 0) then
c
c         Copying the corresponding data.
c
          if (pershowertables) then
c
c           Copying full table information
c
            do i = 1, lastshower
c
c             Shower header.
c
              read(shzut, err = 1030, end = 1030)
     +          (obslevscraj(k), k = 1, kfields),
     +          (obslevscra1(k), k = 1, rfields)
              write(amshzut, err = 3010)
     +          (obslevscraj(k), k = 1, kfields),
     +          (obslevscra1(k), k = 1, rfields)
              read(shzut, err = 1030, end = 1030)
     +          (obslevscrai(k), k = 1, 10)
              write(amshzut, err = 3010)
     +          (obslevscrai(k), k = 1, 10)
c
c             Tables.
c
              j = 31
              do m = 1, obslevscrai(21)
                i1 = obslevscrai(j + 3)
                i2 = obslevscrai(j + 4)
                do k = 1, obslevscrai(j + 1)
                  read(shzut, end = 1030, err = 1030)
     +              (obslevscra4(l), l = i1, i2)
                  write(amshzut, err = 3010)
     +              (obslevscra4(l), l = i1, i2)
                enddo
                j = j + 10
              enddo
c
c             End shower mark.
c
              read(shzut, err = 1030, end = 1030) i1
              write(amshzut, err = 3010) i1
c
              lsh = i
            enddo
c
          else
c
            do i = 1, lastshower
              read(shzut, err = 1030, end = 1030)
     +          (obslevscraj(k), k = 1, kfields),
     +          (obslevscra1(k), k = 1, rfields)
              write(amshzut, err = 3010)
     +          (obslevscraj(k), k = 1, kfields),
     +          (obslevscra1(k), k = 1, rfields)
              lsh = i
            enddo
c
          endif
        endif
c
      else
        lsh = lastshower
      endif
c
 1030 continue
c
c     Checking error condition
c
      if (lsh .ne. lastshower) then
c
        call errprint(0, '$A30', 3, 'pershowerdatmerge',
     +                '(Data completed with dummy records).',
     +                1, shzut, 0, 0.d0, ' ')
        i0 = -99999
        do k = 1, rfields
          obslevscra1(k) = 0
        enddo
        if (pershowertables) then
c
          do l = 2, 10
            obslevscrai(l) = 0
          enddo
          do l = 0, obslevscrai(25)
            obslevscra4(l) = 0
          enddo
c
          do i = lsh + 1, lastshower
            write(amshzut, err = 3010)
     +        i0, (obslevscra1(k), k = 1, rfields)
            write(amshzut, err = 3010)
     +        i, (obslevscrai(k), k = 2, 10)
            j = 31
            do m = 1, obslevscrai(21)
              i1 = obslevscrai(j + 3)
              i2 = obslevscrai(j + 4)
              do k = 1, obslevscrai(j + 1)
                write(amshzut, err = 3010)
     +            (obslevscra4(l), l = i1, i2)
              enddo
              j = j + 10
            enddo
c
            i1 = -99966
            write(shzut, err = 3010) i1
c
          enddo
c
        else
c
          do i = lsh + 1, lastshower
            write(amshzut, err = 3010)
     +        i0, (obslevscra1(k), k = 1, rfields)
          enddo
c
        endif
      endif
c
c     Merging complete.
c
      close(shzut)
      close(amshzut)
c
      rc = 0
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A09', 4, 'pershowerdatmerge',
     +              ' ', 1, amshzut, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine pershowerdatmerge
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine statmerge(ndata, ld1, stdata, masterdata)
c
c     Merging a giving set of statistical data into a general one.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     ndata........... (input, integer) The number of data items.
c     ld1............. (input, integer) Leading dimension of arrays
c                      stdata and masterdata.
c     stdata.......... (input, double precision,
c                      array(ld1, ndata)) Statistical data array
c                      containing the following data:
c                           stdata(1, *) = sums of x
c                           stdata(3, *) = sums of x^2
c                           stdata(4, *) = minimum x
c                           stdata(5, *) = maximum x
c                      Element 2 is not used here.
c     masterdata...... (input-output, double precision,
c                      array(ld1, ndata)) Master statistical data array
c                      containing similar data as in stdata.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ndata, ld1
      double precision  stdata(ld1, ndata)
      double precision  masterdata(ld1, ndata)
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, ndata
        masterdata(1, i) = masterdata(1, i) + stdata(1, i)
        masterdata(3, i) = masterdata(3, i) + stdata(3, i)
        if (stdata(4, i) .lt. masterdata(4, i))
     +       masterdata(4, i) = stdata(4, i)
        if (stdata(5, i) .gt. masterdata(5, i))
     +       masterdata(5, i) = stdata(5, i)
      enddo
c
      return
      end
c     --- End of routine statmerge
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine statpak(stsizesw, ndata, ld1, masterdata, stdata)
c
c     Processing merged statistical data sets and copying the
c     transformed data onto the usual arrays.
c
c     Written by: S. J. Sciutto, Fermilab 2003; La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     stsizesw........ (logical, integer) TRUE if the size of the
c                      statistical set is 1. FALSE otherwise.
c     ndata........... (input, integer) The number of data items.
c     ld1............. (input, integer) Leading dimension of arrays
c                      masterdata and stdata.
c     masterdata...... (input, double precision,
c                      array(ld1, ndata)) Master statistical data array
c                      containing the following data:
c                           masterdata(1, *) = sums of x
c                           masterdata(3, *) = sums of x^2
c                           masterdata(4, *) = minimum x
c                           masterdata(5, *) = maximum x
c                      Element 2 is not used here.
c     stdata.......... (output, double precision,
c                      array(ld1, ndata)) Statistical data array
c                      containing similar data as in masterdata.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           stsizesw
      integer           ndata, ld1
      double precision  masterdata(ld1, ndata)
      double precision  stdata(ld1, ndata)
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      if (stsizesw) then
c
c       Merged data correspond to a single data point.
c
        do i = 1, ndata
          stdata(1, i) = masterdata(1, i)
          stdata(2, i) = 0
          stdata(3, i) = masterdata(3, i)
          stdata(4, i) = masterdata(1, i)
          stdata(5, i) = masterdata(1, i)
        enddo
c
      else
c
c       Merged data correspond to multiple data points.
c
        do i = 1, ndata
          stdata(1, i) = masterdata(1, i)
          stdata(2, i) = 0
          stdata(3, i) = masterdata(3, i)
          stdata(4, i) = masterdata(4, i)
          stdata(5, i) = masterdata(5, i)
        enddo
c
      endif
c
      return
      end
c     --- End of routine statpak
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'adfwrite.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
