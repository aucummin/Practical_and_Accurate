c
c     FILE: idfwrite.f                      CREATION DATE: 22/JUN/1996.
c                                       LAST MODIFICATION: 19/AUG/2003.
c
c     Routines to save internal data. Used when ending runs and task
c     interruptions and restarts.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine idfwrite
c
c     Saving data into the internal dump file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2001, 2003.
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
      include 'xfitpar.f'
      include 'versionpar.f'
      include 'idfhead.f'
c
c     Declaration of shared data.
c
      include 'maincomm.f'
      include 'initcomm.f'
      include 'thincomm.f'
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
      integer           iep, i, j, k, l, m, i0, i1, i2
      integer           lsh, lshr, intut, irc
      integer           nefffds, kfields, rfields
      character*24      version
c
c     FIRST EXECUTABLE STATEMENT
c
      if (lgfisclosed) then
        iep = 0
      else
        iep = 2
      endif
c
c     Opening the file.
c
      auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // idfext //
     +                'TMP'
      open(10, file = auxfilestring, status = 'UNKNOWN',
     +         form = 'UNFORMATTED', err = 3010)
c
c     Writing the file header.
c
      version = aires_version
c
      write(10, err = 3010) idfheader0, idfintheader0, version
c
c     Writing original version information and related data.
c
      if (.not. oversionset) then
        original_version = aires_version
        nverchanges      = 0
      endif
      write(10, err = 3010) original_version, nverchanges, oversionset,
     +                      compildate, compilwho, compilstar
c
      write(10, err = 3010) cruserlen, crhostlen,
     +                      cruser, crhost
c
c     The variables qualifying the idf file are now set.
c
      call strimcopy(version, 24, idfversion, idfverlen)
      idfvnecurrent = .false.
      idfcdate      = compildate
      idfcwho       = compilwho
      idfcstar      = compilstar
      idfcruserlen  = cruserlen
      idfcrhostlen  = crhostlen
      idfcruser     = cruser
      idfcrhost     = crhost
c
c     Writing a record of zeros to allocate some spare fields.
c     First record is used now to label number of stacks for
c     weight limiting factors.
c
      i = 0
      write(10, err = 3010) npstacks - 1, (i, j = 2, 10)
c
c     Writing basic data.
c
      write(10, err = 3010) (datistr0(i), i = 1, 3),
     +                      (cpu0(i), i = 1, 3),
     +                      shinidati
      write(10, err = 3010) wdirnamelen(1), tasknamelen,
     +                      tasknamever, tasknameset
      if (wdirnamelen(1) .gt. 0)
     +   write(10, err = 3010) wdirname(1)(1:wdirnamelen(1))
      if (tasknamelen .gt. 0)
     +   write(10, err = 3010) taskname(1:tasknamelen)
c
      write(10, err = 3010) mtotshowers, runshowers, cpuperrun,
     +                      processjobs,
     +                      totshwset, runshwset, cpurunset,
     +                      processjobsset,
     +                      usedefaultd, usedefaults, usedefault,
     +                      lastshower, currshower,
     +                      processnumber, jobnumber, shcomplete,
     +                      stopchsize1, stopchsize2, stopchsize3
c
      write(10, err = 3010) nshprimary, pryenergymin, pryeminset
      if (nshprimary .gt. 0)
     +  write(10, err = 3010) (shprimary(i), i = 1, nshprimary),
     +                        (shprimarywt(i), i = 1, nshprimary),
     +                        (shprimarywt0(i), i = 1, nshprimary)
c
      write(10, err = 3010) currethin,
     +                      (thinmaxw(i), i = 1, npstacks),
     +                      thinmarker
c
      write(10, err = 3010) atmoslabel, atmoslset
c
c     Writing a record of zeros to allocate some spare fields.
c     Now the firsts fields are the first shower number and related
c     parameters.
c
      i = 0
      write(10, err = 3010) firstshowernon1, firstshowerset,
     +                      (i, j = 1, 8)
c
c     Random number generators shared data.
c     (Random generator identification is set to 1 in this version,
c     0 corresponds to previous versions where the original seed was
c     not saved).
c
      i = 1
      write(10, err = 3010) i
      write(10, err = 3010) xran, xran2, original_seed
c
c     Main input data arrays.
c
      i = 0
      write(10, err = 3010) nfidata, niidata, nlidata,
     +                      nsidata, sidatalen,
     +                      (i, j = 1, 6)
      write(10, err = 3010) nfidata0, niidata0, nlidata0,
     +                      nsidata0, sidatalen0,
     +                      (i, j = 1, 6)
c
      if (nfidata .gt. 0) then
        write(10, err = 3010)
     +            ((fidata(j, i), j = 0, 3), i = 1, nfidata),
     +            (fidatawaset(i), i = 1, nfidata),
     +            (fdbdry(i), i = 1, nfidata)
      endif
c
      if (niidata .gt. 0) then
        write(10, err = 3010)
     +            ((iidata(j, i), j = 0, 3), i = 1, niidata),
     +            (iidatawaset(i), i = 1, niidata),
     +            (idbdry(i), i = 1, niidata)
      endif
c
      if (nlidata .gt. 0) then
        write(10, err = 3010)
     +            ((lidata(j, i), j = 0, 2), i = 1, nlidata),
     +            (lidatawaset(i), i = 1, nlidata)
      endif
c
      if (nsidata .gt. 0) then
        write(10, err = 3010)
     +            ((sidata(j, i), j = 0, 6), i = 1, nsidata),
     +            (sidatawaset(i), i = 1, nsidata)
        write(10, err = 3010) sidatastring(1:sidatalen)
      endif
c
c     Main output data arrays.
c
      write(10, err = 3010) nfodata, niodata, nlodata,
     +                      nfodata0, niodata0, nlodata0
c
      if (nfodata .gt. 0) then
        write(10, err = 3010)
     +            ((fodata(j, i), j = 1, 5), i = 1, nfodata),
     +            (fosamples(i), i = 1, nfodata),
     +            (foupdate(i), i = 1, nfodata)
      endif
c
      if (niodata .gt. 0) then
        write(10, err = 3010)
     +            ((iodata(j, i), j = 1, 3), i = 1, niodata),
     +            (iosamples(i), i = 1, niodata),
     +            (ioupdate(i), i = 1, niodata)
      endif
c
      if (nlodata .gt. 0) then
        write(10, err = 3010)
     +            (lodata(i), i = 1, nlodata)
      endif
c
c     Observing levels data.
c
      write(10, err = 3010)
     +          (iobslevdat(i), i = 0, 1), obslevset, nobslevelsp1
      write(10, err = 3010)
     +          ((fobslevdat(i, j), i = 0, 1), j = 1, 2)
      write(10, err = 3010)
     +          obslevstep, obslevminz, obslevmaxz, obslevca,
     +          obslevcb, obslevl0
c
c     Intersection of the observing levels with the shower axis.
c
      write(10, err = 3010) (obslevsa(i), i = 0, nobslevelsp1)
      write(10, err = 3010) (zsfact(i), i = 0, 3)
c
c     Markers for observing levels that are recorded into the
c     longitudinal file(s).
c
      if (nobslevels .gt. 0) then
        write(10, err = 3010) (olfilesave(i), i = 1, nobslevels)
      endif
c
c     Some basic shower parameters.
c
      write(10, err = 3010) initshdir, currshdir,
     +                      cinjz, showert0, primcode
      write(10, err = 3010) primfdata
c
      write(10, err = 3010) dtoplost, hcosinedummy,
     +                      hrmaxlx, hrmaxly, hrminlx, hrminly
c
      if (nobslevelsp1 .gt. 0) then
        write(10, err = 3010)
     +            ((obslevcore(i, j), i = 1, 5), j = 1, nobslevelsp1)
        write(10, err = 3010)
     +            (obslevt0(j), j = 1, nobslevelsp1)
      endif
c
      write(10, err = 3010) shgbx, shgbz, emagfac, shgbon
c
c     Shower maximum data.
c
      write(10, err = 3010) shxsamples, mxxmaxd
      write(10, err = 3010) ((shxmax(i, j), i = 1, 5), j = 1, mxxmaxd),
     +                      (shnmax(i), i = 1, 5)
c
c     Table (histogram) data, including some spare positions for
c     eventual future table types (Notice that the last two positions
c     are now used for the unweighted tables).
c
      i = 0
      write(10, err = 3010)
     +          nttabins, nttabinsp1, maxtabcode,
     +          nlhtables, nlhtable1,
     +          nldtables, nldtable1,
     +          ntdtables, ntdtable1,
     +          npshtables, npshtable1,
     +          nlitables, nlitable1,
     +          (i, j = 1, 2),
     +          nlhtables, nldtables
c
c     Longitudinal tables.
c
      write(10, err = 3010)
     +      (((lhistn(j, i, k), j = 1, 5), i = 1, nobslevelsp1),
     +      k = 1, nlhtables)
c
      write(10, err = 3010)
     +      (((lhiste(j, i, k), j = 1, 5), i = 1, nobslevelsp1),
     +      k = 1, nlhtables)
c
      write(10, err = 3010)
     +      (((wlhistn(j, i, k), j = 1, 5), i = 1, nobslevelsp1),
     +      k = 1, nlhtables)
c
      write(10, err = 3010)
     +      (((lhistn(j, i, k + mxlhtable), j = 1, 5),
     +      i = 1, nobslevelsp1), k = 1, nlhtables)
c
      write(10, err = 3010)
     +      (((wlhistn(j, i, k + mxlhtable), j = 1, 5),
     +      i = 1, nobslevelsp1), k = 1, nlhtables)
c
c     Lateral distribution tables.
c
      write(10, err = 3010)
     +     (((rthistn(j, i, k), j = 1, 5), i = 0, nttabinsp1),
     +      k = 1, nldtables)
c
      write(10, err = 3010)
     +     (((rthiste(j, i, k), j = 1, 5), i = 0, nttabinsp1),
     +      k = 1, nldtables)
c
      write(10, err = 3010)
     +     (((wrthistn(j, i, k), j = 1, 5), i = 0, nttabinsp1),
     +      k = 1, nldtables)
c
      write(10, err = 3010)
     +     (((wrthiste(j, i, k), j = 1, 5), i = 0, nttabinsp1),
     +      k = 1, nldtables)
c
c     Time distribution tables.
c
      write(10, err = 3010)
     +     ((rtsampl(i, k), i = 0, nttabinsp1), k = 1, ntdtables)
      write(10, err = 3010)
     +     (((rthistt(j, i, k), j = 1, 5), i = 0, nttabinsp1),
     +      k = 1, ntdtables)
c
c     Deposited energy and tables.
c
      do l = 0, mxlitable, mxlitable
c
        write(10, err = 3010)
     +       (((llhistn(j, i, k + l), j = 1, 5), i = 1, nobslevelsp1),
     +       k = 1, nlitables)
c
        write(10, err = 3010)
     +       (((llhiste(j, i, k + l), j = 1, 5), i = 1, nobslevelsp1),
     +       k = 1, nlitables)
c
        write(10, err = 3010)
     +       (((wllhistn(j, i, k + l), j = 1, 5), i = 1, nobslevelsp1),
     +       k = 1, nlitables)
c
      enddo
c
      write(10, err = 3010)
     +     (((lihiste(j, i, k), j = 1, 5), i = 1, nobslevelsp1),
     +     k = 1, nlitables)
c
c     Input and output data associated variables (Directive names,
c     etc.)
c
      write(10, err = 3010) ncommands, ncommands0, mxcdl,
     +                      nallodata, nallodata0, ngrdprint, lallcht
c
c     Input directive names and related data.
c
      if (ncommands .gt. 0) then
        do i = 1, ncommands
          write(10, err = 3010) clgname(i), cname(i),
     +                          ccode(i), minclen(i), aditem(i),
     +                          veryimpdir(i), wngonset(i)
        enddo
      endif
c
c     Output variable names and related data.
c
      if (nallodata .gt. 0) then
        do i = 1, nallodata
          write(10, err = 3010) odataname(i),
     +                          odatatype(i), odataitem(i),
     +                          veryimpodata(i)
        enddo
      endif
c
c     Table (histogram) names and related data.
c
      do i = 1, nlhtables
        write(10, err = 3010) lhnamen(i), lhnamee(i), lhpclen(i),
     +                        lhcoden(i), lhcodee(i)
      enddo
c
      if (ngrdprint .gt. 0) then
        write(10, err = 3010) (grdporder(i), i = 1, ngrdprint),
     +                        (grdpspa(i), i = 1, ngrdprint)
      endif
c
      do i = 1, nlhtables
        write(10, err = 3010) wlhnamen(i), wlhcoden(i)
      enddo
c
      do i = 1, nldtables
        write(10, err = 3010) ldnamen(i), ldnamee(i),
     +                        ldcoden(i), ldcodee(i)
      enddo
c
      do i = 1, nldtables
        write(10, err = 3010) wldnamen(i), wldnamee(i),
     +                        wldcoden(i), wldcodee(i)
      enddo
c
      do i = 1, ntdtables
        write(10, err = 3010) tdnamen(i), tdcoden(i)
      enddo
c
      do i = 1, npshtables
        write(10, err = 3010) pshname(i), pshcode(i)
      enddo
c
      do i = 1, nlitables
        write(10, err = 3010) llnamen(i), llnamee(i),
     +                        linamee(i), wllnamen(i),
     +                        llcoden(i), llcodee(i),
     +                        licodee(i), wllcoden(i)
      enddo
c
c     Writing a record of zeros to allocate some spare fields.
c     First element is now used to store the size of the site library.
c     Second element is now used to store the number of "special"
c     particles that were defined.
c     Third and fourth elements are now used to store the numbers
c     of global variables (dynamic and static, respectively).
c
      i = 0
      write(10, err = 3010) nlibsites, nescpcles,
     +                      nglobar(1), nglobar(2),
     +                      (i, j = 5, 10)
c
c     Saving the site library.
c
      if (nlibsites .gt. 0) then
        write(10, err = 3010) (sitename(i), i = 0, nlibsites)
        write(10, err = 3010) (sitenlen(i), i = 0, nlibsites)
        write(10, err = 3010) (sitelat(i), i = 0, nlibsites)
        write(10, err = 3010) (sitelong(i), i = 0, nlibsites)
        write(10, err = 3010) (siteground(i), i = 0, nlibsites)
      endif
c
c     Saving data related with "Special" particles.
c
      if (nescpcles .gt. 0) then
        write(10, err = 3010) lastescpcle, nescodes,
     +                        pescode1, pescode2, espms
        write(10, err = 3010) (escpclename(i), i = 1, nescpcles)
        write(10, err = 3010)
     +       ((escmacropos(j, i), j = 1, 4), i = 0, nescpcles)
        write(10, err = 3010) escpclemacro(1:escmacropos(4, nescpcles))
        write(10, err = 3010) (escmacrover(i), i = 1, nescpcles)
        write(10, err = 3010) (escmacrouse(i), i = 1, nescpcles)
        write(10, err = 3010) ((nsprimpart(j, i), j = 1, 3),
     +                         i = 1, nescpcles)
c
        i = 0
        write(10, err = 3010) nintintvar, nintfltvar, recordspvar0,
     +                        i, i
        write(10, err = 3010) mxintintvar, mxintfltvar, i, i, i
c
        if (nintintvar .gt. 0) then
          write(10, err = 3010) (spintvar(i), i = 1, nintintvar)
        endif
        if (nintfltvar .gt. 0) then
          write(10, err = 3010) (spfltvar(i), i = 1, nintfltvar)
        endif
c
      endif
c
c     Saving global variables.
c
      do j = 1, 2
        if (nglobar(j) .gt. 0) then
          write(10, err = 3010) (globnam(i, j), i = 1, nglobar(j)),
     +                          (globlen(i, j), i = 1, nglobar(j))
          write(10, err = 3010) globstrlen(j),
     +                          (globdfend(i, j), i = 0, nglobar(j))
          if (globstrlen(j) .gt. 0) then
            write(10, err = 3010) globarstring(j)(1:globstrlen(j))
          endif
        endif
      enddo
c
c     Saving model specific variables.
c
      call modelidfwrite(10, irc)
      if (irc .ne. 0) goto 3030
      i = -99999
      call idfput(10, i, 0.d0, i, 0, i, .true., irc)
      if (irc .ne. 0) goto 3030
c
c     AIRES versions 1.2.0 and previous contained only one particle
c     buffer (ground particle buffer) associated with compressed I/O.
c     This buffer was saved right here. Now all the buffer stuff
c     was moved into a subroutine, and the saving operations placed
c     below.
c
c     Saving the cio buffers and related variables.
c
      write(10, err = 3010) nhwciofiles, nciofiles,
     +                      nciofilesu, mxciofiles
c
      if (nciofilesu .gt. 0) then
        write(10, err = 3010) (ciofilesu(i), i = 1, nciofilesu)
        do i = 1, nciofilesu
          j = ciofilesu(i)
          write(10, err = 3010) pofilext(j), pofileno(j),
     +                          cioreclast(j), lastciopointer(i),
     +                          ciorlastcp(j), ciowblocks(j)
          if (cioreclast(j) .gt. 0) then
            write(10, err = 3010) ciorecord1(j)(1:cioreclast(j))
          endif
        enddo
      endif
c
      if (nciofiles .gt. 0) then
        do i = 1, nciofiles
          write(10, err = 3010) nrectypes(i)
          do j = 0, nrectypes(i)
            write(10, err = 3010)
     +        totrecfields(j, i),
     +        ciodynfield(j, i), ciodynfwcix(j, i)
            nefffds = max(totrecfields(j, i), ciodynfwcix(j, i))
            write(10, err = 3010)
     +        (ciofminv(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofmaxv(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofwsc0(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofwsc1(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofrsc0(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofrsc1(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofwca(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofwcb(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofrca(k, j, i), k = 1, nefffds)
            write(10, err = 3010)
     +        (ciofrcb(k, j, i), k = 1, nefffds)
          enddo
c
c         Saving the specifications on which particles are to be saved
c         in the corresponding cio file.
c
          write(10, err = 3010) anypinfile(i)
          write(10, err = 3010)
     +      (allpclesave(k, i), k = -maxpcle, maxncode)
c
        enddo
      endif
c
      if (nhwciofiles .gt. 0) then
        write(10, err = 3010) (ciofiles(i), i = 1, nhwciofiles)
      endif
c
c     Saving statistical variables related with cio particle buffers.
c
      call ciostatsave(10, irc)
      if (irc .ne. 0) goto 3010
c
c     Saving stack names, processing modes, etc.
c
      write(10, err = 3010) npstacks, minpstack
      write(10, err = 3010) (psta_mode(i), i = 1, npstacks)
      write(10, err = 3010) (psta_n(i), i = 0, npstacks)
      write(10, err = 3010) (psta_model_n(i), i = 1, npstacks)
      write(10, err = 3010) ((psta_rn(j, i), j = 1, 2),
     +                       i = 1, npstacks)
c
c     Saving stack statistical data and related variables.
c
      write(10, err = 3010) (avgtotsize(i), i = 1, npstacks)
      write(10, err = 3010) ((peakstsize(j, i), j = 1, 3),
     +                       i = 1, npstacks)
c
c     The following patch ensures compatibility with old integer
c     "procentries".
c
      k = -3456789
      write(10, err = 3010) ((k, j = 1, 3), i = 1, npstacks)
      write(10, err = 3010) ((procentries(j, i), j = 1, 3),
     +                       i = 1, npstacks)
c
      write(10, err = 3010) ((hardswrite(j, i), j = 1, 3),
     +                       i = 1, npstacks)
      write(10, err = 3010) ((hardsread(j, i), j = 1, 3),
     +                       i = 1, npstacks)
      write(10, err = 3010) ((callcounter(j, i), j = 1, 2),
     +                       i = 0, npstacks)
c
      write(10, err = 3010) ((totpcles(j, i), j = 1, 5),
     +                       i = 0, npstacks)
      write(10, err = 3010) ((eloss(j, i), j = 1, 5),
     +                       i = 0, npstacks)
      write(10, err = 3010) ((nplost(j, i), j = 1, 5),
     +                       i = 0, npstacks)
      write(10, err = 3010) ((elost(j, i), j = 1, 5),
     +                       i = 0, npstacks)
      write(10, err = 3010) ((nplowe(j, i), j = 1, 5),
     +                       i = 0, npstacks)
      write(10, err = 3010) ((elowe(j, i), j = 1, 5),
     +                       i = 0, npstacks)
      write(10, err = 3010) ((nprgnd(j, i), j = 1, 5),
     +                       i = 0, npstacks)
      write(10, err = 3010) ((eprgnd(j, i), j = 1, 5),
     +                       i = 0, npstacks)
c
c     Information about unphysical particles, first interaction, etc.
c
      write(10, err = 3010) (nnotap(j), j = 1, 5)
      write(10, err = 3010) (enotap(j), j = 1, 5)
      write(10, err = 3010) (nneutrino(j), j = 1, 5)
      write(10, err = 3010) (eneutrino(j), j = 1, 5)
      write(10, err = 3010) (aveprim(j), j = 1, 5)
      write(10, err = 3010) ((fstintdp(j, i), j = 1, 5), i = 1, 2)
c
      write(10, err = 3010) (fstposdp(i), i = 1, 5), fstdpmanual
      write(10, err = 3010) fstintnotset, fstintauto, specialprim,
     +                      specialprimlab, nspecialprim
c
c     Writing a record of zeros to allocate some spare fields.
c     The first field is now used to identify the format of the
c     shower maximum data file.
c
      k = 6
      i = 0
      write(10, err = 3010) k, (i, j = 1, 9)
c
c     SAVING DATA STORED IN OTHER INTERNAL FILES.
c
c     Remarks.
c
      write(10, err = 3010) remark
      if (remark) then
c
c       Opening the remarks file.
c
        intut = rmkut
        open(rmkut, file = rmkfn, status = 'OLD',
     +              form = 'UNFORMATTED', err = 3020)
c
 1010   continue
        read(rmkut, err = 3020, end = 1020) i
        write(10, err = 3010) i
c
        if (i .gt. 0) then
          read(rmkut, err = 3020, end = 1020) auxline(1:i)
          write(10, err = 3010) auxline(1:i)
        endif
        goto 1010
 1020   continue
        close(rmkut)
        i = -99999
        write(10, err = 3010) i
c
      endif
c
c     Listing of shower maximum data and related quantities (shower per
c     shower).
c
      lsh     = -1
      rfields = nxfitpar + nshff * nlhtables + 5
c
c     Opening the corresponding internal file.
c
      open(shzut, file = shzfn, status = 'OLD',
     +            form = 'UNFORMATTED', err = 1030)
c
c     Copying the file header.
c
      read(shzut, err = 1030, end = 1030)
     +     i0, i, lshr, kfields, rfields, (obslevscrai(j), j = 21, 26)
      write(10, err = 3010) i0, i, lshr, kfields, rfields,
     +                      (obslevscrai(j), j = 21, 26)
      if (pershowertables) then
        j = 31
        do i = 1, obslevscrai(21)
          read(shzut, err = 1030, end = 1030)
     +                 (obslevscrai(j + l), l = 1, 10)
          write(10, err = 3010)
     +                 (obslevscrai(j + l), l = 1, 10)
          obslevscrai(j + 1) = obslevscrai(j + 1) * obslevscrai(j + 2)
          j = j + 10
        enddo
        read(shzut, err = 1030, end = 1030) i
        write(10, err = 3010) i
      endif
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
              write(10, err = 3010)
     +          (obslevscraj(k), k = 1, kfields),
     +          (obslevscra1(k), k = 1, rfields)
              read(shzut, err = 1030, end = 1030)
     +          (obslevscrai(k), k = 1, 10)
              write(10, err = 3010) (obslevscrai(k), k = 1, 10)
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
                  write(10, err = 3010) (obslevscra4(l), l = i1, i2)
                enddo
                j = j + 10
              enddo
c
c             End shower mark.
c
              read(shzut, err = 1030, end = 1030) i1
              write(10, err = 3010) i1
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
              write(10, err = 3010)
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
        call errprint(iep, '$A30', 3, 'idfwrite',
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
            write(10, err = 3010) i0, (obslevscra1(k), k = 1, rfields)
            write(10, err = 3010) i, (obslevscrai(k), k = 2, 10)
            j = 31
            do m = 1, obslevscrai(21)
              i1 = obslevscrai(j + 3)
              i2 = obslevscrai(j + 4)
              do k = 1, obslevscrai(j + 1)
                write(10, err = 3010) (obslevscra4(l), l = i1, i2)
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
            write(10, err = 3010) i0, (obslevscra1(k), k = 1, rfields)
          enddo
c
        endif
      endif
c
c     Writing trailing record.
c
      if (lastshower .gt. 0) write(10, err = 3010) lsh
c
      close(shzut)
c
c     Writing a record of zeros to allocate some spare fields.
c     Now the firs element is set to a positive value to signal that
c     the particle buffers related with compressed I/O files are
c     saved here.
c
      k = 1
      i = 0
      write(10, err = 3010) k, (i, j = 2, 10)
c
c     Saving the cio particle buffers.
c
      call ciobufsave(10, irc)
      if (irc .ne. 0) goto 3010
c
c     Saving the stacks.
c
      call stacp2s(10, irc)
c
      if (irc .eq. 5) goto 3010
      if (irc .eq. 8) call errprint(iep, '*', 3, 'idfwrite',
     +  'Error reading particle stacks. Stacks not completely saved.',
     +  0, 0, 0, 0.d0, ' ')
c
c     All done, closing the temporary file, deleting the existing
c     idf file (if any), and renaming the temporary one.
c
      close(10)
      i = leadingfnlen(2) + 4
      call rmfile(10, auxfilestring(1:i))
      call mvfile(auxfilestring(1:i+3), auxfilestring(1:i), irc)
      if (irc .ne. 0) goto 3010
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(iep, '$A09', 4, 'idfwrite', ' ',
     +              1, 10, 0, 0.d0,
     +              auxfilestring(1:leadingfnlen(2)+4))
c
      return
 3020 continue
c
      call errprint(iep, '$A30', 4, 'idfwrite',
     +              ' ', 1, intut, 0, 0.d0, ' ')
      return
c
 3030 continue
      call errprint(iep, '$A09', 4, 'idfwrite',
     + '(Error condition enabled during call to "modelidfwrite")',
     + 1, 10, 0, 0.d0, auxfilestring(1:leadingfnlen(2)+4))
c
      return
c
      end
c     --- End of routine idfwrite
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine idfput(iounit, nflt, fltvar, nint, intvar,
     +                  nlog, logvar, rc)
c
c     Auxiliary routine for saving internal variables into the IDF
c     file.
c     The saving operation is done with a special format, so the
c     section can be restored even when model information is not
c     available.
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
c     nflt........... (input, integer) Number of floating point data
c                     to save.
c     fltvar......... (input, double precision, array(nflt)) Floating
c                     point data.
c     nint........... (input, integer) Number of integer data to save.
c     intvar......... (input, integer, array(nint)) Integer data.
c     nlog........... (input, integer) Number of logical data to save.
c     logvar......... (input, logical, array(nlog)) Logical data.
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the saving operation is completed
c                     successfully. Nonzero otherwise.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iounit, nflt, nint, nlog, rc
      double precision  fltvar(nflt)
      integer           intvar(nint)
      logical           logvar(nlog)
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Saving the variable together with array size information.
c
      write(iounit, err = 3010) nflt, nint, nlog
c
      if (nflt .gt. 0) then
        write(iounit, err = 3010) (fltvar(i), i = 1, nflt)
      endif
c
      if (nint .gt. 0) then
        write(iounit, err = 3010) (intvar(i), i = 1, nint)
      endif
c
      if (nlog .gt. 0) then
        write(iounit, err = 3010) (logvar(i), i = 1, nlog)
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
c     --- End of routine idfput
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciostatsave(iounit, rc)
c
c     Saving the statistical counters related with the cio particle
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
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the saving operation is completed
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
      integer           iounit, rc
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
c
c     FIRST EXECUTABLE STATEMENT
c
c     Saving the ground particle buffer related data.
c
      write(iounit, err = 3010) ngndlowp
      write(iounit, err = 3010) ngndhighp
      write(iounit, err = 3010) egndlowp
      write(iounit, err = 3010) egndhighp
c
c     Saving statistical data associated with the other cio particle
c     buffers.
c
      write(iounit, err = 3010) nciofiles
      if (nciofiles .gt. 1) then
        write(iounit, err = 3010)
     +                (((noplowp(j, k, i), j = 1, 5), k = 1, 2),
     +                                     i = 2, nciofiles)
        write(iounit, err = 3010)
     +                ((nophighp(j, i), j = 1, 5), i = 2, nciofiles)
        write(iounit, err = 3010)
     +                (((eoplowp(j, k, i), j = 1, 5), k = 1, 2),
     +                                     i = 2, nciofiles)
        write(iounit, err = 3010)
     +                ((eophighp(j, i), j = 1, 5), i = 2, nciofiles)
      endif
c
      rc = 0
      return
c
c     Error exit
c
 3010 continue
      rc = 64
      return
c
      end
c     --- End of routine ciostatsave
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciobufsave(iounit, rc)
c
c     Saving the particle buffers associated with compressed I/O files.
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
c     rc............. (output, integer) Return code. It must be set
c                     to zero if the saving operation is completed
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
      integer           iounit, rc
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l
c
c     FIRST EXECUTABLE STATEMENT
c
c     Saving the ground particle buffer.
c
      write(iounit, err = 3010) npgndbuff
      if (npgndbuff .gt. 0) then
        write(iounit, err = 3010)
     +       ((fgndbuff(j, i), j = 1, maxstalen), i = 1, npgndbuff)
        write(iounit, err = 3010) (gndbuffsta(i), i = 1, npgndbuff)
      endif
c
c     Saving the particle buffers associated with other compressed
c     files.
c
      write(iounit, err = 3010) nciofiles
      if (nciofiles .gt. 1) then
        write(iounit, err = 3010) (npopbuff(k), k = 2, nciofiles)
        do k = 2, nciofiles
          l = npopbuff(k)
          if (l .gt. 0) then
            write(iounit, err = 3010)
     +           (opbuffsta(i, k), i = 1, l)
            write(iounit, err = 3010)
     +           ((opbuffiaux(j, i, k), j = 1, nopauxi), i = 1, l)
            write(iounit, err = 3010)
     +           ((opbufffaux(j, i, k), j = 1, nopauxf), i = 1, l)
            write(iounit, err = 3010)
     +           ((fopbuff(j, i, k), j = 1, maxstalen), i = 1, l)
          endif
        enddo
      endif
c
      rc = 0
      return
c
c     Error exit
c
 3010 continue
      rc = 66
      return
c
      end
c     --- End of routine ciobufsave
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'idfwrite.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
