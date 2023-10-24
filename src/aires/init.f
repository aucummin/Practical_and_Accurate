c
c     FILE: init.f                          Creation date: 18/JUN/1996.
c                                       LAST MODIFICATION: 05/MAY/2004.
c
c     This file contains several initialization routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine init0
c
c     Initializations to be performed before scanning the input file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2001, 2003;
c                                Fermilab 2003; La Plata 2004.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'compildat.f'
      include 'initpar.f'
      include 'kernelpar.f'
      include 'pclepar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, i1, i2, ncodes, pgcode, llen, irc, np
      double precision  ftmp1, ftmp2
      integer           npstacks0
      integer           stackf(npstacks)
      double precision  stackw(npstacks)
      character*32      stpclenames(npstacks)
      double precision  e2gev
c
c     FIRST EXECUTABLE STATEMENT
c
c     Initializing the basic units: Unit conversion, particle data
c     systems, etc.
c
      call basicstart0
c
c     Global program data.
c
      compildate      = aires_cdate
      compilwho       = aires_cwho
      compilstar      = aires_cstar
c
      adfnmergedfiles = 0
c
c     No global variables are defined by default.
c
      do i = 1, 2
        nglobar(i)      = 0
        globdfend(0, i) = 0
        globstrlen(i)   = 0
        globarstring(i) = ' '
      enddo
      thereareglobars = .false.
c
c     Default brackets for global variable substitution.
c
      bracketson = .true.
      opbr       = defopbr
      clbr       = defclbr
      brec       = labelchar
c
c     Setting some variables related with parameters that
c     do not accept default values (And also some related
c     quantities).
c
      mtotshowers     = 0
      firstshowernon1 = 0
      runshowers      = -1
      cpuperrun       = -1
      processjobs     = -1
      totshwset       = 0
      firstshowerset  = 0
      runshwset       = 0
      cpurunset       = 0
      processjobsset  = 0
c
      nforcextset  = 0
      notforcext   = .false.
c
      nshprimary   = 0
      pryeminset   = 0
      sprimlog     = 0
c
      atmoslabel   = 0
      atmoslset    = 0
c
c     Default basic parameters and file names.
c
      do i = 1, mxwdir
        wdirname(i)    = ' '
        wdirnamelen(i) = 0
      enddo
c
      inputsearchpath = ' '
      searchpathlen   = 0
      taskname        = 'GIVE_ME_A_NAME_PLEASE'
      tasknamelen     = 21
      tasknameset     = 0
      tasknamever     = 0
      runforcein      = .false.
      remark          = .false.
      fullinputlist   = .false.
      fulloutputlist  = .false.
      sryfn           = sryext
      adfile          = .false.
      sryison         = .true.
      tssison         = .false.
      stackinfo       = .false.
      texfn           = texext
      latexsry        = .false.
      inpcheckonly    = .false.
      idlerrsev       = 3
c
      if (pgmcode .lt. 2000) then
        idlerrsev2 = idlerrsev
      else
        idlerrsev2 = 2
      endif
c
      tableindex      = .false.
      icommentchar    = ichar('#')
c
c     Assigning the input directive names.
c     Synonyms are stored as duplicate commands codes.
c
      ncommands = 0
c
c     Other initializations related with input directives.
c
      sidatastring = '#'
c
c     Section 1: Basic and input management directives.
c
      call addirective('Exit'              , -07,  3)
      call addirective('x'                 , -07,  1)
      call addirective('End'               , -05,  1)
c
      call addirective('Remark'            , 010,  3)
      call addirective('SetGlobal'         , 012,  5)
      call addirective('DelGlobal'         , 014,  5)
      call addirective('Import'            , 016,  3)
      call addirective('ImportShell'       , 017,  8)
      call addirective('Brackets'          , 018,  3)
      call addirective('Skip'              , 020,  3)
      call addirective('Input'             , 022,  3)
      call addirective('InputPath'         , 024,  6)
      call addirective('Echo'              , 025,  4)
      call addirective('Trace'             , 026,  5)
      call addirective('CheckOnly'         , 028,  5)
      call addirective('StopOnError'       , 029,  9)
      call addirective('ForceInit'         , 030,  8)
      call addirective('Prompt'            , 032,  4)
      call addirective('Help'              , 034,  2)
      call addirective('help'              , 034,  2)
      call addirective('?'                 , 035,  1)
      call addirective('ForceModelName'    , 040,  8)
      call addirective('Shell'             , 042,  2)
c
c     Section 2: Initial conditions and run control.
c
      call addirective('TaskName'          , 210,  4)
      call addirective('TotalShowers'      , 212,  7)
      call addirective('FirstShowerNumber' , 213,  7)
      call addirective('ShowersPerRun'     , 214,  8)
      call addirective('MaxCpuTimePerRun'  , 216,  6)
      call addirective('RunsPerProcess'    , 218,  8)
      call addirective('DumpFile'          , 220,  4)
c
c     Section 3: Physical and math. quantities (I).
c
      call addirective('PrimaryParticle'   , 410,  7)
      call addirective('PrimaryEnergy'     , 412,  8)
      call addirective('PrimaryZenAngle'   , 414,  8)
      call addirective('PrimaryAzimAngle'  , 416,  8)
      call addirective('ThinningEnergy'    , 418,  4)
      call addirective('InjectionAltitude' , 420,  6)
      call addirective('InjectionDepth'    , 420, 10)
      call addirective('GroundAltitude'    , 422,  6)
      call addirective('GroundDepth'       , 422,  7)
      call addirective('Atmosphere'        , 424,  5)
      call addirective('GeomagneticField'  , 426,  6)
      call addirective('Site'              , 428,  4)
      call addirective('AddSite'           , 430,  7)
      call addirective('Date'              , 432,  4)
      call addirective('AddSpecialParticle', 434,  7)
      call addirective('SpecialParticLog'  , 436, 14)

c     Added by Matias Tueros on Nov 2011 for ZHAireS IDL
      call addirective('AddAntenna'        , 438, 10)
      call addirective('AddFrequency'      , 440, 12)
      call addirective('AddFraunhofrAngle' , 442, 15)
      call addirective('AddFraunhofrPhi'   , 444, 15)
      
c     End addition            

c
c     Section 3b: Physical and math. quantities (II).
c
      call addirective('RandomSeed'        , 490,  6)
c
c     Section 4: Output control.
c
      call addirective('InputListing'      , 610,  8)
      call addirective('OutputListing'     , 611,  9)
      call addirective('SaveInFile'        , 612,  4)
      call addirective('SaveNotInFile'     , 614,  7)
      call addirective('RecordObsLevels'   , 615,  8)
      call addirective('SeparateShowers'   , 616,  9)
      call addirective('FileDirectory'     , 618,  7)
      call addirective('ObservingLevels'   , 620,  9)
      call addirective('ELimsTables'       , 622,  6)
      call addirective('RLimsTables'       , 624,  6)
      call addirective('RLimsFile'         , 626,  6)
      call addirective('StackInformation'  , 628,  6)
      call addirective('PrintTables'       , 630,  5)
      call addirective('ExportTables'      , 632,  5)
      call addirective('ExportPerShower'   , 633,  7)
      call addirective('TableIndex'        , 634,  6)
      call addirective('Summary'           , 636,  3)
      call addirective('TSSFile'           , 637,  3)
      call addirective('LaTeX'             , 638,  3)
      call addirective('CommentCharacter'  , 640, 11)
      call addirective('ADFile'            , 642,  3)
c
      if (ncommands .gt. maxccodes) call errprint(0, '$A37', 4,
     +                              'init0', ' ', 0, 0, 0, 0.d0, ' ')
c
c     All the "hardwired" directives are "vid" (Very important
c     directives):
c
      do i = 1, ncommands
        veryimpdir(i) = .true.
        wngonset(i)   = .false.
        clgname(i)    = ' '
        aditem(i)     = 0
      enddo
c
c     Setting the number of float and logical data used in connection
c     with the hardwired directives (several spare positions are left
c     for future use), and related quantities.
c
      nfidata   = 60
      niidata   = 20
      nlidata   = 10
      nsidata   = 10
      sidatalen = 31
c
c     End of "hardwired" directives.
c
c     Setting assignment flags to "not assigned"
c
      do i = 1, mxfidata
        fidatawaset(i) = 0
      enddo
      do i = 1, mxiidata
        iidatawaset(i) = 0
      enddo
      do i = 1, mxlidata
        lidatawaset(i) = 0
      enddo
      do i = 1, mxsidata
        sidatawaset(i) = 0
      enddo
c
c     Preparing for definitions of "dynamical" directives.
c
      ncommands0 = ncommands + 1
      nfidata0   = nfidata + 1
      niidata0   = niidata + 1
      nlidata0   = nlidata + 1
      nsidata0   = nsidata + 1
      sidatalen0 = sidatalen + 1
c
      do i = 1, 7
        lastdcode(i) = 609 + 200 * i
      enddo
c
c     Defining some internal input data directives.
c
      call newinpstring('PerShowerData', 9,
     +                  'Individual shower data', 2,
     +                  'None Brief Full', ' ', 1,
     +                  'Brief', 'Full')
c
      ftmp2 = e2gev(80.d0, 'KeV', i)
      call newinpreal(4, 'GammaCutEnergy', 8,
     +                'Cut energy for gammas', 2,
     +                ftmp2, 1, ftmp2, 0.d0)
c
      ftmp2 = e2gev(80.d0, 'KeV', i)
      call newinpreal(4, 'ElectronCutEnerg', 11,
     +                'Cut energy for e+ e-', 2,
     +                ftmp2, 1, ftmp2, 0.d0)
c
      ftmp1 = e2gev(10.d0, 'MeV', i)
      ftmp2 = e2gev(500.d0, 'KeV', i)
      call newinpreal(4, 'MuonCutEnergy', 7,
     +                'Cut energy for mu+ mu-', 2,
     +                ftmp1, 1, ftmp2, 0.d0)
c
      ftmp1 = e2gev(60.d0, 'MeV', i)
      ftmp2 = e2gev(500.d0, 'KeV', i)
      call newinpreal(4, 'MesonCutEnergy', 8,
     +                'Cut energy for mesons', 2,
     +                ftmp1, 1, ftmp2, 0.d0)
c
      ftmp1 = e2gev(120.d0, 'MeV', i)
      ftmp2 = e2gev(500.d0, 'KeV', i)
      call newinpreal(4, 'NuclCutEnergy', 7,
     +                'Cut energy for nucleons', 2,
     +                ftmp1, 1, ftmp2, 0.d0)
c
      call newinpswitch('SetTimeAtInjection', 7,
     +                  'Time starts at injection', 0,
     +                  .true., .true.)
c
      call newinpswitch('SetTopAtInjection', 6,
     +                  'Top surf. fixed at injection', 0,
     +                  .true., .true.)
c
      call newinpswitch('PropagatePrimary', 10,
     +                  'Initial primary propagation', 0,
     +                  .true., .true.)
c
      call newinpswitch('RecordSpecPrimaries', 11,
     +                  'Record special primaries', 2,
     +                  .true., .true.)
c
      call newinpint('SPMaxFieldsToAdd', 11,
     +               'Max fields added by ext module', 0,
     +               16, 3, 4, min(mxintfltvar, 600))
c
      call newinpreal(1, 'ResamplingRatio', 6,
     +                'Resampling ratio', 2,
     +                prsratio0def, 3, 1.d0, 250.d0)
c
      call newinpstring('ForceLowEDecays', 10,
     +                  'Force low egy. decays', 0,
     +                  'Normal Always Never', ' ', 2,
     +                  'Normal', 'Always')
c
      call newinpstring('ForceLowEAnnihilation', 10,
     +                  'Force low egy. annihilation', 0,
     +                  'Normal Always Never', ' ', 2,
     +                  'Normal', 'Always')

c Added by Matias Tueros Nov 2011 for ZHS IDL integration
      call newinpswitch('ZHAireS',7,
     +                  'ZHAireS Radio Routines', 2,
     +                  .false.,.true.)
c
      call newinpswitch('FresnelFreq', 11,
     +                  'Fresnel Frequency', 0,
     +                  .false.,.true.)
c     
      call newinpswitch('FraunhoferFreq', 14,
     +                  'Fraunhofer Frequency', 0,
     +                  .false.,.true.)
c
      call newinpswitch('FresnelTime', 11,
     +                  'Fresnel Time', 0,
     +                  .false.,.true.)
c     
      call newinpswitch('FraunhoferTime', 14,
     +                  'Fraunhofer Time', 0,
     +                  .false.,.true.)    
c
      call newinpswitch('IncludeHadrons', 14,
     +                  'ZHAires includes Hadrons', 2,
     +                  .false.,.true.)
c
      call newinpswitch('ConstRefrIndex', 14,
     +                  'Constant Refraction Index', 1,
     +                  .false.,.true.)
c         
      call newinpreal(1, 'RefractionIndex', 15,
     +                'Index of Refraction', 0,
     +                1.000325d0, 3, 0.d0, 2.d0) 
c
      call newinpreal(3, 'TimeDomainBin', 13,
     +                'Time Domain Bin Size', 0,
     +                 0.50d-9, 1, 0.0d0, 250.d0) 
c         
c end addition
c         
      print *, 'Nov 2011: Add. ZHAireS radio emission'
c end addition
c
c     Initializing the thinning algorithm.
c
      call thininit0(thinningon)
      lidatawaset(2) = 1
c
c     Initializing the library of sites.
c     (0 corresponds to the default site: Site00).
c
      nlibsites = -1
c
      call addlibsite('Site00', 0.0d0, 0.0d0, 0.0d0, i)
c
c     Some well-known sites (Name, latitude, longitude, ground level).
c
      call addlibsite('SouthPole'    ,   -90.0d0,    0.0d0, 3127.d0, i)
      call addlibsite('ElNihuil'     ,   -35.2d0,  -69.2d0, 1400.d0, i)
      call addlibsite('Millard'      ,    39.1d0, -112.6d0, 1400.d0, i)
      call addlibsite('AGASA'        , 35.7833d0, 138.50d0, 900.0d0, i)
      call addlibsite('CASKADE'      , 49.0925d0, 8.8758d0, 112.0d0, i)
      call addlibsite('FlysEye'      ,    41.0d0, -112.0d0, 850.0d0, i)
      call addlibsite('Dugway'       ,    40.0d0, -113.0d0, 1550.d0, i)
      call addlibsite('ElBarreal'    ,    31.5d0, -107.0d0, 1200.d0, i)
      call addlibsite('HaverahPark'  ,   53.97d0, -1.637d0,  220.d0, i)
      call addlibsite('Puebla'       ,    19.5d0,  -98.0d0, 2200.d0, i)
      call addlibsite('SydneyArray'  ,   -30.5d0, -149.6d0,  250.d0, i)
      call addlibsite('Yakutsk'      ,    61.7d0,  129.4d0,  850.d0, i)
c
c     Setting output variables related data.
c
      nallodata = 0
      nfodata   = 0
      niodata   = 0
      nlodata   = 0
c
      nallodata0 = nallodata + 1
      nfodata0   = nfodata + 1
      niodata0   = niodata + 1
      nlodata0   = nlodata + 1
c
c     Defining the tables of observables (histograms).
c
      call initable
c
c     Calling the model initialization routine
c
      do i = 1, npstacks
        stackf(i) = 0
        stackw(i) = 1
      enddo
      npstacks0 = npstacks
c
      call modelini0(npstacks0, stackf, stackw, stpclenames)
c
c     Assigning to the particles their corresponding stacks.
c     Many particle specifications, separated by blanks are
c     allowed for each stack.
c
      llen = len(stpclenames(1))
      do i = npstacks0, 1, -1
        i2 = 0
        np = 0
 1010   continue
        call getpclecode(.true., stpclenames(i), llen, i1, i2,
     +                   ncodes, pgcode, irc)
c
        if (irc .eq. 8) goto 1020
        np = np + 1
        if ((irc .le. 3) .and. (irc .ne. 2)) then
          if (irc .ne. 3) then
c
c           Single particle specified
c
            allpclesta0(pgcode) = i
c
          else
c
c           Particle group specified
c
            if (ncodes .eq. -1) then
c
c             This corresponds to the "remaining particles" group.
c             All particles not assigned a stack up to now go to
c             the present one.
c
              do j = -maxpcle, maxncode
                if (allpclesta0(j) .lt. i) allpclesta0(j) = i
              enddo
c
            else if (ncodes .gt. 0) then
c
              if (pgcode .le. maxpgroup) then
c
c               A significative normal group was specified.
c
                do j = -maxpcle, maxncode
                  if (allpgroup(pgcode, j)) allpclesta0(j) = i
                enddo
c
              else
c
c               A "generic" group was specified.
c
                pgcode = pgcode - maxpgroup
                do j = 1, gengrpsize(pgcode)
                  allpclesta0(gengrppcle(j - 1, pgcode)) = i
                enddo
c
              endif
            endif
          endif
c
        else
          call errprint(0, '$A44', 4, 'init0', ' ',
     +                  1, i, 0, 0.d0, stpclenames(i)(i1:i2))
        endif
        goto 1010
 1020   continue
        if (np .le. 0) then
          call errprint(0, '$A44', 4, 'init0', ' ',
     +                  1, i, 0, 0.d0, stpclenames(i)(i1:i2))
        endif
      enddo
c
c     Setting stack categories and sizes (this last setting will be
c     used for the first shower only).
c
      call setstacat(npstacks, stackf)
      call resizesta(npstacks, stackw)
c
c     Initialization of processing and output parameters.
c
      obslevset  = 0
      nttabins   = tdbins
      nttabinsp1 = nttabins + 1
      nttabinsp2 = nttabins + 2
c
      return
      end
c     --- End of routine init0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine init1
c
c     Initializations to be performed after scanning the input file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
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
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      logical           wng
      logical           texspecial
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the existence of TeX special characters.
c
      if (latexsry) then
c
        wng = .false.
        do i = 1, tasknamelen
          if (texspecial(taskname(i:i), j)) then
            if (j .gt. 0) wng = .true.
          endif
        enddo
c
        if (wng) call errprint(0, '*', 2, 'init1',
     +    'Task name contains TeX special characters.$' //
     +    'LaTeX file may be unprocessable.',
     +    0, 0, 0, 0.d0, taskname(1:tasknamelen))
c
      endif
c
c     Resetting basic parameters and file names.
c
      if (wdirnamelen(1) .gt. 0) then
        do i = 2, mxwdir - 1
          if (wdirnamelen(i) .le. 0) then
            wdirname(i)    = wdirname(1)
            wdirnamelen(i) = wdirnamelen(1)
          endif
        enddo
      endif
c
      do i = 1, mxwdir - 1
        call absfnset(wdirname(i), wdirnamelen(i),
     +                taskname, tasknamelen,
     +                leadingfn(i), leadingfnlen(i))
      enddo
c
      call absfnset(wdirname(mxwdir), wdirnamelen(mxwdir),
     +              randomfnh, len(randomfnh),
     +              leadingfn(mxwdir), leadingfnlen(mxwdir))
c
c     Setting some file management related data.
c
      lgfisclosed = .true.
      mmacutok    = .true.
c
      return
      end
c     --- End of routine init1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine init3(ouflag)
c
c     Initializations to be performed before starting an ulterior
c     simulation job.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001, 2003.
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c                      In this routine, additionaly, ouflag
c                      equal to zero will mean that the particle
c                      stacks will not be read in from the
c                      internal dump file, and reassignation of
c                      dynamical variables is not carried on.
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
      integer           ouflag
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, irc, aux1, auxv(2)
      equivalence       (aux1, auxv(1))
      character*64      auxstring
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting data related to dynamical variables.
c
c     Opening the idf file and reading it.
c
      call idfread(' ', ouflag, 4, irc)
c
      if ((ouflag .ne. 0) .or. inpcheckonly) then
c
c       Checking if any filename variable was modified.
c
        if (wdirname(1)(1:wdirnamelen(1)) .ne.
     +      xwdirname(1:xwdirnamelen)) then
          call errprint(ouflag, '$A32', 2, 'init3',
     +                  wdirname(1)(1:max(1, wdirnamelen(1))),
     +                  0, 0, 0, 0.d0,
     +                  xwdirname(1:max(1, xwdirnamelen)))
        endif
c
        if ((taskname(1:tasknamelen) .ne. xtaskname(1:xtasknamelen))
     +      .or. (tasknamever .ne. xtasknamever)) then
          auxv(1) = tasknamever
          auxv(2) = xtasknamever
          call errprint(ouflag, '$A33', 2, 'init3',
     +                  taskname(1:max(1, tasknamelen)),
     +                  2, auxv, 0, 0.d0,
     +                  xtaskname(1:max(1, xtasknamelen)))
        else
          tasknameset = xtasknameset
        endif
c
c       Checking if any "dynamic" variable was modified.
c
        if (mtotshowers .ne. xtotshowers) then
          call putlog(ouflag, .true.,
     +                'Changing total number of showers.')
          write(auxstring, 2010) 'From: '
          call intnice(xtotshowers, 1, auxstring(7:64), i)
          aux1 = i + 11
          write(auxstring(aux1-4:aux1), 2010) ' to: '
          call intnice(mtotshowers, 1, auxstring(aux1+1:64), i)
          aux1 = aux1 + i
          call putlog(ouflag, .false., auxstring(1:aux1))
        endif
 2010   format(a)
c
        if (runshowers .ne. xrunshowers) then
          call putlog(ouflag, .true.,
     +                'Changing maximum number of showers per run.')
          write(auxstring, 2010) 'From: '
          call intnice(xrunshowers, 1, auxstring(7:64), i)
          aux1 = i + 11
          write(auxstring(aux1-4:aux1), 2010) ' to: '
          call intnice(runshowers, 1, auxstring(aux1+1:64), i)
          aux1 = aux1 + i
          call putlog(ouflag, .false., auxstring(1:aux1))
          
        endif
c
        if (processjobs .ne. xprocessjobs) then
          call putlog(ouflag, .true.,
     +                'Changing number of runs per process.')
          write(auxstring, 2010) 'From: '
          call intnice(xprocessjobs, 1, auxstring(7:64), i)
          aux1 = i + 11
          write(auxstring(aux1-4:aux1), 2010) ' to: '
          call intnice(processjobs, 1, auxstring(aux1+1:64), i)
          aux1 = aux1 + i
          call putlog(ouflag, .false., auxstring(1:aux1))
        endif
c
        if (cpuperrun .ne. xcpuperrun) then
          call putlog(ouflag, .true.,
     +                'Changing maximum cpu time per run.')
          write(auxstring, 2010) 'From: '
          call tnice(xcpuperrun, 1, auxstring(7:64), i)
          aux1 = i + 11
          write(auxstring(aux1-4:aux1), 2010) ' to: '
          call tnice(cpuperrun, 1, auxstring(aux1+1:64), i)
          aux1 = aux1 + i
          call putlog(ouflag, .false., auxstring(1:aux1))
        endif
c
        usedefaultd = (runshwset .le. 0) .or. (cpurunset .le. 0) .or.
     +                (processjobsset .le. 0) .or.
     +                (tasknameset .le. 0)
        usedefault  = usedefaultd .or. usedefaults
c
c       Calling the atmospheric model initializing routine.
c
        call atmosinit(max(1, atmoslabel), atmosmodel)
c
c       Checking if the particle stacks were successfully read.
c
        if (irc .gt. 0) then
          call errprint(ouflag, '*', 3, 'init3',
     +    'Error reading particle stacks from idf file.$'//
     +    'Assuming empty stacks.',
     +    0, 0, 0, 0.d0, ' ')
c
          shcomplete = .false.
        endif
c
      else
c
c       Setting dynamical variables from their values obtained
c       from the idf file.
c
        call setdynfromidf
c
c       Calling the atmospheric model initializing routine.
c
        call atmosinit(max(1, atmoslabel), atmosmodel)
c
      endif
c
c     Synchronizing the random number generator with the already read
c     status variables.
c
      call ranauxsync
c
      return
      end
c     --- End of routine init3
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine addirective(dirname, dircode, mindirlen)
c
c     Adding a new directive to the directive arrays.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Directive name. If longer
c                      than the maximum allowed, it is truncated.
c     dircode......... (input, integer) Directive code.
c     mindirlen....... (input, integer) Number of characters of the
c                      minimum acceptable abbreviation for the
c                      directive name.
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
      character*(*)     dirname
      integer           dircode, mindirlen
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      ncommands = ncommands + 1
      cname(ncommands)   = dirname
      ccode(ncommands)   = dircode
      minclen(ncommands) = mindirlen
c
      return
      end
c     --- End of routine addirective
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine addlibsite(name, lat, long, grd, irc)
c
c     Adding a new site to the site library.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     name............ (input, character*(*)) Site name. Cannot be more
c                      than 16 characters long.
c     lat............. (input, double precision) Site latitude, in
c                      degrees, from -90 to 90.
c     long............ (input, double precision) Site longitude, in
c                      degrees, from -180 to 180.
c     grd............. (input, double precision) Ground altitude, in
c                      m.a.s.l., from 0 to 100 km.
c     irc............. (output, integer) Return code. Zero means normal
c                      return.
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
      character*(*)     name
      double precision  lat, long, grd
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, l
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking input parameters.
c
      if (nlibsites .ge. mxlibsites) then
        call errprint(0, '$A37', 3, 'addlibsite',
     +       '(Site library is full).', 0, 0, 0, 0.d0, ' ')
        irc = 10
        return
      endif
c
      irc = 5
      if ((lat .lt. -90) .or. (lat .gt. 90)) then
        call errprint(0, '$A20', 3, 'addlibsite',
     +       '(Latitude out of range)', 0, 0, 1, lat, ' ')
        return
      endif
c
      if ((long .lt. -180) .or. (long .gt. 180)) then
        call errprint(0, '$A20', 3, 'addlibsite',
     +       '(Longitude out of range)', 0, 0, 1, long, ' ')
        return
      endif
c
c     Checking the altitude. Negative values will be interpreted
c     as depths in g/cm2 and converted to altitudes immediately
c     after initializing the atmospheric model.
c
      if ((grd .lt. -1500.d0) .or. (grd .gt. 100.d3)) then
        call errprint(0, '$A20', 3, 'addlibsite',
     +       '(Altitude/depth out of range)', 0, 0, 1, grd, ' ')
        return
      endif
c
c     Searching repetitions.
c
      l = min(16, len(name))
c
      do i = 0, nlibsites
        if (name(1:l) .eq. sitename(i)(1:l)) then
          call errprint(0, '$A20', 3, 'addlibsite',
     +         '(Name already exists)', 0, 0, 1, grd, name(1:l))
          return
        endif
      enddo
c
c     Everything OK. Assigning the new site to the library.
c
      nlibsites = nlibsites + 1
c
      sitename(nlibsites) = name
      sitenlen(nlibsites) = l
      sitelat(nlibsites)  = lat
      sitelong(nlibsites) = long
      siteground(nlibsites) = grd
c
      irc = 0
      return
      end
c     --- End of routine addlibsite
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setstackw(sw)
c
c     Setting the stack weights.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2003.
c
c     Arguments:
c     =========
c
c     sw.............. (output, double precision, array(npstacks)) The
c                      stack weights, set accordingly with past stack
c                      usage.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      double precision  sw(npstacks)
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  sw0, sw1
c
c     FIRST EXECUTABLE STATEMENT
c
      sw0 = 0
      do i = 1, npstacks
        sw(i) = 1 + procentries(1, i)
        sw0   = sw0 + sw(i)
      enddo
c
      do i = 1, npstacks
        sw1 = sw(i) / sw0
        if (stacateg(i) .gt. 0) then
          sw1 = 2 + sw1
        else
          sw1 = 1 + 2 * sw1
        endif
        sw(i) = sw1 * peakstsize(1, i)
      enddo
c
      return
      end
c     --- End of routine setstackw
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'init.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
