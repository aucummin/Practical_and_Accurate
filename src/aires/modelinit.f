c
c     FILE: modelinit.f                     Creation date: 17/AUG/1996.
c                                       LAST MODIFICATION: 29/SEP/2005.
c
c     Routines to initialize the parameters used by the different
c     interaction models.
c
c     AIRES and MOCCA:
c
c     This version of the particle processing routines is based on the
c     well-known MOCCA air-shower simulation program, written by
c     A. M. Hillas (University of Leeds), released in May 1987 (but
c     with several posterior modifications). The differences with
c     the original MOCCA algorithms are the following:
c
c     1) The LPM effect procedures have been completely rewritten.
c     2) Kaon, eta meson and lambda baryon propagating capabilities.
c        These particles were not considered in MOCCA.
c     3) The heavy particle knock-on electron emission and continuous
c        energy losses procedures have been extensively modified.
c     4) Original muonic bremsstrahlung and pair production algorithms.
c        These processes were not considered in MOCCA.
c     5) Completely new implementation of the Hillas Splitting
c        Algorithm.
c
c     The routines to process particles include: Photon, electron and
c     positron, heavy neutral particles and heavy charged particles
c     related procedures; and auxiliary procedures placed in file
c     "modelutils.f"
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelini0(nstacks, stackcat, stackweight, stackpcles)
c
c     Initializing the different stack processing routines (I).
c     This routine is invoked at the beginning of every process, just
c     before beginning the scanning of the input data file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 2000, 2001, 2002;
c                                Fermilab 2003; La Plata 2005.
c
c
c     Arguments:
c     =========
c
c     nstacks........ (input, integer) The number of particle
c                     stacks currently defined in the kernel.
c     stackcat....... (output, integer, array(nstacks)) Stack
c                     categories. If stackcat(i) > 0 then the
c                     corresponding stack is taken into account
c                     by the size limiting system. The stacks
c                     not labelled this was are temporarily not
c                     processed when the size of the former ones
c                     exceeds certain limit.
c     stackweight.... (output, double precision, array(nstacks))
c                     Weights to set the size of the stacks for the
c                     first shower. The size of the stacks for the
c                     first shower will be set proportionally to the
c                     absolute value of the corresponding weights. For
c                     subsequent showers the stack sizes are set
c                     internally.
c     stackpcles..... (output, character*(*), array(nstacks))
c                     Description of the particles processed by each
c                     stack. Maximum length: 32 characters.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'schedulerpar.f'
      include 'modelpar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           nstacks
      integer           stackcat(nstacks)
      double precision  stackweight(nstacks)
      character*(*)     stackpcles(nstacks)
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  kemin, kenucmin
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the number of stacks.
c
      if (nstacks .ne. 4) call errprint(0, '*', 4, 'modelini1',
     +  'The number of stacks is not the expected one.',
     +  1, nstacks, 0, 0.d0, ' ')
c
c     Setting categories and weights.
c
      stackcat(gamma_sta)   = gamma_sta_cat
      stackcat(eplumin_sta) = eplumin_sta_cat
      stackcat(heavynt_sta) = heavynt_sta_cat
      stackcat(heavych_sta) = heavych_sta_cat
c
      stackweight(gamma_sta)   = gamma_sta_w
      stackweight(eplumin_sta) = eplumin_sta_w
      stackweight(heavynt_sta) = heavynt_sta_w
      stackweight(heavych_sta) = heavych_sta_w
c
c     Describing the particles processed at each stack.
c
      stackpcles(gamma_sta)   = gamma_sta_pcles
      stackpcles(eplumin_sta) = eplumin_sta_pcles
      stackpcles(heavynt_sta) = heavynt_sta_pcles
      stackpcles(heavych_sta) = heavych_sta_pcles
c
c     Defining model error messages.
c     Error messages are defined calling "mtxtappend". The meaning
c     of the different arguments is explained within the code
c     of the mentioned routine. Error identification strings are
c     4 character strings. Reserved words are: '*' (dummy error/
c     informative message) and all strings beginning with '$'
c     (reserved for Aires internal messages).
c
      call mtxtappnd(2, 'NSEC', 4,
     + 'Too many secondaries generated by collision routine.')
c
      call mtxtappnd(2, 'INVD', 4,
     + 'Invalid particle code in decay section.')
c
c     Switchs to select among different model options.
c
      call newinpswitch('LPMEffect', 3, 'LPM effect',
     +                  1, .true., .true.)
      call newinpswitch('DielectricSuppression', 13,
     +                  'Dielectric Suppression',
     +                  1, .true., .true.)
      call newinpswitch('MuonBremsstrahlung', 8, 'Muon bremsstrahlung',
     +                  1, .true., .true.)
      call newinpswitch('NuclCollisions', 8, 'Nuclear collisions',
     +                  1, .true., .true.)
      call newinpswitch('PhotoNuclear', 8, 'Photonuclear reactions',
     +                  1, .true., .true.)
c
c
c     Defining some input directives related with parameters
c     of the physical models.
c     For "newinpreal" calls: Argument number 6 is the default
c     value of the respective variable. If it is not adimensional,
c     it is expressed in the default unit: m (length), sec (time), GeV
c     (energy), g/cm2 (matter length); the first parameter indicates
c     the kind of variable: 1 number with no unit specification,
c     2 length, 3 time and 4 energy.
c
c     Energy thresholds.
c
      call newinpreal(4, 'GammaRoughCut', 6, 'Gamma rough egy. cut',
     +                1, 75.0d-6, 3, 45.0d-6, 5.d-3)
c
      call newinpreal(4, 'ElectronRoughCut', 9, 'e+e- rough egy. cut',
     +                1, 70.0d-6, 3, 45.0d-6, 5.d-3)
c
c     Medium parameters, etc.
c
      call newinpreal(1, 'AirZeff', 4, 'Effective Z for air',
     +                1, zeffdef, 1, 1.d0, 0.d0)
c
      call newinpreal(1, 'AirAvgZ/A', 9, 'Average Z/A for air',
     +                1, avzoveradef, 1, 0.d0, 0.d0)
c
      call newinpreal(1, 'AirRadLength', 7, 'Air radiation length',
     +                1, radlendef, 1, 0.1d0, 0.d0)
c
c     Output variables.
c
      call newoutint('High E had. coll. calls', 0, helenucalls(1))
      call newoutint('Low E had. coll. calls', 0, helenucalls(2))
      call newoutint('High E nuc. coll. calls', 0, helenucalls(3))
      call newoutint('Low E nuc. coll. calls', 0, helenucalls(4))
c
c     End of general definitions.
c
c     Setting the base model identification.
c
      propmodelid0 = 1000000
c
c     Calling the external model initialization routine (I).
c
      call extmodelini0(propmodelid0, kemin, kenucmin)
c
c     Energy threshold for hadronic collisions.
c
      call newinpreal(4, 'MinExtCollEnergy', 11,
     +                'Min. ext. collision energy',
     +                0, kemin, 1, kemin / 4, 0.d0)
c
c     Energy threshold for nucleus-nucleus collisions.
c
      call newinpreal(4, 'MinExtNucCollEnergy', 14,
     +                'Min. ext. nuc. coll. energy',
     +                0, kenucmin, 1, kenucmin / 4, 0.d0)
c
c     Energy threshold for external mean free paths.
c
      call newinpreal(4, 'MFPThreshold', 7, 'Hadronic MFP threshold',
     +                0, kemin, 1, 0.1d0, 0.d0)
c
      return
c
      end
c     --- End of routine modelini0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelinitask(nstacks, stackname, stackmode,
     +                        ldr, stackrname, modelname)
c
c     Initializing the different stack processing routines (II).
c     One per stack.
c     This routine is invoked at the beginning of a new task.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2002, 2003.
c
c
c     Arguments:
c     =========
c
c     nstacks........ (input, integer) The number of particle
c                     stacks currently defined in the kernel.
c     stackname...... (output, character*(*), array(nstacks)) Stack
c                     name. Maximum length: 42 characters.
c     stackmode...... (output, integer, array(nstacks)) Stack
c                     processing mode.
c     ldr............ (input, integer) Leading dimension of array
c                     stackrname.
c     stackrname..... (output, character*(*), array(ldr, nstacks))
c                     Names of the stack processing routines. Usually
c                     there are two routines per stack. Maximum
c                     length: 16 characters.
c     modelname...... (output, character*(*), array(nstacks))
c                     Name of the model used for each stack.
c                     Maximum length: 42 characters.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'schedulerpar.f'
c
c     Declaration of arguments.
c
      integer           nstacks, ldr
      character*(*)     stackname(nstacks)
      integer           stackmode(nstacks)
      character*(*)     stackrname(ldr, nstacks)
      character*(*)     modelname(nstacks)
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      extmname
      integer           lm
      double precision  getinpreal
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting stack processing modes.
c
      stackname(gamma_sta)       = gamma_sta_n
      stackmode(gamma_sta)       = gamma_sta_mode
      stackrname(1, gamma_sta)   = gamma_sta_rn1
      stackrname(2, gamma_sta)   = gamma_sta_rn2
c
      stackname(eplumin_sta)     = eplumin_sta_n
      stackmode(eplumin_sta)     = eplumin_sta_mode
      stackrname(1, eplumin_sta) = eplumin_sta_rn1
      stackrname(2, eplumin_sta) = eplumin_sta_rn2
c
      stackname(heavynt_sta)     = heavynt_sta_n
      stackmode(heavynt_sta)     = heavynt_sta_mode
      stackrname(1, heavynt_sta) = heavynt_sta_rn1
      stackrname(2, heavynt_sta) = heavynt_sta_rn2
c
      stackname(heavych_sta)     = heavych_sta_n
      stackmode(heavych_sta)     = heavych_sta_mode
      stackrname(1, heavych_sta) = heavych_sta_rn1
      stackrname(2, heavych_sta) = heavych_sta_rn2
c
c     Setting model names. This includes also some settings for
c     the external collision routine.
c
      call extmodelinitask(extmname, externalhcoll)
c
      if (externalhcoll) then
c
c       The external model is switched ON.
c
        call strim(16, extmname, lm)
c
        modelname(gamma_sta)       =
     +                 'Std. model for gammas + ' // extmname
        modelname(heavynt_sta)     =
     +                 'Std. model for neutral pcles + ' // extmname
        modelname(heavych_sta)     =
     +                 'Std. model for heavy pcles + ' // extmname
c
c       Setting the energy thresholds.
c
        highcollthresh(1) = getinpreal('MinExtCollEnergy')
        highcollthresh(2) = getinpreal('MinExtNucCollEnergy')
c
      else
c
c       The external model is switched OFF.
c
        modelname(gamma_sta)       = 'Aires std. model for gammas'
        modelname(heavynt_sta)     =
     +                 'Aires std. model for heavy neutral pcles.'
        modelname(heavych_sta)     =
     +                 'Aires std. model for heavy charged pcles.'
c
c       Setting the energy thresholds to "infinite" so the external
c       routines will never be called.
c
        highcollthresh(1) = 9.d35
        highcollthresh(2) = 9.d35
c
      endif
c
c     The hadronic collisions are not used in the e+e- stack.
c
      modelname(eplumin_sta) = 'Aires std. model for e+e-'
c
c     Some variables to zero...
c
      do lm = 1, 4
        hadcollcalls(lm) = 0
      enddo
c
c     Setting model identification.
c
      propmodelid = propmodelid0
c
      return
c
      end
c     --- End of routine modelinitask
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelinirun
c
c     Initializing the different stack processing routines (III).
c     One per stack.
c     This routine is invoked at every run, just before entering
c     the main kernel routine (scheduler).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999;
c                                La Plata 2000, 2001, 2002, 2003, 2005.
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
      include 'modelpar.f'
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  memu, memu2, mmu2
      parameter         (memu  = electronmass / muonmass)
      parameter         (memu2 = memu * memu)
      parameter         (mmu2  = muonmass * muonmass)
c
      integer           i, iextmfp
      character*16      qstring
      double precision  zthird, depth0, rhofac
      double precision  ftmp1, ftmp2
      logical           externalmfp, externalnucmfp
      double precision  getinpreal, depthfromz, adstydepth
      logical           getinpswitch
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the model identification.
c
      if (propmodelid .ne. propmodelid0) then
        call errprint(0, '*', 4, 'modelinirun',
     +                'Propagating model identification error.',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
c     Setting physical constants.
c
c     Intrinsic parameters for air.
c
c     Default values:
c                          zeff     7.265
c                      avzovera     0.499
c                     radlength    37.1 g/cm2
c
      zeff      = getinpreal('AirZeff')
      avzovera  = getinpreal('AirAvgZ/A')
      radlength = getinpreal('AirRadLength')
c
c     Energy cuts.
c
      gammacut1 = getinpreal('GammaCutEner')
      epmcut1   = getinpreal('ElectronCutEner')
      muoncut1  = getinpreal('MuonCutEner')
      heavycut1 = getinpreal('MesonCutEner')
      nuclcut1  = getinpreal('NuclCutEner')
      tepmcut1  = epmcut1 + electronmass
c
c     LPM effect and dielectric suppression thresholds.
c
      if (getinpswitch('LPMEffect')) then
        lpmthreshold = 1.d5
      else
        lpmthreshold = 3.d35
      endif
c
      if (getinpswitch('DielectricSuppression')) then
        dielsthreshold = 0.25d0
      else
        dielsthreshold = 3.d35
      endif
c
c     Muon bremsstrahlung and muonic pair production thresholds.
c
      if (getinpswitch('MuonBremsstrahlung')) then
        minmubremegy = max(minmubremegy0, 1.03d0 * muoncut1)
      else
        minmubremegy = 1.00d35
      endif
c
c     Nuclear collisions energy threshold.
c
      if (getinpswitch('NuclCollisions')) then
        nucollthreshold = 0.110d0
      else
        nucollthreshold = 3.d35
      endif
      if (getinpswitch('PhotoNuclear')) then
        gacollthreshold = 0.160d0
      else
        gacollthreshold = 3.d35
      endif
c
c     External hadronic collisions thresholds and related quantities.
c
      do i = 1, 2
        highcollthresh2(i) = 1.5d0 * highcollthresh(i)
        highcollfac2(i)    = 1 /
     +                       (highcollthresh2(i) - highcollthresh(i))
      enddo
c
c     Energy thresholds for full tracking of gammas and e+ e-:
c     Particles with energies below these limits are only
c     followed roughly.
c
      grough = getinpreal('GammaRoughCut')
      erough = getinpreal('ElectronRoughCut')
c
c     Atmospheric density constant.
c
      depth0 = depthfromz(1000.d0, i)
      rhofac = adstydepth(depth0, i) / depth0
c
c     DERIVED PARAMETERS.
c
c     Note on units: Length unit: meters. Time unit: seconds.
c                    Depth unit: g/cm2. Energy unit: GeV.
c
      uradlength  = 1.d0 / radlength
      thirdlogz   = log(zeff) / 3.d0
      zthird      = exp(thirdlogz)
      zlog        = log(183.d0) - thirdlogz
      zlogradlen  = zlog * radlength
      screenlim   = 100.0d0 / zthird
      screenfac   = screenlim * electronmass
c
c     Parameters for Single and multiple scattering algorithm.
c
      cmul00 = 8.2767d0 - log(fjscat)
      ftmp1  = sqrt(cmul00)
      cmul10 =  0.809d0 * ftmp1
      cmul11 =  0.246d0 * ftmp1
      cmul12 = -0.055d0 * ftmp1
c
c     Parameters for photoelectric process.
c
      photoelec0  = 4.05d-05 * zeff * (1.0d0 + 0.03d0 * zeff)
      photoelrate = 9.26d-18 * exp(3.11d0 * log(zeff))
c
c     Parameters for pair production. The constants are adjusted to
c     minimize the screening corrections to the rough cross section.
c
      ppcrsnifty  = uradlength
      spconst     = 0.72d0 * uradlength / zlog
      spconst0    = -spconst * log(3.5d0 * electronmass)
      egyppifty   = exp((ppcrsnifty - spconst0) / spconst)
c
      ccoll       = 0.15d0 * avzovera
      compf0      = -2.d0 * electronmass * ccoll
c
c     lpmfac/X gives the energy Elpm divided by 8.
c
      lpmfac      = 960.55d0 * radlength / rhofac
      foverzlog   = -0.25d0 / zlog
      s1sqlpm     = exp(1.d0 / foverzlog)
c
c     Constant to evaluate plasma frequency.
c
      ydielfac    = plasmaf0 * avzovera * rhofac
c
c     e+e- bremsstrahlung, and positron annihilation.
c
      minbremenergy  = max(erough, 1.05d0 * grough,
     +                     1.08d0 * epmcut1, minbremegy0)
      ebremenergycut = max(min(erough, grough), gammacut1,
     +                     ebremegycut0)
      ebremenergycut = min(ebremenergycut, 0.97d0 * minbremenergy)
      mefo           = (1 + 12 * zlog) / 9
      brempath       = radlength * zlog / mefo
      sphotlrate     = 1.25d0 * ebremenergycut / radlength
c
      ftmp1          = avzoveradef / avzovera
      panns10        = panns1073 * ftmp1
      panns20        = panns2073 * ftmp1
      panns30        = panns3073 * ftmp1
      ftmp1          = log(ftmp1)
      pannv40        = pannv4073 + ftmp1
      pannv50        = pannv5073 + ftmp1
c
c     e+ and e- knock-on processes and ionization losses.
c
      aelos        = (aelos73 / avzoveradef) * avzovera + sphotlrate
      belos        = (belos73 / avzoveradef) * avzovera
      celos        = celos73
      aelosl       = (aelosl73 / avzoveradef) * avzovera + sphotlrate
      belosl       = (belosl73 / avzoveradef) * avzovera
      celosl       = celosl73
      delosl       = (delosl73 / avzoveradef) * avzovera
      aplos        = aelos
      bplos        = (bplos73 / avzoveradef) * avzovera
      cplos        = cplos73
      aplosl       = (aplosl73 / avzoveradef) * avzovera + sphotlrate
      bplosl       = (bplosl73 / avzoveradef) * avzovera
      cplosl       = cplosl73
      dplosl       = (dplosl73 / avzoveradef) * avzovera
      koratefpa    =   0.153d-3 * avzovera * invkomin
      koratefpb    = - 0.153d-3 * avzovera
      koratefea    =   0.345d-3 * avzovera * invkomin
      koratefeb    = - 2 * 0.345d-3 * avzovera
c
c     Constants related with heavy particle knock-on processes.
c
      heavymineko2 = (heavymineko273 / avzoveradef) * avzovera
      amlos        = (amlos73 / avzoveradef) * avzovera
      bmlos        = (bmlos73 / avzoveradef) * avzovera
      cmlos        = cmlos73
      dmlos        = (dmlos73 / avzoveradef) * avzovera
      zmlos        = zmlos73
c
c     Muon bremsstrahlung and pair production constants (energy
c     thresholds were already set, see above). Muon bremsstrhalung
c     parameters do take into account nuclear size and atomic electron
c     muon bremsstrahlung (Can. J. of Phys., 46 (1968) S378;
c     (Phys. Atomic Nuclei, 60 (1997) 576).
c
      ftmp1         = log((avzoveradef * (zeffdef + 1)) /
     +                    (avzovera * (zeff + 1)))
      mbrmfp0       = mbrmfp073 + ftmp1
      mppmfp0       = mppmfp073 + ftmp1
c
      mubremegycut  = max(min(erough, grough), min(gammacut1, epmcut1),
     +                    mubremegycut0)
      mubremegycut  = min(mubremegycut, 0.9d0 * minmubremegy)
      RZeff3        = 189 / zthird
      ftmp1         = exp(0.5d0)
      mubremupar    = 0.75d0 * ftmp1 * muonmass * zthird
      muppepar      = 2 * ftmp1 * electronmass * RZeff3
      ftmp2         = 189 * muonmass / (electronmass * zthird)
      mubremphin0   = log(ftmp2 / 1.5d0) - thirdlogz
      mubremphin1   = ftmp1 * ftmp2 * muonmass / 2
      ftmp2         = electronmass * (zthird ** 2)
      mubremphie3   = 1429 * muonmass / ftmp2
      mubremphie0   = log(mubremphie3)
      mubremphie1   = (muonmass ** 3) / (2 * sqremass * ftmp1)
      mubremphie2   = mubremphie3 * ftmp1 * muonmass / 2
      mubremphi0    = mubremphin0 + mubremphie0 / zeff
      muppf1        = (1.5d0 * memu * zthird) ** 2
      muppf6        = 189 * (2 / (3 * memu * (zthird ** 2)))
c
c     Hadronic mfp's stuff
c
      call getinpstring('MFPHadronic', qstring, i)
c
      externalmfp = .true.
      if (qstring(1:i) .eq. 'Standard') then
        externalmfp = .false.
        iextmfp     = 1
      else if (qstring(1:i) .eq. 'SIBYLL') then
        iextmfp = 1
      else if (qstring(1:i) .eq. 'QGSJET') then
        iextmfp = 2
      else if (qstring(1:i) .eq. 'QGSJET-II') then
        iextmfp = 3
      else
        call errprint(0, '*', 4, 'modelinirun',
     +                'Unknown hadronic MFP specification',
     +                0, 0, 0, 0.d0, qstring(1:i))
      endif
c
      if (mod(propmodelid0, 10000) .ge. 2000) then
c
c       The hadronic models can provide external nuc-nuc mfp's.
c
        externalnucmfp = getinpswitch('ExtNucNucMFP')
c
      else
c
c       Nuc-nuc mfp's must be evaluated with the built-in algorithm.
c
        externalnucmfp = .false.
c
      endif
c
c     Initializing hadronic MFP parameterizations.
c
      call hmfpinit(iextmfp, externalmfp, externalnucmfp,
     +              getinpreal('MFPThreshold'))
c
c     Some variable shared data are initialized in "modelnewshower"
c
c     Calling the external model initialization routine (III).
c
      call extmodelinirun(externalhcoll)
c
      return
c
      end
c     --- End of routine modelinirun
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelnewshower
c
c     Initializing the different stack processing routines (IV).
c     One per stack.
c     This routine is invoked before starting the simulation of a new
c     shower.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  urandomt
c
c     FIRST EXECUTABLE STATEMENT
c
c     Refreshing variable shared data.
c
      exp1 = -log(urandomt(0.d0))
      exp2 = -log(urandomt(0.d0))
c
      return
c
      end
c     --- End of routine modelnewshower
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelaftershower
c
c     Model calculatios to be performed after completion of a shower.
c     This routine is invoked after the simulation of a shower
c     completes (empty stacks) and before any statistical calculation
c     is done.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2002.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Updating model variables.
c
      do i = 1, 4
        call outintupdate(helenucalls(i), hadcollcalls(i))
      enddo
c
      return
c
      end
c     --- End of routine modelaftershower
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelidfwrite(iounit, rc)
c
c     Saving internal variables at the end of a run.
c     This routine is invoked when finishing any run.
c     The variables must be saved using routine idfput.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2002.
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
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           iounit, rc
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Saving variable shared data.
c
      call idfput(iounit, 1, exp1, 0, 0, 0, .true., rc)
      if (rc .ne. 0) return
      call idfput(iounit, 1, exp2, 0, 0, 0, .true., rc)
      if (rc .ne. 0) return
      call idfput(iounit, 2, highcollthresh, 0, 0, 1, externalhcoll,
     +            rc)
      if (rc .ne. 0) return
      call idfput(iounit, 0, 0.d0, 4, hadcollcalls, 0, .true., rc)
      if (rc .ne. 0) return
      call idfput(iounit, 0, 0.d0, 1, propmodelid, 0, .true., rc)
c
      return
c
      end
c     --- End of routine modelidfwrite
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelidfread(iounit, rc)
c
c     Reading internal variables at the end of a run.
c     This routine is invoked when starting any subsequent process,
c     BEFORE modelinirun is called.
c     The variables must be restored using routine idfget.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2002.
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
c                     to zero if the reading operation is completed
c                     successfully. Nonzero otherwise.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           iounit, rc
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           idy
      double precision  fdy
      logical           ldy
c
c     FIRST EXECUTABLE STATEMENT
c
c     Restoring variable shared data.
c
      call idfget(iounit, .true., 1, exp1, 0, idy, 0, ldy, rc)
      if (rc .ne. 0) return
      call idfget(iounit, .true., 1, exp2, 0, idy, 0, ldy, rc)
      if (rc .ne. 0) return
      call idfget(iounit, .true., 2, highcollthresh, 0, idy,
     +            1, externalhcoll, rc)
      if (rc .ne. 0) return
      call idfget(iounit, .true., 0, fdy, 4, hadcollcalls, 0, ldy, rc)
      if (rc .ne. 0) return
      call idfget(iounit, .true., 0, fdy, 1, propmodelid, 0, ldy, rc)
c
      return
      end
c     --- End of routine modelidfread
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'modelinit.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
