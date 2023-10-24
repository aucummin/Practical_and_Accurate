c
c     FILE: initsim.f                       Creation date: 14/JUL/1997.
c                                       LAST MODIFICATION: 15/MAR/2011.
c
c     This file contains several initialization routines called from
c     the simulation programs.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine initialremark
c
c     Inserting (if it corresponds) an initial remark before the
c     user-specified remarks.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initrem.f'
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, ll, sll
c
c     FIRST EXECUTABLE STATEMENT
c
      call strimcopy(inirem, len(inirem), auxline, ll)
      if (ll .gt. 0) then
        sll = min(ll + 1, 75)
        write(auxfilestring, 2010) ('-', i = 1, sll)
 2010   format(75a1)
        call rmkcopy(auxfilestring, sll)
        call rmkcopy(auxline, ll)
        call rmkcopy(auxfilestring, sll)
      endif
c
      return
      end
c     --- End of routine initialremark
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine init2
c
c     Initializations to be performed before starting the first
c     simulation job.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999;
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
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      double precision  swt, awt
      double precision  depthfromz
c
c     FIRST EXECUTABLE STATEMENT
c
c     Miscellaneous initializations.
c
      oversionset   = .false.
c
      datistr0(1)   = datistr0(4)
      cpu0(1)       = 0
      cpu0(2)       = 0
      cpu0(3)       = 0
c
      stopchsize1   = 4000
      stopchsize2   = 0
      stopchsize3   = 0
c
      lastshower    = 0
      currshower    = 0
      shcomplete    = .true.
      jobnumber     = 0
      processnumber = 1
c
c     Starting the random number generator.
c
      call raninit(inputrseed)
c
c     Calling the model initialization routine (II).
c
      psta_n(0) = ' '
      call modelinitask(npstacks, psta_n(1), psta_mode,
     +                  2, psta_rn, psta_model_n)
c
c     Normalizing the primary particles weights.
c
      if (nshprimary .gt. 1) then
        swt = 0
        do i = 1, nshprimary
          swt = swt + abs(shprimarywt(i))
        enddo
        if (swt .gt. 0) then
          awt = 0
          do i = 1, nshprimary - 1
            awt = awt + abs(shprimarywt(i)) / swt
            shprimarywt0(i) = awt
          enddo
          shprimarywt0(nshprimary) = 1
        else
          call errprint(0, '*', 4, 'init2',
     +                  'The primary particle weights sum zero!',
     +                  0, 0, nshprimary, shprimarywt, ' ')
        endif
      else
        shprimarywt(1) = 1
      endif
c
c     Evaluating the static shower bounding box parameters.
c     Particles outside the bounding box will be considered "lost"
c     particles.
c
c     Position of top surface.
c
      dtoplost = max(depthfromz(injz + 4.d3, i), 1.d-15)
c
c     Zeroing the main counters and histogram arrays.
c
      call table0
c
c     Initializing the stack statistic arrays and related variables.
c
      do i = 1, npstacks
        avgtotsize(i)     = 0
        peakstsize(1, i)  = 0
        peakstsize(2, i)  = 2100000000
        peakstsize(3, i)  = 0
        procentries(1, i) = 0
        procentries(2, i) = 2100000000d10
        procentries(3, i) = 0
        hardswrite(1, i)  = 0
        hardswrite(2, i)  = 2100000000
        hardswrite(3, i)  = 0
        hardsread(1, i)   = 0
        hardsread(2, i)   = 2100000000
        hardsread(3, i)   = 0
      enddo
c
      do i = 0, npstacks
        callcounter(1, i) = 0
        callcounter(2, i) = 0
      enddo
c
      call statzero(npstacksp1, 5, nplost)
      call statzero(npstacksp1, 5, elost)
      call statzero(npstacksp1, 5, nplowe)
      call statzero(npstacksp1, 5, elowe)
      call statzero(npstacksp1, 5, nprgnd)
      call statzero(npstacksp1, 5, eprgnd)
      call statzero(npstacksp1, 5, eloss)
      call statzero(npstacksp1, 5, totpcles)
c
      call statzero(1, 5, nnotap)
      call statzero(1, 5, enotap)
      call statzero(1, 5, nneutrino)
      call statzero(1, 5, eneutrino)
      call statzero(2, 5, fstintdp)
      call statzero(1, 5, aveprim)
c
c     Special primary statistical variables and related.
c
      do i = 1, nescpcles
        nsprimpart(1, i) = 0
        nsprimpart(2, i) = 2100000000
        nsprimpart(3, i) = 0
      enddo
c
      nintintvar   = 0
      nintfltvar   = 0
      recordspvar0 = .false.
c
c     Setting the first compressed file name number to use.
c
      if (ciosplitevery .gt. 0) then
        j = firstshowernon1 + 1
      else
        j = 0
      endif
c
      do i = 1, npofiles
        pofileno(i) = j
      enddo
c
c     Initializing the cio particle buffers.
c
      call initciopbuf
c
      return
      end
c     --- End of routine init2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine init4
c
c     Initializations to be performed before entering the
c     simulation process.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2003, 2004.
c
C     FIELD TOOL CHANGES VERSION: 0.01 -  18/6/2009
C     inclusion of call to field tool initialization  
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'pclepar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
      include 'pclecodes.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
      include 'cioauxcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      double precision  tmp1
      double precision  rmin0depth, rminfilel
      double precision  depthfromz, getinpreal
      logical           getinpswitch
c
c     FIRST EXECUTABLE STATEMENT
c
c     Basic init level 4 stuff.
c
      call init4s
c
c     Setting basic kernel parameters and file names.
c
      nstopfiles = 2
      stopfile(1) = 'AIRES.STOP'
      stopfile(2) = leadingf0(1:leadingf0len) // '.STOP'
      if (wdirnamelen(2) .gt. 0) then
        nstopfiles = 4
        stopfile(3) = wdirname(2)(1:wdirnamelen(2)) // 'AIRES.STOP'
        stopfile(4) = leadingfn(2)(1:leadingfnlen(2)) // '.STOP'
      endif
c
      dstime     = cpu0(4)
      checkcpu   = (cpuperrun .gt. 0)
      checknsh   = (runshowers .gt. 0)
c
      if (processjobs .gt. 0) then
        processruns = processjobs
      else
        processruns = 2095338998
      endif
c
c     Initializing stack variables.
c
      minpstack  = 1
c
c     Setting the histogram pointers.
c
      call tablepointer
c
c     Evaluating some variables needed in the kernel.
c
      tmp1        = depthfromz(injz, atmlayer0)
      groundz00   = rearth + groundz
      injz00      = rearth + injz
      groundz00sq = groundz00 ** 2
      iovgz00sq   = (injz00 / groundz00) ** 2
c
c     varzendis equals 1 or 2. 1 implies that the zenith angle
c     distributes with probability A * sin(Z); 2 with probability
c     B * sin(Z) * cos(Z) = B/2 * sin(2 * Z)
c
      prycoszmin = cos(pryzenithmax * pi180 * varzendis)
      prycoszmax = cos(pryzenithmin * pi180 * varzendis)
      prycosfac  = upi180 / varzendis
c
c     Cut energies.
c
      tmp1 = getinpreal('MesonCutEnergy')
      mincutegy   = tmp1
      do i = -maxpcle, maxpcle
        cutenergy(i, 1) = tmp1
      enddo
c
      nucutenergy = getinpreal('NuclCutEnergy')
      if (nucutenergy .lt. mincutegy) mincutegy = nucutenergy
      do i = lambdacode, maxpcle
        cutenergy(-i, 1) = nucutenergy
        cutenergy( i, 1) = nucutenergy
      enddo
c
      cutenergy(-gammacode, 1) = getinpreal('GammaCutEner')
      cutenergy( gammacode, 1) = cutenergy(-gammacode, 1)
      if (cutenergy(gammacode, 1) .lt. mincutegy)
     +  mincutegy = cutenergy(gammacode, 1)
c
      cutenergy(electroncode, 1) = getinpreal('ElectronCutEner')
      cutenergy(positroncode, 1) = cutenergy(electroncode, 1)
      if (cutenergy(electroncode, 1) .lt. mincutegy)
     +   mincutegy = cutenergy(electroncode, 1)
c
      cutenergy(muminuscode, 1) = getinpreal('MuonCutEner')
      cutenergy(mupluscode, 1) = cutenergy(muminuscode, 1)
      if (cutenergy(muminuscode, 1) .lt. mincutegy)
     +   mincutegy = cutenergy(muminuscode, 1)
c
c     When ForceLowEDecays (ForceLowEAnnihilation) is true, then
c     cutenergy(*, 2) is zero for decaying particles (positrons);
c     Otherwise it is equal to cutenergy(1, *).
c     Actually, the "zero" it is replaced by 1/2 eV to avoid eventual
c     problems with some divisions.
c
      do i = -maxpcle, maxpcle
        cutenergy(i, 2) = cutenergy(i, 1)
      enddo
c
      call getinpstring('ForceLowEDecays', auxline, j)
c
      forcelowedecay = .false.
      if (auxline(1:j) .eq. 'Always') then
        forcelowedecay = .true.
        do i = -maxpcle, maxpcle
          if ((pclelife(i) .ge. 0) .and. (pclelife(i) .lt. 0.5d0))
     +    then
            cutenergy(i, 2) = 5.0d-10
          endif
        enddo
      else if (auxline(1:j) .eq. 'Normal') then
        do i = -maxpcle, maxpcle
          if ((pclelife(i) .ge. 0) .and. (pclelife(i) .lt. 0.5d0) .and.
     +        (cutenergy(i, 1) .lt. pclemass(i)))
     +    then
            cutenergy(i, 2) = 5.0d-10
            forcelowedecay = .true.
          endif
        enddo
      endif
c
      call getinpstring('ForceLowEAnnihilation', auxline, j)
c
      forceloweannih = .false.
      if (auxline(1:j) .eq. 'Always') then
        forceloweannih             = .true.
        cutenergy(positroncode, 2) = 5.0d-10
      else if (auxline(1:j) .eq. 'Normal') then
        if ((cutenergy(gammacode, 1) .lt. 1.2d-3) .and.
     +      (cutenergy(positroncode, 1) .lt. 1.4d-3))  then
          forceloweannih             = .true.
          cutenergy(positroncode, 2) = 5.0d-10
        endif
      endif
c
c     Thinning stuff.
c
      call thininit4
c
c     Geomagnetic field related variables.
c
      tmp1    = - pi180 * geobi
      igbx    = geob * cos(tmp1)
      igbz    = geob * sin(tmp1)
c
      if (geobfluc .ge. 0) then
        igbfluc = geobfluc
      else
        igbfluc = abs(geobfluc) * geob
      endif
c
c     Compressed output files related data.
c
      ciofsplit = (ciosplitevery .gt. 0)
c
      do i = 1, nciofiles
        j = 14 + 2 * i
        rminfile(i)  = fidata(0, j)
        rmaxfile(i)  = fidata(0, j + 1) 
        r2minfile(i) = rminfile(i) ** 2
        r2maxfile(i) = rmaxfile(i) ** 2
      enddo
c
      prsratio0  = getinpreal('ResamplingRatio')
      prssqratio = 1 / (prsratio0 ** 2)
c
      rmin0depth = 0.8d0 * injdepth + 0.2d0 * groundepth
      rminfilel  = rminfile(longiciofile)
c
      do i = 1, nobslevels
        if (obslevdepth(i) .lt. rmin0depth) then
          olrminfile(i, 1) = 0
          totobslev        = i
        else if (obslevdepth(i) .le. groundepth) then
          olrminfile(i, 1) = (obslevdepth(i) - rmin0depth) *
     +                    (rminfilel / (groundepth - rmin0depth))
          totobslev        = i
        else
          olrminfile(i, 1) = rminfilel
        endif
        olrminfile(i, 0) = olrminfile(i, 1) / prsratio0
        olrminfile(i, 2) = olrminfile(i, 1) ** 2
      enddo
c
c     Miscellaneous.
c
      notproprim    = (.not. getinpswitch('PropagatePrimary'))
      resetclock    = (.not. getinpswitch('SetTimeAtInjecti'))
      resetdtoplost = (.not. getinpswitch('SetTopAtInjectio'))
      recordspp     = getinpswitch('RecordSpecPrimaries')
c
c     Calling the model initialization routine (III).
c
      call modelinirun
c
C     FIELD TOOL++++
c     initfield has the fieldtool.inp compatible initialization
c     it is left(unchanged) for backwards compatibility, but should be removed
      call initfield
c     initfieldidl is exactly the same routine, but uses the data from
c     gathered from the idls.
      call initfieldidl
C     ++++++++++++++
      return
      end
c     --- End of routine init4
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newprocess
c
c     Updating the process variables.
c
c     Written by: S. J. Sciutto, La Plata 1996.
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
c     FIRST EXECUTABLE STATEMENT
c
c     Updating the process counter.
c
      processnumber  = processnumber + 1
c
      return
      end
c     --- End of routine newprocess
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newrun
c
c     Updating the run (job) variables.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  dummy
c
c     FIRST EXECUTABLE STATEMENT
c
c     Updating the run variables.
c
      jobnumber  = jobnumber + 1
c
      dummy = cpu0(2) + cpu0(3)
      if (dummy .gt. 0) then
        stopchsize1 = (stopchsize2 + stopchsize3) *
     +                           (stopchsecs / dummy) + 1
      endif
c
      return
      end
c     --- End of routine newrun
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c MATIAS: These routines should be in a separate file, fieldinit.f
c and then you just have to include it in the compilation configuration
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine initfieldidl
c
c     ZHAireS initialization subroutine compatible with the IDL interface
c     Written by Matias Tueros, March 2012 on Santiago de Compostela.
c     based on version: 0.02 - 25/3/2010 Washington Carvalho
c
      use Dyn_Array
      implicit none
c
c     Compilation parameters.
c
c     field tool parameters
      include 'fieldcomm.f'
c     include constant commons (for pi and cspeed)
      include 'constants.f'
c     TEST: injz,injdepth,groundalt,etc...
      include 'initpar.f'
      include 'initcomm.f'
c
c
c     Declaration of shared data.
c
c     functions that will be needed, and are defined in another file, and has no sense to include
      double precision getinpreal
      logical          getinpswitch
c
c     Declaration of internal variables and arrays.
c
c MATIAS: variable "angles" was moved to the common
      integer          nlines
      double precision dtmax,dtmin,dtna
      double precision zenithdeg,coszenith,sinzenith,tanzenith,hant
      double precision cheran, cherdg,checos,factor,groundaltitude
      double precision rproj,phidirrad,phiantrad,angle
c
      integer j,t,k,nu,na,jchec,eloss,trackf,pimu,calcf
      integer cfreq,ctime,cfreqfres,ctimefres,iosn,uvarn
c     critical visibility parameters
      double precision pctheta, cospctheta,sinpctheta
c
c     FIRST EXECUTABLE STATEMENT
c
c     Check if field tool exists. If it does exist, then
c     the initialization was already made, and you need to get outa here!
      if(fieldtool) return

      call putlog(0, .true., 'ZHAires IDL: Initializing ZHS routines')
c
c     ==============
c     ZEROING THINGS (this should be done elsewhere)
c     ==============
c     initializing total track sums
      tracksum=0.d0
      sumdeltaz=0.d0
      sumabdeltaz=0.d0
      excesssum=0.d0
      excesssumz=0.d0
      excessabsumz=0.d0
c     initializing PIMU track sums
      pimutracksum=0.d0
      pimusumdeltaz=0.d0
      pimusumabdeltaz=0.d0
      pimuexcesssum=0.d0
      pimuexcesssumz=0.d0
      pimuexcessabsumz=0.d0
c     per particle
      ptracksum=0.d0
      psumdeltaz=0.d0
      psumabdeltaz=0.d0
      pbartracksum=0.d0
      pbarsumdeltaz=0.d0
      pbarsumabdeltaz=0.d0
      piptracksum=0.d0
      pipsumdeltaz=0.d0
      pipsumabdeltaz=0.d0
      pimtracksum=0.d0
      pimsumdeltaz=0.d0
      pimsumabdeltaz=0.d0
      muptracksum=0.d0
      mupsumdeltaz=0.d0
      mupsumabdeltaz=0.d0
      mumtracksum=0.d0
      mumsumdeltaz=0.d0
      mumsumabdeltaz=0.d0
      eptracksum=0.d0
      epsumdeltaz=0.d0
      epsumabdeltaz=0.d0
      emtracksum=0.d0
      emsumdeltaz=0.d0
      emsumabdeltaz=0.d0
c
c     ===========================
c     Setting auxiliart variables
c     ===========================
      zenithdeg=pryzenithmin
      coszenith=cos(zenithdeg*dacos(-1.d0)/180.d0)
      sinzenith=sin(zenithdeg*dacos(-1.d0)/180.d0)
      tanzenith=tan(zenithdeg*dacos(-1.d0)/180.d0)

      phidirrad=pryazimmin*pi180
c     //set phidirrad in the rangle [0,2pi]
      if(phidirrad .lt. 0.d0) then 
         phidirrad=phidirrad+2.d0*pi
      endif
      
c     Earth radius plus ground altitude (small correction)
      Rtg=rearth+groundz
      minparcrit=0.d0
c
c     Matias:Array zeroing was here, but now is done during memmory allocation.
c
c     setting radegr
      radegr=180.D0/pi

c      initialize factor_t
      factor_t=1.d0/cspeed
c
c Reading IDL directives
c            
c Added by Matias Tueros Nov 2011 for ZHS integration      
c -Output in this section should be removed, as the same output is produced by zhsairestatus
c  MATIAS: trackflag currently controls if the field calculation
c  is done or not, sometimes, in the eplusminus. This should be replaced by calcfield only.
      calcfield=getinpswitch("ZHAireS")
      if(calcfield) then
          trackflag=.true.
          call putlog(0,.false.
     &,'ZHAireS: Radio Emission calculation is ON.')
      else
          call putlog(0,.false.
     &,'ZHAireS: Radio Emission calculation is OFF.')
          calcfield=.false.
          trackflag=.false.
          return
      endif
c      
      calcfreqfres=getinpswitch("FresnelFreq")
      if(calcfreqfres.and.calcfield) then
c          call putlog(0,.false.
c     &,'ZHAireS: Fresnel in the Frequency domain is ON.')
      else
c          call putlog(0,.false.
c     &,'ZHAireS: Fresnel in the Frequency domain is OFF.')
          calcfreqfres=.false.
      endif     
c
      calcfreq=getinpswitch("FraunhoferFreq")
      if(calcfreq.and.calcfield) then
c          call putlog(0,.false.
c     &,'ZHAireS: Fraunhofer in the Frequency domain is ON.')
      else
c          call putlog(0,.false.
c     &,'ZHAireS: Fraunhofer in the Frequency domain is OFF.')
          calcfreq=.false.
      endif 
c      
      calctimefres=getinpswitch("FresnelTime")
      if(calctimefres.and.calcfield) then
c          call putlog(0,.false.
c     &,'ZHAireS: Fresnel in the Time domain is ON.')
      else
c          call putlog(0,.false.
c     &,'ZHAireS: Fresnel in the Time domain is OFF.')
          calctimefres=.false.
      endif     
c
        calctime=getinpswitch("FraunhoferTime")
      if(calctime.and.calcfield) then
c          call putlog(0,.false.
c     &,'ZHAireS: Fraunhofer in the Time domain is ON.')
      else
c          call putlog(0,.false.
c     &,'ZHAireS: Fraunhofer in the Time domain is OFF.')
          calctime=.false.
      endif
c        
      countpimu=getinpswitch("IncludeHadrons")
      if(countpimu.and.calcfield) then
c          call putlog(0,.false.
c     &,'ZHAireS: Including emission from hadrons.')
      else
c          call putlog(0,.false.
c     &,'ZHAireS: NOT Including emission from hadrons')
          countpimu=.false.
      endif    
c        
c     Refraction index
      ref_n=getinpreal('RefractionIndex')
c     variable?
      usevarn=getinpswitch('ConstRefrIndex')
      usevarn=.not.usevarn  
c      if(ref_n.lt.0) then
c          usevarn=.true.
c          ref_n = -ref_n
c          call putlog(0,.false.
c     &,'ZHAireS: Using variable refraction index')       
c      else
c          usevarn=.false.
c      endif     
c
      dt_bin_s=getinpreal('TimeDomainBin');
      dt_bin_ns=dt_bin_s*1.d9   
c      write(*,*) "ZHAireS: Time bin size (ns)",dt_bin_ns
c      write(*,*) "dt_bin ns",dt_bin_ns,"s",dt_bin_s  
c
c     AS LONG AS THIS IS AIRES ONLY, no ICE will ever be required
      usezhseloss=.false.        
c     
c End addition
c      
c     At this stage, you should have available the information about
c    
c     usezhseloss=.true o false true ZHS, false 
c     trackflag=.true o false (si no es true, no se hace nada)
c     pimu !flag for counting mu+- and pi+-: 1=ON  0=Off
c     countpimu=.true o false
c     calcf      !Field calculation (empsum call) 1=ON 2=Off
c     dt_bin_ns   !Reads bin size (ns)
c     ref_n
c     new input variable: use variable n: 1=ON, 0=OFF  ///////
c     uvarn-> usevarn
c
c====================================================================
c ENERGY CUTS (Medium dependent!)
c===================================================================
c     For now usezhloss MEANS ICE!!!
c     TEST: use "usezhseloss" to set min and max time
c     this will never happen in AIRES
      if(usezhseloss)then
c     set cuts for ICE here (Cerenkov threshold)
         fpicut=0.5d0  !MeV
         fmucut=0.5d0 !MeV
         fpcut=0.5d0    !MeV
         fpicut=fpicut*1.d-3  !transform to GeV
         fmucut=fmucut*1.d-3  !transform to GeV
         fpcut=fpcut*1.d-3    !transform to GeV
      else
c     This will always happen
c     set cuts - CHANGE CUTS FOR AIR HERE (Cerenkov threshold)!
         fpicut=29.12d0  !MeV
         fmucut=22.04d0 !MeV
         fpcut=195.77    !MeV
         fpicut=fpicut*1.d-3  !transform to GeV
         fmucut=fmucut*1.d-3  !transform to GeV
         fpcut=fpcut*1.d-3    !transform to GeV
      endif
c
c================================================================
c      Refraction index
c================================================================
      if(usevarn .and. (usezhseloss .eqv. .false.) )then
         usevarn=.true.
         call putlog(0,.false.
     &        ,'ZHAireS: Variable refractive index model is ON.')
c     set ns(sea level n for variable n) and kr
         ns=1.d6*(ref_n-1.d0)
         write(*,"(A35,1X,F6.1,4x,A3,1x,F9.5)"),
     + "      Variable n model: Setting ns=",ns,"kr=",kr
      else
         usevarn=.false.
         call putlog(0,.false.
     &        ,'ZHAireS: : Variable refractive index model is OFF.')
      endif

c  checking if the time window is small enough
      if(abs(endtime)> dt_bin_ns*maxt/2 .or. 
     +           abs(starttime)> dt_bin_ns*maxt/2 )then
        print*,"Critical ERROR: TOO MUCH TIME RESOLUTION!!!"
        print*,"Bin size (ns):",dt_bin_ns
        print*,"End of pulse expected at (ns):",endtime
        print*,"Begin of pulse expected at (ns):",starttime
        print*,"Maximum absolute time for this bin size (ns):",
     +              dt_bin_ns*maxt/2
        print*,"Please increase time bin size!!!!!!"
        stop
      endif             
c Number of required time bins (the time bining needs to be simetrical,
c as the implementation requieres to do everything from maxthalf)
c thus, i use the bigest among starttime and endtime to calculate ntbins.
c this wastes bins, and could be solved changing maxthalf to a number not 
c fixed in the half of the array
       if(abs(starttime)<abs(endtime)) then
         ntbins=2*abs(endtime)/dt_bin_ns
       else
         ntbins=2*abs(starttime)/dt_bin_ns
       endif
       write(*,*) "      number of time bins",ntbins
       write(*,*) "      starttime",starttime,"endtime",endtime
       TIME_MIN=starttime
       TIME_MAX=endtime
c=============================================================
c Fresnel time conditioning. Note that this will not be used if simulation is fraunhfr only.
c=============================================================
      if(calctimefres .or. calcfreqfres)then
c        print*,"Antena number and x,y,z [m],t0 [ns] in radio system:"
        dtmax=-1.d10
        dtmin= 1.d10
        do na=1,namax
c     calculate critical visibility parameter (this should be set in its own section for clarity)
          h0ant(na)=zant(na)
          if( (zant(na) .lt. 10.d0) .or. usezhseloss) then
            parcrit(na)=0.d0 !antenna lower than 10m above ground
          else
            cospctheta=Rtg/(Rtg+zant(na))
            pctheta=acos(cospctheta)
            sinpctheta=sin(pctheta)
            dcrit(na)=Rtg*sinpctheta
            parcrit(na)=(Rtg*(cospctheta-1.d0)-zant(na))/
     +              (Rtg*sinpctheta)
c           print*,"Non-zero parcrit: antn=",na,"zant(na)",zant(na),
c     +              "parcrit=",parcrit(na)
            if(parcrit(na)<minparcrit)then
              minparcrit=parcrit(na)
c             print*,"new minparcrit=",minparcrit
            endif
          endif
c     CHANGED INPUT FORMAT!!!! Now ZANTnew is in relation to the ground, (this should be set in its own section for clarity)
c     (for antennas on the ground, zantnew=0)
c     so will have to make ZANT=injz-(groundz+ZANTnew)
c     BUT ONLY FOR SHOWERS IN AIR!!!!!! 
c     Original ZHS system is still used for ice
          if(usezhseloss .eqv. .FALSE.)then 
             hant=zant(na) !Antenna heigh above ground
             zant(na)=injz-(groundz+zant(na))
          endif            
c     phi of antenna
          phiantrad=atan2(yant(na),xant(na))
          if(phiantrad .lt. 0)then
            phiantrad=2.d0*pi+phiantrad
          endif         
c     projection of r of antenna on shower phi direction
          angle=dabs(phidirrad-phiantrad)
c
          rproj=sqrt(yant(na)*yant(na)+xant(na)*xant(na))*
     +   cos(angle)
c
          dtna=-( hant/coszenith + (rproj-hant*tanzenith)*sinzenith)
     +     /cspeed
c
c         passing dtna to a fixed number of time bins
          dtna=INT(dtna/dt_bin_s)*dt_bin_s
          dt(na)=dtna
c
          if(dtna .gt. dtmax)then
            dtmax=dtna
          endif
          if(dtna .lt. dtmin)then
            dtmin=dtna
          endif         
c          write(*,"(I4,4F10.1)"),
c     +              na,xant(na),yant(na),zant(na),dt(na)*1.d9
        enddo
c if time calculation in fresnel domine was requested
        if((usezhseloss .eqv. .FALSE.) .and. calctimefres)then
c     Will now reset new min/max time, depending on shower inclination:
c     new time shift allows signal to start from t=0 on, but
c     if antenna is above sea levelinclined, the signal can reach it before t=0    
          write(*,"(A21,1x,F10.2)"),
     +         "      Maximum Required Delta t (ns):",dtmax*1.d9
          write(*,"(A21,1x,F10.2)"),
     +         "      Minimum Required Delta t (ns):",dtmin*1.d9
          TIME_MIN=dtmin*1.e9+starttime
          TIME_MAX=endtime+dtmax*1.e9
        endif                  !end if !usezhseloss&&calctimefres           
c
      endif !end if calctimefres or calcfreqfres
c
      write(*,"(A24,1x,F8.1,4x,A9,F8.1)"),"      Setting  Time_min:",
     +        TIME_MIN,"Time_max:",TIME_MAX
c     
c==============================================================
c memmory allocation
c=============================================================
c at this point of the program, we know exactly how many antenas, how many angles,etc we will be using
c and wich calculations we will perform. We can now allocate the necessary arrays in an efficient way
c
      maxthalf=ntbins/2   
c
      call fieldmemalloc()
c
c=============================================================
c     START OF FIELD TOOL's DEFINE GRID BLOCK =================
c     changed FACTOR for use with meters
      FACTOR=2.D0*PI/2.99792458D2 !  Frequency must be in MHz
      CHECOS=1.D0/REF_N
      CHERAN=ACOS(CHECOS)       ! Radians
      CHERDG=CHERAN*RADEGR      ! Degrees
C     
C     **************************************************************************
C     GRID in frequency and observation angle with respect to shower axis.
C     It also calculates factors that are frequently used and puts them in 
C     an array in order to speed up the code.
C     **************************************************************************
C     
      DO NU=1,NUMAX
         FACFRQ(NU)=FACTOR*FREQ(NU)
         FRQCOM(NU)=FREQ(NU)
         JEND(NU)=JMAX
      END DO
c     
      DO J=1,JMAX
         IF (abs(ANGLES(J)) .LT. 1.d-4) then
            JCHEC=J
         endif
         IF (ANGLES(J) .LT. -CHERDG) THEN
            THETA(J) = 0.
         ELSE
            THETA(J)=CHERDG+ANGLES(J)
         END IF
      END DO
      
      DO J=1,JMAX
         THETA(J)=THETA(J)/RADEGR
         SINTET(J)=SIN(THETA(J))
         SINTET2(J)=SINTET(J)*SINTET(J)
         SINMU(J)=SINTET(J)*REF_N
         COSTET(J)=COS(THETA(J))
         COSTET2(J)=COSTET(J)*COSTET(J)
         COSMU(J)=REF_N*COSTET(J)
         CSMU_1(J)=REF_N*COSTET(J)-1.D0
      END DO

      DO K=1,KMAX
         PHI(K)=PHI(K)/RADEGR
         SINPHI(K)=SIN(PHI(K))
         COSPHI(K)=COS(PHI(K))
      END DO  
      
c     END OF FIELD TOOL DEFINE GRID BLOCK==========
c
c     MATIAS:Log Things for IDL. Standard output
      call zhairestatus(6, nlines)
c     MATIAS:Log Things for .lgf
      call zhairestatus(7, nlines)
c     
c ==========================================================
c Output Files
c ==========================================================
c     open output files for root
      if(calcfreq)then
         open(unit=551,status='old',access='append',
     +        file='freqfraunh-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File freqfraunh-root.dat exists.')
            write(551,333) '# new run started'
            close(551)
         else
            open(unit=551,status='new',file='freqfraunh-root.dat')
            write(551,333) '# -----------Freq Fraunhofer file----------'
            write(551,333) '# Columns:'
            write(551,333) '#        1 - Shower Number'
            write(551,333) '#        2 - Theta #'
            write(551,333) '#        3 - Phi #'
            write(551,333) '#        4 - Theta (deg)'
            write(551,333) '#        5 - Phi (deg)'
            write(551,333) '#        6 - Frequency #'
            write(551,333) '#        7 - Frequency (MHz)'
            write(551,333) '#        8 - R x |E| (V/M MHz)'
            write(551,333) '#        9 - Phase'
            write(551,333) '#        10 - R x Ex (V/M MHz)'
            write(551,333) '#        11 - Phase Ex'
            write(551,333) '#        12 - R x Ey (V/M MHz)'
            write(551,333) '#        13 - Phase Ey'
            write(551,333) '#        14 - R x Ez (V/M MHz)'
            write(551,333) '#        15 - Phase Ez'
            if (countpimu) then
               write(551,333)
     +  '# Charged Pions and Muons were added to these total sums!!!'
               write(551,333) '#'
            endif
            close(551) 
         endif
      endif


      if(calctime)then
         open(unit=552,status='old',access='append',
     +        file='timefraunh-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File timefraunh-root.dat exists.')
            write(552,333) '# new run started'
            close(552)
         else
            open(unit=552,status='new',file='timefraunh-root.dat')
            write(552,333) '# -----------Time Fraunhofer file----------'
            write(552,333) '# Columns:'
            write(552,333) '#        1 - Shower Number'
            write(552,333) '#        2 - Theta #'
            write(552,333) '#        3 - Phi #'
            write(552,333) '#        4 - Theta (deg)'
            write(552,333) '#        5 - Phi (deg)'
            write(552,333) '#        6 - Time (ns)'
            write(552,333) '#        7 - R x |A| (V/M)'
            write(552,333) '#        8 - R x Ax  (V/M)'
            write(552,333) '#        9 - R x Ay  (V/M)'
            write(552,333) '#        10- R x Az  (V/M)'
            write(552,333) '#        11- R x |E| (V/M)'
            write(552,333) '#        12- R x Ex  (V/M)'
            write(552,333) '#        13- R x Ey  (V/M)'
            write(552,333) '#        14- R x Ez  (V/M)'
            if (countpimu) then
               write(552,333)
     +    '# Charged Pions and Muons were added to these total sums!!!'
               write(552,333) '#'
            endif
            close(552)
         endif
      endif 
      
      if(calcfreqfres)then
         open(unit=553,status='old',access='append',
     +        file='freqfresnel-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File freqfresnel-root.dat exists.')
            write(553,333) '# new run started'
            close(553)
         else
            open(unit=553,status='new',file='freqfresnel-root.dat')
            write(553,333) '# -----------Freq Fresnel file----------'
            write(553,333) '# Columns:'
            write(553,333) '#        1 - Shower Number'
            write(553,333) '#        2 - Antenna Number'
            write(553,333) '#        3 - Antenna X (m)'
            write(553,333) '#        4 - Antenna Y (m)'
            write(553,333) '#        5 - Antenna Z (m)'
            write(553,333) '#        6 - Frequency #'
            write(553,333) '#        7 - Frequency (MHz)'
            write(553,333) '#        8 - |E| (V/M MHz)'
            write(553,333) '#        9 -  Ex  (V/M MHz)'
            write(553,333) '#        10 - Ey  (V/M MHz)'
            write(553,333) '#        11 - Ez  (V/M MHz)'
            if (countpimu) then
               write(553,333)
     +    '# Charged Pions and Muons were added to these total sums!!!'
               write(553,333) '#'
            endif
            close(553)
         endif
      endif
      
      if(calctimefres)then
         open(unit=554,status='old',access='append',
     +        file='timefresnel-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File timefresnel-root.dat exists.')
            write(554,333) '# new run started'
            close(554)
         else
            open(unit=554,status='new',file='timefresnel-root.dat')
            write(554,333) '# -----------Time Fresnel file----------'
            write(554,333) '# Columns:'
            write(554,333) '#        1 - Shower Number'
            write(554,333) '#        2 - Antenna Number'
            write(554,333) '#        3 - Antenna X (m)'
            write(554,333) '#        4 - Antenna Y (m)'
            write(554,333) '#        5 - Antenna Z (m)'
            write(554,333) '#        6 - Time (ns)'
            write(554,333) '#        7 - |A| (V/M)'
            write(554,333) '#        8 -  Ax  (V/M)'
            write(554,333) '#        9 -  Ay  (V/M)'
            write(554,333) '#        10 - Az  (V/M)'
            write(554,333) '#        11 -|E| (V/M)'
            write(554,333) '#        12 - Ex  (V/M)'
            write(554,333) '#        13 - Ey  (V/M)'
            write(554,333) '#        14 - Ez  (V/M)'
            if (countpimu) then
               write(554,333)
     +    '# Charged Pions and Muons were added to these total sums!!!'
               write(554,333) '#'
            endif
            close(554)
         endif
      endif
c
 150  continue
c     Opening tracksum file
      open(unit=411,status='old',access='append',
     +file='tracklengths.dat',err=160)
      call putlog(0,.false.,'Field Tool: File tracklengths.dat exists.')
      write(411,333) '# new run started'
      close(411)
c     Opening PIMU tracksum file
      open(unit=412,status='old',access='append',
     +file='pimutracklengths.dat',err=200)
      call putlog(0,.false.
     +,'Field Tool: File pimutracklengths.dat exists.')
      write(412,333) '# new run started'
      close(412)
c     Opening per particle tracksum file
      open(unit=413,status='old',access='append',
     +file='perpcletracklengths.dat',err=200)
      call putlog(0,.false.
     +,'Field Tool: File perpcletracklengths.dat exists.')
      write(413,333) '# new run started'
      close(413)
      return
c
 160  continue
      open(unit=411,status='new',file='tracklengths.dat')
      write(411,333) '# -----------Tracksum file-------------'
      write(411,333) '# Columns:     (m)'
      write(411,333) '#        1-Shower Number'
      write(411,333) '#        2-Tracksum'
      write(411,333) '#        3-Sum Delta_z'
      write(411,333) '#        4-Sum Abs(Delta_z)'
      write(411,333) '#        5-Excess track sum (electron-positron)'
      write(411,333) '#        6-Excess sum Delta_z'
      write(411,333) '#        7-Excess sum Abs(Delta_z)'
      write(411,333) '#'
      if (countpimu) then
         write(411,333)
     + '# Charged Pions and Muons were added to these total sums!!!'
         write(411,333) '#'
      endif
      close(411)
c     test if PIMU tracksum file exists (it shouldn't at this point)
      open(unit=412,status='old',access='append',
     +file='pimutracklengths.dat',err=170)
      call putlog(0,.false.
     +,'Field Tool: ERROR-Only some tracklength file exists. Stopping.')
      stop
c      
 170  continue
      open(unit=412,status='new',file='pimutracklengths.dat')
      write(412,333) '# -----------PIMU Tracksum file-------------'
      write(412,333) '# Columns:     (m)'
      write(412,333) '#        1-Shower Number'
      write(412,333) '#        2-Tracksum'
      write(412,333) '#        3-Sum Delta_z'
      write(412,333) '#        4-Sum Abs(Delta_z)'
      write(412,333) '#        5-Excess track sum (negative-positive)'
      write(412,333) '#        6-Excess sum Delta_z'
      write(412,333) '#        7-Excess sum Abs(Delta_z)'
      write(412,333) '#'  
      if (countpimu) then
         write(412,333)
     + '#Charged Pions and Muons tracks are also being added to totals.'
         write(412,333)
     + '#Totals are in tracklengths.dat file.'         
         write(412,333) '#'
      endif
      close(412)
c     test if perpcle tracksum file exists (it shouldn't at this point)
      open(unit=413,status='old',access='append',
     +file='perpcletracklengths.dat',err=180)
      call putlog(0,.false.
     +,'Field Tool: ERROR-Only some tracklength file exists. Stopping.')
      stop      

 180   continue
      open(unit=413,status='new',file='perpcletracklengths.dat')
      write(413,333) '# -----------PIMU Tracksum file-------------'
      write(413,333) '# Columns:     (m)'
      write(413,333) '#        1-Shower Number'
      write(413,333) '#        2-Electron Tracksum'
      write(413,333) '#        3-Electron Sum Delta_z'
      write(413,333) '#        4-Electron Sum Abs(Delta_z)'
      write(413,333) '#        5-Positron Tracksum'
      write(413,333) '#        6-Positron Sum Delta_z'
      write(413,333) '#        7-Positron Sum Abs(Delta_z)'
      write(413,333) '#        8-Pi plus Tracksum'
      write(413,333) '#        9-Pi plus  Sum Delta_z'
      write(413,333) '#        10-Pi plus Sum Abs(Delta_z)'
      write(413,333) '#        11-Pi minus Tracksum'
      write(413,333) '#        12-Pi minus  Sum Delta_z'
      write(413,333) '#        13-Pi minus Sum Abs(Delta_z)'
      write(413,333) '#        14-Mu plus Tracksum'
      write(413,333) '#        15-Mu plus  Sum Delta_z'
      write(413,333) '#        16-Mu plus Sum Abs(Delta_z)'
      write(413,333) '#        17-Mu minus Tracksum'
      write(413,333) '#        18-Mu minus  Sum Delta_z'
      write(413,333) '#        19-Mu minus Sum Abs(Delta_z)'
      write(413,333) '#        20-Proton Tracksum'
      write(413,333) '#        21-Proton Sum Delta_z'
      write(413,333) '#        22-Proton Sum Abs(Delta_z)'
      write(413,333) '#        23-Pbar Tracksum'
      write(413,333) '#        24-Pbar Sum Delta_z'
      write(413,333) '#        25-Pbar Sum Abs(Delta_z)'
      write(413,333) '#'  
      close(413)
      return
c
 200  continue !only some tracksum file exists:  ERROR
      call putlog(0,.false.
     +,'Field Tool: ERROR-Only some tracklength file exists. Stopping.')
      stop
c
 333  format(A)
c
      return
      end
c     --- End of routine initfieldidl
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine initfield
c
c     ZHAireS initialization subroutine
c     version: 0.02 - 25/3/2010 Washington Carvalho
c
      use Dyn_Array
      implicit none
c     field tool parameters
      include 'fieldcomm.f'
c     include constant commons (for pi and cspeed)
      include 'constants.f'
c     TEST: injz,injdepth,groundalt,etc...
      include 'initpar.f'
      include 'initcomm.f'
c///////////////////
c
c     internal variables
c     MATIAS: the array angles was moved to fieldcomm common.
      integer nlines
      double precision minz,maxz,dmint,dmaxt,dtmax,dtmin,dtna
      double precision zenithdeg,coszenith,sinzenith,tanzenith,hant
      double precision cheran, cherdg,checos,factor,groundaltitude
      double precision rmax,rmin,rproj,l0,phidirrad,phiantrad,angle
      integer imint,imaxt
c
      integer j,t,k,nu,na,jchec,eloss,trackf,pimu,calcf
      integer cfreq,ctime,cfreqfres,ctimefres,iosn,uvarn
c     critical visibility parameters
      double precision pctheta, cospctheta,sinpctheta
c
      zenithdeg=pryzenithmin
      coszenith=cos(zenithdeg*dacos(-1.d0)/180.d0)
      sinzenith=sin(zenithdeg*dacos(-1.d0)/180.d0)
      tanzenith=tan(zenithdeg*dacos(-1.d0)/180.d0)
c
c     MATIAS l0 is never used. Consider for removal
c      l0=30000./coszenith
      
      phidirrad=pryazimmin*pi180
c     //set phidirrad in the rangle [0,2pi]
      if(phidirrad .lt. 0.d0) then 
         phidirrad=phidirrad+2.d0*pi
      endif
      
c     Earth radius plus ground altitude (small correction)
      Rtg=rearth+groundz
      minparcrit=0.d0

ccc   ==============

c Matias: dynamic arrays modification
c the initialization will be done during allocation     
c
c     initializing total track sums
      tracksum=0.d0
      sumdeltaz=0.d0
      sumabdeltaz=0.d0
      excesssum=0.d0
      excesssumz=0.d0
      excessabsumz=0.d0
c     initializing PIMU track sums
      pimutracksum=0.d0
      pimusumdeltaz=0.d0
      pimusumabdeltaz=0.d0
      pimuexcesssum=0.d0
      pimuexcesssumz=0.d0
      pimuexcessabsumz=0.d0
c     per particle
      ptracksum=0.d0
      psumdeltaz=0.d0
      psumabdeltaz=0.d0
      pbartracksum=0.d0
      pbarsumdeltaz=0.d0
      pbarsumabdeltaz=0.d0
      piptracksum=0.d0
      pipsumdeltaz=0.d0
      pipsumabdeltaz=0.d0
      pimtracksum=0.d0
      pimsumdeltaz=0.d0
      pimsumabdeltaz=0.d0
      muptracksum=0.d0
      mupsumdeltaz=0.d0
      mupsumabdeltaz=0.d0
      mumtracksum=0.d0
      mumsumdeltaz=0.d0
      mumsumabdeltaz=0.d0
      eptracksum=0.d0
      epsumdeltaz=0.d0
      epsumabdeltaz=0.d0
      emtracksum=0.d0
      emsumdeltaz=0.d0
      emsumabdeltaz=0.d0

c     setting radegr
      radegr=180.D0/pi
c     
c     opening input file
      open(unit=211,status='old',file='fieldtool.inp',err=300)
c
      call putlog(0, .true., 'Field Tool: Initializing Field Tool.')
      fieldtool=.true.
c     MATIAS: WARNING PLACED FOR IDL COMPATIBILITY
      write(*,*) " "
      write(*,*) "WARNING: fieldtool.inp found, and will be honored"
      write(*,*) "for back compatibility. The settings shown in the"
      write(*,*) "AIRES summary will be incorrect."
      write(*,*) " "
c
c     The first 2 parameters are mandatory
      
      read(211,*) trackf    !Tracklength calculations: 1=ON  0=Off
      read(211,*) eloss     !dEdX to use:              1=ZHS 0=Tierras

c     Before reading more input parameters set eloss, trackflag:
c     Setting which eloss to use
      if(eloss .eq. 1)then
         usezhseloss=.true.
         call putlog(0,.false.,'Field Tool: Using ZHS e+- energy loss.')
      else
         usezhseloss=.false.
         call putlog(0,.false.
     &,'Field Tool: Using TIERRAS/AIRES e+- energy loss.')
      endif
c
c     Will set time_min/max and energy cuts according to usezhsloss //////// 
c     For now usezhloss MEANS ICE!!!
c     TEST: use "usezhseloss" to set min and max time
      if(usezhseloss)then

         if(calctimefres .and. calctime .eqv. .false. )then
            TIME_MIN=0.d0
            TIME_MAX=2000.d0
         else if(calctimefres .and. calctime)then
            TIME_MIN=-300.d0
            TIME_MAX=2000.d0
            print*,
     + "Running Fresnel and Fraunhoffer in ice at the same time!"
            print*,"Time Range may be inadequate!!!!"
         else
            TIME_MIN=-300.d0
            TIME_MAX=300.d0
         endif

c     TEST: Force time window here:
c         TIME_MIN=-4.d4
c         TIME_MAX=-3.d4
c     /////////////////////////////

c     set cuts for ICE here (Cerenkov threshold)
         fpicut=0.5d0  !MeV
         fmucut=0.5d0 !MeV
         fpcut=0.5d0    !MeV
ccccccccccccccccccccccccccccccccccccccc


         fpicut=fpicut*1.d-3  !transform to GeV
         fmucut=fmucut*1.d-3  !transform to GeV
         fpcut=fpcut*1.d-3    !transform to GeV
      else
c     Will reset new min/max time later, depending on zenith angle:
c     new time shift allows signal to start from t=0 on, but
c     for inclined showers the signal can reach it before t=0. 
c     For now will set for vertical shower:
         TIME_MIN=-200.d0
         TIME_MAX=10000.d0

c     set cuts - CHANGE CUTS FOR AIR HERE!
c     starting with the same cuts. Will increase to see effects
         fpicut=29.12d0  !MeV
         fmucut=22.04d0 !MeV
         fpcut=195.77    !MeV
         fpicut=fpicut*1.d-3  !transform to GeV
         fmucut=fmucut*1.d-3  !transform to GeV
         fpcut=fpcut*1.d-3    !transform to GeV
c     will set ns (sea level n for variable n) later, after ref_n is read
      endif
C     
c     //////////////////////////////////////////////////////////////////////
c
c     turning tracklength calculation ON or OFF
      if(trackf .eq. 1)then
         trackflag=.true.
         call putlog(0,.false.
     &,'Field Tool: Tracklength calculation is ON.')
c
c     If trackflag is on, read up to calcf
         read(211,*) pimu !flag for counting mu+- and pi+-: 1=ON  0=Off
         read(211,*) calcf      !Field calculation (empsum call) 1=ON 2=Off

c     initialize factor_t
         factor_t=1.d0/cspeed
c
         if(pimu .eq. 1)then
            countpimu=.true.
            call putlog(0,.false.
     &,'Field Tool: Taking into account pi+-, mu+-, p and pbar tracks.')
            print*,"Internal Cuts pi,mu,p (GeV):"
            write(*,'(3E14.3)'), fpicut,fmucut,fpcut
         else
            countpimu=.false.
            call putlog(0,.false.
     &,'Field Tool: NOT taking into account pi+-, mu+-, p and pbar.')
         endif
c     
         if(calcf .eq. 1) then
            calcfield=.true.
            call putlog(0,.false.
     &,'Field Tool: Field calculation is ON.')
         else
            calcfield=.false.
            calcfreq=.false.
            calctime=.false.
            calcfreqfres=.false.
            calctimefres=.false.
            call putlog(0,.false.
     &,'Field Tool: Field calculation is OFF.')
            close(211)          !close input file
            goto 150 !sets tracklength output files
         endif
      else
         trackflag=.false.
         countpimu=.false.
         calcfield=.false.
         call putlog(0,.false.
     &,'Field Tool: Tracklength calculation is OFF.')
         call putlog(0,.false.
     &,'Field Tool: Field calculation is OFF.')
         close(211) !close input file
         return     !no more setting needed if not calculating tracks
      endif
c
c     Other parameters are read and grid set only if calcfield is ON
c
      read(211,*) cfreq
      if (cfreq .eq. 1)then
         calcfreq=.true.
         call putlog(0,.false.
     &,'Field Tool: Fraunhoffer frequency-domain calculation is ON.')
      else
         calcfreq=.false.
         call putlog(0,.false.
     &,'Field Tool: Fraunhoffer frequency-domain calculation is OFF.')
      endif
      read(211,*) ctime
      if (ctime .eq. 1)then
         calctime=.true.
         call putlog(0,.false.
     &,'Field Tool: Fraunhoffer time-domain calculation is ON.')
      else
         calctime=.false.
         call putlog(0,.false.
     &,'Field Tool: Fraunhoffer time-domain calculation is OFF.')
      endif
      read(211,*) cfreqfres
      if (cfreqfres .eq. 1)then
         calcfreqfres=.true.
         call putlog(0,.false.
     &,'Field Tool: Fresnel frequency-domain calculation is ON.')
      else
         calcfreqfres=.false.
         call putlog(0,.false.
     &,'Field Tool: Fresnel frequency-domain calculation is OFF.')
      endif
      read(211,*) ctimefres
      if (ctimefres .eq. 1)then
         calctimefres=.true.
         call putlog(0,.false.
     &,'Field Tool: Fresnel time-domain calculation is ON.')
      else
         calctimefres=.false.
         call putlog(0,.false.
     &,'Field Tool: Fresnel time-domain calculation is OFF.')
      endif
c     
c     WILL STILL READ THE WHOLE GRID, EVEN IF NOT PERFORMING ALL CALCULATIONS!!
c     this is good, since you don't have to change fieldtool.inp
c
      read(211,*) dt_bin_ns   !Reads bin size (ns)
      dt_bin_s=dt_bin_ns*1.d-9
      read(211,*) ref_n

c     new input variable: use variable n: 1=ON, 0=OFF  ///////
      read(211,*) uvarn
      if((uvarn .eq. 1) .and. (usezhseloss .eqv. .false.) )then
         usevarn=.true.
         call putlog(0,.false.
     &        ,'Field Tool: Variable refractive index model is ON.')
c     set ns(sea level n for variable n) and kr
         ns=1.d6*(ref_n-1.d0)
         write(*,"(A35,1X,F6.1,4x,A3,1x,F9.5)"),
     + "      Variable n model: Setting ns=",ns,"kr=",kr
      else
         usevarn=.false.
         call putlog(0,.false.
     &        ,'Field Tool: Variable refractive index model is OFF.')
      endif
C     ////////////////////////////////////////////////////////

      
      read(211,*) numax,(freq(nu),nu=1,numax)
      read(211,*) jmax,(angles(j),j=1,jmax)
      read(211,*) kmax,(phi(k),k=1,kmax)
      read(211,*) namax,(xant(na),yant(na),zant(na),na=1,namax)

c=============================================================
c fresnel time conditioning. Note that this will not be used if simulation is fraunhfr only.
c=============================================================
      if(calctimefres .or. calcfreqfres)then
         print*,"Antena number and x,y,z [m],t0 [ns] in radio system:"
         rmax=-1.d10
         rmin= 1.d10
         dtmax=-1.d10
         dtmin= 1.d10
         do na=1,namax
c     calculate critical visibility parameter
            h0ant(na)=zant(na)
            if( (zant(na) .lt. 10.d0) .or. usezhseloss) then
               parcrit(na)=0.d0 !antenna lower than 10m above ground
            else
               cospctheta=Rtg/(Rtg+zant(na))
               pctheta=acos(cospctheta)
               sinpctheta=sin(pctheta)
               dcrit(na)=Rtg*sinpctheta
               parcrit(na)=(Rtg*(cospctheta-1.d0)-zant(na))/
     +              (Rtg*sinpctheta)
c               print*,"Non-zero parcrit: antn=",na,"zant(na)",zant(na),
c     +              "parcrit=",parcrit(na)
               if(parcrit(na)<minparcrit)then
                  minparcrit=parcrit(na)
c                  print*,"new minparcrit=",minparcrit
               endif
            endif
c     CHANGED INPUT FORMAT!!!! Now ZANTnew is in relation to the ground,
c     (for antennas on the ground, zantnew=0)
c     so will have to make ZANT=injz-(groundz+ZANTnew)
c     BUT ONLY FOR SHOWERS IN AIR!!!!!! 
c     Original ZHS system is still used for ice
            if(usezhseloss .eqv. .FALSE.)then 
               hant=zant(na) !Antenna heigh above ground
               zant(na)=injz-(groundz+zant(na))
            endif            
c     phi of antenna
            phiantrad=atan2(yant(na),xant(na))
            if(phiantrad .lt. 0)then
               phiantrad=2.d0*pi+phiantrad
            endif         
c     projection of r of antenna on shower phi direction
            angle=dabs(phidirrad-phiantrad)
c
            rproj=sqrt(yant(na)*yant(na)+xant(na)*xant(na))*
     +   cos(angle)
c
            dtna=-( hant/coszenith + (rproj-hant*tanzenith)*sinzenith)
     +     /cspeed
c
c         passing dtna to a fixed number of time bins
          dtna=INT(dtna/dt_bin_s)*dt_bin_s
          dt(na)=dtna
c
            if(dtna .gt. dtmax)then
               dtmax=dtna
            endif
            if(dtna .lt. dtmin)then
               dtmin=dtna
            endif
            write(*,"(I4,4F10.1)"),
     +               na,xant(na),yant(na),zant(na),dt(na)*1.d9
         enddo
c if time calculation in fresnel domine was requested
         if((usezhseloss .eqv. .FALSE.) .and. calctimefres)then
c     Will now reset new min/max time, depending on shower inclination:
c     new time shift allows signal to start from t=0 on, but
c     if antenna is above sea levelinclined, the signal can reach it before t=0 
      
            write(*,"(A21,1x,F10.2)"),
     +           "Maximum Required Delta t (ns):",dtmax*1.d9
            write(*,"(A21,1x,F10.2)"),
     +           "Minimum Required Delta t (ns):",dtmin*1.d9
            
c Matias: This 4 variables are redundant. Check if they are used elsewhere or delete them
            dmint=dtmin
            dmaxt=dtmax

            imint=dtmin*1.e9+starttime
            imaxt=endtime+dtmax*1.e9

            if(abs(endtime)> dt_bin_ns*maxt/2 .or. 
     +           abs(starttime)> dt_bin_ns*maxt/2 )then
               print*,"WARNING: TOO MUCH TIME RESOLUTION!!!"
               print*,"Bin size (ns):",dt_bin_ns
               print*,"End of pulse expected at (ns):",endtime
               print*,"Begin of pulse expected at (ns):",starttime
               print*,"Maximum absolute time for this bin (ns):",
     +              dt_bin_ns*maxt/2
               print*,"Please increase time bin size!!!!!!"
               print*,
     +      "(or increase maxt on file fieldcomm.f and recompile)"
            endif 

            TIME_MIN=imint
            TIME_MAX=imaxt
            
         endif                  !end if !usezhseloss

         if(calctimefres)then
            write(*,"(A18,1x,F8.1,4x,A9,F8.1)"),"Setting  Time_min:",
     +        TIME_MIN,"Time_max:",TIME_MAX
         endif
c
      endif !end if calctimefres or calcfreqfres
c     
c     
c     closing input file
      close(211)
c     
c==============================================================
c memmory allocation
c=============================================================
c at this point of the program, we know exactly how many antenas, how many angles,etc we will be using
c and wich calculations we will perform. We can now allocate the necessary arrays in an efficient way
c
      ntbins=maxt
      maxthalf=maxt/2
c     remember to change maxthalf if you change ntbins!      
      call fieldmemalloc()
c
c==============================================================
c     START OF FIELD TOOL's DEFINE GRID BLOCK =================
c     changed FACTOR for use with meters
      FACTOR=2.D0*PI/2.99792458D2 !  Frequency must be in MHz
      CHECOS=1.D0/REF_N
      CHERAN=ACOS(CHECOS)       ! Radians
      CHERDG=CHERAN*RADEGR      ! Degrees
C     
C     **************************************************************************
C     GRID in frequency and observation angle with respect to shower axis.
C     It also calculates factors that are frequently used and puts them in 
C     an array in order to speed up the code.
C     **************************************************************************
C     
      DO NU=1,NUMAX
         FACFRQ(NU)=FACTOR*FREQ(NU)
         FRQCOM(NU)=FREQ(NU)
         JEND(NU)=JMAX
      END DO
c     
      DO J=1,JMAX
         IF (abs(ANGLES(J)) .LT. 1.d-4) then
            JCHEC=J
         endif
         IF (ANGLES(J) .LT. -CHERDG) THEN
            THETA(J) = 0.
         ELSE
            THETA(J)=CHERDG+ANGLES(J)
         END IF
      END DO
      
      DO J=1,JMAX
         THETA(J)=THETA(J)/RADEGR
         SINTET(J)=SIN(THETA(J))
         SINTET2(J)=SINTET(J)*SINTET(J)
         SINMU(J)=SINTET(J)*REF_N
         COSTET(J)=COS(THETA(J))
         COSTET2(J)=COSTET(J)*COSTET(J)
         COSMU(J)=REF_N*COSTET(J)
         CSMU_1(J)=REF_N*COSTET(J)-1.D0
      END DO

      DO K=1,KMAX
         PHI(K)=PHI(K)/RADEGR
         SINPHI(K)=SIN(PHI(K))
         COSPHI(K)=COS(PHI(K))
      END DO  
      
c     END OF FIELD TOOL DEFINE GRID BLOCK==========
c     MATIAS:Log configuration. Standard output
      call zhairestatus(6, nlines)
c     MATIAS:Log configuration. lgf file
      call zhairestatus(7, nlines)
c
c     open output files for root
      if(calcfreq)then
         open(unit=551,status='old',access='append',
     +        file='freqfraunh-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File freqfraunh-root.dat exists.')
            write(551,333) '# new run started'
            close(551)
         else
            open(unit=551,status='new',file='freqfraunh-root.dat')
            write(551,333) '# -----------Freq Fraunhofer file----------'
            write(551,333) '# Columns:'
            write(551,333) '#        1 - Shower Number'
            write(551,333) '#        2 - Theta #'
            write(551,333) '#        3 - Phi #'
            write(551,333) '#        4 - Theta (deg)'
            write(551,333) '#        5 - Phi (deg)'
            write(551,333) '#        6 - Frequency #'
            write(551,333) '#        7 - Frequency (MHz)'
            write(551,333) '#        8 - R x |E| (V/M MHz)'
            write(551,333) '#        9 - Phase'
            write(551,333) '#        10 - R x Ex (V/M MHz)'
            write(551,333) '#        11 - Phase Ex'
            write(551,333) '#        12 - R x Ey (V/M MHz)'
            write(551,333) '#        13 - Phase Ey'
            write(551,333) '#        14 - R x Ez (V/M MHz)'
            write(551,333) '#        15 - Phase Ez'
            if (countpimu) then
               write(551,333)
     +  '# Charged Pions and Muons were added to these total sums!!!'
               write(551,333) '#'
            endif
            close(551) 
         endif
      endif


      if(calctime)then
         open(unit=552,status='old',access='append',
     +        file='timefraunh-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File timefraunh-root.dat exists.')
            write(552,333) '# new run started'
            close(552)
         else
            open(unit=552,status='new',file='timefraunh-root.dat')
            write(552,333) '# -----------Time Fraunhofer file----------'
            write(552,333) '# Columns:'
            write(552,333) '#        1 - Shower Number'
            write(552,333) '#        2 - Theta #'
            write(552,333) '#        3 - Phi #'
            write(552,333) '#        4 - Theta (deg)'
            write(552,333) '#        5 - Phi (deg)'
            write(552,333) '#        6 - Time (ns)'
            write(552,333) '#        7 - R x |A| (V/M)'
            write(552,333) '#        8 - R x Ax  (V/M)'
            write(552,333) '#        9 - R x Ay  (V/M)'
            write(552,333) '#        10- R x Az  (V/M)'
            write(552,333) '#        11- R x |E| (V/M)'
            write(552,333) '#        12- R x Ex  (V/M)'
            write(552,333) '#        13- R x Ey  (V/M)'
            write(552,333) '#        14- R x Ez  (V/M)'
            if (countpimu) then
               write(552,333)
     +    '# Charged Pions and Muons were added to these total sums!!!'
               write(552,333) '#'
            endif
            close(552)
         endif
      endif 
      
      if(calcfreqfres)then
         open(unit=553,status='old',access='append',
     +        file='freqfresnel-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File freqfresnel-root.dat exists.')
            write(553,333) '# new run started'
            close(553)
         else
            open(unit=553,status='new',file='freqfresnel-root.dat')
            write(553,333) '# -----------Freq Fresnel file----------'
            write(553,333) '# Columns:'
            write(553,333) '#        1 - Shower Number'
            write(553,333) '#        2 - Antenna Number'
            write(553,333) '#        3 - Antenna X (m)'
            write(553,333) '#        4 - Antenna Y (m)'
            write(553,333) '#        5 - Antenna Z (m)'
            write(553,333) '#        6 - Frequency #'
            write(553,333) '#        7 - Frequency (MHz)'
            write(553,333) '#        8 - |E| (V/M MHz)'
            write(553,333) '#        9 -  Ex  (V/M MHz)'
            write(553,333) '#        10 - Ey  (V/M MHz)'
            write(553,333) '#        11 - Ez  (V/M MHz)'
            if (countpimu) then
               write(553,333)
     +    '# Charged Pions and Muons were added to these total sums!!!'
               write(553,333) '#'
            endif
            close(553)
         endif
      endif
      
      if(calctimefres)then
         open(unit=554,status='old',access='append',
     +        file='timefresnel-root.dat',iostat=iosn)
         if(iosn .eq. 0)then
            call putlog(0,.false.,
     +           'Field Tool: File timefresnel-root.dat exists.')
            write(554,333) '# new run started'
            close(554)
         else
            open(unit=554,status='new',file='timefresnel-root.dat')
            write(554,333) '# -----------Time Fresnel file----------'
            write(554,333) '# Columns:'
            write(554,333) '#        1 - Shower Number'
            write(554,333) '#        2 - Antenna Number'
            write(554,333) '#        3 - Antenna X (m)'
            write(554,333) '#        4 - Antenna Y (m)'
            write(554,333) '#        5 - Antenna Z (m)'
            write(554,333) '#        6 - Time (ns)'
            write(554,333) '#        7 - |A| (V/M)'
            write(554,333) '#        8 -  Ax  (V/M)'
            write(554,333) '#        9 -  Ay  (V/M)'
            write(554,333) '#        10 - Az  (V/M)'
            write(554,333) '#        11 -|E| (V/M)'
            write(554,333) '#        12 - Ex  (V/M)'
            write(554,333) '#        13 - Ey  (V/M)'
            write(554,333) '#        14 - Ez  (V/M)'
            if (countpimu) then
               write(554,333)
     +    '# Charged Pions and Muons were added to these total sums!!!'
               write(554,333) '#'
            endif
            close(554)
         endif
      endif
c
 150  continue
c     Opening tracksum file
      open(unit=411,status='old',access='append',
     +file='tracklengths.dat',err=160)
      call putlog(0,.false.,'Field Tool: File tracklengths.dat exists.')
      write(411,333) '# new run started'
      close(411)
c     Opening PIMU tracksum file
      open(unit=412,status='old',access='append',
     +file='pimutracklengths.dat',err=200)
      call putlog(0,.false.
     +,'Field Tool: File pimutracklengths.dat exists.')
      write(412,333) '# new run started'
      close(412)
c     Opening per particle tracksum file
      open(unit=413,status='old',access='append',
     +file='perpcletracklengths.dat',err=200)
      call putlog(0,.false.
     +,'Field Tool: File perpcletracklengths.dat exists.')
      write(413,333) '# new run started'
      close(413)


      return
c
 160  continue
      open(unit=411,status='new',file='tracklengths.dat')
      write(411,333) '# -----------Tracksum file-------------'
      write(411,333) '# Columns:     (m)'
      write(411,333) '#        1-Shower Number'
      write(411,333) '#        2-Tracksum'
      write(411,333) '#        3-Sum Delta_z'
      write(411,333) '#        4-Sum Abs(Delta_z)'
      write(411,333) '#        5-Excess track sum (electron-positron)'
      write(411,333) '#        6-Excess sum Delta_z'
      write(411,333) '#        7-Excess sum Abs(Delta_z)'
      write(411,333) '#'
      if (countpimu) then
         write(411,333)
     + '# Charged Pions and Muons were added to these total sums!!!'
         write(411,333) '#'
      endif
      close(411)
c     test if PIMU tracksum file exists (it shouldn't at this point)
      open(unit=412,status='old',access='append',
     +file='pimutracklengths.dat',err=170)
      call putlog(0,.false.
     +,'Field Tool: ERROR-Only some tracklength file exists. Stopping.')
      stop
c      
 170  continue
      open(unit=412,status='new',file='pimutracklengths.dat')
      write(412,333) '# -----------PIMU Tracksum file-------------'
      write(412,333) '# Columns:     (m)'
      write(412,333) '#        1-Shower Number'
      write(412,333) '#        2-Tracksum'
      write(412,333) '#        3-Sum Delta_z'
      write(412,333) '#        4-Sum Abs(Delta_z)'
      write(412,333) '#        5-Excess track sum (negative-positive)'
      write(412,333) '#        6-Excess sum Delta_z'
      write(412,333) '#        7-Excess sum Abs(Delta_z)'
      write(412,333) '#'  
      if (countpimu) then
         write(412,333)
     + '#Charged Pions and Muons tracks are also being added to totals.'
         write(412,333)
     + '#Totals are in tracklengths.dat file.'         
         write(412,333) '#'
      endif
      close(412)
c     test if perpcle tracksum file exists (it shouldn't at this point)
      open(unit=413,status='old',access='append',
     +file='perpcletracklengths.dat',err=180)
      call putlog(0,.false.
     +,'Field Tool: ERROR-Only some tracklength file exists. Stopping.')
      stop      

 180   continue
      open(unit=413,status='new',file='perpcletracklengths.dat')
      write(413,333) '# -----------PIMU Tracksum file-------------'
      write(413,333) '# Columns:     (m)'
      write(413,333) '#        1-Shower Number'
      write(413,333) '#        2-Electron Tracksum'
      write(413,333) '#        3-Electron Sum Delta_z'
      write(413,333) '#        4-Electron Sum Abs(Delta_z)'
      write(413,333) '#        5-Positron Tracksum'
      write(413,333) '#        6-Positron Sum Delta_z'
      write(413,333) '#        7-Positron Sum Abs(Delta_z)'
      write(413,333) '#        8-Pi plus Tracksum'
      write(413,333) '#        9-Pi plus  Sum Delta_z'
      write(413,333) '#        10-Pi plus Sum Abs(Delta_z)'
      write(413,333) '#        11-Pi minus Tracksum'
      write(413,333) '#        12-Pi minus  Sum Delta_z'
      write(413,333) '#        13-Pi minus Sum Abs(Delta_z)'
      write(413,333) '#        14-Mu plus Tracksum'
      write(413,333) '#        15-Mu plus  Sum Delta_z'
      write(413,333) '#        16-Mu plus Sum Abs(Delta_z)'
      write(413,333) '#        17-Mu minus Tracksum'
      write(413,333) '#        18-Mu minus  Sum Delta_z'
      write(413,333) '#        19-Mu minus Sum Abs(Delta_z)'
      write(413,333) '#        20-Proton Tracksum'
      write(413,333) '#        21-Proton Sum Delta_z'
      write(413,333) '#        22-Proton Sum Abs(Delta_z)'
      write(413,333) '#        23-Pbar Tracksum'
      write(413,333) '#        24-Pbar Sum Delta_z'
      write(413,333) '#        25-Pbar Sum Abs(Delta_z)'
      write(413,333) '#'  
      close(413)
      return
c
 200  continue !only some tracksum file exists:  ERROR
      call putlog(0,.false.
     +,'Field Tool: ERROR-Only some tracklength file exists. Stopping.')
      stop
c
 333  format(A)
c
c     input file is not present, turning field tool off
c     and setting eloss to TIERRAS 
 300  continue !reaches this point if no fieldtool.inp available
c      call putlog(0,.false.,'Field Tool: fieldtool.inp was not found.')
c     turning field tool OFF
      trackflag=.false.
      calcfield=.false.
      countpimu=.false.
      usezhseloss=.false.
c     MATIAS: Commented for IDL compatibility.
c      call putlog(0,.false.
c     &,'Field Tool: Using TIERRAS e+- energy loss.')
c      call putlog(0,.false.
c     &,'Field Tool: Tracklength calculation is OFF.')
c      call putlog(0,.false.,'Field Tool: Field calculation is OFF.')
c
      fieldtool=.false.
      return
      end
c     --- End of routine initfieldidl     
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initsim.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
