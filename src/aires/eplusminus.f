c
c     FILE: eplusminus.f                    Creation date: 21/AUG/1996.
c                                       LAST MODIFICATION: 15/MAR/2011.
c
c     This file contains the routines which deal with e+ e-
c     processing.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine epmadv(npart, pdatarr, nrem, remidx)
c
c     Advancing electron or positrons and deciding their fates.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 2000, 2001, 2003.
c
C     ZHAireS alterations
C     version 0.02 - 25/3/2010 Washington Carvalho
C
C
c     Arguments:
c     =========
c
c     npart.......... (input, integer) The number of entries to
c                     process.
c     pdatarr........ (input, character*(*), array(npart))
c                     Array containing the particle data.
c     nrem........... (output, integer) Number of remaining particles
c                     in the stack.
c     remidx......... (output, integer, array(npart)) Indices of
c                     remaining particles.
c
c
      use iso_c_binding
      use grid
      implicit none
c
c     Compilation parameters.
c
c     TEST: injz,injdepth,groundalt,etc...
      
      include 'initpar.f'
      include 'initcomm.f'
c///////////////////

      include 'constants.f'
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'modelpar.f'
c
c     Declaration of arguments.
c
      integer           npart, nrem
      character*(*)     pdatarr(npart)
      integer           remidx(npart)
c
c     Particle data storage templates
c
      include 'pdata.f'
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
      include 'modelcomm.f'
C
C     FIELD TOOL: include field tool's commons
      include 'fieldcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           istack, charge
      double precision  inke2, lnvmin, meanbrempathneg
      double precision  f, u, s, v, vv, uvv, wsq, q, ke2
      double precision  path, step, maxpath, totpath
      double precision  alos, clos, blos, egyc, eminloss
      double precision  alosl, closl, blosl, dlosl
      double precision  korate, koratefa, koratefb, kopath
      double precision  lossrate, weff, prevtime
      logical           geosw
      logical           ispositron, nomore, plost, plowe
      double precision  screen
      double precision  fa4, fb4
      double precision  va, vb, vf, qq
      double precision  c, ke1, ct2, st2, cp2, sp2, upsec0(3)
      logical           goon, fatebrem, fatekno, fatecarry
      double precision  urandomt, urandom


C     ZHAireS variables++++++++++++++++

      double precision DELTAZ,DELTAX,DELTAY
      DOUBLE PRECISION Wgt0,lateral
      double precision track
      double precision injectionaltitude,injectiondepth
      double precision groundaltitude,zenithdeg,coszenith
      integer ityp

c     variables for xyz_m
      double precision ctstart,ctend,estart,eend
      double precision x_m,y_m,z_m
      double precision x1_m,y1_m,z1_m,x2_m,y2_m,z2_m

      double precision a,b,cc,d,e,ff,g,h,i,j,k

C     ZHS dEdX variables++++++++++++++++
      Integer itypzhs
      double precision EI,DEI,AP,TE1,T0
      double precision invkominzhs,koratefpazhs,koratefpbzhs
      double precision koratefeazhs,koratefebzhs
C     Ground correction variables
      double precision expeceend,oldstep,oldk

c     AP:  Bremsstrahlung threshold (MeV)
c     TE1: Ionization (Delta ray) threshold (MeV) 
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     initializing variables
c
c     used to transform aires time into s (t0=0s when front reaches ground)
      injectionaltitude=injz
      injectiondepth=injdepth
      groundaltitude=groundz
c     NOTE: zenith angle must be fixed for all showers!
c     If not must use showercomm.f to get particular zenith angle (not yet done)
      zenithdeg=pryzenithmin
      coszenith=cos(zenithdeg*dacos(-1.d0)/180.d0)
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++


c
c     FIRST EXECUTABLE STATEMENT
c     FIELD TOOL: initialize variables for ZHS dEdX+++++++++++++++++++++
      if(usezhseloss)then
         AP=0.1!0.1 MeV  (calculation of lossrate in ZHS is in MeV)
         TE1=1.d-4/electronmass !0.1 MeV in emass units
c     AP and TE1 are processed by elossZHS and should be in MeV units and
c     Me units, respectively
         T0=3.07441783*0.924/0.0788316652
c     initialize variables for lower Knock-on threshold
         invkominzhs=2.d4       !1/0.05MeV in GeV^-1
c     note that KO will be treated down to 2*komin !!
         koratefpazhs    =   0.153d-3 * avzovera * invkominzhs
         koratefpbzhs    =   koratefpb
         koratefeazhs    =   0.345d-3 * avzovera * invkominzhs
         koratefebzhs    =   koratefeb
      endif
c     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
      nrem = 0
c
      eminloss = emineploss
c
c     Particle stack loop.
c
      do istack = 1, npart
c
c       Decoding the particle data.
c
        pclerecord = pdatarr(istack)
c
c       Now the following variables are defined and available:
c
c         prcode          Particle type (code).
c         prenergy        Energy (GeV).
c         prwt            Weight (GE 1).
c         prx             \
c         pry              > Particle coordinates (m).
c         prz             /
c         prvz            Vertical (local) altitude (m).
c         prdepth         Depth (g/cm2).
c         prtime          Current particle time (*).
c         prpx            \
c         prpy             > Direction of the impulse (current).
c         prpz            /
c         prpxpr          \  Direction of the impulse (used for last
c         prpypr           > straight line propagation).
c         prpzpr          /
c         prcdepth        Particle creation depth (g/cm2).
c         prctime         Particle creation time (*).
c         prlhdepth       Depth of last hadronic interaction.
c         prbeta          Particle speed (internal use only).
c         prcal           Current atmospheric layer.
c         prlol           Observing level internal data (do not mo-
c                         dify and transmit for decay products).
c         prlowe          Switch to label low energetic particles.
c
c         psparef         double precision spare positions.
c         psparei         Integer spare positions.
c         psparel         Logical spare positions.
c
c       Particle code, prlol and prcal are integers,
c       prlowe is logical and all other variables are double precision.
c       All the named variables can also be referred as positions of
c       the main particle data array "frecord" replacing the "pr" by
c       "ix", for example: pry is equivalent to frecord(ixy).
c
c       (*) Time is set to zero either at the beginning of the shower
c           or at the fisrt primary interaction.
c
c       In this routine prcode should be either "electroncode" or
c       "positroncode".
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c          psparel(1) is true when the particle must be processed
c                     in the second stack.
c
c       BEGINNING OF PARTICLE PROCESSING.
c
c       Setting some variables that depend on the sign of the charge.
c

C     FIELD TOOL: new particle block ++++++++++++++++++++++++
c     New particle !!!!
        if(trackflag) then
c     particle code ityp (electron=1 positron=-1)
           if (prcode .eq. positroncode) then
              ityp=-1
              itypzhs=1
           else
              ityp=1
              itypzhs=0
           endif
c     Weigth
           Wgt0=prwt

c     Variables for this point==================
c     Set particle x and y
           x_m=prx
           y_m=pry
c     calculate z_m from prz
           z_m=(injectionaltitude-prz)
c     ==========================================
c     set this point as start point
           x1_m=x_m
           y1_m=y_m
           z1_m=z_m
c     for empsum_t call
           ctstart=prtime
           estart=(prenergy+electronmass)*1.d3
c
        endif
c     FIELD TOOL: end of new particle block ++++++++++++

        if (prcode .eq. positroncode) then
c
          ispositron = .true.
          charge     = 1
c
          if (prlowe) then
            plowe = .true.
            goto 1100
          endif
c
          alos       = aplos
          blos       = bplos
          clos       = cplos
          alosl      = aplosl
          blosl      = bplosl
          closl      = cplosl
          dlosl      = dplosl
          koratefa   = koratefpa
          koratefb   = koratefpb
c     FIELD TOOL: Set parameters for dEdX calculation
          if(usezhseloss)then
             itypzhs=1          !positron
             koratefa   = koratefpazhs
             koratefb   = koratefpbzhs
          endif
c     +++++++++++++
c
        else
c
          ispositron = .false.
          charge     = -1
          alos       = aelos
          blos       = belos
          clos       = celos
          alosl      = aelosl
          blosl      = belosl
          closl      = celosl
          dlosl      = delosl
          koratefa   = koratefea
          koratefb   = koratefeb
c     FIELD TOOL: Set parameters for dEdX calculation
          if(usezhseloss)then
             itypzhs=0          !electron
             koratefa   = koratefeazhs
             koratefb   = koratefebzhs
          endif
c     +++++++++++++          
c
        endif
c
c       Other particle initializations.
c
        call lgtinit(prcode, charge, pclerecord, frecord, geosw)
c
 1005   continue
c
        fatebrem   = .false.
        fatekno    = .false.
        fatecarry  = .true.
        psparel(1) = .false.
        psparei(0) = 0
c
        if (prenergy .gt. minbremenergy) then
c
          lnvmin          = log(ebremenergycut / prenergy)
          meanbrempathneg = brempath / lnvmin
          path            = meanbrempathneg * log(urandomt(0.d0))
c
c         Fate is bremsstrahlung (first trial).
c
          fatebrem  = .true.
          fatecarry = .false.
c
        else
          path   = 1.d8
          lnvmin = -0.001d0
        endif
c
        korate = koratefa + koratefb / prenergy
c
c       Checking knock-on.
c
        if (korate .gt. 1.d-4) then
c
c         exp1 is to reduce calls to log.
c
          if (exp1 .lt. 0) exp1 = -log(urandomt(0.d0))
          kopath = exp1 / korate
c
          if (kopath .lt. path) then
c
c           Fate is knockon.
c
            path = kopath
            exp1 = - log(urandomt(0.d0))
            q    = 1.d0 / prenergy
c     FIELD TOOL: use zhs minimum KO energy+++++++++
            if(usezhseloss)then
               ke2  = 1.d0 / (q + urandom() * (invkominzhs - q))
            else
               ke2  = 1.d0 / (q + urandom() * (invkomin - q))
            endif
c     ++++++++++++++++++++++++++++++++++++++++++++++
c
            fatebrem  = .false.
            fatekno   = .true.
            fatecarry = .false.
c
          else
c
            exp1 = exp1 - path * korate
c
          endif
        endif
c
c       Now the electron/positron is going to be advanced.
c       Notice that our z axis points upwards, and therefore prpz is
c       always negative for descending particles.
c
c       The advancing process is done in repeated steps to account
c       for energy loosing. This imposes "in-process" longitudinal
c       monitoring.
c
        totpath = path
c
 1010   continue
c
c       Processing a fraction of the entire path.
c
        if (prdepth .gt. 200.d0) then
          if (prenergy .le. 1.6d-3) then
            maxpath = 0.16d0
          else if (prenergy .gt. 0.04d0) then
            maxpath = 4.00d0
          else
            maxpath = 0.10d3 * prenergy
          endif
        else if (prdepth .gt. 1.0d0) then
          if (prenergy .le. 2.5d-3) then
            maxpath = 0.0177d0
          else if (prenergy .gt. 0.04d0) then
            maxpath = 0.2828d0
          else
            maxpath = 7.0711d0 * prenergy
          endif
          maxpath = maxpath * sqrt(prdepth)
        else if (prdepth .gt. (4 * xquantum)) then
          maxpath = prdepth / 4
        else
          maxpath = xquantum
        endif
c
        if (totpath .gt. maxpath) then
          path    = maxpath
          totpath = totpath - path
        else
          path    = totpath
          totpath = 0
        endif
c
c       Check on positron annihilation in flight.
c       This is done because cross-section rises quickly as energy
c       falls.
c
        if (ispositron) then
c
c         f is approximate estimation of mean kinetic energy along
c         path. Rough loss rate is ~2 MeV/(g/cm2) for air.
c
          f = max(0.0d0, prenergy - 2.0d-3 * path)
          f = 0.5d0 * (f + prenergy)
c
c         Underestimated mfp for annihilation.
c
          if (f .lt. panne1lim) then
            s = panns10 * f * (f + panns11) / (f + panns12)
          else if (f .le. panne2lim) then
            s = panns20 * (f + panns21)
          else
            s = panns30 * (f + panns31)
          endif
c
          if ((s * urandom()) .lt. path) then
c
c           Do more detailed test for annihilation.
c
c           Rejection factor calculated using a fit to Heitler formula.
c
            u = log(f)
            if (f .le. panne4lim) then
              v = pannv40 + u * (pannv41 + u * pannv42)
            else
              v = pannv50 + u * (pannv51 + u * pannv52) +
     +            pannv53 / (u + pannv54)
            endif
            v = s * exp(v)
c
            if (urandom() .lt. v) then
c
c             Fate is annihilation.
c
              fatebrem   = .false.
              fatekno    = .false.
              fatecarry  = .false.
              psparei(0) = 5
              psparel(1) = .true.
              path       = path * urandomt(2.d-8)
              totpath    = 0
c
            endif
          endif
        endif
c
c       We treat the atmosphere as a continuum (Layers are for
c       calculation purposes only. No physical meaning.)
c       This implies pathinmed being equal to path.
c
        step = 0.5d0 * path
c
c       Saving current time and particle energy.
c
        prevtime = prtime
        inke2    = prenergy + electronmass
c
c       Entering the advance and longitudinal monitoring procedure
c       (1st. half path).
c
c     FIELD TOOL: choose which lossrate to use+++++++++++++++++++++++
        if(usezhseloss)then     !use ZHS lossrate
c
c     EI is TOTAL energy in MeV, prenergy is kinectic energy in GeV
           EI=(prenergy+electronmass)*1.d3
           if(EI .ge. 120.0)then
              lossrate=1.6512e-3 !GeV/gcm2
           else
              call lossrateZHS(itypzhs,EI,TE1,AP,DEI)
c     DEI/T0 is lossrate in MeV/g/cm2 and lossrate is in GeV/g/cm2
              lossrate=DEI/(T0*1.d3)
           endif
c
c     original lossrate calculation
        else if (prenergy .ge. eminloss) then
           lossrate = alos - blos / (prenergy + clos)
        else
           egyc     = prenergy + closl
           lossrate = alosl + (dlosl / egyc - blosl) / egyc
        endif
c     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     TEST: analyse energy: ERASE//////////////
c        expeceend=(prenergy-lossrate*step+electronmass)*1.d3
c        oldstep=step
        oldk=prenergy
c     /////////////////////////////////////////

c
        call lgtmaster(prcode, step, electronmass, lossrate, epmcut1,
     +                 pclerecord, frecord, prlol, prcal, prlowe,
     +                 nomore, plost, plowe)

C     FIELD TOOL: start of first advance block ++++++++++++
        if (trackflag) then
c     particle advanced by step!!
c     Variables for this point==================
           x_m=prx
           y_m=pry
c     calculate z_m from prz
           z_m=(injectionaltitude-prz)
c     ==========================================
c     calculate Deltas==========================
           deltax=x_m-x1_m
           deltay=y_m-y1_m
           deltaz=z_m-z1_m

c     for empsum_t call
           x2_m=x_m
           y2_m=y_m
           z2_m=z_m
           ctend=prtime
           eend=(prenergy+electronmass)*1.d3

c     TEST: AIRES fixes the energy when the track goes underground,
c     but it doesn't fix the end position of track!
c     Revert to "original" final energy and check ground on empsum_XX
           if( (nomore) .and. (.not. plowe) .and. (.not. plost))then
              eend=(oldk-lossrate*step+electronmass)*1.d3
c$$$              if(step .ne. oldstep)then
c$$$                 print*,"$$$$$$$$$$$$$$$$$$$$$$$$$"
c$$$                 print*,
c$$$     +             "oldstep:",oldstep,"newstep:",step
c$$$                 print*,
c$$$     +                "oldK:",oldk," newK:",prenergy,"cut:",epmcut1
c$$$                 print*,
c$$$     +                "new eend:",eend,"calcnewk:",oldk-lossrate*step
c$$$                 print*,"$$$$$$$$$$$$$$$$$$$$$$$$$"
c$$$              endif
c$$$              if(eend .lt. electronmass*1.e3) then
c$$$                 print*,"lowEend:",eend,"K=",
c$$$     +                1.e-3*(eend-electronmass*1.e3)
c$$$              endif
           endif
c     
c     tracksums (from fieldcomm.f)
           track=Wgt0*dsqrt(deltax*deltax+deltay*deltay+deltaz*deltaz)
           tracksum=tracksum+track
           sumdeltaz=sumdeltaz+Wgt0*deltaz
           sumabdeltaz=sumabdeltaz+Wgt0*dabs(deltaz)
           excesssum=excesssum+ityp*track
           excesssumz=excesssumz+ityp*Wgt0*deltaz
           excessabsumz=excessabsumz+ityp*Wgt0*dabs(deltaz)
c     per particle
           if(prcode .eq. electroncode)then
              emtracksum=emtracksum+track
              emsumdeltaz=emsumdeltaz+Wgt0*deltaz
              emsumabdeltaz=emsumabdeltaz+Wgt0*dabs(deltaz)
           else
              eptracksum=eptracksum+track
              epsumdeltaz=epsumdeltaz+Wgt0*deltaz
              epsumabdeltaz=epsumabdeltaz+Wgt0*dabs(deltaz)
           endif

      call update_vector(prcode,x1_m,y1_m,z1_m,ctstart,
     +       x2_m, y2_m, z2_m, ctend, estart, eend, wgt0, xant(1),
     +       yant(1), zant(1))
c      call add_one

c     ==========================================
c     Call field tool's EMPSUM subroutine
           if(calcfield)then
              if(calcfreq)then
                 call empsum(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
              if(calctime)then
                 call empsum_t(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
              if(calcfreqfres)then
                 call empsum_fr(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
              if(calctimefres)then
                 call empsum_t_fr(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
           endif
c
c      EXIT
c     =================================
c     set this point as new start point
           x1_m=x_m
           y1_m=y_m
           z1_m=z_m
           estart=eend
           ctstart=ctend
c     
        endif
C     FIELD TOOL: end of first advance block++++++++++++++++++++++++++++

c
        if (nomore) goto 1100
c
c       End of advancing sector.
c
c       We now know that the particle is still stacked.
c
        wsq  = (prenergy + electronmass) * inke2
        weff = sqrt(wsq)
c
c       Magnetic field deflection. The deflection is evaluated here,
c       but takes into account the two halves the path is divided in.
c       It is assumed that the difference between the corresponding
c       lengths (in meters) of both halves is small, especially for
c       those particles suffering significant magnetic delflection
c       (low energy ones).
c
        if (geosw) then
          call geobdeflect(prtime - prevtime, weff, frecord(ixpx))
        endif
c
c       Treating Coulomb and multiple scattering.
c
        if ((weff .gt. erough) .and. (path .gt. 0.002d0)) then
          call coulscatter(epmchimin, frecord, path, sqremass,
     +                     wsq, weff)
        endif 
c
c       Entering the advance and longitudinal monitoring procedure
c       (2nd. half path).
c
c     FIELD TOOL: choose which lossrate to use++++++++++++++++
        if(usezhseloss)then     !use ZHS lossrate
c
c     EI is TOTAL energy in MeV, prenergy is kinectic energy in GeV
           EI=(prenergy+electronmass)*1.d3
           if(EI .ge. 120.0)then
              lossrate=1.6512e-3 !GeV/gcm2
           else
              call lossrateZHS(itypzhs,EI,TE1,AP,DEI)
c     DEI/T0 is lossrate in MeV/g/cm2 and lossrate is in GeV/g/cm2
              lossrate=DEI/(T0*1.d3)
           endif
c     
c     original lossrate calculation
        else if (prenergy .ge. eminloss) then
           lossrate = alos - blos / (prenergy + clos)
        else
           egyc     = prenergy + closl
           lossrate = alosl + (dlosl / egyc - blosl) / egyc
        endif

c     TEST: analyse energy: ERASE//////////////
c        expeceend=(prenergy-lossrate*step+electronmass)*1.d3
c        oldstep=step
        oldk=prenergy
c     /////////////////////////////////////////

c       
        call lgtmaster(prcode, step, electronmass, lossrate, epmcut1,
     +                 pclerecord, frecord, prlol, prcal, prlowe,
     +                 nomore, plost, plowe)
c     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     FIELD TOOL: start of second advance block  +++++++++
        if (trackflag) then
c     particle advanced by step!!
c     Variables for this point==================
           x_m=prx
           y_m=pry
c     calculate z_m from prz
           z_m=(injectionaltitude-prz)
c     ==========================================
c     calculate Deltas==========================
           deltax=x_m-x1_m
           deltay=y_m-y1_m
           deltaz=z_m-z1_m
c     for empsum_t call
           x2_m=x_m
           y2_m=y_m
           z2_m=z_m
           ctend=prtime
           eend=(prenergy+electronmass)*1.d3
c     TEST: AIRES fixes the energy when the track goes underground,
c     but it doesn't fix the end position of track!
c     Revert to "original" final energy and check ground on empsum_XX
           if( (nomore) .and. (.not. plowe) .and. (.not. plost))then
              eend=(oldk-lossrate*step+electronmass)*1.d3
c$$$              if(step .ne. oldstep)then
c$$$                 print*,"$$$$$$$$$$$$$$$$$$$$$$$$$"
c$$$                 print*,
c$$$     +             "oldstep:",oldstep,"newstep:",step
c$$$                 print*,
c$$$     +                "oldK:",oldk," newK:",prenergy,"cut:",epmcut1
c$$$                 print*,
c$$$     +                "new eend:",eend,"calcnewk:",oldk-lossrate*step
c$$$                 print*,"$$$$$$$$$$$$$$$$$$$$$$$$$"
c$$$              endif
c$$$              if(eend .lt. electronmass*1.e3) then
c$$$                 print*,"lowEend:",eend,"K=",
c$$$     +                1.e-3*(eend-electronmass*1.e3)
c$$$              endif
           endif

c     tracksums (from fieldcomm.f)
           track=Wgt0*dsqrt(deltax*deltax+deltay*deltay+deltaz*deltaz)
           tracksum=tracksum+track
           sumdeltaz=sumdeltaz+Wgt0*deltaz
           sumabdeltaz=sumabdeltaz+Wgt0*dabs(deltaz)
           excesssum=excesssum+ityp*track
           excesssumz=excesssumz+ityp*Wgt0*deltaz
           excessabsumz=excessabsumz+ityp*Wgt0*dabs(deltaz)
c     per particle
           if(prcode .eq. electroncode)then
              emtracksum=emtracksum+track
              emsumdeltaz=emsumdeltaz+Wgt0*deltaz
              emsumabdeltaz=emsumabdeltaz+Wgt0*dabs(deltaz)
           else
              eptracksum=eptracksum+track
              epsumdeltaz=epsumdeltaz+Wgt0*deltaz
              epsumabdeltaz=epsumabdeltaz+Wgt0*dabs(deltaz)
           endif

c     ==========================================
      call update_vector(prcode,x1_m,y1_m,z1_m,ctstart,
     +       x2_m, y2_m, z2_m, ctend, estart, eend, wgt0, xant(1),
     +       yant(1), zant(1))
     
c     Call field tool's EMPSUM subroutine
           if(calcfield)then
              if(calcfreq)then
                 call empsum(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
              if(calctime)then
                 call empsum_t(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
              if(calcfreqfres)then
                 call empsum_fr(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
              if(calctimefres)then
                 call empsum_t_fr(ityp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                y2_m,z2_m,ctend,estart,eend,Wgt0)
              endif
           endif
           
c      EXIT
c
c     =================================

c     set this point as new start point and set weight
           x1_m=x_m
           y1_m=y_m
           z1_m=z_m
           estart=eend
           ctstart=ctend
c     
        endif
C     FIELD TOOL: end of second advance block ++++++++++++


        if (nomore) goto 1100
c
c       End of advancing sector.
c
c       We now know that the particle is still stacked.
c
c       If there remain some path pieces to advance, return to the
c       initial point.
c
        if (totpath .gt. 0) goto 1010
c
c       The complete path has been advanced, and the particle was
c       not lost.
c       Accordingly with the current particle fate we are going
c       to keep the particle in the stack or return to the
c       beginning for further advancing it.
c
        if (fatecarry) goto 1005
c
        if (fatebrem) then
c
c         Current fate is bremsstrahlung.
c
          if (prenergy .le. ebremenergycut) goto 1005
c
          ke2 = prenergy *
     +          ((ebremenergycut / prenergy) ** urandomt(0.d0))
c
c         The preceding approximate distribution produces secondary
c         gammas with energies in the range [ebremenergycut, prenergy].
c
c         Taking into account atomic screening effects. The following
c         code tries to reproduce Rossi's differential cross-sections.
c
          u  = prenergy + electronmass
          v  = ke2 / u
          vv = 1 - v
          va = 1 + vv ** 2
          vb = 0.66666666667d0 * vv
c
          if (vv .gt. 0) then
            uvv    = u * vv
            screen = screenfac * v / uvv
          else
            uvv    = electronmass
            screen = screenlim
          endif
c
          if (screen .lt. 2.d0) then
            fa4 = 5.21d0 - screen * (0.98d0 - 0.158d0 * screen)
            fb4 = fa4 - 0.168d0 * exp(-4 * screen)
            f   = va * (fa4 - thirdlogz) - vb * (fb4 - thirdlogz)
          else
            c   = 1.d0 /
     +            (screen * (2.281d0 + 0.025d0 * screen ** 2))
            f   = 4.798317367d0 - log(screen) - c - thirdlogz
            f   = f * (va - vb)
          endif
c
          if ((urandom() * mefo) .lt. f) then
c
c           Correction for Landau-Pomeranchuk-Migdal effect.
c
            if (prenergy .gt. lpmthreshold) then
              call lpmeffect(ke2, u, u - ke2, prdepth, .false., goon)
            else
              goon = .true.
            endif
c
c           End of processing lpm effect.
c
            if (goon) then
c
c             Ok with bremsstrahlung. (if goon is false no action is
c             taken and the particle remains in the stack for
c             further processing).
c
              ke1 = prenergy - ke2
              c   = 40.d6 * (u ** 2) / log(2 * uvv /
     +                                     (v * electronmass))
              f   = urandom()
              ct2 = 1 + (f - 1) / (0.5d0 + c * f)
c
c             Deflecting and emitting the radiated photon.
c            
              call deflectr(frecord(ixpx), ct2, st2, cp2, sp2, upsec0)
c
              f = pfreeegy(prcode)
              call stack1pclek(1, prenergy + f, ke1 + f,
     +                         gammacode, ke2,
     +                         prwt, frecord(ixx), prdepth, prtime,
     +                         prlhdepth, upsec0, prlol, prcal, goon)

c     FIELD TOOL: Setting new weight after secondary emission
              if (trackflag) Wgt0=prwt        
c     +++++++++++++++++++++++++

c
c             Advancing a new time only if the particle must continue
c             in the stack.
c
              if (.not. goon) goto 1120
c
c             The particle looses energy.
c
              prenergy = prenergy - ke2
              plowe    = (prenergy .le. epmcut1)
              prlowe   = plowe
              if (plowe) goto 1100
c
            endif
          endif
          goto 1005
c
        else if (fatekno) then
c
c         Current fate is knock-on.
c         Checking the energy of secondary electron, taking into
c         account the current primary energy.
c
          if (ke2 .lt. 2.d-6) goto 1005
c
          if (ispositron) then
c
            if (ke2 .gt. (prenergy - 2.d-6)) goto 1005
c
c           Bhabha cross sections for positrons.
c
            u  = prenergy + electronmass
            q  = ke2 / prenergy
            f  = 1 - q
            qq = f ** 2
            vf = (1 - q * f) ** 2
            if (q .lt. 0.2d0) then
              v = vf
            else
              v = 0.21d-3 + 0.82d-3 * q
              v = 1 - v / (prenergy + 0.875d-3 +
     +                    52.5d-3 * f * (qq ** 2))
              v = v * vf
            endif
c
          else
c
            if ((2 * ke2) .gt. prenergy) goto 1005
c
c           Cross section for electrons.
c
            u  = prenergy + electronmass
            q  = ke2 / prenergy
            f  = 1.d0 / (1.d0 - q)
            qq = (u / prenergy) ** 2
            v  = qq * (f ** 2) + q ** 2
            v  = v - (3 * qq - 1) * q * f
            v  = v * prenergy / (2.25d0 * (u + electronmass))
c
          endif
c
          if (urandom() .lt. v) then
c
c           Knock-on accepted.
c           Ok with knock-on. (otherwise no action is taken and
c           the particle remains in the stack for further processing).
c
            ke1 = prenergy - ke2
            ct2 = sqrt(ke2 * (u + electronmass) /
     +                 (prenergy * (ke2 + twoemass)))
c
c           Deflecting and emitting the secondary electron.
c
            call deflectr(frecord(ixpx), ct2, st2, cp2, sp2, upsec0)
c
            f = pfreeegy(prcode)
            call stack1pclek(1, prenergy + f, ke1 + f,
     +                       electroncode, ke2, prwt,
     +                       frecord(ixx), prdepth,
     +                       prtime, prlhdepth, upsec0,
     +                       prlol, prcal, goon)

c     FIELD TOOL: Setting new weight after secondary emission
            if (trackflag) Wgt0=prwt        
c     +++++++++++++++++++++++++
c
c           Advancing a new time only if the particle must continue
c           in the stack.
c
            if (.not. goon) goto 1120
c
c           The particle looses energy.
c
            prenergy = ke1
            plowe    = (prenergy .le. epmcut1)
            prlowe   = plowe
            if (plowe) goto 1100
c
          endif
          goto 1005
c
        endif
c
c       The particle's fate is annihilation.
c       Keeping in the stack for further processing.
c
        nrem         = nrem + 1
        remidx(nrem) = istack
c
c       Reencoding the patricle data record.
c
        pdatarr(istack) = pclerecord
c
        goto 1120
c
c       Entry for lost and low energetic particles.
c
 1100   continue
c
c       Low energetic particles are kept in the stack, and
c       if positrons they are forced to annihilation.
c
        if (plowe) then
c
          psparel(1) = (ispositron .and. forceloweannih)
          if (psparel(1)) then
            psparei(0) = 5
            ke1        = prenergy * urandomt(1.d-4)
            call plostegy(prcode, prlol, prwt * (prenergy - ke1))
            prenergy   = ke1
          else
            psparei(0) = 9
          endif
c
c         Keeping in the stack for further processing.
c
          nrem         = nrem + 1
          remidx(nrem) = istack
c
c         Reencoding the patricle data record.
c
          pdatarr(istack) = pclerecord
        
c
        endif
c
 1120   continue
c
c       End of particle stack loop.
c
      enddo
c
      return
      end
c     --- End of routine epmadv
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine epmdecay(npart, pdatarr, nrem, remidx)
c
c     Processing the electrons and positrons that have already been
c     advanced by routine epmadv, accordingly with the fates that were
c     set.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000.
c
c
c     Arguments:
c     =========
c
c     npart.......... (input, integer) The number of entries to
c                     process.
c     pdatarr........ (input, character*(*), array(npart))
c                     Array containing the particle data.
c     nrem........... (output, integer) Number of remaining particles
c                     in the stack.
c     remidx......... (output, integer, array(npart)) Indices of
c                     remaining particles.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      integer           npart, nrem
      character*(*)     pdatarr(npart)
      integer           remidx(npart)
c
c     Particle data storage templates
c
      include 'pdata.f'
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
      include 'modelcomm2.f'
c
c     Declaration of internal variables and arrays.
c
      integer           istack
      double precision  u, vv, c, f
      double precision  p, pp, uop, e0, e1
      logical           ktp
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      nrem = 0
c
      do istack = 1, npart
c
c       Decoding the particle data.
c
        pclerecord = pdatarr(istack)
c
c       Now the following variables are defined and available:
c
c         prcode          Particle type (code).
c         prenergy        Energy (GeV).
c         prwt            Weight (GE 1).
c         prx             \
c         pry              > Particle coordinates (m).
c         prz             /
c         prvz            Vertical (local) altitude (m).
c         prdepth         Depth (g/cm2).
c         prtime          Current particle time (*).
c         prpx            \
c         prpy             > Direction of the impulse (current).
c         prpz            /
c         prpxpr          \  Direction of the impulse (used for last
c         prpypr           > straight line propagation).
c         prpzpr          /
c         prcdepth        Particle creation depth (g/cm2).
c         prctime         Particle creation time (*).
c         prlhdepth       Depth of last hadronic interaction.
c         prbeta          Particle speed (internal use only).
c         prcal           Current atmospheric layer.
c         prlol           Observing level internal data (do not mo-
c                         dify and transmit for decay products).
c         prlowe          Switch to label low energetic particles.
c
c         psparef         double precision spare positions.
c         psparei         Integer spare positions.
c         psparel         Logical spare positions.
c
c       Particle code, prlol and prcal are integers,
c       prlowe is logical and all other variables are double precision.
c       All the named variables can also be referred as positions of
c       the main particle data array "frecord" replacing the "pr" by
c       "ix", for example: pry is equivalent to frecord(ixy).
c
c       (*) Time is set to zero either at the beginning of the shower
c           or at the fisrt primary interaction.
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c          psparel(1) is true when the particle must be processed
c                     in the second stack.
c
c       BEGINNING OF PARTICLE PROCESSING.
c
c       Switching accordigly with the preselected fate.
c
c       Knock on is completely processed in the advance procedure.
c       Bremsstrahlung fate is completely processed in the
c       advance routine, so only annihilation must be
c       processed here.
c
        if (psparel(1)) then
c
c         Processing annihilation.
c
          u  = prenergy + twoemass
          p  = u / electronmass
          c  = p * p - 2
          pp = sqrt(p * (p - 2))
          e0 = 1 / (p + pp)
          vv = log((1 - e0) / e0)
c
c         Acceptance-rejection algorithm to simulate Heitler's formula.
c
 1010     continue
          e1 = e0 * exp(urandom() * vv)
          f  = 1 / e1 + p * (p * e1 - 2)
          if ((c * urandom()) .lt. f) goto 1010
c
c         Evaluating energies and angles of secondary gammas.
c
          esec(1) = e1 * u
          esec(2) = u - esec(1)
          uop     = p / pp
          wa(1)   = (1 - electronmass / esec(1)) * uop
          wa(2)   = (1 - electronmass / esec(2)) * uop
c
c         Deflecting and stacking the gammas.
c
          call pdeflectr(frecord(ixpx), wa, 3, upsec)
c
          seccode(1) = gammacode
          seccode(2) = gammacode
c
          call stacknpcled(1, u, 2, seccode, esec,
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prlhdepth, 3, upsec, prlol, prcal,
     +                     0, ktp, prwt, wa)
c
        else
c
c         The particle is kept in the stack.
c
          nrem         = nrem + 1
          remidx(nrem) = istack
c
        endif
c
      enddo
c
      return
c
      end
c     --- End of routine epmdecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine empsum(itypin,x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,
     +     Wgt0)
c
c     FIELD TOOL empsum2 (Jaime) subroutine
c     calculates fields for individual tracks and adds it to common 
c     zsum_xyz arrays
c
c     arguments:
c     
c     ityp         --> integer input: particle type (-1=positron, 1=electron)
c     [x,y,z]1     --> double precision input: start track position (m)
c     [x,y,z]2     --> double precision input: end track position (m)
c     ct1          --> start track time*c (m)
c     ct2          --> end track time*c (m)
c     E1           --> start track energy (MeV)
c     E2           --> end track energy (MeV)
c     Wgt0         --> double precision input: particle weight
c
      use Dyn_Array
      implicit none
c     include field tool's commons
      include 'fieldcomm.f'
      include 'constants.f'
c
c     local variables
      integer ityp,itypin
      integer j,nu,k
      double precision x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,Wgt0
      double precision e_av,g,g2,beta,r2_r1,phi1,dphi
      double precision beta_x,beta_y,beta_z
      double precision const1,cherfac_1,denom,ux,uy,uz
      double precision beta_perp,beta_perp_x,beta_perp_y,beta_perp_z
      complex*16 phase1,dphase,ephase1,edphase
      double precision e_m      !electron mass in MeV

c     Changes to include pi+-, p, p-bar and mu+- %%%%%%%%%%%%%%%%%%%%%
c     ityp: mu+=-3, mu-=3, pi+=-2, pi-=2, p=-4, p-bar=4
c     needs to set mass accordingly and change ityp back to sgn(ityp)
c
c     muons
      if (abs(itypin) .eq. 3)then
         e_m = 105.658367d0
         ityp=itypin/3  !change ityp back to +-1
c     pions
      else if (abs(itypin) .eq. 2)then
         e_m = 139.57018d0
         ityp=itypin/2  !change ityp back to +-1
c     protons/anti-protons
      else if (abs(itypin) .eq. 4)then
         e_m = protonmass*1.d3
         ityp=itypin/4 !change ityp back to +-1
c     e+-
      else
         e_m = electronmass*1.d3
         ityp=itypin
      endif
c     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c     Begin field tool's empsum block ++++++++
      if (Wgt0.le.1.d-10) return ! Do not compute EMPSUM when Wgt0 = 0 (protected for accuracy)
c ######
c  SIGN
c ######
c According to Feynman's formula a positive charge q>0
c travelling along z>0 produces an electric field 
c proportional to -q beta_perp

c Minus sign because ITYP=1 is an electron
c                    ITYP=-1 is a positron 
      CONST1=-Wgt0*DBLE(ITYP)

c Average beta in track
      E_AV=(E1+E2)/2.d0
      G=E_AV/e_M
      G2=G*G
      BETA=SQRT(1.d0-1.d0/G2)
c BETA is parallel to the vector (r2-r1)
c Compute unitary vector parallel to (r2-r1)
      R2_R1=SQRT((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
      IF (R2_R1.EQ.0.d0) THEN
         RETURN
      END IF 
      BETA_X=BETA*(X2-X1)/R2_R1
      BETA_Y=BETA*(Y2-Y1)/R2_R1
      BETA_Z=BETA*(Z2-Z1)/R2_R1

c Assume azimuth observation angle phi=0

      DO J=1,JMAX               ! Loop in theta

         DO K=1,KMAX            ! Loop in phi
            CHERFAC_1 = CT1 - SINMU(J)*COSPHI(K)*X1 
     +           - SINMU(J)*SINPHI(K)*Y1 - COSMU(J)*Z1 ! m 
            DENOM= 1.d0 - SINMU(J)*COSPHI(K)*BETA_X
     +           - SINMU(J)*SINPHI(K)*BETA_Y - COSMU(J)*BETA_Z

c k x (k x beta) 
            UX = SINTET(J)*COSPHI(K)
            UY = SINTET(J)*SINPHI(K)
            UZ = COSTET(J)

            BETA_PERP_X=-(UY*UY+UZ*UZ)*BETA_X + UX*UY*BETA_Y + 
     +           UX*UZ*BETA_Z
            BETA_PERP_Y=UX*UY*BETA_X - (UX*UX+UZ*UZ)*BETA_Y + 
     +           UY*UZ*BETA_Z
            BETA_PERP_Z=UX*UZ*BETA_X + UY*UZ*BETA_Y - 
     +           (UX*UX+UY*UY)*BETA_Z

c WARNING
c k x (k x beta) is actually -beta_perp and NOT beta_perp 
c which accounts for the minus sign needed in -q beta_perp for q>0
            BETA_PERP=sqrt(BETA_PERP_X*BETA_PERP_X+
     +           BETA_PERP_Y*BETA_PERP_Y + BETA_PERP_Z*BETA_PERP_Z)

c FACFRQ = 2*pi*frequency_MHz/(c*rho)
            DO NU=1,NUMAX
               PHI1=FACFRQ(NU)*CHERFAC_1
               DPHI=FACFRQ(NU)*(CT2-CT1)*DENOM
               PHASE1=PHI1*(0.D0,1.D0)
               DPHASE=DPHI*(0.D0,1.D0)
        
               EDPHASE=EXP(DPHASE)
               EPHASE1=EXP(PHASE1)
               
               IF (ABS(DPHI).LT. 1.D-8) THEN
                  ZSUM2_X(J,K,NU) = ZSUM2_X(J,K,NU) + 
     +                 CONST1*BETA_PERP_X*FACFRQ(NU)*
     +                 (CT2-CT1)*EPHASE1*(0.D0,1.D0)
                  
                  ZSUM2_Y(J,K,NU) = ZSUM2_Y(J,K,NU) + 
     +                 CONST1*BETA_PERP_Y*FACFRQ(NU)*(CT2-CT1)*
     +                 EPHASE1*(0.D0,1.D0)
                  
                  ZSUM2_Z(J,K,NU) = ZSUM2_Z(J,K,NU) + 
     +                 CONST1*BETA_PERP_Z*FACFRQ(NU)*(CT2-CT1)*
     +                 EPHASE1*(0.D0,1.D0)
                  
c Note ZSUM2 is wrong (especially at theta=0 deg.)
c due to modulus of BETA_PERP which is always positive
c so that cancellations of components are not correct
c ARADIUS and APHASE however should be ok 
                  ZSUM2(J,K,NU) = ZSUM2(J,K,NU) + 
     +                 CONST1*BETA_PERP*FACFRQ(NU)*(CT2-CT1)*
     +                 EPHASE1*(0.D0,1.D0)
                  
               ELSE

                  ZSUM2_X(J,K,NU) = ZSUM2_X(J,K,NU) + 
     +                 CONST1*BETA_PERP_X*EPHASE1*
     +                 (EDPHASE-(1.D0,0.D0))/DENOM
                  
                  ZSUM2_Y(J,K,NU) = ZSUM2_Y(J,K,NU) + 
     +                 CONST1*BETA_PERP_Y*EPHASE1*
     +                 (EDPHASE-(1.D0,0.D0))/DENOM
                  
                  ZSUM2_Z(J,K,NU) = ZSUM2_Z(J,K,NU) + 
     +                 CONST1*BETA_PERP_Z*EPHASE1*
     +                 (EDPHASE-(1.D0,0.D0))/DENOM

c Note ZSUM2 is wrong (especially at theta=0 deg.)
c due to modulus of BETA_PERP which is always positive
c so that cancellations of components are not correct
c ARADIUS and APHASE however should be ok 
                  ZSUM2(J,K,NU) = ZSUM2(J,K,NU) + 
     +                 CONST1*BETA_PERP*EPHASE1*
     +                 (EDPHASE-(1.D0,0.D0))/DENOM
               END IF

            END DO              ! End loop in frequency
         END DO                 ! End loop in theta
      END DO                    ! End loop in phi
c     End field tool's empsum block ++++++++++
c     
      return
      end
c     --- End of routine empsum
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine empsum_t(itypin,x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,
     +     Wgt0)
c     
c     ZHAireS empsum_t subroutine
c     calculates vector potential as a function of time for individual tracks 
c     and adds it to common asum_x, asum_y, asum_z arrays
c
c     arguments:
c     
c     ityp         --> integer input: particle type (-1=positron, 1=electron)
c     [x,y,z]1     --> double precision input: start track position (m)
c     [x,y,z]2     --> double precision input: end track position (m)
c     ct1          --> start track time*c (m)
c     ct2          --> end track time*c (m)
c     E1           --> start track energy (MeV)
c     E2           --> end track energy (MeV)
c     Wgt0         --> double precision input: particle weight
c
      use Dyn_Array
      implicit none
c     include field tool's commons
      include 'fieldcomm.f'
c     include constant commons (for pi)
      include 'constants.f'
c     
c     input variables
      integer ityp,itypin
      double precision x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,Wgt0
c     
c     local variables
      integer j,k,it,it1,it2,it_sta,it_end
      double precision deltat_1,deltat_2,deltat_sta,deltat_end
      double precision beta,beta_x,beta_y,beta_z
      double precision beta_perp,beta_perp_x,beta_perp_y,beta_perp_z
      double precision e_av,g,g2,r2_r1,dtt,f
      double precision const1,const2,aux1,auxb,denom,cherfac_1,cherfac_2
      double precision ux,uy,uz
      double precision e_m      !electron mass in MeV

c     Changes to include pi+-, p, p-bar and mu+- %%%%%%%%%%%%%%%%%%%%%
c     ityp: mu+=-3, mu-=3, pi+=-2, pi-=2, p=-4, p-bar=4
c     needs to set mass accordingly and change ityp back to sgn(ityp)
c
c     muons
      if (abs(itypin) .eq. 3)then
         e_m = 105.658367d0
         ityp=itypin/3  !change ityp back to +-1
c     pions
      else if (abs(itypin) .eq. 2)then
         e_m = 139.57018d0
         ityp=itypin/2  !change ityp back to +-1
c     protons/anti-protons
      else if (abs(itypin) .eq. 4)then
         e_m = protonmass*1.d3
         ityp=itypin/4  !change ityp back to +-1
c     e+-
      else
         e_m = electronmass*1.d3
         ityp=itypin
      endif
c     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c     Begin ZHAireS' empsum block ++++++++
c     
      if (Wgt0.le.1.d-10) return ! Do not compute EMPSUM when Wgt0 = 0 (protected for accuracy)
      

c     ######      
c     SIGN 
c     ######      
c     According to Feynman's formula a positive charge q>0
c     travelling along z>0 produces an electric field      
c     proportional to -q beta_perp or equivalently
c     the vector potential (computed in this routine) 
c     is proportional to q beta_perp.
c     For instance for observation of a single track
c     with q>0 at theta = 90 deg, the z-component
c     of the vector potential should be _ a positive
c     step followed by a negative one _| |_
      
c     Factor 2 comes from the sum of the 2 sign functions after 
c     the transformation to the time domain
c     Minus sign because ITYP=1 is an electron
c     ITYP=-1 is a positron 
      CONST1=-2.d0*Wgt0*DBLE(ITYP)
c     
c     Average beta in track
      E_AV=(E1+E2)/2.d0
      G=E_AV/e_M
      G2=G*G
      BETA=SQRT(1.d0-1.d0/G2)
c     BETA is parallel to the vector (r2-r1)
c     Compute unitary vector parallel to (r2-r1)
      R2_R1=SQRT((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
      IF (R2_R1.EQ.0.d0) THEN
         RETURN
      END IF 
      BETA_X=BETA*(X2-X1)/R2_R1
      BETA_Y=BETA*(Y2-Y1)/R2_R1
      BETA_Z=BETA*(Z2-Z1)/R2_R1
c     
c     
c     FACTOR_T = 1/c
      DO J=1,JMAX               !loop in theta
         do k=1,kmax            !loop in phi
            AUX1 = SINMU(J)*COSPHI(K)*X1+
     +           SINMU(J)*SINPHI(K)*Y1+COSMU(J)*Z1
            AUXB = SINMU(J)*COSPHI(K)*BETA_X+
     +           SINMU(J)*SINPHI(K)*BETA_Y+COSMU(J)*BETA_Z
            DENOM = 1.d0 - AUXB
            
            CHERFAC_1 = CT1 - AUX1 ! m
            DELTAT_1 = FACTOR_T*CHERFAC_1 ! s
            CHERFAC_2 = CT2 - AUXB*(CT2-CT1) - AUX1 !  m 
            DELTAT_2 = FACTOR_T*CHERFAC_2 ! s
            DTT = FACTOR_T*(CT2-CT1) ! s
c     
c     k x (k x beta) 
            UX = SINTET(J)*COSPHI(K)
            UY = SINTET(J)*SINPHI(K)
            UZ = COSTET(J)
            BETA_PERP_X=-(UY*UY+UZ*UZ)*BETA_X+UX*UY*BETA_Y+UX*UZ*BETA_Z
            BETA_PERP_Y=UX*UY*BETA_X-(UX*UX+UZ*UZ)*BETA_Y+UY*UZ*BETA_Z
            BETA_PERP_Z=UX*UZ*BETA_X+UY*UZ*BETA_Y-(UX*UX+UY*UY)*BETA_Z
            
            BETA_PERP=SQRT(BETA_PERP_X*BETA_PERP_X +
     +           BETA_PERP_Y*BETA_PERP_Y +
     +           BETA_PERP_Z*BETA_PERP_Z)
            
c     WARNING
c     k x (k x beta) is actually -beta_perp and NOT beta_perp => change signs
            BETA_PERP_X=-BETA_PERP_X
            BETA_PERP_Y=-BETA_PERP_Y
            BETA_PERP_Z=-BETA_PERP_Z
c     
c     Find time bins in which the contribution to vector potential is non-zero
            IF (DELTAT_1.LT.0.d0) THEN
               IT1=MAXTHALF + INT(DELTAT_1/DT_BIN_S)-1
            ELSE
               IT1=MAXTHALF + INT(DELTAT_1/DT_BIN_S)
            END IF
c     
            IF (DELTAT_2.LT.0.) THEN
               IT2=MAXTHALF + INT(DELTAT_2/DT_BIN_S)-1
            ELSE
               IT2=MAXTHALF + INT(DELTAT_2/DT_BIN_S)
            END IF
c     
            IF (IT1.LE.IT2) THEN 
               IT_STA=IT1
               IT_END=IT2
            ELSE
               IT_STA=IT2
               IT_END=IT1
            END IF   
c     
c     Get rid of times outside time range
            IF (IT_STA.GT.ntbins.OR.IT_END.LT.1) GOTO 200 ! Do not compute field
            IF (IT_STA.LT.1) IT_STA=1
            IF (IT_END.GT.ntbins) IT_END=ntbins
c     
            CONST2=CONST1
c     Change of sign when DELTAT_2 < DELTAT_1
            IF (DELTAT_2.LT.DELTAT_1) THEN
               CONST2=-CONST1
            END IF 
c     
c     Note ASUM_i is multiplied by e*mu_r/(8.*pi*epsilon_0*c)
c     when writing it to the file. One factor 1/c goes into beta 
            
c     Distribute vector potential among bins in time
c     and calculate average vector potential in each bin        
c     (see my notes: MC implementation)
c     Since we take the derivative at the end the important 
c     point is to conserve the structure from bin to bin.
            IF (DELTAT_2.LT.DELTAT_1) THEN 
               DELTAT_STA=DELTAT_2
               DELTAT_END=DELTAT_1
            ELSE
               DELTAT_STA=DELTAT_1
               DELTAT_END=DELTAT_2
            END IF
c     
            DO IT=IT_STA,IT_END,1
               IF(IT_STA.EQ.IT_END) THEN ! Subtrack contained in bin
c     
                  F=(DELTAT_END-DELTAT_STA)/DT_BIN_S
c     
                  IF (ABS(DENOM).GT.1.D-15) THEN
                     ASUM_X(IT,J,K) = ASUM_X(IT,J,K) + 
     +                    ABS(F)*CONST2*BETA_PERP_X/DENOM
                     ASUM_Y(IT,J,K) = ASUM_Y(IT,J,K) + 
     +                    ABS(F)*CONST2*BETA_PERP_Y/DENOM
                     ASUM_Z(IT,J,K) = ASUM_Z(IT,J,K) + 
     +                    ABS(F)*CONST2*BETA_PERP_Z/DENOM
                  ELSE          ! Cherenkov angle approximation
                     ASUM_X(IT,J,K) = ASUM_X(IT,J,K) + 
     +                    ABS(DTT)*CONST2*BETA_PERP_X
                     ASUM_Y(IT,J,K) = ASUM_Y(IT,J,K) + 
     +                    ABS(DTT)*CONST2*BETA_PERP_Y
                     ASUM_Z(IT,J,K) = ASUM_Z(IT,J,K) + 
     +                    ABS(DTT)*CONST2*BETA_PERP_Z
                  END IF
                  
               ELSE             ! Subtrack crossing one or more bins in time
c     Correct for bin edges               
                  F=((IT_STA+1-MAXTHALF)*DT_BIN_S-DELTAT_STA)/DT_BIN_S
c     
                  IF (IT.EQ.IT_STA) THEN ! Start of subtrack 
                     ASUM_X(IT,J,K) = ASUM_X(IT,J,K) + 
     +                    ABS(F)*CONST2*BETA_PERP_X/DENOM
                     ASUM_Y(IT,J,K) = ASUM_Y(IT,J,K) + 
     +                    ABS(F)*CONST2*BETA_PERP_Y/DENOM
                     ASUM_Z(IT,J,K) = ASUM_Z(IT,J,K) + 
     +                    ABS(F)*CONST2*BETA_PERP_Z/DENOM
c     
                  ELSE IF (IT.EQ.IT_END) THEN ! End of subtrack
                     F=((IT_END+1-MAXTHALF)*DT_BIN_S-DELTAT_END)/
     +                    DT_BIN_S
                     ASUM_X(IT,J,K) = ASUM_X(IT,J,K) + 
     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_X/DENOM
                     ASUM_Y(IT,J,K) = ASUM_Y(IT,J,K) + 
     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_Y/DENOM
                     ASUM_Z(IT,J,K) = ASUM_Z(IT,J,K) + 
     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_Z/DENOM
                  ELSE
                     
                     F=1.d0
                     ASUM_X(IT,J,K) = ASUM_X(IT,J,K) +
     +                    F*CONST2*BETA_PERP_X/DENOM
                     ASUM_Y(IT,J,K) = ASUM_Y(IT,J,K) +
     +                    F*CONST2*BETA_PERP_Y/DENOM
                     ASUM_Z(IT,J,K) = ASUM_Z(IT,J,K) +
     +                    F*CONST2*BETA_PERP_Z/DENOM
                  END IF   
c     
               END IF
c     
            END DO
c     
 200        CONTINUE
c     
         END DO
      END DO
      
c     End ZHAireS' empsum_t block ++++++++
c     
      return
      end
c     --- End of routine empsum_t
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine empsum_fr(itypin,x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,
     +     Wgt0)
c
c     FIELD TOOL empsum2_fresnel subroutine
c     calculates fields for individual tracks and adds it to common 
c     zsum_xyz_fr arrays
c
c     arguments:
c     
c     ityp         --> integer input: particle type (-1=positron, 1=electron)
c     [x,y,z]1     --> double precision input: start track position (m)
c     [x,y,z]2     --> double precision input: end track position (m)
c     ct1          --> start track time*c (m)
c     ct2          --> end track time*c (m)
c     E1           --> start track energy (MeV)
c     E2           --> end track energy (MeV)
c     Wgt0         --> double precision input: particle weight
c
      use Dyn_Array
      implicit none
      include 'initpar.f'
      include 'initcomm.f'
c     include field tool's commons
      include 'fieldcomm.f'
      include 'constants.f'
c
c     local variables
      integer ityp,itypin
      integer nu,na
      double precision x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,Wgt0
      double precision e_av,g,g2,beta,r2_r1,phi1,dphi,R
      double precision beta_x,beta_y,beta_z
      double precision const1,cherfac_1,denom,ux,uy,uz,auxb,ct0
      double precision beta_perp,beta_perp_x,beta_perp_y,beta_perp_z
      complex*16 phase1,phase2,dphase,ephase1,ephase2,edphase,dephase
      complex*16 phase0,ephase0
      double precision e_m      !electron mass in MeV
      double precision udx,udy,udz,udct,ude,udzp,ufactor,sin2tet,fraufac
      double precision stepx,stepy,stepz,stepct,stepe
      double precision sx1,sx2,sy1,sy2,sz1,sz2,se1,se2,sct1,sct2
      integer npieces, iu
c     variables for variable n calculation
      double precision avn,oldrefn,hd,h0,rh,nh,x0,y0,z0,zprime
      double precision zgcrit1,zgcrit2,dist

c     Changes to include pi+-, p, p-bar and mu+- %%%%%%%%%%%%%%%%%%%%%
c     ityp: mu+=-3, mu-=3, pi+=-2, pi-=2, p=-4, p-bar=4
c     needs to set mass accordingly and change ityp back to sgn(ityp)
c
c     muons
      if (abs(itypin) .eq. 3)then
         e_m = 105.658367d0
         ityp=itypin/3  !change ityp back to +-1
c     pions
      else if (abs(itypin) .eq. 2)then
         e_m = 139.57018d0
         ityp=itypin/2  !change ityp back to +-1
c     protons/anti-protons
      else if (abs(itypin) .eq. 4)then
         e_m = protonmass*1.d3
         ityp=itypin/4 !change ityp back to +-1
c     e+-
      else
         e_m = electronmass*1.d3
         ityp=itypin
      endif
c     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cc     Begin field tool's empsum block ++++++++
      if (Wgt0.le.1.d-10) return ! Do not compute EMPSUM when Wgt0 = 0 (protected for accuracy)
      

C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C     Track tests:
C     remove/fix underground tracks
C     Check visibility for general case (antennas on ground)
C     NOTE: AIRES doesn't care about fixing the path when the 
C           particle reaches the ground, only energy. So we may
C           have underground tracks reaching this point
C
c     calculate mid-point
      x0=(x1+x2)/2.d0
      y0=(y1+y2)/2.d0
      z0=(Z1+Z2)/2.d0
c     Do not compute emission if track starts underground 
      zgcrit1=injz-groundz+Rtg-sqrt(Rtg*Rtg-(x1*x1+y1*y1))
      if(z1 .gt. zgcrit1) then
         return
      endif
C
C
C     If track ends underground, "fix" it
      zgcrit2=injz-groundz+Rtg-sqrt(Rtg*Rtg-(x2*x2+y2*y2))
      if(z2 .gt. zgcrit2)then
c     calculate original deltas
         udx=x2-x1
         udy=y2-y1
         udz=z2-z1
         udct=ct2-ct1
         ude=e2-e1 
c     change z2 to ground and calculate factor   
         z2=zgcrit2
         udzp=z2-z1             !new udz'
         ufactor=udzp/udz
c     change other endpoint parameters
         x2=x1+ufactor*udx
         y2=y1+ufactor*udy
         ct2=ct1+ufactor*udct
         e2=e1+ufactor*ude
c     recalculate new mid-point
         x0=(x1+x2)/2.d0
         y0=(y1+y2)/2.d0
         z0=(Z1+Z2)/2.d0
      endif
C
C     Test track visibility (general case for antenna on ground)
      if( (z0 .gt. (injz-groundz) ) .and. (minparcrit .eq. 0.d0))then
c         print*,x0,y0,z0
         return
      endif
C     End track tests
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


c     Variable n: save original ref_n
      oldrefn=ref_n

c ######
c  SIGN
c ######
c According to Feynman's formula a positive charge q>0
c travelling along z>0 produces an electric field 
c proportional to -q beta_perp

c     Minus sign because ITYP=1 is an electron
c                        ITYP=-1 is a positron 
      CONST1=-Wgt0*DBLE(ITYP)

c     Average beta in track
      E_AV=(E1+E2)/2.d0
      G=E_AV/e_M
      G2=G*G
      BETA=SQRT(1.d0-1.d0/G2)


c     BETA is parallel to the vector (r2-r1)
c     Compute unitary vector parallel to (r2-r1)
      R2_R1=SQRT((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
      IF (R2_R1.EQ.0.d0) THEN
         RETURN
      END IF 
      BETA_X=BETA*(X2-X1)/R2_R1
      BETA_Y=BETA*(Y2-Y1)/R2_R1
      BETA_Z=BETA*(Z2-Z1)/R2_R1
c     middle point time
      ct0=(ct1+ct2)/2.d0
      
      DO NA=1,NAMAX             ! Loop in antenna positions


C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     Track visibility check (curved earth and antenna above ground)
         if(minparcrit .lt. 0.d0)then !If not general case (already checked)
            dist=sqrt(x0*x0+y0*y0)
            zprime=injz-groundz-z0; !z in a system with z origin at ground level
C     Check visibility for this antenna
            if(dist .gt. dcrit(na) .and. 
     +           zprime .lt. dist*parcrit(na)+h0ant(na))then
c              EMISSION POINT IS NOT VISIBLE!!!
c               print*,x0,y0,z0
               goto 2777        ! DO NOT CALCULATE FOR THIS ANTENNA!!
            endif
         endif 
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

         
c     Vector k from average point of track to observer
         UX = XANT(NA)-x0
         UY = YANT(NA)-y0 !m  
         UZ = ZANT(NA)-z0  
         
c     Variable n integral calculation ///////////////////
         if(usevarn)then 
            hd=(injz-zant(na))/1.d3 !detector altitude
            h0=(injz-(z1+z2)/2.d0)/1.d3
            if( (h0-hd) .gt. 1.d-10 )then
               avn=(ns/(kr*(hd-h0)))*(exp(kr*hd)-exp(kr*h0))
            else
               avn=ns*exp(kr*h0)
               print*,"Effective n: h0=hd"
            endif
            ref_n=1.d0+1.d-6*avn !average (effective) n
            rh=ns*exp(kr*h0)    !refractivity at emission
            nh=1.d0+1.d-6*rh    !n at emission
         else
            nh=ref_n
         endif
         
c     ///////////////////////////////////////////////////
c     
c     Distance from average point of track to observer
         R = SQRT(UX*UX+UY*UY+UZ*UZ) ! m
c     Unit vector k
         UX=UX/R
         UY=UY/R
         UZ=UZ/R
c         AUXB = REF_N*UX*BETA_X + REF_N*UY*BETA_Y + REF_N*UZ*BETA_Z !nB.k
c     new auxb using n(h) and not neff
         AUXB = nh*UX*BETA_X + nh*UY*BETA_Y + nh*UZ*BETA_Z !n(h)B.k
c         DENOM= 1.d0 - REF_N*UX*BETA_X - REF_N*UY*BETA_Y -
c     +        REF_N*UZ*BETA_Z
         DENOM= 1.d0 - AUXB !denom uses n(h)

c     test for fraunhoffer in track validity %%%%%%%%%%%%
c         sin2tet=1.d0-(auxb/ref_n/beta)*(auxb/ref_n/beta)
         sin2tet=1.d0-(auxb/nh/beta)*(auxb/nh/beta)
c         fraufac=2*pi*R2_R1*R2_R1*sin2tet/R/fraulambda
         fraufac=6.3*R2_R1*R2_R1*sin2tet/R/fraulambda
         if( (R .ne. 0.d0) .and. (fraufac .gt. 1.))then
c     Track doesn't pass validity test: splitting!
c     reset ref_n
            ref_n=oldrefn
c     calculate number of division
            npieces=sqrt(fraufac)+1
c     calculate original deltas
            udx=x2-x1
            stepx=udx/npieces
            udy=y2-y1
            stepy=udy/npieces
            udz=z2-z1
            stepz=udz/npieces
            udct=ct2-ct1
            stepct=udct/npieces
            ude=e2-e1
            stepe=ude/npieces
            
            sx1=x1
            sy1=y1
            sz1=z1
            sct1=ct1
            se1=e1
c     splitting track
            do iu=1,npieces
c     change other endpoint parameters
               sx2=sx1+stepx
               sy2=sy1+stepy
               sz2=sz1+stepz
               sct2=sct1+stepct
               se2=se1+stepe
c     send track segment for single antenna field calculation
               call empsum_fr_single(ityp,sx1,sy1,sz1,sct1,sx2,sy2,
     + sz2,sct2,se1,se2,Wgt0,e_m,na)
c     set endpoint as new starting point
               sx1=sx2
               sy1=sy2
               sz1=sz2
               sct1=sct2
               se1=se2
            enddo
            
         else !Track is OK, no splitting needed

c k x (k x beta) 
            BETA_PERP_X=-(UY*UY+UZ*UZ)*BETA_X + UX*UY*BETA_Y + 
     +           UX*UZ*BETA_Z
            BETA_PERP_Y=UX*UY*BETA_X - (UX*UX+UZ*UZ)*BETA_Y + 
     +           UY*UZ*BETA_Z
            BETA_PERP_Z=UX*UZ*BETA_X + UY*UZ*BETA_Y - 
     +           (UX*UX+UY*UY)*BETA_Z
         
c     WARNING
c     k x (k x beta) is actually -beta_perp and NOT beta_perp 
c     which accounts for the minus sign needed in -q beta_perp for q>0
            BETA_PERP=sqrt(BETA_PERP_X*BETA_PERP_X+
     +           BETA_PERP_Y*BETA_PERP_Y + BETA_PERP_Z*BETA_PERP_Z)

            DO NU=1,NUMAX

c     new freq fresnel
c     iw(t1+nR/c) different for each track in Fresnel
c     algoritmo com ponto médio
              PHASE1=FACFRQ(NU)*(CT1+REF_N*R-AUXB*(CT1-CT0))*(0.D0,1.D0)
c iw(t2+nR/c) different for each track in Fresnel
              PHASE2=FACFRQ(NU)*(CT2+REF_N*R-AUXB*(CT2-CT0))*(0.D0,1.D0)
c e^iw(t1+nR/c) different for each track in Fresnel
              EPHASE1=EXP(PHASE1)
c e^iw(t2+nR/c) different for each track in Fresnel
              EPHASE2=EXP(PHASE2)  
          
              DEPHASE=EPHASE2-EPHASE1

c     TEST DENOM*omega
              if(dabs(facfrq(nu)*denom*cspeed) .lt. 1.e-15)then !Cerenkov
                 print*,"Cerenkov freqfresnel!!!:",
     +                denom*facfrq(nu)*cspeed
c     calculate phase0 and ephase0 only if needed (cherenkov angle)
                 phase0=facfrq(nu)*REF_N*(R+AUXB*ct0)*(0.d0,1.d0)
                 ephase0=exp(phase0)

                 ZSUM2_X_FR(NA,NU) = ZSUM2_X_FR(NA,NU) + 
     +                CONST1*BETA_PERP_X*EPHASE0*facfrq(nu)*(ct2-ct1)*
     +                (0.d0,1.d0)/R
                 ZSUM2_Y_FR(NA,NU) = ZSUM2_Y_FR(NA,NU) + 
     +                CONST1*BETA_PERP_Y*EPHASE0*facfrq(nu)*(ct2-ct1)*
     +                (0.d0,1.d0)/R
                 ZSUM2_Z_FR(NA,NU) = ZSUM2_Z_FR(NA,NU) + 
     +                CONST1*BETA_PERP_Z*EPHASE0*facfrq(nu)*(ct2-ct1)*
     +                (0.d0,1.d0)/R
                 
              else

                 ZSUM2_X_FR(NA,NU) = ZSUM2_X_FR(NA,NU) + 
     +                CONST1*BETA_PERP_X*DEPHASE/denom/R
                 ZSUM2_Y_FR(NA,NU) = ZSUM2_Y_FR(NA,NU) + 
     +                CONST1*BETA_PERP_Y*DEPHASE/denom/R
                 ZSUM2_Z_FR(NA,NU) = ZSUM2_Z_FR(NA,NU) + 
     +                CONST1*BETA_PERP_Z*DEPHASE/denom/R

              endif

           END DO               ! End loop in frequency
         
        endif
c     %%%%%%%%
C     reset ref_n
        ref_n=oldrefn
 2777   continue
      END DO                    ! End loop in antenna positions
c     End field tool's empsum block ++++++++++
c     
      ref_n=oldrefn
      return
      end
c     --- End of routine empsum_fr
c     
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine empsum_fr_single(ityp,x1,y1,z1,ct1,x2,y2,z2,ct2,
     +     e1,e2,Wgt0,e_m,antn)
c
c     FIELD TOOL empsum2_fresnel subroutine
c     calculates fields for a SINGLE ANTENNA (track division)
c
c     arguments:
c     
c     ityp         --> integer input: particle type (-1=positron, 1=electron)
c     [x,y,z]1     --> double precision input: start track position (m)
c     [x,y,z]2     --> double precision input: end track position (m)
c     ct1          --> start track time*c (m)
c     ct2          --> end track time*c (m)
c     E1           --> start track energy (MeV)
c     E2           --> end track energy (MeV)
c     Wgt0         --> double precision input: particle weight
c     antn         --> Number of antenna to make the calculation
c     e_m          --> Mass of particle 
c
      use Dyn_Array
      implicit none
      include 'initpar.f'
      include 'initcomm.f'
c     include field tool's commons
      include 'fieldcomm.f'
      include 'constants.f'
c
c     local variables
      integer ityp,antn
      integer nu,na
      double precision x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,Wgt0
      double precision e_av,g,g2,beta,r2_r1,phi1,dphi,R
      double precision beta_x,beta_y,beta_z
      double precision const1,cherfac_1,denom,ux,uy,uz,auxb,ct0
      double precision beta_perp,beta_perp_x,beta_perp_y,beta_perp_z
      complex*16 phase1,phase2,dphase,ephase1,ephase2,edphase,dephase
      complex*16 phase0,ephase0
      double precision e_m      !electron mass in MeV
c     variables for variable n calculation
      double precision avn,oldrefn,hd,h0,rh,nh,x0,y0,z0,zprime,dist
c     save original ref_n
      oldrefn=ref_n
c ######
c  SIGN
c ######
c According to Feynman's formula a positive charge q>0
c travelling along z>0 produces an electric field 
c proportional to -q beta_perp

C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C     check general visibility (needed because Z0 changes when splitting) 
      x0=(x1+x2)/2.d0
      y0=(y1+y2)/2.d0
      z0=(Z1+Z2)/2.d0

C     Check track visibility (general case for antennas on ground)
      if( (z0 .gt. (injz-groundz) ) .and. (minparcrit .eq. 0.d0))then
c         print*,x0,y0,z0
         return
      endif
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      x0=(x1+x2)/2.d0
      y0=(y1+y2)/2.d0
      z0=(Z1+Z2)/2.d0

c Minus sign because ITYP=1 is an electron
c                    ITYP=-1 is a positron 
      CONST1=-Wgt0*DBLE(ITYP)

c Average beta in track
      E_AV=(E1+E2)/2.d0
      G=E_AV/e_M
      G2=G*G
      BETA=SQRT(1.d0-1.d0/G2)


c BETA is parallel to the vector (r2-r1)
c Compute unitary vector parallel to (r2-r1)
      R2_R1=SQRT((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
      IF (R2_R1.EQ.0.d0) THEN
         RETURN
      END IF 
      BETA_X=BETA*(X2-X1)/R2_R1
      BETA_Y=BETA*(Y2-Y1)/R2_R1
      BETA_Z=BETA*(Z2-Z1)/R2_R1

      ct0=(ct1+ct2)/2.d0
      
      NA=antn

C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     Track visibility check (curved earth and antenna above ground)
         if(minparcrit .lt. 0.d0)then !If not general case (already checked)
            dist=sqrt(x0*x0+y0*y0)
            zprime=injz-groundz-z0; !z in a system with z origin at ground level
C     Check visibility for this antenna
            if(dist .gt. dcrit(na) .and. 
     +           zprime .lt. dist*parcrit(na)+h0ant(na))then
c              EMISSION POINT IS NOT VISIBLE!!!
c               print*,x0,y0,z0
               return        ! DO NOT CALCULATE FOR THIS ANTENNA!!
            endif
         endif 
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&      

      
c Vector k from average point of track to observer
      UX = XANT(NA)-x0
      UY = YANT(NA)-y0          !m  
      UZ = ZANT(NA)-z0 
      

c     Variable n integral calculation ///////////////////
         if(usevarn)then 
            hd=(injz-zant(na))/1.d3 !detector altitude
            h0=(injz-(z1+z2)/2.d0)/1.d3
            if((h0-hd) .gt. 1.d-10)then
               avn=(ns/(kr*(hd-h0)))*(exp(kr*hd)-exp(kr*h0))
            else
               avn=ns*exp(kr*h0)
               print*,"Effective n: h0=hd"
            endif
            ref_n=1.d0+1.d-6*avn !average (effective) n
            rh=ns*exp(kr*h0) !refractivity at emission
            nh=1.d0+1.d-6*rh !n at emission
         else 
            nh=ref_n
         endif

c     //////////////////////////////////////////////////

c     Distance from average point of track to observer
      R = SQRT(UX*UX+UY*UY+UZ*UZ) ! m
c     Unit vector k
      UX=UX/R
      UY=UY/R
      UZ=UZ/R

c      AUXB = REF_N*UX*BETA_X + REF_N*UY*BETA_Y + REF_N*UZ*BETA_Z !nB.k
c     new auxb using n(h) and not neff
         AUXB = nh*UX*BETA_X + nh*UY*BETA_Y + nh*UZ*BETA_Z !n(h)B.k
c         DENOM= 1.d0 - REF_N*UX*BETA_X - REF_N*UY*BETA_Y -
c     +        REF_N*UZ*BETA_Z
         DENOM= 1.d0 - AUXB !denom uses n(h)

c     k x (k x beta) 
      BETA_PERP_X=-(UY*UY+UZ*UZ)*BETA_X + UX*UY*BETA_Y + 
     +     UX*UZ*BETA_Z
      BETA_PERP_Y=UX*UY*BETA_X - (UX*UX+UZ*UZ)*BETA_Y + 
     +     UY*UZ*BETA_Z
      BETA_PERP_Z=UX*UZ*BETA_X + UY*UZ*BETA_Y - 
     +     (UX*UX+UY*UY)*BETA_Z
         
c     WARNING
c     k x (k x beta) is actually -beta_perp and NOT beta_perp 
c     which accounts for the minus sign needed in -q beta_perp for q>0
      BETA_PERP=sqrt(BETA_PERP_X*BETA_PERP_X+
     +     BETA_PERP_Y*BETA_PERP_Y + BETA_PERP_Z*BETA_PERP_Z)
      DO NU=1,NUMAX
         
c     new freq fresnel
c     iw(t1+nR/c) different for each track in Fresnel
c     algoritmo com ponto médio
         PHASE1=FACFRQ(NU)*(CT1+REF_N*R-AUXB*(CT1-CT0))*(0.D0,1.D0) 
c     iw(t2+nR/c) different for each track in Fresnel
         PHASE2=FACFRQ(NU)*(CT2+REF_N*R-AUXB*(CT2-CT0))*(0.D0,1.D0)
c     e^iw(t1+nR/c) different for each track in Fresnel
         EPHASE1=EXP(PHASE1)
c     e^iw(t2+nR/c) different for each track in Fresnel
         EPHASE2=EXP(PHASE2)  
         
         DEPHASE=EPHASE2-EPHASE1
         
c     TEST DENOM*omega
         if(dabs(facfrq(nu)*denom*cspeed) .lt. 1.e-15)then !Cerenkov
            print*,"Cerenkov freqfresnel!!!:",
     +           denom*facfrq(nu)*cspeed
c     calculate phase0 and ephase0 only if needed (cherenkov angle)
            phase0=facfrq(nu)*REF_N*(R+AUXB*ct0)*(0.d0,1.d0)
            ephase0=exp(phase0)
            
            ZSUM2_X_FR(NA,NU) = ZSUM2_X_FR(NA,NU) + 
     +           CONST1*BETA_PERP_X*EPHASE0*facfrq(nu)*(ct2-ct1)*
     +           (0.d0,1.d0)/R
            ZSUM2_Y_FR(NA,NU) = ZSUM2_Y_FR(NA,NU) + 
     +           CONST1*BETA_PERP_Y*EPHASE0*facfrq(nu)*(ct2-ct1)*
     +           (0.d0,1.d0)/R
            ZSUM2_Z_FR(NA,NU) = ZSUM2_Z_FR(NA,NU) + 
     +           CONST1*BETA_PERP_Z*EPHASE0*facfrq(nu)*(ct2-ct1)*
     +           (0.d0,1.d0)/R
            
         else
            
            ZSUM2_X_FR(NA,NU) = ZSUM2_X_FR(NA,NU) + 
     +           CONST1*BETA_PERP_X*DEPHASE/denom/R
            ZSUM2_Y_FR(NA,NU) = ZSUM2_Y_FR(NA,NU) + 
     +           CONST1*BETA_PERP_Y*DEPHASE/denom/R
            ZSUM2_Z_FR(NA,NU) = ZSUM2_Z_FR(NA,NU) + 
     +           CONST1*BETA_PERP_Z*DEPHASE/denom/R
            
         endif
      END DO                    ! End loop in frequency
      
      
c     Reset ref_n
      ref_n=oldrefn
c     End field tool's empsum block ++++++++++
      return
      end
c     --- End of routine empsum_fr
c     
c     <---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c     
      subroutine empsum_t_fr(itypin,x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,
     +     Wgt0)
c     
c     ZHAireS empsum_t Fresnel subroutine
c     calculates vector potential as a function of time for individual tracks 
c     and adds it to common asum_x_fr, asum_y_fr, asum_z_fr arrays
c
c     arguments:
c     
c     ityp         --> integer input: particle type (-1=positron, 1=electron)
c     [x,y,z]1     --> double precision input: start track position (m)
c     [x,y,z]2     --> double precision input: end track position (m)
c     ct1          --> start track time*c (m)
c     ct2          --> end track time*c (m)
c     E1           --> start track energy (MeV)
c     E2           --> end track energy (MeV)
c     Wgt0         --> double precision input: particle weight
c
      use Dyn_Array
      implicit none

c     include field tool's commons
      include 'fieldcomm.f'
c     include constant commons (for pi)
      include 'constants.f'
c     
c     TEST: injz,injdepth,groundalt,etc...
      include 'initpar.f'
      include 'initcomm.f'
      

c     input variables
      integer ityp,itypin
      double precision x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,Wgt0
c     
c     local variables
      integer na,it,it1,it2,it_sta,it_end
      double precision deltat_1,deltat_2,deltat_sta,deltat_end,R
      double precision beta,beta_x,beta_y,beta_z,zenithdeg,coszenith
      double precision beta_perp,beta_perp_x,beta_perp_y,beta_perp_z
      double precision e_av,g,g2,r2_r1,dtt,f
      double precision const1,const2,aux1,auxb,denom,cherfac_1,cherfac_2
      double precision ux,uy,uz,ct0
      double precision e_m      !electron mass in MeV

c     underground correction variables
      double precision udx,udy,udz,udct,ude,udzp,ufactor,sin2tet,fraufac
      double precision stepx,stepy,stepz,stepct,stepe
      double precision sx1,sx2,sy1,sy2,sz1,sz2,se1,se2,sct1,sct2
      integer npieces, iu
c     variables for variable n calculation
      double precision avn,oldrefn,hd,h0,rh,nh,x0,y0,z0,zprime
      double precision zgcrit1,zgcrit2,zgcrit0,dist

c     TEST-histogram filling speedup
      double precision f1,f2,f3,f4,f5
      double precision value1x,value1y,value1z
      double precision value2x,value2y,value2z
      double precision incr1x,incr1y,incr1z
      double precision incr2x,incr2y,incr2z
      double precision incr3x,incr3y,incr3z
      double precision incr4x,incr4y,incr4z
      double precision incr5x,incr5y,incr5z
      

      zenithdeg=pryzenithmin
      coszenith=cos(zenithdeg*dacos(-1.d0)/180.d0)

c     Changes to include pi+-, p, p-bar and mu+- %%%%%%%%%%%%%%%%%%%%%
c     ityp: mu+=-3, mu-=3, pi+=-2, pi-=2, p=-4, p-bar=4
c     needs to set mass accordingly and change ityp back to sgn(ityp)
c
c     muons
      if (abs(itypin) .eq. 3)then
         e_m = 105.658367d0
         ityp=itypin/3  !change ityp back to +-1
c     pions
      else if (abs(itypin) .eq. 2)then
         e_m = 139.57018d0
         ityp=itypin/2  !change ityp back to +-1
c     protons/anti-protons
      else if (abs(itypin) .eq. 4)then
         e_m = protonmass*1.d3
         ityp=itypin/4 !change ityp back to +-1
c     e+-
      else
         e_m = electronmass*1.d3
         ityp=itypin
      endif
c     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c     Begin ZHAireS' empsum block ++++++++
c     
      if (Wgt0.le.1.d-10) return ! Do not compute EMPSUM when Wgt0 = 0 (protected for accuracy)
     
c$$$c     TEST: PER GAMMA //////////////////
c$$$      E_AV=(E1+E2)/2.d0
c$$$      G=E_AV/e_M      
c$$$c     do not compute field for gammas outside range
c$$$      if ((G .le. 1000.) .or. (G .gt. 1000.))return
c$$$c     //////////////////////////////////




c     save original ref_n
      oldrefn=ref_n

c     ######      
c     SIGN 
c     ######      
c     According to Feynman's formula a positive charge q>0
c     travelling along z>0 produces an electric field      
c     proportional to -q beta_perp or equivalently
c     the vector potential (computed in this routine) 
c     is proportional to q beta_perp.
c     For instance for observation of a single track
c     with q>0 at theta = 90 deg, the z-component
c     of the vector potential should be _ a positive
c     step followed by a negative one _| |_
      
c     Factor 2 comes from the sum of the 2 sign functions after 
c     the transformation to the time domain
c     Minus sign because ITYP=1 is an electron
c     ITYP=-1 is a positron 
      CONST1=-2.d0*Wgt0*DBLE(ITYP)
      

C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C     Track tests:
C     remove/fix underground tracks
C     Check visibility for general case (antennas on ground)
C     NOTE: AIRES doesn't care about fixing the path when the 
C           particle reaches the ground, only energy. So we may
C           have underground tracks reaching this point
C
c     calculate mid-point
      x0=(x1+x2)/2.d0
      y0=(y1+y2)/2.d0
      z0=(Z1+Z2)/2.d0
c     Do not compute emission if track starts underground 
      zgcrit1=injz-groundz+Rtg-sqrt(Rtg*Rtg-(x1*x1+y1*y1))
      if(z1 .gt. zgcrit1) then
         return
      endif
C
C
C     If track ends underground, "fix" it
      zgcrit2=injz-groundz+Rtg-sqrt(Rtg*Rtg-(x2*x2+y2*y2))
      if(z2 .gt. zgcrit2)then
c     calculate original deltas
         udx=x2-x1
         udy=y2-y1
         udz=z2-z1
         udct=ct2-ct1
         ude=e2-e1 
c     change z2 to ground and calculate factor   
         z2=zgcrit2
         udzp=z2-z1             !new udz'
         ufactor=udzp/udz
c     change other endpoint parameters
         x2=x1+ufactor*udx
         y2=y1+ufactor*udy
         ct2=ct1+ufactor*udct
         e2=e1+ufactor*ude
c     recalculate new mid-point
         x0=(x1+x2)/2.d0
         y0=(y1+y2)/2.d0
         z0=(Z1+Z2)/2.d0
      endif
C
C     Test track visibility (general case for antenna on ground)
      if( (z0 .gt. (injz-groundz) ) .and. (minparcrit .eq. 0.d0))then
c         print*,x0,y0,z0
         return
      endif
C     End track tests
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

c     
c     Average beta in track
      E_AV=(E1+E2)/2.d0
      G=E_AV/e_M
      G2=G*G
      BETA=SQRT(1.d0-1.d0/G2)
c     BETA is parallel to the vector (r2-r1)
c     Compute unitary vector parallel to (r2-r1)
      R2_R1=SQRT((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))

      IF (R2_R1.EQ.0.d0) THEN
         RETURN
      END IF 
      BETA_X=BETA*(X2-X1)/R2_R1
      BETA_Y=BETA*(Y2-Y1)/R2_R1
      BETA_Z=BETA*(Z2-Z1)/R2_R1
c     mid point time
      ct0=(ct1+ct2)/2.d0
c     
      DO NA=1,NAMAX             ! Loop in antenna positions

C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     Track visibility check (curved earth and antenna above ground)
         if(minparcrit .lt. 0.d0)then !If not general case (already checked)
            dist=sqrt(x0*x0+y0*y0)
            zprime=injz-groundz-z0; !z in a system with z origin at ground level
C     Check visibility for this antenna
            if(dist .gt. dcrit(na) .and. 
     +           zprime .lt. dist*parcrit(na)+h0ant(na))then
c              EMISSION POINT IS NOT VISIBLE!!!
c               print*,x0,y0,z0
               goto 2888        ! DO NOT CALCULATE FOR THIS ANTENNA!!
            endif
         endif 
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

c Vector k from average point of track to observer
         UX = XANT(NA)-(X1+X2)/2.d0
         UY = YANT(NA)-(Y1+Y2)/2.d0 !m  
         UZ = ZANT(NA)-(Z1+Z2)/2.d0  

c     Variable n integral calculation ///////////////////
         if(usevarn)then 
            hd=(injz-zant(na))/1.d3 !detector altitude
            h0=(injz-(z1+z2)/2.d0)/1.d3
            if(dabs(hd-h0) .gt. 1.d-10)then
               avn=(ns/(kr*(hd-h0)))*(exp(kr*hd)-exp(kr*h0))
            else
               avn=ns*exp(kr*h0)
               print*,"Effective n: h0=hd"
            endif
            ref_n=1.d0+1.d-6*avn !average (effective) n
            rh=ns*exp(kr*h0) !refractivity at emission
            nh=1.d0+1.d-6*rh !n at emission
         else 
            nh=ref_n
         endif
c     ///////////////////////////////////////////////////


c     Distance from average point of track to observer
         R = SQRT(UX*UX+UY*UY+UZ*UZ) ! m
         
c     Unit vector k
         UX=UX/R
         UY=UY/R
         UZ=UZ/R
c         AUXB = REF_N*UX*BETA_X + REF_N*UY*BETA_Y + REF_N*UZ*BETA_Z
c     new auxb using n(h) instead of neff (ref_n)
         AUXB = nh*UX*BETA_X + nh*UY*BETA_Y + nh*UZ*BETA_Z
         DENOM = 1.d0 - AUXB
         

c     test for fraunhoffer in track validity %%%%%%%%%%%%
         sin2tet=1.d0-(auxb/nh/beta)*(auxb/nh/beta)
c         fraufac=2*pi*R2_R1*R2_R1*sin2tet/R/fraulambda
c         fraufac=6.3*R2_R1*R2_R1*sin2tet/R/fraulambda
         fraufac=R2_R1*R2_R1*14.137/R/fraulambda
c         print *, fraulambda
c         print *, fraufac
         !print *, fraufac
         !print *, "If larger than 1, splitting track"
         if( (R .ne. 0.d0) .and. (fraufac .gt. 1.))then
c     Track doesn't pass validity test: splitting!
c     reset ref_n
            ref_n=oldrefn
c     calculate number of division
            npieces=sqrt(fraufac)+1
c     calculate original deltas
            udx=x2-x1
            stepx=udx/npieces
            udy=y2-y1
            stepy=udy/npieces
            udz=z2-z1
            stepz=udz/npieces
            udct=ct2-ct1
            stepct=udct/npieces
            ude=e2-e1
            stepe=ude/npieces

            sx1=x1
            sy1=y1
            sz1=z1
            sct1=ct1
            se1=e1
c     splitting track
            do iu=1,npieces
c     change other endpoint parameters
               sx2=sx1+stepx
               sy2=sy1+stepy
               sz2=sz1+stepz
               sct2=sct1+stepct
               se2 = e2
c               se2=se1+stepe
               
c               average_energy = (e2+e1)/2
c               se2 = e1+(e2-e1)/npieces
c               average_energy = (e1+e1+(e2-e1)/npieces)/2
c               = e1+deltae/2

c     send track segment for single antenna field calculation
               call empsum_t_fr_single(ityp,sx1,sy1,sz1,sct1,sx2,sy2,
     +              sz2,sct2,se1,se2,Wgt0,e_m,na)
c     set endpoint as new starting point
               sx1=sx2
               sy1=sy2
               sz1=sz2
               sct1=sct2
c               se1=se2
            enddo

   
         else  !Track is OK, no splitting needed

            DELTAT_1 = FACTOR_T*(CT1 + REF_N*R  - AUXB*(CT1-CT0)) -
     + dt(na)
            DELTAT_2 = FACTOR_T*(CT2 + REF_N*R  - AUXB*(CT2-CT0)) -
     + dt(na)
            DTT = FACTOR_T*(CT2-CT1)
c     
c     k x (k x beta) 
c     beta perp in relation to U
            BETA_PERP_X=-(UY*UY+UZ*UZ)*BETA_X+UX*UY*BETA_Y+UX*UZ*BETA_Z
            BETA_PERP_Y=UX*UY*BETA_X-(UX*UX+UZ*UZ)*BETA_Y+UY*UZ*BETA_Z
            BETA_PERP_Z=UX*UZ*BETA_X+UY*UZ*BETA_Y-(UX*UX+UY*UY)*BETA_Z
   
            BETA_PERP=SQRT(BETA_PERP_X*BETA_PERP_X +
     +           BETA_PERP_Y*BETA_PERP_Y +
     +           BETA_PERP_Z*BETA_PERP_Z)
            
c     WARNING
c     k x (k x beta) is actually -beta_perp and NOT beta_perp => change signs
            BETA_PERP_X=-BETA_PERP_X
            BETA_PERP_Y=-BETA_PERP_Y
            BETA_PERP_Z=-BETA_PERP_Z
c     
c     Find time bins in which the contribution to vector potential is non-zero
            IF (DELTAT_1.LT.0.d0) THEN
               IT1=MAXTHALF + INT(DELTAT_1/DT_BIN_S)-1
            ELSE
               IT1=MAXTHALF + INT(DELTAT_1/DT_BIN_S)
            END IF
c     
            IF (DELTAT_2.LT.0.) THEN
               IT2=MAXTHALF + INT(DELTAT_2/DT_BIN_S)-1
            ELSE
               IT2=MAXTHALF + INT(DELTAT_2/DT_BIN_S)
            END IF
c     
            IF (IT1.LE.IT2) THEN 
               IT_STA=IT1
               IT_END=IT2
            ELSE
               IT_STA=IT2
               IT_END=IT1
            END IF   
c     
c     Get rid of times outside time range
            IF (IT_STA.GT.ntbins.OR.IT_END.LT.1) GOTO 200 ! Do not compute field
            IF (IT_STA.LT.1) IT_STA=1
            IF (IT_END.GT.ntbins) IT_END=ntbins
c     
            CONST2=CONST1
c     Change of sign when DELTAT_2 < DELTAT_1
            IF (DELTAT_2.LT.DELTAT_1) THEN
               CONST2=-CONST1
            END IF 
c     
c     Note ASUM_i is multiplied by e*mu_r/(8.*pi*epsilon_0*c)
c     when writing it to the file. One factor 1/c goes into beta 
            
c     Distribute vector potential among bins in time
c     and calculate average vector potential in each bin        
c     (see my notes: MC implementation)
c     Since we take the derivative at the end the important 
c     point is to conserve the structure from bin to bin.
            IF (DELTAT_2.LT.DELTAT_1) THEN 
               DELTAT_STA=DELTAT_2
               DELTAT_END=DELTAT_1
            ELSE
               DELTAT_STA=DELTAT_1
               DELTAT_END=DELTAT_2
            END IF
c     
C     TEST-histogram filling speedup
            value1x=CONST2*BETA_PERP_X/DENOM/R
            value1y=CONST2*BETA_PERP_Y/DENOM/R
            value1z=CONST2*BETA_PERP_Z/DENOM/R

            value2x=CONST2*BETA_PERP_X/R
            value2y=CONST2*BETA_PERP_Y/R
            value2z=CONST2*BETA_PERP_Z/R

            f1=ABS((DELTAT_END-DELTAT_STA)/DT_BIN_S)
            f2=ABS(DTT/DT_BIN_S)
            F3=ABS(((IT_STA+1-MAXTHALF)*DT_BIN_S-DELTAT_STA)/DT_BIN_S)
            F4=1.d0-
     +        ABS( ((IT_END+1-MAXTHALF)*DT_BIN_S-DELTAT_END)/DT_BIN_S)

            incr1x=f1*value1x
            incr1y=f1*value1y
            incr1z=f1*value1z
            
            incr2x=f2*value2x
            incr2y=f2*value2y
            incr2z=f2*value2z

            incr3x=f3*value1x
            incr3y=f3*value1y
            incr3z=f3*value1z

            incr4x=f4*value1x
            incr4y=f4*value1y
            incr4z=f4*value1z
            
            incr5x=value1x
            incr5y=value1y
            incr5z=value1z
            

            DO IT=IT_STA,IT_END,1
               IF(IT_STA.EQ.IT_END) THEN ! Subtrack contained in bin
c     
c                  F=(DELTAT_END-DELTAT_STA)/DT_BIN_S
c     
c$$$                  IF (ABS(DENOM).GT.1.D-13) THEN
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Z/DENOM/R

                  IF (ABS(DENOM).GT.1.D-13) THEN
                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr1x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr1y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr1z

                  ELSE          ! Cherenkov angle approximation
c                     F=DTT/DT_BIN_S
                     print*,"CERENKOV!!!!!!!!!!!!!!"
                   
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_X/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Y/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Z/R
                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr2x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr2y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr2z
                     
                  END IF
                  
               ELSE             ! Subtrack crossing one or more bins in time
c     Correct for bin edges               
c                  F=((IT_STA+1-MAXTHALF)*DT_BIN_S-DELTAT_STA)/DT_BIN_S
                  
                  IF (IT.EQ.IT_STA) THEN ! Start of subtrack 
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Z/DENOM/R

                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr3x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr3y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr3z
c     
                  ELSE IF (IT.EQ.IT_END) THEN ! End of subtrack
c                     F=((IT_END+1-MAXTHALF)*DT_BIN_S-DELTAT_END)/
c     +                    DT_BIN_S

c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_Z/DENOM/R

                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr4x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr4y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr4z
                  ELSE
                     
c                     F=1.d0
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) +
c$$$     +                    F*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) +
c$$$     +                    F*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) +
c$$$     +                    F*CONST2*BETA_PERP_Z/DENOM/R
                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr5x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr5y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr5z

                  END IF   
c     
               END IF
c     
            END DO              ! End loop in time
            
            
            
            
            
            
         endif
c     %%%%%%%%
c     
 200     CONTINUE
c     reset ref_n
         ref_n=oldrefn    
c     
 2888    continue
      END DO                    ! End loop in antenna positions
c     End ZHAireS' empsum_t block ++++++++
c     
      return
      end
c     --- End of routine empsum_t_fr
c     

c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c     
      subroutine empsum_t_fr_single(ityp,x1,y1,z1,ct1,x2,y2,z2,ct2,
     +     e1,e2,Wgt0,e_m,antn)
c     
c     ZHAireS empsum_t Fresnel subroutine
c     calculates vector potential as a function of time for individual tracks 
c     and adds it to common asum_x_fr, asum_y_fr, asum_z_fr arrays
c
c     arguments:
c     
c     ityp         --> integer input: particle type (-1=positron, 1=electron)
c     [x,y,z]1     --> double precision input: start track position (m)
c     [x,y,z]2     --> double precision input: end track position (m)
c     ct1          --> start track time*c (m)
c     ct2          --> end track time*c (m)
c     E1           --> start track energy (MeV)
c     E2           --> end track energy (MeV)
c     Wgt0         --> double precision input: particle weight
c     antn         --> Number of antenna to make the calculation
c     e_m          --> Mass of particle 
c
      use Dyn_Array
      implicit none

c     include field tool's commons
      include 'fieldcomm.f'
c     include constant commons (for pi)
      include 'constants.f'
c     
c     TEST: injz,injdepth,groundalt,etc...
      include 'initpar.f'
      include 'initcomm.f'
      

c     input variables
      integer ityp
      double precision x1,y1,z1,ct1,x2,y2,z2,ct2,e1,e2,Wgt0
c     
c     local variables
      integer na,it,it1,it2,it_sta,it_end
      double precision deltat_1,deltat_2,deltat_sta,deltat_end,R
      double precision beta,beta_x,beta_y,beta_z,zenithdeg,coszenith
      double precision beta_perp,beta_perp_x,beta_perp_y,beta_perp_z
      double precision e_av,g,g2,r2_r1,dtt,f
      double precision const1,const2,aux1,auxb,denom,cherfac_1,cherfac_2
      double precision ux,uy,uz,ct0
      double precision e_m      !electron mass in MeV

c     underground correction variables
      double precision udx,udy,udz,udct,ude,udzp,ufactor,sin2tet,fraufac
      double precision stepx,stepy,stepz,stepct,stepe
      integer antn
c     variables for variable n calculation
      double precision avn,oldrefn,hd,h0,rh,nh,x0,y0,z0,zprime,dist

c     TEST-histogram filling speedup
      double precision f1,f2,f3,f4,f5
      double precision value1x,value1y,value1z
      double precision value2x,value2y,value2z
      double precision incr1x,incr1y,incr1z
      double precision incr2x,incr2y,incr2z
      double precision incr3x,incr3y,incr3z
      double precision incr4x,incr4y,incr4z
      double precision incr5x,incr5y,incr5z

      zenithdeg=pryzenithmin
      coszenith=cos(zenithdeg*dacos(-1.d0)/180.d0)


c     Begin ZHAireS' empsum block ++++++++
c     
      if (Wgt0.le.1.d-10) return ! Do not compute EMPSUM when Wgt0 = 0 (protected for accuracy)
      
c     save original ref_n
      oldrefn=ref_n

C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C     check general visibility (needed because Z0 changes when splitting) 
      x0=(x1+x2)/2.d0
      y0=(y1+y2)/2.d0
      z0=(Z1+Z2)/2.d0
      
c      print *, x1
c      print *, y1
c      print *, z1
      
c      print *, x2
c      print *, y2
c      print *, z2
      
c      print *, "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy"

C     Check track visibility (general case for antennas on ground)
      if( (z0 .gt. (injz-groundz) ) .and. (minparcrit .eq. 0.d0))then
c         print*,x0,y0,z0
         return
      endif
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


c     ######      
c     SIGN 
c     ######      
c     According to Feynman's formula a positive charge q>0
c     travelling along z>0 produces an electric field      
c     proportional to -q beta_perp or equivalently
c     the vector potential (computed in this routine) 
c     is proportional to q beta_perp.
c     For instance for observation of a single track
c     with q>0 at theta = 90 deg, the z-component
c     of the vector potential should be _ a positive
c     step followed by a negative one _| |_
      
c     Factor 2 comes from the sum of the 2 sign functions after 
c     the transformation to the time domain
c     Minus sign because ITYP=1 is an electron
c     ITYP=-1 is a positron 
      CONST1=-2.d0*Wgt0*DBLE(ITYP)

c     
c     Average beta in track
      E_AV=(E1+E2)/2.d0
      G=E_AV/e_M
      G2=G*G
      BETA=SQRT(1.d0-1.d0/G2)
c     BETA is parallel to the vector (r2-r1)
c     Compute unitary vector parallel to (r2-r1)
      R2_R1=SQRT((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))

      BETA_X=BETA*(X2-X1)/R2_R1
      BETA_Y=BETA*(Y2-Y1)/R2_R1
      BETA_Z=BETA*(Z2-Z1)/R2_R1

c     mid point time
      ct0=(ct1+ct2)/2.d0

c     set antenna number (from input)
      NA=antn

C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     Track visibility check (curved earth and antenna above ground)
         if(minparcrit .lt. 0.d0)then !If not general case (already checked)
            dist=sqrt(x0*x0+y0*y0)
            zprime=injz-groundz-z0; !z in a system with z origin at ground level
C     Check visibility for this antenna
            if(dist .gt. dcrit(na) .and. 
     +           zprime .lt. dist*parcrit(na)+h0ant(na))then
c              EMISSION POINT IS NOT VISIBLE!!!
c               print*,x0,y0,z0
               return        ! DO NOT CALCULATE FOR THIS ANTENNA!!
            endif
         endif 
C     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

c Vector k from average point of track to observer
      UX = XANT(NA)-(X1+X2)/2.d0
      UY = YANT(NA)-(Y1+Y2)/2.d0 !m  
      UZ = ZANT(NA)-(Z1+Z2)/2.d0  

c     Variable n integral calculation ///////////////////
         if(usevarn)then 
            hd=(injz-zant(na))/1.d3 !detector altitude
            h0=(injz-(z1+z2)/2.d0)/1.d3
            if(dabs(hd-h0) .gt. 1.d-10)then
               avn=(ns/(kr*(hd-h0)))*(exp(kr*hd)-exp(kr*h0))
            else
               avn=ns*exp(kr*h0)
               print*,"Effective n: h0=hd"
            endif
            ref_n=1.d0+1.d-6*avn !average (effective) n
            rh=ns*exp(kr*h0) !refractivity at emission
            nh=1.d0+1.d-6*rh !n at emission
         else 
            nh=ref_n
         endif
c     ///////////////////////////////////////////////////



      R = SQRT(UX*UX+UY*UY+UZ*UZ) ! m

c     Unit vector k
      UX=UX/R
      UY=UY/R
      UZ=UZ/R
      
c      AUXB = REF_N*UX*BETA_X + REF_N*UY*BETA_Y + REF_N*UZ*BETA_Z
c     new auxb using n(h) and not neff (ref_n)
      AUXB = nh*UX*BETA_X + nh*UY*BETA_Y + nh*UZ*BETA_Z
      DENOM = 1.d0 - AUXB
      
      DELTAT_1 = FACTOR_T*(CT1 + REF_N*R  - AUXB*(CT1-CT0)) - 
     + dt(na)
      DELTAT_2 = FACTOR_T*(CT2 + REF_N*R  - AUXB*(CT2-CT0)) -
     + dt(na)
      DTT = FACTOR_T*(CT2-CT1)
c     
c     k x (k x beta) 
c     beta perp in relation to U
      BETA_PERP_X=-(UY*UY+UZ*UZ)*BETA_X+UX*UY*BETA_Y+UX*UZ*BETA_Z
      BETA_PERP_Y=UX*UY*BETA_X-(UX*UX+UZ*UZ)*BETA_Y+UY*UZ*BETA_Z
      BETA_PERP_Z=UX*UZ*BETA_X+UY*UZ*BETA_Y-(UX*UX+UY*UY)*BETA_Z
      
      BETA_PERP=SQRT(BETA_PERP_X*BETA_PERP_X +
     +     BETA_PERP_Y*BETA_PERP_Y +
     +     BETA_PERP_Z*BETA_PERP_Z)
      
c     WARNING
c     k x (k x beta) is actually -beta_perp and NOT beta_perp => change signs
      BETA_PERP_X=-BETA_PERP_X
      BETA_PERP_Y=-BETA_PERP_Y
      BETA_PERP_Z=-BETA_PERP_Z
c     
c     Find time bins in which the contribution to vector potential is non-zero
      IF (DELTAT_1.LT.0.d0) THEN
         IT1=MAXTHALF + INT(DELTAT_1/DT_BIN_S)-1
      ELSE
         IT1=MAXTHALF + INT(DELTAT_1/DT_BIN_S)
      END IF
c     
      IF (DELTAT_2.LT.0.) THEN
         IT2=MAXTHALF + INT(DELTAT_2/DT_BIN_S)-1
      ELSE
         IT2=MAXTHALF + INT(DELTAT_2/DT_BIN_S)
      END IF
c     
      IF (IT1.LE.IT2) THEN 
         IT_STA=IT1
         IT_END=IT2
      ELSE
         IT_STA=IT2
         IT_END=IT1
      END IF   
c     
c     Get rid of times outside time range
      IF (IT_STA.GT.ntbins.OR.IT_END.LT.1) GOTO 200 ! Do not compute field
      IF (IT_STA.LT.1) IT_STA=1
      IF (IT_END.GT.ntbins) IT_END=ntbins
c     
      CONST2=CONST1
c     Change of sign when DELTAT_2 < DELTAT_1
      IF (DELTAT_2.LT.DELTAT_1) THEN
         CONST2=-CONST1
      END IF 
c     
c     Note ASUM_i is multiplied by e*mu_r/(8.*pi*epsilon_0*c)
c     when writing it to the file. One factor 1/c goes into beta 
      
c     Distribute vector potential among bins in time
c     and calculate average vector potential in each bin        
c     (see my notes: MC implementation)
c     Since we take the derivative at the end the important 
c     point is to conserve the structure from bin to bin.
      IF (DELTAT_2.LT.DELTAT_1) THEN 
         DELTAT_STA=DELTAT_2
         DELTAT_END=DELTAT_1
      ELSE
         DELTAT_STA=DELTAT_1
         DELTAT_END=DELTAT_2
      END IF
c    
C     TEST-histogram filling speedup
            value1x=CONST2*BETA_PERP_X/DENOM/R
            value1y=CONST2*BETA_PERP_Y/DENOM/R
            value1z=CONST2*BETA_PERP_Z/DENOM/R

            value2x=CONST2*BETA_PERP_X/R
            value2y=CONST2*BETA_PERP_Y/R
            value2z=CONST2*BETA_PERP_Z/R

            f1=ABS((DELTAT_END-DELTAT_STA)/DT_BIN_S)
            f2=ABS(DTT/DT_BIN_S)
            F3=ABS(((IT_STA+1-MAXTHALF)*DT_BIN_S-DELTAT_STA)/DT_BIN_S)
            F4=1.d0-
     +        ABS( ((IT_END+1-MAXTHALF)*DT_BIN_S-DELTAT_END)/DT_BIN_S)

            incr1x=f1*value1x
            incr1y=f1*value1y
            incr1z=f1*value1z
            
            incr2x=f2*value2x
            incr2y=f2*value2y
            incr2z=f2*value2z

            incr3x=f3*value1x
            incr3y=f3*value1y
            incr3z=f3*value1z

            incr4x=f4*value1x
            incr4y=f4*value1y
            incr4z=f4*value1z
            
            incr5x=value1x
            incr5y=value1y
            incr5z=value1z
            

            DO IT=IT_STA,IT_END,1
               IF(IT_STA.EQ.IT_END) THEN ! Subtrack contained in bin
c     
c                  F=(DELTAT_END-DELTAT_STA)/DT_BIN_S
c     
c$$$                  IF (ABS(DENOM).GT.1.D-13) THEN
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Z/DENOM/R

                  IF (ABS(DENOM).GT.1.D-13) THEN
                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr1x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr1y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr1z

                  ELSE          ! Cherenkov angle approximation
c                     F=DTT/DT_BIN_S
                     print*,"CERENKOV!!!!!!!!!!!!!!"
                   
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_X/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Y/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Z/R
                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr2x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr2y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr2z
                     
                  END IF
                  
               ELSE             ! Subtrack crossing one or more bins in time
c     Correct for bin edges               
c                  F=((IT_STA+1-MAXTHALF)*DT_BIN_S-DELTAT_STA)/DT_BIN_S
                  
                  IF (IT.EQ.IT_STA) THEN ! Start of subtrack 
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    ABS(F)*CONST2*BETA_PERP_Z/DENOM/R

                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr3x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr3y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr3z
c     
                  ELSE IF (IT.EQ.IT_END) THEN ! End of subtrack
c                     F=((IT_END+1-MAXTHALF)*DT_BIN_S-DELTAT_END)/
c     +                    DT_BIN_S

c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + 
c$$$     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + 
c$$$     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + 
c$$$     +                    (1.d0-ABS(F))*CONST2*BETA_PERP_Z/DENOM/R

                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr4x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr4y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr4z
                  ELSE
                     
c                     F=1.d0
c$$$                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) +
c$$$     +                    F*CONST2*BETA_PERP_X/DENOM/R
c$$$                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) +
c$$$     +                    F*CONST2*BETA_PERP_Y/DENOM/R
c$$$                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) +
c$$$     +                    F*CONST2*BETA_PERP_Z/DENOM/R
                     ASUM_X_FR(IT,NA) = ASUM_X_FR(IT,NA) + incr5x
                     ASUM_Y_FR(IT,NA) = ASUM_Y_FR(IT,NA) + incr5y
                     ASUM_Z_FR(IT,NA) = ASUM_Z_FR(IT,NA) + incr5z

                  END IF   
c     
               END IF
c     
            END DO              ! End loop in time



c     
 200  CONTINUE
c     reset ref_n
      ref_n=oldrefn
c     End ZHAireS' empsum_t block ++++++++
c     
      return
      end
c     --- End of routine empsum_t_fr
c     
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->






c     VALID ONLY FOR ICE!!!!!!!!!!!!!
      SUBROUTINE lossrateZHS(I,E,TE1,AP,DEI)
C     _______________________________________________________________
C     Ionization losses are defined as:
C     
C     1) Moeller electrons with kinetic energy up to TE1
C     (these are accounted for in the integrated expression
C     of ionization implemented in the subroutine, first
C     term in the formula for DEI: (AL+F-D)/BETA2)
C     
C     2) Bremsstrahlung photons of energy < AP
C     (last term in the expression for DEI)
      
C     
C     ***************************************************************
C     CALCULATES THE IONIZATION LOSS BY FORD & NELSON
C     E  -  PARTICLE TOTAL ENERGY (MeV)
C     DEI - dEdX (Mev / g/cm2)
C     I = 0  -  ELECTRON
C     I = 1  -  POSITRON
C     AP = Photon threshold (MeV)
C     TE1 = delta ray threshold (MeV) 
C     ***************************************************************

      implicit none

c     input
      Integer I
      double precision E,TE1,AP
c     output
      double precision DEI
      double precision FACNOR


c     Ionization loss parameters
      double precision C,A,RM,X0,X1,AI
      

c     other variables
      double precision BETA2,BETA2LN,G,G2,ETA,TAU
      double precision Y,TMAX1,DE,D2,F,X,D,AL

      parameter(FACNOR=3.07441783) !Factor de normalizacion al comienzo de 2.13.5 egs
      parameter(C=-3.502) !parametro del density effect
      parameter(A=.2065)  !parametro del density effect
      parameter(RM=3.007) !parametro del density effect
      parameter(X0=0.24)  !parametro del density effect
      parameter(X1=2.5)  !parametro del density effect
      parameter(AI=1.46771D-4) ! AI is the averaed adjusted mean ionization energy, in mass units


      G=E/0.51099906  ! electron mass
      G2=G*G
      BETA2=1.-1./G2
      IF (G .LT. 10.) THEN
         BETA2LN=DLOG(BETA2)
      ELSE
         BETA2LN=-1./G2*(1.+(0.5+0.33333333/G2)/G2)
      END IF
      ETA=DSQRT(BETA2)*G
      TAU=G-1.
      Y=1./(G+1.)
      TMAX1=TAU/2.
      IF(I.EQ.1) TMAX1=TAU
      DE=AMIN1(TMAX1,TE1)
      D2=DE*DE
C
c    F function     
      IF(I.EQ.0)THEN !electron
         F=DLOG((TAU-DE)*DE)+TAU/(TAU-DE)+(D2/2.+(2.*TAU+1.)*DLOG(1.-DE/
     &TAU))/(G   *G   )   -1.-BETA2
      ELSE !positron
         F=DLOG(TAU*DE)-BETA2*(TAU+2.*DE-1.5*D2*Y-(DE-DE*D2/3.)*Y*Y-
     &(D2/2.-TAU*DE*D2/3.+D2*D2/4.)*Y*Y*Y)/TAU
      ENDIF
C     
c     density correction (2.13.19) EGS
      X=DLOG10(ETA)
      IF(X.GE.X1) D=4.606*X+C !4.606 comes from 2*ln(10)
      IF((X.LT.X1).AND.(X.GT.X0)) D=4.606*X+C+A*(X1-X)**RM !rm is Ms is egs notation
      IF(X.LT.X0) D=0.
C     
      AL=DLOG(2.*(TAU+2.)/(AI*AI))
C     
      DEI=FACNOR*((AL+F-D)/BETA2 !Sub cutof atomic electrons energy loss term
     &+ AP*((1.-0.5*AP/E)*(4.+1./LOG(183./2.))+(AP/E)**2.)/3.) !soft bremstralung
      
      RETURN
      END
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c     
c     End of file 'eplusminus.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
