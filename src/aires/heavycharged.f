c
c     FILE: heavycharged.f                  Creation date: 27/AUG/1996.
c                                       LAST MODIFICATION: 16/MAR/2011.
c
c     This file contains the routines which deal with heavy charged
c     particle processing.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine heavychadv(npart, pdatarr, nrem, remidx)
c
c     Advancing heavy charged particles and deciding their fates.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2001, 2002, 2003.
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
c     TEST: injz,injdepth,groundalt,etc...
      
      include 'initpar.f'
      include 'initcomm.f'
c///////////////////
      include 'constants.f'
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'modelpar.f'
      include 'hmfppar.f'
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
      include 'modelcomm2.f'
      include 'hmfpcomm.f'
C
C     FIELD TOOL: include field tool's commons
      include 'fieldcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  kom0, nupmaxpar, muppnu02i, muppnu02isq
      parameter         (kom0 = (1 + (electronmass / muonmass) ** 2)
     +                          / twoemass)
      parameter         (nupmaxpar   = sqrmumass / twoemass)
      parameter         (muppnu02i   = 0.01d0)
      parameter         (muppnu02isq = muppnu02i ** 2)
c
      integer           istack, i
      integer           charge, massa, apcode, nucint, imfp
      double precision  mass, mfp, pspfac
      double precision  mm, xe, ukomax, ke, ke1, ke2, ch2
      double precision  u, uu, lnu, g, gg, wsq, weff, prevtime
      logical           geosw
      double precision  muonmratio, inke, nuke, freegy, cutegy
      double precision  alos, blos, clos, dlos, zlos
      double precision  path, collpath, totpath
      double precision  maxpath, step
      double precision  lifedist, lifepath, mupath
      double precision  lossrate
      logical           notnuc, apcodelt30, ismuon
      logical           scatterpath
      logical           nomore, plost, plowe, goon
      double precision  v, vc, v0vc, lv0vc, vma, uvv, dta, gv, c, f
      double precision  muppnu02, muppnu02sq
      double precision  numin1, numax1, rho1, rhomax, vmv
      double precision  ct2, st2, cp2, sp2, upsec0(3)
      double precision  urandom, urandomt, ppath
      double precision  hmfplowe, nucnucmfp
      double precision  mupp0rho0g
c



C     ZHAireS variables++++++++++++++++
      double precision deltaz,deltax,deltay
      double precision Wgt0,lateral
      double precision track
      double precision injectionaltitude,injectiondepth
      double precision groundaltitude,zenithdeg,coszenith
      integer ityp,itypemp
      logical pimu !true if pion or muon
      double precision ctstart,ctend,estart,eend
      double precision x_m,y_m,z_m
      double precision x1_m,y1_m,z1_m,x2_m,y2_m,z2_m
C     dEdx test variables
      double precision ei,ef,de
c
c     initializing variables
c
c     used to transform aires time into s (t0=0s at injection time)
      injectionaltitude=injz
      injectiondepth=injdepth
      groundaltitude=groundz
c     NOTE: zenith angle must be fixed for all showers!
c     If not must use showercomm.f to get particular zenith angle (not yet done)
      zenithdeg=pryzenithmin
      coszenith=cos(zenithdeg*dacos(-1.d0)/180.d0)
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++



c     FIRST EXECUTABLE STATEMENT
c
      nrem = 0
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
c       The particles processed in this routine are heavy charged
c       particles (muons, pi+-, proton, nuclei).
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c
c       BEGINNING OF PARTICLE PROCESSING.
c

C     FIELD TOOL: new particle block ++++++++++++++++++++++++
c     New particle !!!!
        if(trackflag) then
c     test if muon or charged pion and set itypempsum
c     CHANGED ITYP for empsum calls(itypemp): mu+=-3,mu-=3,pi+=-2,pi-=2
c     to identify and set mass in empsum subs
    
c     TEST: new - no threshold test! Include p and p-bar
           if((prcode .eq. mupluscode) .and.
     +          (prenergy .ge. fmucut)) then
              ityp=-1
              itypemp=-3
              pimu=.true.
           else if ((prcode .eq. pipluscode) .and. 
     +             (prenergy .ge. fpicut)) then
              ityp=-1
              itypemp=-2
              pimu=.true.
           else if ((prcode .eq. muminuscode) .and. 
     +             (prenergy .ge. fmucut)) then
              ityp=1
              itypemp=3
              pimu=.true.
           else if ((prcode .eq. piminuscode) .and. 
     +             (prenergy .ge. fpicut)) then
              ityp=1
              itypemp=2
              pimu=.true.
           else if ((prcode .eq. protoncode) .and. 
     +             (prenergy .ge. fpcut)) then
              ityp=-1
              itypemp=-4
              pimu=.true.
           else if ((prcode .eq. pbarcode) .and. 
     +             (prenergy .ge. fpcut)) then
              ityp=1
              itypemp=4
              pimu=.true.
           else
              pimu=.false.
           endif
c     ////////////////


c     check if it's a relevant particle
           if(pimu) then
c     Weigth
              Wgt0=prwt

c     Variables for this point==================
           
c     set particle x and y in m
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
              estart=(prenergy+pclemass(prcode))*1.d3
           endif
        endif
c     FIELD TOOL: end of new particle block ++++++++++++

        apcode = abs(prcode)
c
        if (prcode .lt. minncode) then
c
c         The particle is not a nucleus.
c         abs(charge) is 1 so charge ** 2 is always 1.
c
          apcodelt30 = (apcode .lt. neutroncode)
c
c         Low energetic particles are forced to decays.
c
          if (prlowe .and. apcodelt30) then
            plowe = .true.
            goto 1100
          endif
c
          ismuon  = (apcode .eq. mupluscode)
          notnuc  = .true.
          mass    = pclemass(prcode)
          charge  = pcleq(prcode)
          freegy  = pfreeegy(prcode)
          pspfac  = heavymineko2
          nucint  = 8
          if (ismuon) then
            cutegy  = muoncut1
          else
            imfp = pcode2highemfpset(prcode)
            if (apcodelt30) then
              cutegy = heavycut1
            else
              cutegy = nuclcut1
            endif
          endif
c
          alos = amlos
          blos = bmlos
          dlos = dmlos
c
        else
c
c         The particle is a nucleus.
c
          notnuc     = .false.
          ismuon     = .false.
          apcodelt30 = .false.
          mass       = nucmass(prcode)
          massa      = nuca(prcode)
          charge     = nucz(prcode)
          ch2        = charge ** 2
          imfp       = nucleonhset
          freegy     = 0
          nucint     = 80
          cutegy     = nuclcut1
          pspfac     = heavymineko2 / ch2
c
          alos       = amlos * ch2
          blos       = bmlos * ch2
          dlos       = dmlos * ch2
c
        endif
c
        mm         = mass ** 2
        muonmratio = mass / muonmass
        blos       = blos  * muonmratio
        clos       = cmlos * muonmratio
        dlos       = dlos  * muonmratio
        zlos       = zmlos * muonmratio
c
c       Other particle initializations.
c
        call lgtinit(prcode, charge, pclerecord, frecord, geosw)
c
 1005   continue
c
        if (notnuc) then
c
c         The particle is not a nucleus.
c
          ke = prenergy
c
        else
c
c         The particle is a nucleus.
c
          ke = prenergy / massa
c
        endif
c
c       Selecting fate, starting with knock-on.
c       Knock-on is negligible when gamma < 2, i.e., penergy < mass.
c
        if (prenergy .gt. mass) then
          psparei(0) = 6
          path       = pspfac * log(urandomt(0.d0))
        else
          psparei(0) = 9
          path       = 7777
        endif
c
c       Comparing with other alternatives.
c
        if (ismuon) then
c
c         The particle is a muon. Checking paths for
c         muon bremsstrahlung and muonic pair production.
c
          if (ke .gt. minmubremegy) then
c
            u = 1 / (mmfpl0 + log(ke))
c
c           Muonic pair production.
c
            mfp = exp(mppmfp0 + u * (mppmfp1 +
     +                          u * (mppmfp2 + u * mppmfp3)))
c
            mupath = -mfp * log(urandomt(0.d0))
c
            if (mupath .lt. path) then
c
c             Fate is muonic pair production.
c
              psparei(0) = 3
              path       = mupath
            endif
c
c           Bremsstrahlung.
c
            mfp = exp(mbrmfp0 + u * (mbrmfp1 +
     +                          u * (mbrmfp2 + u * mbrmfp3)))
c
            mupath = -mfp * log(urandomt(0.d0))
c
            if (mupath .lt. path) then
c
c             Fate is bremsstrahlung.
c
              psparei(0) = 4
              path       = mupath
            endif
          endif
c
        else
c
c         The particle is not a muon.
c
c         Would nuclear interaction occur earlier?
c
          if (ke .gt. nucollthreshold) then
c
            if (notnuc) then
c
c             Hadron-nucleus interaction.
c
              u = log(ke)
c
              if (externalmfp .and. (ke .gt. mfpthreshold)) then
c
c               External cross sections.
c               If we are here the particle is either a nucleon,
c               a nucleus or a charged meson.
c
                mfp = mfp8ppk(1, imfp) *
     +                     (1 + u * (mfp8ppk(2, imfp) +
     +                          u * (mfp8ppk(3, imfp) +
     +                               u * mfp8ppk(4, imfp)))) /
     +                     (1 + u * (mfp8ppk(5, imfp) +
     +                          u * (mfp8ppk(6, imfp) +
     +                          u * (mfp8ppk(7, imfp) +
     +                               u * mfp8ppk(8, imfp)))))
c
              else
c
c               Using low energy cross sections.
c
                mfp = hmfplowe(prcode, u)
c
              endif
c
            else
c
c             Nucleus-nucleus interaction.
c
              mfp = nucnucmfp(massa, charge, ke)
c
            endif
c
            collpath = - mfp * log(urandomt(0.d0))
c
            if (collpath .lt. path) then
c
c             Fate is nucint.
c
              psparei(0) = nucint
c
              path = collpath
            endif
          endif
c
        endif
c
c       Would decay occur earlier?
c
        if (apcodelt30) then
c
c         The particle is neither a nucleon nor a nucleus.
c
          nuke     = max(cutegy, ke - 2.d-3 * path)
          gg       = (1 + ke / mass) * (1 + nuke / mass)
          lifedist = - cpclelife(prcode) * sqrt(gg - 1)
     +                                   * log(urandomt(0.d0))
          lifepath = ppath(lifedist, frecord(ixpx), prdepth,
     +                     frecord(ixx))
c
          if (lifepath .lt. path) then
c
c           Decay occurs before reaching predicted end.
c
            psparei(0) = 7
            path       = lifepath
c
          endif
        endif
c
c       Now the particle is going to be advanced.
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
c       The amount of depth to advance at each step depends on the
c       altitude of the particle.
c
        if (prdepth .gt. 200.d0) then
          if (prenergy .le. 1.6d-3) then
            maxpath = 0.16d0
          else if (prenergy .gt. 0.05d0) then
            maxpath = 5.00d0
          else
            maxpath = 0.10d3 * prenergy
          endif
        else if (prdepth .gt. 2.00d0) then
          maxpath = 0.3536d0 * sqrt(prdepth)
        else if (prdepth .gt. (4 * xquantum)) then
          maxpath = prdepth / 4
        else
          maxpath = xquantum
        endif
c
        if (totpath .gt. maxpath) then
          path        = maxpath
          totpath     = totpath - path
        else
          path        = totpath
          totpath     = 0
        endif
        scatterpath = (path .gt. 0.002d0)
c
c       We treat the atmosphere as a continuum (Layers are for
c       calculation purposes only. No physical meaning.)
c       This implies pathinmed being equal to path.
c
        step = 0.5d0 * path
c
c       Entering the advance and longitudinal monitoring procedure
c       (1st. half path).
c
c       Saving old kinetic energy and time.
c
        inke     = prenergy
        prevtime = prtime
c
        lossrate = alos - blos / (prenergy + clos) +
     +                    dlos / (prenergy + zlos)

c
        call lgtmaster(prcode, step, mass, lossrate, cutegy,
     +                 pclerecord, frecord, prlol, prcal, prlowe,
     +                 nomore, plost, plowe)
c

C     FIELD TOOL: start of first advance block ++++++++++++
        if (trackflag .and. pimu) then
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
           eend=(prenergy+pclemass(prcode))*1.d3
c     pimu tracksums (from fieldcomm.f)
           track=Wgt0*dsqrt(deltax*deltax+deltay*deltay+deltaz*deltaz)
           pimutracksum=pimutracksum+track
           pimusumdeltaz=pimusumdeltaz+Wgt0*deltaz
           pimusumabdeltaz=pimusumabdeltaz+Wgt0*dabs(deltaz)
           pimuexcesssum=pimuexcesssum+ityp*track
           pimuexcesssumz=pimuexcesssumz+ityp*Wgt0*deltaz
           pimuexcessabsumz=pimuexcessabsumz+ityp*Wgt0*dabs(deltaz)

c     per particle
              if(prcode .eq. pipluscode)then

                 piptracksum=piptracksum+track
                 pipsumdeltaz=pipsumdeltaz+Wgt0*deltaz
                 pipsumabdeltaz=pipsumabdeltaz+Wgt0*dabs(deltaz)
              else if(prcode .eq. piminuscode)then

                 pimtracksum=pimtracksum+track
                 pimsumdeltaz=pimsumdeltaz+Wgt0*deltaz
                 pimsumabdeltaz=pimsumabdeltaz+Wgt0*dabs(deltaz)

              else if(prcode .eq. mupluscode)then
                 
                 muptracksum=muptracksum+track
                 mupsumdeltaz=mupsumdeltaz+Wgt0*deltaz
                 mupsumabdeltaz=mupsumabdeltaz+Wgt0*dabs(deltaz)

              else if(prcode .eq. muminuscode)then
              
                 mumtracksum=mumtracksum+track
                 mumsumdeltaz=mumsumdeltaz+Wgt0*deltaz
                 mumsumabdeltaz=mumsumabdeltaz+Wgt0*dabs(deltaz)
                 
              else if(prcode .eq. protoncode)then

                 ptracksum=ptracksum+track
                 psumdeltaz=psumdeltaz+Wgt0*deltaz
                 psumabdeltaz=psumabdeltaz+Wgt0*dabs(deltaz)

              else
                 
                 pbartracksum=pbartracksum+track
                 pbarsumdeltaz=pbarsumdeltaz+Wgt0*deltaz
                 pbarsumabdeltaz=pbarsumabdeltaz+Wgt0*dabs(deltaz)

              endif
c     total tracksums (from fieldcomm.f)
           if(countpimu)then
              tracksum=tracksum+track
              sumdeltaz=sumdeltaz+Wgt0*deltaz
              sumabdeltaz=sumabdeltaz+Wgt0*dabs(deltaz)
              excesssum=excesssum+ityp*track
              excesssumz=excesssumz+ityp*Wgt0*deltaz
              excessabsumz=excessabsumz+ityp*Wgt0*dabs(deltaz)

c     Call field tool's EMPSUM subroutine
              if(calcfield)then
                 if(calcfreq)then
                    call empsum(itypemp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                   y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
                 if(calctime)then
                    call empsum_t(itypemp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                   y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
                 if(calcfreqfres)then
                    call empsum_fr(itypemp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                   y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
                 if(calctimefres)then
                    call empsum_t_fr(itypemp,x1_m,y1_m,z1_m,ctstart,
     +                   x2_m,y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
              endif
           endif
c
c     =================================
        
c     set this point as new start point
           x1_m=x_m
           y1_m=y_m
           z1_m=z_m
           estart=eend
           ctstart=ctend
        endif
C     FIELD TOOL: end of first advance block++++++++++++++++++++++++++++


        if (nomore) goto 1100
c
c       End of advancing sector.
c
c       We now know that the particle is still stacked.
c
        wsq  = (prenergy + mass) * (inke + mass)
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
c       No magnetic field currently considered.
c
c       Treating Coulomb scattering.
c
        if (scatterpath) then
          call coulscatter(hvychimin, frecord, path, mm, wsq, weff)
        endif 
c
c       Entering the advance and longitudinal monitoring procedure
c       (2nd. half path).
c
        lossrate = alos - blos / (prenergy + clos) +
     +                    dlos / (prenergy + zlos)
c
        call lgtmaster(prcode, step, mass, lossrate, cutegy,
     +                 pclerecord, frecord,
     +                 irecord(ixlol), prcal, prlowe,
     +                 nomore, plost, plowe)
c
C     FIELD TOOL: start of second advance block  +++++++++
        if (trackflag .and. pimu) then
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
           eend=(prenergy+pclemass(prcode))*1.d3

c     pimu tracksums (from fieldcomm.f)
           track=Wgt0*dsqrt(deltax*deltax+deltay*deltay+deltaz*deltaz)
           pimutracksum=pimutracksum+track
           pimusumdeltaz=pimusumdeltaz+Wgt0*deltaz
           pimusumabdeltaz=pimusumabdeltaz+Wgt0*dabs(deltaz)
           pimuexcesssum=pimuexcesssum+ityp*track
           pimuexcesssumz=pimuexcesssumz+ityp*Wgt0*deltaz
           pimuexcessabsumz=pimuexcessabsumz+ityp*Wgt0*dabs(deltaz)
c     per particle
              if(prcode .eq. pipluscode)then

                 piptracksum=piptracksum+track
                 pipsumdeltaz=pipsumdeltaz+Wgt0*deltaz
                 pipsumabdeltaz=pipsumabdeltaz+Wgt0*dabs(deltaz)
              else if(prcode .eq. piminuscode)then

                 pimtracksum=pimtracksum+track
                 pimsumdeltaz=pimsumdeltaz+Wgt0*deltaz
                 pimsumabdeltaz=pimsumabdeltaz+Wgt0*dabs(deltaz)

              else if(prcode .eq. mupluscode)then
                 
                 muptracksum=muptracksum+track
                 mupsumdeltaz=mupsumdeltaz+Wgt0*deltaz
                 mupsumabdeltaz=mupsumabdeltaz+Wgt0*dabs(deltaz)

              else if(prcode .eq. muminuscode)then
              
                 mumtracksum=mumtracksum+track
                 mumsumdeltaz=mumsumdeltaz+Wgt0*deltaz
                 mumsumabdeltaz=mumsumabdeltaz+Wgt0*dabs(deltaz)
                 
              else if(prcode .eq. protoncode)then

                 ptracksum=ptracksum+track
                 psumdeltaz=psumdeltaz+Wgt0*deltaz
                 psumabdeltaz=psumabdeltaz+Wgt0*dabs(deltaz)

              else
                 
                 pbartracksum=pbartracksum+track
                 pbarsumdeltaz=pbarsumdeltaz+Wgt0*deltaz
                 pbarsumabdeltaz=pbarsumabdeltaz+Wgt0*dabs(deltaz)

              endif
c     total tracksums (from fieldcomm.f)
           if(countpimu)then
              tracksum=tracksum+track
              sumdeltaz=sumdeltaz+Wgt0*deltaz
              sumabdeltaz=sumabdeltaz+Wgt0*dabs(deltaz)
              excesssum=excesssum+ityp*track
              excesssumz=excesssumz+ityp*Wgt0*deltaz
              excessabsumz=excessabsumz+ityp*Wgt0*dabs(deltaz)

c     Call field tool's EMPSUM subroutine
              if(calcfield)then
                 if(calcfreq)then
                    call empsum(itypemp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                   y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
                 if(calctime)then
                    call empsum_t(itypemp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                   y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
                 if(calcfreqfres)then
                    call empsum_fr(itypemp,x1_m,y1_m,z1_m,ctstart,x2_m,
     +                   y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
                 if(calctimefres)then
                    call empsum_t_fr(itypemp,x1_m,y1_m,z1_m,ctstart,
     +                   x2_m,y2_m,z2_m,ctend,estart,eend,Wgt0)
                 endif
              endif
           endif
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
        if (psparei(0) .eq. 6) then
c
c         Current fate is knock-on.
c
c         Applying correction to the cross section at low energies.
c
          if (prenergy .le. mass) goto 1005
c
          g = 1 + prenergy / mass
c
          if (g .lt. 60.d0) then
            f = g - 2
            f = (hkcfa + f) / (hkcfb + f + hkcfc / f)
            if (f .lt. urandom()) goto 1005
          endif

c         Knock-on accepted. Checking energy of secondary electron.
c
          gg     = g * g
          f      = gg - 1
          ukomax = kom0 + g / mass
          c      = ukomax / gg
          ukomax = ukomax / f
c
 1030     continue
c
          xe  = urandomt(0.d0)
          ke2 = 1 / (uheavymineko * (1 - xe) + ukomax * xe)
          gv  = c * ke2
c
          if (gv .ge. urandom()) goto 1030
c
          ke1 = prenergy - ke2
c
c         Now we evaluate an approximate emission angle for the
c         electron. The projectile particle is not deflected.
c         Such approximation is justified in most cases where
c         the projectile particle is much heavier than the
c         electron.
c
          ct2 = sqrt(ke2 / (ke2 + twoemass))
c            
          call deflectr(frecord(ixpx), ct2, st2, cp2, sp2, upsec0)
c
c         Emitting the secondary electron:
c
          call stack1pclek(1, prenergy + freegy, ke1 + freegy,
     +                     electroncode, ke2, prwt,
     +                     frecord(ixx), prdepth, prtime,
     +                     prlhdepth, upsec0,
     +                     prlol, prcal, goon)
c

c     FIELD TOOL: Setting new weight after secondary emission
              if (trackflag .and. pimu) Wgt0=prwt        
c     +++++++++++++++++++++++++

c         Continuing only if the particle was not deleted.
c
          if (.not. goon) goto 1120
c
c         The particle looses energy.
c
          prenergy = ke1
c
          goto 1005
c
        else if (psparei(0) .eq. 3) then
c
c         Current fate is muonic pair production.
c         (This section written together with A. Cillis).
c
          if (prenergy .le. mubremegycut) goto 1005
c
c         Evaluating energy of secondary electrons.
c
          u  = prenergy + muonmass
          uu = 1 / u
c
c         Energy dependent initialization.
c
	  mesqrte2f  = muppepar * uu
	  numin1      = (4 * electronmass) * uu
	  numax1      = 1 - mubremupar * uu
          vc         = mubremegycut * uu
          if (vc .lt. muppnu02i) then
            muppnu02   = muppnu02i
            muppnu02sq = muppnu02isq
          else
            muppnu02   = 1.015d0 * vc
            muppnu02sq = muppnu02 ** 2
          endif
          v0vc       = muppnu02 / vc
          lv0vc      = log(v0vc)
          vma        = lv0vc + (1 - (muppnu02 / numax1) ** 2) / 2
c
          lnu        = log(u)
          muppugmax  = exp(-0.49743 + lnu * (-1.0782 +
     +                                lnu * ( 0.11963 +
     +                                lnu * (-0.0074335 +
     +                                lnu * ( 0.22987E-03 +
     +                                lnu * (-0.27547E-05))))) )
c
 1040     continue
          f = vma * urandomt(0.d0)
          if (f .le. lv0vc) then
            v  = vc * exp(f)
            gv = mupp0rho0g(v)
          else
            v  = muppnu02 / sqrt(1 + 2 * (lv0vc - f))
            gv = v * v * mupp0rho0g(v) / muppnu02sq
          endif
          vmv    = numin1 / v
          rhomax = sqrt(1 - vmv) *
     +             (1 - (6 * sqrmumass) / (u * u * (1 - v)))
          gv     = gv * rhomax * (1 - 0.666666667d0 * sqrt(vmv))
          if (gv .lt. urandom()) goto 1040
c
          rho1        = (2 * urandomt(0.d0) - 1) * rhomax
c
          ke1        = v * u
          esec(3)    = prenergy - ke1
          ke1        = ke1 / 2
          ke2        = rho1 * ke1
          ke1        = ke1 - electronmass
          esec(1)    = ke1 + ke2
          esec(2)    = ke1 - ke2
          seccode(1) = positroncode
          seccode(2) = electroncode
c
c         The primary is processed as "secondary number 3"
c
          seccode(3) = prcode
c
          do i = 1, 3
            upsec(i, 3) = frecord((ixpx - 1) + i)
          enddo
c
c         Deflecting and stacking the electrons.
c         The upsec0(1), (2) represent ct1 and ct2 respectively.
c
          ct2       = 1 - 0.5d0 * (muonmass / u) ** 2
          upsec0(1) = ct2
          upsec0(2) = ct2
c
          call pdeflectr(frecord(ixpx), upsec0, 3, upsec)
c
c         Emitting the secondary electrons.
c
          call stacknpcled(1, prenergy,
     +                     3, seccode, esec, prwt,
     +                     frecord(ixx), prdepth, prtime,
     +                     prlhdepth, 3, upsec,
     +                     prlol, prcal, 3, goon, prwt, wa)
c

c     FIELD TOOL: Setting new weight after secondary emission
          if (trackflag .and. pimu) Wgt0=prwt        
c     +++++++++++++++++++++++++
          


c         Continuing only if the primary particle was not deleted.
c
          if (.not. goon) goto 1120
c
c         The particle looses energy.
c
          prenergy = esec(3)
c
          goto 1005
c
        else if (psparei(0) .eq. 4) then
c
c         Current fate is muon bremsstrahlung.
c         (This section written together with A. Cillis).
c
          if (prenergy .le. mubremegycut) goto 1005
c
c         Evaluating energy of radiated photon.
c
          u   = prenergy + muonmass
          vc  = mubremegycut / u
          vma = 1 - mubremupar / u
c
 1060     continue
c
          v   = vc * ((vma / vc) ** urandomt(0.d0))
          uvv = u * (1 - v)
          dta = v / uvv
          gv  = mubremphin0 - log(1 + mubremphin1 * dta)
c
          if (v * (1 + nupmaxpar / u) .lt. 1) then
            c = (1 + mubremphie1 * dta) *
     +          (1 + mubremphie2 * dta)
            if (c .lt. mubremphie3) then
              gv = gv + (mubremphie0 - log(c)) / zeff
            endif
          endif
c
          gv  = gv * (1 + v * (0.75d0 * v - 1))
c
          if (gv .lt. (mubremphi0 * urandom())) goto 1060
c
          c   = 1500.d0 * (u ** 2) / log(2 / (dta * muonmass))
          f   = urandom()
          ct2 = 1 + (f - 1) / (0.5d0 + c * f)
          ke2 = v * u
          ke1 = prenergy - ke2
c
c         Deflecting and emitting the radiated photon.
c            
          call deflectr(frecord(ixpx), ct2, st2, cp2, sp2, upsec0)
c
          call stack1pclek(1, prenergy + freegy, ke1 + freegy,
     +                     gammacode, ke2, prwt,
     +                     frecord(ixx), prdepth, prtime,
     +                     prlhdepth, upsec0,
     +                     prlol, prcal, goon)
c
c     FIELD TOOL: Setting new weight after secondary emission
          if (trackflag .and. pimu) Wgt0=prwt        
c     +++++++++++++++++++++++++


c         Continuing only if the particle was not deleted.
c
          if (.not. goon) goto 1120
c
c         The particle looses energy.
c
          prenergy = ke1
c
          goto 1005
c
        endif
c
c       All checks passed.
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
c       Entry for lost or low energetic particles.
c
 1100   continue
c
c       Low energetic particles are kept in the stack and
c       unstable ones forced to decays if necessary.
c
        if (plowe) then
c
          if (apcodelt30 .and. forcelowedecay) then
            psparei(0) = 7
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
c     --- End of routine heavychadv
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine heavychdecay(npart, pdatarr, nrem, remidx)
c
c     Processing the heavy charged particles that have already been
c     advanced by routine heavychadv, accordingly with the fates that
c     were set.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2000, 2001,
c                                         2003.
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
      include 'pclepar.f'
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
      include 'pclecomm.f'
      include 'modelcomm2.f'
c
c     Declaration of internal variables and arrays.
c
      integer           istack
c
      double precision  mupimr, epim, ppim
      parameter         (mupimr = muonmass / chpimass)
      parameter         (epim = (1 + mupimr ** 2) / 2)
      parameter         (ppim = 1 - epim)
c
      integer           apcode, n, idummy
      double precision  beta, costh, ccth, etot
      double precision  pl, pt2, p2, p, x, e
      logical           ktp
      integer           nfrag
      double precision  efrag
      double precision  urandom, urandomt
      double precision etotdecay
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
c       Particle code is integer, prlol and prcal are integers,
c       prolin is logical and all other variables are double precision.
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c
c       BEGINNING OF PARTICLE PROCESSING.
c
c       Switching accordigly with the preselected fate.
c
c       Knock-on is completely treated in the advance routine.
c
        if (psparei(0) .eq. 7) then
c
c         Fate is decays.
c         Here the particle is neither a nucleon nor a nucleus.
c
          apcode = abs(prcode)
          nsec   = 2
c
          if (apcode .eq. mupluscode) then
c
c           The particle is a muon, which decays to e+/e- + nu.
c
 1010       continue
            x = 0.5d0 * urandomt(1.2d-3 / muonmass)
            if (urandom() .ge. (4 * (x ** 2) * (3 - 4 * x))) goto 1010
c
            etot       = prenergy + muonmass
            e          = x * muonmass
            costh      = 2 * urandom() - 1
            p2         = e ** 2 - sqremass
            p          = sqrt(p2) * costh
            beta       = sqrt(1 - (muonmass / etot) ** 2)
            esec(1)    = etot * (e + beta * p) / muonmass
            esec(2)    = etot - esec(1)
            esec(1)    = esec(1) - electronmass
            pt2        = p2 * (1 - costh ** 2)
            pl         = etot * (p + beta * e) / muonmass
            p          = sqrt(pl ** 2 + pt2)
            wa(1)      = pl / p
c
c           The angle for the neutrino does not matter.
c
            wa(2)      = 1
c
            seccode(1) = sign(positroncode, prcode)
            seccode(2) = nuecode
            
c           Deflecting the secondaries
c
            call pdeflectr(frecord(ixpx), wa, 3, upsec)
c
          else if (apcode .eq. pipluscode) then
c
c           Pi+- decay to muon + neutrino(mu).
c
            etot       = prenergy + chpimass
            costh      = 2 * urandom() - 1
            beta       = sqrt(1 - (chpimass / etot) ** 2)
            ccth       = costh * ppim
            esec(1)    = etot * (epim + beta * ccth)
            esec(2)    = etot - esec(1)
            esec(1)    = esec(1) - muonmass
            pl         = etot * (ccth + beta * epim)
            p          = sqrt(pl ** 2 +
     +                        (ppim * chpimass) ** 2 *
     +                        (1 - costh ** 2))
            wa(1)      = pl / p
c
c           The angle for the neutrino does not matter.
c
            wa(2)      = 1
c
            seccode(1) = sign(mupluscode, prcode)
            seccode(2) = numucode

c           Deflecting the secondaries
c
            call pdeflectr(frecord(ixpx), wa, 3, upsec)
c
          else
c
c           Other decays are not so frequent and are processed in a
c           separate routine.
c

             etotdecay=prenergy+pclemass(prcode)
             
             call rarechdecay(prcode, prenergy, frecord(ixpx),
     +                       nsec, 3, seccode, esec, upsec)
            etot = prenergy + pclemass(prcode)

c
          endif
c
c         Stacking the secondaries
c
          call stacknpcled(2, etot, nsec, seccode, esec,
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prlhdepth, 3, upsec, prlol, prcal,
     +                     0, ktp, prwt, wa)
c
        else if (psparei(0) .eq. 8) then
c
c         Fate is nucint (elementary particles).
c
          call hnucoll((abs(prcode) .ge. lambdacode), prcode, prwt,
     +                 prenergy, frecord(ixpx), prlol,
     +                 maxsecpcles, 3,
     +                 nsec, seccode, esec, upsec, secgen, wa)
c
          if (nsec .gt. (maxsecpcles - 20))
     +      call errprint(2, 'NSEC', 4, 'heavychdecay', ' ',
     +                    1, nsec, 0, 0.d0, ' ')
c
c         Stacking the generated secondaries
c
          call stacknpcled(2, prenergy, nsec, seccode, esec,
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prdepth, 3, upsec, prlol, prcal,
     +                     0, ktp, prwt, wa)
c
        else if (psparei(0) .eq. 80) then
c
c         Fate is nucint (nuclei).
c
          e = prenergy / nuca(prcode)
c
          call nucnucoll(prcode, nuca(prcode), nucz(prcode), prwt,
     +                   e, frecord(ixpx), prlol,
     +                   maxsecpcles, 3, nsec, nfrag, efrag,
     +                   seccode, esec, upsec, secgen, wa)
c
          if (nsec .gt. (maxsecpcles - 20))
     +      call errprint(2, 'NSEC', 4, 'heavychdecay', ' ',
     +                    1, nsec, 0, 0.d0, ' ')
c
c         Stacking the generated secondaries
c
c         Fragments and elastically scattered nucleons are stacked
c         unconditionally.
c
          call stacknpcleu(2, nfrag, seccode, esec,
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prdepth, 3, upsec, prlol, prcal, idummy)
c
c         Particles from inelastic processes.
c
          e = prenergy - efrag
          n = nfrag + 1
c
          call stacknpcled(2, e, nsec - nfrag, seccode(n), esec(n),
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prdepth, 3, upsec(1, n), prlol, prcal,
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
      end
c     --- End of routine heavychdecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine rarechdecay(pcode, penergy, up,
     +                       nsec, ldu, seccode, esec, upsec)
c
c     Processimg decays of heavy charged particles (excluding muons and
c     pions).
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     pcode.......... (input, integer) Particle code.
c     penergy........ (input, double precision) Particle kinetic
c                     energy (GeV).
c     up............. (input, double precision, array(3)) Direction of
c                     motion of incoming particle.
c     nsec........... (output, integer) Number of secondaries after the
c                     decay.
c     ldu............ (input, integer) Leading dimension of array
c                     upsec.
c     seccode........ (output, integer, array(nsec)) Secondary particle
c                     codes.
c     esec........... (output, double precision, arrya(nsec)) Kinetic
c                     energy of secondaries.
c     upsec.......... (output, double precision, array(ldu, nsec))
c                     Direction of motion of secondaries.
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
      integer           pcode
      double precision  penergy
      double precision  up(3)
      integer           nsec, ldu
      integer           seccode(nsec)
      double precision  esec(nsec)
      double precision  upsec(ldu, nsec)
c
c     Declaration of internal variables and arrays.
c
      integer           apcode
      double precision  primass, branch
      double precision  secmass1, secmass2, secmass3
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Switching accordingly with particle code (muons and charged pions
c     are treated separately).
c
c     All the decays considered here are 2 body decays, with the sole
c     exception of some K+- 3-body decays (see below).
c
      nsec = 2
c
      apcode = abs(pcode)
c
      if (apcode .eq. kpluscode) then
c
c       K+- decays.
c
        primass = chkmass
        branch  = urandom()
c
        if (branch .lt. 0.63516d0) then
c
c         K+- --> mu+- nu_mu           (63.516%).
c
          seccode(1) = sign(mupluscode, pcode)
          seccode(2) = numucode
          secmass1   = muonmass
          secmass2   = 0
c
        else if (branch .lt. 0.84678d0) then
c
c         K+- --> pi+- pi0             (21.162%).
c
          seccode(1) = sign(pipluscode, pcode)
          seccode(2) = pizerocode
          secmass1   = chpimass
          secmass2   = pizeromass
c
        else
c
c         3 body branches.
c
          nsec = 3
c
          if (branch .lt. 0.90269d0) then
c
c           K+- --> pi+- pi+- pi-+     ( 5.591%).
c
            seccode(1) = sign(pipluscode, pcode)
            seccode(2) = seccode(1)
            seccode(3) = - seccode(1)
            secmass1   = chpimass
            secmass2   = chpimass
            secmass3   = chpimass
c
          else if (branch .lt. 0.95090d0) then
c
c           K+- --> pi0 e+- nu_e       ( 4.821%).
c
            seccode(1) = pizerocode
            seccode(2) = sign(positroncode, pcode)
            seccode(3) = nuecode
            secmass1   = pizeromass
            secmass2   = electronmass
            secmass3   = 0
c
          else if (branch .lt. 0.98270d0) then
c
c           K+- --> pi0 mu+- nu_mu     ( 3.180%).
c
            seccode(1) = pizerocode
            seccode(2) = sign(mupluscode, pcode)
            seccode(3) = numucode
            secmass1   = pizeromass
            secmass2   = muonmass
            secmass3   = 0
c
          else
c
c           K+- --> pi+- pi0 pi0       ( 1.730%).
c
            seccode(1) = sign(pipluscode, pcode)
            seccode(2) = pizerocode
            seccode(3) = pizerocode
            secmass1   = chpimass
            secmass2   = pizeromass
            secmass3   = pizeromass
c
          endif
c
          call threebdecay(penergy, primass, up,
     +                     secmass1, secmass2, secmass3,
     +                     esec, ldu, upsec)
          return
c
        endif
c
      else if (apcode .eq. sigmapluscode) then
c
c       Sigma+ decays.
c
        primass = sigmaplusmass
c
        if (urandom() .lt. 0.5163d0) then
c
c         Sigma+ -->  p pi0   (51.63%)
c
          seccode(1) = sign(protoncode, pcode)
          seccode(2) = pizerocode
          secmass1   = protonmass
          secmass2   = pizeromass
c
        else
c
c         Sigma+ -->  n pi+   (48.37%)
c
          seccode(1) = sign(neutroncode, pcode)
          seccode(2) = sign(pipluscode, pcode)
          secmass1   = neutronmass
          secmass2   = chpimass
c
        endif
c
      else if (apcode .eq. sigmaminusbcode) then
c
c       Sigma- decays.
c
c       Sigma- -->  n pi-   (100%)
c
        primass    = sigmaminusmass
        seccode(1) = sign(neutroncode, -pcode)
        seccode(2) = sign(pipluscode, pcode)
        secmass1   = neutronmass
        secmass2   = chpimass
c
      else if (apcode .eq. ximinusbcode) then
c
c       Xi- decays.
c
c       Xi- -->  Lambda pi-   (100%)
c
        primass    = ximinusmass
        seccode(1) = sign(lambdacode, -pcode)
        seccode(2) = sign(pipluscode, pcode)
        secmass1   = lambdamass
        secmass2   = chpimass
c
      else if (apcode .eq. omegaminusbcode) then
c
c       Omega- decays.
c
        primass = omegaminusmass
        branch  = urandom()
c
        if (branch .lt. 0.678d0) then
c
c         Omega- -->  Lambda K-   (67.8%)
c
          seccode(1) = sign(lambdacode, -pcode)
          seccode(2) = sign(kpluscode, pcode)
          secmass1   = lambdamass
          secmass2   = chkmass
c
        else if (branch .lt. 0.914d0) then
c
c         Omega- -->  Xi0 pi-   (23.6%)
c
          seccode(1) = sign(xi0code, -pcode)
          seccode(2) = sign(pipluscode, pcode)
          secmass1   = xi0mass
          secmass2   = chpimass
c
        else
c
c         Omega- -->  Xi- pi0   (8.6%)
c
          seccode(1) = sign(ximinusbcode, pcode)
          seccode(2) = pizerocode
          secmass1   = ximinusmass
          secmass2   = pizeromass
c
        endif
c
      else
c
c       Error. Particle code not adequate for this routine.
c
        call errprint(2, 'INVD', 4, 'rarechdecay', ' ',
     +                1, pcode, 1, penergy, ' ')
c
      endif
c
      call twobdecay(penergy, primass, up,
     +               secmass1, secmass2, esec, ldu, upsec)
c
      return
      end
c     --- End of routine rarechdecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'heavycharged.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
