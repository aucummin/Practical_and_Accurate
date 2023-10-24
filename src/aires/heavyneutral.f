c
c     FILE: heavyneutral.f                  Creation date: 29/AUG/1996.
c                                       LAST MODIFICATION: 14/JUL/2003.
c
c     This file contains the routines which deal with heavy neutral
c     particle processing (pizero, neutron, etc.).
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine heavyntadv(npart, pdatarr)
c
c     Advancing heavy neutral particles and deciding their fates.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2002.
c
c     Arguments:
c     =========
c
c     npart.......... (input, integer) The number of entries to
c                     process.
c     pdatarr........ (input, character*(*), array(npart))
c                     Array containing the particle data.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'modelpar.f'
      include 'hmfppar.f'
c
c     Declaration of arguments.
c
      integer           npart
      character*(*)     pdatarr(npart)
c
c     Particle data storage templates
c
      include 'pdata.f'
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
      include 'modelcomm.f'
      include 'hmfpcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           istack, apcode, imfp
      double precision  mass, mfp, gamf
      double precision  lifedist, lifepath, u
      double precision  path, collpath, tmp1
      logical           apcodelt30
      double precision  urandomt, ppath, hmfplowe
c
c     FIRST EXECUTABLE STATEMENT
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
c       The particles processed in this routine are heavy neutral
c       particles (pizero, neutron, etc.)
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c          psparel(1) is true for decaying particles.
c
c       BEGINNING OF PARTICLE PROCESSING.
c
c       We know that here the particles are not nuclei
c
        mass       = pclemass(prcode)
        apcode     = abs(prcode)
        apcodelt30 = (apcode .lt. neutroncode)
        gamf       = (1 + prenergy / mass)
        prbeta     = sqrt(1 - 1 / (gamf ** 2))
c
c       Low energetic particles are forced to decays when necessary.
c
        if (prlowe) then
          psparel(1) = (apcodelt30 .and. forcelowedecay)
          if (psparel(1)) then
            psparei(0) = 7
          else
            psparei(0) = 9
          endif
          goto 1120
        endif
c
        psparel(1) = .false.
        path       = 5000.d0
c
        if (apcodelt30) then
c
c         The particle is not a neutron. Checking decay.
c
          lifedist = - cpclelife(prcode) * prbeta * gamf
     +                                   * log(urandomt(0.d0))
          lifepath = ppath(lifedist, frecord(ixpx), prdepth,
     +                     frecord(ixx))
c
          if (lifepath .lt. path) then
c
c           Decay occurs before reaching predicted end.
c
            psparei(0) = 7
            psparel(1) = .true.
c
            path = lifepath
c
          endif
c
        else if (prenergy .le. nucollthreshold) then
c
c         Low energy neutrons can undergo elastic collisions. This
c         is treated approximately here. The parameterization for
c         the mean free path comes from a fit to experimental data.
c
          mfp = 732.6807d0 * (prenergy ** 1.16728d0)
c
c         Fate is nuclear collision (elastic branch).
c
          psparei(0) = 8
          psparel(1) = .true.
          path       = - mfp * log(urandomt(0.d0))
c
        endif
c
c       Would nuclear inelastic interaction occur earlier?
c
        if (prenergy .gt. nucollthreshold) then
c
          u = log(prenergy)
c
          if (externalmfp .and. (prenergy .gt. mfpthreshold)) then
c
c           External cross sections.
c           If we are here the particle is either a neutron,
c           a pi-0, a K0 or a Lambda.
c
            imfp = pcode2highemfpset(prcode)
c
            mfp = mfp8ppk(1, imfp) *
     +            (1 + u * (mfp8ppk(2, imfp) +
     +                 u * (mfp8ppk(3, imfp) +
     +                 u * mfp8ppk(4, imfp)))) /
     +            (1 + u * (mfp8ppk(5, imfp) +
     +                 u * (mfp8ppk(6, imfp) +
     +                 u * (mfp8ppk(7, imfp) +
     +                 u * mfp8ppk(8, imfp)))))
c
          else
c
c           Using low energy cross sections
c
            mfp = hmfplowe(prcode, u)
c
          endif
c
          collpath = - mfp * log(urandomt(0.d0))
c
          if (collpath .lt. path) then
c
c           Fate is nucint.
c
            psparei(0) = 8
            psparel(1) = .true.
            path       = collpath
          endif
        endif
c
c       Now the particle is going to be advanced.
c       (No deflection for neutral particles).
c
        call padvance(path, frecord(ixpxpr), prbeta,
     +                prdepth, frecord(ixx), prtime, prlol, prcal,
     +                tmp1)
c
 1120   continue
c
c       Reencoding the patricle data record.
c
        pdatarr(istack) = pclerecord
c
c       End of particle stack loop.
c
      enddo
c
      return
      end
c     --- End of routine heavyntadv
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine heavyntdecay(npart, pdatarr, nrem, remidx)
c
c     Processing the heavy neutral particles that have already been
c     advanced by routine heavyntadv, accordingly with the fates that
c     were set.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2000, 2001,
c                                         2002, 2003.
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
      double precision  costh, etot, freegy, pl, ehalf
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
c       The particles processed in this routine are heavy neutral
c       particles (pizero, neutron, etc.)
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c          psparel(1) is true for decaying particles.
c
c       BEGINNING OF PARTICLE PROCESSING.
c
c       Selecting only those particle actually suffering a
c       modification.
c
        if (psparel(1)) then
c
c         Switching accordigly with the preselected fate.
c
          if (psparei(0) .eq. 7) then
c
c           Fate is decays.
c
            if (prcode .eq. pizerocode) then
c
c             Pi0 decays
c
              etot = prenergy + pizeromass
c
              if (urandom() .lt. 0.9880195d0) then
c
c               Pi0 --> gamma gamma     (98.80195%)
c
                nsec       = 2
c
                costh      = 2 * urandom() - 1
                ehalf      = etot / 2
                esec(1)    = ehalf * (1 + prbeta * costh)
                esec(2)    = etot - esec(1)
                pl         = ehalf * (costh + prbeta)
                wa(1)      = pl / esec(1)
                wa(2)      = (etot * prbeta - pl) / esec(2)
                seccode(1) = gammacode
                seccode(2) = gammacode
c
c               Deflecting and stacking the gamma rays.
c
                call pdeflectr(frecord(ixpx), wa, 3, upsec)
c
              else
c
c               Pi0 --> e+ e- gamma     (1.19805%)
c
                nsec       = 3
                seccode(1) = positroncode
                seccode(2) = electroncode
                seccode(3) = gammacode
c
                call threebdecay(prenergy, pizeromass, frecord(ixpx),
     +                           electronmass, electronmass, 0.d0,
     +                           esec, 3, upsec)
c
              endif
c
            else
c
c             Other decays are not so frequent and are processed in a
c             separate routine.
c
              call rarentdecay(prcode, prenergy, frecord(ixpx),
     +                         nsec, 3, seccode, esec, upsec)
              etot = prenergy + pclemass(prcode)
c
            endif
c
            call stacknpcled(1, etot, nsec, seccode, esec,
     +                       prwt, frecord(ixx), prdepth, prtime,
     +                       prlhdepth, 3, upsec, prlol, prcal,
     +                       0, ktp, prwt, wa)
c
          else
c
c           Fate is nucint.
c
            call hnucoll((abs(prcode) .ge. lambdacode), prcode, prwt,
     +                   prenergy, frecord(ixpx), prlol,
     +                   maxsecpcles, 3,
     +                   nsec, seccode, esec, upsec, secgen, wa)
c
            if (nsec .gt. (maxsecpcles - 20))
     +        call errprint(2, 'NSEC', 4, 'heavyntdecay', ' ',
     +                      1, nsec, 0, 0.d0, ' ')
c
c           Stacking the generated secondaries
c
            call stacknpcled(2, freegy, nsec, seccode, esec,
     +                       prwt, frecord(ixx), prdepth, prtime,
     +                       prdepth, 3, upsec, prlol, prcal,
     +                       0, ktp, prwt, wa)
c
          endif
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
c     --- End of routine heavyntdecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine rarentdecay(pcode, penergy, up,
     +                       nsec, ldu, seccode, esec, upsec)
c
c     Processimg decays of heavy neutral particles (excluding pi0's).
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
c     Switching accordingly with particle code (pi0 is treated
c     separately).
c
      apcode = abs(pcode)
c
      if (pcode .eq. k0scode) then
c
c       K0 short decays.
c
        primass = k0mass
        nsec    = 2
c
        if (urandom() .lt. 0.6861d0) then
c
c         K0s -->  pi+ pi-   (68.61%).
c
          seccode(1) = pipluscode
          seccode(2) = piminuscode
          secmass1   = chpimass
          secmass2   = chpimass
c
        else
c
c         K0s -->  pi0 pi0   (31.39%).
c
          seccode(1) = pizerocode
          seccode(2) = pizerocode
          secmass1   = pizeromass
          secmass2   = pizeromass
c
        endif
c
      else if (pcode .eq. k0lcode) then
c
c       K0 long decays (only the branches with more than 5%
c       probability are considered).
c
        primass = k0mass
        nsec    = 3
        branch  = urandom()
c
        if (branch .lt. 0.3892d0) then
c
c         K0l -->  pi+- e-+ nu_e   (38.92%).
c
          seccode(1) = nuecode
          seccode(2) = pipluscode
          seccode(3) = electroncode
          secmass1   = 0
          secmass2   = chpimass
          secmass3   = electronmass
c
          if (urandom() .lt. 0.5d0) then
            seccode(2) = - seccode(2)
            seccode(3) = - seccode(3)
          endif
c
        else if (branch .lt. 0.6619d0) then
c
c         K0l -->  pi+- mu-+ nu_mu   (27.27%).
c
          seccode(1) = numucode
          seccode(2) = pipluscode
          seccode(3) = muminuscode
          secmass1   = 0
          secmass2   = chpimass
          secmass3   = muonmass
c
          if (urandom() .lt. 0.5d0) then
            seccode(2) = - seccode(2)
            seccode(3) = - seccode(3)
          endif
c
        else if (branch .lt. 0.8739d0) then
c
c         K0l -->  3 pi0   (21.20%).
c
          seccode(1) = pizerocode
          seccode(2) = pizerocode
          seccode(3) = pizerocode
          secmass1   = pizeromass
          secmass2   = pizeromass
          secmass3   = pizeromass
c
        else
c
c         K0l -->  pi0 pi+ pi-   (12.61%).
c
          seccode(1) = pizerocode
          seccode(2) = pipluscode
          seccode(3) = piminuscode
          secmass1   = pizeromass
          secmass2   = chpimass
          secmass3   = chpimass
c
        endif
c
      else if (pcode .eq. etacode) then
c
c       eta decays (only the branches with more than 5%
c       probability are considered).
c
        primass = etamass
        branch  = urandom()
c
        if (branch .lt. 0.4151d0) then
c
c         eta -->  2 gamma      (41.51%).
c
          nsec       = 2
          seccode(1) = gammacode
          seccode(2) = gammacode
          secmass1   = 0
          secmass2   = 0
c
        else if (branch .lt. 0.7546d0) then
c
c         eta -->  3 pi0        (33.95%).
c
          nsec       = 3
          seccode(1) = pizerocode
          seccode(2) = pizerocode
          seccode(3) = pizerocode
          secmass1   = pizeromass
          secmass2   = pizeromass
          secmass3   = pizeromass
c
        else
c
c         eta -->  pi0 pi+ pi-  (24.54%).
c
          nsec       = 3
          seccode(1) = pizerocode
          seccode(2) = pipluscode
          seccode(3) = piminuscode
          secmass1   = pizeromass
          secmass2   = chpimass
          secmass3   = chpimass
c
        endif
c
      else if (apcode .eq. lambdacode) then
c
c       Lambda decays (only the branches with more than 1%
c       probability are considered).
c
        primass = lambdamass
        nsec    = 2
c
        if (urandom() .lt. 0.6409d0) then
c
c         lambda -->  proton + charged pion      (64.09%).
c
          seccode(1) = sign(protoncode, pcode)
          seccode(2) = sign(pipluscode, -pcode)
          secmass1   = protonmass
          secmass2   = chpimass
c
        else
c
c         lambda -->  neutron + pi0              (35.91%).
c
          seccode(1) = sign(neutroncode, pcode)
          seccode(2) = pizerocode
          secmass1   = neutronmass
          secmass2   = pizeromass
c
        endif
c
      else if (apcode .eq. sigma0code) then
c
c       Sigma0 decays.
c
c       Sigma0 -->  Lambda gamma   (100%)
c
        primass    = sigma0mass
        nsec       = 2
        seccode(1) = sign(lambdacode, pcode)
        seccode(2) = gammacode
        secmass1   = lambdamass
        secmass2   = 0
c
      else if (apcode .eq. xi0code) then
c
c       Xi0 decays.
c
c       Xi0 -->  Lambda pi0   (100%)
c
        primass    = xi0mass
        nsec       = 2
        seccode(1) = sign(lambdacode, pcode)
        seccode(2) = pizerocode
        secmass1   = lambdamass
        secmass2   = pizeromass
c
      else
c
c       Error. Particle code not adequate for this routine.
c
        call errprint(2, 'INVD', 4, 'rarentdecay', ' ',
     +                1, pcode, 1, penergy, ' ')
c
      endif
c
      if (nsec .eq. 2) then
        call twobdecay(penergy, primass, up,
     +                 secmass1, secmass2, esec, ldu, upsec)
      else
        call threebdecay(penergy, primass, up,
     +                   secmass1, secmass2, secmass3,
     +                   esec, ldu, upsec)
      endif
c
      return
      end
c     --- End of routine rarentdecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'heavyneutral.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
