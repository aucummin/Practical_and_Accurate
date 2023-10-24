c
c     FILE: gammarays.f                     Creation date: 14/AUG/1996.
c                                       LAST MODIFICATION: 06/MAR/2002.
c
c     This file contains the routines which deal with gamma ray
c     processing.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine gammaadv(npart, pdatarr)
c
c     Advancing gamma rays and deciding their fates.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2000, 2002.
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
      include 'pclecodes.f'
      include 'hdatapar.f'
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
      include 'modelcomm.f'
      include 'showercomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           istack, i
      double precision  path, e1, e2, fre1, fre2
      double precision  ppcrsn, compcrsn, phpicrsn
      double precision  totcrsn, utotcrsn, totcrsn2
      double precision  v, vv, vvv, v22, s, g, p, b, c, f, u
      double precision  vmin, logvmin, tmp1
      double precision  fa4, fb4, screen
      logical           photozone, goon
      double precision  upsec0(3, 2), ct12(2), ct1, ct2
      equivalence       (ct12(1), ct1), (ct12(2), ct2)
      double precision  urandomt, urandom
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
c       In this routine prcode should be "gammacode" (gamma photon)
c       for all particles.
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c          psparef(0) is the energy of secondary 1.
c          psparef(1) is the energy of secondary 2.
c          psparef(2) is cos(deflection angle) for secondary 1.
c          psparef(3) is cos(deflection angle) for secondary 2.
c
c       For compton:
c
c          psparef(2) \
c          psparef(3)  > Unitary vector marking the direction of the
c          psparef(4) /  emerging electron.
c
c       BEGINNING OF PARTICLE PROCESSING.
c
c       Updating the previous-to-current direction.
c
        prpxpr = prpx
        prpypr = prpy
        prpzpr = prpz
c
c       Evaluating the cross sections.
c
        photozone = (prenergy .lt. photoelec0)
c
        if ((prenergy .ge. grough) .or. photozone) then
c
          if (photozone) then
c
c           The particle is in the zone of large photoelectric effect
c           and negligible pair production.
c
            ppcrsn = photoelrate / (prenergy ** 3)
c
          else if (prenergy .ge. egyppifty) then
c
c           The cross section is approximately constant for very large
c           energies.
c
            ppcrsn = ppcrsnifty
c
          else if (prenergy .gt. (3.5d0 * electronmass)) then
c
c           The following approximate expression for lower energies
c           needs corrections that will be applied later.
c
            ppcrsn = spconst * log(prenergy) + spconst0
c
          else
c
c           The theoretical threshold for pair production is twice the
c           electron rest mass, but screening makes negligible the
c           cross section for gamma energies up to 3.5 me.
c
            ppcrsn = 0.0d0
c
          endif
c
c         Compton effect.
c
          vmin     = electronmass / (electronmass + 2 * prenergy)
          logvmin  = log(vmin)
          compcrsn = compf0 * logvmin / prenergy
c
c         Photopion production.
c         The threshold for this process is 160 MeV. It can be set to a
c         rather high value to artificially switch off these processes.
c
          if (prenergy .le. gacollthreshold) then
            phpicrsn = 0
          else
c
c           Low energy cross sections are consistents with experimental
c           data (Baldini et. al (1988)).
c
            if (prenergy .ge. 14.772d0) then
              phpicrsn = 5.9d-5
            else if (prenergy .gt. 0.3232d0) then
              phpicrsn = 5.8162d-5 + 1.2372d-5 / prenergy
            else
              phpicrsn = 5.9095d-4 * (prenergy - 0.160d0)
            endif
c
            if (prenergy .lt. 0.45d0) then
              phpicrsn = phpicrsn + 1.69d-4 /
     +                 (1 + (15.0d0 * (prenergy - 0.3232d0)) ** 2)
            else if (prenergy .lt. 0.7877d0) then
              phpicrsn = phpicrsn + 7.32d-5 /
     +                 (1 + (7.50d0 * (0.7877d0 - prenergy)) ** 3)
            else if (prenergy .lt. 2.5d0) then
              phpicrsn = phpicrsn + 7.32d-5 /
     +                 (1 + (4.10d0 * (prenergy - 0.7877d0)) ** 2)
            else if (prenergy .gt. 200.0d0) then
              v = log(prenergy) - 5.298317367d0
              s = 1 + v * (0.0273d0 + v * 0.01d0)
              phpicrsn = phpicrsn * s
            endif
c
          endif
c
          totcrsn2 = compcrsn + phpicrsn
          totcrsn  = totcrsn2 + ppcrsn
          utotcrsn = 1.d0 / totcrsn
c
          path = 0
c
c         Loop to determine gamma's fate.
c
 1010     continue
c
c         Resetting the path (our z axis points upwards).
c
          path = path - utotcrsn * log(urandomt(0.d0))
c
c         No special coding for the cosine ct1 and ct2 needed.
c
          g    = urandom() * totcrsn
          if (g .lt. phpicrsn) then
c
c           Fate = nucint or decays
c
            if (prenergy .lt. 2.5d0) then
              psparei(0) = 7
            else
              psparei(0) = 8
            endif
c
c           No deflection here, and the angles are meaningless.
c
          else if (g .lt. totcrsn2) then
c
c           Fate = compton
c
c           The photon remains and one electron is created.
c
            psparei(0) = 1
c
            v       = exp(urandom() * logvmin)
            e1      = prenergy * v
            e2      = prenergy - e1
            ct12(1) = 1 + electronmass * (v - 1) / e1
c
c           Avoiding producing compton recoil electron below 3 keV.
c
            if (e2 .lt. 3.0d-6) then
              goto 1010
            else
              g = 0.5d0 * (1 + v * (v + ct1 ** 2 - 1))
              if (urandom() .gt. g) then
                goto 1010
              else
                fre2    = e2
                fre1    = e1
                p       = sqrt((electronmass + e2) ** 2 - sqremass)
                ct12(2) = (prenergy - e1 * ct1) / p
              endif
            endif
c
c           Deflecting the photon and evaluating the direction of
c           the emerging electron.
c
            call pdeflectr(frecord(ixpxpr), ct12, 3, upsec0)
c
            do i = 1, 3
              frecord((ixpx - 1) + i) = upsec0(i, 1)
              psparef(1 + i)          = upsec0(i, 2)
            enddo
c
c           The new electron will be stacked later.
c
          else if (photozone) then
c
c           Fate = photoelectric
c
            psparei(0) = 2
c
            e2   = prenergy
            fre2 = prenergy
c
c           Approximate evaluation of the deflection angle.
c
            psparef(2) = 1 - 27d-6 / (prenergy + 15d-6)
c
c           The photon will be eliminated from the stack.
c           A new electron will be stacked.
c
          else
c
c           Fate = pairproduction.
c
            psparei(0) = 3
c
c           Evaluating the total energy of the secondaries.
c           Taking into account that they are electrons and so their
c           energies must be greater than the rest mass.
c
            vmin   = twoemass / prenergy
            v      = urandomt(vmin)
            vmin   = vmin / 2
            v      = v - vmin
            vv     = 1 - v
            vvv    = v * vv
            v22    = 1 - 2 * vvv
c
c           Screening corrections accordingly with Rossi's book
c           (Bethe-Heitler theory).
c
            screen = screenfac / (prenergy * vvv)
c
            if (screen .lt. 2.0d0) then
              fa4 = 5.21d0 - screen * (0.98d0 - 0.158d0 * screen)
              fb4 = fa4 - 0.168d0 * exp(-4 * screen)
              g   = v22 * (fa4 - thirdlogz) +
     +              0.66666666666667d0 * vvv * (fb4 - thirdlogz)
            else
              c   = 1.d0 /
     +              (screen * (2.281d0 + 0.025d0 * screen ** 2))
              g   = log(vvv / vmin) - c + 0.1931471806d0
              g   = g * (v22 + 0.66666666666667d0 * vvv)
            endif
c
            if ((zlogradlen * ppcrsn * urandom()) .gt. g) then
              goto 1010
            else
c
              e1 = v  * prenergy
              e2 = vv * prenergy
c
c             Correction for Landau-Pomeranchuk-Migdal effect.
c
              if (prenergy .gt. lpmthreshold) then
                call lpmeffect(prenergy, e1, e2,
     +                         prdepth - path * currshdir(3),
     +                         .true., goon)
                if (.not. goon) goto 1010
              endif
c
c             End of processing lpm effect.
c
c             Assigning the secondaries. Now completely asymmetric
c             pairs are accepted. In such cases the high energy
c             secondary is emitted in the direction of the primary.
c
              e1   = e1 - electronmass
              e2   = e2 - electronmass
              fre2 = e2
              fre1 = prenergy - fre2
c
c             Particle 1 (2) is positron (electron).
c
              if ((e1 .le. 5.d-6) .or. (e2 .le. 5.d-6)) then
c
                ct1 = 1
                ct2 = 1
c
              else
c
c               Calculating the emission angles for e- and e+
c
                f   = (0.5d0 - vvv) / (0.9d0 - 1.2d0 * vvv)
                g   = f * (electronmass / e2)  ** 2
                b   = 2 * urandom() / g
                b   = b + 4 / (1 + 3 / b)
                ct2 = 1 + (g * b - 2) / (1 + b)
                u   = fre1 - electronmass
                g   = f * (electronmass / u)  ** 2
                b   = 2 * urandom() / g
                b   = b + 4 / (1 + 3 / b)
                ct1 = 1 + (g * b - 2) / (1 + b)
c
              endif
c
              psparef(2) = ct1
              psparef(3) = ct2
c
            endif
c
c           Deflection of the electrons will be done later.
c
          endif
c
        else
c
c         The energy is less than the "grough" threshold. Approximate
c         treatment forcing photoelectric effect.
c
          psparei(0) = 2
c
          e2   = prenergy
          fre2 = prenergy
c
c         (no deflection here)
c
          psparef(2) = 1
c
c         The path is set to a given, fixed value.
c
          path = 18.d0
c
        endif
c
c       Now the gamma ray is going to be advanced.
c       (Notice that gammas travel at the speed of light: beta = 1).
c
        call padvance(path, frecord(ixpxpr), 1.d0,
     +                prdepth, frecord(ixx), prtime, prlol, prcal,
     +                tmp1)
c
c       Setting the remaining spare or internal fields.
c
        prbeta     = 1
        psparef(0) = e1
        psparef(1) = e2
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
c     --- End of routine gammaadv
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine gammadecay(npart, pdatarr, nrem, remidx)
c
c     Processing the gamma photons that have already been advanced
c     by routine gammaadv, accordingly with the fates that were
c     set.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2000, 2001.
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
      include 'modelcomm.f'
      include 'modelcomm2.f'
c
c     Declaration of internal variables and arrays.
c
      integer           istack, idummy
      double precision  mmm, mm2, pre0
      integer           pq0, pq1, pq2, pg2
      logical           pc3
      double precision  x, mlo, mhi, mfb, cp1, sp1
      logical           ktp
      double precision  urandom
c
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
c       In this routine prcode should be 1 (gamma photon) for all
c       particles.
c
c       The following spare fields are used:
c
c          psparei(0) is a code for the particle fate.
c          psparef(0) is the energy of secondary 1.
c          psparef(1) is the energy of secondary 2.
c          psparef(2) is cos(deflection angle) for secondary 1.
c          psparef(3) is cos(deflection angle) for secondary 2.
c
c       For compton:
c
c          psparef(2) \
c          psparef(3)  > Unitary vector marking the direction of the
c          psparef(4) /  emerging electron.
c
c       BEGINNING OF PARTICLE PROCESSING.
c
c       Switching accordigly with the preselected fate.
c
        if (psparei(0) .eq. 1) then
c
c         Fate is compton.
c         The gamma ray stands in the stack (with less energy)
c         and one electron is emitted.
c
c         Stacking the electron.
c         The spare fields 2, 3 and 4 represent the direction
c         of the emerging electron.
c
          call stack1pclek(1, prenergy, psparef(0),
     +                     electroncode, psparef(1),
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prlhdepth, psparef(2), prlol, prcal, ktp)
c
c         Modifying the gamma's energy.
c
          prenergy = psparef(0)
c
c         Reencoding the patricle data record if the particle must
c         remain in the stack.
c
          if (ktp) then
            nrem            = nrem + 1
            remidx(nrem)    = istack
            pdatarr(istack) = pclerecord
          endif
c
        else if (psparei(0) .eq. 2) then
c
c         Fate is photoelectric.
c         The gamma ray is deleted and one electron is stacked, with
c         no thinning. The electron is previously deflected.
c
          call deflectr(frecord(ixpxpr), psparef(2), psparef(3),
     +                  cp1, sp1, frecord(ixpx))
c
          call stacknpcleu(1, 1, electroncode, psparef(1),
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prlhdepth, 3, frecord(ixpx), prlol, prcal,
     +                     idummy)
c
        else if (psparei(0) .eq. 3) then
c
c         Fate is pair production.
c         The gamma ray is deleted and e+ and e- are stacked.
c
c         Deflecting and stacking the electrons.
c         The spare fields 2 and 3 represent ct1 and ct2.
c
          call pdeflectr(frecord(ixpx), psparef(2), 3, upsec)
c
          seccode(1) = positroncode
          seccode(2) = electroncode
c
          call stacknpcled(2, prenergy, 2, seccode, psparef(0),
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prlhdepth, 3, upsec, prlol, prcal,
     +                     0, ktp, prwt, wa)
c
        else if (psparei(0) .eq. 7) then
c
c         Fate is decays
c
c         Photonuclear resonance formed.
c         The energy threshold for this process is 160 MeV,
c         so here prenergy is greater than 0.160 (GeV).
c
          mmm  = sqrt(avnucmass * (avnucmass + 2 * prenergy))
          pre0 = prenergy + avnucmass - mmm
c
          if (mmm .gt. 1.375d0) then
c
            if (urandom() .lt. (9.d-1 * (mmm - 1.050d0))) then
c
c             3 body decay via intermediate fireball.
c             The "fireball" will not be stacked. It will be
c             processed here.
c
              pq0 = urandom() + 0.5d0
c
              if (urandom() .lt. 0.5d0) then
c
c               Decay to nucleon + fireball.
c
                pg2 = gnppbar
                pc3 = .false.
                pq2 = urandom() + 0.5d0
                mlo = 2 * chpimass + 1.1d-3
c
              else
c
c               Decay to pion + fireball.
c
                pg2 = gpion
                pc3 = .true.
                pq2 = 3 * urandom()
                pq2 = pq2 - 1
                mlo = chpimass + avnucmass + 1.1d-3
c
              endif
c
c             Selecting a random number with distribution
c             p(x) = 6 * x * (1 - x)  (0 < x < 1).
c
 1110         continue
              x = sqrt(urandom())
              if (urandom() .lt. x) goto 1110
c
c             Setting parameters of first particle and
c             evaluating kinematics of first decay.
c
              seccode(3) = gengrppcle(pq2, pg2)
c
              mm2 = pclemass(seccode(3))
              mhi = mmm - mm2 - 1.1d-3
              mfb = mlo + x * (mhi - mlo)
c
              call twobdecay(pre0, mmm, frecord(ixpx), mm2, mfb,
     +                       esec(3), 3, upsec(1, 3))
c
c             Evaluation of free energies will be done
c             when processing the respective secondaries.
c
c             Fireball decaying.
c
              pq1 = pq0 - pq2
              pq0 = pq1
c
              if (pc3) then
c
c               Fireball generates nucleon + pion
c
                if (pq0 .eq. -1) then
                  pq1 = 0
                else if (pq0 .eq. 2) then
                  pq1 = 1
                else
                  pq1 = urandom() + 0.5d0
                endif
c
c               Setting the second secondary (nucleon).
c
                seccode(1) = gengrppcle(pq1, gnppbar)
c
              else
c
c               Fireball generates pion + pion
c
                pq1 = 0
                if (pq0 .eq. 0) then
                  if (urandom() .lt. 0.75d0) pq1 = 1
                endif
c
c               Setting the second secondary (pion).
c
                seccode(1) = gengrppcle(pq1, gpion)
c
              endif
c
c             Setting the third secondary (pion).
c
              pq2 = pq0 - pq1
c
              seccode(2) = gengrppcle(pq2, gpion)
c
c             Evaluating kinematics of second decay.
c
              call twobdecay(esec(4), mfb, upsec(1, 4),
     +                       pclemass(seccode(1)),
     +                       pclemass(seccode(2)),
     +                       esec, 3, upsec)
c
              nsec = 3
c
            else
c
c             2 body decay of initial state to nucleon + pion.
c
              pq1 = 3 * urandom()
              if (pq1 .eq. 1) then
                pq2 = 0
              else if (pq1 .eq. 2) then
                pq2 = 1
              else
                pq2 = urandom() + 0.5d0
              endif
c
              seccode(1) = gengrppcle(pq1, gpion)
              seccode(2) = gengrppcle(pq2, gnppbar)
c
c             Evaluating kinematics of two body decay.
c
              call twobdecay(pre0, mmm, frecord(ixpx),
     +                       pclemass(seccode(1)),
     +                       pclemass(seccode(2)),
     +                       esec, 3, upsec)
c
              nsec = 2
c
            endif
c
          else
c
c           Low energetic photons decay to pion + nucleon.
c
c           Selecting the meson's charge.
c
            if (prenergy .gt. 0.230d0) then
c
              pq1 = urandom() + 0.3333333333d0
c
              if (urandom() .lt. avzovera) then
                pq2 = 1 - pq1
              else
                pq2 =   pq1
                pq1 = - pq1
              endif

            else
c
c             Near theshold the direct channel dominates.
c
              if (urandom() .lt. avzovera) then
                pq1 =  1
                pq2 =  0
              else
                pq1 = -1
                pq2 =  1
              endif
c
            endif
c
            nsec = 2
            seccode(1) = gengrppcle(pq1, gpion)
            seccode(2) = gengrppcle(pq2, gnppbar)
c
c           Evaluating kinematics of two body decay.
c
            call twobdecay(pre0, mmm, frecord(ixpx),
     +                     pclemass(seccode(1)),
     +                     pclemass(seccode(2)),
     +                     esec, 3, upsec)
c
          endif
c
c         Stacking the generated secondaries
c
          call stacknpcled(2, prenergy, nsec, seccode, esec,
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prdepth, 3, upsec, prlol, prcal,
     +                     0, ktp, prwt, wa)
c
        else if (psparei(0) .eq. 8) then
c
c         Fate is nucint.
c
          call gnucoll(prwt, prenergy, frecord(ixpx), prlol,
     +                 maxsecpcles, 3,
     +                 nsec, seccode, esec, upsec, secgen, wa)
c
          if (nsec .gt. (maxsecpcles - 20))
     +      call errprint(2, 'NSEC', 4, 'gammadecay', ' ',
     +                    1, nsec, 0, 0.d0, ' ')
c
c         Stacking the generated secondaries
c
          call stacknpcled(2, prenergy, nsec, seccode, esec,
     +                     prwt, frecord(ixx), prdepth, prtime,
     +                     prdepth, 3, upsec, prlol, prcal,
     +                     0, ktp, prwt, wa)
c
        endif
c
      enddo
c
      return
c
      end
c     --- End of routine gammadecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'gammarays.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
