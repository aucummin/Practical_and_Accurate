c
c     FILE: stackpcle.f                     Creation date: 13/AUG/1996.
c                                       LAST MODIFICATION: 05/JUL/2002.
c
c     This file contains the routines which deal with the particle
c     stacking. II: Routines with the standard thinning algorithm
c     introduced by A. M. Hillas.
c     These routines also check the particles for low energy,
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacknpclet(icut, np, pcode, penergy, pwt,
     +                       pxyzv, pdepth, ptime, plhdepth,
     +                       ldp, ppxyz, lastol, curral)
c
c     Stacking np particles with global thinning control.
c     Many particle properties (such as position for example) are
c     assumed to be the same for all particles.
c
c     This routine is used only when first thinning the particles. It
c     is not necessary then to check for maximum weights.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2000, 2001,
c                                         2002.
c
c     Arguments:
c     =========
c
c     icut............ (input, integer) Label of cut energies to use:
c                      1 = normal cuts. 2 = zero cuts for decaying
c                      particles.
c     np.............. (input, integer) The number of particles to
c                      stack (save into the corresponding particle
c                      stack.
c     pcode........... (input, integer, array(np)) The particle types.
c     penergy......... (input, double precision, array(np)) The
c                      energies of the particles.
c     pwt,
c     pxyzv(4),
c     pdepth,
c     ptime,
c     plhdepth........ (input, double precision) Particle properties
c                      common to all particles.
c     ldp............. (input, integer) Leading dimension of array
c                      ppxyz
c     ppxyz........... (input, double precision, array(ldp, np))
c                      Directions of motion of the particles.
c     lastol.......... (input, integer, array(2)) Last observing levels
c                      passed by the particles.
c     curral.......... (input, integer) The atmospheric layer the
c                      particles are currently passing through.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'     (Included by 'pstackpar.f')
c     include 'pstackpar.f'     (Included by 'pdata.f')
      include 'pclepar.f'
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      integer           icut, np, ldp
      integer           pcode(np)
      double precision  penergy(np)
      double precision  pxyzv(4)
      double precision  pdepth
      double precision  pwt, ptime, plhdepth
      double precision  ppxyz(ldp, np)
      integer           lastol(2)
      integer           curral
c
c     Particle data storage templates
c
      include 'pdata.f'
c
c     Declaration of shared data.
c
      include 'thincomm.f'
      include 'pstackcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ip, ipcode, prstack
      logical           toplost
      double precision  ecut, ecut1, free
      integer           allstae
c
c     FIRST EXECUTABLE STATEMENT
c
      toplost = (pdepth .le. dtoplost)
c
c     Encoding the invariant part of the record.
c
      prx       = pxyzv(1)
      pry       = pxyzv(2)
      prz       = pxyzv(3)
      prvz      = pxyzv(4)
      prdepth   = pdepth
      prtime    = ptime
      prlhdepth = plhdepth
c
      prlol     = lastol(1)
      prlolsa   = lastol(2)
      prcal     = curral
c
c     Creation time and depth are the current time and depth.
c
      prcdepth = pdepth
      prctime  = ptime
c
      do ip = 1, np
c
c       "Null" (unphysical) particles are eliminated and
c       neutrinos are currently not tracked.
c
        if (pcode(ip) .eq. nullcode) then
c
          call notapcle(penergy(ip), pwt)
c
        else if (allpgroup(6, pcode(ip))) then
c
          nneutrino(2) = nneutrino(2) + pwt
          eneutrino(2) = eneutrino(2) + pwt * penergy(ip)
c
        else
c
c         Determining the stack to use.
c
          ipcode   = pcode(ip)
          prstack  = allpclesta0(ipcode)
c
c         Checking the parameters of the particle.
c
          if (toplost .and. (ppxyz(3, ip) .ge. 0)) then
c
c           The particle is going upwards, and reached higher than the
c           injection altitude: It's a lost particle. Recording and
c           ignoring.
c
            nplost(2, prstack) = nplost(2, prstack) + pwt
            elost(2, prstack)  = elost(2, prstack) +
     +                                  pwt * penergy(ip)
c
          else
c
c           The particle will be stacked only if it has enough
c           energy.
c
c           Completing the record encoding.
c
            prcode   = pcode(ip)
            prenergy = penergy(ip)
            prwt     = pwt
            prpx     = ppxyz(1, ip)
            prpy     = ppxyz(2, ip)
            prpz     = ppxyz(3, ip)
            prpxpr   = ppxyz(1, ip)
            prpypr   = ppxyz(2, ip)
            prpzpr   = ppxyz(3, ip)
c
c           Checking thin and cut energy.
c
            if (ipcode .le. maxpcle) then
              ecut  = cutenergy(ipcode, icut)
              ecut1 = cutenergy(ipcode, 1)
              free  = prenergy + pfreeegy(ipcode)
            else
              ecut  = nucutenergy
              ecut1 = ecut
              free  = prenergy
            endif
c
c           Thinning energy check.
c
            if (free .ge. currethin) then
c
c             Energy is higher than thinning energy. Stacking.
c
c             Normal energy threshold check.
c
              prlowe = .false.
c
              chpsta(allstae(prstack, 1) + 1) = pclerecord
              totpcles(2, prstack) = totpcles(2, prstack) + prwt
c
            else
c
c             Energy is lower than thinning energy.
c
              if (penergy(ip) .le. ecut) then
c
c               The particle is removed because of its low energy.
c
                call pclelowe(prstack, ipcode, penergy(ip), pwt,
     +                        lastol)
c
              else
c
c               Energy cut check passed.
c
c               Stacking conditionally and modifying the weight.
c
c               Updating the global thinning marker.
c
                thinmarker = thinmarker - free
c
                if (thinmarker .le. 0) then
c
c                 Stacking with modified particle weight.
c
                  prwt = prwt * (currethin / free)
c
c                 Refreshing the thinning marker.
c
                  thinmarker = thinmarker + currethin
c
c                 Normal energy threshold check.
c
                  prlowe = (prenergy .le. ecut1)
c
c                 Stacking.
c
                  chpsta(allstae(prstack, 1) + 1) = pclerecord
                  totpcles(2, prstack) = totpcles(2, prstack) + prwt
c
                endif
              endif
            endif
          endif
        endif
c
      enddo
      return
c
      end
c     --- End of routine stacknpclet
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stack1pclek(icut, tfenergy0, fenergy0,
     +                       pcode, penergy, pwt, pxyzv,
     +                       pdepth, ptime, plhdepth, ppxyz,
     +                       lastol, curral, keep)
c
c     Stacking 1 particle emitted by other, the primary particle
c     is not destroyed and therefore must be taken into account in
c     the thinning algorithm. It is assumed that both the primary and
c     secondary particle belong to stacks having the same weight
c     limiting factors.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2001, 2002.
c
c     Arguments:
c     =========
c
c     icut............ (input, integer) Label of cut energies to use:
c                      1 = normal cuts. 2 = zero cuts for decaying
c                      particles.
c     tfenergy0....... (input, double precision) Total free energy.
c     fenergy0........ (input, double precision) Free energy of
c                      "particle 0".
c     pcode........... (input, integer) The particle type.
c     pwt............. (input-output, integer) As input: Original
c                      weight of both particles. As output: Current
c                      weight of "particle 0".
c     penergy,
c     pxyzv(4),
c     pdepth,
c     ptime,
c     plhdepth........ (input, double precision) Particle properties.
c     ppxyz........... (input, double precision, array(3))
c                      Direction of motion of the particle.
c     lastol.......... (input, integer, array(2)) Last observing levels
c                      passed by the particles.
c     curral.......... (input, integer) The atmospheric layer the
c                      particles are currently passing through.
c     keep............ (output, logical) True if particle 0 must be
c                      kept.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'     (Included by 'pstackpar.f')
c     include 'pstackpar.f'     (Included by 'pdata.f')
      include 'pclepar.f'
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      integer           icut
      double precision  tfenergy0, fenergy0
      integer           pcode
      double precision  penergy
      double precision  pxyzv(4)
      double precision  pdepth
      double precision  pwt, ptime, plhdepth
      double precision  ppxyz(3)
      integer           lastol(2)
      integer           curral
      logical           keep
c
c     Particle data storage templates
c
      include 'pdata.f'
c
c     Declaration of shared data.
c
      include 'thincomm.f'
      include 'pstackcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           prstack
      double precision  fenergy, tfenergy, ecut, ecut1, neww0
      integer           allstae
      logical           gothin, stac
      double precision  thinmaxzwg, thinmaxzwp
      double precision  thinmaxzw, thinmaxzdel
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      keep = .true.
c
      if (tfenergy0 .ge. currethin) then
c
c       Stacking the particle with independent thinning.
c
        call stacknpclet(icut, 1, pcode, penergy, pwt, pxyzv,
     +                   pdepth, ptime, plhdepth, 3, ppxyz,
     +                   lastol, curral)
c
      else
c
c       "Null" (unphysical) particles are eliminated and
c       neutrinos are currently not tracked.
c
        if (pcode .eq. nullcode) then
c
          call notapcle(penergy, pwt)
c
        else if (allpgroup(6, pcode)) then
c
          nneutrino(2) = nneutrino(2) + pwt
          eneutrino(2) = eneutrino(2) + pwt * penergy
c
        else
c
c         Determining the stack to use.
c
          prstack = allpclesta0(pcode)
c
c         Checking the parameters of the particle.
c
          if ((ppxyz(3) .ge. 0) .and. (pdepth .le. dtoplost)) then
c
c           The particle is going upwards, and reached higher than the
c           injection altitude: It's a lost particle. Recording and
c           ignoring.
c
            nplost(2, prstack) = nplost(2, prstack) + pwt
            elost(2, prstack)  = elost(2, prstack) +
     +                                  pwt * penergy
c
          else
c
c           The particle will be stacked only if it has enough
c           energy.
c
c           Checking cut energy.
c
            if (pcode .le. maxpcle) then
              ecut    = cutenergy(pcode, icut)
              ecut1   = cutenergy(pcode, 1)
              fenergy = pfreeegy(pcode)
            else
              ecut    = nucutenergy
              ecut1   = ecut
              fenergy = 0
            endif
c
            if (penergy .le. ecut) then
c
c             The particle is removed because of its low energy.
c
              call pclelowe(prstack, pcode, penergy, pwt, lastol)
c
            else
c
c             Energy cut check passed.
c             Checking maximum weight.
c
              thinmaxzwg  = thinmaxwg(prstack)
              thinmaxzwp  = thinmaxwp(prstack)
              thinmaxzw   = thinmaxw(prstack)
              thinmaxzdel = thinmaxdel(prstack)
c
              if (pwt .le. thinmaxzwg) then
                gothin = .true.
              else if (pwt .ge. thinmaxzw) then
                gothin = .false.
              else
                gothin = (pwt - thinmaxzwg) .le.
     +                   (thinmaxzdel * urandom())
              endif
c
              if (gothin) then
c
c               Attempting thinning operation.
c
c               Analysing how large can the new weights be.
c
                fenergy  = fenergy + penergy
                tfenergy = fenergy + fenergy0
                neww0    = pwt * tfenergy
c
                if (neww0 .le. (thinmaxzwp * min(fenergy, fenergy0)))
     +          then
c
c                 The new weights cannot be exceedingly large. It is OK
c                 to thin out one of the particles: Only one particle
c                 will be kept, with probability proportional to its
c                 free energy.
c
                  if ((tfenergy * urandom()) .lt. fenergy) then
c
c                   Emitting the secondary with modified weight and
c                   deleting the primary.
c
                    stac = .true.
                    keep = .false.
                    prwt = neww0 / fenergy
c
                  else
c
c                   Only the primary will be kept, with modified
c                   weight.
c
                    stac = .false.
                    pwt  = neww0 / fenergy0
c
                  endif
c
                else
c
c                 There is risk of obtaining a too large weight: No
c                 thinning is done, and both particles are kept with
c                 unchanged weights.
c
                  stac = .true.
                  prwt = pwt
c
                endif
c
              else
c
c               The original weight is too high. Stopping thinning.
c               Both particles are kept.
c
                stac = .true.
                prwt = pwt
c
              endif
c
c             Stacking the new particle if necessary.
c
              if (stac) then
c
c               Record encoding (The weight has already been set).
c
                prcode    = pcode
                prenergy  = penergy
                prx       = pxyzv(1)
                pry       = pxyzv(2)
                prz       = pxyzv(3)
                prvz      = pxyzv(4)
                prdepth   = pdepth
                prtime    = ptime
                prlhdepth = plhdepth
                prpx      = ppxyz(1)
                prpy      = ppxyz(2)
                prpz      = ppxyz(3)
                prpxpr    = ppxyz(1)
                prpypr    = ppxyz(2)
                prpzpr    = ppxyz(3)
                prlol     = lastol(1)
                prlolsa   = lastol(2)
                prcal     = curral
c
c               Creation time and depth are the current time and depth.
c
                prcdepth = pdepth
                prctime  = ptime
c
c               Normal energy threshold check.
c
                prlowe   = (prenergy .le. ecut1)
c
c               Stacking.
c
                chpsta(allstae(prstack, 1) + 1) = pclerecord
                totpcles(2, prstack) = totpcles(2, prstack) + prwt
c
              endif
            endif
          endif
        endif
      endif
c
      return
c
      end
c     --- End of routine stack1pclek
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacknpcled(icut, primenergy,
     +                       np, pcode, penergy, pwt, pxyzv,
     +                       pdepth, ptime, plhdepth, ldp, ppxyz,
     +                       lastol, curral,
     +                       ntokeep, keptpcle, keptweight, w1)
c
c     Stacking many particles (for example coming from a decay of
c     a "primary" particle). The secondaries are stacked accordingly
c     with thinning rules. Many particle properties (such as position
c     for example) are assumed to be the same for all particles.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2001, 2002.
c
c     Arguments:
c     =========
c
c     icut............ (input, integer) Label of cut energies to use:
c                      1 = normal cuts. 2 = zero cuts for decaying
c                      particles.
c     primenergy...... (input, double precision) The energy of the
c                      primary particle.
c     np.............. (input, integer) The number of particles to
c                      stack (save into the corresponding particle
c                      stack.
c     pcode........... (input, integer, array(np)) The particle types.
c     penergy......... (input, double precision, array(np)) The
c                      particle energies (GeV).
c     pwt,
c     pxyzv(4),
c     pdepth,
c     ptime,
c     plhdepth........ (input, double precision) Particle properties
c                      common to all particles.
c     ldp............. (input, integer) Leading dimension of array
c                      ppxyz
c     ppxyz........... (input, double precision, array(ldp, np))
c                      Directions of motion of the particles.
c     lastol.......... (input, integer, array(2)) Last observing levels
c                      passed by the particles.
c     curral.......... (input, integer) The atmospheric layer the
c                      particles are currently passing through.
c     ntokeep......... (input, integer) If positive and not greater than
c                      np, then all particles in the range [ntokeep, np]
c                      are not going to be stacked when accepted.
c                      Instead, such particles are going to be marked
c                      as accepted. If ntokeep is zero, negative or
c                      greater than np, then all selected secondaries
c                      will be stacked. For simplicity, the algorithm
c                      assumes that all the particles from ntokeep to
c                      np are neither neutrinos nor unphysical
c                      particles.
c     keptpcle........ (output, logical, array(ntokeep)) Used only when
c                      ntokeep is positive and not greater than np.
c                      keptpcle(i) = TRUE (FALSE) means that particle
c                      labelled ntokeep+i-1 has been accepted for
c                      further propagation.
c     keptweight...... (output, double precision, array(ntokeep)) Used
c                      only when ntokeep is positive and not greater
c                      than np. keptweights(i) is the new weight of
c                      the ith accepted particle.
c     w1.............. (sratch, double precision, array(0:np)) Working
c                      space of dimension np + 1.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'     (Included by 'pstackpar.f')
c     include 'pstackpar.f'     (Included by 'pdata.f')
      include 'pclepar.f'
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      integer           icut
      double precision  primenergy
      integer           np, ldp
      integer           pcode(np)
      double precision  penergy(np)
      double precision  pxyzv(4)
      double precision  pdepth
      double precision  pwt, ptime, plhdepth
      double precision  ppxyz(ldp, np)
      integer           lastol(2)
      integer           curral, ntokeep
      logical           keptpcle(np)
      double precision  keptweight(np)
      double precision  w1(0:np)
c
c     Particle data storage templates
c
      include 'pdata.f'
c
c     Declaration of shared data.
c
      include 'thincomm.f'
      include 'pstackcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ip, jp, ipcode, prstack, np1, keepfirst
      logical           nothin, toplost, stac1
      integer           nthsec, ncopy
      double precision  free, ecut, etot, emin, uer, neww, neww0
      double precision  thinmaxzwg, thinmaxzwp
      double precision  thinmaxzw, thinmaxzdel, wfmin
      integer           wfsta
      integer           allstae
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the indices related with the set of particles that must
c     not be stacked in case if selection (only marked). This may be
c     an empty set.
c
      np1 = np
      if ((ntokeep .gt. 0) .and. (ntokeep .le. np)) then 
        np1 = ntokeep - 1
        do ip = ntokeep, np
          keptpcle(ip - np1)   = .true.
          keptweight(ip - np1) = pwt
        enddo
      endif
c
c     Checking if the primary's weight is larger than the maximum
c     allowed. In this case all the secondaries are stacked
c     directly, without thinning.
c
      if (pwt .le. thinmaxwg(currstack)) then
        nothin = .false.
      else if (pwt .ge. thinmaxw(currstack)) then
        nothin = .true.
      else
        nothin = (pwt - thinmaxwg(currstack)) .gt.
     +           (thinmaxdel(currstack) * urandom())
      endif
c
      if (nothin) then
c
        call stacknpcleu(icut, np1, pcode, penergy, pwt,
     +                   pxyzv, pdepth, ptime, plhdepth,
     +                   ldp, ppxyz, lastol, curral, prstack)
c
        return
c
      endif
c
c     All particles with energies greater than the thinning energy
c     will be stacked; and one additional will be selected from the
c     remaining ones.
c
      toplost   = (pdepth .le. dtoplost)
      keepfirst = np1 + 1
c
c     Encoding the invariant part of the record.
c
      prx       = pxyzv(1)
      pry       = pxyzv(2)
      prz       = pxyzv(3)
      prvz      = pxyzv(4)
      prdepth   = pdepth
      prtime    = ptime
      prlhdepth = plhdepth
c
      prlol     = lastol(1)
      prlolsa   = lastol(2)
      prcal     = curral
c
c     Creation time and depth are the current time and depth.
c
      prcdepth = pdepth
      prctime  = ptime
c
c     Discarding neutrinos and unphisical or very low energetic
c     particles, and setting normalized weights.
c
      nthsec = 0
      w1(0)  = 0
      emin   = 5.d35
      wfmin  = 5d35
      wfsta  = 1
c
      do ip = 1, np
c
        w1(ip) = w1(ip - 1)
        ipcode = pcode(ip)
c
c       "Null" (unphysical) particles are eliminated and
c       neutrinos are currently not tracked.
c
        if (ipcode .eq. nullcode) then
c
          call notapcle(penergy(ip), pwt)
c
        else if (allpgroup(6, ipcode)) then
c
          nneutrino(2) = nneutrino(2) + pwt
          eneutrino(2) = eneutrino(2) + pwt * penergy(ip)
c
        else
c
c         Determining the stack to use for this particle.
c
          prstack  = allpclesta0(ipcode)
c
c         Checking the parameters of the particle.
c
          if (toplost .and. (ppxyz(3, ip) .ge. 0)) then
c
c           The particle is going upwards, and reached the injection
c           altitude: It's a lost particle. Recording and ignoring.
c
            nplost(2, prstack) = nplost(2, prstack) + pwt
            elost(2, prstack)  = elost(2, prstack) +
     +                           pwt * penergy(ip)
c
          else
c
c           The particle will be stacked directly if it has enough
c           energy.
c
            if (ipcode .le. maxpcle) then
              ecut = cutenergy(ipcode, icut)
              free = penergy(ip) + pfreeegy(ipcode)
            else
              ecut = nucutenergy
              free = penergy(ip)
            endif
c
c           Checking the particle's energy.
c
            if (free .ge. currethin) then
c
c             The energy is above the thinning threshold: It will be
c             propagated unconditionally, or marked as accepted.
c
              if (ip .lt. keepfirst) then
c
c               Completing the encoding of the particle.
c
                prcode   = pcode(ip)
                prenergy = penergy(ip)
                prwt     = pwt
                prpx     = ppxyz(1, ip)
                prpy     = ppxyz(2, ip)
                prpz     = ppxyz(3, ip)
                prpxpr   = ppxyz(1, ip)
                prpypr   = ppxyz(2, ip)
                prpzpr   = ppxyz(3, ip)
c
c               Normal energy threshold check.
c
                prlowe   = .false.
c
c               Stacking.
c
                chpsta(allstae(prstack, 1) + 1) = pclerecord
                totpcles(2, prstack) = totpcles(2, prstack) + prwt
c
              endif
c
            else if (penergy(ip) .le. ecut) then
c
c             The particle is removed because of its low energy.
c
              if (ip .lt. keepfirst) then
                call pclelowe(prstack, ipcode, penergy(ip), pwt,
     +                        lastol)
              else
                keptpcle(ip - np1) = .false.
              endif
c
            else
c
c             The energy is below the thinning threshold and above
c             the corresponding cut energy. The particle is kept for
c             further processing.
c
              nthsec = nthsec + 1
              w1(ip) = w1(ip) + free
              if (ip .ge. keepfirst) keptpcle(ip - np1) = .false.
              if (free .lt. emin) emin = free
c
              if (thinmaxw(prstack) .lt. wfmin) then
                wfmin = thinmaxw(prstack)
                wfsta = prstack
              endif
c
            endif
          endif
        endif
c
      enddo
c
      if (nthsec .le. 0) return
c
      etot = w1(np)
c
      thinmaxzwg  = thinmaxwg(wfsta)
      thinmaxzwp  = thinmaxwp(wfsta)
      thinmaxzw   = thinmaxw(wfsta)
      thinmaxzdel = thinmaxdel(wfsta)
c
      if (nthsec .le. 1) then
c
c       Just one remaining particle. Stacking it unconditionally.
c
        stac1 = .false.
        prwt  = pwt
c        
      else if (nthsec .le. 3) then
c
c       There is a small number of secondaries. The thinning operation
c       will be performed only if the weights are not too large.
c
        neww0 = pwt * etot
c
        if (neww0 .le. (thinmaxzwp * emin)) then
c
c         The new weights cannot be exceedingly large. It is OK to thin
c         out one of the particles: Only one particle will be kept,
c         with probability proportional to its free energy.
c
c         Selecting the particle to keep.
c
          uer = urandom() * etot
          do ip = 1, np
            jp = ip
            if (uer .lt. w1(ip)) goto 1010
          enddo
 1010     continue
          free = w1(jp) - w1(jp - 1)
c
c         Setting the new weight.
c
          stac1 = .true.
          ncopy = 1
          neww  = neww0 / free
          prwt  = neww
c
        else
c
c         There is risk of obtaining a too large weight: No thinning is
c         done, and all particles are kept with unchanged weights.
c
          stac1 = .false.
          prwt  = pwt
c
        endif
c
      else
c
c       There is a large number of secondaries still not stacked.
c       Thinning must be carried on to avoid an unnecessarly large
c       number of secondaries. The new particle will be stored using
c       multiple entries if necessary.
c
c       Selecting the particle to keep.
c
        uer = urandom() * etot
        do ip = 1, np
          jp = ip
          if (uer .lt. w1(ip)) goto 1020
        enddo
 1020   continue
        free = w1(jp) - w1(jp - 1)
c
        stac1 = .true.
        neww  = pwt * (etot / free)
        ncopy = neww / thinmaxzwp
        ncopy = ncopy + 1
        prwt  = neww / ncopy
c
      endif
c
c     Stacking the secondaries, or marking them for acceptance.
c
      if (stac1) then
c
c       Only one secondary emitted.
c
        if (jp .lt. keepfirst) then
c
c         Encoding the record. The weight was already set.
c
          prcode   = pcode(jp)
          prenergy = penergy(jp)
          prpx     = ppxyz(1, jp)
          prpy     = ppxyz(2, jp)
          prpz     = ppxyz(3, jp)
          prpxpr   = ppxyz(1, jp)
          prpypr   = ppxyz(2, jp)
          prpzpr   = ppxyz(3, jp)
c
c         Normal energy threshold check.
c
          if (prcode .le. maxpcle) then
            prlowe = (prenergy .le. cutenergy(prcode, 1))
          else
            prlowe = .false.
          endif
c
c         Stacking the thinned-in particle.
c
          prstack = allpclesta0(prcode)
          totpcles(2, prstack) = totpcles(2, prstack) + neww
c
          if (ncopy .le. 1) then
            chpsta(allstae(prstack, 1) + 1) = pclerecord
          else
            call stackncopy(ncopy, prstack, pclerecord)
          endif
c
        else
c
c         Marking the particle to be kept. We assume that the need
c         for additional copies never arises here. This can be
c         ensured if the "keep secondary" mode is called only for
c         processes with at most three secondaries.
c
          keptpcle(jp - np1)   = .true.
          keptweight(jp - np1) = neww
c
        endif
c
      else
c
c       Emitting all the remaining secondaries.
c
        do ip = 1, np1
c
          if (w1(ip) .gt. w1(ip - 1)) then
c
c           Encoding the record. The weight was already set.
c
            prcode   = pcode(ip)
            prenergy = penergy(ip)
            prpx     = ppxyz(1, ip)
            prpy     = ppxyz(2, ip)
            prpz     = ppxyz(3, ip)
            prpxpr   = ppxyz(1, ip)
            prpypr   = ppxyz(2, ip)
            prpzpr   = ppxyz(3, ip)
c
c           Normal energy threshold check.
c
            if (prcode .le. maxpcle) then
              prlowe = (prenergy .le. cutenergy(prcode, 1))
            else
              prlowe = .false.
            endif
c
c           Stacking the particle and checking that the weight
c           does not overpass the corresponding maximum.
c
            prstack = allpclesta0(prcode)
            totpcles(2, prstack) = totpcles(2, prstack) + prwt
            if (prwt .le. thinmaxwp(prstack)) then
              chpsta(allstae(prstack, 1) + 1) = pclerecord
            else
              ncopy = prwt / thinmaxwp(prstack)
              ncopy = ncopy + 1
              prwt  = prwt / ncopy
              call stackncopy(ncopy, prstack, pclerecord)
            endif
c
          endif
        enddo
c
        do ip = keepfirst, np
          if (w1(ip) .gt. w1(ip - 1)) keptpcle(ip - np1) = .true.
        enddo
c          
      endif
c
      return
c
      end
c     --- End of routine stacknpcled
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'stackpcle.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
