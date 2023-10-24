c
c     FILE: stackpcle_noth.f                Creation date: 01/MAY/2003.
c                                       LAST MODIFICATION: 04/JUN/2003.
c
c     This file contains the routines which deal with the particle
c     stacking. II: Routines that normally deal with thinning, but
c     adapted to impose no sampling of any kind.
c     These routines also check the particles for low energy,
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacknpclet(icut, np, pcode, penergy, pwt,
     +                       pxyzv, pdepth, ptime, plhdepth,
     +                       ldp, ppxyz, lastol, curral)
c
c     Stacking np particles.
c     Many particle properties (such as position for example) are
c     assumed to be the same for all particles.
c
c     Written by: S. J. Sciutto, La Plata 2003.
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
      include 'pstackcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ip, ipcode, prstack
      logical           toplost
      double precision  ecut, ecut1
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
            else
              ecut  = nucutenergy
              ecut1 = ecut
            endif
c
c           Low energy check.
c
            if (prenergy .gt. ecut) then
c
c             Energy is higher than cut energy. Stacking.
c
c             Normal energy threshold check.
c
              prlowe = (prenergy .le. ecut1)
c
              chpsta(allstae(prstack, 1) + 1) = pclerecord
              totpcles(2, prstack) = totpcles(2, prstack) + prwt
c
            else
c
c             The particle is removed because of its low energy.
c
              call pclelowe(prstack, ipcode, penergy(ip), pwt,
     +                      lastol)
c
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
c     Stacking 1 particle emitted by other.
c
c     Written by: S. J. Sciutto, La Plata 2003.
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
c     FIRST EXECUTABLE STATEMENT
c
      keep = .true.
c
      call stacknpclet(icut, 1, pcode, penergy, pwt, pxyzv,
     +                 pdepth, ptime, plhdepth, 3, ppxyz,
     +                 lastol, curral)
c
      return
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
c     a "primary" particle).
c
c     Written by: S. J. Sciutto, La Plata 2003.
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
c     Declaration of internal variables and arrays.
c
      integer           ip, np1
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
c     Stacking the other particles.
c
      call stacknpclet(icut, np1, pcode, penergy, pwt,
     +                 pxyzv, pdepth, ptime, plhdepth,
     +                 ldp, ppxyz, lastol, curral)
c
      return
c
      end
c     --- End of routine stacknpcled
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'stackpcle_noth.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
