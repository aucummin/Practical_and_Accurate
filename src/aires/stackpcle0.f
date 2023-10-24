c
c     FILE: stackpcle0.f                    Creation date: 13/AUG/1996.
c                                       LAST MODIFICATION: 13/JUN/2003.
c
c     This file contains the routines which deal with the particle
c     stacking. I: Fundamental routines.
c     These routines also check the particles for low energy.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacknpcleu(icut, np, pcode, penergy, pwt,
     +                       pxyzv, pdepth, ptime, plhdepth,
     +                       ldp, ppxyz, lastol, curral, nstacked)
c
c     Stacking particles unconditionally, that is without checking
c     the thinning energy.
c     Spare positions are not taken into account.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2000, 2001,
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
c                      stack).
c     pcode........... (input, integer, array(np)) The particle types.
c     penergy......... (input, double precision, array(np)) The
c                      energies of the particles.
c     pwt, pxyzv(4),
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
c     nstacked........ (output, integer) The number of particles that
c                      were actually stacked (not including low energy
c                      particles, neutrinos, etc.).
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
      integer           np, ldp
      integer           pcode(np)
      double precision  penergy(np)
      double precision  pxyzv(4)
      double precision  pdepth
      double precision  pwt, ptime, plhdepth
      double precision  ppxyz(ldp, np)
      integer           lastol(2)
      integer           curral, nstacked
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
      integer           prstack, ip, ipcode, ncopy
      logical           toplost
      double precision  ecut, ecut1
      integer           allstae
c
c     FIRST EXECUTABLE STATEMENT
c
      toplost  = (pdepth .le. dtoplost)
      nstacked = 0
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
          ipcode  = pcode(ip)
          prstack = allpclesta0(ipcode)
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
            elost(2, prstack)  = elost(2, prstack) + pwt * penergy(ip)
c
          else
c
c           The particle will be stacked only if it has enough
c           energy.
c
            if (ipcode .le. maxpcle) then
              ecut  = cutenergy(ipcode, icut)
              ecut1 = cutenergy(ipcode, 1)
            else
              ecut  = nucutenergy
              ecut1 = ecut
            endif
c
            if (penergy(ip) .le. ecut) then
c
c             The particle is removed because of its low energy.
c
              call pclelowe(prstack, ipcode, penergy(ip), pwt, lastol)
c
            else
c
c             Energy cut check passed. Stacking.
c
c             Encoding the record.
c
              prcode   = ipcode
              prenergy = penergy(ip)
              prpx     = ppxyz(1, ip)
              prpy     = ppxyz(2, ip)
              prpz     = ppxyz(3, ip)
              prpxpr   = prpx
              prpypr   = prpy
              prpzpr   = prpz
c
c             Normal energy threshold check.
c
              prlowe = (prenergy .le. ecut1)
c
              if (pwt .le. thinmaxwp(prstack)) then
                prwt  = pwt
                chpsta(allstae(prstack, 1) + 1) = pclerecord
              else
                ncopy = pwt / thinmaxwp(prstack)
                ncopy = ncopy + 1
                prwt  = pwt / ncopy
                call stackncopy(ncopy, prstack, pclerecord)
              endif
c
              totpcles(2, prstack) = totpcles(2, prstack) + pwt
              nstacked             = nstacked + 1
c
            endif
          endif
        endif
c
      enddo
c
      return
c
      end
c     --- End of routine stacknpcleu
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stackncopy(ncopy, stacknumber, pclerecord)
c
c     Stacking ncopy copies of a given particle record in a given
c     particle stack.
c
c     Written by: S. J. Sciutto, La Plata 2001, 2003.
c
c     Arguments:
c     =========
c
c     ncopy........... (input, integer) The number of copies to
c                      stack.
c     stacknumber..... (input, integer) Stack number.
c     pclerecord...... (input, character*(*)) Particle record to
c                      use for the copies.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'     (Included by 'pstackpar.f')
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           ncopy, stacknumber
      character*(*)     pclerecord
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ip, jp, scopy, ptr1, stchunk
      integer           allstae
c
c     FIRST EXECUTABLE STATEMENT
c
      stchunk = freestaentr(2, stacknumber)
      scopy   = ncopy
      do ip = 1, ncopy - stchunk, stchunk
        ptr1 = allstae(stacknumber, stchunk)
        do jp = 1, stchunk
          chpsta(ptr1 + jp) = pclerecord
        enddo
        scopy = scopy - stchunk
      enddo
      if (scopy .le. 0) return
      ptr1 = allstae(stacknumber, scopy)
      do jp = 1, scopy
        chpsta(ptr1 + jp) = pclerecord
      enddo
c
      return
      end
c     --- End of routine stackncopy
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine notapcle(penergy, pwt)
c
c     "Stacking" a unphysical particle.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     penergy......... (input, double precision) Energy of the
c                      "particle".
c     pwt............. (input, double precision) Its weight.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      double precision  penergy, pwt
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      nnotap(2) = nnotap(2) + pwt
      enotap(2) = enotap(2) + pwt * penergy
c
      return
c
      end
c     --- End of routine notapcle
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'stackpcle0.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
