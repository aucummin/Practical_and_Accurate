c
c     FILE: kernel3.f                       Creation date: 04/JAN/1997.
c                                       LAST MODIFICATION: 03/APR/2003.
c
c     This file contains the simulation-steering routines, third part:
c     Longitudinal development monitoring routines.
c
c     Basic version: No longitudinal compressed file(s) defined.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stackmoni1(lastpartic0, stacknumber, lastpartic)
c
c     Evaluating the status of processed particles (I):
c     Longitudinal development, ground level reaching, particle
c     losses, etc.
c     Also, the fast particle stack is recompressed.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2000, 2002,
c                                         2003.
c
c     Arguments:
c     =========
c
c     lastpartic0..... (input, integer) Position of the last particle
c                      in the sequential part of the fast stack.
c     stacknumber..... (input, integer) The number of the stack to
c                      process.
c     lastpartic...... (output, integer) The position of the last
c                      particle in the sequential part of the
c                      recompressed stack.
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
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           lastpartic0, stacknumber
      integer           lastpartic
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pstackcomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
      integer           ipcode
      integer           oldlol, newlol, minlol, maxlol
      integer           histoi, iobl
      double precision  wt1, wte
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scanning the sequential part of the corresponding stack.
c
      lastpartic = stabeg0(stacknumber)
c
      do i = stabeg(stacknumber), lastpartic0
c
        ipcode = ipsta(ixpcode, i)
c
c       Analysing new particle position and histogram recording.
c
c       Part I: Horizontal observing levels.
c
        newlol = obslevca * fpsta(ixdepth, i) + obslevcb
        newlol = max(0, min(newlol, totobslev))
        oldlol = ipsta(ixlol, i)
c
        if (newlol .ne. oldlol) then
c
c         The particle has crossed one or more observing levels.
c
          ipsta(ixlol, i) = newlol
c
          if (newlol .gt. oldlol) then
            minlol = oldlol
            maxlol = newlol
          else
            minlol = newlol
            maxlol = oldlol
          endif
c
          histoi = allpclelh(ipcode)
          wt1    = fpsta(ixwt, i)
          wte    = wt1 * fpsta(ixenergy, i)
c
          do iobl = minlol + 1, maxlol
            wlhistn(2, iobl, histoi) = wlhistn(2, iobl, histoi) + 1
            lhistn(2, iobl, histoi)  = lhistn(2, iobl, histoi)  + wt1
            lhiste(2, iobl, histoi)  = lhiste(2, iobl, histoi)  + wte
          enddo
        endif
c
c       Part II: Planes othogonal to the shower axis.
c
        oldlol = ipsta(ixlolsa, i)
        call salastol(fpsta(ixx, i), oldlol, newlol)
c
        if (newlol .ne. oldlol) then
c
c         The particle has passed across one or more observing levels.
c
          ipsta(ixlolsa, i) = newlol
c
          if (newlol .gt. oldlol) then
            minlol = oldlol
            maxlol = newlol
          else
            minlol = newlol
            maxlol = oldlol
          endif
c
          histoi  = allpclelh(ipcode) + mxlhtable
          wt1     = fpsta(ixwt, i)
c
c         No energy tables evaluated by now.
c
          do iobl = minlol + 1, maxlol
            wlhistn(2, iobl, histoi) = wlhistn(2, iobl, histoi) + 1
            lhistn(2, iobl, histoi)  = lhistn(2, iobl, histoi)  + wt1
          enddo
        endif
c
c       Analysing if the ground level has been reached.
c
        if (fpsta(ixdepth, i) .ge. groundepth) then
c
c         The particle reached ground, it is processed and deleted from
c         the stack.
c
          call pcleground(stacknumber, chpsta(i))
c
        else if ( (fpsta(ixdepth, i) .le. 0)
     +            .or.
     +            ((fpsta(ixpz, i) .ge. 0) .and.
     +             (fpsta(ixdepth, i) .le. dtoplost))
     +            .or.
     +            ((fpsta(ixpx, i) .gt. 0) .and.
     +             (fpsta(ixx, i) .gt. hrmaxlx))
     +            .or.
     +            ((fpsta(ixpx, i) .lt. 0) .and.
     +             (fpsta(ixx, i) .lt. hrminlx))
     +            .or.
     +            ((fpsta(ixpy, i) .gt. 0) .and.
     +             (fpsta(ixy, i) .gt. hrmaxly))
     +            .or.
     +            ((fpsta(ixpy, i) .lt. 0) .and.
     +             (fpsta(ixy, i) .lt. hrminly))
     +            .or.
     +            (fpsta(ixz, i) .lt. zcglobmin)
     +           ) then
c
c
c         The particle is going upwards, and reached "higher" than the
c         top of the atmosphere, or is exiting the shower bounding box:
c         It's a lost particle, and is deleted from the stack.
c
          wt1 = fpsta(ixwt, i)
          wte = wt1 * fpsta(ixenergy, i)
c
          nplost(2, stacknumber) = nplost(2, stacknumber) + wt1
          elost(2, stacknumber)  = elost(2, stacknumber)  + wte
c
        else
c
c         The particle remains in the stack.
c
          lastpartic = lastpartic + 1
          chpsta(lastpartic) = chpsta(i)
c
        endif
c
      enddo
c
      return
c
      end
c     --- End of routine stackmoni1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine lgtinit(ipcode, charge, chpdata, fpdata, geosw)
c
c     Initialization routine that must be called before starting
c     the processing of a particle which is tracked with the help of
c     routine "lgtmaster". At the same time, this routine initializes
c     internal geomagnetic field variables.
c
c     Note: This subroutine should be used ONLY for those stacks
c     processed with routine adscansta0.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998.
c
c     Arguments:
c     =========
c
c     ipcode.......... (input, integer) Particle code.
c     charge.......... (input, integer) Particle charge.
c     chpdata......... (input, character*(*)) Particle record.
c     fpdata.......... (input, double precision, array(maxstalen))
c                      Array containing the floating point data of the
c                      corresponding particle. The elements of this
c                      array are the same as the fields in the
c                      particle stack array.
c     geosw........... (output, logical) True if the geomagnetic field
c                      is enabled. False otherwise.
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
      integer           ipcode, charge
      character*(*)     chpdata
      double precision  fpdata(1)
      logical           geosw
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the geomagnetic switch.
c
      geosw = shgbon
c
c     Evaluating the product of the charge and the magnetic field.
c
      if (geosw) then
        facgbx = charge * shgbx
        facgbz = charge * shgbz
      endif
c
      return
c
      end
c     --- End of routine lgtinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine lgtmaster(ipcode, path, mass, lossrate, cutegy,
     +                     chpdata, fpdata, plol, pcal, plowe,
     +                     done, lost, lowe)
c
c     Evaluating the status of a particle being processed.
c     Longitudinal development, ground level reaching, particle
c     losses, energy losses. The particle is also advanced the
c     specified path.
c
c     Note this subroutine should be used ONLY for those stacks
c     processed with routine adscansta0.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2001, 2002, 2003.
c
c     Arguments:
c     =========
c
c     ipcode.......... (input, integer) Particle code.
c     path............ (input-output, double precision) The path to
c                      advance the particle (g/cm2). Can be modified
c                      for very low energetic particles.
c     mass............ (input, double precision) Particle mass.
c     lossrate........ (input, double precision) Energy loss rate, in
c                      GeV/(g/cm2).
c     cutegy.......... (input, double precision) Cut energy for the
c                      particle (in GeV).
c     chpdata......... (input, character*(*)) Particle record.
c     fpdata.......... (input, double precision, array(maxstalen))
c                      Array containing the floating point data of the
c                      corresponding particle. The elements of this
c                      array are the same as the fields in the
c                      particle stack array.
c     plol............ (input-output, integer, array(2)) Internal
c                      particle data.
c     pcal............ (input-output, integer) Internal particle data.
c     plowe........... (input-output, logical) Internal particle data.
c     done............ (output, logical) If true then the particle
c                      is "done" (lost, low energy or reached ground).
c     lost............ (output, logical) True if particle is lost.
c                      False otherwise.
c     lowe............ (output, logical) True if particle has low
c                      energy. False otherwise.
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
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           ipcode
      double precision  path, mass, lossrate, cutegy
      character*(*)     chpdata
      double precision  fpdata(maxstalen)
      integer           plol(2)
      integer           pcal
      logical           plowe
      logical           done, lost, lowe
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  olde, newe, dtae, egylss, beta, gg
      integer           cst
      double precision  dptold
      double precision  dptnow
      integer           oldlol, newlol, minlol, maxlol
      integer           histoi, histoii, iobl
      double precision  wt1, wte, wtefac, wteo, wted
      double precision  wtotloss, wdtae, weprev, wepost
c
c     FIRST EXECUTABLE STATEMENT
c
c     New energy of the particle.
c
      olde = fpdata(ixenergy)
      dtae = lossrate * path
      newe = olde - dtae
c
c     Checking if the particle has become a low energy particle.
c
      plowe = (newe .lt. cutegy)
c
      if (plowe) then
c
        if (olde .gt. cutegy) then
c
          newe = cutegy
          dtae = olde - newe
          path = dtae / lossrate
c
        else
c
c         The old energy is too low. The particle will no more
c         be tracked.
c
          done = .true.
          lost = .false.
          lowe = .true.
          return
c
        endif
      endif
c
c     Mean particle speed.
c
      gg   = (1 + olde / mass) * (1 + newe / mass)
      beta = sqrt(1 - 1.d0 / gg)
c
      wt1  = fpdata(ixwt)
c
c     Advancing the particle.
c
c     Updating the speed and previous-to-current direction.
c
      fpdata(ixbeta) = beta
      fpdata(ixpxpr) = fpdata(ixpx)
      fpdata(ixpypr) = fpdata(ixpy)
      fpdata(ixpzpr) = fpdata(ixpz)
c
      call padvance(path, fpdata(ixpxpr), beta, fpdata(ixdepth),
     +              fpdata(ixx), fpdata(ixtime), plol, pcal,
     +              dptold)
c
c     Updating the energy.
c
      fpdata(ixenergy) = newe
      wtotloss         = wt1 * dtae
c
c     Updating longitudinal development.
c
      cst    = currstack
      dptnow = fpdata(ixdepth)
c
c     Analysing new particle position and histogram recording.
c
c     Part I: Horizontal observing levels.
c
      newlol = obslevca * dptnow + obslevcb
      newlol = max(0, min(newlol, totobslev))
      oldlol = plol(1)
      histoii = allpcleli(ipcode)
c
      if (newlol .ne. oldlol) then
c
c       The particle has passed across one or more observing levels.
c       We are therefore assuming that the zv component of the
c       direction of motion is not zero.
c
        histoi  = allpclelh(ipcode)
        wte     = wt1 * newe
        weprev  = wt1 * dtae
        plol(1) = newlol
c
        if (newlol .gt. oldlol) then
          minlol = oldlol
          maxlol = newlol
          wepost = 0
        else
          minlol = newlol
          maxlol = oldlol
          wepost = weprev
          weprev = 0
        endif
c
c       Evaluating the energy loss factor.
c
        egylss = dtae / (dptold - dptnow)
        wtefac = wt1 * egylss
c
        do iobl = minlol + 1, maxlol
          wlhistn(2, iobl, histoi) = wlhistn(2, iobl, histoi) + 1
          lhistn(2, iobl, histoi)  = lhistn(2, iobl, histoi)  + wt1
          wted = wtefac * (obslevdepth(iobl) - dptnow)
          wteo = wte + wted
          lhiste(2, iobl, histoi)  = lhiste(2, iobl, histoi)  + wteo
          lihiste(2, iobl, histoii) =
     +            lihiste(2, iobl, histoii) + abs(weprev - wted)
          weprev = wted
        enddo
c
        lihiste(2, maxlol + 1, histoii) =
     +          lihiste(2, maxlol + 1, histoii) + abs(weprev - wepost)
c
      else
c
c       The particle has not crossed any observing level. It is
c       only needed to record the energy losses.
c
        lihiste(2, newlol + 1, histoii) =
     +          lihiste(2, newlol + 1, histoii) + wtotloss
c
      endif
c
c     Part II: Planes othogonal to the shower axis.
c
      oldlol = plol(2)
      call salastol(fpdata(ixx), oldlol, newlol)
c
      if (newlol .ne. oldlol) then
c
c       The particle has passed across one or more observing levels.
c
        plol(2) = newlol
        if (newlol .gt. oldlol) then
          minlol = oldlol
          maxlol = newlol
        else
          minlol = newlol
          maxlol = oldlol
        endif
c
        histoi  = allpclelh(ipcode) + mxlhtable
c
c       No energy tables evaluated by now.
c
        do iobl = minlol + 1, maxlol
          wlhistn(2, iobl, histoi) = wlhistn(2, iobl, histoi) + 1
          lhistn(2, iobl, histoi)  = lhistn(2, iobl, histoi)  + wt1
        enddo
      endif
c
c     Analysing if the ground level has been reached.
c
      if (dptnow .ge. groundepth) then
c
c       The particle reached ground.
c       Here we are assuming also that the direction of motion cannot
c       be horizontal.
c
c       Correcting the energy.
c
        dtae  = (groundepth - dptnow) * dtae / (dptold - dptnow)
        newe  = newe + dtae
        fpdata(ixenergy) = newe
c
        call pcleground(cst, chpdata)
c
c       Correcting the energy loss records.
c
        wdtae    = - wt1 * dtae
        wtotloss = wtotloss + wdtae
c
        lihiste(2, newlol + 1, histoii) =
     +          lihiste(2, newlol + 1, histoii) + wdtae
c
c       All done, the particle is marked for deletion.
c       lowe is turned to .false. to prevent that the calling
c       program will interpret the particle as a low-energetic one.
c
        done = .true.
        lost = .false.
        lowe = .false.
c
      else if (((fpdata(ixpz) .ge. 0) .and. (dptnow .le. dtoplost))
     +         .or.
     +          (dptnow .le. 0)
     +          .or.
     +          (fpdata(ixz) .lt. zcglobmin)
     +         ) then
c
c       The particle is going upwards, and reached higher than the
c       injection altitude, or reached "higher" than the top of the
c       atmosphere: It's a lost particle. Marking for deletion.
c       lowe is turned to .false. to prevent that the calling
c       program will interpret the particle as a low-energetic one.
c
        wte = wt1 * newe
        nplost(2, cst) = nplost(2, cst) + wt1
        elost(2, cst)  = elost(2, cst)  + wte
c
        done = .true.
        lost = .true.
        lowe = .false.
c
      else
c
c       The particle will be maintained only if it has enough
c       energy.
c
        done = plowe
        lost = .false.
        lowe = plowe
c
      endif
c
c     Per stack recording of the energy loss.
c
      eloss(2, cst) = eloss(2, cst) + wtotloss
c
      return
c
      end
c     --- End of routine lgtmaster
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'kernel3.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
