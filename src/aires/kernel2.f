c
c     FILE: kernel2.f                       Creation date: 16/JUL/1996.
c                                       LAST MODIFICATION: 03/MAY/2004.
c
c     This file contains the simulation-steering routines, second part:
c     Basic monitoring routines and related ones. Routines tfac_tat monitor
c     longitudinal development are placed in kernel3.f
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stackmoni0(nspartic, stacknumber,
     +                      nrem, nextrem, lastpartic)
c
c     Evaluating the status of processed particles (0):
c     Ground level reaching, particle losses, etc.
c     Also, the fast particle stack is recompressed.
c     Longitudinal development is not tracked.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2002, 2003.
c
c
c     Arguments:
c     =========
c
c     nspartic........ (input, integer) Number of particles in the
c                      sequential part of the fast stack.
c     stacknumber..... (input, integer) The number of the stack to
c                      process.
c     nrem............ (input, integer) Number of remaining particles
c                      in the stack (this was designed for fast
c                      processing when this number is low).
c     nextrem......... (input, integer, array(nspartic)) Indices of
c                      remaining particles.
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
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           nspartic, stacknumber, nrem
      integer           nextrem(nspartic)
      integer           lastpartic
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, i0, offs0
      double precision  wt1, wte
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scanning the sequential part of the corresponding stack.
c
      offs0      = stabeg0(stacknumber)
      lastpartic = offs0
c
      do i0 = 1, nrem
        i = offs0 + nextrem(i0)
c
c       Analysing if the ground level has been reached.
c
        if (fpsta(ixdepth, i) .ge. groundepth) then
c
c         The particle reached ground.
c
          call pcleground(stacknumber, chpsta(i))
c
c         All done, the particle is deleted from the stack.
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
c     --- End of routine stackmoni0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stackmoni2(nspartic, stacknumber, nrem, nextrem,
     +                      lastpartic)
c
c     Evaluating the status of processed particles (II):
c     Decays, particle losses, etc.
c     Also, the fast particle stack is recompressed.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2001.
c
c     Arguments:
c     =========
c
c     nspartic........ (input, integer) Number of particles in the
c                      sequential part of the fast stack.
c     stacknumber..... (input, integer) The number of the stack to
c                      process.
c     nrem............ (input, integer) Number of remaining particles
c                      in the stack (this was designed for fast
c                      processing when this number is low).
c     nextrem......... (input, integer, array(nspartic)) Indices of
c                      remaining particles.
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
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           nspartic, stacknumber, nrem
      integer           nextrem(nspartic)
      integer           lastpartic
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pstackcomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, i0, offs0
      integer           ipcode
      double precision  wt1, wte, tmp1, tmp2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scanning the sequential part of the corresponding stack.
c
      offs0      = stabeg0(stacknumber)
      lastpartic = offs0
c
      do i0 = 1, nrem
        i = offs0 + nextrem(i0)
c
        if ( (fpsta(ixdepth, i) .le. 0)
     +       .or.
     +       ((fpsta(ixpx, i) .gt. 0) .and.
     +        (fpsta(ixx, i) .gt. hrmaxlx))
     +       .or.
     +       ((fpsta(ixpx, i) .lt. 0) .and.
     +        (fpsta(ixx, i) .lt. hrminlx))
     +       .or.
     +       ((fpsta(ixpy, i) .gt. 0) .and.
     +        (fpsta(ixy, i) .gt. hrmaxly))
     +       .or.
     +       ((fpsta(ixpy, i) .lt. 0) .and.
     +        (fpsta(ixy, i) .lt. hrminly))
     +       .or.
     +       (fpsta(ixz, i) .lt. zcglobmin)
     +     ) then
c
c         The particle is exiting the shower bounding box, or reached
c         "higher" than the top of the atmosphere: It's a lost
c         particle. Deleting.
c
          wt1 = fpsta(ixwt, i)
          wte = wt1 * fpsta(ixenergy, i)
          nplost(2, stacknumber) = nplost(2, stacknumber) + wt1
          elost(2, stacknumber)  = elost(2, stacknumber)  + wte
c
        else
c
c         The particle will be maintained only if it has enough
c         energy.
c
          ipcode = ipsta(ixpcode, i)
          if (ipcode .le. maxpcle) then
            tmp1 = cutenergy(ipcode, 1)
            tmp2 = cutenergy(ipcode, 2)
          else
            tmp1 = nucutenergy
            tmp2 = nucutenergy
          endif
c
          if (fpsta(ixenergy, i) .le. tmp2) then
c
c           The particle is removed because of its low energy.
c
            call pclelowe(stacknumber, ipsta(ixpcode, i),
     +                    fpsta(ixenergy, i), fpsta(ixwt, i),
     +                    ipsta(ixlol, i))
c
          else
c
c           The particle remains in the stack.
c
c           Checking normal energy threshold.
c
            lpsta(ixlowe, i) = (fpsta(ixenergy, i) .le. tmp1) 
c
            lastpartic = lastpartic + 1
            chpsta(lastpartic) = chpsta(i)
c
          endif
        endif
c
      enddo
c
      return
      end
c     --- End of routine stackmoni2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pcleground(stacknumber, chpclerecord)
c
c     Processing a particle that reached ground level.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2002.
c
c     Arguments:
c     =========
c
c     stacknumber..... (input, integer) The number of the stack
c                      currently being processed.
c     chpclerecord.... (input, character*(*)) Particle data.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
c     include 'pstackpar.f'      (Included by 'pbuffpar.f')
c     include 'ciopar.f'         (Included by 'pbuffpar.f')
      include 'pbuffpar.f'
c
c     Declaration of arguments.
c
      integer           stacknumber
      character*(*)     chpclerecord
c
c     Declaration of shared data.
c
      include 'pgndbuffcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Appending the record into the ground particle buffer.
c     If the buffer is full, ship the buffered particles first.
c
      if (npgndbuff .ge. gndbuffsize) call pcleship1
c
c     Appending the record.
c
      npgndbuff = npgndbuff + 1
c
      gndbuffsta(npgndbuff) = stacknumber
      gndbuffrec(npgndbuff) = chpclerecord
c
      return
c
      end
c     --- End of routine pcleground
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pclelowe(stacknumber, pcode, penergy, pwt, lastol)
c
c     Processing a particle whose energy is below the corresponding
c     threshold energy.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     stacknumber..... (input, integer) The number of the stack
c                      currently being processed.
c     pcode........... (input, integer) Particle code.
c     penergy......... (input, double precision) Particle kinetic
c                      energy (GeV).
c     pwt............. (input, double precision) Particle statistical
c                      weight.
c     lastol.......... (input, integer, array(2)) Last observing level
c                      crossed information.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
      include 'pclepar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           stacknumber, pcode
      double precision  penergy, pwt
      integer           lastol(2)
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ihi, iol
      double precision  pwe
c
c     FIRST EXECUTABLE STATEMENT
c
      pwe = pwt * penergy
c
c     Recording per stack information.
c
      nplowe(2, stacknumber) = nplowe(2, stacknumber) + pwt
      elowe(2, stacknumber)  = elowe(2, stacknumber)  + pwe
c
c     Recording per particle information.
c
      ihi = allpcleli(pcode)
      iol = lastol(1) + 1
      llhistn(2, iol, ihi)  = llhistn(2, iol, ihi)  + pwt
      wllhistn(2, iol, ihi) = wllhistn(2, iol, ihi) + 1
      llhiste(2, iol, ihi)  = llhiste(2, iol, ihi)  + pwe
c
      ihi = ihi + mxlitable
      iol = lastol(2) + 1
      llhistn(2, iol, ihi)  = llhistn(2, iol, ihi)  + pwt
      wllhistn(2, iol, ihi) = wllhistn(2, iol, ihi) + 1
      llhiste(2, iol, ihi)  = llhiste(2, iol, ihi)  + pwe
c
      return
c
      end
c     --- End of routine pclelowe
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine plostegy(ppcode, plol, lostegy)
c
c     Manual recording of energy lost in the medium.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2001.
c
c     Arguments:
c     =========
c
c     ppcode.......... (input, integer) Code of the particle that
c                      initiates the energy loss.
c     plol............ (input, integer) Last observing level crossed
c                      by the initiating particle.
c     lostegy......... (input, double precision) The total amount of
c                      lost energy, in GeV.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
      include 'pclepar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           ppcode, plol
      double precision  lostegy
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ihi
c
c     FIRST EXECUTABLE STATEMENT
c
c     Recording per stack information.
c
      eloss(2, currstack) = eloss(2, currstack) + lostegy
c
c     Recording per particle information.
c
      ihi = allpcleli(ppcode)
      lihiste(2, plol + 1, ihi)  = lihiste(2, plol + 1, ihi) + lostegy
c
      return
c
      end
c     --- End of routine plostegy
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newprimary
c
c     Initializations for a new shower: Selecting primary particle
c     parameters, setting the proper environment and stacking the
c     primary particlein the corresponding stack.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'thincomm.f'
      include 'kernelcomm.f'
      include 'showercomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      double precision  ran, ugam, e1ug
      double precision  cozenith, sizenith, sizen2, coazim, siazim
      double precision  inja, drz, rxy, rxyp, rnn, ola, olt, olz
      double precision  eth(2)
      double precision  primener, primzenith, primazim
      double precision  primx, primy
      double precision  pprim(3)
      double precision  pprimx, pprimy, pprimz
      equivalence       (pprim(1), pprimx)
      equivalence       (pprim(2), pprimy)
      equivalence       (pprim(3), pprimz)
      double precision  dmboxt, dmboxu, dmboyu
      integer           primlol(2)
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the beginning-of-shower date and time.
c
      call intdati(shinidati)
c
c     Selecting the primary code.
c
      if (nshprimary .le. 1) then
        primcode = shprimary(1)
      else
c
c       Selecting among all possibilities.
c
        ran = urandom()
        do i = 1, nshprimary
          j = i
          if (ran .lt. shprimarywt0(i)) goto 1010
        enddo
 1010   continue
        primcode = shprimary(j)
c
      endif
c
c     Selecting the primary energy and thinning energy.
c     pryenergymin was already checked to be greater than
c     1 eV.
c
      if (pryenergymin .ge. pryenergymax) then
        primener = pryenergymin
      else
c
c       Varying energy.
c
        ran = urandom()
        if (pryenergyslp .ne. 0) then
          ugam = - pryenergyslp
          e1ug = pryenergymin ** ugam
          primener = (e1ug +
     +                ran * (pryenergymax ** ugam - e1ug)
     +               ) ** (1.d0 / ugam)
        else
          primener = pryenergymin *
     +               exp(ran * log(pryenergymax / pryenergymin))
        endif
      endif
c
c     Checking consistency of primary energy and cut energies.
c
      if (primener .le. minethin) then
        eth(1) = primener
        eth(2) = minethin
        call errprint(1, '$A43', 3, 'newprimary', ' ',
     +                0, 0, 2, eth, ' ')
        primener = minethin
      endif
c
      aveprim(2) = primener
c
c     Selecting the zenith angle.
c
      if (pryzenithmin .ge. pryzenithmax) then
        primzenith = pryzenithmin
      else
c
c       Varying zenith angle.
c
        primzenith = prycosfac *
     +               acos(prycoszmin +
     +                    urandom() * (prycoszmax - prycoszmin))
      endif
c
c     Selecting the azimuth angle.
c
      if (pryazimmin .ge. pryazimmax) then
        primazim = pryazimmin
      else
c
c       Varying azimuth angle.
c
        primazim = pryazimmin +
     +               urandom() * (pryazimmax - pryazimmin)
      endif
c
c     If the azimuth angle is geographical, it is necessary to correct
c     it to obtain magnetic azimuth.
c
      if (geognorth) primazim = geobd - primazim
c
c     Reducing to the (-pi, pi] interval.
c
      if (primazim .gt. 180) then
        primazim = primazim - 360
      else if (primazim .le. -180) then
        primazim = primazim + 360
      endif
c
c     Thinning energy and related parameters.
c
      call showerthinit(primener)
c
c     The compressed file record will be effectively written once
c     the first interaction data is known.
c     Only the real array is set here.
c
      primfdata(1) = log(primener)
      primfdata(2) = primzenith
      primfdata(3) = primazim
      primfdata(4) = currethin
c
c     Converting zenith and azimuth into radians.
c
      primzenith = pi180 * primzenith
      primazim   = pi180 * primazim
c
c     Evaluating the central injection altitude. To this end we take
c     into account the Earth's curvature.
c
      cozenith = cos(primzenith)
      sizenith = sin(primzenith)
      sizen2   = sizenith ** 2
      inja     = groundz00 * (sqrt(iovgz00sq - sizen2) - cozenith)
      drz      = inja * cozenith
      rxy      = inja * sizenith
      cinjz    = drz + groundz
c
      primfdata(6) = cinjz
c
c     Evaluating the initial coordinates and direction of motion.
c
      coazim = cos(primazim)
      siazim = sin(primazim)
      primx  = rxy * coazim
      primy  = rxy * siazim
c
      rnn = hplanelim0 + hplanelim1 * injz
      if ((abs(primx) .lt. rnn) .and. (abs(primy) .lt. rnn)) then
        cinjz = injz
        drz   = cinjz - groundz
      endif
c
      rnn    = - sqrt(rxy ** 2 + drz ** 2)
      pprimx = primx / rnn
      pprimy = primy / rnn
      pprimz = drz   / rnn
c
      do i = 1, 3
        initshdir(i) = pprim(i)
        currshdir(i) = pprim(i)
        zsfact(i)    = - pprim(i)
      enddo
c
      zsfact(0) = - zsfact(3) * groundz
c
c     Evaluating the global time shift (Remember that internal time
c     unit makes c = 1).
c
      showert0 = drz / cozenith
c
c     Evaluating the intersections of the shower axis with the
c     observing levels, the corresponding time delay corrections,
c     and related quantities.
c
      olt = showert0 - inja
      do i = 1, nobslevelsp1
        ola = groundz00 *
     +        (sqrt(((rearth + obslevz(i)) / groundz00) ** 2 -
     +              sizen2) - cozenith)
        obslevt0(i)      = olt + ola
        olz              = ola * cozenith
        obslevsa(i)      = olz / zsfact(3)
        olz              = olz + groundz
        ola              = ola * sizenith
        obslevcore(1, i) = ola * coazim
        obslevcore(2, i) = ola * siazim
        obslevcore(3, i) = olz
        olz              = -(rearth + olz)
        obslevcore(4, i) = obslevcore(1, i) / olz
        obslevcore(5, i) = obslevcore(2, i) / olz
      enddo
c
      obslevsa(0)             = 1.d20 + 1.d10 * obslevsa(1)
      obslevsa(totobslev + 1) = -1.d35
c
c     Evaluating the dynamic shower bounding box parameters.
c     Particles going outside the bounding box will be considered
c     "lost" particles.
c
      dmboxt  = injz * ((dmboxdepth0 / groundepth) ** 2) +
     +          dmbox / max(cozenith, dmboxsf)
      dmboxt  = min(dmboxt, groundz00)
      rxyp    = min(rxy + rxysh0, groundz00)
      dmboxu  = rxyp * coazim
      dmboyu  = rxyp * siazim
      hrmaxlx = max(dmboxu,  dmboxt)
      hrminlx = min(dmboxu, -dmboxt)
      hrmaxly = max(dmboyu,  dmboxt)
      hrminly = min(dmboyu, -dmboxt)
c
c     Saving the initial space-depth coordinates of the primary.
c
      fstposdp(1)  = primx
      fstposdp(2)  = primy
      fstposdp(3)  = cinjz
      fstposdp(4)  = injz
      fstposdp(5)  = injdepth
c
c     Setting crossed observing levels data. Notice that, by default,
c     the injection point is in the shower axis.
c
      primlol(1)   = obslevl0
      primlol(2)   = obslevl0
c
c     Setting the geomagnetic field to use during the simulation of
c     the present shower.
c
      call setgeobfield
c
c     Marking the beginning of a new shower
c
      currshower = lastshower + 1
      shcomplete = .false.
c
c     Stacking the primary particle(s).
c
      call stackprimary(primcode, primener,
     +                  fstposdp, injdepth, showert0, pprim,
     +                  primlol, atmlayer0)
c
c     Calling the "every shower" model initializing routine.
c
      call modelnewshower
c
      return
c
      end
c     --- End of routine newprimary
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setgeobfield
c
c     Setting the geomagnetic field internal variables.
c     The constants here evaluated correspond to the scaled magnetic
c     field:
c
c         B' = 2.d-18 * c (m/s) * B (nT)
c
c     which appears in the equation
c
c                               ~
c                              dt (m)
c         u(t + 2dt) = u(t) + ---------  u x B'
c                              E (GeV)
c
c                                              ~
c     where dt = (half distance) / speed, and dt = (half dist)/ beta.
c     The other constant evaluated corresponds to an energy bound
c     to detect the regime where the corrections to u are less than
c     10^4 for a half distance of 50 m.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  x, z, r
      double precision  shbfac
      parameter         (shbfac = 2.d-18 * cspeed)
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      shgbx = igbx
      shgbz = igbz
c
      if (geobswitch .ge. 2) then
c
c       Adding gaussian fluctuation to the magnetic field components.
c       The standard deviation of the fluctuations of each component
c       is the input fluctuation divided by sqrt(2).
c
 1010   continue
        x = 2 * urandom() - 1
        z = 2 * urandom() - 1
        r = x ** 2 + z ** 2
        if ((r .ge. 1.d0) .or. (r .eq. 0)) goto 1010
c
        r = igbfluc * sqrt(-log(r) / r)
c
        shgbx = shgbx + r * x
        shgbz = shgbz + r * z
c
      endif
c
      if ((shgbx .eq. 0) .and. (shgbz .eq. 0)) then
c
        shgbon  = .false.
        emagfac = 0
c
      else
c
        shgbon  = (geobswitch .gt. 0)
c
c       The components of the geomagnetic field are scaled to allow
c       working with energies and masses in GeV and particle charges
c       expressed as multiples of the electron charge.
c
        shgbx   = shbfac * shgbx
        shgbz   = shbfac * shgbz
        emagfac = 5.d5 * sqrt(shgbx ** 2 + shgbz ** 2)
c
      endif
c
      return
c
      end
c     --- End of routine setgeobfield
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setfirstint(stacknumber, ilast0, ilast)
c
c     Setting the first interaction depth.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2003,
c                                         2004.
c
c
c     Arguments:
c     =========
c
c     stacknumber..... (input, integer) The number of the stack where
c                      the primary is.
c     ilast0.......... (input, integer) Position of last record
c                      previous to the stack scanning.
c     ilast........... (input, integer) Current position of last
c                      record.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           stacknumber, ilast0, ilast
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'showercomm.f'
      include 'pstackcomm.f'
      include 'hdatacomm.f'
      include 'constants.f'
c
c     Declaration of internal variables and arrays.
c
      integer           firstrec
      integer           i, j
      double precision  drz, rxy, oldshowert0
      integer           ipdata(12), ipdaux(12)
      double precision  xslant, depthfromz, zfromdepth
c
c     FIRST EXECUTABLE STATEMENT
c
      if (fstintauto .and. (ilast0 .eq. ilast)) then
c
c       The depth of first interaction is set from the stack data.
c
        firstrec = stabeg(stacknumber)
c
c       If the not-primary-advancing shitch is on, the position
c       of the primary is reset to the inital position, and the
c       longitudinal monitoring system is reset to zero.
c
        if (notproprim) then
c
          do i = 1, 5
            fpsta((ixx - 1) + i, firstrec) = fstposdp(i)
          enddo
          fpsta(ixtime, firstrec)  = - showert0
c
          ipsta(ixlol, firstrec)   = obslevl0
          ipsta(ixlolsa, firstrec) = obslevl0
c
          call table0l2
c
        endif
c
c       The first interaction depth is the current particle depth.
c
        fstintdp(2, 1) = fpsta(ixdepth, firstrec)
c
        if (resetclock) then
c
c         Re-evaluating the expected time delays (See "newprimary"),
c         and the global time shift. This requires also evaluating
c         ovserving levels time shifts.
c
          oldshowert0 = showert0
          drz         = fpsta(ixz, firstrec) - groundz
          rxy         = sqrt(fpsta(ixx, firstrec) ** 2 +
     +                       fpsta(ixy, firstrec) ** 2)
          drz         = rxy ** 2 + drz ** 2
          showert0    = sqrt(drz)
c
          do i = 1, nobslevelsp1
            obslevt0(i) = obslevt0(i) + (showert0 - oldshowert0)
          enddo
c
c         If resetclock is true, then time must start with the
c         first interaction.
c
          do i = 1, npstacks
            do j = stabeg(i), laststaentr(i)
              fpsta(ixtime, j) = - showert0
            enddo
          enddo
c
        endif
c
      else
c
c       The depth of first interaction was already evaluated within
c       routine "newprimary", or cannot be evaluated from stack data.
c       In this case the "not propagating" and/or "time resetting"
c       switchs are not taken into account.
c
        fstintdp(2, 1) = fstdpmanual
c
      endif
c
c     Evaluating the first interaction slant depth.
c
      fstintdp(2, 2) = xslant(fstintdp(2, 1), 0.d0,
     +                        -initshdir(3), groundz)
c
c     Correcting the position of the top surface of the shower
c     bounding box, if necessary.
c
      if (resetdtoplost) then
        drz      = zfromdepth(fstintdp(2, 1), i)
        drz      = max(drz, groundz, 0.d0)
        drz      = drz + max(6.d3 - 0.02d0 * drz, 4.d3)
        dtoplost = max(depthfromz(drz, i), 1.d-15)
      endif
c
c     Saving the primary data into the compressed files.
c     (Setting the remainig fields first. Other fields were
c     previously set in "newprimary").
c
      ipdata(1)    = primcode
      ipdata(2)    = firstshowernon1 + currshower
      do i = 3, 8
        ipdata(i) = shinidati(i - 2)
      enddo
c
      primfdata(5) = fstintdp(2, 1)
      primfdata(7) = ucspeed * showert0
c
      call allciosave(0, 0, 1, 8, ipdata, primfdata, ipdaux)
c
c     Setting to .false. the switch to determine the first
c     interaction depth. This will prevent resetting this parameter
c     again during the current shower.
c
      fstintnotset = .false.
c
      return
c
      end
c     --- End of routine setfirstint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine aftershower
c
c     Calculations to be done after completing the simulation of a
c     shower.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999;
c                                Fermilab 1999; La Plata 1999, 2000.
c
C     FIELD TOOL alterations: call writefield after the shower
C     version: 0.01 - 18/6/2009  Washington Carvalho
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c     include 'kernelpar.f'         (included by 'pstackpar.f')
      include 'pstackpar.f'
      include 'pclepar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'pclecomm.f'
      include 'pstackcomm.f'
      include 'showercomm.f'
c
C     FIELD TOOL: include field tool commons++++++
      include 'fieldcomm.f'
C     ++++++++++++++++++++++++++++++++++++++++++++
C
c     Declaration of internal variables and arrays.
c
      integer           i, ss
      double precision  fs, tmp1, tmp2, sw(npstacks)
      double precision  xmax(2), nxmax, sumofsq
      integer           xirc
      integer           ipdata(20), ipdaux(20)
      double precision  rpdata(30)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Completing compressed particle file(s) processing.
c
      call pcleshipall
c
c     Calling the model "after shower" routine.
c
      call modelaftershower
c
c     Histogram statistics, and evaluating the depth of the
c     shower maximum (Xmax) and the number of charged particles
c     at that point (NXmax).
c     These calculations MUST be done before the stack statistical
c     calculations.
c
      call table1(fstintdp(2, 1), fstintdp(2, 2), aveprim(2),
     +            primfdata(2), primfdata(3),
     +            xmax, nxmax, sumofsq, xirc)
c
      rpdata(10) = xmax(1)
      rpdata(11) = nxmax
      ipdata( 2) = abs(xirc)
c
c     Stack statistics.
c
      do i = 1, npstacks
c
        avgtotsize(i) = avgtotsize(i) + totstasize(i)
c
        ss = maxstasize(i)
        peakstsize(1, i) = peakstsize(1, i) + ss
        if (ss .lt. peakstsize(2, i)) peakstsize(2, i) = ss
        if (ss .gt. peakstsize(3, i)) peakstsize(3, i) = ss
        maxstasize(i) = 0
c
        fs = totalloc(i)
        procentries(1, i) = procentries(1, i) + fs
        if (fs .lt. procentries(2, i)) procentries(2, i) = fs
        if (fs .gt. procentries(3, i)) procentries(3, i) = fs
c
        ss = nwhoper(i)
        hardswrite(1, i) = hardswrite(1, i) + ss
        if (ss .lt. hardswrite(2, i)) hardswrite(2, i) = ss
        if (ss .gt. hardswrite(3, i)) hardswrite(3, i) = ss
c
        ss = nrhoper(i)
        hardsread(1, i) = hardsread(1, i) + ss
        if (ss .lt. hardsread(2, i)) hardsread(2, i) = ss
        if (ss .gt. hardsread(3, i)) hardsread(3, i) = ss
c
        callcounter(1, i) = callcounter(1, i) + callcounter(2, i)
        callcounter(2, i) = 0
c
        nplost(2, 0)   = nplost(2, 0)   + nplost(2, i)
        elost(2, 0)    = elost(2, 0)    + elost(2, i)
        nplowe(2, 0)   = nplowe(2, 0)   + nplowe(2, i)
        elowe(2, 0)    = elowe(2, 0)    + elowe(2, i)
        nprgnd(2, 0)   = nprgnd(2, 0)   + nprgnd(2, i)
        eprgnd(2, 0)   = eprgnd(2, 0)   + eprgnd(2, i)
        eloss(2, 0)    = eloss(2, 0)    + eloss(2, i)
        totpcles(2, 0) = totpcles(2, 0) + totpcles(2, i)
c
      enddo
c
c     Setting cio trailing record data.
c
      ipdata( 1) = firstshowernon1 + currshower
      rpdata( 1) = totpcles(2, 0)
      rpdata( 2) = nplost(2, 0)
      rpdata( 3) = nplowe(2, 0)
      rpdata( 4) = nprgnd(2, 0)
      rpdata(12) = elost(2, 0)
      rpdata(13) = elowe(2, 0)
      rpdata(14) = eprgnd(2, 0)
      rpdata(17) = eloss(2, 0)
c
c     Updating stack counters.
c
      call statupdate(npstacksp1, 5, nplost)
      call statupdate(npstacksp1, 5, elost)
      call statupdate(npstacksp1, 5, nplowe)
      call statupdate(npstacksp1, 5, elowe)
      call statupdate(npstacksp1, 5, nprgnd)
      call statupdate(npstacksp1, 5, eprgnd)
      call statupdate(npstacksp1, 5, eloss)
      call statupdate(npstacksp1, 5, totpcles)
c
c     "Null" particles.
c
      rpdata( 5) = nnotap(2)
      rpdata(15) = enotap(2)
      call statupdate(1, 5, nnotap)
      call statupdate(1, 5, enotap)
c
c     Neutrinos.
c
      rpdata( 6) = nneutrino(2)
      rpdata(16) = eneutrino(2)
      call statupdate(1, 5, nneutrino)
      call statupdate(1, 5, eneutrino)
c
c     Special primary statistics.
c
      if (specialprim) then
        do i = 1, nescpcles
          if (i .eq. specialprimlab) then
            escmacrouse(i)   = escmacrouse(i) + 1
            ss               = nspecialprim
            nsprimpart(1, i) = nsprimpart(1, i) + ss
            if (ss .lt. nsprimpart(2, i)) nsprimpart(2, i) = ss
            if (ss .gt. nsprimpart(3, i)) nsprimpart(3, i) = ss
          endif
        enddo
      endif
c
c     Depth of first interaction.
c
      call statupdate(2, 5, fstintdp)
c
c     Statistics of the primary energy.
c
      call statupdate(1, 5, aveprim)
c
c     Numbers and energies of particles not included in the particle
c     file(s): Updating is done below, and assignment varies from file
c     to file. Therefore, the assignments are done in the special
c     routine which writes the end of shower record. Affected fields
c     are: rpdata(7) to rpdata(9), and rpdata(18) to rpdata(20).
c
c     Ending date and cpu time accounting.
c
      call dati(datistr0(2))
      call cputime(.false., tmp1, tmp2)
      cpu0(3) = cpu0(3) + tmp1 - cpu0(4)
      cpu0(2) = cpu0(2) + cpu0(3)
      cpu0(4) = tmp1
c
      stopchsize3 = stopchsize3 + stopchsize2
      stopchsize2 = 0
      if (cpu0(2) .gt. 0) then
        stopchsize1 = stopchsize3 * (stopchsecs / cpu0(2)) + 0.5d0
      endif
c
c     Cio dates and cpu times.
c
      rpdata(21) = cpu0(3)
      call intdati(ipdata(3))
c
c     Recording the end of shower information, and marking the
c     final used positions of the respective buffers.
c
      call endciosave(8, ipdata, rpdata, 7, 9, 18, 20, ipdaux)
      call markallcio
c
c     Updating numbers and energies of particles not included in the
c     particle file(s).
c
      call updateciopbuf
C
C     FIELD TOOL: if calcfield is on, call writefield after shower+++
      if(calcfield)then
         call writefield(currshower)
      else if(trackflag)then !if only trackflag is on, call writetracks
         call writetracks(currshower)
      endif
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c     Marking the shower as completed.
c
      lastshower = currshower
      shcomplete = .true.
c
c     Clearing and resizing the stacks. The new sizes are proportional
c     to the average stack requirements.
c
      call setstackw(sw)
      call resizesta(npstacks, sw)
c
c     Resetting the shower CPU time counter.
c
      cpu0(3) = 0
c
      return
c
      end
c     --- End of routine aftershower
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c  MATIAS: this subroutine should be in a separate file, fieldoutput.f
c  and then included in the compilation configuration.
c
      subroutine writefield(showern)
c     writes field tool output files from common zsum and asum_xyz after 
c     shower. After writing it resets zsum and asum_xyz for next shower
c
c     version: 0.02 - 25/3/2010 Washington Carvalho
c
      use Dyn_Array
      implicit none
      include 'fieldcomm.f'

c     
c     showern ---> integer input, shower number
c
c     internal variables
      integer showern
      integer j,k,nu,t,na
c     Matias: Changed dimensions of arrays to use only the necesary
      double precision asum(ntbins,jmax,kmax)
      double precision asum_fr(ntbins,namax)
      double precision asum_perp(ntbins,jmax,kmax)
      double precision aradius(jmax,kmax,numax)
      double precision aradius_x(jmax,kmax,numax)
      double precision aradius_y(jmax,kmax,numax)
      double precision aradius_z(jmax,kmax,numax)
      double precision aradius_fr(namax,numax)
      double precision aradius_x_fr(namax,numax)
      double precision aradius_y_fr(namax,numax)
      double precision aradius_z_fr(namax,numax)
      double precision aphase(jmax,kmax,numax)
      double precision aphase_x(jmax,kmax,numax)
      double precision aphase_y(jmax,kmax,numax)
      double precision aphase_z(jmax,kmax,numax)
      double precision aphase_fr(namax,numax)
      double precision aphase_x_fr(namax,numax)
      double precision aphase_y_fr(namax,numax)
      double precision aphase_z_fr(namax,numax)
      complex*16  zsum2_fr(namax,numax)
      double precision asum_perp_x,asum_perp_y,asum_perp_z
      double precision ef(maxk),ef_perp(maxk)
      double precision ef_x(maxk),ef_y(maxk),ef_z(maxk)
      double precision ef_fr,ef_x_fr,ef_y_fr,ef_z_fr
      double precision fac_t
      double precision ux,uy,uz
      parameter (fac_t=2.40160339973116d-18) !fac_t=(e mu_r)/(8*pi*epsilon_0*c)
      double precision time
      character*4 suffix
c
c     Matias: This initialization is necessarty cos zsum2_fr is never used!
c     This bug? was present in all previous versions of ZHAireS (0.9, v25, v28) but 
c     the array was initialized automatically.  
c     Initializing zsum2_fr
      do nu=1,numax
         do na=1,namax
            zsum2_fr(na,nu)=(0.d0,0.d0)
         enddo
      enddo
c
      call putlog(0, .true., 'Field Tool: Writing output files.')
c
      write(suffix,107) showern
 107  format(i4.4)
c
c     STARTING FIELD TOOL WRITE_OUTPUT BLOCK =================
      if(calcfreq) then
      DO NU=1,NUMAX
         DO J=1,JMAX
            DO K=1,KMAX
c     9.6064136D-12=(e mu_r)1.e6/(2 pi epsilon_0 c)
c     Part of the factor accompanying the dimensionless part of the electric field.
c     The rest of this factor goes into the subroutine EMPSUM in the variable
c     FACFRQ.
               ZSUM2_X(J,K,NU)=ZSUM2_X(J,K,NU)*9.6064136D-12
               ZSUM2_Y(J,K,NU)=ZSUM2_Y(J,K,NU)*9.6064136D-12
               ZSUM2_Z(J,K,NU)=ZSUM2_Z(J,K,NU)*9.6064136D-12
               ZSUM2(J,K,NU)=ZSUM2(J,K,NU)*9.6064136D-12
               
               ARADIUS(J,K,NU)=SQRT(ZSUM2_X(J,K,NU)*
     +              CONJG(ZSUM2_X(J,K,NU)) + ZSUM2_Y(J,K,NU)*
     +              CONJG(ZSUM2_Y(J,K,NU))+ ZSUM2_Z(J,K,NU)*
     +              CONJG(ZSUM2_Z(J,K,NU)))
c MATIAS: dimag and dreal are obsolete and very dangerous complex functions, that expect a complex(8) as argument
c in this implementation, we use complex(16) (using a double for the imaginary parta nad a double for the ral part)
c this can lead to errors, specially when changing the compiler. The suggestion is to change dimag for DIMAG and dreal for DREAL

c Minus sign so that phase agrees with ZHS PRD 92
               APHASE(J,K,NU)=-ATAN2(DIMAG(ZSUM2(J,K,NU)),
     +              DREAL(ZSUM2(J,K,NU)))

c x,y,z components
               ARADIUS_X(J,K,NU)=SQRT(ZSUM2_X(J,K,NU)*
     +              CONJG(ZSUM2_X(J,K,NU)))
               APHASE_X(J,K,NU)=-ATAN2(DIMAG(ZSUM2_X(J,K,NU)),
     +              DREAL(ZSUM2_X(J,K,NU)))

               ARADIUS_Y(J,K,NU)=SQRT(ZSUM2_Y(J,K,NU)*
     +              CONJG(ZSUM2_Y(J,K,NU)))
               APHASE_Y(J,K,NU)=-ATAN2(DIMAG(ZSUM2_Y(J,K,NU)),
     +              DREAL(ZSUM2_Y(J,K,NU)))
               
               ARADIUS_Z(J,K,NU)=SQRT(ZSUM2_Z(J,K,NU)*
     +              CONJG(ZSUM2_Z(J,K,NU)))
               APHASE_Z(J,K,NU)=-ATAN2(DIMAG(ZSUM2_Z(J,K,NU)),
     +              DREAL(ZSUM2_Z(J,K,NU)))
            END DO
         END DO
      END DO
      endif !calcfreq
c     time domain asum_xyz and asum
      if(calctime) then
      DO T=1,ntbins
         DO J=1,JMAX
            DO K=1,KMAX
               ASUM_X(T,J,K)=FAC_T*ASUM_X(T,J,K)
               ASUM_Y(T,J,K)=FAC_T*ASUM_Y(T,J,K)
               ASUM_Z(T,J,K)=FAC_T*ASUM_Z(T,J,K)
c     Minus sign in modulus so that the electric field has
c     the correct bipolarity. Note that modulus gives always
c     positive sign. No need to put sign by hand in the x,y,z
c     components of vector potential.
               ASUM(T,J,K)=-sqrt(
     +              ASUM_X(T,J,K)*ASUM_X(T,J,K)+
     +              ASUM_Y(T,J,K)*ASUM_Y(T,J,K)+
     +              ASUM_Z(T,J,K)*ASUM_Z(T,J,K))
            END DO
         END DO
      END DO
      endif !(calctime)
c
c     ========================
c     Fresnel calculations
      if(calcfreqfres) then
      DO NU=1,NUMAX
         DO NA=1,NAMAX
c 9.6064136D-12=(e mu_r)1.e6/(2 pi epsilon_0 c)
c Factor accompanying the dimensionless part of the electric field.
c The rest of this factor goes into the subroutine EMPSUM in the 
c variable FACFRQ.
            ZSUM2_X_FR(NA,NU)=ZSUM2_X_FR(NA,NU)*9.6064136D-12
            ZSUM2_Y_FR(NA,NU)=ZSUM2_Y_FR(NA,NU)*9.6064136D-12
            ZSUM2_Z_FR(NA,NU)=ZSUM2_Z_FR(NA,NU)*9.6064136D-12
c
            ARADIUS_FR(NA,NU)=SQRT(
     +           ZSUM2_X_FR(NA,NU)*CONJG(ZSUM2_X_FR(NA,NU))+
     +           ZSUM2_Y_FR(NA,NU)*CONJG(ZSUM2_Y_FR(NA,NU))+
     +           ZSUM2_Z_FR(NA,NU)*CONJG(ZSUM2_Z_FR(NA,NU)))
c
c     Minus sign so that phase agrees with ZHS PRD 92             
            APHASE_FR(NA,NU)=
     +   -ATAN2(DIMAG(ZSUM2_FR(NA,NU)),DREAL(ZSUM2_FR(NA,NU)))
c
c     x,y,z components
            ARADIUS_X_FR(NA,NU)=SQRT(
     +      ZSUM2_X_FR(NA,NU)*CONJG(ZSUM2_X_FR(NA,NU)))
            APHASE_X_FR(NA,NU)=-ATAN2(DIMAG(ZSUM2_X_FR(NA,NU)),
     +           DREAL(ZSUM2_X_FR(NA,NU)))
c
            ARADIUS_Y_FR(NA,NU)=SQRT(
     +           ZSUM2_Y_FR(NA,NU)*CONJG(ZSUM2_Y_FR(NA,NU)))
            APHASE_Y_FR(NA,NU)=-ATAN2(DIMAG(ZSUM2_Y_FR(NA,NU)),
     +           DREAL(ZSUM2_Y_FR(NA,NU)))
c
            ARADIUS_Z_FR(NA,NU)=SQRT(
     +           ZSUM2_Z_FR(NA,NU)*CONJG(ZSUM2_Z_FR(NA,NU)))
            APHASE_Z_FR(NA,NU)=-ATAN2(DIMAG(ZSUM2_Z_FR(NA,NU)),
     +           DREAL(ZSUM2_Z_FR(NA,NU)))
c            
         END DO
      END DO            
      endif !(calcfreqfres)       
c     Time domain
c     FAC_T = (e mu_r)/(8 pi epsilon_0 c)        
      if(calctimefres) then
      DO T=1,ntbins
         DO NA=1,NAMAX
            ASUM_X_FR(T,NA)=FAC_T*ASUM_X_FR(T,NA)
            ASUM_Y_FR(T,NA)=FAC_T*ASUM_Y_FR(T,NA)
            ASUM_Z_FR(T,NA)=FAC_T*ASUM_Z_FR(T,NA)
c     Minus sign in modulus so that the electric field has
c     the correct bipolarity. Note that modulus gives always
c     positive sign. No need to put sign by hand in the x,y,z
c     components of vector potential.
            ASUM_FR(T,NA)=-sqrt(
     +           ASUM_X_FR(T,NA)*ASUM_X_FR(T,NA)+
     +           ASUM_Y_FR(T,NA)*ASUM_Y_FR(T,NA)+
     +           ASUM_Z_FR(T,NA)*ASUM_Z_FR(T,NA))
c
         END DO
      END DO
      endif !(calctimefres)
c     =============================
c     now, lets outout all this
c     =============================
c     open all files
      if(calcfreq)then
      if(empoutput) then !begin empoutput if (noindent)
         OPEN(323,FILE='PF_s'//suffix//'_XYZ.EMP',STATUS='UNKNOWN')
         OPEN(324,FILE='PS_s'//suffix//'.EMP',STATUS='UNKNOWN')
         OPEN(325,FILE='PF_s'//suffix//'.EMP',STATUS='UNKNOWN')
         OPEN(326,FILE='ZS_s'//suffix//'.EMP',STATUS='UNKNOWN')
         OPEN(327,FILE='ZF_s'//suffix//'.EMP',STATUS='UNKNOWN')
      endif ! empoutput if
c     old file (output for root with all showers)
         open(551,file='freqfraunh-root.dat',status='old',
     +        access='append')
         write(551,'(A1)') '#'
         write(551,'(A12,I5)') '# new shower',showern
         write(551,'(A1)') '#'
      endif
c
c     old files
      open(328,file='tracklengths.dat',status='old',access='append')
      open(329,file='pimutracklengths.dat',status='old',access='append')
      open(330,file='perpcletracklengths.dat',
     + status='old',access='append')
c
c     Vector potential time domain  
      if(calctime)then
      if(empoutput) then !begin empoutput if (noindent)
         OPEN(345,FILE='VP_s'//SUFFIX//'_T_PLOT.EMP',STATUS='UNKNOWN')
         OPEN(346,FILE='VP_s'//SUFFIX//'_T.EMP',STATUS='UNKNOWN')
         OPEN(348,FILE='VP_s'//SUFFIX//'_T_XYZ.EMP',STATUS='UNKNOWN')
         OPEN(350,FILE='VP_s'//SUFFIX//'_T_PERP.EMP',STATUS='UNKNOWN')
c     Electric field time domain
         OPEN(351,FILE='EF_s'//SUFFIX//'_T.EMP',STATUS='UNKNOWN')
         OPEN(352,FILE='EF_s'//SUFFIX//'_T_XYZ.EMP',STATUS='UNKNOWN')
         OPEN(353,FILE='EF_s'//SUFFIX//'_T_PERP.EMP',STATUS='UNKNOWN')
      endif !end empoutput if
c     old file (output for root with all showers)
         open(552,file='timefraunh-root.dat',status='old',
     +        access='append')
         write(552,'(A1)') '#'
         write(552,'(A12,I5)') '# new shower',showern
         write(552,'(A1)') '#'
      endif
c     
c     Electric field fresnel
      if(calcfreqfres)then
      if(empoutput) then !begin empoutput if (noindent)
         OPEN(365,FILE='PF_s'//SUFFIX//'_FRES.EMP',STATUS='UNKNOWN')
         OPEN(366,FILE='PF_s'//SUFFIX//'_XYZ_FRES.EMP',STATUS='UNKNOWN')
      endif
c     old file (output for root with all showers)
         open(553,file='freqfresnel-root.dat',status='old',
     +        access='append')
         write(553,'(A1)') '#'
         write(553,'(A12,I5)') '# new shower',showern 
         write(553,'(A1)') '#'
      endif
c     Vector potential time domain fresnel
      if(calctimefres)then
      if(empoutput) then !begin empoutput if (noindent)
         OPEN(376,FILE='VP_s'//SUFFIX//'_T_FRES.EMP',STATUS='UNKNOWN')
         OPEN(378,FILE='VP_s'//SUFFIX//'_T_XYZ_FRES.EMP',
     +        STATUS='UNKNOWN')
c     Electric field time domain fresnel
         OPEN(387,FILE='EF_s'//SUFFIX//'_T_FRES.EMP',STATUS='UNKNOWN')
         OPEN(389,FILE='EF_s'//SUFFIX//'_T_XYZ_FRES.EMP',
     +        STATUS='UNKNOWN')
      endif !empoutput if
c     old file (output for root with all showers)
         open(554,file='timefresnel-root.dat',status='old',
     +        access='append')
         write(554,'(A1)') '#'
         write(554,'(A12,I5)') '# new shower',showern
         write(554,'(A1)') '#'
      endif

 333  format(A)     
c

      if(calcfreq)then !start calcfreq if (no indent!)
      if(empoutput) then !begin empoutput if (noindent)
      if (countpimu) then
         write(324,333)
     + '#Charged Pions and Muons contributions are included.'
         write(324,333) '#'
      endif
     
      
      WRITE(324,333) '# Distance x Electric field per MHz  (m V/m /MHz)'
      WRITE(324,333) '#'
      WRITE(324,333) '#  Theta     Phi       Modulus             Phase  
     +         freq'
c
      if (countpimu) then
         write(326,333)
     + '#Charged Pions and Muons contributions are included.'
         write(326,333) '#'
      endif
      WRITE(326,333) '# Distance x Electric field per MHz  (m V/m /MHz)'
      WRITE(326,333) '#'
      WRITE(326,333) '#  Theta        Phi           Real E         
     +   Imaginary E      freq'
      endif    !end empoutput if
      endif    !end calcfreq if (no indent)
c
c     time domain vector potential related files =======
      if(calctime)then  !start clactime if (noindent)
      if(empoutput) then !begin empoutput if (noindent)
      DO J=1,JMAX
         WRITE(345,'(A46,1F14.6)') 
     + '# Time domain vector potential at Angle [deg] ',THETA(J)*RADEGR
         WRITE(345,'(A26,5F14.6)') '# at azimuth angles [deg] ',
     +(PHI(K)*RADEGR,K=1,KMAX)
         WRITE(346,'(A46,1F14.6)') 
     + '# Time domain vector potential at Angle [deg] ',THETA(J)*RADEGR
         WRITE(346,'(A26,5F14.6)') '# at azimuth angles [deg] ',
     +(PHI(K)*RADEGR,K=1,KMAX)
         WRITE(348,'(A40,1F14.6)') 
     + '# Vector potential x,y,z at Angle [deg] ',THETA(J)*RADEGR
         WRITE(348,'(A26,5F14.6)') '# at azimuth angles [deg] ',
     +(PHI(K)*RADEGR,K=1,KMAX)
         WRITE(350,'(A40,1F14.6)') 
     + '# Vector potential perp. at Angle [deg] ',THETA(J)*RADEGR
         WRITE(350,'(A26,5F14.6)') '# at azimuth angles [deg] ',
     +(PHI(K)*RADEGR,K=1,KMAX)
c         
         WRITE(351,'(A44,1F14.6)')
     + '# Time domain electric field at Angle [deg] ',THETA(J)*RADEGR
         WRITE(351,'(A26,5F14.6)') '# at azimuth angles [deg] ',
     +(PHI(K)*RADEGR,K=1,KMAX)
         WRITE(352,'(A38,1F14.6)') 
     + '# Electric field x,y,z at Angle [deg] ',THETA(J)*RADEGR
         WRITE(352,'(A26,5F14.6)') '# at azimuth angles [deg] ',
     +(PHI(K)*RADEGR,K=1,KMAX)
         WRITE(353,'(A38,1F14.6)') 
     + '# Electric field perp. at Angle [deg] ',THETA(J)*RADEGR
         WRITE(353,'(A26,5F14.6)') '# at azimuth angles [deg] ',
     +(PHI(K)*RADEGR,K=1,KMAX)

         DO T=1,ntbins
            TIME=(T-MAXTHALF)*DT_BIN_NS+0.5*DT_BIN_NS
            IF (TIME.GT.TIME_MAX.OR.TIME.LT.TIME_MIN) GOTO 500 
c     write vector potentialS
            WRITE(346,904) TIME,(ASUM(T,J,K),K=1,KMAX)
            WRITE(348,944) TIME,
     +           (ASUM_X(T,J,K),ASUM_Y(T,J,K),ASUM_Z(T,J,K),K=1,KMAX)


            DO K=1,KMAX
               UX = SINTET(J)*COSPHI(K)
               UY = SINTET(J)*SINPHI(K)
               UZ = COSTET(J)
               ASUM_PERP_X = -(UY*UY+UZ*UZ)*ASUM_X(T,J,K) 
     +              + UX*UY*ASUM_Y(T,J,K)  
     +              + UX*UZ*ASUM_Z(T,J,K)
               ASUM_PERP_Y =    UX*UY*ASUM_X(T,J,K) 
     +              - (UX*UX+UZ*UZ)*ASUM_Y(T,J,K) 
     +              + UY*UZ*ASUM_Z(T,J,K)
               ASUM_PERP_Z = UX*UZ*ASUM_X(T,J,K)
     +              + UY*UZ*ASUM_Y(T,J,K)
     +              - (UX*UX+UY*UY)*ASUM_Z(T,J,K)
               
               ASUM_PERP(T,J,K)=-SQRT(ASUM_PERP_X*ASUM_PERP_X +
     +              ASUM_PERP_Y*ASUM_PERP_Y +
     +              ASUM_PERP_Z*ASUM_PERP_Z)
c     calculate field
               EF_X(k)=-(ASUM_X(T+1,J,K)-ASUM_X(T,J,K))/DT_BIN_S
               EF_Y(k)=-(ASUM_Y(T+1,J,K)-ASUM_Y(T,J,K))/DT_BIN_S
               EF_Z(k)=-(ASUM_Z(T+1,J,K)-ASUM_Z(T,J,K))/DT_BIN_S
               EF_PERP(k)= -(ASUM_PERP(T+1,J,K)-
     +              ASUM_PERP(T,J,K))/DT_BIN_S
               EF(k)=-(ASUM(T+1,J,K)-ASUM(T,J,K))/DT_BIN_S
            END DO
            WRITE(350,904) TIME,(ASUM_PERP(T,J,K),K=1,KMAX)
            

c     Output for plotting purposes with gnuplot        
            WRITE(345,904) 
     +           (T-MAXTHALF)*DT_BIN_NS,(ASUM(T,J,K),K=1,KMAX)
            WRITE(345,904)
     +           (T+1-MAXTHALF)*DT_BIN_NS,(ASUM(T,J,K),K=1,KMAX)
c     write fields
            WRITE(351,904) TIME,(EF(K),K=1,KMAX)
            WRITE(352,944) TIME,(EF_X(K),EF_Y(K),EF_Z(K),K=1,KMAX)
            WRITE(353,904) TIME,(EF_PERP(K),K=1,KMAX)
 500        CONTINUE  
         END DO
         
         WRITE(345,*) ''
         WRITE(346,*) ''
         WRITE(348,*) ''
         WRITE(350,*) ''
         WRITE(351,*) ''
         WRITE(352,*) ''
         WRITE(353,*) ''
      END DO 
 904  FORMAT (1X,1F15.5,1P,5(1E18.10))
 944  FORMAT (1X,1F15.5,1P,5(3E18.10))
      endif !empoutput if (noindent)
      endif !end cacltime if (no indent)

c     end time domain vector potential related files =======
c
      write(328,114) 
     + showern, tracksum,sumdeltaz,sumabdeltaz,
     + excesssum,excesssumz,excessabsumz
      write(329,125) 
     + showern, pimutracksum,pimusumdeltaz,pimusumabdeltaz,
     + pimuexcesssum,pimuexcesssumz,pimuexcessabsumz
      write(330,126)
     + showern,emtracksum,emsumdeltaz,emsumabdeltaz,
     + eptracksum,epsumdeltaz,epsumabdeltaz,
     + piptracksum,pipsumdeltaz,pipsumabdeltaz,
     + pimtracksum,pimsumdeltaz,pimsumabdeltaz,
     + muptracksum,mupsumdeltaz,mupsumabdeltaz,
     + mumtracksum,mumsumdeltaz,mumsumabdeltaz,
     + ptracksum,psumdeltaz,psumabdeltaz,
     + pbartracksum,pbarsumdeltaz,pbarsumabdeltaz
 114  format(I4,6E12.5)
 125  format(I4,6E13.5)
 126  format(I4,24E13.5)
c
c     write freq spectrum xyz
c     header file 323 - freq. spectrum xyz 

      if(calcfreq)then   !begin calcfreq if (noindent)
      if(empoutput) then !begin empoutput if (noindent)
      if(countpimu) then 
         write(323,333)
     +        '#Charged Pions and Muons contributions are included.'
         write(323,333) '#'
      endif
      DO J=1,JMAX
         WRITE(323,'(A55,1F14.6)')
     +        '# Frequency Spectrum (x,y,z) components at Angle [deg] ',
     +        THETA(J)*RADEGR 
         WRITE(323,'(A25,5(1F14.6))') '# at azimuth angles [deg]',
     +        (PHI(K)*RADEGR,K=1,KMAX)
         DO NU=1,NUMAX
            WRITE(323,933) FREQ(NU),
     +           (ARADIUS_X(J,K,NU),APHASE_X(J,K,NU),
     +           ARADIUS_Y(J,K,NU),APHASE_Y(J,K,NU),
     +           ARADIUS_Z(J,K,NU),APHASE_Z(J,K,NU),
     +           K=1,KMAX)
         END DO
         WRITE(323,*) ' '
      END DO
 933  FORMAT(1X,1F15.3,1P,5(6E18.10))
c     write freq spectrum
      do k=1,kmax
         DO NU=1,NUMAX
            DO J=1,JMAX
               
               WRITE (324,100) RADEGR*THETA(J),RADEGR*PHI(K),
     +              ARADIUS(J,K,NU),APHASE(J,K,NU),FREQ(NU)
               WRITE (326,100) RADEGR*THETA(J),RADEGR*PHI(K),
     +              DREAL(ZSUM2(J,K,NU)),DIMAG(ZSUM2(J,K,NU)),FREQ(NU)
            END DO
            WRITE (324,333) ' '
            WRITE (326,333) ' '
         END DO
      END DO
 100  FORMAT (1X,2F8.4,3E18.10)
c
      if (countpimu) then
         write(325,333)
     + '#Charged Pions and Muons contributions are included.'
         write(325,333) '#'
      endif
      WRITE(325,333) '# Distance x Electric field per MHz  (m V/m /MHz)'
      WRITE(325,333) '#'
      WRITE(325,333) '#      Frequency          Modulus           Phase 
     +         Theta       Phi'
c      
      if (countpimu) then
         write(327,333)
     + '#Charged Pions and Muons contributions are included.'
         write(327,333) '#'
      endif
      WRITE(327,333) '# Distance x Electric field per MHz  (m V/m /MHz)'
      WRITE(327,333) '#'
      WRITE(327,333) '#      Frequency          Real E          Imaginar
     +y E      Theta      Phi'
c
      do k=1,kmax
         DO J=1,JMAX
            DO NU=1,NUMAX
              WRITE(325,115) FRQCOM(NU),ARADIUS(J,K,NU),APHASE(J,K,NU),
     +              THETA(J)*RADEGR, PHI(K)*RADEGR
              WRITE(327,115) FRQCOM(NU),DREAL(ZSUM2(J,K,NU)),
     +           DIMAG(ZSUM2(J,K,NU)),THETA(J)*RADEGR, PHI(K)*RADEGR
            END DO
            WRITE(325,333) ' '
            WRITE(327,333) ' '
         END DO
      END DO
 115  FORMAT (1X,3E18.10,2F10.4) 
      endif   !end if empoutput (no indent)
      endif   !end calcfreq if (no indent)

c     Write Fresnel files
c     Fresnel PF (spectrum) files
c     
      if(calcfreqfres)then  !start calcfreqfres if (no indent)
      if(empoutput) then !begin empoutput if (noindent)
      if (countpimu) then
         write(365,333)
     +        '#Charged Pions and Muons contributions are included.'
         write(365,333) '#'
      endif
      DO NA=1,NAMAX
         WRITE(365,'(A44,1X,3F14.6)') 
     + '# Frequency Spectrum at antenna position [m]',
     +        XANT(NA),YANT(NA),ZANT(NA)
         WRITE(365,333) "#    Frequency      Modulus         Phase"
         DO NU=1,NUMAX
            WRITE(365,903) FREQ(NU),
     +           ARADIUS_FR(NA,NU),APHASE_FR(NA,NU)
         END DO
         WRITE(365,*) ''
      END DO
 903  FORMAT (1X,1F15.3,1P,2E18.10)
c     
      if (countpimu) then
         write(366,333)
     +        '#Charged Pions and Muons contributions are included.'
         write(366,333) '#'
      endif
      DO NA=1,NAMAX
         WRITE(366,'(A55,1X,3F14.6)') 
     +        '# Frequency Spectrum components at antenna position [m]',
     +        XANT(NA),YANT(NA),ZANT(NA)
         WRITE(366,333) "#    Freq.      Modulus X           Phase X
     +     Modulus Y           Phase Y         Modulus Z           Phase
     + Z"
         DO NU=1,NUMAX
            WRITE(366,933) FREQ(NU),
     +           ARADIUS_X_FR(NA,NU),APHASE_X_FR(NA,NU),
     +           ARADIUS_Y_FR(NA,NU),APHASE_Y_FR(NA,NU),
     +           ARADIUS_Z_FR(NA,NU),APHASE_Z_FR(NA,NU)
         END DO
         WRITE(366,*) ''
      END DO
      endif !empoutput
      endif !end calcfreqfres if (noindent)
c
c     Fresnel Vector Potential
      if(calctimefres)then   !start calctimefres if (no indent)
      if(empoutput) then !begin empoutput if (noindent)
      if (countpimu) then
         write(376,333)
     +        '#Charged Pions and Muons contributions are included.'
         write(376,333) '#'
         write(378,333)
     +        '#Charged Pions and Muons contributions are included.'
         write(378,333) '#'
      endif
      DO NA=1,NAMAX
         WRITE(376,'(A54,3F14.6)')
     +        '# Time domain vector potential at antenna position [m]',
     +        XANT(NA),YANT(NA),ZANT(NA)
         WRITE(376,333) "#        Time          Modulus"
         WRITE(378,'(A53,3F14.6)')
     +        '# Vector potential components at antenna position [m]',
     +        XANT(NA),YANT(NA),ZANT(NA)
         WRITE(378,333) "#      Time(ns)    X component      Y component
     +       Z component"
c Matias: The -2 is to fix some problem with the derivative in the final bin
         DO T=1,ntbins-2
            TIME=(T-MAXTHALF)*DT_BIN_NS+0.5*DT_BIN_NS+dt(na)*1.d9
            IF (TIME.GT.TIME_MAX.OR.TIME.LT.TIME_MIN) GOTO 550
            WRITE(376,904) TIME,ASUM_FR(T,NA)
            WRITE(378,944) TIME,ASUM_X_FR(T,NA),
     +           ASUM_Y_FR(T,NA),ASUM_Z_FR(T,NA)
 550        CONTINUE
         END DO
         WRITE(376,*) ''
         WRITE(378,*) ''
      END DO
c     
c     Write Fresnel Field
c
      if (countpimu) then
         write(387,333)
     +        '#Charged Pions and Muons contributions are included.'
         write(387,333) '#'
         write(389,333)
     +        '#Charged Pions and Muons contributions are included.'
         write(389,333) '#'
      endif      
      
      do na=1,namax
         WRITE(387,'(A40,1X,3F14.6)') 
     +        '# Electric Field at antenna position [m]',
     +        XANT(NA),YANT(NA),ZANT(NA)
         WRITE(387,333) "#   Time(ns)     Field(V/m)"           
         WRITE(389,'(A51,1X,3F14.6)') 
     +        '# Electric Field components at antenna position [m]',
     +        XANT(NA),YANT(NA),ZANT(NA)
         WRITE(389,333) "#   Time(ns)   Field_X (V/m)    Field_Y (V/m)  
     +   Field_Z (V/m)"
c Matias: The -2 is to fix some problem with the derivative in the final bin
         do t=1,ntbins-2
            TIME=(T-MAXTHALF)*DT_BIN_NS+0.5*DT_BIN_NS+dt(na)*1.d9
            IF (TIME.GT.TIME_MAX.OR.TIME.LT.TIME_MIN) GOTO 660
            EF_FR=-(ASUM_FR(T+1,NA)-ASUM_FR(T,NA))/DT_BIN_S
            EF_X_FR=-(ASUM_X_FR(T+1,NA)-ASUM_X_FR(T,NA))/DT_BIN_S
            EF_Y_FR=-(ASUM_Y_FR(T+1,NA)-ASUM_Y_FR(T,NA))/DT_BIN_S
            EF_Z_FR=-(ASUM_Z_FR(T+1,NA)-ASUM_Z_FR(T,NA))/DT_BIN_S
            WRITE(387,964) TIME,EF_FR
            WRITE(389,965) TIME,EF_X_FR,EF_Y_FR,EF_Z_FR
 964        FORMAT(1X,1F11.3,E18.10)
 965        FORMAT(1X,1F11.3,3(1E18.10))
 660        continue
         enddo
         write(387,*)''
         write(389,*)''
      enddo
      endif    !empoutput if (noindent)
      endif   !end calctimefres if (no indent)
c
c     write -root output files&&&&&&&&&&&&&&&&&&&&&&
c
c     freqfraunh-root file
      if(calcfreq)then
         do k=1,kmax
            do j=1,jmax
               do nu=1,numax
                  write(551,freqfraufmt) showern,j,k,theta(j)*radegr,
     +                 phi(k)*radegr,nu,freq(nu),aradius(j,k,nu),
     +                 aphase(j,k,nu),
     +                 aradius_x(j,k,nu),aphase_x(j,k,nu),
     +                 aradius_y(j,k,nu),aphase_y(j,k,nu),
     +                 aradius_z(j,k,nu),aphase_z(j,k,nu)
               enddo
            enddo
         enddo
c 5551    format(3I4, 2F14.6, I4, F11.3, 1P, 8E18.10)
      endif

c     timefraunh-root file
      if(calctime)then
         do k=1,kmax
            do j=1,jmax
c Matias: The -2 is to fix some problem with the derivative in the final bin
               do t=1,ntbins-2

c               time calculation
                  TIME=(T-MAXTHALF)*DT_BIN_NS+0.5*DT_BIN_NS
                  IF (TIME.GT.TIME_MAX.OR.TIME.LT.TIME_MIN) GOTO 6661

c               field calculation
                  EF_X(k)=-(ASUM_X(T+1,J,K)-ASUM_X(T,J,K))/DT_BIN_S
                  EF_Y(k)=-(ASUM_Y(T+1,J,K)-ASUM_Y(T,J,K))/DT_BIN_S
                  EF_Z(k)=-(ASUM_Z(T+1,J,K)-ASUM_Z(T,J,K))/DT_BIN_S
                  EF(k)=-(ASUM(T+1,J,K)-ASUM(T,J,K))/DT_BIN_S
                  
                  write(552,timefraufmt) showern,j,k,theta(j)*radegr,
     +                 phi(k)*radegr,time,asum(t,j,k),asum_x(t,j,k),
     +                 asum_y(t,j,k),asum_z(t,j,k),
     +                 ef(k),ef_x(k),ef_y(k),ef_z(k)
                
 6661             continue
               enddo
            enddo
         enddo
c 5552    format(3I4, 2F14.6, F15.5, 1P, 8E18.10)
      endif

c     freqfresnel-root file
      if(calcfreqfres)then
         do na=1,namax
            do nu=1,numax
               write(553,freqfresfmt) showern,na,xant(na),yant(na),
     +     zant(na),nu,freq(nu),aradius_fr(na,nu),aradius_x_fr(na,nu),
     +     aradius_y_fr(na,nu),aradius_Z_fr(na,nu)
            enddo
         enddo
c 5553    format(2I5, 3F14.6, I4, F17.3, 1P, 4E18.10)
      endif

c     timefresnel-root file
      if(calctimefres)then
         do na=1,namax
c Matias: The -2 is to fix some problem with the derivative in the final bin
            do t=1,ntbins-2
               
c             time calculation
               TIME=(T-MAXTHALF)*DT_BIN_NS+0.5*DT_BIN_NS+dt(na)*1.d9
               IF (TIME.GT.TIME_MAX.OR.TIME.LT.TIME_MIN) GOTO 6664

c             field calculation
               EF_FR=-(ASUM_FR(T+1,NA)-ASUM_FR(T,NA))/DT_BIN_S
               EF_X_FR=-(ASUM_X_FR(T+1,NA)-ASUM_X_FR(T,NA))/DT_BIN_S
               EF_Y_FR=-(ASUM_Y_FR(T+1,NA)-ASUM_Y_FR(T,NA))/DT_BIN_S
               EF_Z_FR=-(ASUM_Z_FR(T+1,NA)-ASUM_Z_FR(T,NA))/DT_BIN_S

               write(554,timefresfmt) showern,na,xant(na),yant(na),
     +   zant(na),time,asum_fr(t,na),asum_x_fr(t,na),asum_y_fr(t,na),
     +   asum_z_fr(t,na),ef_fr,ef_x_fr,ef_y_fr,ef_z_fr
                  
 6664          continue
            enddo
         enddo
c 5554    format(2I5, 3F14.6, F15.5, 1P, 8E18.10)
      endif



c     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
c     close all files
      if(calcfreq)then
      if(empoutput) then !begin empoutput if (noindent)
         CLOSE(323)
         CLOSE(324)
         CLOSE(325)
         CLOSE(326)
         CLOSE(327)
       endif
         close(551)
      endif
      
      close(328)
      close(329)

         if(calctime)then
         if(empoutput) then !begin empoutput if (noindent)
            close(345)
            close(346)
            close(348)
            close(350)
            close(351)
            close(352)
            close(353)
          endif
            close(552)
         endif

         if(calcfreqfres)then
         if(empoutput) then !begin empoutput if (noindent)
            close(365)
            close(366)
         endif
            close(553)
         endif
         if(calctimefres)then
         if(empoutput) then !begin empoutput if (noindent)
            close(376)
            close(378)
            close(387)
            close(389)
          endif
            close(554)
         endif
c
c     SPECIAL NOTE: SINCE THE ARRAY INITIALIZATION IS DONE ONLY ONCE, MEMORY
c     IS NOT BEING DEALLOCATED AND THE ARRAY IS ZEROED AFTER THE FILES ARE WRITEN
c     Be carefull, this is no longer true. Arrays are allocated in fieldparser.f
c
c     Resetting zsum
      if(calcfreq) then
        do j=1,jmax
           do nu=1,numax
              do k=1,kmax
                 zsum2(j,k,nu)=(0.d0,0.d0)
                 zsum2_x(j,k,nu)=(0.d0,0.d0)
                 zsum2_y(j,k,nu)=(0.d0,0.d0)
                 zsum2_z(j,k,nu)=(0.d0,0.d0)
              enddo
           enddo
        enddo
      endif
c
c     reseting asum_x,y,z
      if(calctime) then
        do t=1,ntbins
           do j=1,jmax
              do k=1,kmax
                 asum_x(t,j,k)=0.d0
                 asum_y(t,j,k)=0.d0
                 asum_z(t,j,k)=0.d0
              enddo
           enddo
        enddo
      endif
c
c     Resetting asum_xyz_fr 
      if(calctimefres) then
        do t=1,ntbins
           do na=1,namax
              asum_x_fr(t,na)=0.d0
              asum_y_fr(t,na)=0.d0
              asum_z_fr(t,na)=0.d0
           enddo
       enddo
      endif
c
c     Resetting zsum2_xyz_fr
      if(calcfreqfres) then
        do nu=1,numax
           do na=1,namax
              zsum2_fr(na,nu)=(0.d0,0.d0)
              zsum2_x_fr(na,nu)=(0.d0,0.d0)
              zsum2_y_fr(na,nu)=(0.d0,0.d0)
              zsum2_z_fr(na,nu)=(0.d0,0.d0)
           enddo
        enddo
      endif
c
c     Resetting total tracks
      tracksum=0.d0
      sumdeltaz=0.d0
      sumabdeltaz=0.d0
      excesssum=0.d0
      excesssumz=0.d0
      excessabsumz=0.d0
c
c     Resetting PIMU tracks
      pimutracksum=0.d0
      pimusumdeltaz=0.d0
      pimusumabdeltaz=0.d0
      pimuexcesssum=0.d0
      pimuexcesssumz=0.d0
      pimuexcessabsumz=0.d0
c     per partticle
      emtracksum=0.d0
      emsumdeltaz=0.d0
      emsumabdeltaz=0.d0
      eptracksum=0.d0
      epsumdeltaz=0.d0
      epsumabdeltaz=0.d0
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
      ptracksum=0.d0
      psumdeltaz=0.d0
      psumabdeltaz=0.d0
      pbartracksum=0.d0
      pbarsumdeltaz=0.d0
      pbarsumabdeltaz=0.d0
      return
      end
c     --- End of routine writefield
c     
c
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine writetracks(showern)
c     writes only tracklength output files
c     after writing it resets tracklengths for next shower
c
c     version: 0.01 - 06/10/2009 Washington Carvalho
c
      implicit none
      include 'fieldcomm.f'

c     
c     showern ---> integer input, shower number
c
c     internal variables
      integer showern
c
      call putlog(0, .true., 'Field Tool: Writing tracklength file.')
c     
      open(328,file='tracklengths.dat',status='old',access='append')
      write(328,114) 
     + showern, tracksum,sumdeltaz,sumabdeltaz,
     + excesssum,excesssumz,excessabsumz
 114  format(I4,1p6E12.5)
      close(328)
c
c     Resetting total tracks
      tracksum=0.d0
      sumdeltaz=0.d0
      sumabdeltaz=0.d0
      excesssum=0.d0
      excesssumz=0.d0
      excessabsumz=0.d0
c
      call putlog(0, .true.,'Field Tool: Writing pimutracklength file.')
c     
      open(329,file='pimutracklengths.dat',status='old',access='append')
      write(329,115) 
     + showern, pimutracksum,pimusumdeltaz,pimusumabdeltaz,
     + pimuexcesssum,pimuexcesssumz,pimuexcessabsumz
 115  format(I4,1p6E13.5)
      close(329)
c
c     Resetting pimu tracks
      pimutracksum=0.d0
      pimusumdeltaz=0.d0
      pimusumabdeltaz=0.d0
      pimuexcesssum=0.d0
      pimuexcesssumz=0.d0
      pimuexcessabsumz=0.d0

c
      call putlog(0, .true.,
     + 'Field Tool: Writing perpcletracklength file.')
c  
      open(330,file='perpcletracklengths.dat',
     + status='old',access='append')
      write(330,116) 
     + showern,emtracksum,emsumdeltaz,emsumabdeltaz,
     + eptracksum,epsumdeltaz,epsumabdeltaz,
     + piptracksum,pipsumdeltaz,pipsumabdeltaz,
     + pimtracksum,pimsumdeltaz,pimsumabdeltaz,
     + muptracksum,mupsumdeltaz,mupsumabdeltaz,
     + mumtracksum,mumsumdeltaz,mumsumabdeltaz,
     + ptracksum,psumdeltaz,psumabdeltaz,
     + pbartracksum,pbarsumdeltaz,pbarsumabdeltaz
 116  format(I4,1p24E13.5)
      close(330)
c     Resetting per partticle
      emtracksum=0.d0
      emsumdeltaz=0.d0
      emsumabdeltaz=0.d0
      eptracksum=0.d0
      epsumdeltaz=0.d0
      epsumabdeltaz=0.d0
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
      ptracksum=0.d0
      psumdeltaz=0.d0
      psumabdeltaz=0.d0
      pbartracksum=0.d0
      pbarsumdeltaz=0.d0
      pbarsumabdeltaz=0.d0      
      return
      end
c     --- End of routine writetracks
c     
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'kernel2.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
