c
c     FILE: modelutils.f                    Creation date: 21/AUG/1996.
c                                       LAST MODIFICATION: 13/MAY/2005.
c
c     This file contains several routines to perform auxiliary
c     calculations related to the interaction models.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine coulscatter(chimin, frecord, path, sqrmass, wsq, weff)
c
c     Processing Coulomb and multiple scattering.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000.
c
c
c     Arguments:
c     =========
c
c     chimin.......... (input, double precision) Minimum significant
c                      chi angle for single scatterings (rather
c                      technical parameter).
c     frecord......... (input, double precision, array(maxstalen))
c                      Array containing the floating point data of the
c                      corresponding particle. The elements of this
c                      array are the same as the fields in the
c                      particle stack array.
c     path............ (input, double precision) The current path the
c                      particle is moving across. It is assumed that it
c                      is large enough to make scattering significant.
c     sqrmass......... (input, double precision) Square of particle
c                      mass.
c     wsq, weff....... (input, double precision) Internal coefficients,
c                      related to the average kinetic energy.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
      include 'modelpar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  chimin
      double precision  frecord(maxstalen)
      double precision  path, sqrmass, wsq, weff
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  rootpath, chic, chic2
      double precision  mu, mugg, v, vm, vv, f, b
      double precision  costheta, sintheta, cdphi, sdphi
      logical           accept
      double precision  xyz(3)
      double precision  urandom, urandomt
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scattering inserted as a single deflection part way along
c     track: Then remaining part bent is followed.
c
      rootpath = sqrt(path)
c
c     The angle "Chi c" of scattering theory.
c
      chic = 0.8092d-3 * rootpath * weff / (wsq - sqrmass)
c
      if (chic .gt. chimin) then
c
c       fjscat is taken into account in factor chic2fac
c
        chic2 = chic2fac * (chic ** 2)
        mugg  = chic2 * sqrmass / wsq
c
 1010   continue
        if (exp2 .ge. fjmod) goto 1020
c
        mu = chic2
        v  = urandom()
        vm = v + mu
        accept = (urandom() .lt. ((v + mugg) / vm))
c
        if (accept) then
c
          vv = 0.89d3 * mu * wsq / vm
          if (vv .gt. 1) then
c
c           Make correction for finite nuclear size.
c           f is the nuclear form factor.
c
            f = 0.14d0 + 13.76d0 / (16.d0 + vv ** 2)
            accept = (urandom() .lt. f)
c
          endif
c
          if (accept) then
c
c           Deflecting the particle.
c
            costheta = (v - mu) / vm
            xyz(1)   = frecord(ixpx)
            xyz(2)   = frecord(ixpy)
            xyz(3)   = frecord(ixpz)
c
            call deflectr(xyz, costheta, sintheta, cdphi, sdphi,
     +                    frecord(ixpx))
c
          endif
        endif
c
        exp2 = exp2 - log(urandomt(0.d0))
c
        goto 1010
 1020   continue
        exp2 = exp2 - fjmod
c
      endif
c
c     Multiple scattering.
c
      if (path .gt. 0.2d0) then
        mu = cmul10 + cmul11 * rootpath + cmul12 * path
        b  = mu * sqrt(-log(urandomt(0.0d0)))
      else
        mu = cmul00 + log(path)
        b  = sqrt(-mu * log(urandomt(0.0d0)))
      endif
c
c     Deflecting.
c
      costheta = cos(chic * b)
      xyz(1)   = frecord(ixpx)
      xyz(2)   = frecord(ixpy)
      xyz(3)   = frecord(ixpz)
c
      call deflectr(xyz, costheta, sintheta, cdphi, sdphi,
     +              frecord(ixpx))
c
      return
      end
c     --- End of routine coulscatter
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine twobdecay(kenergy, massin, uin, massa, massb,
     +                     egyout, ldu, uout)
c
c     Kinematics of decay of particle of mass massin to massa, massb.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1998.
c
c
c     Arguments:
c     =========
c
c     kenergy......... (input, double precision) Kinetic energy of "in"
c                      particle.
c     massin.......... (input, double precision) Mass of "in" particle.
c     uin............. (input, double precision, array(3)) Direction of
c                      motion of "in" particle.
c     massa, massb.... (input, double precision) Masses of "out"
c                      particles.
c     egyout.......... (output, double precision, array(2)) Kinetic
c                      energies of "out" particles.
c     ldu............. (input, integer) Leading dimension of array
c                      "uout".
c     uout............ (output, double precision, array(ldu, 3))
c                      Direction of motion of "out" particles.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  kenergy, massin
      double precision  uin(3)
      double precision  massa, massb
      double precision  egyout(2)
      integer           ldu
      double precision  uout(ldu, 3)
c
c     Declaration of internal variables and arrays.
c
      double precision  costh, sinth, ein, beta, massina, crot(2)
      double precision  ua, ub, pla, plb, pt2, efrac, pfrac
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Determining random direction of particle a
c
      costh   = 1 - 2 * urandom()
      sinth   = sqrt(1 - costh ** 2)
c
c     Evaluating parameters of outgoing particles.
c
      ein       = kenergy + massin
      beta      = sqrt(abs(1 - (massin / ein) ** 2))
      massina   = (massa / massin) ** 2
      efrac     = 0.5d0 * (1 + massina - (massb / massin) ** 2)
      pfrac     = sqrt(max(0.d0, efrac ** 2 - massina))
c
      ua        = ein * (efrac + beta * costh * pfrac)
      ub        = ein - ua
c
      egyout(1) = ua - massa
      egyout(2) = ub - massb
c
c     Evaluating the emerging directions in the lab system.
c
      pla       = ein * (costh * pfrac + beta * efrac)
      pt2       = (massin * pfrac * sinth) ** 2
      crot(1)   = pla / sqrt(pla ** 2 + pt2)
      plb       = ein * beta - pla
      crot(2)   = plb / sqrt(plb ** 2 + pt2)
c
      call pdeflectr(uin, crot, ldu, uout)
c
      return
      end
c     --- End of routine twobdecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine threebdecay(kenergy, massin, uin, massa, massb, massc,
     +                       egyout, ldu, uout)
c
c     Kinematics of decay of particle of mass massin to massa, massb
c     and massc.
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c
c     Arguments:
c     =========
c
c     kenergy......... (input, double precision) Kinetic energy of "in"
c                      particle.
c     massin.......... (input, double precision) Mass of "in" particle.
c     uin............. (input, double precision, array(3)) Direction of
c                      motion of "in" particle.
c     massa, massb
c     massc........... (input, double precision) Masses of "out"
c                      particles. massb and massc should not be
c                      simultaneously zero.
c     egyout.......... (output, double precision, array(3)) Kinetic
c                      energies of "out" particles.
c     ldu............. (input, integer) Leading dimension of array
c                      "uout".
c     uout............ (output, double precision, array(ldu, 3))
c                      Direction of motion of "out" particles.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  kenergy, massin
      double precision  uin(3)
      double precision  massa, massb, massc
      double precision  egyout(3)
      integer           ldu
      double precision  uout(ldu, 3)
c
c     Declaration of internal variables and arrays.
c
      double precision  masq, mbsq, mcsq
      double precision  minsqa, mcnbsq, sfn, sf, esa, esc
      double precision  mbc0sq, mbc1sq, mbc, mbc2, mbcsq, ke2
      double precision  ui2(3)
      integer           i
      double precision  urandom, urandomt
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating the "intermediate mass".
c
      masq   = massa ** 2
      mbsq   = massb ** 2
      mcsq   = massc ** 2
      minsqa = massin ** 2 - masq
      mcnbsq = mcsq - mbsq
      mbc0sq = (massb + massc) ** 2
      mbc1sq = (massin - massa) ** 2
      sfn    = mbc1sq - mbc0sq
c
 1010 continue
c
      mbcsq = mbc0sq + urandomt(0.d0) * sfn
      mbc   = sqrt(mbcsq)
      mbc2  = 2 * mbc
c
      esa   = (minsqa - mbcsq) / mbc2
      esc   = (mcnbsq + mbcsq) / mbc2
      sf    = 2 * sqrt(abs((esa ** 2 - masq) * (esc ** 2 - mcsq)))
c
      if ((sfn * urandom()) .ge. sf) goto 1010
c
c     Intermediate two body decay.
c
      call twobdecay(kenergy, massin, uin, massa, mbc,
     +               egyout(1), ldu, uout)
c
c     Decay of intermediate state.
c
      ke2 = egyout(2)
      do i = 1, 3
        ui2(i) = uout(i, 2)
      enddo
c
      call twobdecay(ke2, mbc, ui2, massb, massc,
     +               egyout(2), ldu, uout(1, 2))
c
      return
      end
c     --- End of routine threebdecay
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine lpmeffect(ephoton, ee1, ee2, refdepth, pairprod, goon)
c
c     Checking Landau-Pomeranchuk-Migdal effect of suppression of
c     radiative electron-photon processes at high energies.
c     This routine implements Migdal's theory virtually exactly, fixing
c     the bugs of the original implementation developed by A. M.
c     Hillas. Dielectric suppression of low energy photons is also
c     taken into account.
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c
c     Arguments:
c     =========
c
c     ephoton......... (input, double precision) Photon energy (GeV).
c     ee1, ee2........ (input, double precision) Energy of the e+-
c                      particles involved (GeV).
c     refdepth........ (input, double precision) Current atmospheric
c                      depth (g/cm2).
c     pairprod........ (input, logical) True if the event is pair
c                      production.
c     goon............ (output, logical) True if the effect does
c                      not suppress the event.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  ephoton, ee1, ee2, refdepth
      logical           pairprod, goon
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  s, ssq, ssc, xi, glpm, phi, egsq, ab, f
      double precision  y, yrat
      double precision  urandom
c
c     Coefficients for the expansion in terms of rational functions of
c     functions G and phi of Migdal's theory.
c     The relative errors of the estimations are less than 0.15% and
c     0.12% for G and phi respectively.
c
      double precision  a2g, a3g, a4g, b1g, b2g, b3g
      double precision  a1p, a2p, a3p, b1p, b2p
c
      data              a2g    / 37.6991118d0 /
      data              a3g    / 48.6420105d0 /
      data              a4g    / 110.116417d0 /
      data              b1g    / 7.47827716d0 /
      data              b2g    / 30.8454520d0 /
      data              b3g    / 50.6094675d0 / 
c
      data              a1p    / 6.d0         /
      data              a2p    / 11.1581413d0 /
      data              a3p    / 18.5565527d0 /
      data              b1p    / 5.06157104d0 /
      data              b2p    / 11.4280547d0 /
c
c     FIRST EXECUTABLE STATEMENT
c
      goon = .true.
c
      ssq = lpmfac * ephoton / (ee1 * ee2 * refdepth)
c
      if (ssq .lt. 1.0d0) then
c
c       LPM effect is significant.
c
c       Solving the recurrence between s and xi(s). This normally
c       converges in a single iteration.
c
        if (ssq .lt. s1sqlpm) then
          xi = 2.d0
        else
          xi = 1.d0 + foverzlog * log(ssq)
        endif
        ssq = ssq / xi
        s   = sqrt(ssq)
        ssc = s
c
        egsq   = ephoton ** 2
        ab     = 4 * ee1 * ee2
c
        if (pairprod) then
c
          ab     = - ab
c
        else
c
c         Taking into account dielectric suppression.
c
          y    = ephoton / ee1
          yrat = ydielfac * refdepth / (y * y)
c
c         Dielectric suppression is negligible when ydiel/y is smaller
c         than 1/2: "dielsthreshold" is set to 0.25 when dielectric
c         suppression is enabled.
c
          if (yrat .gt. dielsthreshold) s = s * (1 + yrat)
c
        endif
c
c       Evaluating the functions G(s) and Phi(s).
c
        glpm = ssq * (a2g + s * (a3g + s * a4g)) /
     +         (1 + s * (b1g + s * (b2g + s * (b3g + s * a4g))))
        phi  = ssc * (a1p + s * (a2p + s * a3p)) /
     +         (1 + s * (b1p + s * (b2p + s * a3p)))
c
c       Evaluating the correction factor.
c
        f = xi * (egsq * glpm + (2 * egsq + ab) * phi)
     +         / (3 * egsq + ab)
c
        if (urandom() .gt. f) goon = .false.
c
      endif
c
      return
      end
c     --- End of routine lpmeffect
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function mupp0rho0g(v)
c
c     The theoretical distribution of the differential probability
c     distribution for muonic pair production, accordingly with the
c     theory of R. P. Kokoulin and A. A. Petrukhin, Proc. 12th ICRC
c     (Hobart), 6, 2436 (1971).
c
c     This function needs some global and energy dependent
c     initializations, that are performed in routines "modelinirun"
c     and "heavychadv", respectively.
c     
c     Written by: A. N. Cillis and S. J. Sciutto, La Plata 2000.
c
c
c     Arguments:
c     =========
c
c     v............... (input, double precision) Ratio between the
c                      energy of the e+e- pair and the (total) muon
c                      energy. v must belong to the [vmin, vmax]
c                      interval (energy dependent range).
c
c     Return value: (double precision) Probability rejection factor
c     ============  (in the interval [0, 1]).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'modelpar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  mupp0rho0g
      double precision  v
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  memu, memu2, memu22
      parameter         (memu   = electronmass / muonmass)
      parameter         (memu2  = memu * memu)
      parameter         (memu22 = 1 / (2 * memu2))
c
      double precision   v1, beta, epsilon, epsilon1, upueps
      double precision   beta2, beta3, beta4
      double precision   numye, denye, Ye
      double precision   numymu, denymu, Ymu
      double precision   fac1, fac1bis, fac2, fac3, fac3bis
      double precision   fac7, fac9, fac11, fac12
      double precision   Le, Lmu, phi
c
c     FIRST EXECUTABLE STATEMENT
c
c     Some variables related with v.
c
      v1       = 1 - v
      beta2    = v * v / v1
      beta     = beta2 / 2
      beta3    = 3 * beta
      beta4    = 2 * beta2
      epsilon  = memu22 * beta
      epsilon1 = 1 + epsilon
      upueps   = epsilon1 / epsilon
c
c     Evaluation of Phie and general factors.
c
c     Auxiliary function Ye
c
      numye = 5 + beta4
      denye = 2 * (1 + beta3) * log(2 + upueps) - beta4
      Ye    = numye / denye
c
c     Some general factors.
c
      fac1 = epsilon1 * (1 + Ye)
      fac2 = RZeff3 * sqrt(fac1)
      fac3 = mesqrte2f * fac1
      fac7 = 2 + beta2
      fac9 = 1 - beta
c
c     Evaluating Le.  
c
      Le = log(fac2 / (1 + (fac3 / v))) - log(1 + muppf1 * fac1) / 2
c
c     e component of phi
c
      phi = Le * ((fac7 + 3 * epsilon) * log(upueps) +
     +            fac9 / epsilon1 - 3)
c
      if (v .ge. 0.03d0) then
c
c       We are in the region where Phimu cannot be neglected.
c
        numymu = 4 + beta3
        denymu = 1 + (1.5d0 + beta2) * log(3 + epsilon)
        Ymu    = numymu / denymu
c
c       Evaluating Lmu.
c
        fac1bis = epsilon1 * (1 + Ymu)
        fac3bis = mesqrte2f * fac1bis
        fac11   = 1 + 1.5d0 * beta
        fac12   = 1 + beta2
c
        Lmu = log(muppf6 / (1 + (fac3bis / v)))
c
c       Adding mu component to phi
c
        phi = phi + memu2 * Lmu *
     +              ((fac11 - fac12 / epsilon) * log(epsilon1) + 
     +               fac9 / upueps + fac12)
c
      endif
c
      if (phi .gt. 0) then
         mupp0rho0g = muppugmax * v1 * phi
      else
         mupp0rho0g = 0
      endif
c
      return
      end
c     --- End of routine mupp0rho0g
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine hmfpinit(iextmfp, usextmfp, usextnucmfp, threshold)
c
c     Initializing the algorithms to evaluate low energy hadronic mean
c     free paths.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2002.
c
c
c     Arguments:
c     =========
c
c     iextmfp......... (input, integer) Index labelling the selected
c                      high energy MFP set.
c     usextmfp........ (input, logical) TRUE if the external MFP's are
c                      enabled.
c     usextnucmfp..... (input, logical) TRUE if the external nucleus-
c                      nucleus MFP's are enabled.
c     threshold....... (input, double precision) Threshold energy
c                      separating the low and high-energy regions.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hmfppar.f'
      include 'pclepar.f'
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      integer           iextmfp
      logical           usextmfp, usextnucmfp
      double precision  threshold
c
c     Declaration of shared data.
c
      include 'hmfpcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     Coefficients for high energy MFP parameterization. All MFP's
c     can be parameterized in the form
c
c                                            2         3
c                         1 + p2 * u + p3 * u  + p4 * u
c     mfp(Egy) = p1 ------------------------------------------
c                                       2         3         4
c                    1 + p5 * u + p6 * u  + p7 * u  + p8 * u
c
c     where u = ln(Egy [GeV]) , 0.1 GeV < Egy < 1e13 GeV.
c
      include 'hmfphighedata.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the particle code to mfp set conversion arrays.
c
      do i = -maxpcle, maxpcle
        pcode2lowemfpset(i)  = pset
        pcode2highemfpset(i) = nucleonhset
      enddo
c
c     High energy region.
c
      pcode2highemfpset(pizerocode)  = pionhset
      pcode2highemfpset(piminuscode) = pionhset
      pcode2highemfpset(pipluscode)  = pionhset
      pcode2highemfpset(k0scode)     = kaonhset
      pcode2highemfpset(k0lcode)     = kaonhset
      pcode2highemfpset(kminuscode)  = kaonhset
      pcode2highemfpset(kpluscode)   = kaonhset
      pcode2highemfpset(etacode)     = pionhset
      pcode2highemfpset(neutroncode) = nucleonhset
      pcode2highemfpset(nbarcode)    = nucleonhset
      pcode2highemfpset(pbarcode)    = nucleonhset
c
c     Low energy region.
c
      pcode2lowemfpset(pizerocode)  = pi0set
      pcode2lowemfpset(piminuscode) = piminusset
      pcode2lowemfpset(pipluscode)  = piplusset
      pcode2lowemfpset(k0scode)     = k0sset
      pcode2lowemfpset(k0lcode)     = k0lset
      pcode2lowemfpset(kminuscode)  = kminusset
      pcode2lowemfpset(kpluscode)   = kplusset
      pcode2lowemfpset(etacode)     = etaset
      pcode2lowemfpset(neutroncode) = nset
      pcode2lowemfpset(nbarcode)    = nbarset
      pcode2lowemfpset(pbarcode)    = pbarset
c
c     Setting the arrays for the high energy MFP's
c
      do j = 1, totmfphsets
        do i = 1, 8
          mfp8ppk(i, j) = mfp8(i, j, iextmfp)
        enddo
      enddo
c
c     Other parameters.
c
      externalmfp    = usextmfp
      externalnucmfp = usextnucmfp
      mfpthreshold   = threshold
c
      return
c
      end
c     --- End of routine hmfpinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function hmfplowe(pcode, lnke)
c
c     Low energy hadronic mean free paths.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
c     Arguments:
c     =========
c
c     pcode.......... (input, integer) Particle code. MUST be a meson
c                     or a baryon.
c     lnke........... (input, double precision) Natural logarithm of
c                     the particle's kinetic energy (in GeV).
c
c
c     Return value: (double precision) The (total) hadronic mean free
c     ============  path (g/cm2), for the particle-air collision.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hmfppar.f'
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      double precision  hmfplowe
      integer           pcode
      double precision  lnke
c
c     Declaration of shared data.
c
      include 'hmfpcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           iset, i, j
c
c     Parameterization data (results of fits to experimental data).
c
      include 'hmfplowedata.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Selecting the data set that must be used.
c
      iset = pcode2lowemfpset(pcode)
c
      if (lnke .ge. mfplglim0(0, iset)) then
c
        hmfplowe = mfplasym1(iset) * (1 + lnke * mfplasya(iset)) /
     +             (1 + lnke * (mfplasyb(iset) +
     +                  lnke * mfplasyc(iset)))
c
      else
c
        do i = 1, mfplnseg(iset)
          j = i
          if (lnke .ge. mfplglim0(i, iset)) goto 1010
        enddo
 1010   continue
c
        hmfplowe = exp(mfpla(j, iset) + lnke * mfplb(j, iset))
c
      endif
c
      return
c
      end
c     --- End of routine hmfplowe
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function nucnucmfp(a, z, ke)
c
c     Nucleus-nucleus (nuc-air) hadronic mean free paths.
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
c
c     Arguments:
c     =========
c
c     a.............. (input, integer) Nucleus mass number.
c     z.............. (input, integer) Nucleus charge.
c     ke............. (input, double precision) kinetic energy per
c                     nucleon (in GeV).
c
c     Return value: (double precision) The (total) hadronic mean free
c     ============  path (g/cm2), for the nucleus-air collision.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'hmfppar.f'
c
c     Declaration of arguments.
c
      double precision  nucnucmfp
      integer           a, z
      double precision  ke
c
c     Declaration of shared data.
c
      include 'hmfpcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  u, mfp
      double precision  extnucnucmfp, hmfplowe
c
c     FIRST EXECUTABLE STATEMENT
c
      if (ke .gt. mfpthreshold) then

        if (externalnucmfp) then
c
c         Evaluating using external (model-dependent) cross sections.
c
          nucnucmfp = extnucnucmfp(a, z, ke)
          return
c
        else
c
c         Using the selected proton mfp's properly scaled.
c
          u   = log(ke)
          mfp = mfp8ppk(1, nucleonhset) *
     +                     (1 + u * (mfp8ppk(2, nucleonhset) +
     +                          u * (mfp8ppk(3, nucleonhset) +
     +                          u * mfp8ppk(4, nucleonhset)))) /
     +                     (1 + u * (mfp8ppk(5, nucleonhset) +
     +                          u * (mfp8ppk(6, nucleonhset) +
     +                          u * (mfp8ppk(7, nucleonhset) +
     +                          u * mfp8ppk(8, nucleonhset)))))
        endif
c
      else
c
c       Using low energy cross sections.
c
        u   = log(ke)
        mfp = hmfplowe(protoncode, u)
c
      endif
c
c     Energy-mass number correction.
c
      mfp = mfp * 0.957527d0 /
     +            (9.10995d-2 + a ** 0.638518d0) ** (2 / 3.d0)
      mfp = mfp * (1 + 0.00593d0 * log(dfloat(a)) * (u - 10.362d0))
c
      nucnucmfp = mfp
c
      return
      end
c     --- End of routine nucnucmfp
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine gnucoll(pwt, fenergy, up, plol, maxsec, ldf,
     +                   nsec, seccode, esec, upsec, iws, fws)
c
c     Gamma-nucleus collision routine.
c     If the particle energy is larger than a given threshold, then
c     an external collision routine (SIBYLL for example) is called;
c     Otherwise the Hillas splitting algorithm is used to generate
c     the secondaries.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000, 2001, 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     pwt............ (input, double precision) Particle weight.
c     fenergy........ (input, double precision) The free energy of the
c                     particle.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     plol........... (input, integer) Last observing level crossed
c                     by the primary particle.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integr) Leading dimension of array upsec.
c     nsec........... (output, integer) Number of generated secondaries
c                     energetic secondaries.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries.
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries.
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries.
c     iws............ (scratch, integer, array(maxsec)) Integer working
c                     space.
c     fws............ (scratch, double precision, array(maxsec)) Real
c                     working space.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      double precision  pwt, fenergy
      double precision  up(3)
      integer           plol, maxsec, ldf, nsec
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           iws(maxsec)
      double precision  fws(maxsec)
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           atarget
      logical           extcoll
      double precision  urandom
      integer           airtarsample
c
c     FIRST EXECUTABLE STATEMENT
c
c     Determining the target mass number.
c
      atarget = airtarsample()
c
c     Switching accordingly with the primary energy.
c
      if (fenergy .ge. highcollthresh2(1)) then
        extcoll = .true.
      else if (fenergy .le. highcollthresh(1)) then
        extcoll = .false.
      else
        extcoll = (urandom() .lt.
     +             (highcollfac2(1) * (fenergy - highcollthresh(1))))
      endif
c
      if (extcoll) then
c
c       High energy collision. Calling external model routine.
c
        call extgnucoll(fenergy, up, atarget, maxsec, ldf,
     +                  nsec, seccode, esec, upsec, iws, fws)
        hadcollcalls(1) = hadcollcalls(1) + 1
c
      else
c
c       Low energy collision. Using Hillas' splitting algorithm.
c
        call gammasplit(fenergy, up, atarget, maxsec, ldf,
     +                  nsec, seccode, esec, upsec, iws, fws)
        hadcollcalls(2) = hadcollcalls(2) + 1
c
      endif
c
c     Evaluating energy losses.
c
      call nucleloss(gammacode, pwt, fenergy, plol, nsec, esec)
c
      return
      end
c     --- End of routine gnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine hnucoll(isbaryon, pcode, pwt, fenergy, up, plol,
     +                   maxsec, ldf, nsec, seccode,
     +                   esec, upsec, iws, fws)
c
c     Hadron-nucleus collision routine.
c     If the particle energy is larger than a given threshold, then
c     an external collision routine (SIBYLL for example) is called;
c     Otherwise the Hillas splitting algorithm is used to generate
c     the secondaries.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000, 2001, 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     isbaryon....... (input, logical) True if the primary particle
c                     is a baryon.
c     pcode.......... (input, integer) Primary particle code.
c     pwt............ (input, double precision) Particle weight.
c     fenergy........ (input, double precision) The free energy of the
c                     particle.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     plol........... (input, integer) Last observing level crossed
c                     by the primary particle.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integr) Leading dimension of array upsec.
c     nsec........... (output, integer) Number of generated secondaries
c                     energetic secondaries.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries.
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries.
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries.
c     iws............ (scratch, integer, array(maxsec)) Integer working
c                     space.
c     fws............ (scratch, double precision, array(maxsec)) Real
c                     working space.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           isbaryon
      integer           pcode
      double precision  pwt, fenergy
      double precision  up(3)
      integer           plol, maxsec, ldf, nsec
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           iws(maxsec)
      double precision  fws(maxsec)
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           atarget
      logical           extcoll
      double precision  urandom
      integer           airtarsample
c
c     FIRST EXECUTABLE STATEMENT
c
c     Determining the target mass number.
c
      atarget = airtarsample()
c
c     Switching accordingly with the primary energy.
c
      if (fenergy .ge. highcollthresh2(1)) then
        extcoll = .true.
      else if (fenergy .le. highcollthresh(1)) then
        extcoll = .false.
      else
        extcoll = (urandom() .lt.
     +             (highcollfac2(1) * (fenergy - highcollthresh(1))))
      endif
c
      if (extcoll) then
c
c       High energy collision. Calling external model routine.
c
        call exthnucoll(isbaryon, pcode, fenergy, up, atarget,
     +                  maxsec, ldf, nsec, seccode,
     +                  esec, upsec, iws, fws)
        hadcollcalls(1) = hadcollcalls(1) + 1
c
      else
c
c       Low energy collision. Using Hillas' splitting algorithm.
c
        call heavysplit(isbaryon, pcode, fenergy, up, atarget,
     +                  maxsec, ldf, nsec, seccode, esec, upsec,
     +                  iws, fws)
        hadcollcalls(2) = hadcollcalls(2) + 1
c
      endif
c
c     Evaluating energy losses.
c
      call nucleloss(pcode, pwt, fenergy, plol, nsec, esec)
c
      return
      end
c     --- End of routine hnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine nucnucoll(pcode, massa, nucz, pwt, kenucleon,
     +                     up, plol, maxsec, ldf, nsec, nfrag, efrag,
     +                     seccode, esec, upsec, iws, fws)
c
c     Nucleus-nucleus collision routine.
c     If the particle energy is larger than a given threshold, then
c     an external collision routine (SIBYLL for example) is called;
c     Otherwise a built-in fragmentation algorithm is used to generate
c     the secondaries.
c
c     Written by: S. J. Sciutto, La Plata 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     pcode.......... (input, integer) Primary particle code.
c     massa.......... (input, integer) Projectile mass number.
c     nucz........... (input, integer) Projectile charge number.
c     pwt............ (input, double precision) Particle weight.
c     kenucleon...... (input, double precision) The average free energy
c                     of the projectile's nucleons.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     plol........... (input, integer) Last observing level crossed
c                     by the primary particle.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integr) Leading dimension of array upsec.
c     nsec........... (output, integer) Number of generated secondaries
c     nfrag.......... (output, integer) Total number of secondary
c                     nuclear fragments and elastically scattered
c                     nucleons.
c     efrag.......... (output, double precision) Total energy of the
c                     emerging fragments.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries. seccode(i),
c                     i = 1,...,nfrag, contains the codes of the
c                     secondary nuclear fragments and elastically
c                     scattered nucleons. For i = nfrag+1,...,nsec,
c                     the array contains the codes of the secondaries
c                     generated inelastically.
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries,
c                     ordered similarly as "seccode".
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries, ordered similarly as
c                     "seccode".
c     iws............ (scratch, integer, array(maxsec)) Integer working
c                     space.
c     fws............ (scratch, double precision, array(maxsec)) Real
c                     working space.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           pcode, massa, nucz
      double precision  pwt, kenucleon
      double precision  up(3)
      integer           plol, maxsec, ldf, nsec, nfrag
      double precision  efrag
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           iws(maxsec)
      double precision  fws(maxsec)
c
c     Declaration of shared data.
c
      include 'modelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           atarget
      logical           extcoll
      double precision  urandom
      integer           airtarsample
c
c     FIRST EXECUTABLE STATEMENT
c
c     Determining the target mass number.
c
      atarget = airtarsample()
c
c     Switching accordingly with the primary energy.
c
      if (kenucleon .ge. highcollthresh2(2)) then
        extcoll = .true.
      else if (kenucleon .le. highcollthresh(2)) then
        extcoll = .false.
      else
        extcoll = (urandom() .lt.
     +             (highcollfac2(2) * (kenucleon - highcollthresh(2))))
      endif
c
      if (extcoll) then
c
c       High energy collision. Calling external model routine.
c
        call extnucnucoll(pcode, massa, nucz, kenucleon, up, atarget,
     +                    maxsec, ldf, nsec, nfrag, efrag, seccode,
     +                    esec, upsec, iws, fws)
        hadcollcalls(3) = hadcollcalls(3) + 1
c
c       Evaluating energy losses.
c
        call nucleloss(pcode, pwt, massa * kenucleon, plol, nsec, esec)
c
      else
c
c       Low energy collision. Using built-in algorithm (energy losses
c       are evaluated internally).
c
        call nucfragment(massa, nucz, pwt, kenucleon, up, atarget,
     +                   plol, maxsec, ldf, nsec, nfrag, efrag,
     +                   seccode, esec, upsec, iws, fws)
        hadcollcalls(4) = hadcollcalls(4) + 1
c
      endif
c
      return
      end
c     --- End of routine nucnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine nucleloss(pcode, pwt, penergy, plol, nsec, esec)
c
c     Evaluating energy lost during nuclear collisions.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2001.
c
c
c     Arguments:
c     =========
c
c     pcode.......... (input, integer) Primary particle code.
c     pwt............ (input, double precision) Particle weight.
c     penergy........ (input, double precision) Initial energy.
c     plol........... (input, integer) Last observing level crossed
c                     by the primary particle.
c     nsec........... (input, integer) Number of generated secondaries
c                     energetic secondaries.
c     esec........... (output, double precision, array(nsec))
c                     Energies of the generated secondaries.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           pcode, plol
      double precision  pwt, penergy
      integer           nsec
      double precision  esec(nsec)
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  totegy
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating energy losses.
c
      totegy = 0
c
      do i = 1, nsec
        totegy = totegy + esec(i)
      enddo
c
      if (totegy .lt. penergy) then
        call plostegy(pcode, plol, pwt * (penergy - totegy))
      endif
c
      return
      end
c     --- End of routine nucleloss
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'modelutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
