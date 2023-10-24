c
c     FILE: splitmodel.f                    Creation date: 30/JUN/2001.
c                                       LAST MODIFICATION: 13/MAY/2005.
c
c     New implementation of the Hillas Splitting Algorithm for
c     hadronic collisions.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine gammasplit(fenergy, up, atarget, maxsec, ldf,
     +                      nsec, seccode, esec, upsec, pk, ws)
c
c
c     Hillas's splitting algorithm (A. M. Hillas, Proc. of the Paris
c     Workshop on Cascade simulations, J. Linsley and A. M. Hillas
c     (eds.), p 39 (1981).). Version suitable for gammas.
c
c     This implementation includes major improvements from the original
c     algorithm, regarding mainly the procedure to assign identity to
c     the energy packets that are generated during the splitting
c     process. See routine "splitkernel".
c
c     Written by: S. J. Sciutto, La Plata 2001, 2005.
c
c
c     Arguments:
c     =========
c
c     fenergy........ (input, double precision) The free energy of the
c                     particle.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     atarget........ (input, integer) Target nucleus mass number.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integer) Leading dimension of array
c                     upsec.
c     nsec........... (output, integer) Number of generated
c                     secondaries.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries.
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries.
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries.
c     pk............. (scratch, integer, array(maxsec)) Integer working
c                     space.
c     ws............. (scratch, double precision, array(maxsec)) Real
c                     working space.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclecodes.f'
      include 'pclepar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  fenergy
      double precision  up(3)
      integer           atarget, maxsec, ldf, nsec
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           pk(maxsec)
      double precision  ws(maxsec)
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
      include 'splitmodeldata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           j, jj
      double precision  lnu, unlu
      double precision  a, b, e
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      lnu   = log(fenergy)
      unlu  = 1 / (lnu + 3.5d0)
c
c     Evaluating probability of generating secondary nucleons.
c
      if (fenergy .gt. 0.5d0) then
c
        prbaryon = 0.27363d0 - lnu * (0.13335d0
     +                       - lnu * (0.04016d0
     +                       - lnu * (0.00631d0
     +                       + lnu * (1.4865d-5 * lnu - 0.00050d0))))
        prkaon   = max(0.01562d0 * lnu - 0.02610d0, 0.d0)
        prnucd   = 1
c
      else
c
c       Below threshold there is no interaction and the primary is
c       returned unchanged.
c
        if (fenergy .le. 0.145d0) then
          nsec        = 1
          seccode(1)  = gammacode
          esec(1)     = fenergy
          upsec(1, 1) = up(1)
          upsec(2, 1) = up(2)
          upsec(3, 1) = up(3)
          return
        endif
c
        prbaryon = max(0.475d0 * (lnu + 1.514d0), 0.d0)
        prkaon   = 0
        prnucd   = 2 * prbaryon
c
      endif
c
      prnucdrf = prbaryon
c
      a = 0.22184d0 + lnu * (0.02318d0
     +              + lnu * (4.7273d-4 - 1.9567d-4 * lnu))
      prqfam(1, pion)   = a
      prqfam(2, pion)   = a
      prqfam(1, baryon) = 0.51010d0 - 0.01121d0 * lnu
      a                 = max(0.02440d0 * lnu - 0.08411d0, 0d0)
      prqfam(2, baryon) = a
      prqfam(3, baryon) = a
c
c     Uniform distribution for charge of kaons.
c
      do jj = 0, 3
        prqfam(jj, kaon) = 0.25d0
      enddo
c
c     Pt distributions.
c
      call ptparset(fenergy, pizeromass)
c
c     Initial splitting of the available energy.
c
c     The first particle is the leading particle, that will be taken
c     as a pi0.
c
      pk(1)   = leader
      e       = urandom() * urandom() * fenergy
      esec(1) = e
      a       = fenergy - e
c
      do jj = 2, 8
        esec(jj) = a * urandom()
        a        = a - esec(jj)
      enddo
c
      if (fenergy .gt. 75.0d0) then
        j = 7 * urandom()
      else
        if (urandom() .lt. prob2pi) then
          j = 0
        else
          b = max(1.621d0 * lnu, 1.d0)
          j = b * urandom()
        endif
      endif
      j = j + 2
c
      do jj = 2, j - 1
        e = e + esec(jj)
      enddo
      esec(j) = fenergy - e
c
      do jj = 2, j
        pk(jj) = glueball
      enddo
c
      qqsp = qqspmes
      nsec = j
c
c     SPLITTING LOOP.
c
c     Invoking the energy splitting algorithm that generates the
c     emerging secondaries.
c
      call splitkernel(up, maxsec, ldf, 99, gpion, 0,
     +                 nsec, pk, seccode, esec, upsec, ws)
c
      return
c
      end
c     --- End of routine gammasplit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine heavysplit(isbaryon, pcode, fenergy, up, atarget,
     +                      maxsec, ldf,
     +                      nsec, seccode, esec, upsec, pk, ws)
c
c     Hillas's splitting algorithm (A. M. Hillas, Proc. of the Paris
c     Workshop on Cascade simulations, J. Linsley and A. M. Hillas
c     (eds.), p 39 (1981).). Version suitable for hadronic projectiles
c     (mesons, nucleons, etc.).
c
c     This implementation includes major improvements from the original
c     algorithm, regarding mainly the procedure to assign identity to
c     the energy packets that are generated during the splitting
c     process. See routine "splitkernel".
c
c     Written by: S. J. Sciutto, La Plata 2001, 2005.
c
c
c     Arguments:
c     =========
c
c     isbaryon....... (input, logical) True if the primary particle
c                     is a baryon.
c     pcode.......... (input, integer) Primary particle code.
c     fenergy........ (input, double precision) The free energy of the
c                     particle.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     atarget........ (input, integer) Target nucleus mass number.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integer) Leading dimension of array
c                     upsec.
c     nsec........... (output, integer) Number of generated
c                     secondaries.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries.
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries.
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries.
c     pk............. (scratch, integer, array(maxsec)) Integer working
c                     space.
c     ws............. (scratch, double precision, array(maxsec)) Real
c                     working space.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclecodes.f'
      include 'pclepar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      logical           isbaryon
      integer           pcode
      double precision  fenergy
      double precision  up(3)
      integer           atarget, maxsec, ldf, nsec
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           pk(maxsec)
      double precision  ws(maxsec)
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
      include 'splitmodeldata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           k, j, jj
      integer           pktype, qin, altqin, qleader, primgrp
      double precision  pkeepch, fenergy0
      double precision  lnu, unlu, a, b, e, eleader
      double precision  qqspmin, qqspmax
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Basic initialization.
c
      qin = pcleq(pcode)
c
      if (fenergy .ge. 2000.d0) then
        lnu = 7.60090246d0
      else if (fenergy .le. 0.1d0) then
        lnu = -2.302585093d0
      else
        lnu = log(fenergy)
      endif
c
      unlu     = 1 / (lnu + 3.5d0)
      prnucd   = 0.9067d0 + 0.3512d0 * unlu
      prnucdrf = 0.1960d0 + 0.8430d0 * unlu
c
c     PRIMARY DEPENDENT INITIALIZATION.
c
      if (isbaryon) then
c
c       BARYON (NUCLEON OR ANTINUCLEON) PRIMARY.
c       =======================================
c
        fenergy0 = fenergy
        pktype   = leader
        altqin   = 1 - qin
        prnucdrf = min(0.10d0 + 4.05d0 * unlu, 1.d0)
        prnucd   = 1
c
        if (pcode .eq. protoncode) then
c
          pkeepch  = max(0.64044d0 - 0.03164d0 * lnu, 0.46d0)
          prbaryon = 0.75871d0 - lnu * (0.02380d0
     +                         + lnu * (0.08391d0
     +                         - lnu * (0.01950d0
     +                         + lnu * (4.4193d-5 * lnu - 0.00159d0))))
          prbaryon = min(prbaryon, 0.625d0)
          prkaon   = max(0.01755d0 * lnu - 0.03524d0, 0d0)
c
          prqfam(1, pion)   = max(0.47325d0 - 0.03344 * lnu, 0.329d0)
          prqfam(2, pion)   = min(0.17988d0 + 0.03253d0 * lnu, 0.313d0)
          prqfam(1, baryon) = 0.62047d0 - 0.02139d0 * lnu
          a                 = max(0.02633d0 * lnu - 0.08890d0, 0d0)
          prqfam(2, baryon) = a
          prqfam(3, baryon) = a
c
        else if (pcode .eq. pbarcode) then
c
          pkeepch  = min(0.39630d0 + 0.01425d0 * lnu, 0.50d0)
          altqin   = 3

          prbaryon = 0.58608d0 - lnu * (0.10488d0
     +                         - lnu * (0.00370d0
     +                         - lnu * (0.00091d0
     +                         + lnu * (1.167d-5 * lnu - 0.00023d0))))
          prkaon   = max(0.01821d0 * lnu - 0.04007d0, 0d0)
c
          prqfam(1, pion)   = 0.26946d0 + lnu * (0.01328d0
     +                                  - lnu *  0.00095d0)
          prqfam(2, pion)   = 0.43367d0 - lnu * (0.01831d0
     +                                  - lnu *  0.00043d0)
          prqfam(1, baryon) = 0.37093d0 + 0.00909d0 * lnu
          prqfam(2, baryon) = 0.22280d0 - 0.01616d0 * lnu
          prqfam(3, baryon) = 0
c
c         A simple but effective way of simulating p-pbar
c         annihilation.
c
          if ((((1 + 5.6d0 * fenergy) ** 0.8d0) * urandom()) .lt. 1)
     +    then
            pktype   = glueball
            fenergy0 = fenergy0 + 2 * protonmass
            prbaryon = 0.55d0 * prbaryon
          endif
c
        else if (pcode .eq. nbarcode) then
c
          pkeepch  = min(0.39630d0 + 0.01425d0 * lnu, 0.50d0)
          qin      =  3
          altqin   = -1

          prbaryon = 0.58608d0 - lnu * (0.10488d0
     +                         - lnu * (0.00370d0
     +                         - lnu * (0.00091d0
     +                         + lnu * (1.167d-5 * lnu - 0.00023d0))))
          prkaon   = max(0.01821d0 * lnu - 0.04007d0, 0d0)
c
          prqfam(1, pion)   = 0.26946d0 + lnu * (0.01328d0
     +                                  - lnu *  0.00095d0)
          prqfam(2, pion)   = 0.43367d0 - lnu * (0.01831d0
     +                                  - lnu *  0.00043d0)
          prqfam(1, baryon) = 0.37093d0 + 0.00909d0 * lnu
          prqfam(2, baryon) = 0
          prqfam(3, baryon) = 0.22280d0 - 0.01616d0 * lnu
c
c         A simple but effective way of simulating n-nbar
c         annihilation.
c
          if ((((1 + 5.6d0 * fenergy) ** 0.8d0) * urandom()) .lt. 1)
     +    then
            pktype   = glueball
            fenergy0 = fenergy0 + 2 * neutronmass
            prbaryon = 0.55d0 * prbaryon
          endif
c
        else
c
c         Neutrons or other neutral baryons.
c
          pkeepch  = max(0.64044d0 - 0.03164d0 * lnu, 0.41d0)
          prbaryon = 0.77461d0 - lnu * (0.02575d0
     +                         + lnu * (0.08569d0
     +                         - lnu * (0.01997d0
     +                         + lnu * (4.5355d-5 * lnu - 0.00163d0))))
          prbaryon = min(prbaryon, 0.625d0)
          prkaon   = max(0.01801d0 * lnu - 0.03735d0, 0d0)
c
          prqfam(1, pion)   = min(0.16843d0 + 0.04177d0 * lnu, 0.310d0)
          prqfam(2, pion)   = max(0.50285d0 - 0.03358d0 * lnu, 0.339d0)
          prqfam(1, baryon) = 0.56823d0 - 0.03081d0 * lnu
          a                 = max(0.02715d0 * lnu - 0.09157d0, 0d0)
          prqfam(2, baryon) = a
          prqfam(3, baryon) = a
c
        endif
        prbaryon = min(prbaryon, 1.d0)
c
        j       = 4.35d0 * urandom()
        j       = j + 4
        e       = urandom() * fenergy0
        a       = fenergy0 - e
        b       = urandom() * a
        a       = a - b
        esec(1) = urandom() * a
        esec(2) = a - esec(1)
        esec(3) = urandom() * b
        esec(4) = b - esec(3)
c
        if (j .gt. 5) then
          b       = urandom() * e
          a       = e - b
          esec(5) = urandom() * a
          esec(6) = a - esec(5)
          esec(7) = urandom() * b
          esec(8) = b - esec(7)
        endif
c
c       Collect unradiated packets to form leading particle.
c
        if (urandom() .lt. pkeepch) then
          qleader = qin
        else
          qleader = altqin
        endif

c     TEST: Force same leading as incident
        qleader = qin
c     ///////////////////////////////////


        primgrp = gnucnucbr
c
        eleader = fenergy0
        do jj = j, 2, -1
          pk(jj)   = glueball
          esec(jj) = esec(jj - 1)
          eleader  = eleader - esec(jj)
        enddo
c
        esec(1) = eleader
        pk(1)   = pktype
c
        qqsp    = fenergy / 6.0d0
        qqspmin = qqspnuc / 2
        qqspmax = qqspnuc
c
      else
c
c       MESON PRIMARY.
c       ==============
c
        prnucdrf = min(0.10d0 + 1.53d0 * sqrt(unlu), 0.9d0)
        prnucd   = min(0.903d0 + 0.088d0 * lnu, 1.d0)
c
        prbaryon = 0.71264d0 - lnu * (0.12767d0
     +                       + lnu * (0.02408d0
     +                       - lnu * (0.00840d0
     +                       + lnu * (2.0771d-5 * lnu - 0.00074d0))))
        prbaryon = min(prbaryon, 0.725d0)
        prkaon   = max(0.01894d0 * lnu - 0.04011d0, 0d0)
c
        if (qin .eq. 0) then
          a                 = 0.33333d0
          prqfam(1, pion)   = a
          prqfam(2, pion)   = a
          prqfam(1, baryon) = 0.50169d0 - 0.00098d0 * lnu
        else if (qin .gt. 0) then
          prqfam(1, pion)   = min(0.69049d0 - lnu * (0.14268d0
     +                                      - lnu * (0.02117d0
     +                                      - lnu *  0.00112d0)),
     +                            1.d0)
          prqfam(2, pion)   = max(0.15293d0 + lnu * (0.05592d0
     +                                      - lnu * (0.00623d0
     +                                      - lnu *  0.00020d0)),
     +                            0.d0)
          prqfam(1, baryon) = 0.52397d0 - 0.01267d0 * lnu
          if (prnucd .lt. 1) prnucdrf = prnucdrf * prnucd
        else
          prqfam(1, pion)   = max(0.15138d0 + lnu * (0.05420d0
     +                                      - lnu * (0.00412d0
     +                                      - lnu *  0.00002d0)),
     +                            0.d0)
          prqfam(2, pion)   = min(0.668893d0 - lnu * (0.13590d0
     +                                      - lnu * (0.01889d0
     +                                      - lnu *  0.00092d0)),
     +                            1.d0)
          prqfam(1, baryon) = 0.47940d0 - 0.00690d0 * lnu
          if (prnucd .lt. 1) prnucdrf = prnucdrf * (1 + prnucd) / 2
        endif
        a                 = max(0.02463d0 * lnu - 0.08062d0, 0d0)
        prqfam(2, baryon) = a
        prqfam(3, baryon) = a
c
        eleader = urandom() * fenergy
        e       = fenergy - eleader
        esec(2) = e
        esec(1) = eleader
        pk(1)   = leader
        pk(2)   = glueball
        j       = 2
c
c        if (urandom() .lt. 0.5d0) then
ccc   TEST: FORCING this NOT to happen
        if (urandom() .lt. -0.5d0) then
          pk(1)    = glueball
          esec(1)  = esec(1) + chpimass + protonmass
ccc   What the hell is this????? meson-proton anihilation with 50% chance???
          eleader  = 0
          qleader  = 0
        else
          b = urandom()
          if (b .lt. 0.80d0) then
            qleader = qin
          else if (b .lt. 0.93d0) then
            qleader = 0
          else
            qleader = - qin
          endif
        endif
c
        primgrp = gpion
c
        jj = j
        do k = 1, jj
          if (pk(k) .eq. glueball) then
            e = esec(k)
            a = urandom() * e
            b = e - a
            if (urandom() .lt. 0.5d0) then
              esec(k) = urandom() * a
              j       = j + 1
              esec(j) = a - esec(k)
            else
              esec(k) = a
            endif
            if (urandom() .lt. 0.5d0) then
              j           = j + 2
              esec(j - 1) = urandom() * b
              esec(j)     = b - esec(j - 1)
            else
              j       = j + 1
              esec(j) = b
            endif
          endif
        enddo
c
        do k = 2, j
          pk(k) = glueball
        enddo
c
        qqsp    = fenergy / 8.0d0
        qqspmin = qqspmes / 4
        qqspmax = qqspmes
c
      endif
c
c     GENERAL INITIALIZATION.
c
c     Charge of kaons: Taken independently of primary.
c
      do k = 0, 3
        prqfam(k, kaon) = 0.25d0
      enddo
c
c     Diminishing baryon probability.
c
      prbaryon = max(prbaryon - 0.075d0, 0.d0)
      prbaryon = min(prbaryon, 0.2d0)
c
c     Pt distributions.
c
      call ptparset(fenergy, pclemass(pcode))
c
c     Splitting parameter.
c
      if (qqsp .gt. qqspmax) then
        qqsp = qqspmax
      else if (qqsp .lt. qqspmin) then
        qqsp = qqspmin
      endif
c
c     Allow for "elastic" collisions at very low energy.
c
      if ((pk(1) .eq. leader) .and. (fenergy .lt. 0.3d0)) then
        if (urandom() .lt. (1 - (fenergy / 0.3d0) ** 2)) then
          qleader = qin
          j = 1
        endif
      endif
c
      nsec = j
c
c     SPLITTING LOOP.
c
c     Invoking the energy splitting algorithm that generates the
c     emerging secondaries.
c
      call splitkernel(up, maxsec, ldf, qin, primgrp, qleader,
     +                 nsec, pk, seccode, esec, upsec, ws)
c
      return
c
      end
c     --- End of routine heavysplit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ptparset(pkegy, pmass)
c
c     Energy dependent pt distribution parameters.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
c     Arguments:
c     =========
c
c     pkegy.......... (input, double precision) Kinetic energy of
c                     primary particle (GeV).
c     pmass.......... (input, double precision) Mass of primary
c                     particle (GeV)
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
      double precision  pkegy, pmass
c
c     Declaration of shared data.
c
      include 'splitmodeldata.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      pofam(pion)   = ptopih
      pofam(kaon)   = ptokah
      pofam(baryon) = ptonuh
c
      return
      end
c     --- End of routine ptparset
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine splitkernel(up, maxsec, ldf, pprim, fleader, pleader,
     +                       nsec, pk, seccode, esec, upsec, ws)
c
c     The kernel of Hillas's splitting algorithm (A. M. Hillas,
c     Proc. of the Paris Workshop on Cascade simulations, J. Linsley
c     and A. M. Hillas (eds.), p 39 (1981).).
c
c     This implementation includes major improvements from the original
c     algorithm, regarding mainly the procedure to assign identity to
c     the energy packets that are generated during the splitting
c     process. In this implementation, the secondaries can be protons,
c     neutrons, pbar, nbar, pions, or kaons. The respective
c     probabilities are calculated from external tables which are set
c     up in the calling program, and depend on the primary and its
c     energy. The probabilities are adjusted to perform a realistic
c     simulation of hadronic collisions, for primary kinetic energies
c     in the range 100 MeV to 2 TeV.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
c     Arguments:
c     =========
c
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integer) Leading dimension of array
c                     upsec.
c     pprim.......... (input, integer) Type of primary particle.
c     fleader,
c     pleader........ (input, integer) Family and type of leading
c                     particle (if any).
c     nsec........... (input-output, integer) As input: The initial
c                     number of energy packets that will be injected in
c                     the main splitting loop. As output: Number of
c                     generated secondaries
c     pk............. (input-output) Internal array containing the
c                     initial types assigned to the energy packets.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries.
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries.
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries.
c     ws............. (scratch, double precision, array(maxsec)) Real
c                     working space.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclecodes.f'
      include 'pclepar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  up(3)
      integer           maxsec, ldf, pprim, fleader, pleader, nsec
      integer           pk(maxsec)
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      double precision  ws(maxsec)
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
      include 'splitmodeldata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           j, jj
      integer           seccode1, pktype
      integer           chargesel, secgrp
      logical           remp
      double precision  a, b, e, erem, po, aa, bb
      double precision  mass, ptot, tpo, tpt, cpt, psq
      double precision  urandom, urandomt
c
c     FIRST EXECUTABLE STATEMENT
c
c     Coverting particle probabilities into accumulative ones, and
c     normalizing family probabilities.
c
      prpion   = max(1 - (prbaryon + prkaon), 0.d0)
      prbaryon = prbaryon + prpion
c
      call chprobnorm
c
      j     = nsec
      nsec  = 0
      erem  = 0
      remp  = .true.
c
c     Energy splitting loop.
c
 1030 continue
c
      e      = esec(j)
      pktype = pk(j)
      j      = j - 1
c
c     Selecting accordingly with category of energy packet.
c
      if (pktype .eq. glueball) then
c
c       Splitting.
c
        a = urandom() * (e + qqsp)
c
        if (a .ge. e) then
          j           = j + 1
          esec(j)     = e
          pk(j)       = particle
        else
          j           = j + 2
          esec(j)     = a
          pk(j)       = particle
          esec(j - 1) = e - a
          pk(j - 1)   = glueball
        endif
c
      else
c
c       Not splitting. Storing as final secondary particle.
c
        if (pktype .ne. leader) then
c
c         Selecting type and charge of secondary.
c
          b = urandom()
          if (b .lt. prpion) then
            pktype = pion
            secgrp = gpion
          else if (b .lt. prbaryon) then
            pktype = baryon
            secgrp = gnucnucbr
          else
            pktype = kaon
            secgrp = gkaon
          endif
c
          b = urandom()
          if (b .lt. prqfam(0, pktype)) then
            chargesel = 0
          else if (b .lt. prqfam(1, pktype)) then
            chargesel = 1
          else if (b .lt. prqfam(2, pktype)) then
            chargesel = 2
          else
            chargesel = 3
          endif
c
          seccode1 = gengrppcle(chargesel, secgrp)
          po       = pofam(pktype)
c
c         Emulating interactions with nucleons within the target
c         nucleus. Some energy packets are treated as kinetic energy
c         packets (the rest mass comes from the evaporated nucleon).
c
          if ((seccode1 .eq. protoncode) .or.
     +        (seccode1 .eq. neutroncode)) then
            if (urandom() .lt. prnucd) then
              e      = e + pclemass(seccode1)
              prnucd = prnucd * prnucdrf
            endif
          endif
c
        else
c
c         Leading particle. The energy of the packet is considered
c         to be kinetic energy if the charge is conserved.
c
          seccode1 = gengrppcle(pleader, fleader)
          if (pleader .eq. pprim) then
            e = e + pclemass(seccode1)
          endif
          if (fleader .eq. gnucnucbr) then
            po = pofam(baryon)
          else
            po = pofam(pion)
          endif
c
        endif
c
        mass = pclemass(seccode1)
c
c       Processing only those energy packets that make physical sense.
c
        if (e .gt. mass) then
c
c         Evaluating deflection angle for the secondary particle.
c
          psq  = e ** 2 - mass ** 2
          ptot = sqrt(psq)
          tpo  = po * (1 - sqrt(urandom() * mass / e))
          if (tpo .lt. ptot) then
            tpo = - tpo / ptot
 1050       continue
            aa  = urandomt(0.d0)
            tpt = tpo * log(aa * urandomt(0.d0))
            if (tpt .gt. pi) goto 1050
          else
            aa = ptot / tpo
            b  = 1 - aa
            bb = aa * (1 + b)
            tpt = (sqrt(b * b + bb * urandom()) - b) / aa
            tpt = pi * (1 - tpt)
          endif
          cpt = cos(tpt)
c
c         Shifting the particle arrays.
c
          nsec = nsec + 1
          do jj = j, nsec, -1
            esec(jj + 1) = esec(jj)
            pk(jj + 1)   = pk(jj)
          enddo
c
c         Cosine of deflection angle.
c
          ws(nsec) = cpt
c
c         Copying the rest of the data. Total energy is transformed
c         into kinetic energy.
c
          seccode(nsec) = seccode1
          esec(nsec)    = e - mass
c
          j  = j + 1
c
        else
c
c         Very low energy packets are accumulated to form a
c         new glueball.
c
          erem  = erem + e
c
        endif
      endif
c
      if (j .gt. nsec) goto 1030
c
c     Radiating new glueball with accumulated residual packets.
c
      if (remp) then
        remp = .false.
        if (erem .gt. 0) then
          j       = nsec + 1
          pk(j)   = glueball
          esec(j) = erem
          erem    = 0
          goto 1030
        endif
      endif
c
c     End of energy splitting loop.
c     All secondaries have been processed.
c
c     Deflecting the generated particles.
c
      call mdeflectr(up, nsec, ws, ldf, upsec)
c
      return
c
      end
c     --- End of routine splitkernel
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine chprobnorm
c
c     Normalizing relative probabilities for meson, kaon and
c     baryon families. The sets of normalized probabilities are stored
c     in the shared probability table used in the splitting kernel.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
      implicit none
c
c     Declaration of shared data.
c
      include 'splitmodeldata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           jj, k
c
c     FIRST EXECUTABLE STATEMENT
c
      prqfam(0, pion)   = 1 - (prqfam(1, pion) + prqfam(2, pion))
      prqfam(0, baryon) = 1 - (prqfam(1, baryon) + prqfam(2, baryon) +
     +                         prqfam(3, baryon))
c
      do k = pion, baryon
        do jj = 1, 2
          prqfam(jj, k) = prqfam(jj, k) + prqfam(jj - 1, k)
        enddo
        prqfam(3, k) = 1
      enddo
      prqfam(2, pion) = 1
c
      return
c
      end
c     --- End of routine chprobnorm
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'splitmodel.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
