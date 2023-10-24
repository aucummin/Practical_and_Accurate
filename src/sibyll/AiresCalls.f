c
c     FILE: AiresCalls.f                    Creation date: 25/JUN/1998.
c                                       LAST MODIFICATION: 29/SEP/2005.
c
c     This file contains the external collision calls which correspond
c     to the SIBYLL 2.1 hadronic collision model.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extmodelname(modelname, namelen)
c
c     Routine to set the name of the external model. The name is a
c     character string that uniquely identifies the model.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2002.
c
c
c     Arguments:
c     =========
c
c     modelname...... (output, character*(*)) Model name.
c     namelen........ (output, integer) Length of model name. Maximum
c                     length is 64 characters.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     modelname
      integer           namelen
c
c     FIRST EXECUTABLE STATEMENT
c
      modelname = 'SIBYLL'
      namelen   = 6
c
      return
c
      end
c     --- End of routine extmodelname.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extmodelini0(modelid, kemin, kenucmin)
c
c     External hadronic interaction model initialization routine (I).
c     This routine is invoked at the beginning of every process, just
c     before beginning the scanning of the input data file.
c
c     This routine corresponds to the SIBYLL hadronic interaction
c     model (see, R. Engel, T. K. Gaisser, T. Stanev, "Air Shower
c     Calculations With the New Version of SIBYLL", proc. of the
c     26th ICRC, Utah 1999, vol. 1, p. 415).
c
c     Written by: S. J. Sciutto, La Plata 1998, 1999, 2001, 2002,
c                                         2005.
c
c
c     Arguments:
c     =========
c
c     modelid........ (input-output, integer) Model identification
c                     number (for internal use).
c     kemin.......... (output, double precision) Indicative value of
c                     the lowest primary energy (GeV) that can be
c                     processed by the algorithm.
c     kenucmin....... (output, double precision) Indicative value of
c                     the lowest primary energy per nucleon (GeV) that
c                     can be processed by the algorithm in the case of
c                     nucleus-nucleus collisions.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           modelid
      double precision  kemin, kenucmin
c
c     FIRST EXECUTABLE STATEMENT
c
c     Defining the MFPHadronic directive with 'SIBYLL' default.
c
      call newinpstring('MFPHadronic', 3, 'Hadronic Mean Free Paths',
     +       2, 'Standard SIBYLL QGSJET QGSJET-II',
     +       ' ',
     +       6, 'SIBYLL', ' ')
c
c     Defining directives to switch On/Off the interaction model and
c     the nucleus-nucleus mfp's.
c
      call newinpswitch('ExtNucNucMFP', 9,
     +                  'SIBYLL nucleus-nucleus MFP',
     +                  0, .true., .true.)
c
      call newinpswitch('ExtCollModel', 7, 'SIBYLL switch',
     +                  2, .true., .true.)
c
c     Setting the model identifier.
c
      modelid = modelid + 12100
c
c     Safe minimum energies for SIBYLL.
c
      kemin    = 160
      kenucmin = 160
c
      return
c
      end
c     --- End of routine extmodelini0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extmodelinitask(modelname, modelison)
c
c     External hadronic interaction model initialization routine (II).
c     This routine is invoked at the beginning of a task.
c
c     This routine corresponds to the SIBYLL hadronic interaction
c     model (see, R. Engel, T. K. Gaisser, T. Stanev, "Air Shower
c     Calculations With the New Version of SIBYLL", proc. of the
c     26th ICRC, Utah 1999, vol. 1, p. 415).
c
c     Written by: S. J. Sciutto, La Plata 1998, 2002.
c
c
c     Arguments:
c     =========
c
c     modelname...... (output, character*(*)) Short name for the
c                     model (maximum 16 characters).
c     modelison...... (output, logical) True if the corresponding
c                     switch is enabled (The IDL switch for the
c                     model is defined in routine extmodelini1).
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     modelname
      logical           modelison
c
c     Declaration of internal variables and arrays.
c
      logical           getinpswitch
c
c     FIRST EXECUTABLE STATEMENT
c
      modelname = 'SIBYLL 2.1'
      modelison = getinpswitch('ExtCollModel')
c
      return
c
      end
c     --- End of routine extmodelinitask
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extmodelinirun(extmodelison)
c
c     External hadronic interaction model initialization routine (III).
c     This routine is invoked at every run, just before entering
c     the main kernel routine (scheduler).
c
c     This routine corresponds to the SIBYLL hadronic interaction
c     model (see, R. Engel, T. K. Gaisser, T. Stanev, "Air Shower
c     Calculations With the New Version of SIBYLL", proc. of the
c     26th ICRC, Utah 1999, vol. 1, p. 415).
c
c     Written by: S. J. Sciutto, La Plata 1998, 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     extmodelison... (input, logical) True if the external model is
c                     going to be indeed used in the simulations.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           extmodelison
c
c     Declaration of shared data.
c
c     Some variables for the AIRES-SIBYLL interface.
c
      integer           LDIFF
      common            /S_CLDIF/ LDIFF
c
c     Control of debug info printout.
c
      integer           DEBLVL
      common            /S_DEBD/ DEBLVL
c
c     FIRST EXECUTABLE STATEMENT
c
      if (.not. extmodelison) return
c
c     Calling SIBYLL initialization routine.
c
      call putlog(0, .true., 'Initializing SIBYLL 2.1 package.')
c
      DEBLVL = 0
      call SIBYLL_START
      LDIFF  = 0
c
      call putlog(0, .true., 'Initialization complete.')
c
      return
c
      end
c     --- End of routine extmodelinirun
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function extnucnucmfp(a, z, ke)
c
c     Nucleus-nucleus (nuc-air) hadronic mean free paths, evaluated
c     using SIBYLL (see, R. Engel, T. K. Gaisser, T. Stanev, "Air Shower
c     Calculations With the New Version of SIBYLL", proc. of the 26th
c     ICRC, Utah 1999, vol. 1, p. 415).
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
c     Declaration of arguments.
c
      double precision  extnucnucmfp
      integer           a, z
      double precision  ke
c
c     Declaration of shared data.
c
c     Some SIBYLL shared variables needed in the AIRES-SIBYLL
c     interface.
c
      real              SSIGNUC(60), ALNUC(60)
      COMMON            /CLENNN/ SSIGNUC, ALNUC
c
c     Declaration of internal variables and arrays.
c
      real              epernuctev
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking SIBYLL to evaluate the cross section and mfp.
c
      epernuctev = 1.d-3 * ke
      call SIGNUC_INI (a, a, epernuctev)
c
      extnucnucmfp = ALNUC(a)
c
      return
      end
c     --- End of routine extnucnucmfp
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extgnucoll(fenergy, up, atarget, maxsec, ldf, nsec,
     +                      seccode, esec, upsec, iws, fws)
c
c     Gamma-nucleus external collision routine.
c
c     This routine processes the inelastic collision via a call
c     to the SIBYLL hadronic interaction model (see, R. Engel, T. K.
c     Gaisser, T. Stanev, "Air Shower Calculations With the New Version
c     of SIBYLL", proc. of the 26th ICRC, Utah 1999, vol. 1, p. 415).
c
c     It is assumed that the primary energy is above the corresponding
c     threshold.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000, 2002, 2005.
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
      double precision  fenergy
      double precision  up(3)
      integer           atarget, maxsec, ldf, nsec
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           iws(maxsec)
      double precision  fws(maxsec)
c
c     Declaration of shared data.
c
c     Some variables for the AIRES-SIBYLL interface.
c
      integer           LDIFF
      common            /S_CLDIF/ LDIFF
c
c     Declaration of internal variables and arrays.
c
      integer           pcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting primary particle code. The current version of
c     SIBYLL only accepts pi0, pi+-, K+-, KL, KS, p, n, pbar, nbar.
c     Gammas will be treated as pi0.
c
      pcode = 10
c
c     Calling the hadron-nucleus collision routine, with diffraction
c     disabled.
c
      LDIFF = -1
c
      call exthnucoll(.false., pcode, fenergy, up, atarget,
     +                maxsec, ldf, nsec, seccode,
     +                esec, upsec, iws, fws)
c
c     Resetting diffraction switch.
c
      LDIFF = 0
c
      return
      end
c     --- End of routine extgnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine exthnucoll(isbaryon, pcode, fenergy, up, atarget,
     +                      maxsec, ldf, nsec, seccode,
     +                      esec, upsec, iws, fws)
c
c     Hadron-nucleus external collision routine.
c
c     This routine processes the inelastic collision via a call
c     to the SIBYLL hadronic interaction model (see, R. Engel, T. K.
c     Gaisser, T. Stanev, "Air Shower Calculations With the New Version
c     of SIBYLL", proc. of the 26th ICRC, Utah 1999, vol. 1, p. 415).
c
c     It is assumed that the primary energy is above the corresponding
c     threshold.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998;
c                                Fermilab 1999;
c                                La Plata 2000, 2001, 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     isbaryon...... (input, logical) True if the primary particle
c                     is a baryon.
c     pcode.......... (input, integer) Primary particle code (Aires).
c     fenergy........ (input, double precision) The free energy of the
c                     particle.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     atarget........ (input, integer) Target nucleus mass number.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integr) Leading dimension of array upsec.
c     nsec........... (output, integer) Number of generated secondaries
c                     energetic secondaries.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries (Aires).
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
      double precision  fenergy
      double precision  up(3)
      integer           atarget, maxsec, ldf, nsec
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           iws(maxsec)
      double precision  fws(maxsec)
c
c     Declaration of shared data.
c
      include 'siblinkcomm.f'
c
c     Some variables for the AIRES-SIBYLL interface.
c
c     Internal variables for the gaussian random number generator.
c
      logical           ISET
c
      common            /GASDEVC/ ISET
c
c     Output particle data arrays from SIBYLL:
c
c      NP            number of final particles.
c      P(1:NP, 1:5)  4-momenta and masses of the final particles.
c      LLIST(1:NP)   Codes of final particles.
c
      integer           sibmaxsec
      parameter         (sibmaxsec = 8000)
c
      integer           NP
      real              P(sibmaxsec, 5)
      integer           LLIST(sibmaxsec)
c
      common            /S_PLIST/ NP, P, LLIST
c
c     Internal SIBYLL array used to transpose P.
c
      integer           sibnmaxsec
      parameter         (sibnmaxsec = 40000)
c
      integer           NPA
      real              PA(5, sibnmaxsec)
      integer           LLA(sibnmaxsec)
c
      common            /S_PLNUC/ NPA, PA, LLA
c
c     Declaration of internal variables and arrays.
c
      integer           sibpcode, i, nstry
      double precision  sqs, gamma, beta
      real              sqsgle
      integer           aair
      integer           nfrag
      double precision  efrag
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting primary particle code. The current version of
c     SIBYLL only accepts pi0, pi+-, K+-, KL, KS,  p,  n, pbar, nbar.
c     It IS assumed that the primary particle does belong to this
c     set. Notice that SIBYLL 2.1 DOES accept pi0's as projectiles.
c
      sibpcode = sibcode(pcode)
c
c     No energy conversion is needed since SIBYLL, like AIRES,
c     uses GeV as enegy unit.
c
c     Evaluating the center-of-mass energy.
c
      sqs    = sqrt(1.877784d0 * (fenergy + 0.93892d0))
      sqsgle = sqs
      gamma  = 0.53253d0 * sqs
      beta   = sqrt(1 - 1 / gamma ** 2)
c
      ISET = .true.
c
c     CALLING SIBYLL WITH AIR TARGET (A specified as an argument).
c
      do nstry = 1, 3
c
        aair = atarget
        call SIBYLL(sibpcode, aair, sqsgle)
        call DECSIB
c
        if ((NP .gt. 0) .and. (NP .le. maxsec)) goto 1010
c
      enddo
c
      call errprint(2, 'NSEC', 4, 'exthnucoll', ' ',
     +              1, NP, 0, 0.d0, ' ')
      return
c
 1010 continue
c
c     Processing produced secondaries.
c
c     Transposing array P.
c
      do i = 1, NP
        PA(1, i) = P(i, 1)
        PA(2, i) = P(i, 2)
        PA(3, i) = P(i, 3)
        PA(4, i) = P(i, 4)
        PA(5, i) = P(i, 5)
      enddo
c
      call anasibsecs(NP, sibmaxsec, PA, LLIST, gamma, beta, up,
     +                maxsec, ldf, nsec, nfrag, efrag,
     +                seccode, esec, upsec)
c
      return
      end
c     --- End of routine exthnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extnucnucoll(pcode, massa, nucz, kenucleon, up,
     +                        atarget, maxsec, ldf, nsec, nfrag, efrag,
     +                        seccode, esec, upsec, iws, fws)
c
c     Nucleus-nucleus external collision routine.
c
c     This routine processes the inelastic collision via a call
c     to the SIBYLL nucleus-nuclues interaction model (see, R. Engel,
C     T. K. Gaisser, T. Stanev, "Air Shower Calculations With the New
C     Version of SIBYLL", proc. of the 26th ICRC, Utah 1999,
C     vol. 1, p. 415).
c
c     It is assumed that the primary energy is above the corresponding
c     threshold.
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
c     kenucleon...... (input, double precision) The average free energy
c                     of the projectile's nucleons.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     atarget........ (input, integer) Target nucleus mass number.
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
      double precision  kenucleon
      double precision  up(3)
      integer           atarget, maxsec, ldf, nsec, nfrag
      double precision  efrag
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           iws(maxsec)
      double precision  fws(maxsec)
c
c     Declaration of shared data.
c
      include 'siblinkcomm.f'
c
c     Some variables for the AIRES-SIBYLL interface.
c
c     Internal variables for the gaussian random number generator.
c
      logical           ISET
c
      common            /GASDEVC/ ISET
c
c     Output particle data arrays from SIBYLL:
c
c      NPA           number of final particles.
c      PA(1:NP, 1:5) 4-momenta and masses of the final particles.
c      LLA(1:NP)     Codes of final particles.
c
      integer           sibnmaxsec
      parameter         (sibnmaxsec = 40000)
c
      integer           NPA
      real              PA(5, sibnmaxsec)
      integer           LLA(sibnmaxsec)
c
      common            /S_PLNUC/ NPA, PA, LLA
c
c     Declaration of internal variables and arrays.
c
      double precision  sqs, gamma, beta
      real              sqsgle
      integer           aair
c
c     FIRST EXECUTABLE STATEMENT
c
c     No energy conversion is needed since SIBYLL, like AIRES,
c     uses GeV as enegy unit.
c
c     Evaluating the center-of-mass energy.
c
      sqs    = sqrt(1.877784d0 * (kenucleon + 0.93892d0))
      sqsgle = sqs
      gamma  = 0.53253d0 * sqs
      beta   = sqrt(1 - 1 / gamma ** 2)
c
      ISET = .true.
c
c     CALLING SIBYLL WITH AIR TARGET (A given as argument).
c
      aair = atarget
      call SIBNUC(massa, aair, sqsgle)
c
      if ((NPA .le. 0) .or. (NPA .gt. maxsec))
     +  call errprint(2, 'NSEC', 4, 'extnucnucoll', ' ',
     +                1, NPA, 0, 0.d0, ' ')
c
c     Processing produced secondaries.
c
      call anasibsecs(NPA, sibnmaxsec, PA, LLA, gamma, beta, up,
     +                maxsec, ldf, nsec, nfrag, efrag,
     +                seccode, esec, upsec)
c
      return
      end
c     --- End of routine extnucnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine anasibsecs(NP, mxnp, P, LLIST, gamma, beta, up,
     +                      maxsec, ldf, nsec, nfrag, efrag,
     +                      seccode, esec, upsec)
c
c     Analysis of secondaries generated by SIBYLL: Building the list
c     of secondaries in AIRES format.
c
c     This routine processes the secondaries generated by the SIBYLL
c     hadronic interaction model (see, R. Engel, T. K. Gaisser,
c     T. Stanev, "Air Shower Calculations With the New Version of
c     SIBYLL", proc. of the 26th ICRC, Utah 1999, vol. 1, p. 415).
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
c
c     Arguments:
c     =========
c
c     NP............ (input, integer) Number of secondaries generated
c                    after the call to SIBYLL.
c     mxnp.......... (input, integer) Maximum value allowed for NP.
c     P............. (input, real, array(5, mxnp)) 4-momentum and
c                    rest mass of the secondaries, as generated by
c                    SIBYLL.
c     LLIST......... (input, integer, array(mxnp)) SIBYLL codes of the
c                    generated secondaries.
c     gamma......... (input, double precision) Gamma factor of CM
c                    system with respect to lab system.
c     beta.......... (input, double precision) Speed of CM system
c                    with respect to lab system.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integr) Leading dimension of array upsec.
c     nsec........... (output, integer) Number of generated secondaries
c                     energetic secondaries.
c     nfrag.......... (output, integer) Total number of secondary
c                     nuclear fragments and elastically scattered
c                     nucleons.
c     efrag.......... (output, double precision) Total energy of the
c                     emerging fragments.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries (Aires).
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries.
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           NP, mxnp
      real              P(5, mxnp)
      integer           LLIST(mxnp)
      double precision  gamma, beta
      double precision  up(3)
      integer           maxsec, ldf, nsec, nfrag
      double precision  efrag
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
c
c     Declaration of shared data.
c
      include 'siblinkcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           sibpcode, asibpcode
      integer           i, isec, jsec, ksec, nsec1
      double precision  ecm, pzcm, energy, px, py, pz, pp
      double precision  restmass, kenergy
      integer           massa, z, irc
      double precision  urandom
      integer           nuclcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Processing produced secondaries.
c
      nsec  = 0
      nsec1 = 0
      nfrag = 0
      efrag = 0
c
      do isec = 1, NP
c
        sibpcode  = LLIST(isec)
        asibpcode = abs(sibpcode)
c
        if ((asibpcode .le. 14) .or.
     +      (asibpcode .eq. 23) .or. (asibpcode .eq. 39) .or.
     +      ((sibpcode .gt. 1000) .and. (sibpcode .lt. 1096)) ) then
c
c         Significative SIBYLL particle or nuclear fragment.
c
c         Center of mass particle data.
c
          px       = P(1, isec)
          py       = P(2, isec)
          pzcm     = P(3, isec)
          ecm      = P(4, isec)
          restmass = P(5, isec)
c
c         Transforming energy to laboratory system.
c
          energy = gamma * (ecm + beta * pzcm)
c
c         For nuclear fragments the energy and impulse are given
c         "per nucleon".
c
          if (sibpcode .gt. 1001) then
            massa  = mod(sibpcode, 1000)
            energy = massa * energy
          endif
c
c         If the energy is high enough, the particle is kept.
c
          if (energy .gt. restmass) then
c
c           Converting total energies into kinetic energies, the mass
c           is taken as returned from SIBYLL.
c
            kenergy = energy - restmass
c
c           Adding the particle to the list of valid secondaries,
c           it is assumed that there is room for it in the
c           corresponding arrays.
c
c           Shifting the neutrinos' section.
c
            nsec1 = nsec1 + 1
            do jsec = nsec, nsec1, -1
              seccode(jsec + 1) = seccode(jsec)
              esec(jsec + 1)    = esec(jsec)
            enddo
            nsec = nsec + 1
c
            if (asibpcode .lt. 100) then
c
c             The secondary is a particle.
c
              seccode(nsec1) = airescode(sibpcode)
              ksec           = nsec1
c
            else
c
c             Emerging nucleus or nucleon.
c
c             Shifting the particle's section.
c
              nfrag = nfrag + 1
              do jsec = nsec1 - 1, nfrag, -1
                seccode(jsec + 1) = seccode(jsec)
                esec(jsec + 1)    = esec(jsec)
                do i = 1, 3
                  upsec(i, jsec + 1) = upsec(i, jsec)
                enddo
              enddo
              ksec = nfrag
c
              if (sibpcode .eq. 1001) then
                if (urandom() .lt. 0.5d0) then
                  seccode(nfrag) = 31
                else
                  seccode(nfrag) = 30
                endif
              else
                z  = 1.0347d0 + 0.4484d0 * massa
                if (z .gt. 36) z = 36
                seccode(nfrag) = nuclcode(z, massa - z, irc)
              endif
c
              efrag = efrag + kenergy
c
            endif
c
c           Particle impulse in lab system (per nucleon for fragments).
c
            pz = gamma * (pzcm + beta * ecm)
            pp = sqrt(px ** 2 + py ** 2 + pz ** 2)
c
c           Assigning emerging angles and energy to secondary particle.
c
            upsec(1, ksec) = px / pp
            upsec(2, ksec) = py / pp
            upsec(3, ksec) = pz / pp
            esec(ksec)     = kenergy
c
          endif
c
        else if (asibpcode .le. 18) then
c
c         The particle is a neutrino.
c
c         Center of mass neutrino data.
c
          ecm    = P(4, isec)
          pzcm   = P(3, isec)
c
c         Transforming energy to laboratory system.
c
          energy = gamma * (ecm + beta * pzcm)
c
c         The neutrino is directly added to the list of secondaries
c         (it will not be further processed).
c
          nsec = nsec + 1
c
          seccode(nsec)  = airescode(sibpcode)
          esec(nsec)     = energy
c
        endif
c
      enddo
c
c     Evaluating the emerging directions of the secondaries.
c
      call mrotate(up, nsec1, ldf, upsec)
c
      return
      end
c     --- End of routine anasibsecs
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'AiresCalls.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
