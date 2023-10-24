c
c     FILE: AiresCalls.f                    Creation date: 29/SEP/2005.
c                                       LAST MODIFICATION: 26/DEC/2005.
c
c     This file contains the external collision calls which correspond
c     to the QGSJET II hadronic collision model.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extmodelname(modelname, namelen)
c
c     Routine to set the name of the external model. The name is a
c     character string that uniquely identifies the model.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2002, 2005.
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
      modelname = 'QGSJET-II'
      namelen   = 9
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
c     This routine corresponds to the QGSJET hadronic interaction
c     model (see, S. Ostapchenko, Nucl. Phys. Proc. Suppl., B151 (2006)
c     143).
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998, 1999, 2001, 2002,
c                                         2005.
c
c
c     Arguments:
c     =========
c
c     modelid........ (input-output, integer) Model identification
c                     number.
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
c     Defining the MFPHadronic directive with 'QGSJET-II' default.
c
      call newinpstring('MFPHadronic', 3, 'Hadronic Mean Free Paths',
     +       2, 'Standard SIBYLL QGSJET QGSJET-II',
     +       ' ',
     +       6, 'QGSJET-II', ' ')
c
c     Defining directives to switch On/Off the interaction model and
c     the nucleus-nucleus mfp's.
c
      call newinpswitch('ExtNucNucMFP', 9,
     +                  'QGSJET-II nucl-nucl MFP',
     +                  0, .true., .true.)
c
      call newinpswitch('ExtCollModel', 7, 'QGSJET-II switch',
     +                  2, .true., .true.)
c
      call newinpswitch('AllowQGSJETInit', 14,
     +                  'QGSJET creates init files',
     +                  0, .false., .true.)
c
c     Setting the model identifier.
c
      modelid = modelid + 22003
c
c     Safe minimum energies for QGSJET-II.
c
      kemin    = 75
      kenucmin = 75
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
c     This routine corresponds to the QGSJET hadronic interaction
c     model (see, S. Ostapchenko, Nucl. Phys. Proc. Suppl., B151 (2006)
c     143).
c
c     Written by: S. J. Sciutto, La Plata 1997, 2002, 2005.
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
      modelname = 'QGSJET-II'
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
c     This routine corresponds to the QGSJET-II hadronic interaction
c     model (see, S. Ostapchenko, Nucl. Phys. Proc. Suppl., B151 (2006)
c     143).
c
c     Written by: S. J. Sciutto, La Plata 1997, 1999, 2002, 2005.
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
c     Some internal data needed in the QGSJET link.
c
      integer           DEBUG
      common            /DEBUG/ DEBUG
      integer           MONIOU
      common            /QGARR43/ MONIOU
c
c     Declaration of internal variables and arrays.
c
      integer           nifiles
      parameter         (nifiles = 2)
c
      logical           ifileok(nifiles)
      integer           i
      logical           getinpswitch
c
c     FIRST EXECUTABLE STATEMENT
c
      if (.not. extmodelison) return
c
      DEBUG  = 0
      MONIOU = 6
c
c     Checking existence of initialization files.
c
      inquire (file = 'qgsdat-II-03', exist = ifileok(1))
      inquire (file = 'sectnu-II-03', exist = ifileok(2))
c
      do i = 1, nifiles
        if (.not. ifileok(i)) goto 1050
      enddo
      goto 1060
 1050 continue
c
c     There are missing initialization files.
c
      if (.not. getinpswitch('AllowQGSJETInit')) then
        call errprint(2, '*', 4, 'AiresCallsQGSJET',
     +    'One or more QGSJET initialization files not found.$' //
     +    'Provide files or run enabling AllowQGSJETInit switch.',
     +    0, 0, 0, 0.d0, ' ')
      endif
c
 1060 continue
c
c     Calling QGSJET initialization routines.
c
      call putlog(0, .true., 'Initializing QGSJET-II package.')
c
      call QGSET
      call QGAINI
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
c     using the QGSJET hadronic interaction model (see, S. Ostapchenko,
c     Nucl. Phys. Proc. Suppl., B151 (2006) 143).
c
c     Written by: S. J. Sciutto, La Plata 2002, 2005.
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
c     Declaration of internal variables and arrays.
c
      double precision  sign, sigo, siga, crsn
      double precision  compos(3)
c
      double precision  QGSECT
c
      data              compos / 0.78479d0, 0.21052d0, 0.00469d0 /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking QGSJET to evaluate the cross section and mfp.
c     Code based in routine QGSSIG written by D. Heck.
c
      sign = QGSECT(ke, 2, a, 14)
      sigo = QGSECT(ke, 2, a, 16)
      siga = QGSECT(ke, 2, a, 40)
      crsn = compos(1) * sign + compos(2) * sigo + compos(3) * siga
c
      extnucnucmfp = 24175.97968d0 / crsn
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
c     to the QGSJET hadronic interaction model (see, S. Ostapchenko,
c     Nucl. Phys. Proc. Suppl., B151 (2006) 143).
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000, 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     fenergy........ (input, double precision) The free energy of the
c                     particle.
c     up............. (input, double precision, array(3)) Unitary vector
c                     marking the direction of the initial impulse.
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
c     Declaration of internal variables and arrays.
c
      integer           pcode, nfrag
      double precision  rr, efrag
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting primary particle code. The current version of
c     QGSJET-II does not accept gammas as projectiles, and so they will
c     be treated as pions.
c
      rr = urandom()
      if (rr .lt. 0.50d0) then
        pcode =  11
      else
        pcode = -11
      endif
c
c     Calling the generic QGSJET collision routine.
c
      call qgsnhnucoll(pcode, 1, fenergy, up, atarget, maxsec, ldf,
     +                 nsec, nfrag, efrag, seccode, esec, upsec)
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
c     to the QGSJET hadronic interaction model (see, S. Ostapchenko,
c     Nucl. Phys. Proc. Suppl., B151 (2006) 143).
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998, 2000, 2001, 2002,
c                                         2005.
c
c
c     Arguments:
c     =========
c
c     isbaryon ...... (input, logical) True if the primary particle
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
c     Declaration of internal variables and arrays.
c
      integer           extpcode, nfrag
      double precision  efrag
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      extpcode = pcode
c
c     Pi0's are not acceptable as QGSJET input particles: Mapping
c     onto charged pions.
c
      if (pcode .eq. 10) then
        if (urandom() .lt. 0.5d0) then
          extpcode = 11
        else
          extpcode = -11
        endif
      endif
c
c     Calling the generic QGSJET collision routine.
c
      call qgsnhnucoll(extpcode, 1, fenergy, up, atarget, maxsec, ldf,
     +                 nsec, nfrag, efrag, seccode, esec, upsec)
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
c     to the QGSJET hadronic interaction model (see, S. Ostapchenko,
c     Nucl. Phys. Proc. Suppl., B151 (2006) 143).
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
c     FIRST EXECUTABLE STATEMENT
c
c     Calling the generic QGSJET collision routine.
c
      call qgsnhnucoll(31, massa, kenucleon, up, atarget, maxsec, ldf,
     +                 nsec, nfrag, efrag, seccode, esec, upsec)
c
      return
      end
c     --- End of routine extnucnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine qgsnhnucoll(pcode, massa, fenergy, up,
     +                       atarget, maxsec, ldf,
     +                       nsec, nfrag, efrag, seccode, esec, upsec)
c
c     Hadron-nucleus and nucleus-nucleus external collision routine.
c
c     This routine processes the inelastic collision via a call
c     to the QGSJET hadronic interaction model (see, S. Ostapchenko,
c     Nucl. Phys. Proc. Suppl., B151 (2006) 143).
c
c     It is assumed that the primary energy is above the corresponding
c     threshold.
c
c     Written by: S. J. Sciutto, La Plata 2005.
c
c
c     Arguments:
c     =========
c
c     pcode.......... (input, integer) Primary particle code (Aires).
c                     For nucleus nucleus collisions the primary
c                     code must be 31.
c     massa.......... (input, integer) Projectile mass number (nucleus-
c                     nucleus collisions) or 1 for hadron-nucleus
c                     collisions.
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
      integer           pcode, massa
      double precision  fenergy
      double precision  up(3)
      integer           atarget, maxsec, ldf, nsec, nfrag
      double precision  efrag
      integer           seccode(maxsec)
      double precision  esec(maxsec)
      double precision  upsec(ldf, maxsec)
c
c     Declaration of shared data.
c
c     Some internal data needed in the QGSJET link.
c
      integer           mxiap, mxnpt
      parameter         (mxiap = 207)
      parameter         (mxnpt = 95000)
c
c     Output particle data arrays from QGSJET:
c
c      NSP           number of secondary particles.
c      NSF           number of secondary nuclear fragments.
c      IAF(1:NSF)    mass number of the secondary nuclear fragments.
c      ESP(4, 1:NSP) 4-momentum of the secondary particles.
c      ICH(1:NSP)    QGSJET codes of secondary particles.
c
      integer           NSP
      common            /QGARR12/ NSP
c
      integer           NSF, IAF(mxiap)
      common            /QGARR13/ NSF, IAF
c
      double precision  ESP(4, mxnpt)
      integer           ICH(mxnpt)
      common            /QGARR14/ ESP, ICH
c
c     Declaration of internal variables and arrays.
c
      double precision  chpimass
      parameter         (chpimass = 0.1395679d0)
      double precision  neutronmass, protonmass
      parameter         (neutronmass  = 0.93956563d0)
      parameter         (protonmass   = 0.93827231d0)
      double precision  avnucmass
      parameter         (avnucmass = (neutronmass + protonmass) / 2)
      integer           extpcode, aextpcode
      double precision  totegy
      integer           nstry, isec, jsec, i, irc
      integer           atot, afragment, zfragment
      double precision  energy, kenergy, px, py, pz, pp
      double precision  urandom
      integer           nuclcode
c
c     QGSJET particle codes conversion arrays.
c
c     List of QGSJET particle codes:
c
c      0    1   -1   2  -2    3  -3    4   -4   5   -5
c     pi0  pi+  pi-  p  pbar  n  nbar  K+  K-  K0S  K0L
c
c       6        -6      7   -7  8    -8       9         -9        10
c     Lambda  LambdaBar  D+  D-  D0  D0Bar  Lambda_C  Lambda_CBar  Eta
c
      integer           qgsjetcode(-31:31)
      integer           airescode(-9:10)
      save              qgsjetcode, airescode
c
      double precision  qgsmass(0:10)
c
c     QGSJET-II beta does not support Lambdas and Etas as projectiles,
c     mapping them onto pions.
c     Also, QGSJET does not support sigma and omega baryons, they are
c     mapped onto nucleons.
c
      data              qgsjetcode /                  -02, -03,
     +                                 -99, -02, -99, -02, -02,
     +                                 -03, -02, -02, -03, -01,
     +                                 -99, -99, -99, -99,  01,
     +                                 -04, -05,  05, -01,  00,
     +                                 -99, -88, -88, -88, -99,
     +                                 -99, -88, -88, -99, -99,
     +                                  88,  88,  88,  99,  99,
     +                                  88,  88,  88,  99,  00,
     +                                  01,  05, -05,  04,  01,
     +                                  99,  99,  99,  99,  03,
     +                                  03,  02,  02,  03,  03,
     +                                  02,  99,  02,  99,  03,
     +                                  02                       /
c
      data              airescode /   999, 999, 999, -20,  13,
     +                                -14, -30, -31, -11,  10,
     +                                 11,  31,  30,  14,  12,
     +                                 20, 999, 999, 999,  15    /
c
      data              qgsmass( 0)  / 0.1349764d0  /
      data              qgsmass( 1)  / 0.13956995d0 /
      data              qgsmass( 2)  / 0.93827231d0 /
      data              qgsmass( 3)  / 0.93956563d0 /
      data              qgsmass( 4)  / 0.493677d0   /
      data              qgsmass( 5)  / 0.497672d0   /
      data              qgsmass( 6)  / 1.115684d0   /
      data              qgsmass(10)  / 0.54730d0    /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting primary particle code.
c
      extpcode = qgsjetcode(pcode)
c
c     CALLING QGSJET COLLISION ROUTINES.
c
      do nstry = 1, 3
c
c       Calling the energy-dependent QGSJET initialization routine.
c
c       The arguments of this routine are:
c       (1) Total projectile energy (Lab. system). Energy per nucleon
c           in the case of nucleus projectile.
c       (2) QGSJET input particle code.
c       (3) Projectile mass number.
c       (4) Target mass number.
c
        call QGINI(fenergy, extpcode, massa, atarget)
c
c       Processing the interaction.
c
        call QGCONF
c
        nsec = NSP + NSF
        if ((nsec .gt. 0) .and. (nsec .le. maxsec)) goto 1010
c
      enddo
c
      call errprint(2, 'NSEC', 4, 'qgsnhnucoll', ' ',
     +              1, nsec, 0, 0.d0, ' ')
      return
c
 1010 continue
c
c     Processing produced secondaries.
c
      nsec   = 0
      nfrag  = 0
      efrag  = 0
      totegy = 0
c
      do isec = 1, NSP
c
        extpcode  = ICH(isec)
        aextpcode = abs(extpcode)
c
c       Energy.
c
        energy = ESP(1, isec)
        totegy = totegy + energy
c
        if ((aextpcode .le. 6) .or. (extpcode .eq. 10)) then
c
c         The particle can be tracked by AIRES.
c
c         If the energy is high enough, the particle is kept.
c
          if (energy .gt. qgsmass(aextpcode)) then
c
c           Particle impulse.
c
            pz = ESP(2, isec)
            px = ESP(3, isec)
            py = ESP(4, isec)
            pp = sqrt(px ** 2 + py ** 2 + pz ** 2)
            px = px / pp
            py = py / pp
            pz = pz / pp
c
c           Adding the particle to the list of valid secondaries,
c           it is assumed that there is room for it in the
c           corresponding arrays.
c
            nsec  = nsec + 1
c
            seccode(nsec)  = airescode(extpcode)
            upsec(1, nsec) = px
            upsec(2, nsec) = py
            upsec(3, nsec) = pz
c
c           Evaluating the kinetic energy.
c
            esec(nsec) = energy - qgsmass(aextpcode)
c
          endif
c
        endif
c
      enddo
c
c     Evaluating the emerging directions of the secondaries.
c
      call mrotate(up, nsec, ldf, upsec)
c
c     Nuclear fragments. We use here the percolation model with no
c     transverse momentum for nuclear fragments.
c
      if (NSF .gt. 0) then
c
c       The nuclear fragments are processed only if there is remaining
c       energy.
c
        atot = 0
        do isec = 1, NSF
          atot = atot + IAF(isec)
        enddo
c
        energy = massa * fenergy - totegy - atot * avnucmass
c
        if (energy .gt. 0) then
c
c         Kinetic energy per nucleon.
c
          energy = energy / atot
c
c         Shifting the particle's section.
c
          nfrag = NSF
          do jsec = nsec, 1, -1
            seccode(jsec + nfrag) = seccode(jsec)
            esec(jsec + nfrag)    = esec(jsec)
            do i = 1, 3
              upsec(i, jsec + nfrag) = upsec(i, jsec)
            enddo
          enddo
          nsec = nsec + nfrag
c
c         Processing the nuclear fragments.
c
          do isec = 1, NSF
c
            afragment = IAF(isec)
            zfragment = 1.0347d0 + 0.4484d0 * afragment
            if (zfragment .le. 1) then
              if (urandom() .lt. 0.5d0) then
                seccode(isec) = 31
              else
                seccode(isec) = 30
              endif
            else
              if (zfragment .gt. 36) zfragment = 36
              seccode(isec) = nuclcode(zfragment,
     +                                 afragment - zfragment, irc)
            endif
c
            kenergy    = energy * afragment
            efrag      = efrag + kenergy
            esec(isec) = kenergy
c
            do i = 1, 3
              upsec(i, isec) = up(i)
            enddo
c
          enddo
        endif
      endif
c
      return
      end
c     --- End of routine qgsnhnucoll
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'AiresCalls.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
