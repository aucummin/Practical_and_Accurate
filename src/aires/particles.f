c
c     FILE: particles.f                     Creation date: 26/JUN/1996.
c                                       LAST MODIFICATION: 20/MAR/2006.
c
c     Routines for particle data management.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine inipcledata
c
c     Initializing the particle data tables.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2001,
c                                         2002, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  t2sec
      integer           nuclcode
      integer           i, j, ncode, z, n ,a
      integer           idummy
c
c     FIRST EXECUTABLE STATEMENT
c
c     ELEMENTARY PARTICLE DATA.
c
c     Initializing.
c
      do i = -maxpcle, maxpcle
        pclename(i)  = 'NOT_USED'
        pclemass(i)  = 0
        pclelife(i)  = -1
        pcleq   (i)  = 0
      enddo
c
c     With this initialization done, only different values will be set.
c
c     NOTE: All the defined codes must match the corresponding values
c           of the parameters of file 'pclecodes.f'
c
c     Particle code 0 not used (It is an "escape" code).
c
c     Particle code 1 corresponds to photon (gamma), and
c     code -1 to Cherenkov photon.
c
      pclename( -1) = 'CvPhoton'
      psynonym(  1) = 'cvphoton'
      psyncode(  1) = -1
c
      pclename(  1) = 'Gamma'
      psynonym(  2) = 'gamma'
      psyncode(  2) = 1
      psynonym(  3) = 'Photon'
      psyncode(  3) = 1
      psynonym(  4) = 'photon'
      psyncode(  4) = 1
c
c     Leptons.
c
      pclename( -2) = 'e-'
      pclemass( -2) = electronmass
      pcleq   ( -2) = -1
      psynonym(  5) = 'Electron'
      psyncode(  5) = -2
      psynonym(  6) = 'electron'
      psyncode(  6) = -2
c
      pclename(  2) = 'e+'
      psynonym(  7) = 'Positron'
      psyncode(  7) = 2
      psynonym(  8) = 'positron'
      psyncode(  8) = 2
c
      pclename( -3) = 'mu-'
      pclemass( -3) = muonmass
      pclelife( -3) = t2sec(2.19703d-6, 'sec', idummy)
      pcleq   ( -3) = -1
      psynonym(  9) = 'Muon-'
      psyncode(  9) = -3
      psynonym( 10) = 'muon-'
      psyncode( 10) = -3
c
      pclename(  3) = 'mu+'
      psynonym( 11) = 'Muon+'
      psyncode( 11) = 3
      psynonym( 12) = 'muon+'
      psyncode( 12) = 3
c
      pclename( -4) = 'tau-'
      pclemass( -4) = taumass
      pclelife( -4) = t2sec(291.0d-15, 'sec', idummy)
      pcleq   ( -4) = -1
c
      pclename(  4) = 'tau+'
c
c     Neutrinos.
c
      pclename( -6) = 'nubar(e)'
c
      pclename(  6) = 'nu(e)'
c
      pclename( -7) = 'nubar(m)'
c
      pclename(  7) = 'nu(mu)'
c
      pclename( -8) = 'nubar(t)'
c
      pclename(  8) = 'nu(tau)'
c
c     Mesons.
c
      pclename(-10) = 'pi0'
      pclemass(-10) = pizeromass
      pclelife(-10) = t2sec(8.4d-17, 'sec', idummy)
c
      pclename( 10) = 'pi0'
c
      pclename(-11) = 'pi-'
      pclemass(-11) = chpimass
      pclelife(-11) = t2sec(26.030d0, 'ns', idummy)
      pcleq   (-11) = -1
c
      pclename( 11) = 'pi+'
c
      pclename(-12) = 'K0S'
      pclemass(-12) = k0mass
      pclelife(-12) = t2sec(8.922d-11, 'sec', idummy)
c
      pclename( 12) = 'K0S'
c
      pclename(-13) = 'K0L'
      pclemass(-13) = k0mass
      pclelife(-13) = t2sec(5.17d-8, 'sec', idummy)
c
      pclename( 13) = 'K0L'
c
      pclename(-14) = 'K-'
      pclemass(-14) = chkmass
      pclelife(-14) = t2sec(12.371d0, 'ns', idummy)
      pcleq   (-14) = -1
c
      pclename( 14) = 'K+'
c
      pclename(-15) = 'eta'
      pclemass(-15) = etamass
      pclelife(-15) = t2sec(5.58d-19, 'sec', idummy)
c
      pclename( 15) = 'eta'
c
c     Baryons.
c
      pclename(-20) = 'lambdab'
      pclemass(-20) = lambdamass
      pclelife(-20) = t2sec(2.632d-10, 'sec', idummy)
c
      pclename( 20) = 'lambda'
c
      pclename(-21) = 'Sigma0b'
      pclemass(-21) = sigma0mass
      pclelife(-21) = t2sec(7.4d-20, 'sec', idummy)
c
      pclename( 21) = 'Sigma0'
c
      pclename(-22) = 'Sigma+b'
      pclemass(-22) = sigmaplusmass
      pclelife(-22) = t2sec(8.018d-11, 'sec', idummy)
      pcleq   (-22) = -1
c
      pclename( 22) = 'Sigma+'
c
      pclename(-23) = 'Sigma-'
      pclemass(-23) = sigmaminusmass
      pclelife(-23) = t2sec(1.479d-10, 'sec', idummy)
      pcleq   (-23) = -1
c
      pclename( 23) = 'Sigma-b'
c
      pclename(-24) = 'Xi0b'
      pclemass(-24) = xi0mass
      pclelife(-24) = t2sec(2.90d-10, 'sec', idummy)
c
      pclename( 24) = 'Xi0'
c
c     Xi+ has not been detected yed. Including it for completeness,
c     with Xi- properties.
c
      pclename(-25) = 'Xi+b'
      pclemass(-25) = ximinusmass
      pclelife(-25) = t2sec(1.639d-10, 'sec', idummy)
      pcleq   (-25) = -1
c
      pclename( 25) = 'Xi+'
c
      pclename(-26) = 'Xi-'
      pclemass(-26) = ximinusmass
      pclelife(-26) = t2sec(1.639d-10, 'sec', idummy)
      pcleq   (-26) = -1
c
      pclename( 26) = 'Xi-b'
c
      pclename(-28) = 'Omega-'
      pclemass(-28) = omegaminusmass
      pclelife(-28) = t2sec(8.21d-9, 'sec', idummy)
      pcleq   (-28) = -1
c
      pclename( 28) = 'Omega-b'
c
c     Baryons (stable).
c
      pclename(-30) = 'nbar'
      pclemass(-30) = neutronmass
      pclelife(-30) = t2sec(889.1d0, 'sec', idummy)
      psynonym( 13) = 'AntiNeutron'
      psyncode( 13) = -30
      psynonym( 14) = 'antineutron'
      psyncode( 14) = -30
c
      pclename( 30) = 'Neutron'
      psynonym( 15) = 'neutron'
      psyncode( 15) = 30
      psynonym( 16) = 'n'
      psyncode( 16) = 30
c
      pclename(-31) = 'pbar'
      pclemass(-31) = protonmass
      pcleq   (-31) = -1
      psynonym( 17) = 'AntiProton'
      psyncode( 17) = -31
      psynonym( 18) = 'antiproton'
      psyncode( 18) = -31
c
      pclename( 31) = 'Proton'
      psynonym( 19) = 'proton'
      psyncode( 19) = 31
      psynonym( 20) = 'p'
      psyncode( 20) = 31
c
c     Checking if storage space is OK.
c
      if (maxpcle .lt. 31) call errprint(0, '*', 4, 'inipcledata',
     +   'Not enough space to store elementary particle data.',
     +   0, 0, 0, 0.d0, ' ')
c
c     COMPLETING THE PARTICLE PROPERTIES.
c
c     Antiparticle data is evaluated from the particle counterpart.
c
      do i = 1, maxpcle
        pclemass(i)   =   pclemass(-i)
        pclelife(i)   =   pclelife(-i)
        cpclelife(-i) =   cspeed * pclelife(-i)
        cpclelife(i)  =   cpclelife(-i)
        pcleq   (i)   = - pcleq(-i)
        pfreeegy(-i)  =   pclemass(-i) + pcleq(-i) * electronmass
        pfreeegy(i)   =   pclemass(i) + pcleq(i) * electronmass
      enddo
c
c     Completing the initialization of free energies.
c
      pfreeegy(electroncode) = 0
      pfreeegy(neutroncode)  = 0
      pfreeegy(protoncode)   = 0
      pfreeegy(pbarcode)     = 0
c
c     "Special" particle set (escape code) initialized as empty set.
c
      nescpcles         = 0
      lastescpcle       = pescode1 - 1
      escmacropos(4, 0) = 0
c
c     NUCLEAR DATA.
c
c     Chemical symbols for the elements up to Z = 36.
c     The default value for the number of neutrons is also set. It is
c     taken as the number of neutrons of the most abundant (in the
c     Earth) stable isotope.
c
      chemname( 1) = 'H'
      chemdefn( 1) = 0
      chemname( 2) = 'He'
      chemdefn( 2) = 2
      chemname( 3) = 'Li'
      chemdefn( 3) = 4
      chemname( 4) = 'Be'
      chemdefn( 4) = 5
      chemname( 5) = 'B'
      chemdefn( 5) = 6
      chemname( 6) = 'C'
      chemdefn( 6) = 6
      chemname( 7) = 'N'
      chemdefn( 7) = 7
      chemname( 8) = 'O'
      chemdefn( 8) = 8
      chemname( 9) = 'F'
      chemdefn( 9) = 10
      chemname(10) = 'Ne'
      chemdefn(10) = 10
      chemname(11) = 'Na'
      chemdefn(11) = 12
      chemname(12) = 'Mg'
      chemdefn(12) = 12
      chemname(13) = 'Al'
      chemdefn(13) = 14
      chemname(14) = 'Si'
      chemdefn(14) = 14
      chemname(15) = 'P'
      chemdefn(15) = 16
      chemname(16) = 'S'
      chemdefn(16) = 16
      chemname(17) = 'Cl'
      chemdefn(17) = 18
      chemname(18) = 'Ar'
      chemdefn(18) = 22
      chemname(19) = 'K'
      chemdefn(19) = 20
      chemname(20) = 'Ca'
      chemdefn(20) = 20
      chemname(21) = 'Sc'
      chemdefn(21) = 24
      chemname(22) = 'Ti'
      chemdefn(22) = 26
      chemname(23) = 'V'
      chemdefn(23) = 28
      chemname(24) = 'Cr'
      chemdefn(24) = 28
      chemname(25) = 'Mn'
      chemdefn(25) = 30
      chemname(26) = 'Fe'
      chemdefn(26) = 30
      chemname(27) = 'Co'
      chemdefn(27) = 32
      chemname(28) = 'Ni'
      chemdefn(28) = 30
      chemname(29) = 'Cu'
      chemdefn(29) = 34
      chemname(30) = 'Zn'
      chemdefn(30) = 34
      chemname(31) = 'Ga'
      chemdefn(31) = 38
      chemname(32) = 'Ge'
      chemdefn(32) = 42
      chemname(33) = 'As'
      chemdefn(33) = 42
      chemname(34) = 'Se'
      chemdefn(34) = 46
      chemname(35) = 'Br'
      chemdefn(35) = 44
      chemname(36) = 'Kr'
      chemdefn(36) = 48
c
c     Assigning basic nuclear properties.
c
      do ncode = minncode, maxncode
c
        call nucldecode(ncode, z, n, a)
c
        nucn(ncode) = n
        if (n .ge. 0) then
          nucz(ncode)    = z
          nuca(ncode)    = a
          nucmass(ncode) = z * protonmass + n * neutronmass
        else
          nucmass(ncode) = -999.d0
        endif
c
      enddo
c
c     Some known nuclei defined as synonyms.
c
      psynonym( 21) = 'Deuterium'
      psyncode( 21) = nuclcode(1, 1, idummy)
      psynonym( 22) = 'deuterium'
      psyncode( 22) = psyncode(21)
      psynonym( 23) = 'Tritium'
      psyncode( 23) = nuclcode(1, 2, idummy)
      psynonym( 24) = 'tritium'
      psyncode( 24) = psyncode(23)
      psynonym( 25) = 'Alpha'
      psyncode( 25) = nuclcode(2, 2, idummy)
      psynonym( 26) = 'alpha'
      psyncode( 26) = psyncode(25)
      psynonym( 27) = 'Iron'
      psyncode( 27) = nuclcode(26, 30, idummy)
      psynonym( 28) = 'iron'
      psyncode( 28) = psyncode(27)
c
c     Auxiliary variables defined for convenience.
c
      alphapcode = nuclcode(2, 2, idummy)
c
c     GROUP DEFINITION.
c
c     A.- "generic particle names" groups.
c         Notice that some particles appear more than once. This is
c         to improve performance of certain routines.
c
c         NOTE: The group indices must match the corresponding
c               parameters defined in file 'pclecodes.f'
c
      gengrpname( 1)     = 'e+-'
      gengrpsize( 1)     = 2
      gengrppcle(0,   1) = electroncode
      gengrppcle(1,   1) = positroncode
c
      gengrpname( 2)     = 'mu+-'
      gengrpsize( 2)     = 2
      gengrppcle(0,   2) = muminuscode
      gengrppcle(1,   2) = mupluscode
c
      gengrpname( 3)     = 'tau+-'
      gengrpsize( 3)     = 2
      gengrppcle(0,   3) = tauminuscode
      gengrppcle(1,   3) = taupluscode
c
      gengrpname( 4)     = 'GPion'
      gengrpsize( 4)     = 3
      gengrppcle(0,   4) = pizerocode
      gengrppcle(1,   4) = pipluscode
      gengrppcle(2,   4) = piminuscode
      gengrppcle(-1,  4) = piminuscode
c
      gengrpname( 5)     = 'GChPion'
      gengrpsize( 5)     = 2
      gengrppcle(0,   5) = piminuscode
      gengrppcle(1,   5) = pipluscode
      gengrppcle(-1,  5) = piminuscode
c
      gengrpname( 6)     = 'GKaon'
      gengrpsize( 6)     = 4
      gengrppcle( 0,  6) = k0scode
      gengrppcle( 1,  6) = k0lcode
      gengrppcle( 2,  6) = kpluscode
      gengrppcle( 3,  6) = kminuscode
      gengrppcle(-1,  6) = kminuscode
c
      gengrpname( 7)     = 'GChKaon'
      gengrpsize( 7)     = 2
      gengrppcle( 0,  7) = kminuscode
      gengrppcle( 1,  7) = kpluscode
c
      gengrpname( 8)     = 'nppbar'
      gengrpsize( 8)     = 3
      gengrppcle( 0,  8) = neutroncode
      gengrppcle( 1,  8) = protoncode
      gengrppcle( 2,  8) = pbarcode
      gengrppcle(-1,  8) = pbarcode
c
      gengrpname( 9)    = 'nnbar'
      gengrpsize( 9)    = 2
      gengrppcle( 0,  9) = neutroncode
      gengrppcle( 1,  9) = nbarcode
c
      gengrpname(10)     = 'ppbar'
      gengrpsize(10)     = 2
      gengrppcle( 0, 10) = protoncode
      gengrppcle( 1, 10) = pbarcode
c
      gengrpname(11)     = 'Nucnucbr'
      gengrpsize(11)     = 5
      gengrppcle( 0, 11) = neutroncode
      gengrppcle( 1, 11) = protoncode
      gengrppcle( 2, 11) = pbarcode
      gengrppcle( 3, 11) = nbarcode
      gengrppcle(-1, 11) = pbarcode
c
      gengrpname(12)     = 'EM'
      gengrpsize(12)     = 3
      gengrppcle( 0, 12) = gammacode
      gengrppcle( 1, 12) = positroncode
      gengrppcle( 2, 12) = electroncode
      gengrppcle(-1, 12) = electroncode
c
c     Checking if storage space is OK.
c
      if ((maxgengroup .lt. 12) .or. (mxgengrpsize1 .lt. 3))
     +   call errprint(0, '*', 4, 'inipcledata',
     +   'Not enough space to store generic group data.',
     +   0, 0, 0, 0.d0, ' ')
c
c     B.- Common groups.
c
c     Group -1 : "Remaining particles" group. This group is used
c                only when assigning save files or stacks.
c     Group  0 : Empty group. No particles belong to it.
c     Group  1 : All Particles and Nuclei.
c     Group  2 : Charged particles or nuclei
c     Group  3 : Neutral massive particles.
c     Group  4 : Nuclei.
c     Group  5 : Hadrons.
c     Group  6 : Neutrinos.
c
      pgname(-1) = 'AnyOther'
      pgname( 0) = 'NoParticles'
      pgname( 1) = 'AllParticles'
      pgname( 2) = 'AllCharged'
      pgname( 3) = 'MassiveNeutral'
      pgname( 4) = 'Nuclei'
      pgname( 5) = 'Hadrons'
      pgname( 6) = 'Neutrinos'
c
c     Setting synonyms for groups. Notice that code for a group must
c     be syngrouplim plus group number.
c     This is the last block of synonyms within this routine.
c
      psynonym( 29) = 'anyother'
      psyncode( 29) = syngrouplim - 1
      psynonym( 30) = 'noparticles'
      psyncode( 30) = syngrouplim + 0
      psynonym( 31) = 'None'
      psyncode( 31) = psyncode(30)
      psynonym( 32) = 'none'
      psyncode( 32) = psyncode(30)
      psynonym( 33) = 'All'
      psyncode( 33) = syngrouplim + 1
      psynonym( 34) = 'all'
      psyncode( 34) = psyncode(33)
      psynonym( 35) = 'allcharged'
      psyncode( 35) = syngrouplim + 2
      psynonym( 36) = 'NeutralMassive'
      psyncode( 36) = syngrouplim + 3
      psynonym( 37) = 'massiveneutral'
      psyncode( 37) = psyncode(36)
      psynonym( 38) = 'neutralmassive'
      psyncode( 38) = psyncode(36)
      psynonym( 39) = 'nuclei'
      psyncode( 39) = syngrouplim + 4
      psynonym( 40) = 'hadrons'
      psyncode( 40) = syngrouplim + 5
      psynonym( 41) = 'neutrinos'
      psyncode( 41) = syngrouplim + 6
c
c     Checking if synonym storage space is OK.
c
      if (maxpsyn .lt. 41) call errprint(0, '*', 4, 'inipcledata',
     +   'Not enough space to store particle synonym data.',
     +   0, 0, 0, 0.d0, ' ')
c
c     Processing the group data.
c
      pgsize(-1) = -1
      do j = 0, maxpgroup
        pgsize(j) = 0
      enddo
c
      do i = -maxpcle, maxncode
        do j = 1, maxpgroup
          allpgroup(j, i) = .false.
        enddo
      enddo
c
      do i = -maxpcle, maxpcle
        if (pclename(i) .ne. 'NOT_USED') then
          allpgroup(1, i) = .true.
          pgsize(1)       = pgsize(1) + 1
          if (pcleq(i) .ne. 0) then
            allpgroup(2, i) = .true.
            pgsize(2)       = pgsize(2) + 1
          else if (pclemass(i) .gt. 0) then
            allpgroup(3, i) = .true.
            pgsize(3)       = pgsize(3) + 1
          endif
          if ((abs(i) .ge. pizerocode) .and. (pclemass(i) .gt. 0)) then
            allpgroup(5, i) = .true.
            pgsize(5)       = pgsize(5) + 1
          endif
        endif
c
      enddo
c
c     Neutrinos group.
c
      do i = 6, 8
        allpgroup(6, -i) = .true.
        allpgroup(6,  i) = .true.
      enddo
      pgsize(6) = 6
c
      do ncode = minncode, maxncode
        if (nucn(ncode) .ge. 0) then
          allpgroup(1, ncode) = .true.
          allpgroup(2, ncode) = .true.
          allpgroup(4, ncode) = .true.
          pgsize(1)           = pgsize(1) + 1
          pgsize(2)           = pgsize(2) + 1
          pgsize(4)           = pgsize(4) + 1
        endif
      enddo
c
c     Some initializations common to all particles. "allpclesave"
c     is initialized within the cioaux package, after knowing
c     what compressed files were actually defined.
c
c     pcleresample is set to "true" only for gammas, electrons and muons.
c     particle stacks are set to 1. Only different stacks must be
c     set. This will be done when initializing the scheduler.
c
      do i = -maxpcle, maxncode
        pcleresample(i)   = .false.
        allpclesta0(i)  = 1
        allpclesta1(i)  = 1
      enddo
c
      pcleresample(gammacode)    = .true.
      pcleresample(electroncode) = .true.
      pcleresample(positroncode) = .true.
      pcleresample(muminuscode)  = .true.
      pcleresample(mupluscode)   = .true.
c
      do j = 1, 3
        anypinfile(j) = .true.
      enddo
c
      return
c
      end
c     --- End of routine inipcledata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getpclecode(advance, string, slen, wfirst, wlast,
     +                       ncodes, pgcode, irc)
c
c     Extracting (optionally) the first word from a character string,
c     and reading it (and eventually other subsequent words) as a
c     particle or group specification
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999.
c
c
c     Arguments:
c     =========
c
c     advance......... (input, logical) Switch to decide whether or
c                      not invoke routine "nextword" to extract the
c                      first word from "string".
c     string.......... (input, character*(*)) The string to scan
c     slen............ (input, integer) String length, or last
c                      position to scan.
c     wfirst.......... (input-output, integer) Position of the first
c                      character of the given or to be localized word.
c     wlast........... (input-output, integer) Position of the last
c                      character of the given or last position scanned.
c     ncodes.......... (output, integer) If zero and irc le 2, it
c                      means that the input specification corresponds
c                      to a single particle. In this case pgcode
c                      contains the corresponding particle code. If
c                      ncodes = n and irc = 3, it means that
c                      the input specification corresponds to a
c                      group of particles of size n. In this case
c                      pgcode contains the group code.
c     pgcode.......... (output, integer) The particle or group code
c                      (see "ncodes").
c     irc............. (output, integer) Return code. 0 means that a
c                      valid particle/nucleus was specified and
c                      processed.
c                      1 means that the particle/nucleus specification
c                      was incomplete and some default values were
c                      provided.
c                      2 means that a "special" (escape) particle code
c                      was specified.
c                      3 means that a valid group of particles was
c                      specified and processed.
c                      8 means that no particle/group was specified.
c                      9 means that and invalid particle/group
c                      specification was processed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      logical           advance
      character*(*)     string
      integer           slen, wfirst, wlast
      integer           ncodes, pgcode, irc
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           nuclcode
      integer           i, ip, wlen, z, n, tmpa, idelim
      logical           delim
      double precision  tmp1, tmp2
      integer           itmp1, itmp2
      character*2       zna(6)
c
      data              zna / 'ZA', 'AZ', 'ZN', 'NZ', 'NA', 'AN' /
c
c     FIRST EXECUTABLE STATEMENT
c
      if (advance) call nextword(string, slen, wfirst, wlast)
c
      if (wfirst .gt. wlast) then
        irc = 8
        return
      endif
c
      irc    = 0
      ncodes = 0
c
c     Analysing the given string.
c
c     Searching for a synonym.
c
      do i = 1, maxpsyn
        ip = i
        if (string(wfirst:wlast) .eq. psynonym(i)) goto 1100
      enddo
c
c     Searching for a group name.
c
      do i = -1, maxpgroup
        ip = i
        if (string(wfirst:wlast) .eq. pgname(i)) goto 1200
      enddo
c
      do i = 1, maxgengroup
        ip = i
        if (string(wfirst:wlast) .eq. gengrpname(i)) goto 1250
      enddo
c
c     Searching for an elementary particle name.
c
      do i = -maxpcle, maxpcle
        ip = i
        if (string(wfirst:wlast) .eq. pclename(i)) goto 1300
      enddo
c
c     Searching for a "special" particle (escape code).
c
      do i = 1, nescpcles
        ip = i
        if (string(wfirst:wlast) .eq. escpclename(i)) goto 1320
      enddo
c
c     Searching for a nucleus specification
c
      wlen = wlast - wfirst + 1
c
      if (wlen .eq. 2) then
c
c       Searching for ZNA specifications.
c
        do i = 1, 6
          ip = i
          if (string(wfirst:wlast) .eq. zna(i)) goto 1400
        enddo
c
      endif
c
c     Searching for mass number separator.
c
      if (wlen .ge. 2) then
        idelim = wfirst + 1
        if (string(idelim:idelim) .eq. adelim) then
          delim = .true.
        else
          idelim = min(idelim + 1, wlast)
          delim  = (idelim .lt. wlast) .and.
     +             (string(idelim:idelim) .eq. adelim)
        endif
      else
        delim = .false.
      endif
c
      if (delim) then
        read(string(idelim+1:wlast), *, err = 3010) tmpa
        idelim = idelim - 1
      else
        idelim = wlast
      endif
c
c     Searching the chemical symbol.
c
      do i = 1, maxnucz
        ip = i
        if (string(wfirst:idelim) .eq. chemname(i)) goto 1500
      enddo
c
c     Invalid particle specification.
c
 3010 continue
      irc = 9
      return
c
 1100 continue
c
c     A synonym was specified.
c     May be a particle or a group synonym.
c
      ip = psyncode(ip)
c
      if (ip .lt. syngroupcode) then
c
c       Particle synonym.
c
        pgcode = ip
        return
c
      else
c
c       Group synonym (processing continued in next section).
c
        ip = ip - syngrouplim
      endif
c
 1200 continue
c
c     A group of particles was specified.
c
      ncodes = pgsize(ip)
      pgcode = ip
      irc    = 3
      return
c
 1250 continue
c
c     A "generic" group was specified.
c
      ncodes = gengrpsize(ip)
      pgcode = ip + maxpgroup
      irc = 3
      return
c
 1300 continue
c
c     An elementary particle was specified.
c
      pgcode = ip
      return
c
 1320 continue
c
c     A "special" particle (escape code) was specified.
c
      pgcode = ip + pescode1 - 1
      irc = 2
      return
c
 1400 continue
c
c     ZNA nuclear specification.
c
      delim = .true.
      call getnumber(.true., string, slen, wfirst, wlast,
     +               .false., 0.0d0, tmp1, irc)
      if (irc .gt. 0) goto 3010
      itmp1 = tmp1
      if (itmp1 .ne. tmp1) goto 3010
      call getnumber(.true., string, slen, wfirst, wlast,
     +               .false., 0.0d0, tmp2, irc)
      if (irc .eq. 4) goto 3010
      itmp2 = tmp2
c
      if (ip .eq. 1) then
c
c       ZA specification.
c
        z = itmp1
        if (irc .ne. 0) then
          n = chemdefn(z)
          delim = .false.
        else
          if (itmp2 .ne. tmp2) goto 3010
          n = itmp2 - z
        endif
c
      else if (ip .eq. 2) then
c
c       AZ specification.
c
        if ((irc .ne. 0) .or. (itmp2 .ne. tmp2)) goto 3010
        z = itmp2
        n = itmp1 - z
c
      else if (ip .eq. 3) then
c
c       ZN specification.
c
        z = itmp1
        if (irc .ne. 0) then
          n = chemdefn(z)
          delim = .false.
        else
          if (itmp2 .ne. tmp2) goto 3010
          n = itmp2
        endif
c
      else if (ip .eq. 4) then
c
c       NZ specification.
c
        n = itmp1
        if ((irc .ne. 0) .or. (itmp2 .ne. tmp2)) goto 3010
        z = itmp2
c
      else if (ip .eq. 5) then
c
c       NA specification.
c
        n = itmp1
        if ((irc .ne. 0) .or. (itmp2 .ne. tmp2)) goto 3010
        z = itmp2 - n
c
      else
c
c       AN specification.
c
        if ((irc .ne. 0) .or. (itmp2 .ne. tmp2)) goto 3010
        n = itmp2
        z = itmp1 - n
c
      endif
c
      goto 1600
c
 1500 continue
c
c     Nucleus specified by chemical symbol.
c
      z = ip
c
      if (delim) then
        n = tmpa - z
      else
        n = chemdefn(z)
      endif
c
 1600 continue
c
c     Final processing of nucleus specification.
c
      pgcode = nuclcode(z, n, irc)
      if (irc .eq. 0) then
        if (.not. delim) irc = 1
      else
        irc = 9
      endif
      return
c
      end
c     --- End of routine getpclecode
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pglist(gcode, size, pcodes)
c
c     Listing the particle/nucleus codes corresponding to a given
c     group.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     gcode........... (input, integer) The group code.
c     size............ (output, integer) If negative, it indicates
c                      that an invalid group was specified. Otherwise
c                      it returns the number of particle/nucleus
c                      belonging to the group.
c     pcodes.......... (output, integer, array(size)) Array
c                      containing the particle codes.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           gcode, size
      integer           pcodes(1)
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, gc
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((gcode .ge. 0) .and. (gcode .le. maxpgroup)) then
c
c       Valid group code.
c
        size = 0
        if (pgsize(gcode) .le. 0) return
c
        do i = -maxpcle, maxpcle
          if (allpgroup(gcode, i)) then
            size = size + 1
            pcodes(size) = i
          endif
        enddo
c
        do i = minncode, maxncode
          if (allpgroup(gcode, i)) then
            size = size + 1
            pcodes(size) = i
          endif
        enddo
c
      else if (gcode .le. (maxpgroup + maxgengroup)) then
c
c       Valid "generic" group code.
c
        gc   = gcode - maxpgroup
        size = gengrpsize(gc)
c
        do i = 0, size - 1
          pcodes(i + 1) = gengrppcle(i, gc)
        enddo
c
      else
c
c       Invalid group specification.
c
        size = -1
c
      endif
      return
c
      end
c     --- End of routine pglist
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pnice(exflag, pcode, pstring, pslen)
c
c     Expressing a particle code in convenient format (particle symbol)
c     for printing.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     exflag.......... (input, integer) Format flag. 0 or negative
c                      means short format (Ex: Fe^56). Positive means
c                      long format (Ex: Fe^56 (Z=26, N=30).)
c     pcode........... (input, integer) The particle code.
c     pstring......... (output, character*(*)) A string containing
c                      the formatted output. Minimum length of this
c                      string: 1, maximum: 18
c     pslen........... (output, integer) Position of last nonblank
c                      character of "pstring".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           exflag, pcode
      character*(*)     pstring
      integer           pslen
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, z, n, a
c
c     FIRST EXECUTABLE STATEMENT
c
c     Switching accordingly with the particle code.
c
      if (abs(pcode) .le. maxpcle) then
c
c       A valid elementary particle code was specified.
c
        pstring = pclename(pcode)
        do i = 8, 1, - 1
          pslen = i
          if (pstring(i:i) .ne. ' ') return
        enddo
c
      else if (((pcode .ge. pescode1) .and.
     +          (pcode .le. lastescpcle))
     +         .or.
     +         ((pcode .ge. -99999) .and.
     +          (pcode .le. (lastescpcle - (pescode1 + 99999))))
     +        ) then
c
c       A valid "Special" particle code was specified.
c
        if (pcode .ge. pescode1) then
          pstring = escpclename(pcode - pescode1 + 1)
        else
          pstring = escpclename(pcode + 100000)
        endif
        do i = 16, 1, - 1
          pslen = i
          if (pstring(i:i) .ne. ' ') return
        enddo
c
      else if ((pcode .ge. minncode) .and.
     +         (pcode .le. maxncode))      then
c
        n = nucn(pcode)
c
        if (n .ge. 0) then
c
c         A valid nucleus code was specified.
c
          z = nucz(pcode)
          a = nuca(pcode)
c
          pstring = chemname(z)
          if (pstring(2:2) .ne. ' ') then
            i = 3
          else
            i = 2
          endif
c
          if (a .lt. 10) then
            write(pstring(i:i+1), 2010) adelim, a
 2010       format(a1, i1)
            i = i + 1
          else
            write(pstring(i:i+2), 2020) adelim, a
 2020       format(a1, i2)
            i = i + 2
          endif
c
c         Long format
c
          if (exflag .gt. 0) then
c
            pslen = i + 13
            write(pstring(i+1:pslen), 2030) ' (Z=', z, ', N=', n, ')'
 2030       format(2(a, i2), a)
c
          else
            pslen = i
          endif
c
        else
c
c         An invalid nucleus code was specified.
c
          pstring = 'INVALID NUC. CODE'
          pslen   = 17
c
        endif
c
      else
c
c       An invalid particle code was specified.
c
        pstring = 'INVALID CODE'
        pslen   = 12
c
      endif
      return
c
      end
c     --- End of routine pnice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function nuclcode(z, n, irc)
c
c     Returning the code corresponding to a nucleus of charge z and
c     n neutrons.
c     The formula used for encoding is:
c
c               nuclcode = (M0 + aa) + (M - 1) z + n
c
c     where M0, M and aa are constants adequately chosen.
c     This method provides a biunivocal nuclei identification system,
c     provided that (z - aa) .le. n .le. (M + z - aa).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     z............... (input, integer) The charge of the nucleus.
c     n............... (input, integer) The number of neutrons.
c     irc............. (output, integer) Return code. 0 means that a
c                      valid pair (z,n) was specified and processed.
c                      3 means that the nucleus (z,n) cannot be
c                      specified with this system. 5 means that
c                      either z or n are out of allowed ranges.
c
c     Return value: (integer) The nucleus code.
c     ============
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           nuclcode
      integer           z, n
      integer           irc
c
c     Declaration of internal variables and arrays.
c
      integer           delta
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the arguments.
c
      if ((z .le. 0) .or. (z .gt. maxnucz) .or. (n .lt. 0)) then
        nuclcode = 0
        irc = 5
        return
      endif
c
      delta = n - z + nuccod2
      if ((delta .lt. 0) .or. (delta .ge. nuccod1)) then
        nuclcode = 0
        irc = 3
      else
c
c       The arguments are valid, encoding.
c
        nuclcode = nuccod0 + nuccod1 * z + delta
        irc      = 0
c
      endif
      return
c
      end
c     --- End of routine nuclcode.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine nucldecode(ncode, z, n, a)
c
c     Returning the charge (z), number of neutrons (n) and mass
c     number (a) of a nucleus of code "ncode"
c     "ncode" is assumed to be a valid code.
c     The formulae used for decoding is:
c
c               z = integer[ (ncode - M0) / M ]
c               n = ncode - (M0 + aa) - (M - 1) z
c               a = z + n
c
c     M0, M and aa are constants adequately chosen.
c     This method provides a biunivocal nuclei identification system,
c     provided that (z - aa) .le. n .le. (M + z - aa).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     ncode........... (input, integer) The nucleus code.
c     z............... (output, integer) The charge of the nucleus.
c     n............... (output, integer) The number of neutrons.
c     a............... (output, integer) The mass number.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           ncode
      integer           z, n, a
c
c     FIRST EXECUTABLE STATEMENT
c
      z = (ncode - nuccod0) / nuccod1
      n = ncode - (nuccod1 - 1) * z - (nuccod0 + nuccod2)
      a = z + n
c
      return
      end
c     --- End of routine nucldecode.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'particles.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
