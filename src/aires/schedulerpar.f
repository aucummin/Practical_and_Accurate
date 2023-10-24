c
c     FILE: schedulerpar.f                  Creation date: 13/NOV/1996.
c                                       LAST MODIFICATION: 27/JAN/1999.
c
c     Definitions of the parameters used to process each particle
c     stack.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     Routines to process stacks and related parameters.
c
c
c     Description of the stacks.
c     For each stack, a stack number and a stack name must be
c     provided, together with one or two stack processing routines,
c     and the corresponding names for them. Particle(s) or group(s)
c     name(s) describing the particles associated with the stack must
c     be provided.
c
c     There are several stack processing modes:
c     (i)   Advance only mode: In this case the stack is processed
c           by a single routine (adroutine).
c     (ia)  Advance only with complex longitudinal tracking:
c           This mode is like mode (i), but no longitudinal tracking
c           is done after calling the advance routine. It must be
c           done within it, with the help of routine "lgtmaster".
c     (ii)  Advance and decay mode: In this case the stack is processed
c           by two routines (adroutine and dyroutine) called
c           sequentially.
c     (iia) Advance and decay with complex longitudinal tracking:
c           This mode is like mode (ii), but no longitudinal tracking
c           is done after calling the advance routine. It must be
c           done within it, with the help of routine "lgtmaster".
c     (iii) Decay only mode: In this case the stack is processed
c           by a single routine (dyroutine). This processing mode
c           is to be used at the very beginning of a shower.
c
c     The stack processing routines are defined in file
c     'schedulerfun.f'
c
c
c     STACK 1: Gamma rays stack (Processing mode ii):
c
c     Particles processed:
c
      character*(*)     gamma_sta_pcles
      parameter         (gamma_sta_pcles = 'Gamma CvPhoton Neutrinos')
c
c     Related parameters:
c
      integer           gamma_sta, gamma_sta_mode
      parameter         (gamma_sta = 1, gamma_sta_mode = 2)
      character*(*)     gamma_sta_n
      parameter         (gamma_sta_n = 'Gamma rays')
      character*(*)     gamma_sta_rn1, gamma_sta_rn2
      parameter         (gamma_sta_rn1 = 'GammaAdv')
      parameter         (gamma_sta_rn2 = 'GammaDecay')
      integer           gamma_sta_cat
      parameter         (gamma_sta_cat = 1)
      double precision  gamma_sta_w
      parameter         (gamma_sta_w = 750.d0)
c
c     STACK 2: e+e- stack (Processing mode iia):
c
c     Particles processed:
c
      character*(*)     eplumin_sta_pcles
      parameter         (eplumin_sta_pcles = 'e+ e-')
c
c     Related parameters:
c
      integer           eplumin_sta, eplumin_sta_mode
      parameter         (eplumin_sta = 2, eplumin_sta_mode = 21)
      character*(*)     eplumin_sta_n
      parameter         (eplumin_sta_n = 'Electron-Positron')
      character*(*)     eplumin_sta_rn1, eplumin_sta_rn2
      parameter         (eplumin_sta_rn1 = 'EpmAdv')
      parameter         (eplumin_sta_rn2 = 'EpmDecay')
      integer           eplumin_sta_cat
      parameter         (eplumin_sta_cat = 1)
      double precision  eplumin_sta_w
      parameter         (eplumin_sta_w = 750.d0)
c
c
c     STACK 3: Massive neutral particles stack (Processing mode ii):
c
c     Particles processed:
c
      character*(*)     heavynt_sta_pcles
      parameter         (heavynt_sta_pcles = 'NeutralMassive')
c
c     Related parameters:
c
      integer           heavynt_sta, heavynt_sta_mode
      parameter         (heavynt_sta = 3, heavynt_sta_mode = 2)
      character*(*)     heavynt_sta_n
      parameter         (heavynt_sta_n = 'Neutral particles')
      character*(*)     heavynt_sta_rn1, heavynt_sta_rn2
      parameter         (heavynt_sta_rn1 = 'HeavyntAdv')
      parameter         (heavynt_sta_rn2 = 'HeavyntDecay')
      integer           heavynt_sta_cat
      parameter         (heavynt_sta_cat = 0)
      double precision  heavynt_sta_w
      parameter         (heavynt_sta_w = 30.d0)
c
c
c     STACK 4: Heavy charged particles stack (Processing mode iia):
c
c     Particles processed:
c
      character*(*)     heavych_sta_pcles
      parameter         (heavych_sta_pcles = 'AnyOther')
c
c     Related parameters:
c
      integer           heavych_sta, heavych_sta_mode
      parameter         (heavych_sta = 4, heavych_sta_mode = 21)
      character*(*)     heavych_sta_n
      parameter         (heavych_sta_n = 'Heavy charged particles')
      character*(*)     heavych_sta_rn1, heavych_sta_rn2
      parameter         (heavych_sta_rn1 = 'HeavychAdv')
      parameter         (heavych_sta_rn2 = 'HeavychDecay')
      integer           heavych_sta_cat
      parameter         (heavych_sta_cat = 0)
      double precision  heavych_sta_w
      parameter         (heavych_sta_w = 150.d0)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'schedulerpar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
