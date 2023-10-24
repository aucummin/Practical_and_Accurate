c
c     FILE: schedulerfun.f                  Creation date: 13/NOV/1996.
c                                       LAST MODIFICATION: 07/JAN/1997.
c
c     Declarations of the subroutines used to process each particle
c     stack.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c(---)(---)(---)(---)(---)(---)(---)*(---)(---)(---)(---)(---)(---)(---)
c(---)(---)(---)(---)(---)(---)(---)*(---)(---)(---)(---)(---)(---)(---)
c
c     Description of the stacks.
c     For each stack, a stack number and a stack name must be
c     provided, together with one or two stack processing routines,
c     and the corresponding names for them. A particle or group name
c     describing the particles associated with the stack must be
c     provided.
c
c     The stack parameters are declared in file 'schedulerpar.f'
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
c     The call statements for the stack processing routines are:
c
c     call adroutine(nparticles, pdata)
c     call adroutine(nparticles, pdata, nrem, remidx)  (ia and
c                                                       iia only)
c
c     with:
c
c     nparticles..... (input, integer) The number of particles to
c                     process.
c     pdata.......... (input-output, character*(*),
c                     array(nparticles)) Array containing the particle
c                     data.
c     nrem........... (output, integer) Number of remaining particles
c                     in the stack.
c     remidx......... (output, integer, array(nparticles)) Indices of
c                     remaining particles.
c
c     call dyroutine(nparticles, pdata, nrem, remidx)
c
c     with:
c
c     nparticles..... (input, integer) The number of particles to
c                     process.
c     pdata.......... (input, character*(*), array(nparticles))
c                     Array containing the particle data.
c     nrem........... (output, integer) Number of remaining particles
c                     in the stack.
c     remidx......... (output, integer, array(nparticles)) Indices of
c                     remaining particles.
c
c
c     STACK 1: Gamma rays stack (Processing mode ii):
c
c     Stack processing routine(s):
c
      external          gammaadv
      external          gammadecay
c
c     STACK 2: e+e- stack (Processing mode iia):
c
c     Stack processing routine(s):
c
      external          epmadv
      external          epmdecay
c
c
c     STACK 3: Massive neutral particles stack (Processing mode ii):
c
c     Stack processing routine(s):
c
      external          heavyntadv
      external          heavyntdecay
c
c     STACK 4: Heavy charged particles stack (Processing mode iia):
c
c     Stack processing routine(s):
c
      external          heavychadv
      external          heavychdecay
c
c(---)(---)(---)(---)(---)(---)(---)*(---)(---)(---)(---)(---)(---)(---)
c(---)(---)(---)(---)(---)(---)(---)*(---)(---)(---)(---)(---)(---)(---)
c
c     End of file 'schedulerfun.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
