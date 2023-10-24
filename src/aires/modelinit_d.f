c
c     FILE: modelinit_d.f                   Creation date: 22/MAR/1997.
c                                       LAST MODIFICATION: 31/AUG/1999.
c
c     Dummy model initialization routines, to ensure compatibility
c     of summary program(s).
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine extmodelname(modelname, namelen)
c
c     Routine to set the name of the external model. The name is a
c     character string that uniquely identifies the model.
c
c     Written by: S. J. Sciutto, La Plata 1999.
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
      modelname = ' '
      namelen   = 0
c
      return
c
      end
c     --- End of routine extmodelname.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelini0(nstacks, stackcat, stackweight, stackpcles)
c
c     Initializing the different stack processing routines (I).
c     This routine is invoked at the beginning of every process, just
c     before beginning the scanning of the input data file.
c
      implicit none
c
c     Declaration of arguments.
c
      integer           nstacks
      integer           stackcat(nstacks)
      double precision  stackweight(nstacks)
      character*(*)     stackpcles(nstacks)
c
c     The number of stacks is (artifically) set to zero to cancel
c     stack initialization.
c
      nstacks = 0
c
      return
      end
c     --- End of routine modelini0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelinitask(nstacks, stackname, stackmode,
     +                        ldr, stackrname, modelname)
c
c     Initializing the different stack processing routines (II).
c     One per stack.
c     This routine is invoked at the beginning of a new task.
c
      implicit none
c
c     Declaration of arguments.
c
      integer           nstacks, ldr
      character*(*)     stackname(nstacks)
      integer           stackmode(nstacks)
      character*(*)     stackrname(ldr, nstacks)
      character*(*)     modelname(nstacks)
c
      return
      end
c     --- End of routine modelinitask
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelinirun
c
c     Initializing the different stack processing routines (III).
c     One per stack.
c     This routine is invoked at every run, just before entering
c     the main kernel routine (scheduler).
c
      implicit none
c
      return
      end
c     --- End of routine modelinirun
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelnewshower
c
c     Initializing the different stack processing routines (IV).
c     One per stack.
c     This routine is invoked before starting the simulation of a new
c     shower.
c
      implicit none
c
      return
      end
c     --- End of routine modelnewshower
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelaftershower
c
c     Model calculatios to be performed after completion of a shower.
c     This routine is invoked after the simulation of a shower
c     completes (empty stacks) and before any statistical calculation
c     is done.
c
      implicit none
c
      return
      end
c     --- End of routine modelaftershower
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelidfwrite(iounit, rc)
c
c     Saving internal variables at the end of a run.
c     This routine is invoked when finishing any run.
c     The variables must be saved using routine idfput.
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iounit, rc
c
      return
      end
c     --- End of routine modelidfwrite
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine modelidfread(iounit, rc)
c
c     Reading internal variables at the end of a run.
c     This routine is invoked when starting any subsequent process,
c     BEFORE modelinirun is called.
c     The variables must be restored using routine idfget.
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iounit, rc
c
      return
      end
c     --- End of routine modelidfread
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'modelinit_d.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
