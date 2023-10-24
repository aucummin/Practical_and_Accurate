c
c     FILE: idfread_auxd.f                  Creation date: 27/OCT/2000.
c                                       LAST MODIFICATION: 27/OCT/2000.
c
c     Dummy routines, to ensure compatibility of external calls to
c     idfread and related routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function defsta(openflag, nsta, firstcl, sizew, basefn)
c
c     Defining and initializing the stacks to use.
c
      implicit none
c
c     Declaration of arguments.
c
      logical           defsta
      integer           openflag, nsta
      integer           firstcl(nsta)
      double precision  sizew(nsta)
      character*(*)     basefn
c
      defsta = .false.
      return
      end
c     --- End of routine defsta.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacp2d(sequ, irc)
c
c     This subroutine reads all the stacks from a sequential
c     unformatted file. The sequential file is assumed to be
c     already opened, and it is not closed after completing
c     the operation.
c     The stacks are cleared before reading data in.
c
      implicit none
c
c     Declaration of arguments.
c
      integer           sequ, irc
c
      return
      end
c     --- End of routine stacp2d
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setstackw(sw)
c
c     Setting the stack weights.
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  sw(99)
c
      return
      end
c     --- End of routine setstackw
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
c     End of file 'idfread_auxd.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
