c
c     FILE: initadf.f                       Creation date: 13/APR/1999.
c                                       LAST MODIFICATION: 27/OCT/2000.
c
c     This file contains several initialization routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine init3adf
c
c     Initializations to be performed before starting an ulterior
c     simulation job.
c
c     Written by: S. J. Sciutto, Fermilab 1999, La Plata 2000.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           irc
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting data related to dynamical variables.
c
c     Opening the idf file and reading it.
c
      call adfread(' ', 4, irc)
c
      adfile = .true.
c
c     Calling the atmospheric model initializing routine.
c
      call atmosinit(max(1, atmoslabel), atmosmodel)
c
      return
      end
c     --- End of routine init3adf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initadf.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
