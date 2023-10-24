c
c     FILE: randomutils.f                   Creation date: 21/MAR/2001.
c                                       LAST MODIFICATION: 21/MAR/2001.
c
c     Random number related utilities.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getrandomseed0()
c
c     This function returns the original seed used to initialize the
c     random number generator, as saved in the dump files.
c     A dump file (either idf or adf) must be downloaded before using
c     this routine; otherwise the return value is undefined.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Return value: (double precision) The recovered original seed.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  getrandomseed0
c
c     Declaration of parameters and shared data.
c
      include 'randomdata.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      getrandomseed0 = auxoriginal_seed
c
      return
c
      end
c     --- End of routine getrandomseed0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'randomutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
