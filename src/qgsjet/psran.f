c
c     FILE: psran.f                         Creation date: 23/OCT/1997.
c                                       LAST MODIFICATION: 23/OCT/1997.
c
c     Pseudorandom number generator for qgsjet.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function psran(arg)
c
c     Pseudorandom number generator for qgsjet.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     arg............ (double precision) Not used.
c
c     Return value: (double precision) The pseudorandom number.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  psran
      double precision  arg
c
c     Declaration of internal variables and arrays.
c
      double precision  urandomt
c
c     FIRST EXECUTABLE STATEMENT
c
      psran = urandomt(0.d0)
c
      return
c
      end
c     --- End of routine psran
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'psran.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
