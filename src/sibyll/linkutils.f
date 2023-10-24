c
c     FILE: linkutils.f                     Creation date: 25/JUN/1998.
c                                       LAST MODIFICATION: 02/MAY/2002.
c
c     Some routines used to link SIBYLL 2.1 to the AIRES system.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine sibyll_start
c
c     SIBYLL initialization routine for the AIRES system.
c     Adapted from the HEMAS interface SIBYLL_START.
c
c     Written by: S. J. Sciutto, La Plata 1998, 2002.
c
c
      implicit none
c
c     Declaration of shared data.
c
c     Data arrays from SIBYLL:
c
      integer           LDIFF
      COMMON            /S_CLDIF/ LDIFF
c
      integer           IDB(49), KDEC(612), LBARP(49)
      real              CBR(102)
      COMMON            /S_CSYDEC/ IDB, CBR, KDEC, LBARP
c
c     Declaration of internal variables and arrays.
c
      integer           j
c
c     FIRST EXECUTABLE STATEMENT
c
c     The random number generator is initialized by aires, so no
c     initialization instructions are placed here.
c
c     Calling sibyll initialization routines.
c
      call SIBYLL_INI
      call NUC_NUC_INI
c
c     Diffractive generation: generate all events:
c
      LDIFF = 0
c
c     Particles that are stable.......
c     SIBYLL has  4   5   6   7   8   9  10 11 12 13 14 15-18
c                mu+ mu- pi0 pi+ pi- K+ K- KL KS p  n  4 nus
c
c                23    39
c                eta   Lambda
c
      do j = 4, 14
        IDB(j) = -ABS(IDB(j))
      enddo
c
      IDB(23) = -ABS(IDB(23))
      IDB(39) = -ABS(IDB(39))
c
      return
      end
c     --- End of routine sibyll_start
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function s_rndm(arg)
c
c     Pseudorandom number generator for SIBYLL 2.1.
c     It uses the intrinsic AIRES random number generator, to return
c     single precision pseudorandom numbers uniformly distributed
c     in the interval (0, 1).
c
c     Written by: S. J. Sciutto, La Plata 1998, 2002.
c
c
c     Arguments:
c     =========
c
c     arg............ (integer) Not used.
c
c     Return value: (real) The pseudorandom number.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      real              s_rndm
      integer           arg
c
c     Declaration of internal variables and arrays.
c
      double precision  urandomt
c
c     FIRST EXECUTABLE STATEMENT
c
      s_rndm = urandomt(0.d0)
c
      return
c
      end
c     --- End of routine s_rndm
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'linkutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
