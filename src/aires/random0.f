c
c     FILE: random0.f                       Creation date: 17/JUN/1996.
c                                       LAST MODIFICATION: 21/MAR/2001.
c
c     Uniform random number generator.
c     This file contain a subroutine package for generating uniformly
c     distributed pseudorandom numbers.
c     This generator is equivalent to the MOCCA program random number
c     generator.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine raninit(semp)
c
c     Initialization of the uniform pseudorandom number generator.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999.
c
c     Arguments:
c     =========
c
c     semp............ (input, double precision) Random seed. If semp
c                      belongs to the interval (0, 1) then it is used
c                      as random seed. Otherwise a seed is internally
c                      provided (using the clock time).
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  semp
c
c     Declaration of parameters and shared data.
c
      include 'randomdata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, m
      double precision  x, x2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the seed.
c
      call setsem(semp)
c
c     Continuing with the initialization.
c
c     First element of the series.
c
      x  = original_seed
      x2 = a0
c
c     "Heating" the initialized series.
c
      do i = 1, 107
c
        x2 = x2 + c1
        if (x2 .gt. z1) x2 = x2 - z2
        x = (x + 0.5d0) * x2
        m = x
        x = x - m
c
      enddo
c
      xran  = x
      xran2 = x2
c
      return
c
      end
c     --- End of routine raninit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setsem(semp)
c
c     Evaluating a seed for the uniform pseudorandom number generator.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2000.
c
c     Arguments:
c     =========
c
c     semp............ (input, double precision) Random seed. If semp
c                      belongs to the interval (0, 1) then it is used
c                      as random seed. Otherwise a seed is internally
c                      provided (using the clock time).
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  semp
c
c     Declaration of parameters and shared data.
c
      include 'randomdata.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  clockrandom
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((semp .gt. 0) .and. (semp .lt. 1)) then
c
c       Externally provided seed.
c
        original_seed = semp
c
      else
c
c       Seed evaluated internally.
c
        original_seed = clockrandom()
c
      endif
c
      return
      end
c     --- End of routine setsem
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ranauxsync
c
c     Setting the current status of the random number generator
c     accordingly with the auxiliary status variable.
c     The previous status variables are superseded.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
      implicit none
c
c     Declaration of parameters and shared data.
c
      include 'randomdata.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      xran          = auxxran
      xran2         = auxxran2
      original_seed = auxoriginal_seed
c
      return
      end
c     --- End of routine ranauxsync
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function urandom()
c
c     This function returns a uniformly distributed (interval [0, 1))
c     pseudorandom number.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c     Return value: (double precision) The pseudorandom number.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  urandom
c
c     Declaration of parameters and shared data.
c
      include 'randomdata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           m
      double precision  x, x2
c
c     FIRST EXECUTABLE STATEMENT
c
      x  = xran
      x2 = xran2
c
      x2 = x2 + c1
      if (x2 .gt. z1) x2 = x2 - z2
      x = (x + 0.5d0) * x2
      m = x
      x = x - m
c
      xran  = x
      xran2 = x2
c
      urandom = x
c
      return
c
      end
c     --- End of routine urandom
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function urandomt(th)
c
c     This function returns a uniformly distributed (interval (th, 1))
c     pseudorandom number. It is assumed that th is a small positive
c     number.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c     Arguments:
c     =========
c
c     th.............. (input, double precision) Lower bound for the
c                      random number to be returned.
c
c     Return value: (double precision) The pseudorandom number.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  urandomt
      double precision  th
c
c     Declaration of parameters and shared data.
c
      include 'randomdata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           m
      double precision  x, x2
c
c     FIRST EXECUTABLE STATEMENT
c
      x  = xran
      x2 = xran2
c
 1010 continue
      x2 = x2 + c1
      if (x2 .gt. z1) x2 = x2 - z2
      x = (x + 0.5d0) * x2
      m = x
      x = x - m
      if (x .le. th) goto 1010
c
      xran  = x
      xran2 = x2
c
      urandomt = x
c
      return
c
      end
c     --- End of routine urandomt
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function clockrandom()
c
c     This function returns a uniformly distributed (interval (0, 1))
c     pseudorandom number generated with the current clock and CPU
c     usage lectures.
c
c     WARNING: THIS FUNCTION IS NOT FOR USE AS A HIGH QUALITY RANDOM
c              NUMBER GENERATOR.
c
c     Multiple calls may return correlated numbers if there is no
c     enough time between invocations. Nevertheless, a sequence of
c     different numbers passes direct 1d and 2d chisquare tests,
c     ensuring a minimum quality for the generated numbers.
c
c     This routine is intended for special applications like
c     generating a single random seed, for example.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c     Return value: (double precision) The pseudorandom number.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  clockrandom
c
c     Declaration of internal variables and arrays.
c
      integer           i1, i2, i3, k0, k1
      integer           idati(6)
      double precision  f0, f1, dummy
c
      double precision  d1
      parameter         (d1 = 4.2457996800000000d12)
c
      double precision  f0save
      integer           k0save, k1save, i2save
      save              f0save, k0save, k1save, i2save
      double precision  m0(0:4), m1(0:4)
c
      data              k0save, k1save / -1, -1 /
c
      data              m0  /  3.3409785643499783d7,
     +                         4.2229873451092387d7,
     +                         7.0059856230199237d7,
     +                         6.3094289404598575d7,
     +                         8.9828383387892293d7  /
c
      data              m1  /  9.3456787321213457d8,
     +                         4.2229873451092387d8,
     +                         1.8450924598701233d8,
     +                         6.3094289404598571d8,
     +                         2.9828383387892291d8  /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Current clock lecture
c
      call intdati(idati)
c
      idati(1) = mod(idati(1), 100) + 1
      idati(2) = idati(2) - 1
      idati(3) = idati(3) - 1
c
c     Current CPU time lecture.
c
      call cputime(.false., f1, dummy)
c
      f1 = 10 * f1 + 1.d-14
      i1 = f1
      f1 = f1 - i1
      i2 = i1 / 1321
      i1 = i1 - 1321 * i2
c
c     Mixing clock and CPU time lectures.
c     k0 belongs to the interval [1, 37200],
c     k1 belongs to the interval [0, 114134399], and
c     k0+37200*k1 belongs to the interval [1, 4.24579968E12].
c
      i2 = 10 * f1
      i2 = i2 + idati(6)
      i3 = 30 * (idati(6) + 59 * f1)
c
c     Evaluating the random number.
c
      k0 = idati(1) + 100 * (idati(2) + 12 * idati(3))
      k1 = i1 + 1321 * (idati(4) + 24 * (idati(5) + i3))
c
      if ((k1 .eq. k1save) .and. (k0 .eq. k0save)) then
        i2save = i2save + 1
        if (i2save .gt. 4) i2save = 0
        f0     = f0save
      else
        i2save = mod(i2, 5)
        k0save = k0
        k1save = k1
        f0     = k0 + 37200.d0 * k1
      endif
c
      f0     = m0(i2save) + f0 * m1(i2save)
      i1     = f0 / d1
      f0     = f0 - i1 * d1
      f0save = f0
c
      clockrandom = (f0 + 1) / (d1 + 1)
c
      end
c     --- End of routine clockrandom
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'random0.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
