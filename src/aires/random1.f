c
c     FILE: random1.f                       Creation date: 12/NOV/1999.
c                                       LAST MODIFICATION: 12/NOV/1999.
c
c     Random number related routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function grandom()
c
c     This function returns a pseudorandom number distributed with a
c     Gaussian distribution of zero mean and unit standard deviation
c     (N(0, 1)).
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c     Return value: (double precision) The gaussian pseudorandom
c     ============  number.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  grandom
c
c     Declaration of internal variables and arrays.
c
      double precision  x, y, r, fac
      double precision  urandom
c
      logical           must_evaluate
      save              must_evaluate
      double precision  grprev
      save              grprev
c
      data              must_evaluate   / .true. /
c
c     FIRST EXECUTABLE STATEMENT
c
      if (must_evaluate) then
c
 1010   continue
        x = 2 * urandom() - 1
        y = 2 * urandom() - 1
        r = x * x + y * y
        if ((r .ge. 1) .or. (r .eq. 0)) goto 1010
        fac = sqrt(-2 * log(r) / r)
c
        grprev        = x * fac
        grandom       = y * fac
        must_evaluate = .false.
c
      else
c
        grandom       = grprev
        must_evaluate = .true.
c
      endif
c
      return
c
      end
c     --- End of routine grandom
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'random1.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
