c
c     FILE: mathutils0.f                    Creation date: 15/AUG/1996.
c                                       LAST MODIFICATION: 21/AUG/2003.
c
c     This file contains several routines to perform mathematical
c     calculations.
c     Part 0: Basic, purely mathematical or geometrical routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine deflectr(uinitial, cdtheta, sdtheta,
     +                    cdphi, sdphi, ufinal)
c
c     Deflecting a unitary vector uinitial in angles delta-theta (given)
c     and delta-phi (chosen at random), to give a new unitary vector
c     ufinal. The values of sin(delta-theta) and sin/cos(delta-phi) are
c     returned for eventual future use.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Parameters:
c     ==========
c
c     uinitial........ (input, double precision, array(3)) Input
c                      vector, assumed as unitary.
c     cdtheta......... (input, double precision) Cos(dtheta).
c     sdtheta......... (output, double precision) Sin(dtheta).
c     cdphi........... (output, double precision) Cos(dphi).
c     sdphi........... (output, double precision) Sin(dphi).
c     ufinal.......... (output, double precision, array(3)) Unitary
c                      deflected vector.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  uinitial(3), ufinal(3)
      double precision  cdtheta, sdtheta
      double precision  cdphi, sdphi
c
c     Declaration of internal variables and arrays.
c
      double precision  trigmax
      parameter         (trigmax = 1.d0 - 5.d-16)
      double precision  f1, f2, f0, fs
      integer           i
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      if (cdtheta .le. trigmax) then
c
        f0      = twopi * urandom()
        cdphi   = cos(f0)
        sdphi   = sin(f0)
        sdtheta = sqrt(1 - cdtheta ** 2)
c
c       Determining if uinitial points in the +/- z direction.
c
        if (abs(uinitial(3)) .lt. 0.999999999999d0) then
c
          f0 = sqrt(1.d0 - uinitial(3) ** 2)
          fs = sdtheta / f0
          f1 = fs * cdphi
          f2 = fs * sdphi * uinitial(3)
c
          ufinal(1) = cdtheta * uinitial(1) + f1 * uinitial(2)
     +                                      + f2 * uinitial(1)
          ufinal(2) = cdtheta * uinitial(2) - f1 * uinitial(1)
     +                                      + f2 * uinitial(2)
          ufinal(3) = cdtheta * uinitial(3) - f0 * sdtheta * sdphi
c
        else
c
          ufinal(1) = sdtheta * cdphi
          ufinal(2) = sdtheta * sdphi * uinitial(3)
          ufinal(3) = cdtheta * uinitial(3)
c
        endif
c
c       Renormalizing to avoid propagation of numerical errors.
c
        f2 = 1.d0 / sqrt(ufinal(1) ** 2 + ufinal(2) ** 2 +
     +                   ufinal(3) ** 2)
        do i = 1, 3
          ufinal(i) = ufinal(i) * f2
        enddo
c
      else
c
c       Deflecting angle is zero.
c
        sdtheta   = 0
        cdphi     = 1
        sdphi     = 0
c
        ufinal(1) = uinitial(1)
        ufinal(2) = uinitial(2)
        ufinal(3) = uinitial(3)
c
      endif
c
      return
      end
c     --- End of routine deflectr
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine mdeflectr(uinitial, n, cdtheta, ldf, ufinal)
c
c     Deflecting a unitary vector uinitial in angles delta-theta (given)
c     and delta-phi (chosen at random), to give new unitary vectors
c     ufinal.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Parameters:
c     ==========
c
c     uinitial........ (input, double precision, array(3)) Input
c                      vector, assumed as unitary.
c     n............... (input, integer) The number of vectors to
c                      deflect.
c     cdtheta......... (input, double precision, array(n)) Cos(dtheta).
c     ldf............. (input, integer) Leading dimension of array
c                      ufinal. Must be greater or equal than 3.
c     ufinal.......... (output, double precision, array(ldf, n))
c                      Unitary deflected vectors.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  uinitial(3)
      integer           n, ldf
      double precision  cdtheta(n)
      double precision  ufinal(ldf, n)
c
c     Declaration of internal variables and arrays.
c
      double precision  sdtheta
      double precision  cdphi, sdphi
      double precision  f1, f2, f3, f0, fs
      integer           i, j
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Determining if uinitial points in the +/- z direction.
c
      if (abs(uinitial(3)) .lt. 0.999999999999d0) then
c
        f3 = sqrt(1.d0 - uinitial(3) ** 2)
c
        do i = 1, n
c
          f0      = twopi * urandom()
          cdphi   = cos(f0)
          sdphi   = sin(f0)
          sdtheta = sqrt(abs(1 - cdtheta(i) ** 2))
c
          fs = sdtheta / f3
          f1 = fs * cdphi
          f2 = fs * sdphi * uinitial(3)
c
          ufinal(1, i) = cdtheta(i) * uinitial(1) +
     +                   f1 * uinitial(2) + f2 * uinitial(1)
          ufinal(2, i) = cdtheta(i) * uinitial(2) -
     +                   f1 * uinitial(1) + f2 * uinitial(2)
          ufinal(3, i) = cdtheta(i) * uinitial(3) -
     +                   f3 * sdtheta * sdphi
c
        enddo
c
      else
c
        do i = 1, n
c
          f0      = twopi * urandom()
          cdphi   = cos(f0)
          sdphi   = sin(f0)
          sdtheta = sqrt(abs(1 - cdtheta(i) ** 2))
c
          ufinal(1, i) = sdtheta * cdphi
          ufinal(2, i) = sdtheta * sdphi * uinitial(3)
          ufinal(3, i) = cdtheta(i) * uinitial(3)
c
        enddo
c
      endif
c
c     Renormalizing to avoid propagation of numerical errors.
c
      do i = 1, n
        f2 = 1.d0 / sqrt(ufinal(1, i) ** 2 + ufinal(2, i) ** 2 +
     +                   ufinal(3, i) ** 2)
        do j = 1, 3
          ufinal(j, i) = ufinal(j, i) * f2
        enddo
      enddo
c
      return
      end
c     --- End of routine mdeflectr
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pdeflectr(uinitial, cdtheta, ldf, ufinal)
c
c     Deflecting a unitary vector uinitial in angles delta-theta (given)
c     and delta-phi (chosen at random), to give two unitary vectors
c     ufinal. The two deflected vectors are in the same plane.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Parameters:
c     ==========
c
c     uinitial........ (input, double precision, array(3)) Input
c                      vector, assumed as unitary.
c     cdtheta......... (input, double precision, array(2)) Cos(dtheta).
c     ldf............. (input, integer) Leading dimension of array
c                      ufinal. Must be greater or equal than 3.
c     ufinal.......... (output, double precision, array(ldf, 2))
c                      Unitary deflected vectors.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  uinitial(3)
      integer           ldf
      double precision  cdtheta(2)
      double precision  ufinal(ldf, 2)
c
c     Declaration of internal variables and arrays.
c
      double precision  sdtheta
      double precision  cdphi(2), sdphi(2)
      double precision  f1, f2, f0, fs
      integer           i, j
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating cos(phi) and sin(phi).
c
      f0       = twopi * urandom()
      cdphi(1) = cos(f0)
      sdphi(1) = sin(f0)
      cdphi(2) = - cdphi(1)
      sdphi(2) = - sdphi(1)
c
c     Determining if uinitial points in the +/- z direction.
c
      if (abs(uinitial(3)) .lt. 0.999999999999d0) then
c
        f0 = sqrt(1.d0 - uinitial(3) ** 2)
c
        do i = 1, 2
c
          sdtheta = sqrt(abs(1 - cdtheta(i) ** 2))
c
          fs = sdtheta / f0
          f1 = fs * cdphi(i)
          f2 = fs * sdphi(i) * uinitial(3)
c
          ufinal(1, i) = cdtheta(i) * uinitial(1) +
     +                   f1 * uinitial(2) + f2 * uinitial(1)
          ufinal(2, i) = cdtheta(i) * uinitial(2) -
     +                   f1 * uinitial(1) + f2 * uinitial(2)
          ufinal(3, i) = cdtheta(i) * uinitial(3) -
     +                   f0 * sdtheta * sdphi(i)
c
        enddo
c
      else
c
        do i = 1, 2
c
          sdtheta = sqrt(abs(1 - cdtheta(i) ** 2))
c
          ufinal(1, i) = sdtheta * cdphi(i)
          ufinal(2, i) = sdtheta * sdphi(i) * uinitial(3)
          ufinal(3, i) = cdtheta(i) * uinitial(3)
c
        enddo
c
      endif
c
c     Renormalizing to avoid propagation of numerical errors.
c
      do i = 1, 2
        f2 = 1.d0 / sqrt(ufinal(1, i) ** 2 + ufinal(2, i) ** 2 +
     +                   ufinal(3, i) ** 2)
        do j = 1, 3
          ufinal(j, i) = ufinal(j, i) * f2
        enddo
      enddo
c
      return
      end
c     --- End of routine pdeflectr
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine mrotate(uinitial, n, ldf, uprime)
c
c     Expressing the coordinates of vectors "uprime" (in the system
c     where "uinitial" is the z axis, in the lab system.
c
c     Written by: S. J. Sciutto, La Plata 1997; Fermilab 2003.
c
c
c     Parameters:
c     ==========
c
c     uinitial........ (input, double precision, array(3)) Input
c                      vector, assumed as unitary. This vector defines
c                      the "local" z axis.
c     n............... (input, integer) The number of vectors to
c                      transform.
c     ldf............. (input, integer) Leading dimension of array
c                      uprime. Must be greater or equal than 3.
c     uprime.......... (input-output, double precision, array(ldf, n))
c                      Unitary vectors. The array contains, as output,
c                      the transformed unitary vectors.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  uinitial(3)
      integer           n, ldf
      double precision  uprime(ldf, n)
c
c     Declaration of internal variables and arrays.
c
      double precision  cdtheta, sdtheta
      double precision  cdphi, sdphi
      double precision  f1, f2, f3, fs
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
c     Determining if uinitial points in the +/- z direction.
c
      if (abs(uinitial(3)) .lt. 0.999999999999d0) then
c
        f3 = sqrt(1.d0 - uinitial(3) ** 2)
c
        do i = 1, n
c
          cdtheta = uprime(3, i)
c
c         The vector will be changed only when necessary.
c
          if (abs(cdtheta) .lt. 0.999999999999d0) then
c
            sdtheta = sqrt(abs(1 - cdtheta ** 2))
            cdphi   = uprime(1, i) / sdtheta
            sdphi   = uprime(2, i) / sdtheta
c
            fs = sdtheta / f3
            f1 = fs * cdphi
            f2 = fs * sdphi * uinitial(3)
c
            uprime(1, i) = cdtheta * uinitial(1) +
     +                     f1 * uinitial(2) + f2 * uinitial(1)
            uprime(2, i) = cdtheta * uinitial(2) -
     +                     f1 * uinitial(1) + f2 * uinitial(2)
            uprime(3, i) = cdtheta * uinitial(3) -
     +                     f3 * sdtheta * sdphi
c
          else
c
            uprime(1, i) = uinitial(1) * cdtheta
            uprime(2, i) = uinitial(2) * cdtheta
            uprime(3, i) = uinitial(3) * cdtheta
c
          endif
        enddo
c
      else
c
        do i = 1, n
c
c         The x component remains unchanged.
c
          uprime(2, i) = uprime(2, i) * uinitial(3)
          uprime(3, i) = uprime(3, i) * uinitial(3)
c
        enddo
c
      endif
c
c     Renormalizing to avoid propagation of numerical errors.
c
      do i = 1, n
        f2 = 1.d0 / sqrt(uprime(1, i) ** 2 + uprime(2, i) ** 2 +
     +                   uprime(3, i) ** 2)
        do j = 1, 3
          uprime(j, i) = uprime(j, i) * f2
        enddo
      enddo
c
      return
      end
c     --- End of routine mrotate
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'mathutils0.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
