c
c     FILE: xghfutils.f                     Creation date: 14/SEP/1999.
c                                       LAST MODIFICATION: 26/OCT/2004.
c
c     This file contains several routines related with calculations
c     using the Gaisser-Hillas function.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ghfpars(nmax, xmax, x0, lambda, vrb, irc)
c
c     Setting the internal quantities needed to work with the
c     Gaisser-Hillas function related routines.
c     The Gaisser-Hillas function for the profile of charged particles
c     (T. K. Gaisser and A. M. Hillas, in Proc. 15th ICRC (Plovdiv),
c     vol. 8, p. 353 (1977)) is given by:
c
c                      /    X - X0   \ [(Xmax-X0)/lambda]
c        Ngh(X) = Nmax | ----------- |                    *
c                      \  Xmax - X0  /
c
c                          /  Xmax - X  \
c                    * exp | ---------- |
c                          \   lambda   /
c
c     for X > X0 (Ngh(X) = 0 for X < X0).
c
c     Written by: S. J. Sciutto, La Plata 1999, 2000.
c
c
c     Arguments:
c     =========
c
c     nmax, xmax, x0,
c     lambda.......... (input, double precision) The four parameters of
c                      the Gaisser-Hillas function. Xmax, X0, and
c                      lambda are expressed in g/cm2.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  nmax, xmax, x0, lambda
      integer           vrb, irc
c
c     Declaration of shared data.
c
      include 'xghfcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the specified parameters.
c
      if ((xmax .le. x0) .or. (xmax .le. 0) .or.
     +    (lambda .le. 0) .or. (nmax .le. 0))    goto 3010
c
c     Assigning parameters and evaluating internal quantities.
c
      ghf_nmax    = nmax
      ghf_xmax    = xmax
      ghf_x0      = x0
      ghf_lambda  = lambda
c
      ghf_xmaxx0  = xmax - x0
      ghf_xmaxx0l = ghf_xmaxx0 / lambda
c
      ghf_x1i0    = (15 * ghf_x0 + ghf_xmax) / 16
      ghf_x1i1    = 2 * ghf_xmax
c
      irc = 0
      if (vrb .eq. 1) then
        call errprint(0, '$I01', 1, 'ghfpars',
     +                'Assignment of GH function parameters.',
     +                0, 0, 0, 0.d0, ' ')
      endif
      return
c
c     Error messages.
c
 3010 continue
      irc = 8
      if (vrb .gt. 0) then
        call errprint(0, '*', max(3, vrb), 'ghfpars',
     +                'Invalid parameter set for GH function.',
     +                0, 0, 0, 0.d0, ' ')
      endif
      return
c
      end
c     --- End of routine ghfpars
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function ghfx(x)
c
c     Evaluating the Gaisser-Hillas function for the profile of charged
c     particles (T. K. Gaisser and A. M. Hillas, in Proc. 15th ICRC
c     (Plovdiv), vol. 8, p. 353 (1977)).
c     The four parameters Nmax, Xmax, X0 and lambda are set by means of
c     the routine "ghfpars".
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     x............... (input, double precision) The atmospheric depth
c                      (g/cm2).
c
c
c     Return value: (double precision) The value of the function at
c     ============  specified x.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  ghfx
      double precision  x
c
c     Declaration of shared data.
c
      include 'xghfcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  fact1, fact2
c
c     FIRST EXECUTABLE STATEMENT
c
      if (x .gt. ghf_x0) then
c
        fact1  = ghf_xmaxx0l * log((x - ghf_x0) / ghf_xmaxx0)
        fact2  = (ghf_xmax - x) / ghf_lambda
c
        ghfx = ghf_nmax * exp(fact1 + fact2)
c
      else
        ghfx = 0
      endif
c
      return
      end
c     --- End of routine ghfx
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function ghfin(n, prepost)
c
c     Evaluating the inverse of the Gaisser-Hillas function (T. K.
c     Gaisser and A. M. Hillas, in Proc. 15th ICRC (Plovdiv), vol. 8,
c     p. 353 (1977))., that is, X such that ghf(X) = n.
c     The four parameters Nmax, Xmax, X0 and lambda are set by means of
c     the routine "ghfpars".
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
c     Arguments:
c     =========
c
c     n............... (input, double precision) The number of
c                      particles. If n < 0 or n > Nmax, the
c                      result is a large negative number.
c     prepost......... (input, integer) Integer parameter labelling
c                      which of the two abscissas X has to be
c                      returned: If prepost <= 0 then X < Xmax;
c                      otherwise x > Xmax.
c                      Notice that the inverse of the gh function
c                      is bi-valuated.
c
c
c     Return value: (double precision) The value of the inverse
c     ============  gh function (in g/cm2), that is, X such that
c                   ghf(X) = n.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  ghfin
      double precision  n
      integer           prepost
c
c     Declaration of shared data.
c
      include 'xghfcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  x1, x10, x1x0, xmx1, fc, foverfp
c
c     FIRST EXECUTABLE STATEMENT
c
c     The following "if branches" separate among different
c     cases.
c
      if ((n .gt. 0) .and. (n .lt. ghf_nmax)) then
c
c       The "normal" case will be solved by Newton-Raphson method.
c
c       Initial value (depends on the selected branch).
c
        if (prepost .le. 0) then
          x10 = ghf_x1i0
        else
          x10 = ghf_x1i1
        endif
        x1 = x10
        fc = ghf_lambda * log(ghf_nmax / n)
c
c       NR iterations.
c
 1010   continue
        if (x1 .le. ghf_x0) then
          x10 = (15 * ghf_x0 + x10) / 16
          x1  = x10
        endif
        x1x0    = x1 - ghf_x0
        xmx1    = ghf_xmax - x1
        foverfp = x1x0 *
     +       (1 + (fc + ghf_xmaxx0 * log(x1x0 / ghf_xmaxx0)) / xmx1)
        x1 = x1 - foverfp
        if (abs(foverfp) .ge. 1.d-12) goto 1010
c
        ghfin = x1
c
      else if (n .eq. 0) then
        if (prepost .le. 0) then
          ghfin = ghf_x0
        else
          ghfin = 5d35
        endif
      else if (n .eq. ghf_nmax) then
        ghfin = ghf_xmax
      else
        ghfin = -5.d35
      endif
c
      return
      end
c     --- End of routine ghfin
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function ghfcint(nmax, xmax, x0, lambda)
c
c     Evaluating the complete integral of the Gaisser-Hillas function,
c     i.e.,
c
c            infty
c        I =  INT   Ngh(X) dX
c             X0
c
c     where Ngh(X) is the Gaisser-Hillas function for the profile of
c     charged particles (T. K. Gaisser and A. M. Hillas, in Proc. 15th
c     ICRC (Plovdiv), vol. 8, p. 353 (1977)):
c
c                      /    X - X0   \ [(Xmax-X0)/lambda]
c        Ngh(X) = Nmax | ----------- |                    *
c                      \  Xmax - X0  /
c
c                          /  Xmax - X  \
c                    * exp | ---------- |
c                          \   lambda   /
c
c     for X > X0 (Ngh(X) = 0 for X < X0).
c
c     Written by: S. J. Sciutto, La Plata 2004.
c
c
c     Arguments:
c     =========
c
c     nmax, xmax, x0,
c     lambda.......... (input, double precision) The four parameters of
c                      the Gaisser-Hillas function. Xmax, X0, and
c                      lambda are expressed in g/cm2.
c                      Notice that nmax is used only as a
c                      multiplicative factor.
c
c
c     Return value: (double precision) The value of the integral, or
c     ============  zero in the case of invalid arguments.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  ghfcint
      double precision  nmax, xmax, x0, lambda
c
c     Declaration of internal variables and arrays.
c
      double precision  xmaxx0l
      double precision  Gamma
c
c     FIRST EXECUTABLE STATEMENT
c
      ghfcint = 0
c
c     Checking the specified parameters.
c
      if ((xmax .le. x0) .or. (xmax .le. 0) .or. (lambda .le. 0))
     +   return
c
c     Evaluating the integral in terms of a Gamma function.
c
      xmaxx0l = (xmax - x0) / lambda
      ghfcint = lambda * nmax *
     +          exp(xmaxx0l * (1 - log(xmaxx0l))) *
     +          Gamma(1 + xmaxx0l)
      return
c
      end
c     --- End of routine ghfcint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function ghfl1jcint(xmax, x0, lambda, jac)
c
c     Evaluating the complete integral of the Gaisser-Hillas function,
c     i.e.,
c
c            infty
c        I =  INT   Ngh(X) dX
c             X0
c
c     where Ngh(X) is a normalized Gaisser-Hillas function for the
c     profile of charged particles (T. K. Gaisser and A. M. Hillas,
c     in Proc. 15th ICRC (Plovdiv), vol. 8, p. 353 (1977)):
c
c                 /    X - X0   \ [(Xmax-X0)/lambda]
c        Ngh(X) = | ----------- |                    *
c                 \  Xmax - X0  /
c
c                       /  Xmax - X  \
c                 * exp | ---------- |
c                       \   lambda   /
c
c     for X > X0 (Ngh(X) = 0 for X < X0).
c
c     Written by: S. J. Sciutto, La Plata 2004.
c
c
c     Arguments:
c     =========
c
c     xmax, x0,
c     lambda.......... (input, double precision) The three parameters of
c                      the normalized Gaisser-Hillas function.
c                      Xmax, X0, and lambda are expressed in g/cm2.
c     jac............. (output, double precision, array(3)) The
c                      derivatives of the integral with respect to
c                      the three parameters of the normalized
c                      function.
c
c
c     Return value: (double precision) The value of the integral, or
c     ============  a large negative number in the case of invalid
c                   arguments.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  ghfl1jcint
      double precision  xmax, x0, lambda
      double precision  jac(3)
c
c     Declaration of internal variables and arrays.
c
      double precision  xmaxx0l, lxmaxx0l, xmaxx0l1, ddd
      double precision  Gamma, Psi
c
c     FIRST EXECUTABLE STATEMENT
c
      ghfl1jcint = -1.d33
      jac(1)     = 0
      jac(2)     = 0
      jac(3)     = 0
c
c     Checking the specified parameters.
c
      if ((xmax .le. x0) .or. (xmax .le. 0) .or. (lambda .le. 0))
     +   return
c
      xmaxx0l   = (xmax - x0) / lambda
      xmaxx0l1  = 1 + xmaxx0l
      lxmaxx0l  = log(xmaxx0l)
c
c     Evaluating the integral in terms of a Gamma function.
c
      ghfl1jcint = xmaxx0l * (1 - lxmaxx0l) +
     +             log(lambda * Gamma(xmaxx0l1))
c
c     Evaluating the drivatives with respect to the GH parameters.
c
      ddd = Psi(xmaxx0l1) - lxmaxx0l
      jac(1) = ddd / lambda
      jac(2) = - jac(1)
      jac(3) = (1 - xmaxx0l * ddd) / lambda
c
      return
c
      end
c     --- End of routine ghfl1jcint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'xghfutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
