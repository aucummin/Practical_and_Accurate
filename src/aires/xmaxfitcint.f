c
c     FILE: xmaxfitcint.f                   Creation date: 25/OCT/2004.
c                                       LAST MODIFICATION: 23/MAR/2005.
c
c     Constrainted Gaisser-Hillas function fit.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fitghfcint(bodata0, eodata0, depths, nallch, weights,
     +                  ws, minnmax, nminratio, profint,
     +                  bodataeff, eodataeff,
     +                  nmax, xmax, x0, lambda, sqsum, irc)
c
c     Evaluating the shower maximum parameters (Nmax, Xmax, X0, lambda)
c     via a constrainted four parameter fit of the available shower
c     data to the Gaisser-Hillas function for the profile of charged
c     particles (T. K. Gaisser and A. M. Hillas, in Proc. 15th ICRC
c     (Plovdiv), vol. 8, p. 353 (1977)):
c
c                                   (Xmax - X0)
c                                   -----------
c                      /  X - X0   \  lambda         / Xmax - X \
c        Nch(X) = Nmax | ----------|             exp | -------- |
c                      \ Xmax - X0 /                 \  lambda  /
c
c     Where:
c
c      Nch(X) is the number of charged particles at depth X (in g/cm2).
c      Nmax is the maximum number of particles, Xmax is the depth of the
c      maximum, X0 and lambda are additional varying parameters.
c
c      Nch(X) is zero for X <= X0.
c
c     The constrainted fit is performed imposing that the integral of
c     Nch(X) with respect to X, in the range X0 to infinity is equal
c     to the input parameter "profint" which must be positive.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998; Fermilab 1999;
c                                La Plata 1999, 2000; Fermilab 2003,
c                                La Plata 2004.
c
c
c     Arguments:
c     =========
c
c     bodata0, eodata0 (input, integer) Positive integer parameters
c                      defining the number of data points to use
c                      in the fit.
c     depths.......... (input, double precision, array(eodata0)) Depths
c                      of the observing levels used in the fit. Only
c                      the range (bodata0:eodata0) is used.
c     nallch.......... (input, double precision, array(eodata0)) Number
c                      of charged particles crossing the different
c                      levels. Only the range (bodata0:eodata0) is
c                      used.
c     weights......... (input, double precision, array(eodata0))
c                      Positive weights to be assigned to each one of
c                      the data points. Only the range
c                      (bodata0:eodata0) is used.
c     ws.............. (input, integer) If ws = 2, the weights are
c                      evaluated internally (proportionally to the
c                      square root of the number of particles).
c                      If ws = 1 they must be provided as input data.
c     minnmax......... (input, double precision) Threshold value for
c                      the maximum number of particles in the input
c                      data set. The fit is not performed if the
c                      maximum number of particles is below this
c                      parameter. If minnmax is negative, it is
c                      taken as zero.
c     nminratio....... (input, double precision) Positive parameter
c                      used to determine the end of the data set.
c                      Must be equal or greater than 5. Once the
c                      maximum of the data set is found. the points
c                      located after this maximum up to the point
c                      where the number of charged particles is less
c                      than the maximum divided nminratio. The
c                      remaining part of the data is not taken into
c                      account in the fit. A similar analysis is
c                      performed with the points located before the
c                      maximum. The recommended value is 100. A very
c                      large value will enforce inclusion of all the
c                      data set.
c     profint......... (input, double precision) Positive number
c                      that gives as input the integral
c
c                                infty
c                                 INT   Nch(X) dX
c                                  X0
c                      whose value is kept fixed during the
c                      constrainted fit.
c     bodataeff,
c     eodataeff....... (output, integer) The actual range of data
c                      points used in the fit.
c     nmax............ (output, double precision) Estimated number of
c                      charged particles at the shower maximum. If no
c                      fit was possible, then the value coming from a
c                      direct estimation from the MC data is returned.
c     xmax............ (output, double precision) Fitted position of
c                      the shower maximum (g/cm2). If no fit was
c                      possible, then the value coming from a direct
c                      estimation from the MC data is returned.
c     x0.............. (output, double precision) Fitted position of
c                      the point where the GH function is zero.
c     lambda.......... (output, double precision) Fitted parameter
c                      lambda.
c     sqsum........... (output, double precision) The resulting
c                      normalized sum of squares:
c
c                                                              2
c                                  1       N   [Nch(i) - GH(i)]
c                      sqsum = ---------- SUM -------------------
c                               N * Nmax  i=1        GH(i)
c
c                      (N is the number of points used in the fit).
c     irc............. (output, integer) Return code. Zero means that
c                      the fit was successfully completed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'xfitpar.f'
c
c     Tolerance factor for routine "lmder"
c
      double precision  lmtol
      parameter         (lmtol = 5.d-9)
c
c     Declaration of arguments.
c
      integer           bodata0, eodata0, ws
      double precision  depths(eodata0)
      double precision  nallch(eodata0)
      double precision  weights(eodata0)
      double precision  minnmax, nminratio, profint
      integer           bodataeff, eodataeff
      double precision  nmax, xmax, x0, lambda, sqsum
      integer           irc
c
c     Declaration of shared data.
c
      include 'xfitcomm.f'
      include 'xfitcintcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, imax, ndataeff
      integer           beginlevel1
      double precision  nmin, nmin0, npart, sd2, snm, swt, swe
      double precision  lnmax
      double precision  params(nxfitpar)
      integer           ncalls1, ncalls2
      double precision  diag(nxfitpar), qtf(nxfitpar)
      integer           ipvt(nxfitpar)
      external          xprofilecint
      double precision  lmddif(mxlongidata)
      double precision  lmdjac(mxlongidata, nxfitpar)
      double precision  wa1(nxfitpar), wa2(nxfitpar), wa3(nxfitpar)
      double precision  wa4(mxlongidata)
      double precision  ghfl1jcint
c
c     FIRST EXECUTABLE STATEMENT
c
      xmax = -1
      nmax = -1.d35
c
c     Basic check of arguments.
c
      irc = 330
      if (profint .le. 0) goto 1100
c
      lprofint = log(profint)
c
c     Searching the maximum number of charged particles in the
c     input data set.
c
      do i = bodata0 + 1, eodata0
        npart = nallch(i)
        if (npart .gt. nmax) then
          imax = i
          nmax = npart
        endif
      enddo
c
      irc = 108
      if (nmax .le. max(minnmax, 0.d0)) goto 1100
c
      xmax  = depths(imax)
      nmin  = max(20.d0, nmax / max(5.d0, nminratio))
      nmin0 = nmin / 8
c
c     Determining the significative fraction of the data set.
c
      bodataeff = imax
      do i = imax, bodata0 + 1, -1
        if (nallch(i) .lt. nmin0) goto 1010
        bodataeff = i
      enddo
 1010 continue
c
      eodataeff = imax
      do i = imax, eodata0
        if (nallch(i) .lt. nmin) goto 1020
        eodataeff = i
      enddo
 1020 continue
c
      beginlevel1 = bodataeff - 1
      ndataeff    = eodataeff - beginlevel1
      nxfitdata   = ndataeff
c
c     Checking that the number of data points is not too low.
c
      irc = 107
      if (ndataeff .lt. 8) goto 1100
c
c     Evaluating the data set to be used in the fitting and estimating
c     the position of the maximum.
c
      swt   = 0
      snm   = sqrt(nmax)
      sd2   = 1.d-4 / snm
c
      if (ws .eq. 2) then
c
c       Weights evaluated internally.
c
        do i = 1, ndataeff
          xfitx(i)   = depths(i + beginlevel1)
          npart      = nallch(i + beginlevel1)
          xfitwt(i)  = snm + sqrt(npart)
          xfitnch(i) = npart
          swt        = swt + xfitwt(i)
        enddo
c
      else
c
c       Weights provided externally.
c
        do i = 1, ndataeff
c
          xfitx(i) = depths(i + beginlevel1)
          npart    = nallch(i + beginlevel1)
          swe      = weights(i + beginlevel1)
c
          if (swe .gt. 0) then
            xfitwt(i) = swe
          else
            xfitwt(i) = sd2
          endif
c
          xfitnch(i) = npart
          swt        = swt + xfitwt(i)
c
        enddo
c
      endif
c
      if (swt .le. 0) goto 1100
      swt = 1.d0 / sqrt(swt)
      do i = 1, ndataeff
        xfitwt(i) = swt * xfitwt(i)
      enddo
c
c     First guess for the function parameters.
c
      params(1) = xmax
      params(2) = depths(bodata0)
      params(3) = 0.0143d0
c
c     Calling the routine "lmder" belonging to the minpack package
c     of the public domain library Netlib. This routine uses the
c     well known Levenberg-Marquardt algorithm.
c
      call lmder(xprofilecint, ndataeff, nxfitpar - 1, params,
     +           lmddif, lmdjac, mxlongidata,
     +           lmtol, lmtol, 0.d0, 10000,
     +           diag, 1, 100.d0, -1, irc, ncalls1, ncalls2,
     +           ipvt, qtf, wa1, wa2, wa3, wa4)
c
      if (irc .ge. 4) goto 1100
c
c     Convergence from "lmder" was obtained.
c     Checking the parameters obtained.
c
      irc = 3
c
      if (params(1) .le. 0) goto 1100
      if (params(2) .ge. params(1)) goto 1100
      if (params(3) .le. 0) goto 1100
c
c     All checks passed. The fit is OK.
c
      irc = 0
c
c     Evaluating the actual parameters of Gaisser-Hillas function.
c
      xmax    = params(1)
      x0      = params(2)
      lambda  = 1 / params(3)
      lnmax   = lprofint - ghfl1jcint(xmax, x0, lambda, wa1)
      nmax    = exp(lnmax)
c
c     Evaluating the normalized sum of squares.
c
      params(4) = params(3)
      params(3) = params(2)
      params(2) = lnmax
c
      do i = 1, ndataeff
        xfitwt(i) = 1
      enddo
c
      call xprofile(ndataeff, nxfitpar, params, lmddif,
     +              lmdjac, mxlongidata, 1)
c
      sd2 = 0
      do i = 1, ndataeff
        sd2 = sd2 + lmddif(i) ** 2 / xfitnch(i)
      enddo
      sqsum = sd2 / (ndataeff * nmax)
c
      return
c
 1100 continue
c
c     Number of data points too low, fitting routine did not converge
c     or the data obtained is meaningless.
c
      x0     = 0
      lambda = 77.77d0
      xmax   = max(xmax, 0.d0)
      nmax   = max(nmax, 0.d0)
      sqsum  = -1
      return
c
      end
c     --- End of routine fitghfcint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine xprofilecint(ndata, nparam, params, diffs, diffjac,
     +                        ldjac, iflag)
c
c     Evaluating the differences (and the corresponding derivatives)
c     between the simulated number of charged particles and the
c     Gaisser-Hillas function for the profile of such particles (T. K.
c     Gaisser and A. M. Hillas, in Proc. 15th ICRC (Plovdiv), vol. 8,
c     p. 353 (1977)):
c
c                                    (Xmax - X0)
c                                    -----------
c                      /   X - X0   \  lambda         / Xmax - X \
c        Nch(X) = Nmax | ---------- |             exp | -------- |
c                      \  Xmax - X0 /                 \  lambda  /
c
c     Where:
c
c      Nch(X) is the number of charged particles at depth X (in g/cm2).
c      Nmax is the maximum number of particles, Xmax is the depth of the
c      maximum, X0 is the depth of the (effective) first interaction and
c      lambda is an additional varying parameter.
c
c      Nch(X) is zero for X <= X0.
c
c     The format of the arguments is set to match the requirements of
c     routine lmder of Netlib minpack package. The arguments actually
c     used are Xmax, X0, and lambda. Nmax is a derived quantity
c     evaluated from the constraint of fixed integral
c
c     Written by: S. J. Sciutto, La Plata 1997, 1999, 2004.
c
c
c     Arguments:
c     =========
c
c     ndata........... (input, integer) The number of data points,
c                      normally the number of observing levels which
c                      are below the first interaction depth plus 1.
c     nparam.......... (input, integer) The number of parameters.
c                      Should be (nxfitpar) 4 (xmax, log(nmax), x1 and
c                      1/lambda).
c     params.......... (input, double precision, array(nparam)) The
c                      values of the function parameters.
c     diffs........... (output, double precision, array(ndata)) The
c                      differences between the estimations and the
c                      corresponding Monte Carlo data.
c     diffjac......... (output, double precision, array(ldjac, nparam))
c                      The jacobian matrix of the difference functions
c                      (ndata X nparam).
c     ldjac........... (input, integer) Leading dimension of array
c                      diffjac.
c     iflag........... (input, integer) Calculations switch. If iflag
c                      is 1, calculate only "diffs" and leave "diffjac"
c                      unchanged. If iflag = 2, calculate only
c                      "diffjac" and leave "diffs" unchanged.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'xfitpar.f'
c
c     Declaration of arguments.
c
      integer           ndata, nparam, ldjac, iflag
      double precision  params(nparam)
      double precision  diffs(ndata)
      double precision  diffjac(ldjac, nparam)
c
c     Declaration of shared data.
c
      include 'xfitcomm.f'
      include 'xfitcintcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           j
      double precision  x0, xmax, lnmax
      double precision  ulambda, lambda
      double precision  umax0
      double precision  xmax0, xi, xmaxi, xix0, a1, b1, fun
      double precision  lijac(3)
      double precision  ghfl1jcint
c
c     FIRST EXECUTABLE STATEMENT
c
      xmax    = params(1)
      x0      = params(2)
      ulambda = params(3)
      lambda  = 1 / ulambda
      lnmax   = lprofint - ghfl1jcint(xmax, x0, lambda, lijac)
c
c     Normally Xmax > X0, if Xmax is set to some value less than X0
c     we set this variable near X0. The returned differences will
c     be surely big, and so will not determine the minimizing point.
c
      xmax0 = max(1.d-35, xmax - x0)
      umax0 = ulambda * xmax0
c
      if (iflag .eq. 1) then
c
c       Evaluating the differences Nch(X) - Nch(calculated).
c
        do j = 1, ndata
c
          xi = xfitx(j)
c
          if (xi .gt. x0) then
            xmaxi = xmax - xi
            xix0  = max(1.d-35, xi - x0)
            a1    = xix0 / xmax0
            b1    = log(a1)
            fun   = exp(lnmax + umax0 * b1 + ulambda * xmaxi)
          else
            fun   = 0
          endif
c
          diffs(j) = xfitwt(j) * (fun - xfitnch(j))
c
        enddo
c
      else
c
c       Evaluating the derivatives of the differences.
c
        do j = 1, ndata
c
          xi = xfitx(j)
c
          if (xi .gt. x0) then
            xmaxi = xmax - xi
            xix0  = max(1.d-35, xi - x0)
            a1    = xix0 / xmax0
            b1    = log(a1)
            fun   = xfitwt(j) *
     +              exp(lnmax + umax0 * b1 + ulambda * xmaxi)
c
            diffjac(j, 1) = fun * (ulambda * b1 - lijac(1))
            diffjac(j, 2) = fun * 
     +                       (ulambda * (1 - b1 - xmax0 / xix0)
     +                        - lijac(2))
            diffjac(j, 3) = fun * (xmax0 * b1 + xmaxi
     +                             + lambda * lambda * lijac(3))
          else
            diffjac(j, 1) = 0
            diffjac(j, 2) = 0
            diffjac(j, 3) = 0
          endif
c
        enddo
c
      endif
c
      return
c
      end
c     --- End of routine xprofile
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'xmaxfit.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
