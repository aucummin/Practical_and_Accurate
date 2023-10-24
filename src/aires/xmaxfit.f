c
c     FILE: xmaxfit.f                       Creation date: 13/JAN/1997.
c                                       LAST MODIFICATION: 15/JAN/2006.
c
c     Xmax and Nmax fitting routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine xmaxfit(singleav, x0, lambda, xmax, nmax, sqsum, irc)
c
c     Evaluating the shower maximum parameters (Xmax, Nmax) via a
c     fit of the available shower data to the Gaisser-Hillas function
c     for the profile of charged particles (T. K. Gaisser and
c     A. M. Hillas, in Proc. 15th ICRC (Plovdiv), vol. 8, p. 353
c     (1977)):
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
c      maximum, X0 is the depth where the GH function is zero and
c      lambda is an additional varying parameter.
c
c      Nch(X) is zero for X <= X0.
c
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998; Fermilab 1999;
c                                La Plata 1999, 2000, 2002, 2003, 2006.
c
c
c     Arguments:
c     =========
c
c     singleav........ (input, integer) 1 to fit using data from the
c                      average shower, and 2 to fit using data from
c                      the current shower.
c     x0.............. (output, double precision) Fitted position of
c                      the point where the GH function is zero.
c     lambda.......... (output, double precision) Fitted parameter
c                      lambda.
c     xmax............ (output, double precision) Fitted position of
c                      the shower maximum (g.cm2). If no fit was
c                      possible, then the value coming from a direct
c                      estimation from the MC data is returned.
c     nmax............ (output, double precision) Estimated number of
c                      charged particles at the shower maximum. If no
c                      fit was possible, then the value coming from a
c                      direct estimation from the MC data is returned.
c     sqsum........... (output, double precision) The resulting sum of
c                      weighted squares.
c     irc............. (output, integer) Return code. Zero means that
c                      the fit was successfully completed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           singleav, irc
      double precision  x0, lambda, xmax, nmax, sqsum
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, beginlevel0, beginleveleff, endleveleff
      double precision  swe
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating the number of data points available for the fit.
c     this depends on the first interaction depth.
c
      beginlevel0 = obslevca * fstintdp(singleav, 1) + obslevcb
      beginlevel0 = max(beginlevel0, 0)
      beginlevel0 = beginlevel0 + 1
c
      if ((beginlevel0 + 7) .ge. obslevlmax) then
        irc    = 110
        lambda = 0
        x0     = 0
        xmax   = 0
        nmax   = 0
        sqsum  = -1
        return
      endif
c
c     Copying the data to the internal arrays.
c
      do i = beginlevel0, obslevlmax
        obslevscra0(i, 1) = obslevdepth(i)
        obslevscra0(i, 2) = lhistn(singleav, i, lallcht)
      enddo
c
c     If singleav is not 2 (then it MUST be 1), then the weights must
c     be evaluated in this routine.
c
      if (singleav .ne. 2) then
        do i = beginlevel0, obslevlmax
          swe = lhistn(3, i, lallcht)
          if (swe .gt. 0) then
            obslevscra0(i, 3) = 1 / swe
          else
            obslevscra0(i, 3) = 0
          endif
        enddo
      endif
c
c     Calling the fitting routine.
c
      call fitghf(beginlevel0, obslevlmax,
     +            obslevscra0(1, 1),
     +            obslevscra0(1, 2),
     +            obslevscra0(1, 3),
     +            singleav, 100.d0, 100.d0,
     +            beginleveleff, endleveleff,
     +            nmax, xmax, x0, lambda, sqsum, irc)
c
      return
      end
c     --- End of routine xmaxfit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fitghf(bodata0, eodata0, depths, nallch, weights, ws,
     +                  minnmax, nminratio, bodataeff, eodataeff,
     +                  nmax, xmax, x0, lambda, sqsum, irc)
c
c     Evaluating the shower maximum parameters (Nmax, Xmax, X0, lambda)
C     via a four parameter fit of the available shower data to the
c     Gaisser-Hillas function for the profile of charged particles
c     (T. K. Gaisser and A. M. Hillas, in Proc. 15th ICRC (Plovdiv),
c     vol. 8, p. 353 (1977)):
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
c     Written by: S. J. Sciutto, La Plata 1997, 1998; Fermilab 1999;
c                                La Plata 1999, 2000; Fermilab 2003;
c                                La Plata 2004, 2006.
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
c                      If ws = 2 the array "weights" is not used.
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
c     Declaration of arguments.
c
      integer           bodata0, eodata0, ws
      double precision  depths(eodata0)
      double precision  nallch(eodata0)
      double precision  weights(eodata0)
      double precision  minnmax, nminratio
      integer           bodataeff, eodataeff
      double precision  nmax, xmax, x0, lambda, sqsum
      integer           irc
c
c     Declaration of shared data.
c
      include 'xfitcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, imax, ndataeff
      integer           beginlevel1, ix1, ix2
      double precision  nmin, nmin0, npart, sd2, snm, swt, swe
      double precision  nmax0, nmax1, lnmax
      double precision  xmax1, xmax2
      double precision  params(nxfitpar)
      integer           ncalls1, ncalls2
      double precision  diag(nxfitpar), qtf(nxfitpar)
      integer           ipvt(nxfitpar)
      external          xprofile
      double precision  lmddif(mxlongidata)
      double precision  lmdjac(mxlongidata, nxfitpar)
      double precision  wa1(nxfitpar), wa2(nxfitpar), wa3(nxfitpar)
      double precision  wa4(mxlongidata)
c
c     Tolerance factor for routine "lmder"
c
      double precision  lmtol
      parameter         (lmtol = 5.d-9)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the maximum number of charged particles in the
c     input data set.
c
      xmax = -1
      nmax = -1.d35
      imax = bodata0
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
c     Estimating "reasonable" bounds for nmax and xmax.
c
      call xmax12set(ndataeff, xfitnch, nmax, nmax0, ix1, ix2)
      xmax1 = xfitx(ix1)
      xmax2 = xfitx(ix2)
      if (ix2 .ge. ndataeff) then
        xmax2 = max(xmax2, xmax + 2 * (xmax - xmax1))
      endif
c
c     First guess for the function parameters.
c
      params(1) = xmax
      lnmax     = log(nmax)
      params(2) = lnmax
      params(3) = depths(bodata0)
      params(4) = 0.0143d0
c
c     Calling the routine "lmder" belonging to the minpack package
c     of the public domain library Netlib. This routine uses the
c     well known Levenberg-Marquardt algorithm.
c
      call lmder(xprofile, ndataeff, nxfitpar, params,
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
      if (params(2) .le. 0) goto 1100
      if (params(2) .ge. (lnmax + 5)) goto 1100
      if (params(3) .ge. params(1)) goto 1100
      if (params(4) .le. 0) goto 1100
c
      nmax1 = exp(params(2))
c
      irc = 1
c
      if (params(1) .lt. xmax1) goto 1090
      if (params(1) .gt. xmax2) goto 1090
c
      if (nmax1 .lt. (nmax - nmax0)) goto 1090
      if (nmax1 .gt. (nmax + nmax0)) goto 1090
c
c     All checks passed. The fit is OK.
c
      irc = 0
c
 1090 continue
c
c     Evaluating the actual parameters of Gaisser-Hillas function.
c
      xmax    = params(1)
      nmax    = nmax1
      x0      = params(3)
      lambda  = 1 / params(4)
c
c     Evaluating the normalized sum of squares.
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
c     --- End of routine fitghf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine xmax12set(ndata, nvsx, nmax, nmax0, ixmax1, ixmax2)
c
c     Determining the "reasonable" interval where the shower maximum
c     (xmax) should be located within.
c     This is an internal routine called from xmaxfit.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     ndata........... (input, integer) Number of available data
c                      points (N versus X). ndata must be positive.
c     nvsx............ (input, double precision, array(ndata)) Data
c                      array.
c     nmax............ (input, double precision) The maximum value the
c                      elements of array nvsx can take.
c     nmax0........... (output, double precision) Estimated maximum
c                      dispersion of the fitted value of nmax with
c                      respect to the given one (It is "reasonable"
c                      that the fitted value should belong to the
c                      interval [nmax-nmax0, nmax+nmax0]).
c     ixmax1, ixmax2.. (output, integer) Estimated range for the fitted
c                      value of xmax (It is "reasonable" that xmax
c                      should belong to the interval [xmax1, xmax2],
c                      where xmax1 (2) is the depth corresponding to
c                      nvsx(ixmax1) (nvsx(ixmax2))).
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ndata
      double precision  nvsx(ndata)
      double precision  nmax, nmax0
      integer           ixmax1, ixmax2
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating nmax0.
c
      nmax0 = 0.5d0 * nmax
c
c     Evaluating ixmax1 and ixmax2.
c
      ixmax1 = 1
      do i = 1, ndata
        if (nvsx(i) .ge. nmax0) goto 1010
        ixmax1 = i
      enddo
 1010 continue
c
      ixmax2 = ndata
      do i = ndata, ixmax1, -1
        if (nvsx(i) .ge. nmax0) goto 1020
        ixmax2 = i
      enddo
 1020 continue
c
      if (abs(ixmax2 - ixmax1) .le. 2) then
        ixmax1 = max(1, ixmax1 - 1)
        ixmax2 = min(ndata, ixmax2 + 1)
      endif
c
      return
      end
c     --- End of routine xmax12set
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine xprofile(ndata, nparam, params, diffs, diffjac,
     +                    ldjac, iflag)
c
c     Evaluating the differences (and the corresponding derivatives)
c     between the simulated number of charged particles and the
c     Gaisser-Hillas function for the profile of such particles (T. K.
c     Gaisser and A. M. Hillas, in Proc. 15th ICRC (Plovdiv), vol. 8,
c     p. 353 (1977)):
c
c                                    (Xmax - X1)
c                                    -----------
c                      /   X - X1   \  lambda         / Xmax - X \
c        Nch(X) = Nmax | ---------- |             exp | -------- |
c                      \  Xmax - X1 /                 \  lambda  /
c
c     Where:
c
c      Nch(X) is the number of charged particles at depth X (in g/cm2).
c      Nmax is the maximum number of particles, Xmax is the depth of the
c      maximum, X1 is the depth of the (effective) first interaction and
c      lambda is an additional varying parameter.
c
c      Nch(X) is zero for X <= X1.
c
c     The format of the arguments is set to match the requirements of
c     routine lmder of Netlib minpack package. The arguments actually
c     used are Xmax, log(Nmax) and X1
c
c     Written by: S. J. Sciutto, La Plata 1997, 1999.
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
c
c     Declaration of internal variables and arrays.
c
      integer           j
      double precision  x1, xmax, lnmax
      double precision  ulambda
      double precision  umax1
      double precision  xmax1, xi, xmaxi, xix1, a1, b1, fun
c
c     FIRST EXECUTABLE STATEMENT
c
      xmax    = params(1)
      lnmax   = params(2)
      x1      = params(3)
      ulambda = params(4)
c
c     Normally Xmax > X1, if Xmax is set to some value less than X1
c     we set this variable near X1. The returned differences will
c     be surely big, and so will not determine the minimizing point.
c
      xmax1 = max(1.d-35, xmax - x1)
      umax1 = ulambda * xmax1
c
      if (iflag .eq. 1) then
c
c       Evaluating the differences Nch(X) - Nch(calculated).
c
        do j = 1, ndata
c
          xi = xfitx(j)
c
          if (xi .gt. x1) then
            xmaxi = xmax - xi
            xix1  = max(1.d-35, xi - x1)
            a1    = xix1 / xmax1
            b1    = log(a1)
            fun   = exp(lnmax + umax1 * b1 + ulambda * xmaxi)
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
          if (xi .gt. x1) then
            xmaxi = xmax - xi
            xix1  = max(1.d-35, xi - x1)
            a1    = xix1 / xmax1
            b1    = log(a1)
            fun   = xfitwt(j) *
     +              exp(lnmax + umax1 * b1 + ulambda * xmaxi)
c
            diffjac(j, 1) = fun * ulambda * b1
            diffjac(j, 2) = fun
            diffjac(j, 3) = fun * ulambda *
     +                            (1 - b1 - xmax1 / xix1)
            diffjac(j, 4) = fun * (xmax1 * b1 + xmaxi)
          else
            diffjac(j, 1) = 0
            diffjac(j, 2) = 0
            diffjac(j, 3) = 0
            diffjac(j, 4) = 0
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
