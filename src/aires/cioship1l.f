c
c     FILE: cioship1l.f                     Creation date: 07/JUL/1997.
c                                       LAST MODIFICATION: 06/MAR/2002.
c
c     This file contains the routines that process the records to be
c     written in the "grdpcles" compressed output file.
c
c     Long record version.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pcleship1
c
c     Processing a full ground level particles buffer.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999; La Plata 1999, 2002.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
c     include 'pstackpar.f'      (Included by 'pbuffpar.f')
c     include 'ciopar.f'         (Included by 'pbuffpar.f')
      include 'pbuffpar.f'
      include 'pclepar.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'showercomm.f'
      include 'pgndbuffcomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, stack, icode, cofile
      integer           ihi, ihj, histoi, histot
      double precision  wt1, wte, tmp1, tmp2, tmp3, tmp4
      double precision  r2, rlog, xgi, ygi, tgi, elog
      double precision  r2minfileg, r2maxfileg
      double precision  r2min0000, pr00
      logical           saveinfile
      integer           ipdata(14)
      double precision  rpdata(12)
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      cofile     = gpcleciofile
      r2minfileg = r2minfile(cofile)
      r2maxfileg = r2maxfile(cofile)
      r2min0000  = prssqratio * r2minfileg
c
c     Loop over the buffer records.
c
      do i = 1, npgndbuff
c
        stack = gndbuffsta(i)
        icode = igndbuff(ixpcode, i)
c
        wt1   = fgndbuff(ixwt, i)
        wte   = wt1 * fgndbuff(ixenergy, i)
c
c       Completing longitudinal information.
c
        histoi = allpclelh(icode)
        wlhistn(2, nobslevelsp1, histoi) =
     +                      wlhistn(2, nobslevelsp1, histoi) + 1
        lhistn(2, nobslevelsp1, histoi)  =
     +                      lhistn(2, nobslevelsp1, histoi)  + wt1
        lhiste(2, nobslevelsp1, histoi)  =
     +                      lhiste(2, nobslevelsp1, histoi)  + wte
c
c       "Per stack" ground particle counting.
c
        nprgnd(2, stack) = nprgnd(2, stack) + wt1
        eprgnd(2, stack) = eprgnd(2, stack) + wte
c
c       Evaluating ground intersection point (coordinates and time).
c       It is assumed that the current altitude of the particle
c       (fpdata(ixz)) is less than or equal to the ground altitude
c       (groundz). Notice that tmp1 is negative for particles going
c       downwards.
c       Spherical calculations are used when the particle is too much
c       far away the origin.
c
        if (fgndbuff(ixz, i) .eq. fgndbuff(ixvz, i)) then
          tmp1 = (groundz - fgndbuff(ixz, i)) / fgndbuff(ixpzpr, i)
        else
          tmp2 = fgndbuff(ixx, i) * fgndbuff(ixpxpr, i) +
     +           fgndbuff(ixy, i) * fgndbuff(ixpypr, i) +
     +           (rearth + fgndbuff(ixz, i)) * fgndbuff(ixpzpr, i)
          tmp3 = groundz00sq - (rearth + fgndbuff(ixvz, i)) ** 2
          tmp4 = sqrt(tmp2 ** 2 + tmp3)
          if (tmp2 .lt. 0) then
            tmp1 = - tmp4 - tmp2
          else
            tmp1 = tmp4 - tmp2
          endif
        endif
c
        xgi  = fgndbuff(ixx, i) + fgndbuff(ixpxpr, i) * tmp1
        ygi  = fgndbuff(ixy, i) + fgndbuff(ixpypr, i) * tmp1
        tgi  = fgndbuff(ixtime, i) + tmp1 / fgndbuff(ixbeta, i)
c
c       Evaluating position in polar coordinates (log r).
c
        r2   = xgi * xgi + ygi * ygi
        rlog = log(max(1.d-35, r2))
c
c       Logarithm of the energy.
c
        elog = log(fgndbuff(ixenergy, i))
c
c       Lateral distribution histograms.
c
        histot = allpcleth(icode)
c
        ihi = factrthn * (rlog + rhn0)
        if (ihi .lt. 0) then
          ihi = 0
        else if (ihi .gt. tdbinsp1) then
          ihi = tdbinsp1
        endif
        wrthistn(2, ihi, histot) = wrthistn(2, ihi, histot) + 1
        rthistn(2, ihi, histot)  = rthistn(2, ihi, histot)  + wt1
c
        ihj = factrthe * (elog + rhe0)
        if (ihj .lt. 0) then
          ihj = 0
        else if (ihj .gt. tdbinsp1) then
          ihj = tdbinsp1
        endif
        wrthiste(2, ihj, histot) = wrthiste(2, ihj, histot) + 1
        rthiste(2, ihj, histot)  = rthiste(2, ihj, histot)  + wt1
c
c       Time distribution evaluation.
c
        histot = allpcleuh(icode)
c
        rthistt(2, ihi, histot) = rthistt(2, ihi, histot) + wt1 * tgi
c
c       Writing the particle into the corresponding compressed
c       output file. It is first necessary to check that its
c       position is in the interesting interval.
c
        if (allpclesave(icode, cofile)) then
          if (r2 .lt. r2min0000) then
            ngndlowp(2, 1) = ngndlowp(2, 1) + wt1
            egndlowp(2, 1) = egndlowp(2, 1) + wte
          else if (r2 .gt. r2maxfileg) then
            ngndhighp(2) = ngndhighp(2) + wt1
            egndhighp(2) = egndhighp(2) + wte
          else
c
c           The particle lies in the file saving zone.
c
            if (pcleresample(icode) .and. (r2 .lt. r2minfileg)) then
c
              ngndlowp(2, 2) = ngndlowp(2, 2) + wt1
              egndlowp(2, 2) = egndlowp(2, 2) + wte
c
c             Additional reduction of the statistical sample.
c
              pr00 = (r2 -
     +                (xgi * initshdir(1) + ygi * initshdir(2)) ** 2)
     +               / r2minfileg
c
              saveinfile = (urandom() .lt. pr00)
              if (saveinfile) wt1 = wt1 / pr00
c
            else
c
c             The particle is saved unconditionally.
c
              saveinfile = .true.
c
            endif
c
            if (saveinfile) then
c
c             All checks ok. Writing particle data.
c
              ipdata(1) = icode
              rpdata(1) = elog
              rpdata(2) = rlog
              rpdata(3) = atan2(ygi, xgi)
              rpdata(4) = fgndbuff(ixpxpr   , i)
              rpdata(5) = fgndbuff(ixpypr   , i)
              rpdata(6) = tgi
              rpdata(7) = wt1
              rpdata(8) = fgndbuff(ixcdepth , i)
              rpdata(9) = fgndbuff(ixlhdepth, i)
c
              call ciowritedefrec(cofile, ipdata, rpdata)
c
            endif
          endif
        endif
c
      enddo
c
c     Emptying the buffer.
c
      npgndbuff = 0
c
      return
c
      end
c     --- End of routine pcleship1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cioship1l.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
