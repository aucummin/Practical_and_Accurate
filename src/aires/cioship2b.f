c
c     FILE: cioship2b.f                     Creation date: 14/JUL/1997.
c                                       LAST MODIFICATION: 06/MAR/2002.
c
c     This file contains the routines that process the records to be
c     written in the "lgtpcles" compressed output file.
c
c     Normal-c record version.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pcleship2
c
c     Processing a full longitudinal tracking file particle buffer.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998; Fermilab 1999;
c                                La Plata 1999, 2002.
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
      include 'opbuffcomm.f'
      include 'showercomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, icode, cofile
      integer           firstol, lastol, olkey
      double precision  uxpr, uypr, uzpr
      double precision  wt1, tmp1, tmp2, tmp3, tmp4
      double precision  xfi, yfi, tfi
      double precision  xfa, yfa, rfa
      double precision  rmaxfilel, pr00
      logical           saveinfile
      integer           ipdata(12)
      double precision  rpdata(12)
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      cofile    = longiciofile
      rmaxfilel = rmaxfile(cofile)
c
c     Loop over the buffer records.
c
      do i = 1, npopbuff(cofile)
c
c       Auxiliary fields available here:
c
c       opbuffiaux(1, *, cofile) \  Information about crossed
c       opbuffiaux(2, *, cofile) /  observing levels.
c
c       opbufffaux(1, *, cofile) -  Energy at first observing level
c
        icode = iopbuff(ixpcode, i, cofile)
        uxpr  = fopbuff(ixpxpr , i, cofile)
        uypr  = fopbuff(ixpypr , i, cofile)
        uzpr  = fopbuff(ixpzpr , i, cofile)
c
c       Evaluating the intersection point with the first observing
c       level crossed (coordinates and time).
c
c       To calculate these quantities is first necessary to determine
c       the first and last observing level crossed. The raw data coming
c       from the kernel provides the index of the level previous to the
c       first (posterior to the last) crossed one when the particle is
c       going downwards (upwards). It is thus necessary to shift one of
c       the input parameters.
c
        firstol = opbuffiaux(1, i, cofile)
        lastol  = opbuffiaux(2, i, cofile)
c
        if (firstol .le. lastol) then
c
c         The particle is going towards lower observing levels
c         (that is, "downwards")
c
          firstol = firstol + 1
c
        else
c
c         The particle is going towards higher observing levels
c         (that is, "upwards")
c
          lastol  = lastol + 1
c
        endif
c
c       Now the intersection point can be evaluated.
c       The (x, y) origin is located at the intersection between the
c       shower axis and the first observing level surface.
c
        xfi  = fopbuff(ixx, i, cofile) - obslevcore(1, firstol)
        yfi  = fopbuff(ixy, i, cofile) - obslevcore(2, firstol)
c
c       For particles near the core, the intersection point is
c       evaluated replacing the observing level spherical surface by
c       the tangent plane passing through the intersection between the
c       observing level surface and the shower axis. Otherwise the
c       intersection point is evaluated using spherical geometry.
c
        if ((abs(xfi) .le. hplanelim) .and. (abs(yfi) .le. hplanelim))
     +  then
          tmp1 = (obslevcore(3, firstol) - fopbuff(ixz, i, cofile) +
     +            obslevcore(4, firstol) * xfi +
     +            obslevcore(5, firstol) * yfi) /
     +           (uzpr - uypr * obslevcore(5, firstol) -
     +                   uxpr * obslevcore(4, firstol))
        else
          tmp3 = rearth + fopbuff(ixz, i, cofile)
          tmp2 = fopbuff(ixx, i, cofile) * uxpr +
     +           fopbuff(ixy, i, cofile) * uypr + tmp3 * uzpr
          tmp3 = obslevz00sq(firstol) - tmp3 ** 2 -
     +           fopbuff(ixx, i, cofile) ** 2 -
     +           fopbuff(ixy, i, cofile) ** 2
          tmp4 = sqrt(max(tmp2 ** 2 + tmp3, 0.d0))
          if (tmp2 .lt. 0) then
            tmp1 = - tmp4 - tmp2
          else
            tmp1 = tmp4 - tmp2
          endif
        endif
c
        xfi  = xfi + uxpr * tmp1
        yfi  = yfi + uypr * tmp1
c
c       Writing the particle into the corresponding compressed
c       output file. It is first necessary to check that its
c       position is in the interesting interval.
c
        xfa = abs(xfi)
        yfa = abs(yfi)
        rfa = olrminfile(firstol, 0)
        wt1 = fopbuff(ixwt, i, cofile)
c
        if ((xfa .lt. rfa) .and. (yfa .lt. rfa)) then
          noplowp(2, 1, cofile) = noplowp(2, 1, cofile) + wt1
        else if ((xfa .gt. rmaxfilel) .or. (yfa .gt. rmaxfilel)) then
          nophighp(2, cofile) = nophighp(2, cofile) + wt1
        else
c
c         The particle lies in the file saving zone.
c
          rfa = olrminfile(firstol, 1)
c
          if (pcleresample(icode) .and. 
     +        ((xfa .lt. rfa) .and. (yfa .lt. rfa))) then
c
c           Additional reduction of the statistical sample.
c
            pr00 = xfa * xfa + yfa * yfa
c
            if (pr00 .lt. olrminfile(firstol, 2)) then
              noplowp(2, 2, cofile) = noplowp(2, 2, cofile) + wt1
              pr00 = pr00 / olrminfile(firstol, 2)
              saveinfile = (urandom() .lt. pr00)
              if (saveinfile) wt1 = wt1 / pr00
            else
              saveinfile = .true.
            endif
c
          else
c
c           The particle is saved unconditionally.
c
            saveinfile = .true.
c
          endif
c
          if (saveinfile) then
c
c           All checks ok. Writing particle data.
c
c           Evaluating integer number giving information about the
c           crossed observing levels.
c           This key number is evaluated in the following way:
c
c             olkey = (first ol) + 512 * (last ol) + 512^2 * updown
c
c           where updown is one if the particles goes upwards, zero
c           otherwise.
c
            olkey = firstol + 512 * lastol
c
c           Correction to identify particles that go upwards.
c
            if (uzpr .gt. 0) olkey = olkey + 262144
c
c           The time is shifted to obtain delay time with respect to
c           the corresponding observing level.
c
            tfi  = obslevt0(firstol) + fopbuff(ixtime, i, cofile) +
     +             tmp1 / fopbuff(ixbeta, i, cofile)
c
            ipdata(1) = icode
            ipdata(2) = olkey
            rpdata(1) = uxpr
            rpdata(2) = uypr
            rpdata(3) = wt1
            rpdata(4) = tfi
            rpdata(5) = xfi
            rpdata(6) = yfi
c
            call ciowritedefrec(cofile, ipdata, rpdata)
c
          endif
        endif
c
      enddo
c
c     Emptying the buffer.
c
      npopbuff(cofile) = 0
c
      return
c
      end
c     --- End of routine pcleship2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cioship2b.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
