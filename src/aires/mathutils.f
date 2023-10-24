c
c     FILE: mathutils.f                     Creation date: 15/AUG/1996.
c                                       LAST MODIFICATION: 14/APR/2003.
c
c     This file contains several routines to perform mathematical
c     calculations.
c     Part 1: Mathematical routines that use physical concepts.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine padvance(path, u, beta, depth, position, time,
     +                    olidx, atlayer, dptprev)
c
c     Changing the coordinates and time of a particle, moving it a
c     given "path" (in g/cm2) in a direction marked by the unitary
c     vector u.
c     The new position is evaluated solving for "newpos" the following
c     line integral:
c
c                  newpos
c          path = INTEGRAL  rho(z) dr
c                  oldpos
c
c     where rho(z) is the density of the medium, which is supposed to
c     depend only on z, the altitude (third component of r).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2003.
c
c
c     Parameters:
c     ==========
c
c     path............ (input, double precision) The path to advance,
c                      in g/cm2.
c     u............... (input, double precision, array(3)) Direction of
c                      motion, expressed as a unitary vector.
c     beta............ (input, double precision) Particle speed over
c                      speed of light.
c     depth........... (input-output, double precision) The particle
c                      depth, in g/cm2.
c     position........ (input-output, double precision, array(4))
c                      Position of the particle (meters). The altitude
c                      is recorded twofold: position(3) is z
c                      coordinate while position(4) is altitude above
c                      sea level, as measured perpendicularly to the
c                      earth's surface.
c     time............ (input-output, double precision) Clock time.
c     olidx........... (input-output, integer) Observing level index.
c                      This variable can be modified when changing
c                      from spherical to plane regime or vice-versa.
c     atlayer......... (output, integer) Atmospheric layer where the
c                      particle is after the move.
c     dptprev......... (output, double precision) The previous particle
c                      depth in g/cm2. Normally this variable is equal
c                      to the initial value of "depth", but may differ
c                      slightly when there is a change from spherical
c                      to plane regime or vice-versa.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  path, beta
      double precision  u(3)
      double precision  depth, time
      double precision  position(4)
      integer           olidx, atlayer
      double precision  dptprev
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  uzabsmin, uzabsmin2, xq4
      parameter         (uzabsmin  = 0.0075d0)
      parameter         (uzabsmin2 = 0.2000d0)
      parameter         (xq4       = xquantum / 4)
      logical           plane
      double precision  hplim, depth2, olddepth, oldz, oldz00
      double precision  oldzv, oldzv00, oldzv00sq
      double precision  newz, newzv, newzv00, newzv00sq
      double precision  proypath, rpath, lu3, fu3, c, d
      double precision  step, rstep, maxstep, ptogo
      integer           l
      double precision  depthfromz, zfromdepth, adstymdepth
c
c     FIRST EXECUTABLE STATEMENT
c
c     The z axis is assumed to point upwards (in the direction of
c     decreasing depth). For particles located near the z axis, all
c     calculations are done assuming that the atmosphere is plane.
c     For far particles, instead, the earth's curvature is taken into
c     account.
c
      dptprev  = depth
      olddepth = depth
      hplim    = hplanelim0 + hplanelim1 * position(4)
c
      if ((abs(position(1)) .lt. hplim) .and.
     +    (abs(position(2)) .lt. hplim)) then
c
c       The particle is near the z axis.
c
        proypath = path * u(3)
        plane    = .true.
c
        if (position(3) .ne. position(4)) then
c
c         The particle comes from the spheric regime.
c
          depth2 = depthfromz(position(3), atlayer)
c
          if (abs(2 * (depth2 - depth)) .lt. abs(proypath)) then
c
c           It is safe to change to plane geometry now.
c
            olddepth    = depth2
            dptprev     = depth2
            olidx       = obslevca * depth2 + obslevcb
            olidx       = max(0, min(olidx, totobslev))
c
          else
c
c           Spherical geometry cannot be abandoned yet.
c
            plane    = .false.
c
          endif
        endif
c
      else
c
c       The particle is far from the z axis. Using spherical geometry.
c
        plane = .false.
c
      endif
c
      if (plane) then
c
c       The particle is near the z axis.
c
        depth = olddepth - proypath
        newz  = zfromdepth(depth, atlayer)
        newzv = newz
c
        if (abs(u(3)) .ge. uzabsmin) then
c
c         The parameters can be evaluated with "vertical" calculations.
c
          rpath = (newz - position(3)) / u(3)
c
        else
c
c         The motion is virtually horizontal, thus with constant
c         density.
c
          rpath = path / adstymdepth(olddepth, l)
c
        endif
c
        position(1) = position(1) + rpath * u(1)
        position(2) = position(2) + rpath * u(2)
c
      else
c
c       The particle is far from the z axis.
c
        oldz   = position(3)
        oldz00 = rearth + oldz
c
        if (position(4) .ne. oldz) then
c
c         The particle was already in spherical geometry regime.
c
          oldzv     = position(4)
          oldzv00   = rearth + oldzv
          oldzv00sq = oldzv00 ** 2
c
        else
c
c         The particle was in plane regime and enters spherical zone.
c
          oldzv00sq   = oldz00 ** 2 + position(1) ** 2 +
     +                                position(2) ** 2
          oldzv00     = sqrt(oldzv00sq)
          oldzv       = oldzv00 - rearth
          olddepth    = depthfromz(oldzv, atlayer)
          dptprev     = olddepth
          olidx       = obslevca * olddepth + obslevcb
          olidx       = max(0, min(olidx, totobslev))
c
        endif
c
        fu3 = position(1) * u(1) + position(2) * u(2) + oldz00 * u(3)
        lu3 = fu3 / oldzv00
c
        if (abs(lu3) .ge. uzabsmin2) then
c
c         The motion is "safely" far from horizontal motion.
c         All calculations will be done vertically. It is assumed here
c         that the horizontal distance from the  central vertical axis
c         does not represent more than 10 geographic degrees (about
c         1100 km).
c
          depth     = olddepth - path * lu3
          newzv     = zfromdepth(depth, atlayer)
          newzv00sq = (rearth + newzv) ** 2
c
          c = newzv00sq - oldzv00sq
          d = sqrt(max(fu3 ** 2 + c, 0.d0))
c
          if (fu3 .le. 0) then
            rpath = - d - fu3
          else
            rpath = d - fu3
          endif
c
          position(1) = position(1) + rpath * u(1)
          position(2) = position(2) + rpath * u(2)
          newz        = position(3) + rpath * u(3)
c
        else
c
c         The motion is not "so vertical" so it is necessary to
c         evaluate the new position more carefully.
c
c         Very long paths are chopped in small segments.
c
          ptogo = path
          rpath = 0
c
 1100     continue
c
          if (depth .ge. 80.d0) then
            maxstep = 80.d0
          else
            maxstep = max(depth, xq4)
          endif
          if (ptogo .gt. maxstep) then
            step = maxstep
          else
            step = ptogo
          endif
c
          fu3       = position(1) * u(1) + position(2) * u(2) +
     +                oldz00 * u(3)
          lu3       = fu3 / oldzv00
          depth     = olddepth - step * lu3
          newzv     = zfromdepth(depth, atlayer)
          newzv00   = rearth + newzv
          newzv00sq = newzv00 ** 2
c
          if (abs(lu3) .ge. uzabsmin) then
c
c           The motion is not horizontal in the "local" system.
c
            c = newzv00sq - oldzv00sq
            d = sqrt(max(fu3 ** 2 + c, 0.d0))
c
            if (fu3 .le. 0) then
              rstep = - d - fu3
            else
              rstep = d - fu3
            endif
c
            position(1) = position(1) + rstep * u(1)
            position(2) = position(2) + rstep * u(2)
            newz        = oldz        + rstep * u(3)
c
          else
c
c           With respect to the local coordinate system, the motion
c           is virtually horizontal, thus with constant density.
c
            rstep = step / adstymdepth(olddepth, l)
c
            position(1) = position(1) + rstep * u(1)
            position(2) = position(2) + rstep * u(2)
            newz        = sqrt(newzv00sq -
     +                         position(1) ** 2 - position(2) ** 2) -
     +                    rearth
c
          endif
c
          ptogo     = ptogo - step
          rpath     = rpath + rstep
          oldz      = newz
          oldz00    = rearth + newz
          oldzv     = newzv
          oldzv00   = newzv00
          oldzv00sq = newzv00sq
          olddepth  = depth
c
          if ((depth .gt. 0) .and. (ptogo .gt. 0)) goto 1100
c
        endif
      endif
c
      position(3) = newz
      position(4) = newzv
c
c     Advancing the clock for this particle. Notice that the time
c     unit used is not "second": With the internal time unit, the
c     speed of light is 1 (with length expressed in meters).
c     Times are automatically corrected on output, so this internal
c     time unit does not affect the user's results.
c
      time = time + abs(rpath) / beta
c
      return
      end
c     --- End of routine padvance.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function ppath(distance, u, depth, position)
c
c     Evaluating the path (in g/cm2) corresponding to a spatial
c     distance "distance" (in m), starting from position "position" and
c     depth "depth", and moving in the direction pointed by the unitary
c     vector "u"
c     The path is evaluated solving the following line integral:
c
c                  newpos
c          path = INTEGRAL  rho(z) dr
c                  oldpos
c
c     where rho(z) is the density of the medium, which is supposed to
c     depend only on z, the altitude (third component of r).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
c
c
c     Parameters:
c     ==========
c
c     distance........ (input, double precision) The spatial distance
c                      (m).
c     u............... (input, double precision, array(3)) Direction of
c                      motion, expressed as a unitary vector.
c     depth........... (input, double precision) The particle depth, in
c                      g/cm2.
c     position........ (input, double precision, array(4)) Position of
c                      the particle (meters). The altitude of the
c                      particle must be recorded twofold: position(3)
c                      corresponds to the z coordinate while
c                      position(4) is the altitude above sea level, as
c                      measured perpendicularly to the earth's surface.
c
c     Return value: (double precision) The corresponding path, in
c     ============  g/cm2.
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
      double precision  ppath
      double precision  distance, depth
      double precision  u(3)
      double precision  position(4)
c
c     Declaration of internal variables and arrays.
c
      double precision  uzabsmin
      parameter         (uzabsmin = 0.0075d0)
      double precision  zv, lu3, newdepth
      integer           l
      double precision  depthfromz, adstymdepth
c
c     FIRST EXECUTABLE STATEMENT
c
c     The z axis is assumed to point upwards (in the direction of
c     decreasing depth). For particles located near the z axis, all
c     calculations are done assuming that the atmosphere is plane.
c     For far particles, instead, the earth's curvature is taken into
c     account.
c
      zv = position(4)
c
      if (zv .eq. position(3)) then
c
c       The particle is near the z axis.
c
        lu3 = u(3)
c
      else
c
c       The particle is far from the z axis.
c
        lu3 = (position(1) * u(1) + position(2) * u(2) +
     +         (rearth + position(3)) * u(3)) / (rearth + zv)
c
      endif
c
      if (abs(lu3) .ge. uzabsmin) then
c
c       The parameters can be evaluated with "vertical" calculations
c       in the local coordinate system.
c
        newdepth = depthfromz(zv + distance * lu3, l)
        ppath    = abs((depth - newdepth) / lu3)
c
      else
c
c       The motion is virtually horizontal (in the local coordinate
c       system), thus with constant density.
c
        ppath = abs(adstymdepth(depth, l) * distance)
c
      endif
c
      return
      end
c     --- End of routine ppath.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine salastol(newpos, oldlol, newlol)
c
c     Calculating the last observing level crossed by the point
c     corresponding to the orthogonal projection of the particle's
c     position onto the shower axis.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
c     Parameters:
c     ==========
c
c     newpos.......... (input, double precision, array(3)) The
c                      particle's position (in m), with respect to
c                      the AIRES coordinate system.
c     oldlol.......... (input, integer) Last observing level crossed.
c     newlol.......... (output, integer) Current value for the
c                      last observing level crossed index.
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
      double precision  newpos(3)
      integer           oldlol, newlol
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'showercomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  sa
      logical           ln, ln1
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating coordinate along shower axis.
c
      sa = zsfact(0) + zsfact(1) * newpos(1) +
     +                 zsfact(2) * newpos(2) +
     +                 zsfact(3) * newpos(3)
c
c     First guess (nothing changed).
c
      newlol = oldlol
c
 1010 continue
c
      ln  = (sa .le. obslevsa(newlol))
      ln1 = (sa .gt. obslevsa(newlol + 1))
c
      if (ln .and. ln1) then
        return
      else if (ln) then
        if (newlol .eq. totobslev) return
        newlol = newlol + 1
      else
        if (newlol .eq. 0) return
        newlol = newlol - 1
      endif
      goto 1010
c
      end
c     --- End of routine salastol.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine geobdeflect(rbpath, totenergy, u)
c
c     Emulating the deflection of a particle due to its interaction
c     with the earth's magnetic field. The change in the direction of
c     motion unitary vector u depends on the effective path
c     (distance / beta) and the total energy (kinetic + rest) of the
c     particle. Its charge is taken into account at the moment of
c     initializing internal quantities (see routine "lgtinit"); and
c     this is done including a factor of 2 so "rbpath" represents
c     actually half of the total distance the particle will be
c     advanced.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Parameters:
c     ==========
c
c     rbpath.......... (input, double precision) Half the effective
c                      path the particle is advanced (distance / beta)
c                      (m).
c     totenergy....... (input, double precision) Total energy (kinetic
c                      plust rest energy, GeV).
c     u............... (input-output, double precision, array(3))
c                      Direction of motion, expressed as a unitary
c                      vector.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      double precision  rbpath
      double precision  totenergy
      double precision  u(3)
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  bp0, bpx, bpz, u1new, u2new, u3new
c
c     FIRST EXECUTABLE STATEMENT
c
      bp0     = rbpath / totenergy
      bpx     = facgbx * bp0
      bpz     = facgbz * bp0
c
      u1new = u(1) + u(2) * bpz
      u2new = u(2) + u(3) * bpx - u(1) * bpz
      u3new = u(3) - u(2) * bpx
c
c     Renormalizing to ensure that output u is unitary.
c
      bp0  = 1.d0 / sqrt(u1new ** 2 + u2new ** 2 + u3new ** 2)
c
      u(1) = bp0 * u1new
      u(2) = bp0 * u2new
      u(3) = bp0 * u3new
c
      return
      end
c     --- End of routine geobdeflect.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'mathutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
