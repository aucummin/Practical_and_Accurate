c
c     FILE: pdata.f                         Creation date: 17/JUL/1996.
c                                       LAST MODIFICATION: 06/MAR/2002.
c
c     This file contains the definitions needed for processing
c     the particle stacks. This file should be included in every
c     routine implemented for such task.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000, 2002.
c
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
c
c     Parameters for data size.
c
      include 'pstackpar.f'
c
c     Particle data template.
c
      double precision  frecord(maxstalen)
      integer           irecord(maxstalen4)
      logical           lrecord(maxstalen4)
      character*(mxbch) pclerecord
c
      equivalence       (frecord, irecord, lrecord, pclerecord)
c
      double precision  prenergy, prbeta, prwt
      double precision  prx, pry, prz, prvz, prdepth
      double precision  prtime, prpx, prpy, prpz
      double precision  prpxpr, prpypr, prpzpr
      double precision  prcdepth, prctime, prlhdepth
      double precision  psparef(0:nfspares1)
c
      integer           prcode, prlol, prlolsa, prcal
      integer           psparei(0:nispares1)
c
      logical           prlowe
      logical           psparel(0:nispares1)
c
      equivalence       (frecord(ixenergy) , prenergy)
      equivalence       (frecord(ixbeta)   , prbeta)
      equivalence       (frecord(ixwt)     , prwt)
      equivalence       (frecord(ixx)      , prx)
      equivalence       (frecord(ixy)      , pry)
      equivalence       (frecord(ixz)      , prz)
      equivalence       (frecord(ixvz)     , prvz)
      equivalence       (frecord(ixdepth)  , prdepth)
      equivalence       (frecord(ixtime)   , prtime)
      equivalence       (frecord(ixpx)     , prpx)
      equivalence       (frecord(ixpy)     , prpy)
      equivalence       (frecord(ixpz)     , prpz)
      equivalence       (frecord(ixpxpr)   , prpxpr)
      equivalence       (frecord(ixpypr)   , prpypr)
      equivalence       (frecord(ixpzpr)   , prpzpr)
      equivalence       (frecord(ixcdepth) , prcdepth)
      equivalence       (frecord(ixctime)  , prctime)
      equivalence       (frecord(ixlhdepth), prlhdepth)
      equivalence       (frecord(ixf0)     , psparef)
c
      equivalence       (irecord(ixpcode)  , prcode)
      equivalence       (irecord(ixlol)    , prlol)
      equivalence       (irecord(ixlolsa)  , prlolsa)
      equivalence       (irecord(ixcal)    , prcal)
      equivalence       (irecord(ixil0)    , psparei)
c
      equivalence       (lrecord(ixlowe)   , prlowe)
      equivalence       (lrecord(ixil0)    , psparel)
c
c     Description of the defined variables:
c
c         prcode          Particle type (code).
c         prenergy        Energy (GeV).
c         prwt            Weight (GE 1).
c         prx             \
c         pry              > Particle coordinates (m).
c         prz             /
c         prvz            Vertical (local) altitude (m).
c         prdepth         Depth (g/cm2).
c         prtime          Current particle time (sec) (*).
c         prpx            \
c         prpy             > Direction of the impulse (current).
c         prpz            /
c         prpxpr          \  Direction of the impulse (used for last
c         prpypr           > straight line propagation).
c         prpzpr          /
c         prcdepth        Particle creation depth (g/cm2).
c         prctime         Particle creation time (*).
c         prlhdepth       Depth of last hadronic interaction.
c         prbeta          Particle speed (internal use only).
c         prcal           Current atmospheric layer.
c         prlol           Observing level internal data (do not mo-
c                         dify and transmit for decay products).
c         prlolsa         Idem prlol.
c         prlowe          Switch to label low energetic particles.
c
c         psparef         double precision spare positions.
c         psparei         Integer spare positions.
c         psparel         Logical spare positions.
c
c       Particle code, prlol and prcal are integers,
c       prlowe is logical and all other variables are double precision.
c       All the named variables can also be referred as positions of
c       the main particle data array "frecord" replacing the "pr" by
c       "ix", for example: pry is equivalent to frecord(ixy).
c
c       (*) Time is set to zero either at the beginning of the shower
c           or at the fisrt primary interaction.
c
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
c
c     End of file 'pdata.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
