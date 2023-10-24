c
c     FILE: pcleutils.f                     Creation date: 10/JUL/2003.
c                                       LAST MODIFICATION: 20/MAR/2006.
c
c     Utility routines for particle data management.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function pclerestmass(pcode)
c
c     Rest mass of a particle (in GeV).
c
c     Written by: S. J. Sciutto, La Plata 2003, 2006.
c
c
c     Arguments:
c     =========
c
c     pcode........... (input, integer) AIRES particle code.
c
c     Return value: (double precision) Particle rest mass in GeV.
c     ============  -1 is returned in case of error (invalid particle
c                   code).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      double precision  pclerestmass
      integer           pcode
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  pm
c
c     FIRST EXECUTABLE STATEMENT
c
      pm = -1
c
c     Switching accordingly with the particle code.
c
      if (abs(pcode) .le. maxpcle) then
c
c       A valid elementary particle code was specified.
c
        if (pclename(pcode) .ne. 'NOT_USED') pm = pclemass(pcode)
c
      else if ((pcode .ge. minncode) .and.
     +         (pcode .le. maxncode))      then
c
c       A nucleus code was specified.
c
        if (nucn(pcode) .ge. 0) pm = nucmass(pcode)
c
      endif
c
      pclerestmass = pm
c
      return
      end
c     --- End of routine pclerestmass
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function pclecharge(pcode)
c
c     Returning the charge of the particle whose code is pcode.
c
c     Written by: S. J. Sciutto, La Plata 2005, 2006.
c
c
c     Arguments:
c     =========
c
c     pcode........... (input, integer) AIRES particle code.
c
c     Return value: (integer) Particle charge.
c     ============  -9999 is returned in case of error (invalid
c                   particle code).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      double precision  pclecharge
      integer           pcode
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           pc, n, a
c
c     FIRST EXECUTABLE STATEMENT
c
      pc = -9999
c
c     Switching accordingly with the particle code.
c
      if (abs(pcode) .le. maxpcle) then
c
c       A valid elementary particle code was specified.
c
        if (pclename(pcode) .ne. 'NOT_USED') pc = pcleq(pcode)
c
      else if ((pcode .ge. minncode) .and.
     +         (pcode .le. maxncode))      then
c
c       A nucleus code was specified.
c
        call nucldecode(pcode, pc, n, a)
c
      endif
c
      pclecharge = pc
c
      return
      end
c     --- End of routine pclecharge
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function pclelifetime(pcode)
c
c     Mean lifetime of a particle (in seconds).
c
c     Written by: S. J. Sciutto, La Plata 2003, 2006.
c
c
c     Arguments:
c     =========
c
c     pcode........... (input, integer) AIRES particle code.
c
c     Return value: (double precision) Particle mean life time in sec.
c     ============  0 corresponds to stable particles (infinite
c                   lifetime), or nuclei (radiactive decay times are
c                   are not taken into account). -1 is returned in case
c                   of error (invalid particle code).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      double precision  pclelifetime
      integer           pcode
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  pmlt
c
c     FIRST EXECUTABLE STATEMENT
c
      pmlt = -1
c
c     Switching accordingly with the particle code.
c
      if (abs(pcode) .le. maxpcle) then
c
c       A valid elementary particle code was specified.
c
        if (pclename(pcode) .ne. 'NOT_USED')
     +    pmlt = max(pclelife(pcode), 0.d0)
c
      else if ((pcode .ge. minncode) .and.
     +         (pcode .le. maxncode))      then
c
c       A nucleus code was specified.
c
        if (nucn(pcode) .ge. 0) pmlt = 0
c
      endif
c
      pclelifetime = pmlt
c
      return
      end
c     --- End of routine pclelifetime
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'pcleutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
