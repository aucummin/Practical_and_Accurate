c
c     FILE: opbuffcomm.f                    Creation date: 10/JUL/1997.
c                                       LAST MODIFICATION: 23/JUN/1999.
c
c     This file contains the definitions of the arrays used to store
c     particle data to be saved in compressed files other than the
c     ground particle file.
c
c     Written by: S. J. Sciutto, La Plata 1997; Fermilab 1999.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     The array indices beginning with 2 are defined that way because
c     "1" refers to the ground particle buffer (treated separately).
c
c     Main buffers
c
      integer           npopbuff(2:mxciofiles)
      integer           opbuffsta(opbuffsize, 2:mxciofiles)
      double precision  opbuffiaux(nopauxi, opbuffsize, 2:mxciofiles)
      double precision  opbufffaux(nopauxf, opbuffsize, 2:mxciofiles)
      double precision  fopbuff(maxstalen, opbuffsize, 2:mxciofiles)
c
      common            /opbuff_c1/ fopbuff, opbufffaux, opbuffiaux,
     +                              opbuffsta, npopbuff
c
c     Equivalencing with other variables to improve stack
c     management.
c
      integer           iopbuff(maxstalen4, opbuffsize, 2:mxciofiles)
      logical           lopbuff(maxstalen4, opbuffsize, 2:mxciofiles)
      character*(mxbch) opbuffrec(opbuffsize, 2:mxciofiles)
c
      equivalence       (fopbuff, iopbuff, lopbuff, opbuffrec)
c
c     Particles not saved in the corresponding files (not all of
c     these variables are meaningful to every file).
c
      double precision  noplowp(5, 2, 2:mxciofiles)
      double precision  nophighp(5, 2:mxciofiles)
      double precision  eoplowp(5, 2, 2:mxciofiles)
      double precision  eophighp(5, 2:mxciofiles)
c
      common            /opclests_c2/ noplowp, nophighp,
     +                                eoplowp, eophighp
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'opbuffcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
