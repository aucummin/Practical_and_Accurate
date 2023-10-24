c
c     FILE: ciomergecomm.f                  Creation date: 18/AUG/2003.
c                                       LAST MODIFICATION: 18/AUG/2003.
c
c     Definitions related with the cio system that are used with
c     AiresMerge.
c     The corresponding main quantities are defined in various
c     common files.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Files effectively defined and in use
c
      integer           amnhwciofiles, amnciofiles, amnciofilesu
c
      common            /amcioaux_c12/ amnhwciofiles, amnciofiles,
     +                                 amnciofilesu
c
c     The array indices beginning with 2 are defined that way because
c     "1" refers to the ground particle buffer (treated separately).
c
c     Particles not saved in the corresponding files (not all of
c     these variables are meaningful to every file).
c
      double precision  amnoplowp(5, 2, 2:mxciofiles)
      double precision  amnophighp(5, 2:mxciofiles)
      double precision  ameoplowp(5, 2, 2:mxciofiles)
      double precision  ameophighp(5, 2:mxciofiles)
c
      common            /amopclests_c2/ amnoplowp, amnophighp,
     +                                  ameoplowp, ameophighp
c
c     Particles not saved in the ground particle file.
c
      double precision  amngndlowp(5, 2)
      double precision  amngndhighp(5)
      double precision  amegndlowp(5, 2)
      double precision  amegndhighp(5)
c
      common            /ampclests_c2/ amngndlowp, amngndhighp,
     +                                 amegndlowp, amegndhighp
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'ciomergecomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
