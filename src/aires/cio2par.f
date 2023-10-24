c
c     FILE: cio2par.f                       Creation date: 09/DEC/1996.
c                                       LAST MODIFICATION: 09/DEC/1996.
c
c     This file contains the compressed i/o system related
c     parameters. Part 2: Additional parameters used for processing
c     already created compressed files.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     mxcio2files is the maximum number of input files that can be
c     opened at the same time.
c
      integer           mxcio2files
c
      parameter         (mxcio2files = 10)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'cio2par.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
