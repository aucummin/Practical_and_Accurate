c
c     FILE: idfhead.f                       Creation date: 23/MAR/1999.
c                                       LAST MODIFICATION: 07/MAY/2002.
c
c     idf/adf/tss file specific parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999, 2002.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     idf specific header.
c
      character*32      idfheader0, adfheader0
      parameter         (idfheader0 = '--- AIRES idf --- ')
      parameter         (adfheader0 = '--- AIRES adf --- ')
      integer           idfintheader0
      parameter         (idfintheader0 = 0)
c
      character*(*)     tssheader0
      parameter         (tssheader0 = ' AIRES TSS --- ')
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'idfhead.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
