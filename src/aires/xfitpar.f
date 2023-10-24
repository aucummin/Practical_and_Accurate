c
c     FILE: xfitpar.f                       Creation date: 13/SEP/1999.
c                                       LAST MODIFICATION: 13/SEP/1999.
c
c     Parameter file for the xmax and nmax fitting routines
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     mxlongidata is the maximum number of data in the longitudinal
c     development.
c
      integer           mxlongidata
      parameter         (mxlongidata = 1024)
c
c     nxfitpar is the number of free parameters in the function used
c     to fit the shower profile for the determination of Xmax and Nmax.
c     Must be greater than 2.
c
      integer           nxfitpar
      parameter         (nxfitpar = 4)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'xfitpar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
