c
c     FILE: syspar.f                        Creation date: 01/JUL/1997.
c                                       LAST MODIFICATION: 01/JUL/1997.
c
c     Miscellaneous system parameters.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     icharsetmin and icharsetmax define the minimum and maximum
c     character codes used by AIRES in its input data streams.
c     For ascii systems icharsetmin (icharsetmax) are set to 32 (126).
c     All characters out of the range [icharsetmin, icharsetmax] are
c     treated as blanks.
c
      integer           icharsetmin, icharsetmax
c
      parameter         (icharsetmin = 32)
      parameter         (icharsetmax = 126)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'syspar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
