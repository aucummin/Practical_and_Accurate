c
c     FILE: cioauxcomm.f                    Creation date: 19/DEC/1996.
c                                       LAST MODIFICATION: 07/JUL/1997.
c
c     Some auxiliary variables to mange the Aires cio system during the
c     air shower simulations.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Files effectively defined.
c
      integer           nhwciofiles, nciofiles
      integer           ciofiles(mxciofiles + 8)
      integer           ciofcateg(mxciofiles + 8)
c
      common            /cioaux_c1/ nhwciofiles, nciofiles,
     +                              ciofiles, ciofcateg
c
c     Files effectively in use.
c
      integer           nciofilesu
      integer           ciofilesu(mxciofiles)
c
      common            /cioaux_c2/ nciofilesu, ciofilesu
c
c     lastciopointer marks the end of shower pointer in the
c     compressed files.
c
      integer           lastciopointer(mxciofiles)
c
      common            /cioaux_c2/ lastciopointer
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'cioauxcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
