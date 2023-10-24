c
c     FILE: pclecodes.f                     Creation date: 21/AUG/1996.
c                                       LAST MODIFICATION: 14/JUL/2003.
c
c     Frequently used elementary particle codes and generic groups,
c     defined as parameters.
c
c     NOTE: All the defined codes must match the codes introduced in
c           routine inipcledata (file 'particles.f')
c
c     Written by: S. J. Sciutto, La Plata 1996, 1998, 2000, 2003.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     Elementary particles codes.
c
      integer           nullcode
      integer           gammacode
      integer           electroncode, positroncode
      integer           mupluscode, muminuscode
      integer           taupluscode, tauminuscode
      integer           nuecode, numucode, nutaucode
      integer           nuebcode, numubcode, nutaubcode
      integer           pipluscode, piminuscode, pizerocode
      integer           kpluscode, kminuscode, k0scode, k0lcode
      integer           etacode, lambdacode, lambdabcode
      integer           sigma0code, sigma0bcode
      integer           sigmapluscode, sigmaplusbcode
      integer           sigmaminuscode, sigmaminusbcode
      integer           xi0code, xi0bcode
      integer           xipluscode, xiplusbcode
      integer           ximinuscode, ximinusbcode
      integer           omegaminuscode, omegaminusbcode
      integer           neutroncode, nbarcode
      integer           protoncode, pbarcode
c
      parameter         (nullcode         =   0)
      parameter         (gammacode        =   1)
      parameter         (electroncode     =  -2)
      parameter         (positroncode     =   2)
      parameter         (mupluscode       =   3)
      parameter         (muminuscode      =  -3)
      parameter         (taupluscode      =   4)
      parameter         (tauminuscode     =  -4)
      parameter         (nuecode          =   6)
      parameter         (nuebcode         =  -6)
      parameter         (numucode         =   7)
      parameter         (numubcode        =  -7)
      parameter         (nutaucode        =   8)
      parameter         (nutaubcode       =  -8)
      parameter         (pipluscode       =  11)
      parameter         (piminuscode      = -11)
      parameter         (pizerocode       =  10)
      parameter         (kpluscode        =  14)
      parameter         (kminuscode       = -14)
      parameter         (k0scode          =  12)
      parameter         (k0lcode          =  13)
      parameter         (etacode          =  15)
      parameter         (lambdacode       =  20)
      parameter         (lambdabcode      = -20)
      parameter         (sigma0code       =  21)
      parameter         (sigma0bcode      = -21)
      parameter         (sigmapluscode    =  22)
      parameter         (sigmaplusbcode   = -22)
      parameter         (sigmaminusbcode  =  23)
      parameter         (sigmaminuscode   = -23)
      parameter         (xi0code          =  24)
      parameter         (xi0bcode         = -24)
      parameter         (xipluscode       =  25)
      parameter         (xiplusbcode      = -25)
      parameter         (ximinusbcode     =  26)
      parameter         (ximinuscode      = -26)
      parameter         (omegaminuscode   = -28)
      parameter         (omegaminusbcode  =  28)
      parameter         (neutroncode      =  30)
      parameter         (nbarcode         = -30)
      parameter         (protoncode       =  31)
      parameter         (pbarcode         = -31)
c
c     "Generic" particle groups.
c
      integer           geplusminus, gmuplusminus, gtauplusminus
      integer           gpion, gchpion, gkaon, gchkaon
      integer           gnppbar, gnnbar, gppbar, gnucnucbr
c
      parameter         (geplusminus   =  1)
      parameter         (gmuplusminus  =  2)
      parameter         (gtauplusminus =  3)
      parameter         (gpion         =  4)
      parameter         (gchpion       =  5)
      parameter         (gkaon         =  6)
      parameter         (gchkaon       =  7)
      parameter         (gnppbar       =  8)
      parameter         (gnnbar        =  9)
      parameter         (gppbar        = 10)
      parameter         (gnucnucbr     = 11)
c
c     Codes that must be set dynamically
c
      integer           alphapcode
c
      common            /dynpcodes/ alphapcode
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'pclecodes.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
