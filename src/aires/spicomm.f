c
c     FILE: spicomm.f                       Creation date: 19/OCT/1999.
c                                       LAST MODIFICATION: 22/AUG/2003.
c
c     Shared variables for the "Special particle" external interface.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003; Fermilab 2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Identification variables.
c
      integer           speidone, spmagic, macroi1, macroi2, macroi3
      character*16      escpclename0
      integer           airescalliversion, escmacrover0
      character*1024    escpclemacro0
c
      common            /spei_c0/ speidone, spmagic, escmacrover0,
     +                            airescalliversion,
     +                            macroi1, macroi2, macroi3,
     +                            escpclename0, escpclemacro0
c
c     Some shower parameters not included in other commons.
c
      integer           primcode0
      double precision  primener0, showert00
      double precision  injpos0(4), uprim0(3)
c
      common            /spei_s0/ primener0, showert00,
     +                            injpos0, uprim0, primcode0
c
c     Running injection point and related variables.
c
      double precision  xinj, yinj, zinj, tinj
      double precision  x1st, y1st, z1st
      integer           set1st
c
      common            /spei_c1/ xinj, yinj, zinj, tinj,
     +                            x1st, y1st, z1st, set1st
c
c     Injection-shower axis coordinate system related variables.
c
      double precision  syschia11, syschia21
      double precision  syschia12, syschia22, syschia32
      double precision  syschia13, syschia23, syschia33
c
      common            /spei_csc/ syschia11, syschia21,
     +                             syschia12, syschia22, syschia32,
     +                             syschia13, syschia23, syschia33
c
c     Variables associated with multiple primaries.
c
      integer           nprimp0
      double precision  eprimp0, eprimpmax
c
      common            /spei_c2/ eprimp0, eprimpmax, nprimp0
c
c     Miscellaneous.
c
      integer           mxintintvar0, mxintfltvar0
c
      common            /speimisc_c/ mxintintvar0, mxintfltvar0
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'spicomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
