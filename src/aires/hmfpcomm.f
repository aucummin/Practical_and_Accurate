c
c     FILE: hmfpcomm.f                      Creation date: 02/JUL/2000.
c                                       LAST MODIFICATION: 15/FEB/2002.
c
c     This file contains shared data used to evaluate hadronic mean
c     free paths.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2002.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     External MFP at high energies.
c
c     Particle code to mfp set conversion array.
c
      integer           pcode2highemfpset(-maxpcle:maxpcle)
c
      common            /hmfphighe_c0/ pcode2highemfpset
c
      double precision  mfp8ppk(8, totmfphsets)
      double precision  mfpthreshold
      logical           externalmfp, externalnucmfp
c
      common            /hmfphighe_c1/ mfp8ppk,
     +                                 mfpthreshold,
     +                                 externalmfp, externalnucmfp
c
c     Built-in MFP's at low energies.
c
c     Particle code to mfp set conversion array.
c
      integer           pcode2lowemfpset(-maxpcle:maxpcle)
c
      common            /hmfplowe_c0/ pcode2lowemfpset
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'hmfpcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
