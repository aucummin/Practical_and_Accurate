c
c     FILE: modelcomm.f                     Creation date: 16/AUG/1996.
c                                       LAST MODIFICATION: 15/FEB/2002.
c
c     This file contains shared data used in the different physical
c     models for particle interactions.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999;
c                                La Plata 2000, 2001, 2002.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Propagating model identification.
c
      integer           propmodelid0, propmodelid
c
      common            /propmodel_c0/ propmodelid0, propmodelid
c
c     Data for gamma ray interactions.
c
      double precision  photoelec0, photoelrate
      double precision  gacollthreshold
      double precision  radlength, uradlength
      double precision  ppcrsnifty, egyppifty
      double precision  spconst, spconst0
      double precision  zlog, zlogradlen
      double precision  compf0, screenlim, screenfac
      double precision  thirdlogz, foverzlog
      double precision  lpmfac, s1sqlpm, ydielfac
      double precision  zeff, avzovera, ccoll
c
      common            /auxmodel1/ photoelec0, photoelrate,
     +                              gacollthreshold,
     +                              radlength, uradlength,
     +                              ppcrsnifty, egyppifty,
     +                              spconst, spconst0,
     +                              zlog, zlogradlen, foverzlog,
     +                              compf0, screenfac,
     +                              screenlim, thirdlogz,
     +                              lpmfac, s1sqlpm, ydielfac,
     +                              zeff, avzovera, ccoll
c
c     Data for e+ e- interactions, and elastic scattering.
c
      double precision  minbremenergy, ebremenergycut, grough, erough
      double precision  brempath, lpmthreshold, dielsthreshold
      double precision  panns10, panns20, panns30
      double precision  pannv40, pannv50
      double precision  aplos, bplos, cplos
      double precision  aplosl, bplosl, cplosl, dplosl
      double precision  aelos, belos, celos
      double precision  aelosl, belosl, celosl, delosl
      double precision  sphotlrate
      double precision  exp1, exp2
      double precision  mefo
      double precision  koratefpa, koratefpb
      double precision  koratefea, koratefeb
      double precision  cmul00, cmul10, cmul11, cmul12
c
      common            /auxmodel2/ minbremenergy, ebremenergycut,
     +                              grough, erough, brempath,
     +                              lpmthreshold, dielsthreshold,
     +                              panns10, panns20, panns30,
     +                              pannv40, pannv50,
     +                              aplos, bplos, cplos,
     +                              aplosl, bplosl, cplosl, dplosl,
     +                              aelos, belos, celos,
     +                              aelosl, belosl, celosl, delosl,
     +                              sphotlrate, exp1, exp2, mefo,
     +                              koratefpa, koratefpb,
     +                              koratefea, koratefeb,
     +                              cmul00, cmul10, cmul11, cmul12
c
c     Data for heavy charged particle interactions.
c
      double precision  heavymineko2
      double precision  amlos, bmlos, cmlos, dmlos, zmlos
      double precision  nucollthreshold
c
      common            /auxmodel3/ heavymineko2,
     +                              amlos, bmlos, cmlos, dmlos, zmlos,
     +                              nucollthreshold
c
c     Data for muon bremsstrahlung and pair production interactions.
c
      double precision  minmubremegy, mubremegycut, mbrmfp0, mppmfp0
      double precision  mubremupar, muppepar, mubremphi0
      double precision  mubremphin0, mubremphin1
      double precision  mubremphie0, mubremphie1
      double precision  mubremphie2, mubremphie3
c
      double precision  mesqrte2f, muppugmax
c
      common            /auxmodel4/ minmubremegy, mubremegycut,
     +                              mubremupar, muppepar,
     +                              mbrmfp0, mppmfp0,
     +                              mubremphi0,
     +                              mubremphin0, mubremphin1,
     +                              mubremphie0, mubremphie1,
     +                              mubremphie2, mubremphie3,
     +                              mesqrte2f, muppugmax
c
      double precision  RZeff3, muppf1, muppf6
c
      common            /secmup0static /
     +                           RZeff3, muppf1, muppf6
c
c     Threshold energies.
c
      double precision  gammacut1, epmcut1, muoncut1, heavycut1
      double precision  nuclcut1
      double precision  tepmcut1
c
      common            /auxmodelcut/ gammacut1, epmcut1, tepmcut1,
     +                                muoncut1, heavycut1, nuclcut1
c
c     Hadron-nucleus, gamma-nucleus and nucleus-nucleus collisions
c     related data.
c
      double precision  highcollthresh(2)
      double precision  highcollthresh2(2)
      double precision  highcollfac2(2)
      logical           externalhcoll
c
      common            /hcoll_c0/ highcollthresh, highcollthresh2,
     +                             highcollfac2, externalhcoll
c
      integer           helenucalls(4)
      integer           hadcollcalls(4)
c
      common            /hcoll_c1/ helenucalls, hadcollcalls
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'modelcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
