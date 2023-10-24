c
c     FILE: modelpar.f                      Creation date: 27/MAY/2000.
c                                       LAST MODIFICATION: 29/JUN/2001.
c
c     This file contains static parameters used in the different
c     physical models for particle interactions.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     Some default parameters characterizing the atmospheric medium.
c
      double precision  zeffdef, avzoveradef, radlendef
c
      parameter         (zeffdef     = 7.265d0)
      parameter         (avzoveradef = 0.499d0)
      parameter         (radlendef   = 37.1d0)
c
c     Parameters for coulomb scattering algorithm.
c
c     *chimin are the thresholds for significant scattering angles.
c     fjmod and fjscat are parameters for single scattering processes
c     (from MOCCA algorithm).
c
      double precision  epmchimin, hvychimin
c
      parameter         (epmchimin = 1.d-5)
      parameter         (hvychimin = 3.d-11)
c
      double precision  fjmod, fjscat
c
      parameter         (fjmod  = 0.40d0)
      parameter         (fjscat = 0.35d0)
c
c     Some related derived parameters.
c
      double precision  chic2fac
c
      parameter         (chic2fac = 0.25d0 / fjscat)
c
c     Parameters for e+ e- knock-on processes.
c
c     komin is the low cut energy for secondaries.
c
      double precision  komin
c
      parameter         (komin = 1.0d-3)
c
c     Parameters for continuous e+ e- energy losses. These parameters
c     correspond to a fit to GEANT3 data.
c
c     emineloss, aelos73,...,dplos73 correspond to the e- (or e+)
c     energy loss parameterization in the case komin = 1 MeV, and
c     Aair/Zair = 2.
c
      double precision  emineploss
      double precision  aelos73, belos73, celos73
      double precision  aelosl73, belosl73, celosl73, delosl73
      double precision  aplos73, bplos73, cplos73
      double precision  aplosl73, bplosl73, cplosl73, dplosl73
c
      parameter         (emineploss = 1.660d-3)
      parameter         (aelos73    = 2.223d-3)
      parameter         (belos73    = 10.84d-6)
      parameter         (celos73    = 18.68d-3)
      parameter         (aelosl73   = 1.706d-3)
      parameter         (belosl73   = 5.310d-8)
      parameter         (celosl73   = 1.d-7)
      parameter         (delosl73   = 4.407d-11)
      parameter         (aplos73    = aelos73)
      parameter         (bplos73    = 10.86d-6)
      parameter         (cplos73    = 16.06d-3)
      parameter         (aplosl73   = 1.628d-3)
      parameter         (bplosl73   = 6.107d-8)
      parameter         (cplosl73   = celosl73)
      parameter         (dplosl73   = 5.069d-11)
c
c     Some related derived parameters.
c
      double precision  invkomin
c
      parameter         (invkomin = 1 / komin)
c
c     Parameters for e+e- bremsstrahlung processes.
c
c     ebremegycut0 is the indicative energy threshold for produced
c     secondaries
c     minbremegy0 is the indicative energy threshold for taking these
c     interactions into account.
c
c     The second threshold MUST be greater than the first one. The
c     actual values used are set accordingly with the input directives
c     (see routine modelinirun).
c
      double precision  ebremegycut0, minbremegy0
c
      parameter         (ebremegycut0 = 0.095d-3)
      parameter         (minbremegy0  = 0.100d-3)
c
c     Parameters for positron annihilation process.
c
      double precision  panns1073, panns11, panns12, panne1lim
      double precision  panns2073, panns21, panne2lim
      double precision  panns3073, panns31
      double precision  panne4lim
      double precision  pannv4073, pannv41, pannv42
      double precision  pannv5073, pannv51, pannv52, pannv53, pannv54
c
      parameter         (panns1073 =  10771.d0)
      parameter         (panns11   =  4.1141d-4)
      parameter         (panns12   = -2.6832d-9)
      parameter         (panne1lim =  2.5119d-3)
      parameter         (panns2073 =  4127.1d0)
      parameter         (panns21   =  5.3042d-3)
      parameter         (panne2lim =  0.3162d0)
      parameter         (panns3073 =  1692.1d0)
      parameter         (panns31   =  0.4680d0)
c
      parameter         (pannv4073 = -8.4367d0)
      parameter         (pannv41   = -1.0172d0)
      parameter         (pannv42   = -3.3412d-2)
      parameter         (panne4lim =  0.1000d0)
      parameter         (pannv5073 = -8.1787d0)
      parameter         (pannv51   = -0.8643d0)
      parameter         (pannv52   = -7.3512d-3)
      parameter         (pannv53   = -1.9787d-2)
      parameter         (pannv54   =  3.0000d0)
c
c     Parameters for heavy particle knock-on processes.
c
c     heavymineko is the hard knock-on electrons energy threshold.
c     Formerly it was an input parameter, but it is better to hardwire
c     it in order to make the knock-on/energy loss algorithms more
c     consistent.
c     The value 3 MeV here selected ensures that everything will
c     work well provided that the kinetic energy of the heavy particles
c     is larger than their rest mass (gamma > 2). This parameter should
c     not be larger than 3 MeV.
c
      double precision  heavymineko
c
      parameter         (heavymineko = 0.003d0)
c
c     Parameterization of the mfp correction factor for low energies.
c
c     cf = (hkcfa + (gamma - 2)) /
c          (hkcfb + (gamma - 2) + hkcfc / (gamma - 2))
c
c     (Data for heavymineko = 3 MeV, should be approximately
c     independent of air parameters).
c
      double precision  hkcfa, hkcfb, hkcfc
c
      parameter         (hkcfa = 6.0951d0)
      parameter         (hkcfb = 6.5372d0)
      parameter         (hkcfc = 13.764d0)
c
c     amlos73,...,zmlos73 correspond to the muon energy loss
c     parameterization in the case heavymineko = 3 MeV, and
c     Aair/Zair = 2.
c
      double precision  amlos73, bmlos73, cmlos73, dmlos73, zmlos73
c
      parameter         (amlos73 = 2.25298d-3)
      parameter         (bmlos73 = 1.29427d-3)
      parameter         (cmlos73 = 1.54914d00)
      parameter         (dmlos73 = 5.08991d-5)
      parameter         (zmlos73 = 1.53181d-6)
c
c     Some related derived parameters.
c
      double precision  heavymineko273, uheavymineko
c
      parameter         (heavymineko273 = -13056.767d0 * heavymineko)
      parameter         (uheavymineko   = 1 / heavymineko)
c
c     Parameters for muon bremsstrahlung and muonic pair production
c     processes.
c
c     mubremegycut is the energy threshold for produced secondaries
c     minmubremegy0 is the energy threshold for taking these
c     interactions into account.
c
c     mubremegycut MUST be greater than 4 electron masses, and the
c     second threshold should be at least 250 MeV larger than the
c     first one.
c
      double precision  mubremegycut0, minmubremegy0
c
      parameter         (mubremegycut0 = 0.01d0)
      parameter         (minmubremegy0 = 1.00d0)
c
c     mmfpl0, mbrmfp0,...,mbrmfp3, and mppmfp0,..., mppmfp3 are
c     constants that correspond to the parameterization of mfp(ke)
c     for muon bremsstrahlung and muonic pair production, respectively.
c     The parameterization is as follow:
c
c     u = 1 / (mmfpl0 + log(ke))
c     ln(mfp) = mxxmfp0 + mxxmfp1 * u + mxxmfp2 * u**2 + mxxmfp3 * u**3
c
c     These constants depend on the minmubremegy threshold, and the
c     following set was evaluated considering a threshold of 10 MeV.
c
      double precision  mmfpl0
      double precision  mbrmfp073, mbrmfp1, mbrmfp2, mbrmfp3
      double precision  mppmfp073, mppmfp1, mppmfp2, mppmfp3
c
      parameter         (mmfpl0    =  6.0d0)
c
      parameter         (mbrmfp073 =  9.40430d0)
      parameter         (mbrmfp1   =  17.8068d0)
      parameter         (mbrmfp2   =  37.7672d0)
      parameter         (mbrmfp3   = -120.840d0)
c
      parameter         (mppmfp073 =  0.956997d0)
      parameter         (mppmfp1   =  96.7070d0)
      parameter         (mppmfp2   = -430.555d0)
      parameter         (mppmfp3   =  1698.80d0)
c
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'modelpar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
