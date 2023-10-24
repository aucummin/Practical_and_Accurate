c
c     FILE: hmfphighedata.f                 Creation date: 02/JUL/2000.
c                                       LAST MODIFICATION: 02/JAN/2006.
c
c     This file contains the definitions of the internal arrays needed
c     for the parameterizations of the high energy hadronic mean free
c     paths.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2002, 2005, 2006.
c
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
c
c     Coefficients for MFP parameterization. All MFP's can be
c     parameterized in the form
c
c                                            2         3
c                         1 + p2 * u + p3 * u  + p4 * u
c     mfp(Egy) = p1 ------------------------------------------
c                                       2         3         4
c                    1 + p5 * u + p6 * u  + p7 * u  + p8 * u
c
c     where u = ln(Egy [GeV]) , 0.1 GeV < Egy < 1e13 GeV.
c
      double precision  mfp8(8, totmfphsets, 3)
c
c     SIBYLL 2.1 MFP's. Nucleons.
c
      data              mfp8(1, 1, 1) /  91.88910D+00 /
      data              mfp8(2, 1, 1) /  0.151165D+00 /
      data              mfp8(3, 1, 1) /  3.772280D-03 /
      data              mfp8(4, 1, 1) /  5.257280D-04 /
      data              mfp8(5, 1, 1) /  0.158422D+00 /
      data              mfp8(6, 1, 1) /  5.667400D-03 /
      data              mfp8(7, 1, 1) /  6.114430D-04 /
      data              mfp8(8, 1, 1) /  5.279360D-05 /
c
c     SIBYLL 2.1 MFP's. Pions.
c
      data              mfp8(1, 2, 1) /  120.8190D+00 /
      data              mfp8(2, 2, 1) / -9.678140D-02 /
      data              mfp8(3, 2, 1) /  3.819630D-03 /
      data              mfp8(4, 2, 1) /  1.436680D-05 /
      data              mfp8(5, 2, 1) / -9.067670D-02 /
      data              mfp8(6, 2, 1) /  4.476070D-03 /
      data              mfp8(7, 2, 1) / -7.579610D-05 /
      data              mfp8(8, 2, 1) /  8.278620D-06 /
c
c     SIBYLL 2.1 MFP's. Kaons.
c
      data              mfp8(1, 3, 1) /  164.7640D+00 /
      data              mfp8(2, 3, 1) / -0.156905D+00 /
      data              mfp8(3, 3, 1) /  5.344300D-03 /
      data              mfp8(4, 3, 1) /  2.194020D-04 /
      data              mfp8(5, 3, 1) / -0.121606D+00 /
      data              mfp8(6, 3, 1) /  4.898220D-03 /
      data              mfp8(7, 3, 1) / -5.733830D-04 /
      data              mfp8(8, 3, 1) /  5.976840D-05 /
c
c     QGSJET 2001c MFP's. Nucleons.
c
      data              mfp8(1, 1, 2) /  99.88580D+00 /
      data              mfp8(2, 1, 2) / -0.568687D+00 /
      data              mfp8(3, 1, 2) /  0.238096D+00 /
      data              mfp8(4, 1, 2) /  1.427790D-02 /
      data              mfp8(5, 1, 2) / -0.566968D+00 /
      data              mfp8(6, 1, 2) /  0.231727D+00 /
      data              mfp8(7, 1, 2) /  1.427030D-02 /
      data              mfp8(8, 1, 2) /  1.260040D-03 /
c
c     QGSJET 2001c MFP's. Pions.
c
      data              mfp8(1, 2, 2) /  132.6440D+00 /
      data              mfp8(2, 2, 2) /  0.435383D+00 /
      data              mfp8(3, 2, 2) /  9.597290D-02 /
      data              mfp8(4, 2, 2) /  8.837220D-03 /
      data              mfp8(5, 2, 2) /  0.431011D+00 /
      data              mfp8(6, 2, 2) /  9.590170D-02 /
      data              mfp8(7, 2, 2) /  1.159190D-02 /
      data              mfp8(8, 2, 2) /  8.922080D-04 /
c
c     QGSJET 2001c MFP's. Kaons.
c
      data              mfp8(1, 3, 2) /  162.5580D+00 /
      data              mfp8(2, 3, 2) / -0.260061D+00 /
      data              mfp8(3, 3, 2) /  0.412872D+00 /
      data              mfp8(4, 3, 2) /  2.437260D-02 /
      data              mfp8(5, 3, 2) / -0.259622D+00 /
      data              mfp8(6, 3, 2) /  0.398771D+00 /
      data              mfp8(7, 3, 2) /  2.483960D-02 /
      data              mfp8(8, 3, 2) /  3.441800D-03 /
c
c     QGSJET II MFP's. Nucleons.
c
      data              mfp8(1, 1, 3) /  98.56440D+00 /
      data              mfp8(2, 1, 3) / -0.858562D+00 /
      data              mfp8(3, 1, 3) /  0.772618D+00 /
      data              mfp8(4, 1, 3) /  3.826240D-02 /
      data              mfp8(5, 1, 3) / -0.855212D+00 /
      data              mfp8(6, 1, 3) /  0.756253D+00 /
      data              mfp8(7, 1, 3) /  3.871770D-02 /
      data              mfp8(8, 1, 3) /  3.936590D-03 /
c
c     QGSJET II MFP's. Pions.
c
      data              mfp8(1, 2, 3) /  130.7760D+00 /
      data              mfp8(2, 2, 3) /  0.359111D+00 /
      data              mfp8(3, 2, 3) / -3.709470D-03 /
      data              mfp8(4, 2, 3) / -7.806770D-05 /
      data              mfp8(5, 2, 3) /  0.359995D+00 /
      data              mfp8(6, 2, 3) /  8.807480D-04 /
      data              mfp8(7, 2, 3) /  1.607340D-03 /
      data              mfp8(8, 2, 3) / -4.419730D-05 /
c
c     QGSJET II MFP's. Kaons.
c
      data              mfp8(1, 3, 3) /  147.4500D+00 /
      data              mfp8(2, 3, 3) / -3.541800D-02 /
      data              mfp8(3, 3, 3) / -1.088490D-03 /
      data              mfp8(4, 3, 3) /  1.128130D-04 /
      data              mfp8(5, 3, 3) / -3.381110D-02 /
      data              mfp8(6, 3, 3) /  3.146730D-03 /
      data              mfp8(7, 3, 3) / -1.878860D-04 /
      data              mfp8(8, 3, 3) /  1.124380D-05 /
c
c
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
cu---uu---uu---uu---uu---uu---uu---u*u---uu---uu---uu---uu---uu---uu---u
c
c     End of file 'hmfphighedata.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
