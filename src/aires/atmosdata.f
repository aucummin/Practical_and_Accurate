c
c     FILE: atmosdata.f                     Creation date: 05/AUG/1996.
c                                       LAST MODIFICATION: 13/MAY/2005.
c
c     Parameters and shared data for the atmospheric model.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2003, 2005.
c
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
c
c     Parameters.
c
c     Number of layers for the density profile model.
c
      integer           mxlayers
      parameter         (mxlayers = 6)
c
c     Parameters for nuclear mass composition.
c     Using only the three most abundant elements: N, O, and Ar.
c
      integer           anuca1, anuca2, anuca3
      double precision  frnuc1, frnuc2, frnuc3
c
      parameter         (anuca1 = 14, frnuc1 = 0.7847d0)
      parameter         (anuca2 = 16, frnuc2 = 0.2105d0)
      parameter         (anuca3 = 40, frnuc3 = 0.0047d0)
c
c     The cross section factors correspond to A^0.91
c
      double precision  anuca1f, anuca2f, anuca3f
c
      parameter         (anuca1f = 11.04019d0)
      parameter         (anuca2f = 12.46663d0)
      parameter         (anuca3f = 28.69952d0)
c
c     Derived parameters for mass composition.
c
      double precision  totnucfr, xfrnuc1, xfrnuc2, xfrnuc3
      double precision  cfrnuc1, cfrnuc2
c
      parameter         (xfrnuc1  = frnuc1 * anuca1f)
      parameter         (xfrnuc2  = frnuc2 * anuca2f)
      parameter         (xfrnuc3  = frnuc3 * anuca3f)
      parameter         (totnucfr = xfrnuc1 + xfrnuc2 + xfrnuc3)
      parameter         (cfrnuc1  = xfrnuc1 / totnucfr)
      parameter         (cfrnuc2  = (xfrnuc1 + xfrnuc2) / totnucfr)
c
c
c     Shared data.
c
      integer           nlayers, nlayers1, nlayersp1
c
      double precision  alaylimz(0:mxlayers)
      double precision  alaylimx(0:mxlayers)
      double precision  alaya1(0:mxlayers)
      double precision  alayb1(0:mxlayers)
      double precision  alayd1(0:mxlayers)
      double precision  alaya2(0:mxlayers)
      double precision  alayb2(0:mxlayers)
      double precision  alayd2(0:mxlayers)
c
      common            /atmosd_c/ alaylimz, alaya1, alayb1, alayd1,
     +                             alaylimx, alaya2, alayb2, alayd2,
     +                             nlayers, nlayers1, nlayersp1
c
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
cr---rr---rr---rr---rr---rr---rr---r*r---rr---rr---rr---rr---rr---rr---r
c
c     End of file 'atmosdata.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
