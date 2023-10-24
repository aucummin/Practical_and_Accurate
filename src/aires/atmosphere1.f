c
c     FILE: atmosphere1.f                   Creation date: 05/AUG/1996.
c                                       LAST MODIFICATION: 13/MAY/2005.
c
c     Description of the atmosphere. I. Density profile routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine atmosinit(modlabel, atmosname)
c
c     Initialization of the atmospheric parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     modlabel........ (input, integer) Label to switch among
c                      atmospheric models. Current options are:
c                          1. Linsley's standard model.
c                          2. Linsley model for the South Pole.
c     atmosname....... (output, character*(*)) A name for the
c                      atmospheric model, to be typed somewhere.
c                      Maximum length: 42 characters.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           modlabel
      character*(*)     atmosname
c
c     Declaration of parameters and shared data.
c
      include 'atmosdata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, irc
      double precision  ai, bi, ci
      double precision  l2meters
c
c     FIRST EXECUTABLE STATEMENT
c
c     Using J. Linsley's standard atmosphere plus a technical extension
c     that allows the model to work up to altitudes of more than
c     400 km. The model is not completely realistic above 120 km.
c
c                  /  ai + bi exp(- z / ci)        i = 1,...,5
c          X(z) = <
c                  \  ai - bi z / ci               i = 6
c
c     For each layer (i), there exist boundaries zlim(i-1), zlim(i).
c     For z > zlim(5) X is zero.
c
c     Setting internal arrays for altitude versus depth functions.
c     Switching between available parameter settings.
c
      if (modlabel .eq. 1) then
c
        atmosname = 'Extended Linsley''s std. atmosphere.'
c
        alaylimz(0) = l2meters(0.0d0, 'km', irc)
c
c       Parameters ai and bi are expressed in g/cm2.
c
c       Standard atmosphere, layer 1:
c
        ai          = -186.5562d0
        bi          = 1222.6562d0
        ci          = l2meters(994186.38d0, 'cm', irc)
        alaylimz(1) = l2meters(4.0d0, 'km', irc)
c
        alaya1(1)   = ai
        alayb1(1)   = bi
        alayd1(1)   = -1.d0 / ci
c
c       Standard atmosphere, layer 2:
c
        ai          =  -94.9199d0
        bi          = 1144.9069d0
        ci          = l2meters(878153.55d0, 'cm', irc)
        alaylimz(2) = l2meters(10.0d0, 'km', irc)
c
        alaya1(2)   = ai
        alayb1(2)   = bi
        alayd1(2)   = -1.d0 / ci
c
c       Standard atmosphere, layer 3:
c
        ai          =    0.61289d0
        bi          = 1305.59480d0
        ci          = l2meters(636143.04d0, 'cm', irc)
        alaylimz(3) = l2meters(40.0d0, 'km', irc)
c
        alaya1(3)   = ai
        alayb1(3)   = bi
        alayd1(3)   = -1.d0 / ci
c
c       Standard atmosphere, layer 4:
c
        ai          =    0.0000d0
        bi          =  540.1778d0
        ci          = l2meters(772170.16d0, 'cm', irc)
        alaylimz(4) = l2meters(100.0d0, 'km', irc)
c
        alaya1(4)   = ai
        alayb1(4)   = bi
        alayd1(4)   = -1.d0 / ci
c
c       Standard atmosphere, layer 5:
c
        ai          =  1.0469564d-10
        bi          =  540.17833d0
        ci          = l2meters(772170.097d0, 'cm', irc)
        alaylimz(5) = l2meters(250.0d0, 'km', irc)
c
        alaya1(5)   = ai
        alayb1(5)   = bi
        alayd1(5)   = -1.d0 / ci
c
c       Standard atmosphere, layer 6:
c
        ai          =  2.614186472d-10
        bi          =  1.000000000d0
        ci          = l2meters(1.64444076d17, 'cm', irc)
        alaylimz(6) = l2meters(429.8874788797d00, 'km', irc)
c
        alaya1(6)   = ai
        alayd1(6)   = -bi / ci
c
        nlayers     = 6
c
      else if (modlabel .eq. 2) then
c
        atmosname = 'Linsley''s atmosphere (South Pole)'
c
        alaylimz(0) = l2meters(0.0d0, 'km', irc)
c
c       Parameters ai and bi are expressed in g/cm2.
c
c       South Pole atmosphere, layer 1:
c
        ai          =  354.2272d0
        bi          =  819.8323d0
        ci          = l2meters(318904.5d0, 'cm', irc)
        alaylimz(1) = l2meters(3219.4633d0, 'm', irc)
c
        alaya1(1)   = ai
        alayb1(1)   = bi
        alayd1(1)   = -1.d0 / ci
c
c       South Pole atmosphere, layer 2:
c
        ai          =  -76.9025d0
        bi          = 1103.3958d0
        ci          = l2meters(779084.29d0, 'cm', irc)
        alaylimz(2) = l2meters(4918.2742d0, 'm', irc)
c
        alaya1(2)   = ai
        alayb1(2)   = bi
        alayd1(2)   = -1.d0 / ci
c
c       South Pole atmosphere, layer 3:
c
        ai          =  -83.0366d0
        bi          = 1107.6716d0
        ci          = l2meters(787227.02d0, 'cm', irc)
        alaylimz(3) = l2meters(8237.087d0, 'm', irc)
c
        alaya1(3)   = ai
        alayb1(3)   = bi
        alayd1(3)   = -1.d0 / ci
c
c       South Pole atmosphere, layer 4:
c
        ai          =    -2.5096d0
        bi          =  1154.2522d0
        ci          = l2meters(624278.25d0, 'cm', irc)
        alaylimz(4) = l2meters(19.173767d0, 'km', irc)
c
        alaya1(4)   = ai
        alayb1(4)   = bi
        alayd1(4)   = -1.d0 / ci
c
c       South Pole atmosphere, layer 5:
c
        ai          = 51.00191738d0
        bi          =  1.00000000d0
        ci          = l2meters(1.0d9, 'cm', irc)
        alaylimz(5) = l2meters(510019.1738d0, 'km', irc)
c
        alaya1(5)   = ai
        alayd1(5)   = -bi / ci
c
        nlayers     = 5
c
      else
c
c       Invalid model label.
c
        call errprint(0, '*', 4, 'atmosinit',
     +                'Invalid atmospheric model label.',
     +                1, modlabel, 0, 0.d0, ' ')
      endif
c
c     Checking model parameters-
c
      if (nlayers .gt. mxlayers) then
        call errprint(0, '*', 4, 'atmosinit',
     +                'Too many layers defined for atmospheric model.',
     +                1, nlayers, 0, 0.d0, ' ')
      endif
c
      nlayers1  = nlayers - 1
      nlayersp1 = nlayers + 1
c
c     For z < 0 the medium is a ficticious medium of density 10 kg/cm3
c     This zone is labelled "zone 0", and the following parameters are
c     to evaluate the depth in this zone.
c
      alaya1(0) = alaya1(1) + alayb1(1)
      alayd1(0) = -1.0d6
c
c     Arrays for inverse function (z from X):
c
      alaylimx(0) = alaya1(1) + alayb1(1)
      alaya2(0)   = - alaya1(0) / alayd1(0)
      alayd2(0)   = 1.d0 / alayd1(0)
c
      do i = 1, nlayers1
c
        alaylimx(i) = alaya1(i) +
     +                alayb1(i) * exp(alayd1(i) * alaylimz(i))
        alaya2(i)   = - alaya1(i) / alayb1(i)
        alayb2(i)   = 1.d0 / alayb1(i)
        alayd2(i)   = 1.d0 / alayd1(i)
c
      enddo
c
      alaylimx(nlayers) = 0
      alaya2(nlayers)   = - alaya1(nlayers) / alayd1(nlayers)
      alayd2(nlayers)   = 1.d0 / alayd1(nlayers)
c
      return
c
      end
c     --- End of routine atmosinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function depthfromz(z, layer)
c
c     Atmospheric depth from vertical altitude. Multilayer atmospheric
c     model.
c     The altitude is measured vertically in meters above sea level
c     (z=0), and the atmospheric depth is given in g/cm2.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     z............... (input, double precision) The vertical altitude
c                      in meters a.s.l.
c     layer........... (output, integer) An integer ranging from zero
c                      to nlayers + 1 indicating the atmospheric layer
c                      corresponding to the given z. Layer 0 is
c                      returned when z is negative, and layer
c                      nlayers + 1 is returned when z is greater than
c                      the maximum significative altitude (top of
c                      atmosphere).
c
c
c     Return value: (double precision) The atmospheric depth (g/cm2).
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  depthfromz
      double precision  z
      integer           layer
c
c     Declaration of parameters and shared data.
c
      include 'atmosdata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      if (z .le. alaylimz(0)) goto 1020
      do i = 1, nlayers1
        layer = i
        if (z .le. alaylimz(i)) goto 1010
      enddo
      layer = nlayers
      if (z .le. alaylimz(nlayers)) goto 1030
c
c     z too high.
c
      layer      = nlayersp1
      depthfromz = -1.d-34
      return
c
c     Intermediate layers.
c
 1010 continue
      depthfromz = alaya1(layer) +
     +             alayb1(layer) * exp(alayd1(layer) * z)
      return
c
c     Top layer or underground (z < 0).
c
 1020 continue
      layer = 0
 1030 continue
      depthfromz = alaya1(layer) + alayd1(layer) * z
      return
c
      end
c     --- End of routine depthfromz
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function zfromdepth(depth, layer)
c
c     Altitude from atmospheric depth. Multilayer atmospheric model.
c     The altitude is measured in meters above sea level (z=0), and the
c     atmospheric depth is given in g/cm2.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     depth........... (input, double precision) Atmospheric depth in
c                      g/cm2.
c     layer........... (output, integer) An integer ranging from zero
c                      to nlayers + 1 indicating the atmospheric layer
c                      corresponding to the given depth. Layer 0 is
c                      returned when the altitude is negative, and
c                      layer nlayers + 1 is returned when depth is
c                      negative (no physical meaning).
c
c
c     Return value: (double precision) The vertical altitude in meters
c     ============  a.s.l.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  zfromdepth
      double precision  depth
      integer           layer
c
c     Declaration of parameters and shared data.
c
      include 'atmosdata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      if (depth .ge. alaylimx(0)) goto 1020
      do i = 1, nlayers1
        layer = i
        if (depth .ge. alaylimx(i)) goto 1010
      enddo
      layer = nlayers
      if (depth .ge. alaylimx(nlayers)) goto 1030
c
c     Negative depth (unphysical)
c
      layer      = nlayersp1
      zfromdepth = alaylimz(nlayers) + 1.d-5
      return
c
c     Intermediate layers.
c
 1010 continue
      zfromdepth = alayd2(layer) *
     +               log(alaya2(layer) + alayb2(layer) * depth)
      return
c
c     Top layer or underground (z < 0).
c
 1020 continue
      layer = 0
 1030 continue
      zfromdepth = alaya2(layer) + alayd2(layer) * depth
      return
c
      end
c     --- End of routine zfromdepth
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function adstymdepth(depth, layer)
c
c     Local density at a given depth. Multilayer atmospheric model.
c     The atmospheric depth is measured in g/cm2, and the density in
c     g/(m cm2).
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     depth........... (input, double precision) Atmospheric depth in
c                      g/cm2.
c     layer........... (output, integer) An integer ranging from zero
c                      to nlayers + 1 indicating the atmospheric layer
c                      corresponding to the given depth. Layer 0 is
c                      returned when the altitude is negative, and
c                      layer nlayers + 1 is returned when depth is
c                      negative (no physical meaning).
c
c
c     Return value: (double precision) The density in g/(m cm2).
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  adstymdepth
      double precision  depth
      integer           layer
c
c     Declaration of parameters and shared data.
c
      include 'atmosdata.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      if (depth .ge. alaylimx(0)) goto 1020
      do i = 1, nlayers1
        layer = i
        if (depth .ge. alaylimx(i)) goto 1010
      enddo
      layer = nlayers
      if (depth .ge. alaylimx(nlayers)) goto 1030
c
c     Negative depth (unphysical)
c
      layer       = nlayersp1
      adstymdepth = 0
      return
c
c     Intermediate layers.
c
 1010 continue
      adstymdepth = alayd1(layer) * (alaya1(layer) - depth)
      return
c
c     Top layer or underground (z < 0).
c
 1020 continue
      layer = 0
 1030 continue
      adstymdepth = - alayd1(layer)
      return
c
      end
c     --- End of routine adstymdepth
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function adstydepth(depth, layer)
c
c     Local density at a given depth. Multilayer atmospheric model.
c     The atmospheric depth is measured in g/cm2, and the density in
c     g/cm3.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     depth........... (input, double precision) Atmospheric depth in
c                      g/cm2.
c     layer........... (output, integer) An integer ranging from zero
c                      to nlayers + 1 indicating the atmospheric layer
c                      corresponding to the given depth. Layer 0 is
c                      returned when the altitude is negative, and
c                      layer nlayers + 1 is returned when depth is
c                      negative (no physical meaning).
c
c
c     Return value: (double precision) The density in g/cm3.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  adstydepth
      double precision  depth
      integer           layer
c
c     Declaration of internal variables and arrays.
c
      double precision  adstymdepth
c
c     FIRST EXECUTABLE STATEMENT
c
      adstydepth = adstymdepth(depth, layer) / 100
c
      return
      end
c     --- End of routine adstydepth
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'atmosphere1.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
