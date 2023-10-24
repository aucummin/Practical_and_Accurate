c
c     FILE: libucomm.f                      Creation date: 07/MAR/2002
c                                       LAST MODIFICATION: 19/SEP/2002.
c
c     Some shared variables used within AIRES library utilities.
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Internal variables to keep information about already read-in
c     records (routine getlgtrecord and related).
c
      integer           mxlgtdata
      parameter         (mxlgtdata = 48)
c
      logical           mustread, mustmodify, needmupd
      integer           svnol, svcurrol
      integer           svfirstol, svlastol, svupdown
      integer           svcrool(mxobslevels)
c
      common            /getlgt_c0/ mustread, mustmodify, needmupd,
     +                              svnol, svcurrol,
     +                              svfirstol, svlastol, svupdown,
     +                              svcrool
c
      integer           nlgtdata
      integer           svintfields(mxlgtdata)
      double precision  svfltfields(mxlgtdata)
c
      common            /getlgt_c1/ svfltfields, svintfields,
     +                              nlgtdata
c
      integer           idxolkey, idxx, idxy, idxt, idxux, idxuy
      integer           idxlegy, idxpcode, idxprimzen, idxprimazi
      logical           uxyfld, betafld
c
      common            /getlgt_c2/ idxolkey, idxx, idxy, idxt,
     +                              idxux, idxuy, idxlegy, idxpcode,
     +                              idxprimzen, idxprimazi,
     +                              uxyfld, betafld
c
      double precision  svolzv(mxobslevels)
      double precision  svx0(mxobslevels)
      double precision  svy0(mxobslevels)
      double precision  svz0(mxobslevels)
      double precision  svt0(mxobslevels)
      double precision  svmx(mxobslevels)
      double precision  svmy(mxobslevels)
      integer           svtotol
c
      common            /getlgt_c3/ svolzv, svx0, svy0, svz0, svt0,
     +                              svmx, svmy, svtotol
c
      double precision  svgroundz, svinjz
      double precision  shux, shuy, shuz
c
      common            /getlgt_c4/ svgroundz, svinjz,
     +                              shux, shuy, shuz
c
      double precision  svux, svuy, svuz, svuv
      double precision  svax, svay, svdt, svzp
c
      common            /getlgt_c5/ svux, svuy, svuz, svuv,
     +                              svax, svay, svdt, svzp
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'libucomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
