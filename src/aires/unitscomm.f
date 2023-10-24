c
c     FILE: unitscomm.f                     Creation date: 25/JUN/1996.
c                                       LAST MODIFICATION: 30/SEP/1997.
c
c     This file contains the physical units related definitions.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Length units.
c
      character*2       luname(nlunits)
      double precision  lconvtomt(0:nlunits), lconvfrommt(0:nlunits)
      double precision  lnicemax(0:nlunits)
      integer           lnicedefunit
c
      common            /lunits_c/ lconvtomt, lconvfrommt,
     +                             lnicemax, lnicedefunit,
     +                             luname
c
c     Time units.
c
      character*3       tuname(ntunits)
      double precision  tconvtosec(0:ntunits), tconvfromsec(0:ntunits)
      double precision  tconvtons(0:ntunits), tconvfromns(0:ntunits)
c
      common            /tunits_c/ tconvtosec, tconvfromsec,
     +                             tconvtons, tconvfromns,
     +                             tuname
c
c     Angle units.
c
      character*3       auname(naunits)
      double precision  aconvtodeg(0:naunits), aconvfromdeg(0:naunits)
c
      common            /aunits_c/ aconvtodeg, aconvfromdeg,
     +                             auname
c
c     Magnetic field units.
c
      character*2       buname(nbunits)
      double precision  bconvtont(0:nbunits), bconvfromnt(0:nbunits)
      double precision  bnicemax(0:nbunits)
      integer           bnicedefunit
c
      common            /bunits_c/ bconvtont, bconvfromnt,
     +                             bnicemax, bnicedefunit,
     +                             buname
c
c     Energy units.
c
      character*3       euname(neunits)
      double precision  econvtogev(0:neunits), econvfromgev(0:neunits)
      double precision  enicemax(0:neunits)
      integer           enicedefunit
c
      common            /eunits_c/ econvtogev, econvfromgev,
     +                             enicemax, enicedefunit,
     +                             euname
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'unitscomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
