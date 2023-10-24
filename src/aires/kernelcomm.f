c
c     FILE: kernelcomm.f                    Creation date: 16/JUL/1996.
c                                       LAST MODIFICATION: 16/JUL/2003.
c
c     This file contains the definitions of the kernel shared
c     variables.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Particle stack names and definition data.
c
      character*42      psta_n(0:npstacks), psta_model_n(npstacks)
      character*16      psta_rn(2, npstacks)
      integer           minpstack
      integer           psta_mode(npstacks)
c
      common            /psta_names_c/ psta_rn, minpstack,
     +                                 psta_n, psta_model_n
      common            /psta_mode_c/ psta_mode
c
c     Stopping files and related variables.
c
      integer           nstopfiles
      character*168     stopfile(4)
c
      common            /stopfiles_c/ nstopfiles, stopfile
c
      integer           stopchsize1, stopchsize2
      double precision  dstime, stopchsize3
c
      common            /stopdata_c/ dstime, stopchsize3,
     +                               stopchsize2, stopchsize1
c
c     Stack statistics and related variables.
c
      double precision  avgtotsize(npstacks)
      double precision  procentries(3, npstacks)
      integer           peakstsize(3, npstacks)
      integer           hardswrite(3, npstacks)
      integer           hardsread(3, npstacks)
      integer           callcounter(2, 0:npstacks)
c
      common            /stacksta_c/avgtotsize, procentries,
     +                              hardswrite, hardsread,
     +                              peakstsize, callcounter
c
c     Particle and energy counters.
c
      double precision  nplost(5, 0:npstacks), elost(5, 0:npstacks)
      double precision  nplowe(5, 0:npstacks), elowe(5, 0:npstacks)
      double precision  nprgnd(5, 0:npstacks), eprgnd(5, 0:npstacks)
      double precision  totpcles(5, 0:npstacks)
      double precision  eloss(5, 0:npstacks)
      double precision  nnotap(5), enotap(5)
      double precision  nneutrino(5), eneutrino(5)
      double precision  aveprim(5)
c
      common            /pclests_c1/ nplost, elost, nplowe, elowe,
     +                               nprgnd, eprgnd, totpcles,
     +                               eloss, nnotap, enotap,
     +                               nneutrino, eneutrino, aveprim
c
c     Quantities related with the shower geometry, used to determine
c     the status of "lost particle".
c
      double precision  groundz00, groundz00sq, injz00, iovgz00sq
      double precision  dtoplost, hcosinedummy
      double precision  hrmaxlx, hrmaxly, hrminlx, hrminly
c
      common            /shbox_c/ groundz00, groundz00sq,
     +                            injz00, iovgz00sq,
     +                            dtoplost, hcosinedummy,
     +                            hrmaxlx, hrmaxly,
     +                            hrminlx, hrminly
c
c     Longitudinal development internal quantities.
c
      double precision  obslevstep
      double precision  obslevminz, obslevmaxz
      double precision  obslevca, obslevcb
      integer           obslevl0, obslevlmax, totobslev
c
      common            /obslev_ic1/ obslevstep, obslevminz,
     +                               obslevmaxz, obslevca, obslevcb,
     +                               obslevl0, obslevlmax, totobslev
c
c     Geomagnetic field internal quantities.
c
c     Components of the input magnetic field.
c
      double precision  igbx, igbz, igbfluc
c
      common            /kgeomag_c0/ igbx, igbz, igbfluc
c
c     Internal variables to use during the simulations.
c
      double precision  shgbx, shgbz, emagfac
      logical           shgbon
c
      common            /kgeomag_c1/ shgbx, shgbz, emagfac, shgbon
c
      double precision  facgbx, facgbz
c
      common            /kgoemag_c2/ facgbx, facgbz
c
c     Internal quantities for special primaries.
c
c     specialprim is a logical switch. True if the primary is a
c     special particle.
c
c     nspecialprim is an integer variable with the number of
c     primary particles for the shower.
c
c     specialprimlab is the label of the current special particle.
c
c     nintintvar, nintfltvar, spintvar, spfltvar are variables related
c     with variables passed to and from the external modules.
c
      logical           specialprim, recordspvar0
      integer           specialprimlab, nspecialprim
      integer           nintintvar, nintfltvar
      integer           spintvar(mxintintvar)
      double precision  spfltvar(mxintfltvar)
c
      common            /kernelspecial_c/ spfltvar, spintvar,
     +                                    nintintvar, nintfltvar,
     +                                    specialprim, recordspvar0,
     +                                    specialprimlab, nspecialprim
c
c     Miscellaneous.
c
c     notproprim is a logical switch. True if the primary must not
c     be propagated at the very beginning of a shower.
c
c     resetclock is a logical switch. True if the clock must be
c     reset at the first interaction.
c
c     resetdtoplost is a logical switch. True if the top surface of the
c     shower bounding box must be reset accordingly with the depth of
c     the first interaction.
c
c     fstintnotset, fstposdp, fstdpmanual fstintnotset and fstintauto
c     are internal variables related with the first interaction depth.
c
c     fstintdp contains statistical data related with the first
c     interaction depth.
c
c     currstack is a shared position to keep track of the stack
c     being processed.
c
c     prycoszmin, prycoszmax are the cosine of the min and max zenith
c     angle, respectively, eventually multiplied by 2 to give a sin cos
c     distribution. prycosfac is the factor needed to reobtain the
c     angle in sexagesimal degrees.
c
      double precision  fstintdp(5, 2)
      double precision  fstposdp(5), fstdpmanual
      double precision  prycoszmin, prycoszmax, prycosfac
      logical           notproprim, resetclock, resetdtoplost
      logical           fstintnotset, fstintauto
      integer           currstack
c
      common            /kernelmisc_c/ fstintdp, fstposdp, fstdpmanual,
     +                                 prycoszmin, prycoszmax,
     +                                 prycosfac,
     +                                 notproprim, resetclock,
     +                                 resetdtoplost,
     +                                 fstintnotset, fstintauto,
     +                                 currstack
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'kernelcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
