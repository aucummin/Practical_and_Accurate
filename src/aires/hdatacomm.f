c
c     FILE: hdatacomm.f                     Creation date: 17/JUL/1996.
c                                       LAST MODIFICATION: 23/NOV/2001.
c
c     This file contains the definitions of the histogram and output
c     variables.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2001.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Observing levels related data.
c
      integer           iobslevdat(0:1)
      double precision  fobslevdat(0:1, 2)
      integer           obslevset
      logical           obszset(2)
c
      common            /obslev_c1/ fobslevdat, iobslevdat,
     +                              obslevset, obszset
c
c     Equivalencing to simplify usage.
c
      integer           nobslevels
      double precision  obslevmind, obslevmaxd
c
      equivalence       (nobslevels, iobslevdat(0))
      equivalence       (obslevmind, fobslevdat(0, 1))
      equivalence       (obslevmaxd, fobslevdat(0, 2))
c
c     Positions of the different observing levels, and related data.
c
      double precision  obslevdepth(mxobslevelsp1)
      double precision  obslevz(mxobslevelsp1)
      double precision  obslevz00sq(mxobslevelsp1)
      double precision  olrminfile(mxobslevelsp1, 0:2)
c
      common            /obslev_cd/ obslevdepth, obslevz, obslevz00sq,
     +                              olrminfile
c
c     Other observing level data.
c
      integer           nobslevelsp1
c
      common            /obslev_c2/ nobslevelsp1
c
c     Observing levels to be saved into longitudinal files.
c
      logical           olfilesave(mxobslevelsp1)
c
      common            /obslev_f1/ olfilesave
c
c     Scratch variables and arrays for longitudinal calculations.
c
      double precision  obslevscra0(mxobslevelsp1, 9)
c
      common            /obslscr/ obslevscra0
c
c     Single precision and integer equivalents.
c
      real              obslevscra4(0:mxobslevelsp1 * 4 - 1)
      equivalence       (obslevscra0, obslevscra4)
      integer           obslevscrai(0:mxobslevelsp1 * 4 - 1)
      equivalence       (obslevscra0(1, 3), obslevscrai)
      integer           obslevscraj(0:mxobslevelsp1 * 4 - 1)
      equivalence       (obslevscra0(1, 5), obslevscraj)
c
      double precision  obslevscra1(mxobslevelsp1)
      equivalence       (obslevscra1, obslevscra0(1, 7))
c
c     Table (histogram) arrays and related quantities.
c
      integer           nlhtables, nldtables, ntdtables, npshtables
      integer           nlhtable1, nldtable1, ntdtable1, npshtable1
      integer           maxtabcode, nttabins, nttabinsp1, nttabinsp2
      integer           lhcoden(mxlhtable)
      integer           lhcodee(mxlhtable)
      integer           ldcoden(mxldtable)
      integer           ldcodee(mxldtable)
      integer           tdcoden(mxtdtable)
      integer           pshcode(mxpshtable)
c
      common            /tables_c0/
     +                    nlhtables, nldtables, ntdtables, npshtables,
     +                    nlhtable1, nldtable1, ntdtable1, npshtable1,
     +                    maxtabcode, nttabins, nttabinsp1, nttabinsp2,
     +                    lhcoden, lhcodee,
     +                    ldcoden, ldcodee,
     +                    tdcoden, pshcode
c
      integer           wlhcoden(mxlhtable)
      integer           wldcoden(mxldtable)
      integer           wldcodee(mxldtable)
c
      common            /tables_c0a/ wlhcoden, wldcoden, wldcodee
c
      character*64      lhnamen(mxlhtable)
      character*64      lhnamee(mxlhtable)
      character*64      ldnamen(mxldtable)
      character*64      ldnamee(mxldtable)
      character*64      tdnamen(mxtdtable)
      character*64      pshname(mxpshtable)
c
      common            /tables_c1/ lhnamen, lhnamee,
     +                              ldnamen, ldnamee,
     +                              tdnamen, pshname
c
      character*64      wlhnamen(mxlhtable)
      character*64      wldnamen(mxldtable)
      character*64      wldnamee(mxldtable)
c
      common            /tables_c1a/ wlhnamen, wldnamen, wldnamee
c
      integer           nlitables
      integer           nlitable1
      integer           llcoden(mxlitable)
      integer           llcodee(mxlitable)
      integer           licodee(mxlitable)
c
      common            /tables_ci0/
     +                    nlitables, nlitable1,
     +                    llcoden, llcodee, licodee
c
      integer           wllcoden(mxlitable)
c
      common            /tables_ci0a/ wllcoden
c
      character*64      llnamen(mxlitable)
      character*64      llnamee(mxlitable)
      character*64      linamee(mxlitable)
c
      common            /tables_ci1/ llnamen, llnamee, linamee
c
      character*64      wllnamen(mxlhtable)
c
      common            /tables_ci1a/ wllnamen
c
c     Longitudinal histograms.
c
      double precision  lhistn(5, mxobslevelsp1, mxlhtable2)
      double precision  lhiste(5, mxobslevelsp1, mxlhtable2)
c
      common            /longitable_cn/ lhistn
      common            /longitable_ce/ lhiste
c
      double precision  wlhistn(5, mxobslevelsp1, mxlhtable2)
c
      common            /longitable_cwn/ wlhistn
c
c     Names for particles or groups associated with the longitudinal
c     histograms.
c
      character*16      lhpclen(mxlhtable)
c
      common            /longitable_cp/ lhpclen
c
c     Auxiliary data for printing ground particles.
c
      integer           grdporder(mxlhtable), grdpspa(mxlhtable)
      integer           ngrdprint
c
      common            /longitable_cor/ grdporder, grdpspa, ngrdprint
c
c     Shower maximum related data.
c
      double precision  shxmax(5, mxxmaxd)
      double precision  shnmax(5)
      integer           shxsamples, lallcht
c
      common            /showerx/ shxmax, shnmax, shxsamples, lallcht
c
c     Transversal histograms.
c
      double precision  rhn0, factrthn, rhni0, factrthni
      double precision  rthistn(5, 0:tdbinsp1, mxldtable)
      double precision  wrthistn(5, 0:tdbinsp1, mxldtable)
      double precision  rhe0, factrthe, rhei0, factrthei
      double precision  rthiste(5, 0:tdbinsp1, mxldtable)
      double precision  wrthiste(5, 0:tdbinsp1, mxldtable)
c
      common            /ldtable_cn/ rhn0, factrthn,
     +                               rhni0, factrthni,
     +                               rthistn, wrthistn
      common            /ldtable_ce/ rhe0, factrthe,
     +                               rhei0, factrthei,
     +                               rthiste, wrthiste
c
c     Time distribution histograms.
c
      double precision  rthistt(5, 0:tdbinsp1, mxtdtable)
      integer           rtsampl(0:tdbinsp1, mxtdtable)
c
      common            /tdtable_ct/ rthistt, rtsampl
c
c     Data for print and export control.
c     Here 1 corresponds to print and 2 to export.
c
      logical           saveshowerdata, pershowertables 
      integer           npexpopt(2), npexpoplen(2)
      integer           pexpoptidx(24, 2), pexpopequiv(24, 2)
      integer           pexpdefopt(mxpexopt, 2)
      character*24      pexpopstr(2)
c
      common            /tprint_c0/ saveshowerdata, pershowertables,
     +                              npexpopt, npexpoplen,
     +                              pexpoptidx, pexpopequiv,
     +                              pexpdefopt, pexpopstr
c
      logical           exportpershower
      integer           ntableprt, ntableexp
      integer           tableprt(mxprtexp), tprtopt(mxprtexp)
      integer           tableexp(mxprtexp), texpopt(mxprtexp)
      integer           expdefopt(mxpexopt), prtdefopt(mxpexopt)
      integer           expmultidx(mxprtexp)
      integer           expmultich0
c
      common            /tprint_c1/ exportpershower,
     +                              ntableprt, ntableexp,
     +                              expdefopt, prtdefopt,
     +                              tableprt, tprtopt,
     +                              tableexp, texpopt,
     +                              expmultidx, expmultich0
c
c     Deposited energy and related histograms.
c
      double precision  llhistn(5, mxobslevelsp1, mxlitable2)
      double precision  llhiste(5, mxobslevelsp1, mxlitable2)
      double precision  lihiste(5, mxobslevelsp1, mxlitable2)
      double precision  wllhistn(5, mxobslevelsp1, mxlitable2)
c
      common            /longitable_cin/ llhistn
      common            /longitable_cie/ llhiste
      common            /longitable_cii/ lihiste
      common            /longitable_cwi/ wllhistn
c
c     Model output variables.
c
      integer           nfodata, niodata, nlodata
      integer           nfodata0, niodata0, nlodata0
      double precision  fodata(5, mxfodata)
      integer           fosamples(mxfodata)
      logical           foupdate(mxfodata)
      integer           iodata(3, mxiodata)
      integer           iosamples(mxfodata)
      logical           ioupdate(mxfodata)
      logical           lodata(mxlodata)
c
      common            /odata_c1/ fodata, fosamples, foupdate,
     +                             iodata, iosamples, ioupdate,
     +                             lodata,
     +                             nfodata, niodata, nlodata,
     +                             nfodata0, niodata0, nlodata0
c
      integer           nallodata, nallodata0
      character*27      odataname(mxallodata)
      integer           odatatype(mxallodata)
      integer           odataitem(mxallodata)
      logical           veryimpodata(mxallodata)
c
      common            /odata_c2/ nallodata, nallodata0,
     +                             odatatype, odataitem,
     +                             veryimpodata, odataname
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'hdatacomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
