c
c     FILE: hmergecomm.f                    Creation date: 18/AUG/2003.
c                                       LAST MODIFICATION: 19/AUG/2003.
c
c     This file contains the definitions of the histogram and output
c     variables, that are required for the AiresMerge program.
c
c     Written by: S. J. Sciutto, Fermilab 2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Observing levels related data.
c
      integer           amiobslevdat(0:1)
      double precision  amfobslevdat(0:1, 2)
c
      common            /amobslev_c1/ amfobslevdat, amiobslevdat
c
c     Table (histogram) arrays and related quantities.
c
      integer           amnlhtables, amnldtables
      integer           amntdtables, amnpshtables
      integer           amnttabins
c
      common            /amtables_c0/
     +                    amnlhtables, amnldtables, amntdtables,
     +                    amnpshtables, amnttabins
c
      integer           amnlitables
c
      common            /amtables_ci0/
     +                    amnlitables
c
c     Longitudinal histograms.
c
      double precision  amlhistn(5, mxobslevelsp1, mxlhtable2)
      double precision  amlhiste(5, mxobslevelsp1, mxlhtable2)
c
      common            /amlongitable_cn/ amlhistn
      common            /amlongitable_ce/ amlhiste
c
      double precision  amwlhistn(5, mxobslevelsp1, mxlhtable2)
c
      common            /amlongitable_cwn/ amwlhistn
c
c     Transversal histograms.
c
      double precision  amrhn0, amfactrthn, amrhni0, amfactrthni
      double precision  amrthistn(5, 0:tdbinsp1, mxldtable)
      double precision  amwrthistn(5, 0:tdbinsp1, mxldtable)
      double precision  amrhe0, amfactrthe, amrhei0, amfactrthei
      double precision  amrthiste(5, 0:tdbinsp1, mxldtable)
      double precision  amwrthiste(5, 0:tdbinsp1, mxldtable)
c
      common            /amldtable_cn/ amrhn0, amfactrthn,
     +                                 amrhni0, amfactrthni,
     +                                 amrthistn, amwrthistn
      common            /amldtable_ce/ amrhe0, amfactrthe,
     +                                 amrhei0, amfactrthei,
     +                                 amrthiste, amwrthiste
c
c     Time distribution histograms.
c
      double precision  amrthistt(5, 0:tdbinsp1, mxtdtable)
      integer           amrtsampl(0:tdbinsp1, mxtdtable)
c
      common            /amtdtable_ct/ amrthistt, amrtsampl
c
c     Data for print and export control.
c     Here 1 corresponds to print and 2 to export.
c
      logical           amsaveshowerdata, ampershowertables
      logical           amsavesheader
c
      common            /amtprint_c0/ amsaveshowerdata,
     +                                ampershowertables,
     +                                amsavesheader
c
c
c     Deposited energy and related histograms.
c
      double precision  amllhistn(5, mxobslevelsp1, mxlitable2)
      double precision  amllhiste(5, mxobslevelsp1, mxlitable2)
      double precision  amlihiste(5, mxobslevelsp1, mxlitable2)
      double precision  amwllhistn(5, mxobslevelsp1, mxlitable2)
c
      common            /amlongitable_cin/ amllhistn
      common            /amlongitable_cie/ amllhiste
      common            /amlongitable_cii/ amlihiste
      common            /amlongitable_cwi/ amwllhistn
c
c     Model output variables.
c
      integer           amnfodata, amniodata, amnlodata
      integer           amnfodata0, amniodata0, amnlodata0
      double precision  amfodata(5, mxfodata)
      integer           amfosamples(mxfodata)
      logical           amfoupdate(mxfodata)
      integer           amiodata(3, mxiodata)
      integer           amiosamples(mxfodata)
      logical           amioupdate(mxfodata)
      logical           amlodata(mxlodata)
c
      common            /amodata_c1/ amfodata, amfosamples, amfoupdate,
     +                               amiodata, amiosamples, amioupdate,
     +                               amlodata,
     +                               amnfodata, amniodata, amnlodata,
     +                               amnfodata0, amniodata0, amnlodata0
c
      integer           amnallodata, amnallodata0
c
      common            /amodata_c2/ amnallodata, amnallodata0
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'hmergecomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
