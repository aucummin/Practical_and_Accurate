c
c     FILE: srycomm.f                       Creation date: 09/JAN/1997.
c                                       LAST MODIFICATION: 14/APR/1999.
c
c     This file contains some shared data using for summary printing.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1999; Fermilab 1999.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     General summary variables.
c
      logical           sryprog
c
      common            /sry0_c/ sryprog
c
c     Running line and page, and page header lines.
c
      integer           currsryline, currsrypage
      integer           nsryhead, sryhlen(10)
      character*80      sryhead(10)
c
      common            /sryhead_c/ currsryline, currsrypage,
     +                              nsryhead, sryhlen, sryhead
c
c     Table printing control variables.
c
      double precision  scaledy, ystart, s1, s2, ca, cb
      integer           kscaley, is1, is2, is3, ih1
      integer           ic2, ic3, ic4, ic5, ic6, ihl, rmss
      logical           plotmm, printmm, xslantcal
      character*12      abscname
      character*72      tfmt1, tfmt2
c
      common            /tableplot_c/ scaledy, ystart, s1, s2, ca, cb,
     +                                kscaley, is1, is2, is3, ihl,
     +                                ih1, ic2, ic3, ic4, ic5, ic6,
     +                                rmss, plotmm, printmm, xslantcal,
     +                                abscname, tfmt1, tfmt2
c
c     Data for bin tabulation.
c
      double precision  linbinca, linbincb
      double precision  logbinca, logbincb
      common            /linbin_c/ linbinca, linbincb,
     +                             logbinca, logbincb
c
c     Other variables for table print/export control.
c
      integer           icommentchar
      logical           tableindex
c
      common            /table_misc/ icommentchar, tableindex
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file srycomm.f
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
