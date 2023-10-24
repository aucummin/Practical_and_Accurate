c
c     FILE: observables.f                   Creation date: 17/JUL/1996.
c                                       LAST MODIFICATION: 03/MAY/2004.
c
c     This file contains several routines to perform statistical
c     calculations.
c     Part I: Routines for initialization and update.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine initable
c
c     Definition of the histograms (tables) used in the simulations.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998; Fermilab 1999;
c                                La Plata 2000, 2001, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*40      namehead, nametrail
      integer           hl, hl1, tl, i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting table names and identification codes.
c
c     1. Longitudinal histograms.
c
      namehead = 'Longitudinal development: '
      hl       = 26
c
      lhpclen( 1) = 'Gamma rays'
      lhnamen( 1) = namehead(1:hl) // 'Gamma rays.'
      lhcoden( 1) = 1001
      lhnamee( 1) = namehead(1:hl) // 'Energy of gamma rays.'
      lhcodee( 1) = 1501
c
      lhpclen( 2) = 'Electrons'
      lhnamen( 2) = namehead(1:hl) // 'Electrons.'
      lhcoden( 2) = 1005
      lhnamee( 2) = namehead(1:hl) // 'Energy of electrons.'
      lhcodee( 2) = 1505
c
      lhpclen( 3) = 'Positrons'
      lhnamen( 3) = namehead(1:hl) // 'Positrons.'
      lhcoden( 3) = 1006
      lhnamee( 3) = namehead(1:hl) // 'Energy of positrons.'
      lhcodee( 3) = 1506
c
      lhpclen( 4) = 'Muons (+)'
      lhnamen( 4) = namehead(1:hl) // 'Muons (+).'
      lhcoden( 4) = 1007
      lhnamee( 4) = namehead(1:hl) // 'Energy of muons (+).'
      lhcodee( 4) = 1507
c
      lhpclen( 5) = 'Muons (-)'
      lhnamen( 5) = namehead(1:hl) // 'Muons (-).'
      lhcoden( 5) = 1008
      lhnamee( 5) = namehead(1:hl) // 'Energy of muons (-).'
      lhcodee( 5) = 1508
c
      lhpclen( 6) = 'Pions (+)'
      lhnamen( 6) = namehead(1:hl) // 'Pions (+).'
      lhcoden( 6) = 1011
      lhnamee( 6) = namehead(1:hl) // 'Energy of pions (+).'
      lhcodee( 6) = 1511
c
      lhpclen( 7) = 'Pions (-)'
      lhnamen( 7) = namehead(1:hl) // 'Pions (-).'
      lhcoden( 7) = 1012
      lhnamee( 7) = namehead(1:hl) // 'Energy of pions (-).'
      lhcodee( 7) = 1512
c
      lhpclen( 8) = 'Kaons (+)'
      lhnamen( 8) = namehead(1:hl) // 'Kaons (+).'
      lhcoden( 8) = 1013
      lhnamee( 8) = namehead(1:hl) // 'Energy of kaons (+).'
      lhcodee( 8) = 1513
c
      lhpclen( 9) = 'Kaons (-)'
      lhnamen( 9) = namehead(1:hl) // 'Kaons (-).'
      lhcoden( 9) = 1014
      lhnamee( 9) = namehead(1:hl) // 'Energy of kaons (-).'
      lhcodee( 9) = 1514
c
      lhpclen(10) = 'Neutrons'
      lhnamen(10) = namehead(1:hl) // 'Neutrons.'
      lhcoden(10) = 1021
      lhnamee(10) = namehead(1:hl) // 'Energy of neutrons.'
      lhcodee(10) = 1521
c
      lhpclen(11) = 'Protons'
      lhnamen(11) = namehead(1:hl) // 'Protons.'
      lhcoden(11) = 1022
      lhnamee(11) = namehead(1:hl) // 'Energy of protons.'
      lhcodee(11) = 1522
c
      lhpclen(12) = 'Anti-protons'
      lhnamen(12) = namehead(1:hl) // 'Antiprotons.'
      lhcoden(12) = 1023
      lhnamee(12) = namehead(1:hl) // 'Energy of Antiprotons.'
      lhcodee(12) = 1523
c
      lhpclen(13) = 'Nuclei'
      lhnamen(13) = namehead(1:hl) // 'Nuclei.'
      lhcoden(13) = 1041
      lhnamee(13) = namehead(1:hl) // 'Energy of nuclei.'
      lhcodee(13) = 1541
c
      lhpclen(14) = 'Other charged'
      lhnamen(14) = namehead(1:hl) // 'Other charged pcles.'
      lhcoden(14) = 1091
      lhnamee(14) = namehead(1:hl) //
     +              'Energy of other charged particles.'
      lhcodee(14) = 1591
c
      lhpclen(15) = 'Other neutral'
      lhnamen(15) = namehead(1:hl) // 'Other neutral pcles.'
      lhcoden(15) = 1092
      lhnamee(15) = namehead(1:hl) //
     +              'Energy of other neutral particles.'
      lhcodee(15) = 1592
c
      nlhtable1 = 15
c
c     Compound tables.
c
      lhpclen(16) = 'Electr & positr'
      lhnamen(16) = namehead(1:hl) // 'e+ and e-'
      lhcoden(16) = 1205
      lhnamee(16) = namehead(1:hl) // 'Energy of e+ and e-'
      lhcodee(16) = 1705
c
      lhpclen(17) = 'All muons'
      lhnamen(17) = namehead(1:hl) // 'mu+ and mu-'
      lhcoden(17) = 1207
      lhnamee(17) = namehead(1:hl) // 'Energy of mu+ and mu-'
      lhcodee(17) = 1707
c
      lhpclen(18) = 'All pions'
      lhnamen(18) = namehead(1:hl) // 'pi+ and pi-'
      lhcoden(18) = 1211
      lhnamee(18) = namehead(1:hl) // 'Energy of pi+ and pi-'
      lhcodee(18) = 1711
c
      lhpclen(19) = 'All kaons'
      lhnamen(19) = namehead(1:hl) // 'K+ and K-'
      lhcoden(19) = 1213
      lhnamee(19) = namehead(1:hl) // 'Energy of K+ and K-'
      lhcodee(19) = 1713
c
      lhpclen(20) = 'All charged'
      lhnamen(20) = namehead(1:hl) // 'All charged particles.'
      lhcoden(20) = 1291
      lhnamee(20) = namehead(1:hl) //
     +              'Energy of all charged particles.'
      lhcodee(20) = 1791
c
c     lallcht is the index corresponding to the "all charged pcles"
c     longitudinal table.
c
      lallcht = 20
c
      lhpclen(21) = 'All neutral'
      lhnamen(21) = namehead(1:hl) // 'All neutral particles.'
      lhcoden(21) = 1292
      lhnamee(21) = namehead(1:hl) //
     +              'Energy of all neutral particles.'
      lhcodee(21) = 1792
c
      lhpclen(22) = 'All particles'
      lhnamen(22) = namehead(1:hl) // 'All particles.'
      lhcoden(22) = 1293
      lhnamee(22) = namehead(1:hl) // 'Energy of all particles.'
      lhcodee(22) = 1793
c
      nlhtables = 22
c
c
c     1a. Unweighted longitudinal tables.
c
c     Unweighted tables are set similarly as the standard longitudinal
c     tables.
c
      hl1      = hl + 1
      namehead = 'Unweighted longit. development: '
      hl       = 32
c
      do i = 1, nlhtable1
        wlhnamen(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        wlhcoden(i) = lhcoden(i) + 300
      enddo
c
      do i = nlhtable1 + 1, nlhtables
        wlhnamen(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        wlhcoden(i) = lhcoden(i) + 200
      enddo
c
c
c     2. Lateral distribution histograms.
c
c     Lateral distribution tables are set similarly as the standard
c     longitudinal tables.
c
      nldtable1 = nlhtable1
      nldtables = nlhtables
c
      namehead = 'Lateral distribution: '
      hl       = 22
c
      do i = 1, nldtables
        ldnamen(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        ldcoden(i) = lhcoden(i) + 1000
      enddo
c
c
c     2a. Unweighted lateral distribution tables.
c
c     Unweighted tables are set similarly as the standard lateral
c     distribution tables.
c
      namehead = 'Unweighted lateral distribution: '
      hl       = 33
c
      do i = 1, nldtable1
        wldnamen(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        wldcoden(i) = ldcoden(i) + 300
      enddo
c
      do i = nlhtable1 + 1, nlhtables
        wldnamen(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        wldcoden(i) = ldcoden(i) + 200
      enddo
c
c
c     3. Lateral energy distribution histograms.
c
c     Energy distribution tables are set similarly as the standard
c     lateral distribution tables.
c
      namehead = 'Energy distribution at ground: '
      hl       = 31
c
      do i = 1, nldtables
        ldnamee(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        ldcodee(i) = ldcoden(i) + 500
      enddo
c
c
c     3a. Unweighted energy distribution tables.
c
c     Unweighted tables are set similarly as the standard lateral
c     and energy distribution tables.
c
      namehead = 'Unweighted energy distribution: '
      hl       = 32
c
      do i = 1, nldtable1
        wldnamee(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        wldcodee(i) = ldcodee(i) + 300
       enddo
c
      do i = nlhtable1 + 1, nlhtables
        wldnamee(i) = namehead(1:hl) // lhnamen(i)(hl1:64)
        wldcodee(i) = ldcodee(i) + 200
      enddo
c
c
c     4. Mean time distribution histograms.
c
      namehead = 'Mean arrival time distribution: '
      hl       = 32
c
      tdnamen( 1) = namehead(1:hl) // 'Gamma rays.'
      tdcoden( 1) = 3001
c
      tdnamen( 2) = namehead(1:hl) // 'Electrons and positrons.'
      tdcoden( 2) = 3005
c
      tdnamen( 3) = namehead(1:hl) // 'Muons.'
      tdcoden( 3) = 3007
c
      tdnamen( 4) = namehead(1:hl) // 'Other charged pcles.'
      tdcoden( 4) = 3091
c
      tdnamen( 5) = namehead(1:hl) // 'Other neutral pcles.'
      tdcoden( 5) = 3092
c
      ntdtable1 = 5
c
c     Compound tables.
c
      tdnamen( 6) = namehead(1:hl) // 'All charged particles.'
      tdcoden( 6) = 3291
c
      tdnamen( 7) = namehead(1:hl) // 'All neutral particles.'
      tdcoden( 7) = 3292
c
      tdnamen( 8) = namehead(1:hl) // 'All particles.'
      tdcoden( 8) = 3293
c
      ntdtables = 8
c
c
c     5. "Per shower" tables.
c
      namehead  = 'Number and energy of ground '
      hl        = 28
      nametrail = ' versus shower number.'
      tl        = 22
c
      pshname( 1) = namehead(1:hl) // 'gammas' // nametrail(1:tl)
      pshcode( 1) = 5001
c
      pshname( 2) = namehead(1:hl) // 'e-' // nametrail(1:tl)
      pshcode( 2) = 5005
c
      pshname( 3) = namehead(1:hl) // 'e+' // nametrail(1:tl)
      pshcode( 3) = 5006
c
      pshname( 4) = namehead(1:hl) // 'mu+' // nametrail(1:tl)
      pshcode( 4) = 5007
c
      pshname( 5) = namehead(1:hl) // 'mu-' // nametrail(1:tl)
      pshcode( 5) = 5008
c
      pshname( 6) = namehead(1:hl) // 'pi+' // nametrail(1:tl)
      pshcode( 6) = 5011
c
      pshname( 7) = namehead(1:hl) // 'pi-' // nametrail(1:tl)
      pshcode( 7) = 5012
c
      pshname( 8) = namehead(1:hl) // 'K+' // nametrail(1:tl)
      pshcode( 8) = 5013
c
      pshname( 9) = namehead(1:hl) // 'K-' // nametrail(1:tl)
      pshcode( 9) = 5014
c
      pshname(10) = namehead(1:hl) // 'neutrons' // nametrail(1:tl)
      pshcode(10) = 5021
c
      pshname(11) = namehead(1:hl) // 'protons' // nametrail(1:tl)
      pshcode(11) = 5022
c
      pshname(12) = namehead(1:hl) // 'pbar' // nametrail(1:tl)
      pshcode(12) = 5023
c
      pshname(13) = namehead(1:hl) // 'nuclei' // nametrail(1:tl)
      pshcode(13) = 5041
c
      pshname(14) = 'Number and energy of other grd. ch. pcles.' //
     +              nametrail(1:tl)
      pshcode(14) = 5091
c
      pshname(15) = 'Number and energy of other grd. nt. pcles.' //
     +              nametrail(1:tl)
      pshcode(15) = 5092
c
      npshtable1 = 15
c
c     Compound tables.
c
      pshname(16) = namehead(1:hl) // 'e+ and e-' //
     +              nametrail(1:tl)
      pshcode(16) = 5205
c
      pshname(17) = namehead(1:hl) // 'mu+ and mu-' //
     +              nametrail(1:tl)
      pshcode(17) = 5207
c
      pshname(18) = namehead(1:hl) // 'pi+ and pi-' //
     +              nametrail(1:tl)
      pshcode(18) = 5211
c
      pshname(19) = namehead(1:hl) // 'K+ and K-' //
     +              nametrail(1:tl)
      pshcode(19) = 5213
c
      pshname(20) = namehead(1:hl) // 'ch. pcles.' //
     +              nametrail(1:tl)
      pshcode(20) = 5291
c
      pshname(21) = namehead(1:hl) // 'nt. pcles.' //
     +              nametrail(1:tl)
      pshcode(21) = 5292
c
      pshname(22) = 'Number and energy of all ground particles' //
     +              nametrail(1:tl)
      pshcode(22) = 5293
c
c     Table for Xmax and Nmax
      pshname(23) = 'Xmax and Nmax (charged particles)' //
     +              nametrail(1:tl)
      pshcode(23) = 5501
c
c     Table for X1 and primary energy vs shower number.
      pshname(24) = 'First interact. depth and primary energy' //
     +              nametrail(1:tl)
      pshcode(24) = 5511
c
c     Table for zenith and azimuth vs shower number.
      pshname(25) = 'Zenith and azimuth angles' //
     +              nametrail(1:tl)
      pshcode(25) = 5513
c
      npshtables = nlhtables + 3
c
c
c     6. Deposited energy and related tables.
c
      namehead = 'Longitudinal development: '
      hl       = 26
c
      llnamen( 1) = namehead(1:hl) // 'Low energy gamma rays.'
      llcoden( 1) = 7001
      llnamee( 1) = namehead(1:hl) // 'Energy of low egy. gamma rays.'
      llcodee( 1) = 7501
      linamee( 1) = namehead(1:hl) // 'Energy deposited by gamma rays.'
      licodee( 1) = 7801
c
      llnamen( 2) = namehead(1:hl) // 'Low energy electrons.'
      llcoden( 2) = 7005
      llnamee( 2) = namehead(1:hl) // 'Energy of low egy. electrons.'
      llcodee( 2) = 7505
      linamee( 2) = namehead(1:hl) // 'Energy deposited by electrons.'
      licodee( 2) = 7805
c
      llnamen( 3) = namehead(1:hl) // 'Low energy positrons.'
      llcoden( 3) = 7006
      llnamee( 3) = namehead(1:hl) // 'Energy of low egy. positrons.'
      llcodee( 3) = 7506
      linamee( 3) = namehead(1:hl) // 'Energy deposited by positrons.'
      licodee( 3) = 7806
c
      llnamen( 4) = namehead(1:hl) // 'Low energy muons (+).'
      llcoden( 4) = 7007
      llnamee( 4) = namehead(1:hl) // 'Energy of low egy. muons(+).'
      llcodee( 4) = 7507
      linamee( 4) = namehead(1:hl) // 'Energy deposited by muons (+).'
      licodee( 4) = 7807
c
      llnamen( 5) = namehead(1:hl) // 'Low energy muons (-).'
      llcoden( 5) = 7008
      llnamee( 5) = namehead(1:hl) // 'Energy of low egy. muons(-).'
      llcodee( 5) = 7508
      linamee( 5) = namehead(1:hl) // 'Energy deposited by muons (-).'
      licodee( 5) = 7808
c
      llnamen( 6) = namehead(1:hl) // 'Other charged low egy. pcles.'
      llcoden( 6) = 7091
      llnamee( 6) = namehead(1:hl) //
     +                        'Egy. of other charged low egy. pcles.'
      llcodee( 6) = 7591
      linamee( 6) = namehead(1:hl) //
     +                       'Egy. deposited by other charged pcles.'
      licodee( 6) = 7891
c
      llnamen( 7) = namehead(1:hl) // 'Other neutral low egy. pcles.'
      llcoden( 7) = 7092
      llnamee( 7) = namehead(1:hl) //
     +                        'Egy. of other neutral low egy. pcles.'
      llcodee( 7) = 7592
      linamee( 7) = namehead(1:hl) //
     +                       'Egy. deposited by other neutral pcles.'
      licodee( 7) = 7892
c
      nlitable1 = 7
c
c     Compound tables.
c
      llnamen( 8) = namehead(1:hl) // 'Low energy e+ and e-'
      llcoden( 8) = 7205
      llnamee( 8) = namehead(1:hl) // 'Energy of low egy. e+ and e-'
      llcodee( 8) = 7705
      linamee( 8) = namehead(1:hl) // 'Energy deposited by e+ and e-'
      licodee( 8) = 7905
c
      llnamen( 9) = namehead(1:hl) // 'Low energy mu+ and mu-'
      llcoden( 9) = 7207
      llnamee( 9) = namehead(1:hl) // 'Energy of low egy. mu+ and mu-'
      llcodee( 9) = 7707
      linamee( 9) = namehead(1:hl) // 'Energy deposited by mu+ and mu-'
      licodee( 9) = 7907
c
      llnamen(10) = namehead(1:hl) // 'All low energy charged pcles.'
      llcoden(10) = 7291
      llnamee(10) = namehead(1:hl) //
     +                          'Egy. of all low egy. charged pcles.'
      llcodee(10) = 7791
      linamee(10) = namehead(1:hl) //
     +                       'Egy. deposited by all charged pcles.'
      licodee(10) = 7991
c
      llnamen(11) = namehead(1:hl) // 'All low energy neutral pcles.'
      llcoden(11) = 7292
      llnamee(11) = namehead(1:hl) //
     +                          'Egy. of all low egy. neutral pcles.'
      llcodee(11) = 7792
      linamee(11) = namehead(1:hl) //
     +                       'Egy. deposited by all neutral pcles.'
      licodee(11) = 7992
c
      llnamen(12) = namehead(1:hl) // 'All low energy pcles.'
      llcoden(12) = 7293
      llnamee(12) = namehead(1:hl) // 'Egy. of all low energy pcles.'
      llcodee(12) = 7793
      linamee(12) = namehead(1:hl) // 'Energy deposited by all pcles.'
      licodee(12) = 7993
c
      nlitables = 12
c
c
c     1a. Unweighted low energy particle tables.
c
c     Unweighted tables are set similarly as the standard low energy
c     tables.
c
      hl1      = hl + 1
      namehead = 'Unweighted longit. devel.: '
      hl       = 27
c
      do i = 1, nlitable1
        wllnamen(i) = namehead(1:hl) // llnamen(i)(hl1:64)
        wllcoden(i) = llcoden(i) + 300
      enddo
c
      do i = nlitable1 + 1, nlitables
        wllnamen(i) = namehead(1:hl) // llnamen(i)(hl1:64)
        wllcoden(i) = llcoden(i) + 200
      enddo
c
c     Maximum table code.
c
      maxtabcode = 7999
c
c     Checking numbers of defined tables.
c
      if ((nlhtables .gt. mxlhtable) .or.
     +    (nlitables .gt. mxlitable) .or.
     +    (nldtables .gt. mxldtable) .or.
     +    (ntdtables .gt. mxtdtable) .or.
     +    (npshtables .gt. mxpshtable)) then
        call errprint(0, '*', 4, 'initable',
     +                'Not enough space to store table definitions.',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
c     Setting initial values for some table parameters.
c
c     By default, all observing levels are initially enabled for
c     recording in longitudinal file(s).
c
      do i = 1, mxobslevelsp1
        olfilesave(i) = .true.
      enddo
c
c     Setting the auxiliary data for summary printing ground
c     particles.
c     "grdorder" indicates the order of appearance of the
c     different items; and "grdpspa" is the spacing with respect
c     to the previous line, using the rules of routine "putsry".
c
c     Gamma rays.
c
      grdpspa( 1)   =  0
      grdporder( 1) =  1
c
c     e+ e-
c
      grdpspa( 2)   =  0
      grdporder( 2) =  2
      grdpspa( 3)   =  9
      grdporder( 3) =  3
      grdpspa( 4)   =  9
      grdporder( 4) = 16
c
c     mu+ mu-
c
      grdpspa( 5)   =  0
      grdporder( 5) =  4
      grdpspa( 6)   =  9
      grdporder( 6) =  5
      grdpspa( 7)   =  9
      grdporder( 7) = 17
c
c     pi+ pi-
c
      grdpspa( 8)   =  0
      grdporder( 8) =  6
      grdpspa( 9)   =  9
      grdporder( 9) =  7
      grdpspa(10)   =  9
      grdporder(10) = 18
c
c     K+ K-
c
      grdpspa(11)   =  0
      grdporder(11) =  8
      grdpspa(12)   =  9
      grdporder(12) =  9
      grdpspa(13)   =  9
      grdporder(13) = 19
c
c     Nucleons.
c
      grdpspa(14)   =  0
      grdporder(14) = 10
      grdpspa(15)   =  9
      grdporder(15) = 11
      grdpspa(16)   =  9
      grdporder(16) = 12
c
c     Nuclei
c
      grdpspa(17)   =  0
      grdporder(17) = 13
c
c     Totals.
c
      grdpspa(18)   =  0
      grdporder(18) = 20
      grdpspa(19)   =  9
      grdporder(19) = 21
      grdpspa(20)   =  9
      grdporder(20) = 22
c
      ngrdprint     = 20
c
c     Setting the print/export options, their defaults and related
c     data.
c
      call iniprtexpopts
c
c     Initializing the number of tables to print/export
c
      ntableprt = 0
      ntableexp = 0
c
      exportpershower = .false.
c
      return
c
      end
c     --- End of routine initable
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine tablepointer
c
c     Setting the primary tables associated with each particle code.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'pclecomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Longitudinal histograms.
c
c     Setting "other charged" and "other neutral" options.
c
      do i = -maxpcle, minncode
        if (allpgroup(2, i)) then
          allpclelh(i) = 14
        else
          allpclelh(i) = 15
        endif
      enddo
c
c     Setting nuclei pointers.
c
      do i = minncode, maxncode
        allpclelh(i) = 13
      enddo
c
c     Setting specific table pointers.
c
      allpclelh(gammacode)    =  1
      allpclelh(electroncode) =  2
      allpclelh(positroncode) =  3
      allpclelh(mupluscode)   =  4
      allpclelh(muminuscode)  =  5
      allpclelh(pipluscode)   =  6
      allpclelh(piminuscode)  =  7
      allpclelh(kpluscode)    =  8
      allpclelh(kminuscode)   =  9
      allpclelh(neutroncode)  = 10
      allpclelh(protoncode)   = 11
      allpclelh(pbarcode)     = 12
c
c     Lateral distribution histograms are set equal to longitudinal
c     histograms.
c
      do i = -maxpcle, maxncode
        allpcleth(i) = allpclelh(i)
      enddo
c
c     Time distribution histograms.
c
c     Setting "other charged" and "other neutral" options.
c
      do i = -maxpcle, minncode
        if (allpgroup(2, i)) then
          allpcleuh(i) = 4
        else
          allpcleuh(i) = 5
        endif
      enddo
c
      do i = minncode, maxncode
        allpcleuh(i) = 4
      enddo
c
c     Setting specific table pointers.
c
      allpcleuh(gammacode)    =  1
      allpcleuh(electroncode) =  2
      allpcleuh(positroncode) =  2
      allpcleuh(mupluscode)   =  3
      allpcleuh(muminuscode)  =  3
c
c     Deposited energy and related histograms.
c
c     Setting "other charged" and "other neutral" options.
c
      do i = -maxpcle, minncode
        if (allpgroup(2, i)) then
          allpcleli(i) = 6
        else
          allpcleli(i) = 7
        endif
      enddo
c
      do i = minncode, maxncode
        allpcleli(i) = 6
      enddo
c
c     Setting specific table pointers.
c
      allpcleli(gammacode)    =  1
      allpcleli(electroncode) =  2
      allpcleli(positroncode) =  3
      allpcleli(mupluscode)   =  4
      allpcleli(muminuscode)  =  5
c
      return
c
      end
c     --- End of routine tablepointer
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine table0
c
c     Zeroing the main histogram arrays and related tasks.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 2000, 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k, k0, l
c
c     FIRST EXECUTABLE STATEMENT
c
c     Longitudinal histograms.
c
      do l = 0, 1
        do k0 = 1, nlhtables
          k = k0 + l * mxlhtable
          call statzero(nobslevelsp1, 5, lhistn(1, 1, k))
          call statzero(nobslevelsp1, 5, lhiste(1, 1, k))
          call statzero(nobslevelsp1, 5, wlhistn(1, 1, k))
        enddo
      enddo
c
c     Shower maximum related data.
c
      call statzero(2, 5, shxmax)
      call statzero(1, 5, shnmax)
c
      shxsamples = 0
c
c     Initializing the Xmax and related data file.
c
      call inipershowerdata
c
c     Lateral distribution histograms.
c
      do k = 1, nldtables
        call statzero(nttabinsp2, 5, rthistn(1, 0, k))
        call statzero(nttabinsp2, 5, rthiste(1, 0, k))
        call statzero(nttabinsp2, 5, wrthistn(1, 0, k))
        call statzero(nttabinsp2, 5, wrthiste(1, 0, k))
      enddo
c
c     Time distribution histograms.
c
      do k = 1, ntdtables
        call statzero(nttabinsp2, 5, rthistt(1, 0, k))
        do i = 0, nttabinsp1
          rtsampl(i, k) = 0
        enddo
      enddo
c
c     Deposited energy and related histograms.
c
      do l = 0, 1
        do k0 = 1, nlitables
          k = k0 + l * mxlitable
          call statzero(nobslevelsp1, 5, llhistn(1, 1, k))
          call statzero(nobslevelsp1, 5, llhiste(1, 1, k))
          call statzero(nobslevelsp1, 5, lihiste(1, 1, k))
          call statzero(nobslevelsp1, 5, wllhistn(1, 1, k))
        enddo
      enddo
c
      return
      end
c     --- End of routine table0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine table0l2
c
c     Zeroing the shower part of the longitudinal arrays. This
c     routine is used to reset counters when needed because of
c     changes in the shower propagating conditions, for example
c     primary repositioning.
c
c     Written by: S. J. Sciutto, La Plata 2004.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k, k0, l
c
c     FIRST EXECUTABLE STATEMENT
c
c     Longitudinal histograms.
c
      do l = 0, 1
        do k0 = 1, nlhtables
          k = k0 + l * mxlhtable
          do i = 1, nobslevelsp1
            lhistn(2, i, k)  = 0
            lhiste(2, i, k)  = 0
            wlhistn(2, i, k) = 0
          enddo
        enddo
      enddo
c
c     Deposited energy and related histograms.
c
      do l = 0, 1
        do k0 = 1, nlitables
          k = k0 + l * mxlitable
          do i = 1, nobslevelsp1
            llhistn(2, i, k)  = 0
            llhiste(2, i, k)  = 0
            lihiste(2, i, k)  = 0
            wllhistn(2, i, k) = 0
          enddo
        enddo
      enddo
c
      return
      end
c     --- End of routine table0l2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine table1(primx1, primx1sl, primegy, primzen, primazim,
     +                  xmax, nmax, sumofsq, irc)
c
c     End of shower histogram updating.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 2000, 2001.
c
c
c     Arguments:
c     =========
c
c     primx1.......... (input, double precision) Vertical depth of
c                      first interaction (g/cm2).
c     primx1sl........ (input, double precision) Slant depth of
c                      first interaction (g/cm2).
c     primegy......... (input, double precision) Primary energy (GeV).
c     primzen......... (input, double precision) Primary zenith angle
c                      (deg).
c     primazim........ (input, double precision) Primary azimuth angle
c                      (deg).
c     xmax............ (output, double precision, array(2)) Fitted
c                      position of the shower maximum (g.cm2) (1:
c                      vertical, 2: slant). If no fit was possible,
c                      then the the value coming from a direct
c                      estimation from the MC data is returned.
c     nmax............ (output, double precision) Estimated number of
c                      charged particles at the shower maximum. If no
c                      fit was possible, then the value coming from a
c                      direct estimation from the MC data is returned.
c     sqsum........... (output, double precision) The resulting sum of
c                      weighted squares.
c     irc............. (output, integer) Return code. Zero means that
c                      the fit was successfully completed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      double precision  primx1, primx1sl, primegy, primzen, primazim
      double precision  xmax(2), nmax, sumofsq
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'showercomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k
      double precision  gan, epmn, mupmn
      double precision  gat, epmt, mupmt
      double precision  ochn, ontn, allchn, allntn, alln
      double precision  ocht, ontt, allcht, allntt, allt
      double precision  x0, lambda
      double precision  xslant
c
c     FIRST EXECUTABLE STATEMENT
c
c     Calculating compound tables.
c
c     Longitudinal histograms.
c
      call compoundtab1(1, nobslevelsp1, mxobslevelsp1, mxlhtable,
     +                  lhistn)
      call compoundtab1(1, nobslevelsp1, mxobslevelsp1, mxlhtable,
     +                  lhiste)
      call compoundtab1(1, nobslevelsp1, mxobslevelsp1, mxlhtable,
     +                  wlhistn)
c
      call compoundtab1(1, nobslevels, mxobslevelsp1, mxlhtable,
     +                  lhistn(1, 1, mxlhtable + 1))
      call compoundtab1(1, nobslevels, mxobslevelsp1, mxlhtable,
     +                  wlhistn(1, 1, mxlhtable + 1))
c
c     Deposited energy and related histograms.
c
      call compoundtab2(1, nobslevelsp1, mxobslevelsp1, mxlitable,
     +                  llhistn)
      call compoundtab2(1, nobslevelsp1, mxobslevelsp1, mxlitable,
     +                  llhiste)
      call compoundtab2(1, nobslevelsp1, mxobslevelsp1, mxlitable,
     +                  wllhistn)
      call compoundtab2(1, nobslevelsp1, mxobslevelsp1, mxlitable,
     +                  llhistn(1, 1, mxlitable + 1))
      call compoundtab2(1, nobslevelsp1, mxobslevelsp1, mxlitable,
     +                  llhiste(1, 1, mxlitable + 1))
      call compoundtab2(1, nobslevelsp1, mxobslevelsp1, mxlitable,
     +                  wllhistn(1, 1, mxlitable + 1))
      call compoundtab2(1, nobslevelsp1, mxobslevelsp1, mxlitable,
     +                  lihiste)
c
c     Lateral distribution histograms.
c
      call compoundtab1(0, nttabinsp1, tdbinsp1, mxldtable, rthistn)
      call compoundtab1(0, nttabinsp1, tdbinsp1, mxldtable, rthiste)
      call compoundtab1(0, nttabinsp1, tdbinsp1, mxldtable, wrthistn)
      call compoundtab1(0, nttabinsp1, tdbinsp1, mxldtable, wrthiste)
c
c     Time distribution tables. These tables are connected to the
c     lateral distribution tables because the time data must be
c     averaged dividing by the corresponding number of particles.
c
      do i = 0, nttabinsp1
c
c       gamma rays
c
        gat = rthistt(2, i, 1)
        gan = rthistn(2, i, 1)
        if (gan .gt. 0) then
          rtsampl(i, 1) = rtsampl(i, 1) + 1
          rthistt(2, i, 1) = gat / gan
        else
          rthistt(2, i, 1) = 0
        endif
c
c       e+ plus e-
c
        epmt = rthistt(2, i, 2)
        epmn = rthistn(2, i, 16)
        if (epmn .gt. 0) then
          rtsampl(i, 2) = rtsampl(i, 2) + 1
          rthistt(2, i, 2) = epmt / epmn
        else
          rthistt(2, i, 2) = 0
        endif
c
c       mu+ plus mu-
c
        mupmt = rthistt(2, i, 3)
        mupmn = rthistn(2, i, 17)
        if (mupmn .gt. 0) then
          rtsampl(i, 3) = rtsampl(i, 3) + 1
          rthistt(2, i, 3) = mupmt / mupmn
        else
          rthistt(2, i, 3) = 0
        endif
c
c       Other charged particles.
c
        ocht = rthistt(2, i, 4)
        ochn = rthistn(2, i, 20) - epmn - mupmn
        if (ochn .gt. 0) then
          rtsampl(i, 4) = rtsampl(i, 4) + 1
          rthistt(2, i, 4) = ocht / ochn
        else
          rthistt(2, i, 4) = 0
        endif
c
c       Other neutral particles.
c
        ontt = rthistt(2, i, 5)
        ontn = rthistn(2, i, 21) - gan
        if (ontn .gt. 0) then
          rtsampl(i, 5) = rtsampl(i, 5) + 1
          rthistt(2, i, 5) = ontt / ontn
        else
          rthistt(2, i, 5) = 0
        endif
c
c       All charged particles.
c
        allcht = epmt + mupmt + ocht
        allchn = rthistn(2, i, 20)
        if (allchn .gt. 0) then
          rtsampl(i, 6) = rtsampl(i, 6) + 1
          rthistt(2, i, 6) = allcht / allchn
        else
          rthistt(2, i, 6) = 0
        endif
c
c       All neutral particles.
c
        allntt = gat + ontt
        allntn = rthistn(2, i, 21)
        if (allntn .gt. 0) then
          rtsampl(i, 7) = rtsampl(i, 7) + 1
          rthistt(2, i, 7) = allntt / allntn
        else
          rthistt(2, i, 7) = 0
        endif
c
c       All particles.
c
        allt = allcht + allntt
        alln = rthistn(2, i, 22)
        if (alln .gt. 0) then
          rtsampl(i, 8) = rtsampl(i, 8) + 1
          rthistt(2, i, 8) = allt / alln
        else
          rthistt(2, i, 8) = 0
        endif
c
      enddo
c
c     Evaluating xmax and nmax.
c
      call xmaxfit(2, x0, lambda, xmax(1), nmax, sumofsq, irc)
c
      xmax(2) = xslant(xmax(1), 0.d0, -initshdir(3), groundz)
c
      if (irc .eq. 0) then
        shxmax(2, 1) = xmax(1)
        shxmax(2, 2) = xmax(2)
        shnmax(2)    = nmax
        shxsamples   = shxsamples + 1
      else
        shxmax(2, 1) = 0
        shxmax(2, 2) = 0
        shnmax(2)    = 0
      endif
c
c     Saving xmax and related data.
c
      if (saveshowerdata) then
        call addpershowerdata(primcode, primegy, primzen, primazim,
     +                        primx1, primx1sl,
     +                        x0, lambda, xmax, nmax, sumofsq, irc)
      endif
c
c     Updating the table arrays.
c
c     Longitudinal histograms.
c
      do k = 1, nlhtables
        call statupdate(nobslevelsp1, 5, lhistn(1, 1, k))
        call statupdate(nobslevelsp1, 5, lhiste(1, 1, k))
        call statupdate(nobslevelsp1, 5, wlhistn(1, 1, k))
        call statupdate(nobslevelsp1, 5, lhistn(1, 1, k + mxlhtable))
        call statupdate(nobslevelsp1, 5, wlhistn(1, 1, k + mxlhtable))
      enddo
c
c     Deposited energy and related histograms.
c
      do k = 1, nlitables
        call statupdate(nobslevelsp1, 5, llhistn(1, 1, k))
        call statupdate(nobslevelsp1, 5, llhiste(1, 1, k))
        call statupdate(nobslevelsp1, 5, wllhistn(1, 1, k))
        call statupdate(nobslevelsp1, 5, llhistn(1, 1, k + mxlitable))
        call statupdate(nobslevelsp1, 5, llhiste(1, 1, k + mxlitable))
        call statupdate(nobslevelsp1, 5, wllhistn(1, 1, k + mxlitable))
        call statupdate(nobslevelsp1, 5, lihiste(1, 1, k))
      enddo
c
c     Shower maximum.
c
      if (irc .eq. 0) then
        call statupdate(2, 5, shxmax)
        call statupdate(1, 5, shnmax)
      endif
c
c     Lateral distribution histograms.
c
      do k = 1, nldtables
        call statupdate(nttabinsp2, 5, rthistn(1, 0, k))
        call statupdate(nttabinsp2, 5, rthiste(1, 0, k))
        call statupdate(nttabinsp2, 5, wrthistn(1, 0, k))
        call statupdate(nttabinsp2, 5, wrthiste(1, 0, k))
      enddo
c
c     Time distribution histograms.
c
      do k = 1, ntdtables
        call statupdate(nttabinsp2, 5, rthistt(1, 0, k))
      enddo
c
      return
      end
c     --- End of routine table1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine compoundtab1(bin1, nbins, mxbins, mxtables, htables)
c
c     Performing end of shower setting of compound tables.
c     I. Main longitudinal and lateral distributions.
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c     Arguments:
c     =========
c
c     bin1............ (input, integer) Starting bin number.
c     nbins........... (input, integer) Last bin number.
c     mxbins.......... (input, integer) One of the dimensions of array
c                      htables. Must be equal or larger than nbins.
c     mxtables........ (input, integer) The number of different tables.
c                      This routine works only if mxtables is 22.
c     htables......... (input-output, double precision,
c                      array(5, bin1:mxbins, mxtables) The data array.
c
      implicit none
c
c     Declaration of arguments.
c
      integer           bin1, nbins, mxbins, mxtables
      double precision  htables(5, bin1:mxbins, mxtables)
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  epmn, mupmn, pipmn, kapmn
      double precision  allchn, allntn
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the compound tables. It is assumed here that the
c     following order applies for all tables:
c
c     Basic tables.
c
c          1. Gamma rays.
c          2. Electrons.
c          3. Positrons.
c          4. Muons (+).
c          5. Muons (-).
c          6. Pions (+).
c          7. Pions (-).
c          8. Kaons (+).
c          9. Kaons (-).
c         10. Neutrons.
c         11. Protons.
c         12. Antiprotons.
c         13. Nuclei.
c         14. Other charged particles.
c         15. Other neutral particles.
c
c     Compound tables.
c
c         16. e+ and e-.
c         17. mu+ and mu-.
c         18. pi+ and pi-.
c         19. K+ and K-.
c         20. All charged particles.
c         21. All neutral particles.
c         22. All particles.
c
      do i = bin1, nbins
c
c       e+ plus e-
c
        epmn              = htables(2, i, 2) + htables(2, i, 3)
        htables(2, i, 16) = epmn
c
c       mu+ plus mu-
c
        mupmn             = htables(2, i, 4) + htables(2, i, 5)
        htables(2, i, 17) = mupmn
c
c       pi+ plus pi-
c
        pipmn             = htables(2, i, 6) + htables(2, i, 7)
        htables(2, i, 18) = pipmn
c
c       K+ plus K-
c
        kapmn             = htables(2, i, 8) + htables(2, i, 9)
        htables(2, i, 19) = kapmn
c
c       All charged particles.
c
        allchn            = epmn + mupmn + pipmn + kapmn +
     +                      htables(2, i, 11) + htables(2, i, 12) +
     +                      htables(2, i, 13) + htables(2, i, 14)
        htables(2, i, 20) = allchn
c
c       All neutral particles.
c
        allntn            = htables(2, i,  1) + htables(2, i, 10) +
     +                      htables(2, i, 15)
        htables(2, i, 21) = allntn
c
c       All particles.
c
        htables(2, i, 22) = allchn + allntn
c
      enddo
c
      return
      end
c     --- End of routine compoundtab1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine compoundtab2(bin1, nbins, mxbins, mxtables, htables)
c
c     Performing end of shower setting of compound tables.
c     II. Deposited energy and related distributions.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     bin1............ (input, integer) Starting bin number.
c     nbins........... (input, integer) Last bin number.
c     mxbins.......... (input, integer) One of the dimensions of array
c                      htables. Must be equal or larger than nbins.
c     mxtables........ (input, integer) The number of different tables.
c                      This routine works only if mxtables is 12.
c     htables......... (input-output, double precision,
c                      array(5, bin1:mxbins, mxtables) The data array.
c
      implicit none
c
c     Declaration of arguments.
c
      integer           bin1, nbins, mxbins, mxtables
      double precision  htables(5, bin1:mxbins, mxtables)
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  epmn, mupmn
      double precision  allchn, allntn
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the compound tables. It is assumed here that the
c     following order applies for all tables:
c
c     Basic tables.
c
c          1. Gamma rays.
c          2. Electrons.
c          3. Positrons.
c          4. Muons (+).
c          5. Muons (-).
c          6. Other charged particles.
c          7. Other neutral particles.
c
c     Compound tables.
c
c          8. e+ and e-.
c          9. mu+ and mu-.
c         10. All charged particles.
c         11. All neutral particles.
c         12. All particles.
c
      do i = bin1, nbins
c
c       e+ plus e-
c
        epmn              = htables(2, i, 2) + htables(2, i, 3)
        htables(2, i,  8) = epmn
c
c       mu+ plus mu-
c
        mupmn             = htables(2, i, 4) + htables(2, i, 5)
        htables(2, i,  9) = mupmn
c
c       All charged particles.
c
        allchn            = epmn + mupmn + htables(2, i, 6)
        htables(2, i, 10) = allchn
c
c       All neutral particles.
c
        allntn            = htables(2, i,  1) + htables(2, i, 7)
        htables(2, i, 11) = allntn
c
c       All particles.
c
        htables(2, i, 12) = allchn + allntn
c
      enddo
c
      return
      end
c     --- End of routine compoundtab2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'observables.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
