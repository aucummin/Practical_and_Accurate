c
c     FILE: initmergecomm.f                 Creation date: 18/AUG/2003.
c                                       LAST MODIFICATION: 19/AUG/2003.
c
c     This file contains the definitions of the variables that are
c     processed during the initialization phase, that are required by
c     the AiresMerge program.
c
c     Written by: S. J. Sciutto, Fermilab, 2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Time related variables.
c
c     cpu0(1) is the total CPU time since the beginning of the task.
c     cpu0(2) is the total CPU time up to the last completed shower.
c     cpu0(3) is a counter of the CPU time for the current shower.
c     cpu0(4) is a counter of the CPU time for the current run/shower.
c
c     datistr0(1) task beginning date and time.
c     datistr0(2) date and time of completion of last shower.
c     datistr0(3) date and time of completion of last run.
c     datistr0(4) current run beginning date and time.
c
c
      double precision  amcpu0(4)
      character*20      amdatistr0(4)
c
      common            /ammaintimes/ amcpu0, amdatistr0
c
c     Variables without default values, and related parameters.
c
      integer           ammtotshowers, ammtotshowersum
      integer           amtotshwset
c
      common            /ammandatory1/ ammtotshowers, ammtotshowersum,
     +                                 amtotshwset
c
c     Main input data arrays and equivalences.
c
      integer           amnfidata, amniidata, amnlidata, amnsidata
c
      common            /aminitd1_c/
     +                      amnfidata, amniidata, amnlidata, amnsidata
c
c     Input/Output file names related data.
c
      character*256     amwdirname
      integer           amwdirnamelen
      character*64      amtaskname
      integer           amtasknamelen
      integer           amtasknameset, amtasknamever
c
      common            /amfilenames_c1/
     +                      amtasknamelen, amtasknameset,
     +                      amtasknamever, amwdirnamelen,
     +                      amtaskname, amwdirname
c
c     Other auxiliary data related to files and input data.
c
      character*16      amrandomfnh
      character*26      amrmkfn, amshzfn, amauxfn
c
      common            /amauxfi_c2/ amrandomfnh,
     +                               amrmkfn, amshzfn, amauxfn
c
c     Input directive names and related variables.
c
      integer           amncommands
c
      common            /amcommand_c/ amncommands
c
c     Global variables and related data.
c
      integer           amnglobar(2)
c
      common            /amglobar_c/ amnglobar
c
c     Library of sites.
c
      integer           amnlibsites
c
      common            /amlibsites_c0/ amnlibsites
c
c     Variables specific to AiresMerge.
c
      integer           nmergedfiles
c
      common            /am_c0/ nmergedfiles
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'initcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
