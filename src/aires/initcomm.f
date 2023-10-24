c
c     FILE: initcomm.f                      Creation date: 19/JUN/1996.
c                                       LAST MODIFICATION: 01/MAY/2003.
c
c     This file contains the definitions of the variables that are
c     processed during the initialization phase.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2001,
c                                         2003.
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
c     shinidati   date and time of beginning of current shower
c                 (integer format)
c
      double precision  cpu0(4)
      integer           shinidati(6)
      character*20      datistr0(4)
c
      common            /maintimes/ cpu0, shinidati, datistr0
c
c     User and host information.
c
      character*24      cruser
      character*64      crhost
      integer           cruserlen, crhostlen
c
      common            /userandhost/ cruserlen, crhostlen,
     +                                cruser, crhost
c
c     Compilation information.
c
      character*11      compildate
      character*48      compilwho
      character*8       compilstar
c
      common            /compildata/ compilwho, compilstar, compildate
c
c     Original version information. The original version is the
c     current program version for simulation programs, but may differ
c     for summary programs (or in converted files).
c
      character*24      original_version
      integer           nverchanges
      logical           oversionset
c
      common            /oversion_c/ oversionset, nverchanges,
     +                               original_version
c
c     Variables without default values, and related parameters.
c
      double precision  cpuperrun
      integer           processjobs
      integer           mtotshowers, runshowers, firstshowernon1
      integer           totshwset, runshwset, firstshowerset
      integer           cpurunset, processjobsset
      integer           processruns
      logical           checkcpu, checknsh
c
      common            /mandatory1/ cpuperrun, processjobs,
     +                               mtotshowers, runshowers,
     +                               firstshowernon1, firstshowerset,
     +                               totshwset, runshwset,
     +                               cpurunset, processjobsset,
     +                               processruns,
     +                               checkcpu, checknsh
c
      integer           nshprimary, sprimlog
      logical           recordspp
      integer           shprimary(maxshprimaries)
      double precision  shprimarywt(maxshprimaries)
      double precision  shprimarywt0(maxshprimaries)
      integer           pryeminset
      double precision  pryenergymin
c
      common            /mandatory2a/ pryenergymin, pryeminset
      common            /mandatory2b/ shprimarywt, shprimarywt0,
     +                                shprimary, nshprimary,
     +                                sprimlog, recordspp
c
c     External model name control.
c
      integer           nforcextset
      logical           notforcext
c
      common            /forcext_c/ nforcextset, notforcext
c
c     Auxiliary variables for number of showers.
c
      double precision  xcpuperrun
      integer           xprocessjobs
      integer           xtotshowers, xrunshowers
      integer           xtotshwset, xrunshwset
      integer           xcpurunset, xprocessjobsset
      integer           lastshower, currshower
      integer           jobnumber, processnumber
      logical           shcomplete
c
      common            /auxsh_c/ xcpuperrun, xprocessjobs,
     +                            xtotshowers, xrunshowers,
     +                            xtotshwset, xrunshwset,
     +                            xcpurunset, xprocessjobsset,
     +                            lastshower, currshower,
     +                            jobnumber, processnumber,
     +                            shcomplete
c
c     Main input data arrays and equivalences.
c
      integer           nfidata, niidata, nlidata, nsidata
      integer           lastdcode(10)
      integer           nfidata0, niidata0, nlidata0, nsidata0
      integer           fdbdry(mxfidata), idbdry(mxiidata)
      double precision  fidata(0:3, mxfidata)
      integer           iidata(0:3, mxiidata)
      logical           lidata(0:2, mxlidata)
      integer           sidata(0:6, mxsidata)
      character*(mxsil) sidatastring
      integer           sidatalen, sidatalen0
c
      integer           fidatawaset(mxfidata)
      integer           iidatawaset(mxiidata)
      integer           lidatawaset(mxlidata)
      integer           sidatawaset(mxsidata)
      logical           usedefault, usedefaultd, usedefaults
c
      common            /initd1_c/ fidata, fidatawaset, fdbdry,
     +                             iidata, iidatawaset, idbdry,
     +                             lidata, lidatawaset,
     +                             nfidata, niidata, nlidata,
     +                             nfidata0, niidata0, nlidata0,
     +                             lastdcode, usedefault,
     +                             usedefaultd, usedefaults
c
      common            /initdstr_c/ sidata, sidatawaset,
     +                               nsidata, nsidata0,
     +                               sidatalen, sidatalen0,
     +                               sidatastring
c
c     Equivalencing with variables to facilitate usage.
c
      double precision  pryenergymax, pryenergyslp
      equivalence       (pryenergymax  , fidata(0,  1))
      equivalence       (pryenergyslp  , fidata(0,  2))
      double precision  pryzenithmin, pryzenithmax
      double precision  pryazimmin, pryazimmax
      equivalence       (pryzenithmin  , fidata(0,  3))
      equivalence       (pryzenithmax  , fidata(0,  4))
      equivalence       (pryazimmin    , fidata(0,  5))
      equivalence       (pryazimmax    , fidata(0,  6))
      double precision  ethinpar
      equivalence       (ethinpar      , fidata(0,  7))
      double precision  injz, injdepth
      equivalence       (injz          , fidata(0,  8))
      equivalence       (injdepth      , fidata(0,  9))
      double precision  groundz, groundepth
      equivalence       (groundz       , fidata(0, 10))
      equivalence       (groundepth    , fidata(0, 11))
      double precision  eminhis, emaxhis
      equivalence       (eminhis       , fidata(0, 12))
      equivalence       (emaxhis       , fidata(0, 13))
      double precision  rminhis, rmaxhis
      equivalence       (rminhis       , fidata(0, 14))
      equivalence       (rmaxhis       , fidata(0, 15))
      double precision  rminfile1, rmaxfile1
      equivalence       (rminfile1     , fidata(0, 16))
      equivalence       (rmaxfile1     , fidata(0, 17))
      double precision  rminfile2, rmaxfile2
      equivalence       (rminfile2     , fidata(0, 18))
      equivalence       (rmaxfile2     , fidata(0, 19))
      double precision  rminfile3, rmaxfile3
      equivalence       (rminfile3     , fidata(0, 20))
      equivalence       (rmaxfile3     , fidata(0, 21))
c
c     Elements fidata(*, i) i = 22, 34 are reserved for future use,
c     in connection with compressed file parameters.
c
      double precision  geob, geobi, geobd, geobfluc
      equivalence       (geob          , fidata(0, 35))
      equivalence       (geobi         , fidata(0, 36))
      equivalence       (geobd         , fidata(0, 37))
      equivalence       (geobfluc      , fidata(0, 38))
c
      double precision  eventdate
      equivalence       (eventdate     , fidata(0, 39))
c
c     Elements fidata(*, i) i = 40, 49 are reserved for future use.
c     The first 6 pairs correspond to compressed file parameters.
c     Element 50 is reserved for the random number generator seed.
c
      double precision  inputrseed
      equivalence       (inputrseed    , fidata(0, 50))
c
      integer           varzendis
      equivalence       (varzendis     , iidata(0,  1))
      integer           ciosplitevery
      equivalence       (ciosplitevery , iidata(0,  2))
      integer           isite
      equivalence       (isite         , iidata(0,  3))
      integer           geobswitch
      equivalence       (geobswitch    , iidata(0,  4))
c
      logical           geognorth
      equivalence       (geognorth     , lidata(0,  1))
      logical           thinningon
      equivalence       (thinningon    , lidata(0,  2))
c
c     Auxiliary shower parameters.
c
      double precision  mincutegy, minethin
      integer           atmlayer0
c
      common            /auxshpar_c/ mincutegy, minethin, atmlayer0
c
c     Input/Output file names related data.
c
      character*96      wdirname(mxwdir)
      integer           wdirnamelen(mxwdir)
      character*184     leadingfn(mxwdir)
      integer           leadingfnlen(mxwdir)
      character*88      leadingf0
      integer           leadingf0len
      character*256     inputsearchpath
      integer           searchpathlen
      character*64      taskname
      integer           tasknamelen
      integer           tasknameset, tasknamever
      logical           inpcheckonly, runforcein
c
      common            /filenames_c1/ wdirnamelen, leadingfnlen,
     +                                 leadingf0len, searchpathlen,
     +                                 tasknamelen, tasknameset,
     +                                 tasknamever,
     +                                 inpcheckonly, runforcein,
     +                                 wdirname, taskname, leadingfn,
     +                                 leadingf0, inputsearchpath
c
c     Auxiliary Input/Output file names related data.
c
      character*96      xwdirname
      integer           xwdirnamelen
      character*64      xtaskname
      integer           xtasknamelen
      integer           xtasknamever
      integer           xtasknameset
c
      common            /auxfi_c1/ xwdirnamelen, xtasknamelen,
     +                             xtasknamever, xtasknameset,
     +                             xwdirname, xtaskname
c
c     Other auxiliary data related to files and input data.
c
      logical           lgfisclosed, mmacutok, ciofsplit
      character*16      randomfnh
      character*26      rmkfn, shzfn, auxfn
      character*8       sryfn, texfn
      character*184     auxfilestring
      character*384     auxline
      character*512     longauxline
c
      common            /auxfi_c2/ lgfisclosed, mmacutok, ciofsplit,
     +                             randomfnh, rmkfn, shzfn, auxfn,
     +                             sryfn, texfn,
     +                             longauxline, auxline,
     +                             auxfilestring
c
c     Tracing and input file nesting related variables.
c
      integer           inpunit, inpnesl, nilines(0:maxinpnl)
      common            /inputfile_c/ inpunit, inpnesl, nilines
c
      logical           traceison, promptison, iprompt
      Common            /tracesw_c/ traceison, promptison, iprompt
c
c     Input directive names and related variables.
c
      integer           ncommands, ncommands0
      character*(mxcdl) cname(maxccodes)
      character*27      clgname(maxccodes)
      integer           ccode(maxccodes), minclen(maxccodes)
      integer           aditem(maxccodes)
      logical           veryimpdir(maxccodes)
      logical           wngonset(maxccodes)
c
      common            /command_c/ ncommands, ncommands0,
     +                              ccode, minclen,
     +                              aditem, veryimpdir, wngonset,
     +                              cname, clgname
c
c     Global variables and related data.
c
      integer           nglobar(2)
      character*(mxcdl) globnam(mxglobar, 2)
      integer           globlen(mxglobar, 2)
      integer           globdfend(0:mxglobar, 2)
      character*(mxgll) globarstring(2)
      integer           globstrlen(2)
      logical           bracketson, thereareglobars
      character*1       opbr, clbr, brec
c
      common            /globar_c/ bracketson, thereareglobars,
     +                             nglobar, globdfend,
     +                             globlen, globstrlen,
     +                             globnam, globarstring,
     +                             opbr, clbr, brec
c
c     Library of sites.
c
      integer           nlibsites
      character*16      sitename(0:mxlibsites)
      integer           sitenlen(0:mxlibsites)
      double precision  sitelat(0:mxlibsites)
      double precision  sitelong(0:mxlibsites)
      double precision  siteground(0:mxlibsites)
c
      common            /libsites_c0/ sitename, sitenlen, nlibsites
      common            /libsites_c1/ sitelat, sitelong, siteground
c
c     Variables for output control, and related.
c
      integer           idlerrsev, idlerrsev2
      logical           fullinputlist, fulloutputlist,
     +                  sryison, tssison,
     +                  adfile, stackinfo, latexsry
c
      common            /outputcontrol_c/
     +                    idlerrsev, idlerrsev2,
     +                    fullinputlist, fulloutputlist,
     +                    sryison, tssison,
     +                    adfile, stackinfo, latexsry
c
c     Atmospheric model related data.
c
      integer           atmoslabel, atmoslset
      character*42      atmosmodel
c
      common            /atmoslabel_c/ atmoslabel, atmoslset,
     +                                 atmosmodel
c
c     File indices for the compressed output files.
c
      integer           gpcleciofile, longiciofile, traceciofile
c
      common            /cofind_c/ gpcleciofile, longiciofile,
     +                             traceciofile
c
c     Auxiliary compressed file parameters.
c
      double precision  rminfile(9), rmaxfile(9)
      double precision  r2minfile(9), r2maxfile(9)
c
      common            /ciofindp_c/ rminfile, rmaxfile,
     +                               r2minfile, r2maxfile
c
c     Variables for rough sampling of particles.
c
      double precision  prsratio0, prssqratio
c
      common            /cioroughs_c/ prsratio0, prssqratio
c
c     Miscellaneous.
c
      logical           remark
      integer           ninierrors
c
      common            /miscinit_c/ remark, ninierrors
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'initcomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
