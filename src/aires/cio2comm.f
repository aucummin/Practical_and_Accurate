c
c     FILE: cio2comm.f                      Creation date: 09/DEC/1996.
c                                       LAST MODIFICATION: 16/JUL/2002.
c
c     This file contains the compresses i/o system common definitions.
c     Part 2: Additional varibles used for processing already created
c     compressed files.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000, 2001, 2002,
c                                         2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Particle input file names and related variables
c
      integer           npifiles(mxcio2files)
      integer           rcversion(mxcio2files)
      integer           pifiledefno(mxcio2files)
      character*180     pifilename(mxcio2files)
      character*12      pifilext(mxcio2files)
      character*64      pifiletitle(mxcio2files)
      character*11      cofcdate(mxcio2files)
      character*48      cofcwho(mxcio2files)
      character*8       cofcstar(mxcio2files)
c
      common            /cio2file_c1/ npifiles, rcversion, pifiledefno,
     +                                pifilename, pifilext,
     +                                pifiletitle,
     +                                cofcwho, cofcstar, cofcdate
c
c     File record definition arrays.
c
      integer           cio2buflen(mxcio2files)
      integer           nrectypes2(mxcio2files)
      integer           cio2reclen(0:mxciortype, mxcio2files)
      integer           cio2reclen0(0:mxciortype, mxcio2files)
      integer           cio2fillpoint(0:mxciortype, mxcio2files)
      integer           totrecfields2(0:mxciortype, mxcio2files)
      integer           nrecfield2(0:mxcioftypes, 0:mxciortype,
     +                            mxcio2files)
      integer           startfield2(0:mxcioftypes, 0:mxciortype,
     +                            mxcio2files)
      character*42      cio2recname(0:mxciortype, mxciofiles)
c
      integer           cio2ffscaled(0:mxciortype, mxcio2files)
      integer           cio2lfnoscal(0:mxciortype, mxcio2files)
      integer           cio2lfscale1(0:mxciortype, mxcio2files)
      integer           cio2nfscale2(0:mxciortype, mxcio2files)
      logical           cio2dynrecty(0:mxciortype, mxcio2files)
      integer           cio2dynfield(0:mxciortype, mxcio2files)
      integer           cio2dynfwcix(0:mxciortype, mxcio2files)
c
      double precision  cio2fminv(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      double precision  cio2fmaxv(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      double precision  cio2fwsc0(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      double precision  cio2fwsc1(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      double precision  cio2frsc0(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      double precision  cio2frsc1(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      double precision  cio2fwca(mxciofields, 0:mxciortype,
     +                           mxcio2files)
      double precision  cio2fwcb(mxciofields, 0:mxciortype,
     +                           mxcio2files)
      double precision  cio2frca(mxciofields, 0:mxciortype,
     +                           mxcio2files)
      double precision  cio2frcb(mxciofields, 0:mxciortype,
     +                           mxcio2files)
      logical           cio2flogs(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      character*42      cio2fname(mxciofields, 0:mxciortype,
     +                            mxcio2files)
c
      common            /cio2file_c2/ cio2buflen, nrectypes2,
     +                                cio2reclen, cio2reclen0,
     +                                cio2fillpoint,
     +                                totrecfields2,  nrecfield2,
     +                                startfield2
c
      common            /cio2file_c3/ cio2fminv, cio2fmaxv,
     +                                cio2fwsc0, cio2fwsc1,
     +                                cio2frsc0, cio2frsc1,
     +                                cio2ffscaled, cio2lfnoscal,
     +                                cio2lfscale1, cio2nfscale2,
     +                                cio2dynrecty,
     +                                cio2dynfield, cio2dynfwcix
c
      common            /cio2file_c4/ cio2fwca, cio2fwcb,
     +                                cio2frca, cio2frcb, 
     +                                cio2flogs,
     +                                cio2recname, cio2fname
c
c     Always positive numbers switchs.
c
      logical           cio2alwpf2(mxciofields, 0:mxciortype,
     +                            mxcio2files)
      logical           cio2alwpff5(mxciofields, 0:mxciortype,
     +                              mxcio2files)
c
      common            /cio2file_c5/ cio2alwpf2, cio2alwpff5
c
c     Additional data for read-only processing.
c
      integer           cio2firstalt(mxcio2files)
      integer           cio2nullcode(mxcio2files)
c
      common            /cio2file_c9/ cio2firstalt, cio2nullcode
c
c     Ascii character set array (old files).
c
      logical           normalcharseq2(mxcio2files)
      logical           normalprtchar2(mxcio2files)
      integer           cio2intdigits(-256:255, mxcio2files)
      character*1       cio2chrdigits(0:codemax1, mxcio2files)
      character*1       ascarrayprt2(-256:255, mxcio2files)
      character*1       numalchr2(2, 3, mxcio2files)
      character*1       spechr2(32, mxcio2files)
c
      common            /cio2_c0/ normalcharseq2, normalprtchar2,
     +                            cio2intdigits, cio2chrdigits,
     +                            ascarrayprt2,
     +                            numalchr2, spechr2
c
c     Logarithm base constants.
c
      integer           filelogbase(mxcio2files)
      double precision  filelbcf(mxcio2files)
c
      common            /cio2_lbc/ filelbcf, filelogbase
c
c     Particle decoding array.
c
      integer           cio2pcledecode(0:codemax1h)
      integer           currcodsys
c
      common            /cio2_pdc/ cio2pcledecode, currcodsys
c
c     Record buffer(s).
c
      integer           currciofile2, lastopciofile2, lastheadfile2
      logical           cio2pen(mxcio2files)
      integer           cio2recprev(mxcio2files)
      integer           cio2reclast(mxcio2files)
      integer           cio2recread(mxcio2files)
      character*(mxciorsize) cio2record1(mxcio2files)
c
      common            /cio20_c/ currciofile2, lastopciofile2,
     +                            lastheadfile2, cio2pen,
     +                            cio2recprev, cio2reclast,
     +                            cio2recread, cio2record1
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file cio2comm.f
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
