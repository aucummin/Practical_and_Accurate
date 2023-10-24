c
c     FILE: ciocomm.f                       Creation date: 27/NOV/1996.
c                                       LAST MODIFICATION: 06/AUG/2003.
c
c     This file contains the compresses i/o system common definitions.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000, 2001, 2003.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Particle output file running numbers, extensions and brief
c     description.
c
      integer           npofiles
      integer           pofileno(mxciofiles)
      character*12      pofilext(mxciofiles)
      character*64      pofiletitle(mxciofiles)
c
      common            /ciofile_c1/ npofiles, pofileno,
     +                               pofilext, pofiletitle
c
c     File record definition arrays.
c
      integer           ciochecksum(mxciofiles)
      integer           ciobuflen(mxciofiles)
      integer           nrectypes(mxciofiles)
      integer           cioreclen(0:mxciortype, mxciofiles)
      integer           cioreclen00(0:mxciortype, mxciofiles)
      integer           ciofillpoint(0:mxciortype, mxciofiles)
      integer           totrecfields(0:mxciortype, mxciofiles)
      integer           nrecfield(mxcioftypes, 0:mxciortype,
     +                            mxciofiles)
      integer           startfield(mxcioftypes, 0:mxciortype,
     +                            mxciofiles)
      character*42      ciorecname(0:mxciortype, mxciofiles)
c
      integer           cioffscaled(0:mxciortype, mxciofiles)
      integer           ciolfnoscal(0:mxciortype, mxciofiles)
      integer           ciolfscale1(0:mxciortype, mxciofiles)
      integer           cionfscale2(0:mxciortype, mxciofiles)
      logical           ciodynrecty(0:mxciortype, mxciofiles)
      integer           ciodynfield(0:mxciortype, mxciofiles)
      integer           ciodynfwcix(0:mxciortype, mxciofiles)
c
      double precision  ciofminv(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofmaxv(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofwsc0(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofwsc1(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofrsc0(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofrsc1(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofwca(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofwcb(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofrca(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      double precision  ciofrcb(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      logical           cioflogs(mxciofields, 0:mxciortype,
     +                            mxciofiles)
      character*42      ciofname(mxciofields, 0:mxciortype,
     +                            mxciofiles)
c
      common            /ciofile_c2/ ciochecksum,
     +                               ciobuflen, nrectypes,
     +                               cioreclen, cioreclen00,
     +                               ciofillpoint,
     +                               totrecfields,  nrecfield,
     +                               startfield
c
      common            /ciofile_c3/ ciofminv, ciofmaxv,
     +                               ciofwsc0, ciofwsc1,
     +                               ciofrsc0, ciofrsc1,
     +                               cioffscaled, ciolfnoscal,
     +                               ciolfscale1, cionfscale2,
     +                               ciodynrecty,
     +                               ciodynfield, ciodynfwcix
c
      common            /ciofile_c4/ ciofwca, ciofwcb,
     +                               ciofrca, ciofrcb, 
     +                               cioflogs, ciorecname, ciofname
c
c     Ascii character set array (local machine).
c
      integer           ciointdigits(-256:255)
      character*1       ciochrdigits(0:codemax1)
      character*1       ascarrayprt(-256:255)
      character*1       numalchr(2, 3)
      character*1       spechr(32)
c
      common            /cio_c0/ ciointdigits, ciochrdigits,
     +                           ascarrayprt, numalchr, spechr
c
c     Auxiliary ascii arrays (local machine).
c
      integer           numalseq0(2, 3)
      integer           speseq0(32)
c
      common            /cio_c0a/ numalseq0, speseq0
c
c     Auxiliary data to code and decode integer and floating point
c     numbers.
c
      double precision  ciofexponentsh2(0:iexpmax1 + 2)
      integer           halfandhalfi1(0:codemax1)
      integer           halfandhalfi2(0:codemax1)
      character*1       halfandhalfch(0:codemaxh, 0:codemaxh)
c
      common            /cio_c1/ ciofexponentsh2,
     +                           halfandhalfi1, halfandhalfi2,
     +                           halfandhalfch
c
c     Lengths of different field types available, and
c     related data.
c
      integer           ciofieldid(mxcioftypes)
      integer           ciofieldlen(mxcioftypes)
      integer           nsubfields(mxcioftypes)
      integer           maxfieldint(mxcioftypes)
      double precision  maxfieldflt(mxcioftypes)
      double precision  maxfieldfltn(mxcioftypes)
      double precision  maxfieldfltx(mxcioftypes)
c
      common            /cio_c2/ maxfieldflt,
     +                           maxfieldfltn, maxfieldfltx,
     +                           ciofieldid, ciofieldlen,
     +                           nsubfields, maxfieldint
c
c     Particle codes encoding related variables.
c
      integer           ciopclecodedata(3)
c
      common            /cio_pc1/ ciopclecodedata
c
c     Record buffer(s) and related data.
c
      integer           cioinitlock
      logical           ciopen(mxciofiles)
      logical           cioscra(mxciofiles)
      integer           cioreclast(mxciofiles)
      integer           ciorlastcp(mxciofiles)
      integer           ciowblocks(mxciofiles)
      character*(mxciorsize) ciorecord1(mxciofiles)
      character*(mxciorsize) ciorecord2(mxciofiles)
      character*(mxciorsize) ciorecord0
c
      common            /cio0_c/ cioinitlock, ciopen, cioscra,
     +                           cioreclast, ciorlastcp, ciowblocks
      common            /cio0_buf/ ciorecord1, ciorecord2, ciorecord0
c
c     Integrity checks related data.
c
      logical           mustcheckcio
c
      common            /cio0_check/ mustcheckcio
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file ciocomm.f
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
