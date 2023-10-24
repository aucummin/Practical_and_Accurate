c
c     FILE: ciopar.f                        Creation date: 27/NOV/1996.
c                                       LAST MODIFICATION: 16/JUL/2003.
c
c     This file contains the compressed i/o system related
c     parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     mxciorsize is the maximum record size
c     mxciofiles is the maximum number of compresssed files.
c     minciout is the starting value to generate the logical
c     unit for a compressed data file (i --> minciout + i)
c
      integer           mxciorsize, mxciofiles, minciout
c
      parameter         (mxciorsize = 4096)
      parameter         (mxciofiles = 3)
      parameter         (minciout   = 10)
c
c     mxciortype is the maximum number of different record types (not
c     counting the default record type).
c     mxciofields is the maximum number of fields within a record.
c
      integer           mxciortype, mxciofields
c
      parameter         (mxciortype      = 8)
      parameter         (mxciofields     = 30)
c
c     mxcioftypes is the maximum number of different fields available.
c     lastdefrecftype is the last field type that can be used within
c     the default record.
c     lastscaledtype1 is the last "integer coded" type of fields
c     lastscaledtype2 is the last "floating point coded" type of fields.
c
c     The remainding parameters define the different record types
c     available.
c
      integer           mxcioftypes, lastdefrecftype
      integer           lastscaledtype1, lastscaledtype2
c
      integer           intint4ftype, intint5ftype, int2ftype
      integer           fltflt5ftype, flt2ftype, flt5ftype
      integer           datiftype
c
      parameter         (intint4ftype    = 1)
      parameter         (intint5ftype    = 1 + intint4ftype)
      parameter         (int2ftype       = 1 + intint5ftype)
      parameter         (fltflt5ftype    = 1 + int2ftype)
      parameter         (flt2ftype       = 1 + fltflt5ftype)
      parameter         (flt5ftype       = 1 + flt2ftype)
      parameter         (datiftype       = 1 + flt5ftype)
c
      parameter         (mxcioftypes     = datiftype)
      parameter         (lastdefrecftype = flt2ftype)
      parameter         (lastscaledtype1 = int2ftype)
      parameter         (lastscaledtype2 = flt5ftype)
c
c     Parameters for dynamically added records.
c
      integer           dynadcountype, dynadftype
      integer           dynadcountlen, dynadflen
c
      parameter         (dynadcountype = int2ftype)
      parameter         (dynadftype    = flt5ftype)
      parameter         (dynadcountlen = 2)
      parameter         (dynadflen     = 5)
c
c     Parameters to manage coding and decoding.
c     Ideally, there are 256 different ascii characters. Unfortunately,
c     it is not possible to use the complete set if full portability
c     is desired. For this reason, a reduced set of 225 (15 ** 2)
c     characters is used. 225 is selected because it is the largest
c     square below 256.
c
      integer           codebase, codebaseh, codebase2
      parameter         (codebaseh = 15)
      parameter         (codebase  = codebaseh * codebaseh)
      parameter         (codebase2 = codebase * codebase)
c
      integer           codemaxh, codemax1, codemax1h
      integer           codemax2, codemax2h, codemaxm
      parameter         (codemaxh  = codebaseh - 1)
      parameter         (codemax1  = codebase - 1)
      parameter         (codemax1h = codebase * codebaseh - 1)
      parameter         (codemax2  = codebase2 - 1)
      parameter         (codemax2h = codebase2 * codebaseh - 1)
      parameter         (codemaxm  = 2147483647)
c
c     Floating point number coding and decoding related parameters.
c
c     Powers of the coding base.
c
      double precision  codebasep0, codebasep1, codebasep2, codebasep3
      double precision  codebasep4, codebasep5, codebasep6, codebasep7
      double precision  codebasep8, codebasep9, codebasepa, codebasepb
      double precision  codebasepc, codebasepd, codebasepe
c
      parameter         (codebasep0 = 1)
      parameter         (codebasep1 = codebaseh)
      parameter         (codebasep2 = codebaseh * codebasep1)
      parameter         (codebasep3 = codebaseh * codebasep2)
      parameter         (codebasep4 = codebaseh * codebasep3)
      parameter         (codebasep5 = codebaseh * codebasep4)
      parameter         (codebasep6 = codebaseh * codebasep5)
      parameter         (codebasep7 = codebaseh * codebasep6)
      parameter         (codebasep8 = codebaseh * codebasep7)
      parameter         (codebasep9 = codebaseh * codebasep8)
      parameter         (codebasepa = codebaseh * codebasep9)
      parameter         (codebasepb = codebaseh * codebasepa)
      parameter         (codebasepc = codebaseh * codebasepb)
      parameter         (codebasepd = codebaseh * codebasepc)
      parameter         (codebasepe = codebaseh * codebasepd)
c
      double precision  codebasen0, codebasen1, codebasen2, codebasen3
      double precision  codebasen4, codebasen5, codebasen6, codebasen7
      double precision  codebasen8, codebasen9, codebasena, codebasenb
      double precision  codebasenc, codebasend, codebasene
c
      parameter         (codebasen0 = 1)
      parameter         (codebasen1 = 1.d0 / codebasep1)
      parameter         (codebasen2 = 1.d0 / codebasep2)
      parameter         (codebasen3 = 1.d0 / codebasep3)
      parameter         (codebasen4 = 1.d0 / codebasep4)
      parameter         (codebasen5 = 1.d0 / codebasep5)
      parameter         (codebasen6 = 1.d0 / codebasep6)
      parameter         (codebasen7 = 1.d0 / codebasep7)
      parameter         (codebasen8 = 1.d0 / codebasep8)
      parameter         (codebasen9 = 1.d0 / codebasep9)
      parameter         (codebasena = 1.d0 / codebasepa)
      parameter         (codebasenb = 1.d0 / codebasepb)
      parameter         (codebasenc = 1.d0 / codebasepc)
      parameter         (codebasend = 1.d0 / codebasepd)
      parameter         (codebasene = 1.d0 / codebasepe)
c
c     Range limits.
c
      integer           iexpmaxh, iexpmaxhn, negshifth
      integer           iexpmax1, iexpmax1n, negshift1
c
      parameter         (iexpmaxh  = codemaxh / 2)
      parameter         (negshifth = iexpmaxh + 1)
      parameter         (iexpmaxhn = codemaxh - negshifth)
      parameter         (iexpmax1  = codemax1 / 2)
      parameter         (negshift1 = iexpmax1 + 1)
      parameter         (iexpmax1n = codemax1 - negshift1)
c
      double precision  cioflmax2, cioflmax2h, cioflmax5
      double precision  cioflmax2n, cioflmax2hn, cioflmax5n
      double precision  cioflmax2x, cioflmax2hx
c
      parameter         (cioflmax2   = (1 - codebasen3) *
     +                                 (codebasep1 ** iexpmaxh))
      parameter         (cioflmax2n  = (1 - codebasen3) *
     +                                 (codebasep1 ** iexpmaxhn))
      parameter         (cioflmax2x  = (1 - codebasen3) *
     +                                 (codebasep1 ** codemaxh))
      parameter         (cioflmax2h  = (1 - codebasen4) *
     +                                 (codebasep1 ** iexpmaxh))
      parameter         (cioflmax2hn = (1 - codebasen4) *
     +                                 (codebasep1 ** iexpmaxhn))
      parameter         (cioflmax2hx = (1 - codebasen4) *
     +                                 (codebasep1 ** codemaxh))
c
c     The maximum exponent for 5-byte floating point numbers is
c     15 ** 112 = 5.275E+131. This number can be out of range
c     in some machines. Should this happen, the following
c     bounds must be set to the maximum floating point number
c     that can be stored in the machine.
c
      parameter         (cioflmax5   = (1 - codebasen8) *
     +                                 (codebasep1 ** iexpmax1))
      parameter         (cioflmax5n  = (1 - codebasen8) *
     +                                 (codebasep1 ** iexpmax1n))
c
c     Rounding terms.
c
      double precision  ciofround2, ciofround2h, ciofround5
      double precision  codebasepe_r2, codebasepe_r2h
      double precision  cioflmax5_r
c
      parameter         (ciofround2     = codebasen3 / 2)
      parameter         (ciofround2h    = codebasen4 / 2)
      parameter         (ciofround5     = codebasen8 / 2)
      parameter         (codebasepe_r2  = (1 - ciofround2) *
     +                                    codebasepe)
      parameter         (codebasepe_r2h = (1 - ciofround2h) *
     +                                    codebasepe)
      parameter         (cioflmax5_r    = (1 - ciofround5) *
     +                                    cioflmax5)
c
c     uciologbase is the factor needed to calculate logarithms in base
c     codebase (currently set to 1 / ln(15)).
c
      double precision  uciologbase
      parameter         (uciologbase = 0.369269373068855045d0)
c
c     Particle coding.
c
c     The primary array for particle coding is defined in the Aires
c     particle processing related files.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'ciopar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
