c
c     FILE: pstackpar.f                     Creation date: 12/JUN/1996.
c                                       LAST MODIFICATION: 06/MAR/2002.
c
c     This file contains the particle stack (buffer) related
c     parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2002.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     maxnsta is the maximum number of stacks.
c     maxstalen is the amount of data at each stack entry, in
c     eight byte units (defined below, after the detailed section).
c     nfspares is the number of real spare positions.
c     nispares8 is half of the integer/logical spare positions.
c
      integer           maxnsta, nfspares, nispares8
c
c     By now "maxnsta" is set equal to "npstacks"
c
      include 'kernelpar.f'
c
      parameter         (maxnsta       = npstacks)
      parameter         (nfspares      = 5)
      parameter         (nispares8     = 1)
c
c     Spare positions derived parameters.
c
      integer           nfspares1, nispares, nispares1
      parameter         (nfspares1 = nfspares - 1)
      parameter         (nispares  = 2 * nispares8)
      parameter         (nispares1 = nispares - 1)
c
c     Detailed positions within the particle record.
c
      integer           ixenergy, ixbeta, ixwt
      integer           ixx, ixy, ixz, ixvz, ixdepth
      integer           ixtime, ixpx, ixpy, ixpz
      integer           ixpxpr, ixpypr, ixpzpr
      integer           ixcdepth, ixctime, ixlhdepth
      integer           ixf0
c
      integer           ixpcode, ixlol, ixlolsa, ixcal
      integer           ixlowe
      integer           ixil0
c
c     Double precision variables (length = 8).
c     The last identifier represents the position of the first element
c     of an array of dimension nfspares.
c     The positions of ixx, ixy, ixz and ixvz MUST be consecutive.
c
      parameter         (ixenergy  = 1)
      parameter         (ixwt      = ixenergy  + 1)
      parameter         (ixbeta    = ixwt      + 1)
      parameter         (ixx       = ixbeta    + 1)
      parameter         (ixy       = ixx       + 1)
      parameter         (ixz       = ixy       + 1)
      parameter         (ixvz      = ixz       + 1)
      parameter         (ixdepth   = ixvz      + 1)
      parameter         (ixtime    = ixdepth   + 1)
      parameter         (ixpx      = ixtime    + 1)
      parameter         (ixpy      = ixpx      + 1)
      parameter         (ixpz      = ixpy      + 1)
      parameter         (ixpxpr    = ixpz      + 1)
      parameter         (ixpypr    = ixpxpr    + 1)
      parameter         (ixpzpr    = ixpypr    + 1)
      parameter         (ixcdepth  = ixpzpr    + 1)
      parameter         (ixctime   = ixcdepth  + 1)
      parameter         (ixlhdepth = ixctime   + 1)
      parameter         (ixf0      = ixlhdepth + 1)
c
c     Integer or logical variables (length = 4).
c     The last identifier represents the position of the first element
c     of an array of dimension nispares.
c     The TOTAL number of integer+logical fields (not including the
c     spare fields) MUST BE EVEN.
c
      parameter         (ixpcode  = 2 * (ixf0 + nfspares1) + 1)
      parameter         (ixlol    = ixpcode  + 1)
      parameter         (ixlolsa  = ixlol    + 1)
      parameter         (ixcal    = ixlolsa  + 1)
c
      parameter         (ixlowe   = ixcal    + 1)
      parameter         (ixil0    = ixlowe   + 2)
c
c     Total length of the particle record.
c     (Evaluated using the last parameter defined in the detailed
c     parameter position section).
c
      integer           maxstalen
      parameter         (maxstalen = (ixil0 + nispares1) / 2)
c
c     mxmidlev is the maximum number of "processing levels" to be
c     defined, that is, the number of alternative ways to partition the
c     stacks for sequential processing.
c     minmidentries is the minimum number of entries allowed for
c     sequential processing.
c     midlevratio is the reduction factor to use when advancing the
c     "processing level".
c
      integer           mxmidlev, minmidentries
      double precision  midlevratio
c
      parameter         (mxmidlev      = 7)
      parameter         (minmidentries = 40)
      parameter         (midlevratio   = 0.45d0)
c
c     maxstaentries is the maximum number of entries in the fast stacks.
c     It is set accordingly with the amount of available stack
c     space (depends on the hardware), defined in parameter
c     "stackb" (file "_stackspec.f"), and with maxstalen.
c     ioreclunit is the size (in bytes) of the unit used to define
c     the direct access file record sizes. Normally the unit is
c     the character, ioreclunit = 1 (this is the FORTRAN 77 standard),
c     but some systems use other nonstandard units (for example
c     old VAX/VMS compilers).
c
      integer           stackb, ioreclunit
c
      include '_stackspec.f'
c
      integer           maxstaentries
c
      parameter         (maxstaentries = (128 * stackb) / maxstalen)
c
c     minstaentries is the minimum number of entries for a single stack.
c     maxentriesbig is the maximum number of entries to use when
c     setting arrays "littlestasize" and "bigstasize", and when
c     determining the parameters of the stack "processing levels".
c     plushentries is an indicative of the number of overflow records
c     used to set the "red light" in the stack size controlling system.
c
      integer           minstaentries, maxentriesbig, plushentries
c
      parameter         (minstaentries = 100 + maxstaentries / 60)
      parameter         (maxentriesbig = 8000)
      parameter         (plushentries  = 100)
c
c     littlesizefrac and bigsizefrac are two real numbers between 0 and
c     1 that define the filling fractions used to switch red and green
c     lights.
c
      double precision  littlesizefrac, bigsizefrac
c
      parameter         (littlesizefrac = 0.20d0)
      parameter         (bigsizefrac    = 0.50d0)
c
c     pstaiou0 is the logical i/o unit parameter. Hard stack i
c     is connected to logical unit pstaiou0 + i
c     hpstarecl in the direct i/o file record length. This
c     parameter is set using parameter ioreclunit.
c
      integer           pstaiou0, hpstarecl
      parameter         (pstaiou0 = 70)
      parameter         (hpstarecl = (8 * maxstalen) / ioreclunit)
c
c     Error codes used in the package:
c
      character*(*)     operr, rderr, wrerr, alerr
      parameter         (operr = '$A01', wrerr = '$A02')
      parameter         (rderr = '$A03', alerr = '$A04')
c
c     Other derived parameters:
c
      integer           mxbch, maxstalen4
      parameter         (mxbch = 8 * maxstalen)
      parameter         (maxstalen4 = 2 * maxstalen)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'pstackpar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
