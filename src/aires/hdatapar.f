c
c     FILE: hdatapar.f                      Creation date: 17/JUL/1996.
c                                       LAST MODIFICATION: 05/MAY/2004.
c
c     Parameter file for the histogram and output data.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2001, 2004.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     mxobslevels is the maximum number of observing levels.
c     With the current configuration, it must be less or equal than
c     616. If larger than 511, it is needed to change the algorithm
c     associated with the crossed observing levels key.
c
      integer           mxobslevels, mxobslevelsp1
      parameter         (mxobslevels   = 511)
      parameter         (mxobslevelsp1 = mxobslevels + 1)
c
c     mxlhtable is the maximum number of longitudinal histograms
c     (tables). This parameter must never be greater than
c     (mxobslevels - 5) / 2.
c     mxldtable is the maximum number of lateral distribution
c     histograms (tables).
c     mxtdtable is the maximum number of time distribution histograms
c     (tables).
c     mxpshtable is the maximum number of "per shower" tables.
c     mxlitable is the maximum number of deposited energy (and related)
c     tables.
c
      integer           mxlhtable, mxldtable, mxtdtable, mxpshtable
      integer           mxlitable
      parameter         (mxlhtable  = 22)
      parameter         (mxldtable  = 22)
      parameter         (mxtdtable  =  8)
      parameter         (mxpshtable = mxlhtable + 3)
      parameter         (mxlitable  = 12)
c
c     tdtbins is the number of bins for lateral and time distribution
c     histograms.
c
      integer           tdbins
      parameter         (tdbins = 40)
c
c     mxxmaxd is the maximum number of Xmax data sets.
c
      integer           mxxmaxd
      parameter         (mxxmaxd  = 2)
c
c     nshff is the number of "families" of fields written in the
c     "per shower" data file. Currently is 3: Number of particles,
c     energies and unweighted entries.
c
      integer           nshff
      parameter         (nshff = 3)
c
c     mxprtexp is the maximum number of tables that can be printed
c     or exported.
c     mxpexopt is the maximum number of options.
c
      integer           mxprtexp, mxpexopt
      parameter         (mxprtexp = 120)
      parameter         (mxpexopt = 5)
c
c     Other derived parameters.
c
      integer           mxlhtable2, mxlitable2, tdbinsp1
      parameter         (mxlhtable2 = 2 * mxlhtable)
      parameter         (mxlitable2 = 2 * mxlitable)
      parameter         (tdbinsp1   = tdbins + 1)
c
c     mxfodata, mxiodata, mxlodata are the maximum number of real,
c     integer and logical data, respectively.
c     mxallodata is the maximum number of all output data items.
c
      integer           mxfodata, mxiodata, mxlodata
      integer           mxallodata
c
      parameter         (mxfodata = 10, mxiodata = 10)
      parameter         (mxlodata = 10, mxallodata = 20)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'hdatapar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
