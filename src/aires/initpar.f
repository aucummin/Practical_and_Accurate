c
c     FILE: initpar.f                       Creation date: 18/JUN/1996.
c                                       LAST MODIFICATION: 23/NOV/2001.
c
c     Parameter for the data initializing and input routines.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 1999, 2001.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     hyperemax is the global maximum energy for a primary (GeV).
c     superemin is the global minimum primary energy (GeV).
c     These parameters are introduced to ensure that the primary energy
c     is safely bounded.
c     The current settings are 500 MeV and 10^23 eV (10^14 GeV).
c
      double precision  superemin, hyperemax
      parameter         (superemin = 0.5d0, hyperemax = 1.0d14)
c
c     mxidir is the maximum number of hardwired input directives.
c     mxidatdir is the maximum number of input data directives.
c     mxcdl is the maximum number of characters for a command
c           identifier and/or a global variable.
c
      integer           mxidir, mxidatdir, mxcdl
      parameter         (mxidir = 50, mxidatdir = 100)
      parameter         (mxcdl = 16)
c
c     The command arrays are sized accordingly with "maxccodes"
c
      integer           maxccodes
      parameter         (maxccodes = mxidir + mxidatdir)
c
c     mxglobar is the maximum number of global variables.
c     mxgll is the maximum number of characters of all the definitions
c     of global variables.
c
      integer           mxglobar, mxgll
      parameter         (mxglobar = 640)
      parameter         (mxgll    = 32 * mxglobar)
c
c     maxshprimaries is the maximum number of primaries accepted.
c
      integer           maxshprimaries
      parameter         (maxshprimaries = 32)
c
c     mxfidata is the maximum number of real input data
c     mxiidata is the maximum number of integer input data
c     mxlidata is the maximum number of logical input data.
c     mxsidata is the maximum number of string input data.
c
      integer           mxfidata, mxiidata, mxlidata, mxsidata
      parameter         (mxfidata = 128, mxiidata = 72)
      parameter         (mxlidata = 40,  mxsidata = 24)
c
c     mxsil is the maximum length of the character data string.
c     mxwdir is the (maximum) number of file directories.
c
      integer           mxsil, mxwdir
      parameter         (mxsil  = 32 * mxsidata)
      parameter         (mxwdir = 4)
c
c     Comment and label markers.
c
      character*1       comchar, labelchar, pathsep
      parameter         (comchar = '#', labelchar = '&')
      parameter         (pathsep = ':')
c
c     Default brackets.
c
      character*1       defopbr, defclbr
      parameter         (defopbr = '{', defclbr = '}')
c
c     Input unit limits for the Input command.
c
      integer           minmacunit, maxmacunit
      parameter         (minmacunit = 60, maxmacunit = 69)
      integer           maxinpnl
      parameter         (maxinpnl = maxmacunit - minmacunit + 1)
c
c     Logical i/o internal units for internal files.
c
      integer           rmkut, shzut
      parameter         (rmkut = 58)
      parameter         (shzut = 20)
c
c     Site library related parameters.
c
      integer           mxlibsites
      parameter         (mxlibsites = 24)
c
c     Extensions used for the output files.
c
      character*4       lgfext, idfext, adfext, tssext, sryext, texext
      parameter         (lgfext = '.lgf')
      parameter         (idfext = '.idf')
      parameter         (adfext = '.adf')
      parameter         (tssext = '.tss')
      parameter         (sryext = '.sry')
      parameter         (texext = '.tex')
c
c     Error names used in these routines:
c
      character*(*)     irerr
      parameter         (irerr = '$A05')
c
c     Parameters for rough sampling of particles.
c
      double precision  prsratio0def
c
      parameter         (prsratio0def = 10)
c
c     Miscellaneous parameters
c
c     LaTeX size parameters (MUST be > 0 !!)
c
      integer           texheighmm, ntexlines
      double precision  texbaselinemm
      parameter         (texheighmm = 220, texbaselinemm = 4.0)
      parameter         (ntexlines = texheighmm / texbaselinemm)
c
      character*1       bs
      parameter         (bs = '\\')
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'initpar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
