c
c     FILE: startstop.f                     Creation date: 19/DEC/1996.
c                                       LAST MODIFICATION: 10/APR/2003.
c
c     This file contains the basic Aires initialization/shutdown
c     routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine basicstart0
c
c     Initializations to be performed at the very beginning of every
c     Aires related program.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           r0
      double precision  clockrandom
c
c     FIRST EXECUTABLE STATEMENT
c
c     Starting the unit conversion, particle data systems, etc.
c
      call setunits
      call inipcledata
c
c     Internal file names.
c
      r0 = 10000 * clockrandom()
      write(randomfnh, 2010) '__Aires', pgmcode, '_', r0
 2010 format(2(a, i4.4))
c
      rmkfn = randomfnh // '.rmks_TMP'
      shzfn = randomfnh // '.shwz_TMP'
      auxfn = randomfnh // '.TMP_'
c
c     Other initializations.
c
      adfnmergedfiles = 0
c
      return
      end
c     --- End of routine basicstart0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine airesshutdown
c
c     Tasks to be performed before stopping any of the AIRES programs.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Deleting internal files and closing the log file.
c
      call rmfile(shzut, shzfn)
      if (remark) call rmfile(rmkut, rmkfn)
      call closelog
c
      return
      end
c     --- End of routine airesshutdown
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'startstop.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
