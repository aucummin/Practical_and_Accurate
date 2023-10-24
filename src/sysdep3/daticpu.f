c
c     FILE: daticpu.f                       Creation date: 17/JAN/1997.
c                                       LAST MODIFICATION: 04/APR/1997.
c
c     Date, time and cputime routines. These routines can be compiled
c     in Silicon Graphics, and DEC ALPHA platforms.
c     NOTE: Some routines of this file contain calls to system
c           dependent routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intdati(idati)
c
c     Current date and time as integer numbers.
c     SYSTEM DEPENDENT CALLS INSIDE!
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     idati........... (output, integer, array(6)) Array containing
c                      the following data:
c                         idati(1) = year (yyyy).
c                         idati(2) = month (1 to 12)
c                         idati(3) = day (1 to 31)
c                         idati(4) = hour (1 to 24)
c                         idati(5) = minutes (0 to 59)
c                         idati(6) = seconds (0 to 59)
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           idati(6)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Calling the system dependent routines (SGI and DEC ALPHA Fortran)
c     that return date and time as a sequence of integers.
c
      call idate(idati(2), idati(3), idati(1))
c
      if (idati(1) .lt. 100) then
        if (idati(1) .gt. 80) then
          idati(1) = idati(1) + 1900
        else
          idati(1) = idati(1) + 2000
        endif
      endif
c
      call itime(idati(4))
c
      return
      end
c     --- End of routine intdati.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cputime(setflag, total, fromlast)
c
c     Total and partial processor times.
c     SYSTEM DEPENDENT CALL INSIDE!
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     setflag......... (input, logical) Logical flag to mark the
c                      beginning of CPU time counts (See fromlast).
c     total........... (output, real*8) Total CPU time, as returned
c                      by the system-dependent call.
c     fromlast........ (output, real*8) CPU time elapsed from the
c                      last call to "cputime" with "setflag" set to
c                      ".true.". This argument may contain an
c                      erroneous value the first time this routine
c                      is called.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           setflag
      real*8            total, fromlast
c
c     Declaration of shared data.
c
      real*8            lastcputime
      save              lastcputime
c
c     Declaration of internal variables and arrays.
c
      real*4            tottime, times2(2)
      real*4            etime
c
c     FIRST EXECUTABLE STATEMENT
c
c     System call (SGI and DEC Alpha Fortran).
c
      tottime = etime(times2)
c
      total   = tottime
c
      fromlast = total - lastcputime
      if (setflag) lastcputime = total
c
      return
      end
c     --- End of routine cputime.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'daticpu.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
