c
c     FILE: daticpu.f                       Creation date: 18/JUN/1996.
c                                       LAST MODIFICATION: 28/OCT/1998.
c
c     Date, time and cputime routines.
c     NOTE: Some routines of this file contain calls to system
c           dependent routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intdati(idati)
c
c     Current date and time as integer numbers.
c     SYSTEM DEPENDENT CALL INSIDE!
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c     Parameters:
c     ==========
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
c     Declaration of internal variables and arrays.
c
      integer           i
      integer           idatilocal(6)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Calling a C routine to read the current date and time.
c     This routine is invoked using LOCAL variables. Using directly
c     the argument proves not to work properly under DEC ALPHA Linux,
c     and, of course, this seems to be a compiler bug.
c
      call ccidati_(idatilocal(1), idatilocal(2), idatilocal(3),
     +              idatilocal(4), idatilocal(5), idatilocal(6))
c
c     Transferring the data to the argument.
c
      do i = 1, 6
        idati(i) = idatilocal(i)
      enddo
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
c     Written by: S. J. Sciutto, La Plata 1996.
c
c     Parameters:
c     ==========
c
c     setflag......... (input, logical) Logical flag to mark the
c                      beginning of CPU time counts (See fromlast).
c     total........... (output, double precision) Total CPU time, as
c                      returned by the system-dependent call.
c     fromlast........ (output, double precision) CPU time elapsed from
c                      the last call to "cputime" with "setflag" set to
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
      double precision  total, fromlast
c
c     Declaration of shared data.
c
      double precision  lastcputime
      save              lastcputime
c
c     Declaration of internal variables and arrays.
c
      double precision  cpusec
c
c     FIRST EXECUTABLE STATEMENT
c
c     Calling a C routine to read the current date and time.
c
      total = cpusec()
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
