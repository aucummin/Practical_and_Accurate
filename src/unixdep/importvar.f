c
c     FILE: importvar.f                     Creation date: 30/JUN/2001.
c                                       LAST MODIFICATION: 30/JUN/2001.
c
c     Routines to import the current value of an enviromental variable.
c     NOTE: The code of these routines may depend on the operating
c           system.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine importvar(varname, value, vlen)
c
c     Getting the current value of an environment variable.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     varname......... (input, character*(*)) Name of the variable.
c     value........... (output, character*(*)) Current value, as
c                      returned by the OS with trailing blanks removed.
c                      The calling program must ensure that the
c                      maximum length of the string is enough to store
c                      the returned value.
c     vlen............ (output, integer) Length of string "value"
c                      (zero if the returned value is blank).
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     varname, value
      integer           vlen
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Under UNIX, environment variables are get via a call to
c     "getenv".
c
      call getenv(varname, value)
c
c     Removing trailing blanks.
c
      do i = len(value), 1, -1
        vlen = i
        if (value(i:i) .ne. ' ') goto 1010
      enddo
      vlen = 0
 1010 continue
c
      return
      end
c     --- End of routine importvar.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'importvar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
