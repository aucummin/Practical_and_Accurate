c
c     FILE: sysspawn.f                      Creation date: 18/OCT/1999.
c                                       LAST MODIFICATION: 05/APR/2002.
c
c     Routines to execute an external module via a OS call.
c     NOTE: The code of these routines may depend on the operating
c           system.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine sysspawn(emodule, stdinput, stdoutput, rc)
c
c     Spawning a slave OS process to run a given module.
c     Special version for SGI systems.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2002.
c
c     Arguments:
c     =========
c
c     emodule......... (input, character*(*)) The module to execute.
c     stdinput........ (input, character*(*)) Name of a file to be
c                      connected to the standard input channel. If the
c                      name is the null or blank string, no assignment
c                      is done, and the modules inherits the standard
c                      input channel of the calling program.
c     stdoutput....... (input, character*(*)) Name of a file to be
c                      connected to the standard output channel. If the
c                      name is the null or blank string, no assignment
c                      is done, and the modules inherits the standard
c                      output channel of the calling program. The
c                      standard error channel is always connected to
c                      this output stream.
c     rc.............. (output, integer) The system return code after
c                      the spawned process ends.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     emodule, stdinput, stdoutput
      integer           rc
c
c     Declaration of internal variables and arrays.
c
      integer           l
      character*512     cline
c
c     FIRST EXECUTABLE STATEMENT
c
      cline = emodule
      l     = len(emodule)
c
      if (stdinput .ne. ' ') then
        cline(l+1:512) = ' < ' // stdinput
        l              = l + len(stdinput) + 3
      endif
c
      if (stdoutput .ne. ' ') then
        cline(l+1:512) = ' > ' // stdoutput
        l              = l + len(stdoutput) + 3
      endif
c
      cline(l+1:512) = ' 2>&1'
      l              = l + 5
c
c     Under UNIX, the external process is launched by means of
c     a "system" call. For SGI systems there is no return code
c     so we always set it to zero.
c
      call system(cline(1:l))
      rc = 0
c
      return
      end
c     --- End of routine sysspawn.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'sysspawn.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
