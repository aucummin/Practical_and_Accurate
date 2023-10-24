c
c     FILE: apopen.f                        Creation date: 29/MAR/1997.
c                                       LAST MODIFICATION: 29/MAR/1997.
c
c     Routines that contain non standard FORTRAN 77 instructions.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine appendopen(ut, file, status, form, irc)
c
c     Opening a sequential file for data appending.
c     Version compatible with FORTRAN compilers running under
c     IBM AIX OS.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c     file............ (input, character*(*)) File name.
c     status.......... (input, character*(*)) Status string ('NEW',
c                      'OLD', or 'UNKNOWN').
c     form............ (input, character*(*)) Record formatting
c                      ('FORMATTED', 'UNFORMATTED' or 'PRINT').
c     irc............. (output, integer) Return code, zero means normal
c                      return. Otherwise irc is the system return code
c                      plus 10000.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ut
      character*(*)     file, status, form
      integer           irc
c
c     FIRST EXECUTABLE STATEMENT
c
c     Opening the file for appending data.
c
      open (ut, file = file, status = status, form = form,
     +          position = 'APPEND', iostat = irc, err = 3010)
c
      irc = 0
      return
c
c     Error opening the file.
c
 3010 continue
      irc = irc + 10000
      return
c
      end
c     --- End of routine appendopen.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'apopen.f'

c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
