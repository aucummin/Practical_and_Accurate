c
c     FILE: mvfile.f                        Creation date: 19/SEP/2000.
c                                       LAST MODIFICATION: 26/SEP/2000.
c
c     Routine to change the name of a file.
c     NOTE: The code of this routine may depend on the platform and/or
c           operating system.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine mvfile(oldname, newname, rc)
c
c     Renaming a file.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c     Arguments:
c     =========
c
c     oldname......... (input, character*(*)) Current file name.
c     newname......... (input, character*(*)) Name of the file after
c                      renaming it. If newname exists, the default
c                      action is to overwrite it.
c     rc.............. (output, integer) The system return code after
c                      the renaming operation. 0 corresponds to success.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     oldname, newname
      integer           rc
c
c     Declaration of internal variables and arrays.
c
      integer           rename
c
c     FIRST EXECUTABLE STATEMENT
c
      rc = rename(oldname, newname)
c
      return
      end
c     --- End of routine mvfile.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'mvfile.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
