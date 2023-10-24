c
c     FILE: cioshipall123.f                 Creation date: 10/JUL/1997.
c                                       LAST MODIFICATION: 10/JUL/1997.
c
c     Completing compressed output file processing. 3 files case.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine pcleshipall
c
c     Writting the buffers of all defined compressed files.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
      implicit none
c
c     FIRST EXECUTABLE STATEMENT
c
c     Completing ground particle file processing
c
      call pcleship1
c
c     Other files.
c
      call pcleship2
      call pcleship3
c
      return
c
      end
c     --- End of routine pcleshipall
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cioshipall123.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
