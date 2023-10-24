c
c     FILE: sypawntest.f                    Creation date: 18/OCT/1999.
c                                       LAST MODIFICATION: 18/OCT/1999.
c
c     Testing routine "sysspawn"
c
      program sysspawntest
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
      implicit none
c
c     Declaration of internal variables and arrays.
c
      character*100     command, sinp, sout
      integer           rc
c
c     FIRST EXECUTABLE STATEMENT
c
      print *, 'Enter command:'
      read 2010, command
 2010 format(a)
      print *, 'Enter standard input file (or blank):'
      read 2010, sinp
      print *, 'Enter standard output file (or blank):'
      read 2010, sout
c
c     Spawning the process.
c
      call sysspawn(command, sinp, sout, rc)
c
      print *, ' '
      print *, 'Return code:', rc
      print *, ' '
c
      end
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'spawntest.f'

