c
c     FILE: setlreclunit.f                  Creation date: 03/JUL/1996.
c                                       LAST MODIFICATION: 03/JUL/1996.
c
c     This file contains a utility program to set the length (in
c     bytes) of the unit used in the qualifier "RECL" required
c     to open direct unformatted fortran i/o files.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      program setlrecl
c
      implicit none
c
c     Compilation parameters.
c
      integer           nbytes, maxvlen
      parameter         (nbytes = 2520)
      parameter         (maxvlen = nbytes / 4)
c
c     Declaration of internal variables and arrays.
c
      integer           lunit, i, j, lr, ndif
      integer*4         v1(maxvlen), v2(maxvlen)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Filling vector 1.
c
      do i = 1, maxvlen
        v1(i) = 32569 - i ** 2
      enddo
c
c     Searching the valid unit.
c
      do j = 3, 0, -1
c
        lunit = 2 ** j
        lr    = nbytes / lunit
c
        close (1)
        open (1, file = 'scratch.scratch', status = 'UNKNOWN',
     +        form = 'UNFORMATTED',
     +        access = 'DIRECT', recl = lr,
     +        err = 3010)
c
c       Attempting write operation.
c
        write(1, rec = 1, err = 1020) v1
c
c       Write OK! Let's go on to read.
c
        close(1)
        open (1, file = 'scratch.scratch', status = 'UNKNOWN',
     +        form = 'UNFORMATTED',
     +        access = 'DIRECT', recl = lr,
     +        err = 3010)
c
c       Attempting write operation.
c
        read(1, rec = 1, err = 1010) v2
c
c       Read OK! Let's compare v1 and v2.
c
        ndif = 0
        do i = 1, maxvlen
          if (v2(i) .ne. v1(i)) ndif = ndif + 1
        enddo
c
        if (ndif .eq. 0) goto 1100
c
c       Mmmmm. There are differences.
c
        print *, 'WARNING: Original and read data do not match'
        goto 1100
c
 1010   continue
c
c       Error in the read operation.
c
        print *, 'WARNING: Error in the read operation.',
     .           ' Unit =', lunit
c
 1020   continue
c
      enddo
c
c     There seems a valid unit was not found.
c
      print *, 'Sorry. Could not found a valid unit.'
      close(1, status = 'DELETE')
      stop
c
 1100 continue
c
c     The valid unit was found.
c
      print *, ' '
      print *, ' Valid unit is ', lunit, ' byte(s)'
      print *, ' '
      close(1, status = 'DELETE')
      stop
c
 3010 continue
c
c     Error opening the scratch files.
c
      print *, 'Cannot open scratch file!'
      close(1, status = 'DELETE')
      stop
      end
c     --- End of program setlrecl.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'setlreclunit.f'
