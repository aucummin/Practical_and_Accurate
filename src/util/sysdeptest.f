c
c     FILE: sysdeptest.f                    Creation date: 18/JUN/1996.
c                                       LAST MODIFICATION: 02/APR/1997.
c
c     Testing system dependent routines.
c     This program calls the following system-dependent routines:
c
c          intdati    (Current date and time in integer format)
c          cputime    (Total elapsed processor time).
c          username   (User and host as character strings).
c
c     The sources of these routines are placed in directories
c     "sysdepN" (N a number). To compile this program it is necessary
c     to select one of them and include the corresponding routines.
c
      program sysdeptest
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
      implicit none
c
c     Declaration of internal variables and arrays.
c
      character*60      user, host
      integer           idati(6), i, j
      real*8            c1, c2, c3, c4, c5, c6
      real*8            x1, x2
      common            /x12/ x1, x2
c
      character*8       datiname(6)
c
      data              datiname / 'Year', 'Month', 'Day',
     .                             'hour', 'min.',  'secds.' /
c
c     FIRST EXECUTABLE STATEMENT
c
      call cputime(.true., c1, c2)
c
      call username(user, i, host, j)
c
      write(6, *)
      write(6, *) 'Username called:'
      write(6, *) 'User is: >', user(1:i), '<'
      write(6, *) 'At host: >', host(1:j), '<'
      write(6, *)
c
      call intdati(idati)
c
      write(6, *) 'Intdati called:'
      do i = 1, 6
        write(6, *) i, ') ', datiname(i), ':', idati(i)
      enddo
c
      call cputime(.false., c3, c4)
      write(6, *)
      write(6, *) 'Consuming some CPU time to check "cputime"...'
c
      x1 = 1
      do i = 1, 500000
        x2 = 1 + 1 / x1
        x1 = 1 / x2
      enddo
c
      call cputime(.true., c5, c6)
c
      write(6, *)
      write(6, *) 'Cputime called three times:'
      write(6, *) 1, ')', c1, c2
      write(6, *) 2, ')', c3, c4
      write(6, *) 3, ')', c5, c6
      write(6, *)
c
      end
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'daticputest.f'

