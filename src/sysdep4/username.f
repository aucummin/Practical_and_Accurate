c
c     FILE: username.f                      Creation date: 29/MAR/1997.
c                                       LAST MODIFICATION: 02/APR/1997.
c
c     Routine to get username and hostname.
c     NOTE: System dependent code. Contain calls to non standard
c           procedures.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine username(user, ul, host, hl)
c
c     Current username and hostname (IBM Fortran (xfl) version).
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Parameters:
c     ==========
c
c     user............ (output, character*(*)) The username.
c     ul.............. (output, integer) Length of string "user".
c     host............ (output, character*(*)) The hostname.
c     hl.............. (output, character(*)) Length of string "host".
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     user, host
      integer           ul, hl
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, irc
      integer           hostnm_
c
c     FIRST EXECUTABLE STATEMENT
c
      call getenv('LOGNAME', user)
      irc = hostnm_(host)
c
c     Eliminating trailing blanks and evaluating length.
c
      do i = 1, len(user)
        j = i
        if (user(i:i) .ne. ' ') goto 1010
      enddo
 1010 continue
      do i = j, len(user)
        if (user(i:i) .eq. ' ') goto 1020
        ul = i - j + 1
        user(ul:ul) = user(i:i)
      enddo
 1020 continue
c
      do i = 1, len(host)
        j = i
        if (host(i:i) .ne. ' ') goto 1030
      enddo
 1030 continue
      do i = j, len(host)
        if (host(i:i) .eq. ' ') goto 1040
        hl = i - j + 1
        host(hl:hl) = host(i:i)
      enddo
 1040 continue
c
      if (ul .le. 0) then
        user = 'UNKNOWN'
        ul   = 7
      endif
      if (hl .le. 0) then
        host = 'UNKNOWN'
        hl   = 7
      endif
c
      return
      end
c     --- End of routine username.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'username.f'

c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
