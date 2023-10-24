c
c     FILE: username.f                      Creation date: 25/JUN/1996.
c                                       LAST MODIFICATION: 29/SEP/2000.
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
c     Current username and hostname.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000.
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
      integer           i, j, ialt
c
c     FIRST EXECUTABLE STATEMENT
c
c     User name.
c
      call getenv('LOGNAME', user)
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
      if (ul .lt. len(user)) user(ul+1:len(user)) = ' '
c
c     Host name. There are two alternatives here.
c
      call getenv('HOSTNAME', host)
c
      if (host .eq. ' ') then
c
        call system('rm -f .Aires_uhnam_TMP')
        call system('hostname > .Aires_uhnam_TMP')
c
        open(57, file = '.Aires_uhnam_TMP', status = 'OLD' ,
     +           err = 3010)
        read(57, 2010, end = 3010, err = 3010) host
 2010   format(a)
        close(57, status = 'DELETE')
c
      endif
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
      if (hl .lt. len(host)) host(hl+1:len(host)) = ' '
c
      return
c
c     Error opening and/or reading scratch file.
c
 3010 continue
      user = 'UNKNOWN'
      ul   = 7
 3020 continue
      host = 'UNKNOWN'
      hl   = 7
c
      close(57, status = 'DELETE')
      return
c
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
