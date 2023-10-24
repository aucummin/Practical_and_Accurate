c
c     FILE: versioncheck.f                  CREATION DATE: 28/NOV/1996.
c                                       LAST MODIFICATION: 25/AUG/1998.
c
c     Checking the relation of the current program version with the
c     version used to write old files.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine versioncheck(otherversion, intversion, irc)
c
c     Analysing a version identifier string and comparing with the
c     current version of the AIRES program.
c
c     The return codes provide information about old versions,
c     which will allow for backwards compatibility.
c
c     Version strings must be formatted as follows:
c
c              nn.mm.ll[x][+]
c
c     where nn, mm and ll are integers and [x] is an optional letter.
c     A plus (+) sign as last character is used for "development
c     versions".

c
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998.
c
c
c     Arguments:
c     =========
c
c     otherversion.... (input, character*(*)) The version to analize.
c     intversion...... (output, integer) The input version in integer
c                      format.
c     irc............. (output, integer) Return code. Zero means
c                      that the given version is equal to the
c                      current one. 1 (2) means that versions are equal
c                      but the current (given) version is a
c                      "development version", that is, it has a '+'
c                      as last character. 5 (6) means that the given
c                      version is previous to the current one, and
c                      correspond to a normal (development)
c                      distribution. 99 means that the given version is
c                      posterior to the current one. 999 means invalid
c                      input version format. Negative codes indicate
c                      that the current version is a development
c                      version.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
c
c     Declaration of arguments.
c
      character*(*)     otherversion
      integer           intversion, irc
c
c     Declaration of internal variables and arrays.
c
      integer           i, l, i1, i2, i3, i4, dot1, dot2
      integer           develv, cdlf
c
c     FIRST EXECUTABLE STATEMENT
c
      intversion = aires_version_no
c
c     Looking if the given and current versions are equal.
c
      irc = 0
      if (otherversion .eq. aires_version) return
c
c     Versions are not equal.
c
      l = len(otherversion)
c
c     Discarding an eventual '+' at the end of the string.
c     This character is appended in development versions.
c
      if (otherversion(l:l) .eq. '+') then
        develv = 1
        l      = l - 1
      else
        develv = 0
      endif
c
c     Getting the version in integer format.
c
      do i = 1, l
        dot1 = i
        if (otherversion(i:i) .eq. '.') goto 1010
      enddo
      goto 3010
 1010 continue
      do i = dot1 + 1, l
        dot2 = i
        if (otherversion(i:i) .eq. '.') goto 1020
      enddo
      goto 3010
 1020 continue
c
      i = ichar(otherversion(l:l))
      if ((i .ge. ichar('a')) .and. (i .le. ichar('z'))) then
        i4 = i - ichar('a') + 1
        l  = l - 1
      else
        i4 = 0
      endif
c
      read(otherversion(1:dot1-1), *, err = 3010) i1
      read(otherversion(dot1+1:dot2-1), *, err = 3010) i2
      read(otherversion(dot2+1:l), *, err = 3010) i3
c
      intversion = i4 + 100 * (i3 + 100 * (i2 + 100 * i1))
c
      if (intversion .le. 0) goto 3010
c
c     Checking if the current version is a "development version".
c
      cdlf = sign(1, aires_dlfac)
c
c     Analysing different cases.
c
      if (intversion .lt. aires_version_no) then
c
c       The other version is previous to the current one.
c
        irc = cdlf * (5 + develv)
c
      else if (intversion .eq. aires_version_no) then
c
c       Both integer versions are equal, but come from different
c       strings. This means that one of the strings has the format
c       of a "development version" (the last character is "+").
c
        irc = cdlf * (1 + develv)
c
      else
c
c       The version is posterior to the current one.
c
        irc = 99
c
      endif
c
      return
c
 3010 continue
c
c     Invalid version format.
c
      irc        = 999
      intversion = 0
c
      return
      end
c     --- End of routine versioncheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'versioncheck.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
