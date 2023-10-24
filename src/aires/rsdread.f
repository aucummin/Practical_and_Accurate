c
c     FILE: rsdread.f                       CREATION DATE: 23/MAR/1999.
c                                       LAST MODIFICATION: 18/JUN/2003.
c
c     Getting the original random seed located in a given idf file.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine rsdread(idffile, vrb, seed, irc)
c
c     Getting the original random seed stored in a specified idf file.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     idffile......... (input, character*(*)) The given file name.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1, 2 or 3 mean that error
c                      messages will be printed. vrb > 3 is similar to
c                      the previous case, but with the additional
c                      action of stopping the program if a fatal error
c                      takes place.
c     seed............ (output, double precision) The recovered random
c                      seed. If the file does not exist, or is not an
c                      idf file, then 0 is returned.
c     irc............. (output, integer) Return code. zero means
c                      successful return. Other return codes are used
c                      (see source listing)
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'idfhead.f'
c
c     Declaration of arguments.
c
      character*(*)     idffile
      integer           vrb, irc
      double precision  seed
c
c     Declaration of internal variables and arrays.
c
      integer           errsev, iiver
      logical           errprt
      integer           i, j
      double precision  z1, z2
      integer           srysev
c
c     idf header related variables.
c
      character*32      idfheader
      integer           idfintheader
      character*24      xidfversion
c
c     FIRST EXECUTABLE STATEMENT
c
      seed = 0
c
      if (vrb .le. 0) then
        errsev = 2
        errprt = .false.
      else if (vrb .le. 3) then
        errsev = 3
        errprt = .true.
      else
        errsev = 4
        errprt = .true.
      endif
c
c     Opening the file.
c
      irc = 115
      open(99, file = idffile, status = 'OLD',
     +        form = 'UNFORMATTED', err = 3010)
c
c     READING HEADER DATA.
c
      read(99, end = 3010, err = 3010) idfheader, idfintheader,
     +                                 xidfversion
c
c     Checking the validity of the file parameters.
c
      if ((idfheader .ne. idfheader0) .or.
     +    (idfintheader .ne. idfintheader0)) then
        if (errprt) then
          call errprint(0, '*', errsev, 'rsdread',
     +      'This file seems not to be a valid AIRES idf file.',
     +      0, 0, 0, 0.d0, idffile)
        endif
        irc = 206
        return
      endif
c
c     Checking input AIRES version, and backwards compatibility stuff.
c
      call strim(24, xidfversion, i)
      call versioncheck(xidfversion(1:i), iiver, irc)
c
      if (irc .ne. 0) then
c
c       Current and input version are not equal.
c
        if (irc .gt. 100) then
c
c         Invalid version format.
c
          irc = 360
          if (errprt) then
            call errprint(0, '$DF1', srysev, 'rsdread',
     +           idffile, 0, 0, 0, 0.d0, xidfversion(1:i))
          endif
          return
c
        else if (irc .gt. 10) then
c
c         File corresponds to a future version of AIRES.
c
          irc = 350
          if (errprt) then
            call errprint(0, '$DF2', srysev, 'rsdread',
     +           idffile, 0, 0, 0, 0.d0, xidfversion(1:i))
          endif
          return
c
        else
c
c         File was written with a previous AIRES version.
c         Applying backwards compatibility.
c
          irc = 300 + abs(irc)
c
c         The original random seed was not saved before version 1.5.5
c
          if (iiver .le. 01050500) goto 1100
c
        endif
c
      endif
c
c     Skipping records. This may depend on file version.
c
      do i = 1, 4
        read(99, end = 3010, err = 3010)
      enddo
c
      read(99, end = 3010, err = 3010) i, j
      if (i .gt. 0) read(99, end = 3010, err = 3010)
      if (j .gt. 0) read(99, end = 3010, err = 3010)
      read(99, end = 3010, err = 3010)
      read(99, end = 3010, err = 3010) i
      if (i .gt. 0) read(99, end = 3010, err = 3010)
c
      do i = 1, 3
        read(99, end = 3010, err = 3010)
      enddo
c
c     Random number generators shared data.
c
      read(99, end = 3010, err = 3010) i
      if (i .gt. 0) then
        read(99, end = 3010, err = 3010) z1, z2, seed
      endif
c
      irc = 0
c
 1100 continue
      close(99)
      return
c
c     Error messages.
c
 3010 continue
      if (errprt) then
        call errprint(0, '$A30', errsev, 'rsdread', ' ',
     +                1, 99, 0, 0.d0, idffile)
      endif
      irc = 97
      close(99)
      return
c
      end
c     --- End of routine rsdread
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'rsdread.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
