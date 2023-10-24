c
c     FILE: stringutils.f                   Creation date: 18/JUN/1996.
c                                       LAST MODIFICATION: 18/JUN/2003.
c
c     This file contains several routines to handle character strings.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getswitch(advance, string, slen, wfirst, wlast,
     +                     usedefault, default, switch, irc)
c
c     Extracting (optionally) the first word from a character string,
c     and reading it as an "On Off" logical switch.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     advance......... (input, logical) Switch to decide whether or
c                      not invoke routine "nextword" to extract the
c                      first word from "string".
c     string.......... (input, character*(*)) The string to scan
c     slen............ (input, integer) String length, or last
c                      position to scan.
c     wfirst.......... (input-output, integer) Position of the first
c                      character of the given or to be localized word.
c     wflast.......... (input-output, integer) Position of the last
c                      character of the given or to be localized word.
c     usedefault...... (input, integer) Switch to decide if a default
c                      value must be assigned when the input word
c                      is not given (empty string) or is invalid.
c                      0 means do not use default value. 1 use default
c                      only for missing specification. 2 use the
c                      default even in the case of an invalid
c                      specification.
c     default......... (input, logical) Default value to use. This
c                      argument is not used if "usedefault" is zero.
c     switch.......... (output, logical) The switch to set.
c     irc............. (output, integer) Return code. 0 means that a
c                      valid switch was specified and processed.
c                      1 means that no switch was specified and
c                      the default value was used. 2 means that no
c                      switch was specified and usedefault was zero.
c                      3 means that an invalid switch was specified
c                      and the default value was used to set the
c                      switch (usedefault >= 2). 4 is like 3 but in
c                      the case usedefault < 2.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           advance
      character*(*)     string
      integer           slen, wfirst, wlast
      integer           usedefault, irc
      logical           default, switch
c
c     FIRST EXECUTABLE STATEMENT
c
      if (advance) call nextword(string, slen, wfirst, wlast)
c
      if (wfirst .le. wlast) then
c
c       Analysing the given string.
c
        if (string(wfirst:wlast) .eq. "On") then
          switch = .true.
          irc = 0
        else if (string(wfirst:wlast) .eq. "Off") then
          switch = .false.
          irc = 0
        else
c
c         Invalid nonempty string.
c
          if (usedefault .ge. 2) then
            switch = default
            irc = 3
          else
            irc = 4
          endif
c
        endif
c
      else
c
c       No string specified.
c
        if (usedefault .ge. 1) then
            switch = default
            irc = 1
        else
            irc = 2
        endif
c
      endif
      return
c
      end
c     --- End of routine getswitch.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getnumber(advance, string, slen, wfirst, wlast,
     +                     usedefault, default, number, irc)
c
c     Extracting (optionally) the first word from a character string,
c     and reading it as a floating point number.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1998.
c
c
c     Arguments:
c     =========
c
c     advance......... (input, logical) Switch to decide whether or
c                      not invoke routine "nextword" to extract the
c                      first word from "string".
c     string.......... (input, character*(*)) The string to scan
c     slen............ (input, integer) String length, or last
c                      tion to scan.
c     wfirst.......... (input-output, integer) Position of the first
c                      character of the given or to be localized word.
c     wflast.......... (input-output, integer) Position of the last
c                      character of the given or to be localized word.
c     usedefault...... (input, integer) Switch to decide if a default
c                      value must be assigned when the input word
c                      is not given (empty string) or is invalid.
c                      0 means do not use default value. 1 use default
c                      only for missing specification. 2 use the
c                      default even in the case of an invalid
c                      specification.
c     default......... (input, double precision) Default value to use.
c                      This argument is not used if "usedefault" is
c                      zero.
c     number.......... (output, double precision) The number read.
c     irc............. (output, integer) Return code. 0 means that a
c                      valid number was specified and processed.
c                      1 means that no number was specified and
c                      the default value was used. 2 means that no
c                      number was specified and usedefault was zero.
c                      3 means that an invalid number was specified
c                      and the default value was used to set the
c                      number (usedefault >= 2). 4 is like 3 but in
c                      the case usedefault < 2.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           advance
      character*(*)     string
      integer           slen, wfirst, wlast
      integer           usedefault, irc
      double precision  default, number
c
c     Declaration of internal variables and arrays.
c
      integer           i, isc, nexp
      double precision  ftmp
c
c     FIRST EXECUTABLE STATEMENT
c
      if (advance) call nextword(string, slen, wfirst, wlast)
c
      if (wfirst .le. wlast) then
c
c       Analysing the given string. Excluding strings with
c       "alphabetic" characters.
c
        nexp = 0
c
        do i = wfirst, wlast
          isc = ichar(string(i:i))
c
          if ((isc .ge. ichar('a')) .and. (isc .le. ichar('z'))) then
            if ((string(i:i) .eq. 'e') .or. (string(i:i) .eq. 'd'))
     +      then
              nexp = nexp + 1
            else
              goto 1010
            endif
          else if ((isc .ge. ichar('A')) .and. (isc .le. ichar('Z')))
     +    then
            if ((string(i:i) .eq. 'E') .or. (string(i:i) .eq. 'D'))
     +      then
              nexp = nexp + 1
            else
              goto 1010
            endif
          endif
        enddo
c
        if (nexp .gt. 1) goto 1010
c
        read(string(wfirst:wlast), *, err = 1010) ftmp
c
c       Valid number.
c
        number = ftmp
        irc = 0
        return
c
 1010   continue
c
c       Invalid number (nonempty string).
c
        if (usedefault .ge. 2) then
          number = default
          irc = 3
        else
          irc = 4
        endif
c
      else
c
c       No string specified.
c
        if (usedefault .ge. 1) then
            number = default
            irc = 1
        else
            irc = 2
        endif
c
      endif
      return
c
      end
c     --- End of routine getnumber.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getsgchar(advance, string, slen, wfirst, wlast,
     +                     usedefault, default, ischar, irc)
c
c     Extracting (optionally) the first word from a character string,
c     and reading from it a single character, in the following way:
c     If the string lenght is 1, the character is taken as this string;
c     otherwise the string is interpreted as an ASCII code (ranging
c     from 0 to 255), if the read operation is successful the read
c     code will be returned. This last option is useful for specifying
c     special and or comment ('#') characters.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
c     Arguments:
c     =========
c
c     advance......... (input, logical) Switch to decide whether or
c                      not invoke routine "nextword" to extract the
c                      first word from "string".
c     string.......... (input, character*(*)) The string to scan
c     slen............ (input, integer) String length, or last
c                      tion to scan.
c     wfirst.......... (input-output, integer) Position of the first
c                      character of the given or to be localized word.
c     wflast.......... (input-output, integer) Position of the last
c                      character of the given or to be localized word.
c     usedefault...... (input, integer) Switch to decide if a default
c                      value must be assigned when the input word
c                      is not given (empty string) or is invalid.
c                      0 means do not use default value. 1 use default
c                      only for missing specification. 2 use the
c                      default even in the case of an invalid
c                      specification.
c     default......... (input, integer) Default value to use.
c                      This argument is not used if "usedefault" is
c                      zero.
c     ischar.......... (output, integer) The ASCII code of the
c                      character read.
c     irc............. (output, integer) Return code. 0 means that a
c                      valid character was specified and processed.
c                      1 means that no number was specified and
c                      the default value was used. 2 means that no
c                      character was specified and usedefault was zero.
c                      3 means that an invalid ascii code was specified
c                      and the default value was used to set the
c                      character (usedefault >= 2). 4 is like 3 but in
c                      the case usedefault < 2.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           advance
      character*(*)     string
      integer           slen, wfirst, wlast
      integer           usedefault, irc
      integer           default, ischar
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  ftmp
c
c     FIRST EXECUTABLE STATEMENT
c
      if (advance) call nextword(string, slen, wfirst, wlast)
c
      if (wfirst .eq. wlast) then
c
c       Single character specification. Returning the character
c
        ischar = ichar(string(wfirst:wfirst))
c
      else if (wfirst .lt. wlast) then
c
c       Trying to get a number from the string.
c
        call getnumber(.false., string, slen, wfirst, wlast,
     +                 0, 0.0d0, ftmp, irc)
c
        i = ftmp
        if ((irc .eq. 0) .and. (i .eq. ftmp) .and.
     +      (i .ge. 0) .and. (i .le. 255))   then
c
c         Character specified via a valid decimal number.
c
          ischar = i
c
        else
c
c         Invalid numeric specification (nonempty string).
c
          if (usedefault .ge. 2) then
            ischar = default
            irc = 3
          else
            irc = 4
          endif
c
        endif
c
      else
c
c       Empty string.
c
        if (usedefault .ge. 1) then
          ischar = default
          irc = 1
        else
          irc = 2
        endif
c
      endif
      return
c
      end
c     --- End of routine getsgchar.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intnice(number, negalt, string, slen)
c
c     Formatting an integer number
c
c     Written by: S. J. Sciutto, La Plata 1996, 2003.
c
c
c     Arguments:
c     =========
c
c     number.......... (input, integer) The number to process.
c     negalt.......... (input, integer) If positive, then the
c                      string returned by routine altstring(negalt,..)
c                      is returned for negative numbers. Otherwise the
c                      number is processed normally without regard to
c                      its sign.
c     string.......... (output, character*(*)) A string containing
c                      the formatted number. Maximum length is 12.
c     slen............ (output, integer) Position of last nonblank
c                      character of "string".
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           number
      integer           negalt
      character*(*)     string
      integer           slen
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((number .lt. 0) .and. (negalt .gt. 0)) then
c
        call altstring(negalt, string, slen)
c
      else
c
        write(string, 2010) number
 2010   format(i11)
        slen = 11
c
      endif
c
c     Removing leading and trailing blanks.
c
      call strim(0, string, slen)
c
      return
      end
c     --- End of routine intnice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltnice(number, negalt, string, slen)
c
c     Formatting a real number
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999.
c
c
c     Arguments:
c     =========
c
c     number.......... (input, double precision) The number to process.
c     negalt.......... (input, integer) If positive, then the
c                      string returned by routine altstring(negalt,..)
c                      is returned for negative numbers. Otherwise the
c                      number is processed normally without regard to
c                      its sign.
c     string.......... (output, character*(*)) A string containing
c                      the formatted number. Maximum length is 14.
c     slen............ (output, integer) Position of last nonblank
c                      character of "string".
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  number
      integer           negalt
      character*(*)     string
      integer           slen
c
c     Declaration of internal variables and arrays.
c
      double precision  ftmp
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((number .lt. 0) .and. (negalt .gt. 0)) then
c
        call altstring(negalt, string, slen)
c
      else
c
        if (number .ne. 0) then
          ftmp = abs(number)
          if ((ftmp .gt. 1.0d-3) .or. (ftmp .lt. 1.0d6)) then
            write(string, 2010) number
 2010       format(1p, g14.7)
          else
            write(string, 2020) number
 2020       format(1p, e14.6)
          endif
          slen = 14
        else
          string = '0.000000'
          slen   = 8
        endif
c
      endif
c
c     Removing leading and trailing blanks, and also leading decimal
c     point.
c
      call snumtrim(string, slen)
c
      return
      end
c     --- End of routine fltnice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine swnice(switch, string, slen)
c
c     Formatting a logical switch.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     switch.......... (input, logical) The switch to process.
c     string.......... (output, character*(*)) A string containing
c                      the formatted switch. Maximum length is 3.
c     slen............ (output, integer) Position of last nonblank
c                      character of "string".
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           switch
      character*(*)     string
      integer           slen
c
c     FIRST EXECUTABLE STATEMENT
c
      if (switch) then
        string = 'On'
        slen   = 2
      else
        string = 'Off'
        slen   = 3
      endif
c
      return
      end
c     --- End of routine swnice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltscale(number, unit, ulen, scaledir, scaleinv)
c
c     Setting scaling factors to convert a real number to be expressed
c     in other units, so that the resulting numbers are suitable
c     for printing in f format.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     number.......... (input, double precision) The number to process.
c                      Usually it represents the maximum number to deal
c                      with.
c     unit............ (output, character*(*)) The new unit, expressed
c                      in the form '10^X' where X is an integer
c                      number. Unit is also used as scratch space, so
c                      its length must be at least 12 characters.
c     ulen............ (output, integer) The length of string unit.
c     scaledir........ (output, double precision) Direct scaling
c                      factor. Converts to the new unit.
c     scaleinv........ (output, double precision) Inverse scaling
c                      factor. Converts from the new unit.
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  number
      character*(*)     unit
      integer           ulen
      double precision  scaledir, scaleinv
c
c     Declaration of internal variables and arrays.
c
      integer           i, itmp1, itmp2
c
c     FIRST EXECUTABLE STATEMENT
c
      if (number .ne. 0) then
c
c       Searching a scaling factor such that the absloute value of the
c       given number is less than 10000 in that unit.
c
        itmp1 = log10(abs(number))
        itmp2 = (itmp1 - 1) / 3
        if (itmp1 .lt. 0) itmp2 = itmp2 - 1
        itmp1 = 3 * itmp2
c
      else
        itmp1 = 0
      endif
c
      call intnice(itmp1, -1, unit, itmp2)
      do i = itmp2, 1, -1
        unit(i+3:i+3) = unit(i:i)
      enddo
      unit(1:3) = '10^'
      ulen      = itmp2 + 3
c
      scaleinv = 10.d0 ** itmp1
      scaledir = 1.d0 / scaleinv
c
      return
      end
c     --- End of routine fltscale.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine nextword(string, slen, wfirst, wlast)
c
c     Extracting the first word from a character string.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     string.......... (input, character*(*)) The string to scan
c     slen............ (input, integer) String length, or last
c                      position to scan.
c     wfirst.......... (output, integer) Position of first character
c                      of the localized word.
c     wlast........... (input-output, integer) As input this argument
c                      plus 1 indicates the first character to scan.
c                      As output it returns the position of the last
c                      character of the localized word.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     string
      integer           slen, wfirst, wlast
c
c     Declaration of internal variables and arrays.
c
      integer           i, wfirst1
      logical           nonblank
c
c     FIRST EXECUTABLE STATEMENT
c
      wfirst1 = wlast + 1
      if (wfirst1 .gt. slen) goto 1100
c
c     Looking for the word boudaries: blanks, special characters or
c     string limits.
c
      do i = wfirst1, slen
        wfirst = i
        if (nonblank(string(i:i))) goto 1010
      enddo
c
c     Only blanks within string, or null string
c
 1100 continue
      wfirst = slen + 1
      wlast  = slen
      return
c
 1010 continue
c
c     Looking for the end of the word.
c
      wlast = wfirst
      do i = wfirst, slen
        if (.not. nonblank(string(i:i))) return
        wlast = i
      enddo
c
      return
c
      end
c     --- End of routine nextword.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine subsort(n, stringarray, c1, c2, isort)
c
c     Sorting (in ascendig order) a given array of strings.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of elements to sort.
c     stringarray..... (input, character*(*), array(n)) The elements
c                      to sort.
c     c1, c2.......... (input, integer) Respectively the first and last
c                      columns of the string array to use for sorting.
c     isort........... (input-output, integer, array(n)) The array of
c                      indices. As input it must contain a set of
c                      indices pointing to the elements of stringarray
c                      that must be sorted. As output, the strings
c                      stringarray(isort(i)), i = 1,...,n, give the
c                      sorted sequence of elements.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           n, c1, c2
      character*(*)     stringarray(n)
      integer           isort(n)
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, chi, chj
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = n - 1, 1, -1
        do j = n, i + 1, -1
          l = j
          do k = c1, c2
            chi = ichar(stringarray(isort(i))(k:k))
            chj = ichar(stringarray(isort(j))(k:k))
            if (chj .ne. chi) then
              if (chj .lt. chi) then
                goto 1020
              else
                goto 1010
              endif
            endif
          enddo
          goto 1020
 1010     continue
        enddo
        goto 1030
 1020   continue
        k = isort(i)
        do j = i, l - 1
          isort(j) = isort(j + 1)
        enddo
        isort(l) = k
 1030   continue
      enddo
c
      return
      end
c     --- End of routine subsort.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function wordlocate(string, words, separator, minlen0)
c
c     Comparing a string against several words concatenated in another
c     string, and returning the ordinal corresponding to the first
c     matching word.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     string.......... (input, character*(*)) The word to locate.
c     words........... (input, character*(*)) The list of words.
c     separator....... (input, character*(*)) Character used to
c                      separate words.
c     minlen0......... (input, integer) The minimum number of
c                      characters that must be provided.
c
c     Return value: (integer) The ordinal of the matching word or zero
c     ============  if there are no matches.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           wordlocate
      character*(*)     string, words, separator
      integer           minlen0
c
c     Declaration of internal variables and arrays.
c
      integer           l1, l2, lw, i
      logical           smatch
c
c     FIRST EXECUTABLE STATEMENT
c
      l1 = 1
      l2 = 0
      lw = len(words)
      do i = 1, lw
        if (words(i:i) .eq. separator) then
          l2 = l2 + 1
          if (smatch(string, words(l1:i-1), minlen0)) goto 1010
          l1 = i + 1
        endif
      enddo
      if (words(lw:lw) .ne. separator) then
        l2 = l2 + 1
        if (smatch(string, words(l1:lw), minlen0)) goto 1010
      endif
c
c     No matching word.
c
      wordlocate = 0
      return
c
 1010 continue
c
      wordlocate = l2
c
      return
      end
c     --- End of routine wordlocate.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine nthword(n, words, separator, fullword, fwlen)
c
c     Finding the nth word in a list of words.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The ordinal of the word to
c                      find, starting by 1 for the first word in the
c                      list.
c     words........... (input, character*(*)) The list of words.
c     separator....... (input, character*(*)) Character used to
c                      separate words.
c     fullword........ (output, character*(*)) The corresponding nth
c                      word.
c     fwlen........... (output, integer) The length of fullword.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           n
      character*(*)     words, separator, fullword
      integer           fwlen
c
c     Declaration of internal variables and arrays.
c
      integer           l1, l2, l3, lw, i
c
c     FIRST EXECUTABLE STATEMENT
c
      l1 = 1
      l2 = 0
      lw = len(words)
      do i = 1, lw
        if (words(i:i) .eq. separator) then
          l2 = l2 + 1
          l3 = i - 1
          if (l2 .eq. n) goto 1010
          l1 = i + 1
        endif
      enddo
      if (words(lw:lw) .ne. separator) then
        l2 = l2 + 1
        l3 = lw
        if (l2 .eq. n) goto 1010
      endif
c
c     Not enough words or invalid n.
c
      fullword = separator
      fwlen    = 0
      return
c
 1010 continue
c
      fullword   = words(l1:l3)
      fwlen      = l3 - l1 + 1
c
      return
      end
c     --- End of routine wordlocate.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function smatch(string1, string0, minlen0)
c
c     Comparing a string against another one, and checking if both
c     strings match in the sense of IDL directive abbreviations.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     string1......... (input, character*(*)) The string to check.
c     string0......... (input, character*(*)) The base string.
c     minlen0......... (input, integer) The minimum number of
c                      characters that must be provided.
c
c     Return value: (logical) .true. if both strings match.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           smatch
      character*(*)     string1, string0
      integer           minlen0
c
c     Declaration of internal variables and arrays.
c
      integer           l1, l2
c
c     FIRST EXECUTABLE STATEMENT
c
      l1 = len(string1)
c
      if (l1 .ge. minlen0) then
        l2 = min(l1, len(string0))
        smatch = (string1 .eq. string0(1:l2))
      else
        smatch = .false.
      endif
      return
c
      end
c     --- End of routine smatch.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine strim(sslen, string, slen)
c
c     Eliminating leading and trailing blanks (or special characters)
c     from a string.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     sslen........... (input, integer) String length indicator. If
c                      sslen > 0 then sslen is used to set the position
c                      of the last character to scan. If sslen = 0, it
c                      is assumed that the position of the last
c                      character to scan is initially stored in
c                      argument slen. If sslen < 0 then the position of
c                      the last character is the length of "string".
c     string.......... (input-output, character*(*)) The string
c                      to process.
c     slen............ (input-output, integer) String length, or
c                      position of the last character.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           sslen
      character*(*)     string
      integer           slen
c
c     Declaration of internal variables and arrays.
c
      integer           slen0, i, j, iskip
      logical           nonblank
c
c     FIRST EXECUTABLE STATEMENT
c
      if (sslen .gt. 0) then
        slen = sslen
      else if (sslen .lt. 0) then
        slen = len(string)
      endif
      slen0 = slen
c
c     Eliminating trailing blanks.
c
      do i = slen, 1, -1
        j = i
        if (nonblank(string(i:i))) goto 1010
      enddo
c
c     Only blanks within string, or null string
c
      slen = 0
      goto 1030
c
 1010 continue
      slen = j
c
c     Looking for leading blanks.
c
      iskip = 0
      do i = 1, j
        if (nonblank(string(i:i))) goto 1020
        iskip = i
      enddo
 1020 continue
c
      if (iskip .le. 0) goto 1030
c
c     Eliminating leading blanks or specials.
c
      do i = iskip + 1, j
        slen = i - iskip
        string(slen:slen) = string(i:i)
      enddo
c
 1030 continue
c
c     The trailing part of the string is blanked
c
      if (slen0 .gt. slen) string(slen+1:slen0) = ' '
c
      return
      end
c     --- End of routine strim.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine strimcopy(string1, slen1, string2, slen2)
c
c     Copying a string while eliminating leading and trailing blanks
c     (or special characters).
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     string1......... (input, character*(*)) The string to process.
c                      This string will not be modified.
c     slen1........... (input, integer) Length of string 1.
c     string2......... (output, character*(*)) A copy of string 1,
c                      with no leading or trailing blanks.
c     slen2........... (output, integer) Length of string 2, or
c                      position of the last character.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     string1, string2
      integer           slen1, slen2
c
c     FIRST EXECUTABLE STATEMENT
c
      string2 = string1
      slen2   = min(slen1, len(string2))
c
      call strim(0, string2, slen2)
c
      return
c
      end
c     --- End of routine strimcopy.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine snumtrim(string, slen)
c
c     Eliminating leading and trailing blanks (or special characters),
c     and leading decimal point from a string.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     string.......... (input-output, character*(*)) The string
c                      to process.
c     slen............ (input-output, integer) String length, or
c                      position of the last character.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     string
      integer           slen
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      logical           nonblank
c
c     FIRST EXECUTABLE STATEMENT
c
c     Transforming leading decimal point '.' into '0.'
c
      do i = 1, slen
        j = i
        if (nonblank(string(i:i))) goto 1010
      enddo
 1010 continue
      if ((j .gt. 1) .and. (string(j:j) .eq. '.')) then
        j = j - 1
        string(j:j) = '0'
      endif
c
c     Eliminating leading and trailing blanks or specials.
c
      call strim(0, string, slen)
c
      return
c
      end
c     --- End of routine snumtrim.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine scleancomm(llen0, line, llen)
c
c     Scanning a string, eliminating leading and triling blanks or
c     special characters, and commented out areas.
c
c     Written by: S. J. Sciutto, La Plata 1996; Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     llen0........... (input, integer) Position of last character to
c                      scan.
c     line............ (input-output, character*(*)) The line to edit
c                      without commented out areas.
c     llen............ (output, integer) Position of the last nonblank
c                      character. Zero for blank lines.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of arguments.
c
      integer           llen0, llen
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, ifirst, ilast, jlast
      logical           nonblank
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching for a comment mark and/or trailing blanks.
c
      jlast = 0
      do i = 1, llen0
        if (line(i:i) .eq. comchar) goto 1020
        jlast = i
      enddo
c
c     No comments.
c
      goto 1030
 1020 continue
c
c     If the comment character is placed at the beginning of the line,
c     then the line is blank.
c
      if (jlast .le. 0) then
        llen = 0
        return
      endif
c
 1030 continue
c
c     Looking for trailing blanks or specials.
c
      do i = jlast, 1, -1
        ilast = i
        if (nonblank(line(i:i))) goto 1040
      enddo
c
c     An empty line was found.
c
      llen = 0
      return
c
 1040 continue
c
c     Searching leading blanks or specials.
c
      do i = 1, ilast
        ifirst = i
        if (nonblank(line(i:i))) goto 1050
      enddo
c
c     This point should never be reached, but, anyway ...
c
      llen = 0
      return
c
 1050 continue
c
c     A nonempty line was found. Eliminating leading blanks or
c     special characters.
c
      if (ifirst .gt. 1) then
        llen = 0
        do i = ifirst, ilast
          llen = llen + 1
          line(llen:llen) = line(i:i)
        enddo
      else
        llen = ilast
      endif
c
      return
      end
c     --- End of routine scleancomm.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine scontain(string1, slen1, maxlen, string2, slen2)
c
c     Limiting the length of a string to a given maximum length.
c     If the string is too long it is written in the form
c
c            'first chars...last chars'
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     string1......... (input, character*(*)) The original string.
c     slen1........... (input, integer) Original string length.
c     maxlen.......... (input, integer) Maximum length of the output
c                      string. Must be greater than 4.
c     string2......... (output, character*(*)) Edited string.
c     slen2........... (output, integer) Edited string length, will
c                      be equal to min(slen1, maxlen).
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     string1, string2
      integer           slen1, maxlen, slen2
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, i0, i1
c
c     FIRST EXECUTABLE STATEMENT
c
      if (slen1 .le. maxlen) then
c
c       Original string fits well into given length.
c
        string2 = string1
        slen2   = slen1
c
      else
c
c       Placing '...' in the middle of the string.
c
        i0 = slen1 - maxlen
        i1 = maxlen / 2 - 1
c
        do i = 1, i1 - 1
          string2(i:i) = string1(i:i)
        enddo
        do i = i1 + 3, maxlen
          j = i0 + i
          string2(i:i) = string1(j:j)
        enddo
        string2(i1:i1+2) = '...'
        slen2            = maxlen
c
      endif
      return
c
      end
c     --- End of routine scontain.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine altstring(index, string, slen)
c
c     Returning one of a set of frequently used keyword strings.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     index........... (input, integer) Keyword label.
c     string.......... (output, character*(*)) The returned string
c     slen............ (output, integer) String length.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           index, slen
      character*(*)     string
c
c     Declaration of internal variables and arrays.
c
      integer           keymax
      parameter         (keymax = 2)
      character*12      keystr(0:keymax)
c
      data              keystr / ' ',
     +                           'Infinite', 'Relative'
     +                         /
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((index .ge. 0) .and. (index .le. keymax)) then
        string = keystr(index)
        slen   = 12
      else
        slen = 0
      endif
      return
c
      end
c     --- End of routine altstring.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function nonblank(ch)
c
c     Determining if a character is nonblank. "Blank" characters are
c     defined as: space (' ') character, or special (nonprintable)
c     characters.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     ch.............. (input, character*1) The character to analyse.
c
c     Return value: (logical) .true. if the character is nonblank.
c     ============
c
      implicit none
c
c     Compilation parameters.
c
      include 'syspar.f'
c
c     Declaration of arguments.
c
      logical           nonblank
      character*1       ch
c
c     Declaration of internal variables and arrays.
c
      integer           ich
c
c     FIRST EXECUTABLE STATEMENT
c
      ich = ichar(ch)
      nonblank = ((ch .ne. ' ') .and.
     +            (ich .ge. icharsetmin) .and. (ich .le. icharsetmax))
c
      return
      end
c     --- End of routine nonblank.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function texspecial(ch, charcat)
c
c     Determining if a character is one of the ten TeX special
c     characters: $ & % # _ { } ~ ^ \.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     ch.............. (input, character*1) The character to analyse.
c     charcat......... (output, integer) Index labelling the kind of
c                      special character: If 0 or 1 then the character
c                      is printable with control sequence \ch. If 0
c                      the character can also be used for file names.
c                      If 2, then the character is one of
c                      (nonprintable) ~ ^ \.
c
c     Return value: (logical) .true. if the character is special.
c     ============
c
      implicit none
c
c     Compilation parameters.
c
      include 'syspar.f'
c
c     Declaration of arguments.
c
      logical           texspecial
      character*1       ch
      integer           charcat
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     TeX special characters.
c
      integer           nspecials, nspecials0, nspecialsf
      parameter         (nspecials = 10)
      parameter         (nspecials0 = 7, nspecialsf = 1)
c
      character*1       texspecialch(nspecials)
c
      data              texspecialch( 1) / '_' /
      data              texspecialch( 2) / '$' /
      data              texspecialch( 3) / '&' /
      data              texspecialch( 4) / '#' /
      data              texspecialch( 5) / '%' /
      data              texspecialch( 6) / '{' /
      data              texspecialch( 7) / '}' /
      data              texspecialch( 8) / '~' /
      data              texspecialch( 9) / '^' /
      data              texspecialch(10) / '\\' /
c
c     FIRST EXECUTABLE STATEMENT
c
      texspecial = .true.
      do i = 1, nspecials
        j = i
        if (ch .eq. texspecialch(i)) goto 1010
      enddo
      texspecial = .false.
      return
 1010 continue
      if (j .le. nspecialsf) then
        charcat = 0
      else if (j .le. nspecials0) then
        charcat = 1
      else
        charcat = 2
      endif
c
      return
      end
c     --- End of routine texspecial.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'stringutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
