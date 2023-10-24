c
c     FILE: ciodecode.f                     Creation date: 27/NOV/1996.
c                                       LAST MODIFICATION: 03/JAN/1997.
c
c     This file contains the routines for decoding data already coded
c     by the compressed i/o system
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intdecode2(n, cstring, ispos, int1)
c
c     Decoding two-character strings into integer numbers.
c     The integers are encoded in 2 characters.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data items to
c                      process.
c     cstring......... (input, character*(*)) The string containing the
c                      encoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last processed character.
c     int1............ (output, integer, array(n)) The decoded
c                      integers.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      character*(*)     cstring
      integer           int1(n)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k
      integer           d0, d1
c
c     FIRST EXECUTABLE STATEMENT
c
c     Data are formatted as numbers expressed in basis codebase (225):
c        digit1 digit0
c        (0 .le. digit0 .le. 255)
c        (0 .le. digit1 .le. 255)
c
      do i = 1, n
c
c       Getting the digits.
c
        k = ispos + 1
        d0 = cio2intdigits(ichar(cstring(k:k)), currciofile2)
        ispos = k + 1
        d1 = cio2intdigits(ichar(cstring(ispos:ispos)), currciofile2)
c
c       Reobtaining the integer number.
c
        int1(i) = d0 + codebase * d1
c
      enddo
c
      return
      end
c     --- End of routine intdecode2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intintdecode4(n, cstring, ispos, int1, int2)
c
c     Decoding four-character strings into sets of two integer numbers.
c     The first integer is encoded in 1 1/2 characters and the second
c     one in 2 1/2 characters.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data pairs to
c                      process.
c     cstring......... (input, character*(*)) The string containing the
c                      encoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last processed character.
c     int1............ (output, integer, array(n)) First datum of the
c                      pairs.
c     int2............ (output, integer, array(n)) Second datum of the
c                      pairs.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      character*(*)     cstring
      integer           int1(n), int2(n)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      integer           d01, d11, d02, d12, d22
      integer           digits(3)
      equivalence       (d01, digits(1))
      equivalence       (d02, digits(2))
      equivalence       (d12, digits(3))
c
c     FIRST EXECUTABLE STATEMENT
c
c     Data are formatted as numbers expressed in basis codebase (225):
c     First number:   digit1 digit0
c                     (0 .le. digit0 .le. 255)
c                     (0 .le. digit1 .le. 15)
c     Second number:  digit2 digit1 digit0
c                     (0 .le. digit0 .le. 255)
c                     (0 .le. digit1 .le. 255)
c                     (0 .le. digit2 .le. 15)
c
      do i = 1, n
c
c       Getting the digits.
c
        do j = 1, 3
          k = ispos + j
          digits(j) = cio2intdigits(ichar(cstring(k:k)), currciofile2)
        enddo
c
c       The last character was set "half and half" with
c       the most significative digits.
c
        ispos = k + 1
        j     = cio2intdigits(ichar(cstring(ispos:ispos)), currciofile2)
        d11   = halfandhalfi1(j)
        d22   = halfandhalfi2(j)
c
c       Reobtaining the integer numbers.
c
        int1(i) = d01 + codebase * d11
        int2(i) = d02 + codebase * (d12 + codebase * d22)
c
      enddo
c
      return
      end
c     --- End of routine intintdecode4
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intintdecode5(n, cstring, ispos, int1)
c
c     Decoding four-character strings into sets of two integer numbers.
c     Each one of the integers is encoded in 2 1/2 characters.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data pairs to
c                      process.
c     cstring......... (input, character*(*)) The string containing the
c                      encoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last processed character.
c     int1............ (output, integer, array(2 * n)) Integer array
c                      containing the decoded pairs of numbers.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      character*(*)     cstring
      integer           int1(1)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, i1, j, k
      integer           d01, d11, d21, d02, d12, d22
      integer           digits(4)
      equivalence       (d01, digits(1))
      equivalence       (d11, digits(2))
      equivalence       (d02, digits(3))
      equivalence       (d12, digits(4))
c
c     FIRST EXECUTABLE STATEMENT
c
c     Data are formatted as numbers expressed in basis codebase (225):
c     First number:   digit2  digit1 digit0
c                     (0 .le. digit0 .le. 255)
c                     (0 .le. digit1 .le. 255)
c                     (0 .le. digit2 .le. 15)
c     Second number:  digit2 digit1 digit0
c                     (0 .le. digit0 .le. 255)
c                     (0 .le. digit1 .le. 255)
c                     (0 .le. digit2 .le. 15)
c
      i1 = -1
      do i = 1, n
c
c       Getting the digits.
c
        do j = 1, 4
          k = ispos + j
          digits(j) = cio2intdigits(ichar(cstring(k:k)), currciofile2)
        enddo
c
c       The last character was set "half and half" with
c       the most significative digits.
c
        ispos = k + 1
        j     = cio2intdigits(ichar(cstring(ispos:ispos)), currciofile2)
        d21   = halfandhalfi1(j)
        d22   = halfandhalfi2(j)
c
c       Reobtaining the integer numbers.
c
        i1 = i1 + 2
        int1(i1)     = d01 + codebase * (d11 + codebase * d21)
        int1(i1 + 1) = d02 + codebase * (d12 + codebase * d22)
c
      enddo
c
      return
      end
c     --- End of routine intintdecode5
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltdecode2(n, alwayspos, cstring, ispos, flt1)
c
c     Decoding two-character strings into floating point numbers.
c     The numbers are encoded in 2 characters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data items to
c                      process.
c     alwayspos....... (input, logical, array(n)) Always positive
c                      numbers switch. If alwayspos(i) is true, then
c                      the sign field is treated as part of the
c                      number exponent, and the resulting number is
c                      always positive.
c     cstring......... (input, character*(*)) The string containing the
c                      encoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last processed character.
c     flt1............ (output, real*8, array(n)) The decoded real
c                      numbers.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      logical           alwayspos(n)
      character*(*)     cstring
      real*8            flt1(n)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      integer           d12, d3, sgexp
      real*8            mantissash2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Data are formatted as numbers expressed in basis codebaseh (15):
c        {0.} mant_1 mant_2 mant_3 sign_and_exponent
c        (0 .le. mant_i .le. codebaseh), j = 1,2,3
c        (0 .le. exponent .le. codebase / 2)
c        (Or (0 .le. exponent .le. codebaseh) for strictly positive
c        numbers).
c
      do i = 1, n
c
c       Getting the digits.
c
        k     = ispos + 1
        d12   = cio2intdigits(ichar(cstring(k:k)), currciofile2)
c
c       The last character was set "half and half" with
c       the last digit of the mantissa and the exponent/sign half
c       character.
c
        ispos = k + 1
        j     = cio2intdigits(ichar(cstring(ispos:ispos)),
     +                        currciofile2)
        d3    = halfandhalfi1(j)
        sgexp = halfandhalfi2(j)
c
c       Reobtaining the number.
c
        mantissash2 = d12 + codebasen1 * d3
        if (alwayspos(i) .or. (sgexp .lt. negshifth)) then
          flt1(i) = mantissash2 * ciofexponentsh2(sgexp)
        else
          flt1(i) = - mantissash2 * ciofexponentsh2(sgexp - negshifth)
        endif
c
      enddo
c
      return
      end
c     --- End of routine fltdecode2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltdecode5(n, cstring, ispos, flt1)
c
c     Decoding 5-character strings into floating point numbers.
c     The numbers are encoded in 5 characters.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data items to
c                      process.
c     cstring......... (input, character*(*)) The string containing the
c                      encoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last processed character.
c     flt1............ (output, real*8, array(n)) The decoded real
c                      numbers.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      character*(*)     cstring
      real*8            flt1(n)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, k0
      integer           dj, sgexp
      real*8            mantissash2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Data are formatted as numbers expressed in basis codebaseh (15):
c        {0.} mant_1 mant_2 ... mant_8 sign_and_exponent
c        (0 .le. mant_i .le. codebaseh), j = 1,...,8
c        (0 .le. exponent .le. codebaseh / 2)
c
      k0 = ispos
      do i = 1, n
c
c       Getting the digits and evaluating the mantissa.
c
        k0 = k0 + 5
        k  = k0
        mantissash2 = 0
        do j = 1, 4
          k  = k0 - j
          dj = cio2intdigits(ichar(cstring(k:k)), currciofile2)
          mantissash2 = dj + codebasen2 * mantissash2
        enddo
c
c       The last character contains the sign/exponent specification.
c
        sgexp = cio2intdigits(ichar(cstring(k0:k0)), currciofile2)
c
c       Reobtaining the number.
c
        if (sgexp .lt. negshift1) then
          flt1(i) = mantissash2 * ciofexponentsh2(sgexp)
        else
          flt1(i) = - mantissash2 * ciofexponentsh2(sgexp - negshift1)
        endif
c
      enddo
      ispos = k0
c
      return
      end
c     --- End of routine fltdecode5
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltfltdecode5(n, alwayspos, cstring, ispos, flt1)
c
c     Decoding 5-character strings into floating point numbers.
c     Pairs of numbers are encoded in 2 1/2 characters each.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data pairs to
c                      process.
c     alwayspos....... (input, logical, array(n)) Always positive
c                      numbers switch. If alwayspos(i) is true, then
c                      the sign field is treated as part of the
c                      number exponent, and the resulting number is
c                      always positive.
c     cstring......... (input, character*(*)) The string containing the
c                      encoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last processed character.
c     flt1............ (output, real*8, array(2 * n)) The decoded real
c                      numbers.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      logical           alwayspos(n)
      character*(*)     cstring
      real*8            flt1(1)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, i1, i2
      integer           sgexp1, sgexp2
      integer           digits(4)
      real*8            mantissash2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Data are formatted as numbers expressed in basis codebaseh (15):
c        {0.} mant_1 ... mant_4 sign_and_exponent
c        (0 .le. mant_i .le. codebaseh), j = 1,...,4
c        (0 .le. exponent .le. codebaseh / 2)
c        (Or (0 .le. exponent .le. codebaseh) for strictly positive
c        numbers).
c
      i2 = 0
      do i = 1, n
c
c       Getting the digits of the mantissas.
c
        do j = 1, 4
          k = ispos + j
          digits(j) = cio2intdigits(ichar(cstring(k:k)), currciofile2)
        enddo
c
c       The last character was set "half and half" with
c       exponent/sign specifications.
c
c       the last digit of the mantissa and the exponent/sign half
c       character.
c
        ispos = k + 1
        j     = cio2intdigits(ichar(cstring(ispos:ispos)),
     +                        currciofile2)
        sgexp1 = halfandhalfi1(j)
        sgexp2 = halfandhalfi2(j)
c
c       Reobtaining the numbers.
c
        i1 = i2 + 1
        i2 = i1 + 1
c
        mantissash2 = digits(1) + codebasen2 * digits(2)
        if (alwayspos(i1) .or. (sgexp1 .lt. negshifth)) then
          flt1(i1) = mantissash2 * ciofexponentsh2(sgexp1)
        else
          flt1(i1) = - mantissash2 *
     +                 ciofexponentsh2(sgexp1 - negshifth)
        endif
c
        mantissash2 = digits(3) + codebasen2 * digits(4)
        if (alwayspos(i2) .or. (sgexp2 .lt. negshifth)) then
          flt1(i2) = mantissash2 * ciofexponentsh2(sgexp2)
        else
          flt1(i2) = - mantissash2 *
     +                 ciofexponentsh2(sgexp2 - negshifth)
        endif
c
      enddo
c
      return
      end
c     --- End of routine fltfltdecode5
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine datidecode(cstring, ispos, dti)
c
c     Decoding a six-character string into the corresponding integer
c     date and time array.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     cstring......... (input, character*(*)) The string containing the
c                      encoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last processed character.
c     dti............. (output, integer, array(6)) The decoded date
c                      and time, in integer format.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      character*(*)     cstring
      integer           ispos
      integer           dti(6)
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k
c
c     FIRST EXECUTABLE STATEMENT
c
c     Decoding.
c
      k      = ispos + 1
      dti(1) = cio2intdigits(ichar(cstring(k:k)), currciofile2)
     +         + 1960
c
      do i = 2, 6
        k = k + 1
        dti(i) = cio2intdigits(ichar(cstring(k:k)), currciofile2)
      enddo
      ispos = k
c
      return
      end
c     --- End of routine datidecode.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine restring(string)
c
c     Processing a string changing the printable characters if
c     necessary. This transformation might be necessary when using data
c     written in a different system.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     string.......... (input-output, character*(*)) The string to
c                      process.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      character*(*)     string
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     If there are no detected transformations in the character set,
c     the string is returned unchanged.
c
      if (normalprtchar2(currciofile2)) return
c
c     Transforming.
c
      do i = 1, len(string)
        string(i:i) = ascarrayprt2(ichar(string(i:i)), currciofile2)
      enddo
c
      return
      end
c     --- End of routine restring.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciodecode.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
