c
c     FILE: ciocode.f                       Creation date: 27/NOV/1996.
c                                       LAST MODIFICATION: 10/NOV/1997.
c
c     This file contains the routines for coding data for the
c     compressed i/o system
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intcode2(n, int1, cstring, ispos)
c
c     Coding integers in two-character strings.
c     It is assumed that there is enough space in "cstring" to store
c     all the required characters (2 * n).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data items to
c                      process.
c     int1............ (input, integer, array(n)) The integers to
c                      code. Must belong to the interval [0, codemax2].
c     cstring......... (output, character*(*)) The string to place the
c                      endoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last character of "cstring" used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      integer           int1(n)
      character*(*)     cstring
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k
      integer           n1, nna, nnb
      integer           d0, d1
c
c     FIRST EXECUTABLE STATEMENT
c
c     Formatting data as numbers expressed in basis codebase (225):
c        digit1 digit0
c        (0 .le. digit0 .le. codemax1)
c        (0 .le. digit1 .le. codemax1)
c
      do i = 1, n
c
        n1 = int1(i)
c
        nna = n1 / codebase
        d0  = n1 - nna * codebase
        nnb = nna / codebase
        d1  = nna - nnb * codebase
c
c       Transforming digits into characters.
c
        k = ispos + 1
        cstring(k:k) = ciochrdigits(d0)
        ispos = k + 1
        cstring(ispos:ispos) = ciochrdigits(d1)
c
      enddo
c
      return
      end
c     --- End of routine intcode2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intintcode4(n, int1, int2, cstring, ispos)
c
c     Coding sets of two integers in four-character strings.
c     First integer in 1 1/2 character and second integer in 2 1/2
c     characters.
c     It is assumed that there is enough space in "cstring" to store
c     all the required characters (4 * n).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data pairs to
c                      process.
c     int1............ (input, integer, array(n)) First datum of the
c                      pairs. Must be an integer between 0 and
c                      codemax1h.
c     int2............ (input, integer, array(n)) Second datum of the
c                      pairs. Must be an integer between 0 and
c                      codemax2h.
c     cstring......... (output, character*(*)) The string to place the
c                      endoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last character of "cstring" used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      integer           int1(n), int2(n)
      character*(*)     cstring
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      integer           n1, n2, nna, nnb
      integer           d01, d11, d02, d12, d22
      integer           digits(3)
      equivalence       (d01, digits(1))
      equivalence       (d02, digits(2))
      equivalence       (d12, digits(3))
c
c     FIRST EXECUTABLE STATEMENT
c
c     Formatting data as numbers expressed in basis codebase (225):
c     First number:   digit1 digit0
c                     (0 .le. digit0 .le. codemax1)
c                     (0 .le. digit1 .le. codemaxh)
c     Second number:  digit2 digit1 digit0
c                     (0 .le. digit0 .le. codemax1)
c                     (0 .le. digit1 .le. codemax1)
c                     (0 .le. digit2 .le. codemaxh)
c
      do i = 1, n
c
        n1 = int1(i)
        n2 = int2(i)
c
        nna = n1 / codebase
        d01 = n1 - nna * codebase
        nnb = nna / codebase
        d11 = nna - nnb * codebase
c
        nna = n2 / codebase
        d02 = n2 - nna * codebase
        nnb = nna / codebase
        d12 = nna - nnb * codebase
        nna = nnb / codebase
        d22 = nnb - nna * codebase
c
c       Transforming digits into characters.
c
        do j = 1, 3
          k = ispos + j
          cstring(k:k) = ciochrdigits(digits(j))
        enddo
c
c       The last character is set "half and half" with
c       the most significative digits.
c
        ispos = k + 1
        cstring(ispos:ispos) = halfandhalfch(d11, d22)
c
      enddo
c
      return
      end
c     --- End of routine intintcode4
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intintcode5(n, int1, cstring, ispos)
c
c     Coding sets of two integers in four-character strings, each one
c     of the integers in 2 1/2 characters.
c     It is assumed that there is enough space in "cstring" to store
c     all the required characters (5 * n).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data pairs to
c                      process.
c     int1............ (input, integer, array(2 * n)) The integers
c                      to code. Every number must be in the range
c                      0,...,codemax2h.
c     cstring......... (output, character*(*)) The string to place the
c                      endoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last character of "cstring" used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      integer           int1(1)
      character*(*)     cstring
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, i1, j, k
      integer           n1, n2, nna, nnb
      integer           d01, d11, d21, d02, d12, d22
      integer           digits(4)
      equivalence       (d01, digits(1))
      equivalence       (d11, digits(2))
      equivalence       (d02, digits(3))
      equivalence       (d12, digits(4))
c
c     FIRST EXECUTABLE STATEMENT
c
c     Formatting data as numbers expressed in basis codebase (225):
c     First number:   digit2  digit1 digit0
c                     (0 .le. digit0 .le. codemax1)
c                     (0 .le. digit1 .le. codemax1)
c                     (0 .le. digit2 .le. codemaxh)
c     Second number:  digit2 digit1 digit0
c                     (0 .le. digit0 .le. codemax1)
c                     (0 .le. digit1 .le. codemax1)
c                     (0 .le. digit2 .le. codemaxh)
c
      i1 = -1
      do i = 1, n
c
        i1 = i1 + 2 
        n1 = int1(i1)
        n2 = int1(i1 + 1)
c
        nna = n1 / codebase
        d01 = n1 - nna * codebase
        nnb = nna / codebase
        d11 = nna - nnb * codebase
        nna = nnb / codebase
        d21 = nnb - nna * codebase
c
        nna = n2 / codebase
        d02 = n2 - nna * codebase
        nnb = nna / codebase
        d12 = nna - nnb * codebase
        nna = nnb / codebase
        d22 = nnb - nna * codebase
c
c       Transforming digits into characters.
c
        do j = 1, 4
          k = ispos + j
          cstring(k:k) = ciochrdigits(digits(j))
        enddo
c
c       The last character is set "half and half" with
c       the most significative digits.
c
        ispos = k + 1
        cstring(ispos:ispos) = halfandhalfch(d21, d22)
c
      enddo
c
      return
      end
c     --- End of routine intintcode5
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltcode2(n, flt1, cstring, ispos)
c
c     Coding floating point numbers in 2-character strings.
c     It is assumed that there is enough space in "cstring" to store
c     all the required characters (2 * n).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data items to
c                      process.
c     flt1............ (input, double precision, array(n)) The real
c                      numbers to code. Must belong to the interval
c                      [-cioflmax2n, cioflmax2]
c     cstring......... (output, character*(*)) The string to place the
c                      endoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last character of "cstring" used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      double precision  flt1(n)
      character*(*)     cstring
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k
      double precision  fn1, fna, mantissa
      integer           exponent
      integer           d12, d3
c
c     FIRST EXECUTABLE STATEMENT
c
c     Formatting data as numbers expressed in basis codebaseh (15):
c        {0.} mant_1 mant_2 mant_3 sign_and_exponent
c        (0 .le. mant_i .le. codebaseh), j = 1,2,3
c        (0 .le. exponent .le. codebaseh / 2)
c        (Or (0 .le. exponent .le. codebaseh) for strictly positive
c        numbers).
c
      do i = 1, n
c
        fn1 = flt1(i)
        fna = abs(fn1)
c
c       Evaluating the exponent.
c
        if (fna .lt. codebasep0) then
          exponent = 0
          mantissa = fna
c
        else if (fna .lt. codebasep1) then
          exponent = 1
          mantissa = fna * codebasen1
c
        else if (fna .lt. codebasep2) then
          exponent = 2
          mantissa = fna * codebasen2
c
        else if (fna .lt. codebasep3) then
          exponent = 3
          mantissa = fna * codebasen3
c
        else if (fna .lt. codebasep4) then
          exponent = 4
          mantissa = fna * codebasen4
c
        else if (fna .lt. codebasep5) then
          exponent = 5
          mantissa = fna * codebasen5
c
        else if (fna .lt. codebasep6) then
          exponent = 6
          mantissa = fna * codebasen6
c
c       The following options apply only for strictly positive numbers.
c
        else if (fna .lt. codebasep7) then
          exponent = 7
          mantissa = fna * codebasen7
c
        else if (fna .lt. codebasep8) then
          exponent = 8
          mantissa = fna * codebasen8
c
        else if (fna .lt. codebasep9) then
          exponent = 9
          mantissa = fna * codebasen9
c
        else if (fna .lt. codebasepa) then
          exponent = 10
          mantissa = fna * codebasena
c
        else if (fna .lt. codebasepb) then
          exponent = 11
          mantissa = fna * codebasenb
c
        else if (fna .lt. codebasepc) then
          exponent = 12
          mantissa = fna * codebasenc
c
        else if (fna .lt. codebasepd) then
          exponent = 13
          mantissa = fna * codebasend
c
        else if (fna .lt. codebasepe_r2) then
          exponent = 14
          mantissa = fna * codebasene
c
        else
c
c         Overflow. Parameter "codebasepe_r2" takes into account
c         the eventual correction for rounding.
c
          call errprint(2, '$COF', 4, 'fltcode2', ' ',
     +                  0, 0, 1, fn1, ' ')
c
        endif
c
c       Rounding the mantissa.
c
        mantissa = mantissa + ciofround2
c
        if (mantissa .gt. 1) then
          mantissa = mantissa * codebasen1
          exponent = exponent + 1
        endif
c
c       Completing the sign/exponent evaluation.
c
        if (fn1 .lt. 0) then
c
c         Checking overflow for negative numbers.
c
          if (exponent .gt. 6) then
            call errprint(2, '$COF', 4, 'fltcode2',
     +                    '(Negative number overflow).',
     +                    0, 0, 1, fn1, ' ')
          endif
c
          exponent = exponent + negshifth
        endif
c
c       Evaluating the digits.
c
        mantissa = codebase * mantissa
        d12      = mantissa
        mantissa = mantissa - d12
        d3       = codebaseh * mantissa
c
c       Transforming digits into characters.
c
        k = ispos + 1
        cstring(k:k) = ciochrdigits(d12)
c
c       The last character is set "half and half" with
c       the last digit of the mantissa and the exponent/sign half
c       character.
c
        ispos = k + 1
        cstring(ispos:ispos) = halfandhalfch(d3, exponent)
c
      enddo
c
      return
      end
c     --- End of routine fltcode2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltcode5(n, flt1, cstring, ispos)
c
c     Coding floating point numbers in 5-character strings.
c     It is assumed that there is enough space in "cstring" to store
c     all the required characters (5 * n).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data items to
c                      process.
c     flt1............ (input, double precision, array(n)) The real
c                      numbers to code. Must belong to the interval
c                      [-cioflmax5n, cioflmax5]
c     cstring......... (output, character*(*)) The string to place the
c                      endoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last character of "cstring" used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      double precision  flt1(n)
      character*(*)     cstring
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      double precision  fn1, fna, mantissa
      integer           exponent
      integer           dj
c
c     FIRST EXECUTABLE STATEMENT
c
c     Formatting data as numbers expressed in basis codebaseh (15):
c        {0.} mant_1 mant_2 ... mant_8 sign_and_exponent
c        (0 .le. mant_i .le. codebaseh), j = 1,...,8
c        (0 .le. exponent .le. codebaseh / 2)
c
      k = ispos
      do i = 1, n
c
        fn1 = flt1(i)
        fna = abs(fn1)
c
c       Evaluating the exponent.
c
        if (fna .lt. codebasep0) then
          exponent = 0
          mantissa = fna
c
        else if (fna .lt. cioflmax5_r) then
          exponent = uciologbase * log(fna)
          exponent = exponent + 1
          mantissa = fna / ciofexponentsh2(exponent + 2)
c
        else
c
c         Overflow. Parameter "cioflmax5_r" takes into account
c         the eventual correction for rounding.
c
          call errprint(2, '$COF', 4, 'fltcode5', ' ',
     +                  0, 0, 1, fn1, ' ')
c
        endif
c
c       Rounding.
c
        mantissa = mantissa + ciofround5
c
        if (mantissa .gt. 1) then
          mantissa = mantissa * codebasen1
          exponent = exponent + 1
        endif
c
c       Evaluating the digits and transforming them into characters.
c
        do j = 1, 4
c
          mantissa = codebase * mantissa
          dj       = mantissa
          mantissa = mantissa - dj
c
          k = k + 1
          cstring(k:k) = ciochrdigits(dj)
c
        enddo
c
c       Setting the exponent and sign.
c
        if (fn1 .lt. 0) exponent = exponent + negshift1
        k = k + 1
        cstring(k:k) = ciochrdigits(exponent)
c
      enddo
      ispos = k
c
      return
      end
c     --- End of routine fltcode5
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fltfltcode5(n, flt1, cstring, ispos)
c
c     Coding sets of two floating point numbers in 5-character strings,
c     each one of the numbers in 2 1/2 characters.
c     It is assumed that there is enough space in "cstring" to store
c     all the required characters (5 * n).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     n............... (input, integer) The number of data pairs to
c                      process.
c     flt1............ (input, double precision, array(2 * n)) The real
c                      numbers to code. Must belong to the interval
c                      [-cioflmax2hn, cioflmax2h]
c     cstring......... (output, character*(*)) The string to place the
c                      endoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last character of "cstring" used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           n, ispos
      double precision  flt1(1)
      character*(*)     cstring
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, i1
      double precision  fn1, fn2, fna, mantissa
      integer           exponent1, exponent2
      integer           digits(4)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Formatting data as numbers expressed in basis codebaseh (15):
c        {0.} mant_1 ... mant_4 sign_and_exponent
c        (0 .le. mant_i .le. codebaseh), j = 1,...,4
c        (0 .le. exponent .le. codebaseh / 2)
c        (Or (0 .le. exponent .le. codebaseh) for strictly positive
c        numbers).
c
      i1 = -1
      do i = 1, n
c
        i1 = i1 + 2
        fn1 = flt1(i1)
        fn2 = flt1(i1 + 1)
c
c       Processing first number.
c
        fna = abs(fn1)
c
c       Evaluating the exponent.
c
        if (fna .lt. codebasep0) then
          exponent1 = 0
          mantissa  = fna
c
        else if (fna .lt. codebasep1) then
          exponent1 = 1
          mantissa  = fna * codebasen1
c
        else if (fna .lt. codebasep2) then
          exponent1 = 2
          mantissa  = fna * codebasen2
c
        else if (fna .lt. codebasep3) then
          exponent1 = 3
          mantissa  = fna * codebasen3
c
        else if (fna .lt. codebasep4) then
          exponent1 = 4
          mantissa  = fna * codebasen4
c
        else if (fna .lt. codebasep5) then
          exponent1 = 5
          mantissa  = fna * codebasen5
c
        else if (fna .lt. codebasep6) then
          exponent1 = 6
          mantissa  = fna * codebasen6
c
c       The following options apply only for strictly positive numbers.
c
        else if (fna .lt. codebasep7) then
          exponent1 = 7
          mantissa  = fna * codebasen7
c
        else if (fna .lt. codebasep8) then
          exponent1 = 8
          mantissa  = fna * codebasen8
c
        else if (fna .lt. codebasep9) then
          exponent1 = 9
          mantissa  = fna * codebasen9
c
        else if (fna .lt. codebasepa) then
          exponent1 = 10
          mantissa  = fna * codebasena
c
        else if (fna .lt. codebasepb) then
          exponent1 = 11
          mantissa  = fna * codebasenb
c
        else if (fna .lt. codebasepc) then
          exponent1 = 12
          mantissa  = fna * codebasenc
c
        else if (fna .lt. codebasepd) then
          exponent1 = 13
          mantissa  = fna * codebasend
c
        else if (fna .lt. codebasepe_r2h) then
          exponent1 = 14
          mantissa  = fna * codebasene
c
        else
c
c         Overflow. Parameter "codebasepe_r2h" takes into account
c         the eventual correction for rounding.
c
          call errprint(2, '$COF', 4, 'fltfltcode5', ' ',
     +                  0, 0, 1, fn1, ' ')
c
        endif
c
c       Rounding the mantissa.
c
        mantissa = mantissa + ciofround2h
c
        if (mantissa .gt. 1) then
          mantissa  = mantissa * codebasen1
          exponent1 = exponent1 + 1
        endif
c
c       Completing the sign/exponent evaluation.
c
        if (fn1 .lt. 0) then
c
c         Checking overflow for negative numbers.
c
          if (exponent1 .gt. 6) then
            call errprint(2, '$COF', 4, 'fltfltcode5',
     +                    '(Negative number overflow).',
     +                    0, 0, 1, fn1, ' ')
          endif
c
          exponent1 = exponent1 + negshifth
        endif
c
c       Evaluating the digits.
c
        mantissa  = codebase * mantissa
        digits(1) = mantissa
        mantissa  = mantissa - digits(1)
        digits(2) = codebase * mantissa
c
c       Processing second number.
c
        fna = abs(fn2)
c
c       Evaluating the exponent.
c
        if (fna .lt. codebasep0) then
          exponent2 = 0
          mantissa  = fna
c
        else if (fna .lt. codebasep1) then
          exponent2 = 1
          mantissa  = fna * codebasen1
c
        else if (fna .lt. codebasep2) then
          exponent2 = 2
          mantissa  = fna * codebasen2
c
        else if (fna .lt. codebasep3) then
          exponent2 = 3
          mantissa  = fna * codebasen3
c
        else if (fna .lt. codebasep4) then
          exponent2 = 4
          mantissa  = fna * codebasen4
c
        else if (fna .lt. codebasep5) then
          exponent2 = 5
          mantissa  = fna * codebasen5
c
        else if (fna .lt. codebasep6) then
          exponent2 = 6
          mantissa  = fna * codebasen6
c
c       The following options apply only for strictly positive numbers.
c
        else if (fna .lt. codebasep7) then
          exponent2 = 7
          mantissa  = fna * codebasen7
c
        else if (fna .lt. codebasep8) then
          exponent2 = 8
          mantissa  = fna * codebasen8
c
        else if (fna .lt. codebasep9) then
          exponent2 = 9
          mantissa  = fna * codebasen9
c
        else if (fna .lt. codebasepa) then
          exponent2 = 10
          mantissa  = fna * codebasena
c
        else if (fna .lt. codebasepb) then
          exponent2 = 11
          mantissa  = fna * codebasenb
c
        else if (fna .lt. codebasepc) then
          exponent2 = 12
          mantissa  = fna * codebasenc
c
        else if (fna .lt. codebasepd) then
          exponent2 = 13
          mantissa  = fna * codebasend
c
        else if (fna .lt. codebasepe_r2h) then
          exponent2 = 14
          mantissa  = fna * codebasene
c
        else
c
c         Overflow. Parameter "codebasepe_r2h" takes into account
c         the eventual correction for rounding.
c
          call errprint(2, '$COF', 4, 'fltfltcode5', ' ',
     +                  0, 0, 1, fn2, ' ')
c
        endif
c
c       Rounding the mantissa.
c
        mantissa = mantissa + ciofround2h
c
        if (mantissa .gt. 1) then
          mantissa  = mantissa * codebasen1
          exponent2 = exponent2 + 1
        endif
c
c       Completing the sign/exponent evaluation.
c
        if (fn2 .lt. 0) then
c
c         Checking overflow for negative numbers.
c
          if (exponent2 .gt. 6) then
            call errprint(2, '$COF', 4, 'fltfltcode5',
     +                    '(Negative number overflow).',
     +                    0, 0, 1, fn2, ' ')
          endif
c
          exponent2 = exponent2 + negshifth
        endif
c
c       Evaluating the digits.
c
        mantissa  = codebase * mantissa
        digits(3) = mantissa
        mantissa  = mantissa - digits(3)
        digits(4) = codebase * mantissa
c
c       Transforming digits into characters.
c
        do j = 1, 4
          k = ispos + j
          cstring(k:k) = ciochrdigits(digits(j))
        enddo
c
c       The last character is set "half and half" with the
c       exponent/sign specifications.
c
        ispos = k + 1
        cstring(ispos:ispos) = halfandhalfch(exponent1, exponent2)
c
      enddo
c
      return
      end
c     --- End of routine fltfltcode5
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine daticode(dti, cstring, ispos)
c
c     Coding an integer date and time specification in a six-character
c     string.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     dti............. (input, integer, array(6)) Integer date and
c                      time array, in the format used by routine
c                      "intdati".
c     cstring......... (output, character*(*)) The string to place the
c                      endoded data.
c     ispos........... (input-output, integer) As input: Position of
c                      first character of "cstring" to use, minus 1.
c                      As output: Last character of "cstring" used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           dti(6)
      character*(*)     cstring
      integer           ispos
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the input data.
c
      if (dti(1) .lt. 1960) then
        call errprint(0, '*', 4, 'daticode',
     +  'This program does not support travels to the past!',
     +  1, dti(1), 0, 0.d0, ' ')
      else if (dti(1) .gt. 2184) then
        call errprint(0, '*', 4, 'daticode',
     +  'It''s time to change the simulation program, right?',
     +  1, dti(1), 0, 0.d0, ' ')
      endif
c
c     Encoding.
c
      k = ispos + 1
      cstring(k:k) = ciochrdigits(dti(1) - 1960)
c
      do i = 2, 6
        k = k + 1
        cstring(k:k) = ciochrdigits(dti(i))
      enddo
      ispos = k
c
      return
      end
c     --- End of routine daticode.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciocode.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
