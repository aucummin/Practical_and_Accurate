c
c     FILE: ciomgr0.f                       Creation date: 27/NOV/1996.
c                                       LAST MODIFICATION: 15/MAR/2001.
c
c     Aires compressed i/o system (0): General initialization routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioinit0
c
c     Initializing the intrinsic arrays of the i/o system.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'pclepar.f'
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, i1, i2, n
      character*1       chj
      character*32      specialsymbols
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the native digit arrays.
c     They are formed by ascii characters, excluding some problematic
c     ones.
c     This code includes now support for systems where ichar can
c     return negative values (in the range [-128, 127], instead of
c     [0, 255]).
c
      do i = -256, 255
        ciointdigits(i) = 0
      enddo
c
      do i = codemax1, 0, -1
        j   = i + (255 - codemax1)
        chj = char(j)
        k   = ichar(chj)
        ciochrdigits(i) = chj
        ciointdigits(k) = i
      enddo
c
c     Eliminating erase character.
c
      j   = 127
      chj = char(j)
      k   = ichar(chj)
      ciointdigits(k) = 0
      i   = 127 - (255 - codemax1)
      j   = (255 - codemax1) - 1
      chj = char(j)
      k   = ichar(chj)
      ciochrdigits(i)   = chj
      ciointdigits(k)   = i
c
c     Local printable character data.
c
      numalseq0(1, 1) = ichar('0')
      numalseq0(2, 1) = ichar('9')
      numalseq0(1, 2) = ichar('a')
      numalseq0(2, 2) = ichar('z')
      numalseq0(1, 3) = ichar('A')
      numalseq0(2, 3) = ichar('Z')
c
      specialsymbols  = ' !"#$%&''()*+-./:;<=>?@[]^_`{|}~\\'
c
      do i = 1, 32
        speseq0(i) = ichar(specialsymbols(i:i))
      enddo
c
c     Printable character data arrays.
c
      chj = char(0)
      do i = -256, 255
        ascarrayprt(i) = chj
      enddo
      do i = 0, 255
        chj = char(i)
        k   = ichar(chj)
        ascarrayprt(k) = chj
      enddo
c
      do j = 1, 3
        do i = 1, 2
          numalchr(i, j) = ciochrdigits(ciointdigits(numalseq0(i, j)))
        enddo
      enddo
c
      do i = 1, 32
        spechr(i) = ciochrdigits(ciointdigits(speseq0(i)))
      enddo
c
c     Setting the arrays for coding and decoding integer numbers.
c
      do i2 = 0, codemaxh
        do i1 = 0, codemaxh
          n = i1 + codebaseh * i2
          halfandhalfch(i1, i2) = ciochrdigits(n)
          halfandhalfi1(n)      = i1
          halfandhalfi2(n)      = i2
        enddo
      enddo
c
c     Setting the arrays for coding and decoding real numbers.
c
      ciofexponentsh2(2) = 1
      do i = 3, iexpmax1 + 2
        ciofexponentsh2(i) = ciofexponentsh2(i - 1) * codebasep1
      enddo
      do i = 1, 0, -1
        ciofexponentsh2(i) = ciofexponentsh2(i + 1) / codebasep1
      enddo
c
c     Setting the particle code shift parameters (these values will be
c     saved in the file headers.
c
      ciopclecodedata(1) = maxpcle
      ciopclecodedata(2) = minncode
      ciopclecodedata(3) = maxncode
c
c     Setting the lengths of the field types available.
c
c     Type 1: One integer in 1 1/2 bytes and another in 2 1/2 bytes.
c
      i = intint4ftype
      ciofieldid(i)  = 10001
      ciofieldlen(i) = 4
      nsubfields(i)  = 2
c
c     (Only the second sub-field can be scaled)
c
      maxfieldint(i) = codemax2h
c
c     Type 2: One integer in 2 1/2 bytes and another in 2 1/2 bytes.
c
      i = intint5ftype
      ciofieldid(i)  = 10002
      ciofieldlen(i) = 5
      nsubfields(i)  = 2
      maxfieldint(i) = codemax2h
c
c     Type 3: One integer in 2 bytes.
c
      i = int2ftype
      ciofieldid(i)  = 10003
      ciofieldlen(i) = 2
      nsubfields(i)  = 1
      maxfieldint(i) = codemax2
c
c     Fields for real numbers (mini floating point format).
c
c     Type 4: Two real numbers in 5 bytes (2 1/2 each).
c
      i = fltflt5ftype
      ciofieldid(i)   = 20001
      ciofieldlen(i)  = 5
      nsubfields(i)   = 2
      maxfieldflt(i)  = cioflmax2h
      maxfieldfltn(i) = cioflmax2hn
      maxfieldfltx(i) = cioflmax2hx
c
c     Type 5: One real number in 2 bytes.
c
      i = flt2ftype
      ciofieldid(i)   = 20002
      ciofieldlen(i)  = 2
      nsubfields(i)   = 1
      maxfieldflt(i)  = cioflmax2
      maxfieldfltn(i) = cioflmax2n
      maxfieldfltx(i) = cioflmax2x
c
c     Type 6: One real number in 5 bytes.
c
      i = flt5ftype
      ciofieldid(i)   = 20003
      ciofieldlen(i)  = 5
      nsubfields(i)   = 1
      maxfieldflt(i)  = cioflmax5
      maxfieldfltn(i) = cioflmax5n
      maxfieldfltx(i) = cioflmax5
c
c     Type 7: Date and time in 6 bytes.
c
      i = datiftype
      ciofieldid(i)  = 50001
      ciofieldlen(i) = 6
      nsubfields(i)  = 1
c
c     No more field types available.
c
c     All done. Setting the init lock
c
      cioinitlock = 3966
c
      return
      end
c     --- End of routine cioinit0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciomgr0.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
