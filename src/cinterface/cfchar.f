c
c     FILE: cfchar.f                        Creation date: 28/MAY/1997.
c                                       LAST MODIFICATION: 02/MAR/2001.
c
c     Character string management routine for C-FORTRAN interface.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cfchar(icharray, string, slen)
c
c     Converting a integer array containing ascii character codes into
c     a FORTRAN CHARACTER string.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     icharray........ (input, integer, array(*)) Integer array with
c                      the string's ascii codes, ending with 0 (not
c                      included in the string).
c     string.......... (output, character*(*)) The output string.
c                      Enough length must be ensured by the calling
c                      program.
c     slen............ (output, integer) The length of the converted
c                      string.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           icharray(1)
      character*(*)     string
      integer           slen
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      slen   = 0
      string = ' '
c
      do i = 1, len(string)
        if (icharray(i) .le. 0) return
        slen = i
        string(i:i) = char(icharray(i))
      enddo
      return
c
      end
c     --- End of routine cfchar.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcharc(string, slen, icharray)
c
c     Converting a FORTRAN CHARACTER string into an integer array
c     containing ascii character codes.
c
c     Written by: S. J. Sciutto, Fermilab 1999; La Plata 2001.
c
c
c     Arguments:
c     =========
c
c     string.......... (input, character*(*)) The input string.
c     slen............ (input, integer) The length of the string to
c                      be converted.
c     icharray........ (output, integer, array(*)) Integer array with
c                      the string's ascii codes, ending with 0 (not
c                      included in the string). Enough length must be
c                      ensured by the calling program.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     string
      integer           slen
      integer           icharray(1)
c
c     Declaration of internal variables and arrays.
c
      integer           i, imax
c
c     FIRST EXECUTABLE STATEMENT
c
      imax = max(slen, 0)
      do i = 1, imax
        icharray(i) = ichar(string(i:i))
      enddo
      icharray(imax + 1) = 0
      return
c
      end
c     --- End of routine fcharc.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cfchar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
