c
c     FILE: speilibfc.f                     Creation date: 29/OCT/1999.
c                                       LAST MODIFICATION: 01/NOV/1999.
c
c     Library of routines related to the "Special primary"
c     external interface. Some utilities needed to build C external
c     programs to process special primaries.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcsprimname(ipname, pnamelen)
c
c     Getting the name of the special primary particle specified in the
c     corresponding IDL instruction. Special version called from C
c     routine "sprimnamec".
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     ipname.......... (output, integer, array(*)) Particle name. The
c                      calling program must provide enough space to
c                      store this string.
c     pnamelen........ (output, integer) Lenght of the particle name
c                      string.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ipname(1)
      integer           pnamelen
c
c     Declaration of internal variables and arrays.
c
      character*16      pname
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking the original procedure.
c
      call sprimname(pname, pnamelen)
c
c     Converting character string into ascii codes.
c
      call fcharc(pname, pnamelen, ipname)
c
      return
      end
c     --- End of routine fcsprimname
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcspeitask(itaskn, tasklen, tver)
c
c     Getting the task name and version corresponding to the current
c     shower. Special version called from C routine speitaskc
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     itaskn.......... (output, integer, array(*)) Task name. The
c                      calling program must provide enough space to
c                      store this string.
c     tasklen......... (output, integer) Lenght of the task name.
c     tver............ (output, integer) Task name version.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           itaskn(1)
      integer           tasklen, tver
c
c     Declaration of internal variables and arrays.
c
      character*64      tskname
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking the original procedure.
c
      call speitask(tskname, tasklen, tver)
c
c     Converting character string into ascii codes.
c
      call fcharc(tskname, tasklen, itaskn)
c
      return
      end
c     --- End of routine fcspeitask
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcspeigetmodname(imn, mnlen, imnfull, mnfullen)
c
c     Getting the name of the module invoked by the simulation
c     program. Special version called from C routine speigetmodnamec.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     imn............. (output, integer, array(*)) Name of external
c                      module. The calling program must provide enough
c                      space to store this array.
c     mnlen........... (output, integer) Lenght of string "mn".
c     imnfull......... (output, integer, array(*)) Full name of
c                      external module (Will be different of "imn" if
c                      the module was placed within one of the
c                      InputPath directories). The calling program
c                      must provide enough space to store this array.
c     mnfullen........ (output, integer) Lenght of string "mnfull".
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           imn(1), imnfull(1)
      integer           mnlen, mnfullen
c
c     Declaration of internal variables and arrays.
c
      character*176     mn
      character*512     mnfull
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking the original procedure.
c
      call speigetmodname(mn, mnlen, mnfull, mnfullen)
c
c     Converting character strings into ascii codes.
c
      call fcharc(mn, mnlen, imn)
      call fcharc(mnfull, mnfullen, imnfull)
c
      return
      end
c     --- End of routine fcspeigetmodname
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcspeigetpars(iparstring, pstrlen)
c
c     Getting the parameter string specified in the corresponding
c     IDL instruction. Special version called from C routine
c     speigetparsc.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     iparstring...... (output, integer, array(*)) Parameter string.
c                      The calling program must provide enough space to
c                      store this string.
c     pstrlen......... (output, integer) Lenght of the parameter
c                      string. Zero if there are no parameters.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iparstring(1)
      integer           pstrlen
c
c     Declaration of internal variables and arrays.
c
      character*176     parstring
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking the original procedure.
c
      call speigetpars(parstring, pstrlen)
c
c     Converting character string into ascii codes.
c
      call fcharc(parstring, pstrlen, iparstring)
c
      return
      end
c     --- End of routine fcspeigetpars
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'speilibfc.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
