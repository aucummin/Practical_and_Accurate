c
c     FILE: cioh1infofc.f                   Creation date: 18/JUN/1998.
c                                       LAST MODIFICATION: 18/APR/1999.
c
c     Aires compressed i/o system (IIIa): Some utilities to process
c     already created compressed files from a C environment.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fccrotaskid(itskname, tsknamelen, tskversion,
     +                       istartdate)
c
c     Getting task name and date from an already read compressed file
c     header.
c
c     Written by: S. J. Sciutto, Fermilab 1999.
c
c
c     Arguments:
c     =========
c
c     itskname........ (output, integer, array(*)) Task name (maximum
c                      length is 64 characters).
c     tsknamelen...... (output, integer) Length of task name.
c     tskversion...... (output, integer) Task version.
c     istartdate...... (output, integer, array(*)) Task starting date,
c                      in the format "dd/mm/yyyy hh:mm:ss".
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           itskname(1)
      integer           tsknamelen, tskversion
      integer           istartdate(1)
c
c     Declaration of internal variables and arrays.
c
      character*64      tskname
      character*20      startdate
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking the original procedure.
c
      call crotaskid(tskname, tsknamelen, tskversion,
     +               startdate)
c
c     Converting character strings into ascii codes.
c
      call fcharc(tskname, tsknamelen, itskname)
      call fcharc(startdate, 20, istartdate)
c
      return
c
      end
c     --- End of routine fccrotaskid
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fcidlcheck(iidldir)
c
c     Checking a string to see if it matches any of the IDL
c     instructions currently defined, that is, the ones corresponding
c     to the last opened compressed file. Special version called from C
c     routine "idlcheckc".
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c
c     Arguments:
c     =========
c
c     iidldir......... (input, integer, array(*)) The IDL directive.
c                      Abbreviations are accepted accordingly with the
c                      usual abbreviation rules, encoded as an integer
c                      array of ascii codes.
c
c     Return value: (integer) If an error occurs, then the returned
c     ============  value will be negative. Other return values are the
c                   following:
c                   0 The string does not match any of the currently
c                     valid IDL instructions.
c                   1 The string matches a directive belonging to the
c                     "basic" instruction set with no parameter(s)
c                     associated with it, for example "Help".
c                   2 The string matches a directive belonging to the
c                     "basic" instruction set. If there is a parameter
c                     associated with the directive, then it can be
c                     obtained by means of routine "croinputdata0".
c                   4 The directive corresponds to a real input
c                     parameter. The parameter can be retrieved by
c                     means of function "getinpreal".
c                   6 The directive corresponds to an integer input
c                     parameter. The parameter can be retrieved by
c                     means of function "getinpint".
c                   8 The directive corresponds to a logical input
c                     parameter. The parameter can be retrieved by
c                     means of function "getinpswitch".
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           fcidlcheck
      integer           iidldir(1)
c
c     Declaration of internal variables and arrays.
c
      character*24      idldir
      integer           lfn
      integer           idlcheck
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(iidldir, idldir, lfn)
c
c     Invoking the original procedure.
c
      fcidlcheck = idlcheck(idldir)
c
      return
      end
c     --- End of routine fcidlcheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cioh1infofc.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
