c
c     FILE: initutils2.f                    Creation date: 26/JUN/2003.
c                                       LAST MODIFICATION: 17/JUL/2003.
c
c     This file contains several auxiliary routines for input
c     processing (Part II, routines for the AIRES library).
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getintfromglobal(gvname, defval, irc)
c
c     Interpreting a global variable as an integer number, and
c     retrieving it.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     gvname.......... (input, character*(*)) Global variable name.
c     defval.......... (input, integer) Default value to use if the
c                      global variable is undefined, or not convertible
c                      to an integer number.
c     irc............. (output, integer) Return code. Zero means that
c                      the return value was successfully read in from
c                      the global variable. irc = 1 means that the
c                      global variable is not defined. irc = 3 means
c                      that the string associated with the global
c                      variable cannot be converted to integer.
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
      integer           getintfromglobal
      character*(*)     gvname
      integer           defval
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           l, sdsw
      integer           ival
      double precision  val
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the global variable exists.
c
      call getglobal(gvname, sdsw, auxline, l)
c
      if (l .le. 0) then
c
c       Undefined variable.
c
        getintfromglobal = defval
        irc              = 1
c
      else
c
c       Trying to read the number.
c
        val = defval
        call getnumber(.false., auxline(1:l), l, 1, l, 2, val,
     +                 val, irc)
        ival = val
        if ((irc .eq. 0) .and. (ival .eq. val)) then
          getintfromglobal = ival
        else
          getintfromglobal = defval
        endif
c
      endif
c
      return
      end
c     --- End of routine getintfromglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getrealfromglobal(gvname, defval, irc)
c
c     Interpreting a global variable as a real number, and retrieving
c     it.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     gvname.......... (input, character*(*)) Global variable name.
c     defval.......... (input, double precision) Default value to
c                      use if the global variable is undefined, or
c                      not convertible to a real number.
c     irc............. (output, integer) Return code. Zero means that
c                      the return value was successfully read in from
c                      the global variable. irc = 1 means that the
c                      global variable is not defined. irc = 3 means
c                      that the string associated with the global
c                      variable cannot be converted to real.
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
      double precision  getrealfromglobal
      character*(*)     gvname
      double precision  defval
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           l, sdsw
      double precision  val
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the global variable exists.
c
      call getglobal(gvname, sdsw, auxline, l)
c
      if (l .le. 0) then
c
c       Undefined variable.
c
        getrealfromglobal = defval
        irc               = 1
c
      else
c
c       Trying to read the floating point number.
c
        call getnumber(.false., auxline(1:l), l, 1, l, 2, defval,
     +                 val, irc)
c
        getrealfromglobal = val
c
      endif
c
      return
      end
c     --- End of routine getrealfromglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setglobal(gvname, vval, irc)
c
c     Setting a dynamic global variable. This routine is primarily
c     designed to be used within a special particle module.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     gvname.......... (input, character*(*)) Global variable name.
c     vval............ (input, character*(*)) String with the
c                      value to be set.
c     irc............. (output, integer) Return code. Zero means that
c                      the variable has been set successfully.
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
      character*(*)     gvname, vval
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           l, i1, i2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing the assignment command.
c
      write(auxline, 2010) 'Dynamic', gvname, vval
 2010 format(a, 2(1x, a))
      call strim(-1, auxline, l)
      i1 = 1
      i2 = 7
c
c     Assigning the variable.
c
      call setglobalvar(0, auxline, l, i1, i2, irc)
c
      return
      end
c     --- End of routine setglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setintglobal(gvname, vval, irc)
c
c     Setting an integer value as a dynamic global variable. This
c     routine is primarily designed to be used within a special
c     particle module.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     gvname.......... (input, character*(*)) Global variable name.
c     vval............ (input, integer) Integer value to set.
c     irc............. (output, integer) Return code. Zero means that
c                      the variable has been set successfully.
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
      character*(*)     gvname
      integer           vval
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           l, i1, i2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing the assignment command.
c
      write(auxline, 2010) 'Dynamic', gvname, vval
 2010 format(a, 1x, a, i12)
      call strim(-1, auxline, l)
      i1 = 1
      i2 = 7
c
c     Assigning the variable.
c
      call setglobalvar(0, auxline, l, i1, i2, irc)
c
      return
      end
c     --- End of routine setintglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setrealglobal(gvname, vval, irc)
c
c     Setting a real value as a dynamic global variable. This
c     routine is primarily designed to be used within a special
c     particle module.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     gvname.......... (input, character*(*)) Global variable name.
c     vval............ (input, double precision) Real value to set.
c     irc............. (output, integer) Return code. Zero means that
c                      the variable has been set successfully.
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
      character*(*)     gvname
      double precision  vval
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           l, i1, i2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing the assignment command.
c
      write(auxline, 2010) 'Dynamic', gvname, vval
 2010 format(a, 1x, a, 1p, g26.16e3)
      call strim(-1, auxline, l)
      i1 = 1
      i2 = 7
c
c     Assigning the variable.
c
      call setglobalvar(0, auxline, l, i1, i2, irc)
c
      return
      end
c     --- End of routine setrealglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initutils2.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
