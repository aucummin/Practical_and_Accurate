c
c     FILE: initutilsfc.f                   Creation date: 20/MAR/1998.
c                                       LAST MODIFICATION: 17/JUL/2003.
c
c     This file contains several auxiliary routines for input
c     processing from a C environment.
c     This file covers all routines corresponding to AIRES source files
c     initutils.f and initutils2.f
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fcgetinpreal(idirname)
c
c     Getting the current value for an already defined real input
c     parameter. Special version called from routine "getinprealc".
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c
c     Arguments:
c     =========
c
c     idirname......... (input, integer, array(*)) Name of the
c                       parameter, encoded as an integer array of
c                       ascii codes.
c
c     Return value: (double precision) The current parameter value.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  fcgetinpreal
      integer           idirname(1)
c
c     Declaration of internal variables and arrays.
c
      character*20      dirname
      integer           ldn
      double precision  getinpreal
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(idirname, dirname, ldn)
c
c     Invoking the original procedure.
c
      fcgetinpreal = getinpreal(dirname(1:ldn))
c
      return
c
      end
c     --- End of routine fcgetinpreal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fcgetinpint(idirname)
c
c     Getting the current value for an already defined integer input
c     parameter. Special version called from routine "getinprealc".
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c
c     Arguments:
c     =========
c
c     idirname......... (input, integer, array(*)) Name of the
c                       parameter, encoded as an integer array of
c                       ascii codes.
c
c     Return value: (integer) The current parameter value.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           fcgetinpint
      integer           idirname(1)
c
c     Declaration of internal variables and arrays.
c
      character*20      dirname
      integer           ldn
      integer           getinpint
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(idirname, dirname, ldn)
c
c     Invoking the original procedure.
c
      fcgetinpint = getinpint(dirname(1:ldn))
c
      return
c
      end
c     --- End of routine fcgetinpint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fcgetinpswitch(idirname)
c
c     Getting the current value for an already defined logical input
c     parameter. Special version called from routine "getipswitchc".
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c
c     Arguments:
c     =========
c
c     idirname......... (input, integer, array(*)) Name of the
c                       parameter, encoded as an integer array of
c                       ascii codes.
c
c     Return value: (logical) The current parameter value.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           fcgetinpswitch
      integer           idirname(1)
c
c     Declaration of internal variables and arrays.
c
      character*20      dirname
      integer           ldn
      logical           getinpswitch
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(idirname, dirname, ldn)
c
c     Invoking the original procedure.
c
      fcgetinpswitch = getinpswitch(dirname(1:ldn))
c
      return
c
      end
c     --- End of routine fcgetinpswitch
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcgetinpstring(idirname, istring, slen)
c
c     Getting the current value for an already defined string input
c     parameter. Special version called from routine "getinstringc".
c
c     Written by: S. J. Sciutto, Fermilab 1999; La Plata 2003
c
c
c     Arguments:
c     =========
c
c     idirname......... (input, integer, array(*)) Name of the
c                       parameter, encoded as an integer array of
c                       ascii codes.
c     istring.......... (output, integer, array(*)) Current parameter
c                       value, encoded as an integer array of ascii
c                       codes.
c     slen............. (output, integer) Length of current value
c                       string.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           idirname(1), istring(1), slen
c
c     Declaration of internal variables and arrays.
c
      character*20      dirname
      integer           ldn
      character*256     string
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(idirname, dirname, ldn)
c
c     Invoking the original procedure.
c
      call getinpstring(dirname(1:ldn), string, slen)
c
c     Converting a character string into ascii codes.
c
      call fcharc(string(1:slen), slen, istring)
c
      return
c
      end
c     --- End of routine fcgetinpstring
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcgetglobal(igvname, sdynsw, igvval, valen)
c
c     Getting the current value of a global varible. The values
c     correspond to the data currently stored in the global area.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
c     Arguments:
c     =========
c
c     igvname......... (input, integer, array(*)) Global variable
c                      name, encoded as an integer array of ASCII
c                      codes.
c     sdynsw.......... (output, integer) Type of variable: 1 dynamic,
c                      2 static, 0 if the variable is undefined.
c     igvval.......... (output, integer, array(*)) The string currently
c                      assigned to the variable, encoded as an integer
c                      array of ASCII codes.
c     valen........... (output, character*(*)) Length of gvval.
c                      valen is negative for undefined variables.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           igvname(1), sdynsw, igvval(1), valen
c
c     Declaration of internal variables and arrays.
c
      character*20      gvname
      integer           lgvn
      character*512     gvval
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(igvname, gvname, lgvn)
c
c     Invoking the original procedure.
c
      call getglobal(gvname(1:lgvn), sdynsw, gvval, valen)
c
c     Converting a character string into ascii codes.
c
      call fcharc(gvval(1:valen), valen, igvval)
c
      return
c
      end
c     --- End of routine fcgetglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fcgetintfromglobal(igvname, defval, irc)
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
c     igvname......... (input, integer, array(*)) Global variable name,
c                      encoded as an integer array of ASCII codes.
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
c     Declaration of arguments.
c
      integer           fcgetintfromglobal
      integer           igvname(1)
      integer           defval
      integer           irc
c
c     Declaration of internal variables and arrays.
c
      character*20      gvname
      integer           lgvn
      integer           getintfromglobal
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(igvname, gvname, lgvn)
c
c     Invoking the original procedure.
c
      fcgetintfromglobal = getintfromglobal(gvname(1:lgvn),
     +                                      defval, irc)
c
      return
      end
c     --- End of routine fcgetintfromglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fcgetrealfromglobal(igvname, defval, irc)
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
c     igvname......... (input, integer, array(*)) Global variable name,
c                      encoded as an integer array of ASCII codes.
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
c     Declaration of arguments.
c
      double precision  fcgetrealfromglobal
      integer           igvname(1)
      double precision  defval
      integer           irc
c
c     Declaration of internal variables and arrays.
c
      character*20      gvname
      integer           lgvn
      double precision  getrealfromglobal
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(igvname, gvname, lgvn)
c
c     Invoking the original procedure.
c
      fcgetrealfromglobal = getrealfromglobal(gvname(1:lgvn),
     +                                        defval, irc)
c
      return
      end
c     --- End of routine fcgetrealfromglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcsetglobal(igvname, ivval, irc)
c
c     Setting a dynamic global variable. This routine is primarily
c     designed to be used within a special particle module.
c     Special version for Fortran C interface.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     igvname......... (input, integer, array(*)) Global variable name,
c                      encoded as an integer array of ASCII codes.
c     ivval........... (input, integer, array(*)) String with the
c                      value to be set, encoded as an integer array
c                      of ASCII codes.
c     irc............. (output, integer) Return code. Zero means that
c                      the variable has been set successfully.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           igvname(1)
      integer           ivval(1)
      integer           irc
c
c     Declaration of internal variables and arrays.
c
      character*20      gvname
      character*256     vval
      integer           lgvn, lvval
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(igvname, gvname, lgvn)
      call cfchar(ivval, vval, lvval)
c
c     Invoking the original procedure.
c
      call setglobal(gvname(1:lgvn), vval(1:lvval), irc)
c
      return
      end
c     --- End of routine fcsetglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcsetintglobal(igvname, vval, irc)
c
c     Setting an integer value as a dynamic global variable. This
c     routine is primarily designed to be used within a special
c     particle module.
c     Special version for Fortran C interface.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     igvname......... (input, integer, array(*)) Global variable name,
c                      encoded as an integer array of ASCII codes.
c     vval............ (input, integer) Integer value to set.
c     irc............. (output, integer) Return code. Zero means that
c                      the variable has been set successfully.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           igvname(1)
      integer           vval
      integer           irc
c
c     Declaration of internal variables and arrays.
c
      character*20      gvname
      integer           lgvn
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(igvname, gvname, lgvn)
c
c     Invoking the original procedure.
c
      call setintglobal(gvname, vval, irc)
c
      return
      end
c     --- End of routine fcsetintglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcsetrealglobal(igvname, vval, irc)
c
c     Setting a real value as a dynamic global variable. This
c     routine is primarily designed to be used within a special
c     particle module.
c     Special version for Fortran C interface.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     igvname......... (input, integer, array(*)) Global variable name,
c                      encoded as an integer array of ASCII codes.
c     vval............ (input, double precision) Real value to set.
c     irc............. (output, integer) Return code. Zero means that
c                      the variable has been set successfully.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           igvname(1)
      double precision  vval
      integer           irc
c
c     Declaration of internal variables and arrays.
c
      character*20      gvname
      integer           lgvn
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character string.
c
      call cfchar(igvname, gvname, lgvn)
c
c     Invoking the original procedure.
c
      call setrealglobal(gvname, vval, irc)
c
      return
      end
c     --- End of routine fcsetrealglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initutilsfc.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
