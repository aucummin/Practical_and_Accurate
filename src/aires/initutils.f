c
c     FILE: initutils.f                     Creation date: 04/JAN/1997.
c                                       LAST MODIFICATION: 17/JUL/2003.
c
c     This file contains several auxiliary routines for input
c     processing.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function commandcode(string, slen, cvindex)
c
c     Checking a string against known command keywords and returning
c     the corresponding code. Zero means that no match was found.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     string.......... (input, character*(*)) Keyword to identify.
c     slen............ (input, integer) Keyword length.
c     cvindex......... (output, integer) Index of the command in the
c                      command name array. Set to a very large integer
c                      if the returned code is zero.
c
c     Return value: (integer) The directive code.
c     ============
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
      integer           commandcode
      character*(*)     string
      integer           slen, cvindex
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, islen, clen
      character*(mxcdl) istring
c
c     FIRST EXECUTABLE STATEMENT
c
      commandcode = 0
c
c     Only the first mxcdl characters of the input string are
c     significative.
c
      islen   = min(slen, mxcdl)
      istring = string(1:islen)
c
      do i = 1, ncommands
        j    = i
        clen = max(islen, minclen(i))
        if (istring(1:clen) .eq. cname(i)(1:clen)) goto 1010
      enddo
c
c     No match.
c
      cvindex = 1999999999
      return
c
 1010 continue
c
c     A match was found.
c
      commandcode = ccode(j)
      cvindex     = j
      return
c
      end
c     --- End of routine commandcode.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function idlcheck(idldir)
c
c     Checking a string to see if it matches any of the IDL
c     instructions currently defined, that is, the ones corresponding
c     to the last opened compressed file.
c
c     Written by: S. J. Sciutto, La Plata 1998, 1999.
c
c
c     Arguments:
c     =========
c
c     idldir.......... (input, character*(*)) The IDL directive.
c                      Abbreviations are accepted accordingly with the
c                      usual abbreviation rules.
c
c     Return value: (integer) If an error occurs, then the returned
c     ============  value will be negative. Other return values are the
c                   following:
c                    0 The string does not match any of the currently
c                      valid IDL instructions.
c                    1 The string matches a directive belonging to the
c                      "basic" instruction set with no parameter(s)
c                      associated with it, for example "Help".
c                    2 The string matches a directive belonging to the
c                      "basic" instruction set. If there is a parameter
c                      associated with the directive, then it can be
c                      obtained by means of routine "croinputdata0".
c                    4 The directive corresponds to a real input
c                      parameter. The parameter can be retrieved by
c                      means of function "getinpreal".
c                    6 The directive corresponds to an integer input
c                      parameter. The parameter can be retrieved by
c                      means of function "getinpint".
c                    8 The directive corresponds to a logical input
c                      parameter. The parameter can be retrieved by
c                      means of function "getinpswitch".
c                   10 The directive correspond to a string input
c                      parameter. The parameter can be retrieved by
c                      means of routine "getinpstring".
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           idlcheck
      character*(*)     idldir
c
c     Declaration of internal variables and arrays.
c
      integer           i, dcode
      integer           commandcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the string in the IDL directive database.
c
      dcode = commandcode(idldir, len(idldir), i)
c
c     Analysing the results.
c
      if (dcode .eq. 0) then
c
c       There is no match.
c
        idlcheck = 0
c
      else if ((dcode .lt. 0) .or. (dcode .lt. 200)) then
c
c       Basic directive with no associated input parameter.
c
        idlcheck = 1
c
      else if (dcode .lt. 800) then
c
c       Basic directive probably having one or more associated input
c       parameters.
c
        idlcheck = 2
c
      else if (dcode .lt. 1600) then
c
c       Real input parameter.
c
        idlcheck = 4
c
      else if (dcode .lt. 1800) then
c
c       Integer input parameter.
c
        idlcheck = 6
c
      else if (dcode .lt. 2000) then
c
c       Logical input parameter.
c
        idlcheck = 8
c
      else if (dcode .lt. 2200) then
c
c       Character string input parameter.
c
        idlcheck = 10
c
      else
c
c       Error!
c
        idlcheck = -99
c
      endif
c
      return
      end
c     --- End of routine idlcheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getinpreal(dirname)
c
c     Getting the current value for an already defined real input
c     parameter
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Name of the parameter.
c
c     Return value: (double precision) The current parameter value.
c     ============
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
      double precision  getinpreal
      character*(*)     dirname
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      string
      integer           slen, i, dcode
      integer           commandcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the directive code.
c
      call strimcopy(dirname, 16, string, slen)
c
      dcode = commandcode(string(1:slen), slen, i)
c
      if ( ((dcode .ge. 800) .and. (dcode .lt. 1600)) .or.
     +     (i .le. ncommands0) ) then
c
c       The specification is OK. Returning the corresponding value.
c
        getinpreal = fidata(0, aditem(i))
c
      else
c
c       Invalid specification.
c
        call errprint(0, '$A38', 4, 'getinpreal', ' ',
     +                0, 0, 0, 0.d0, string(1:slen))
      endif
c
      return
c
      end
c     --- End of routine getinpreal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getinpint(dirname)
c
c     Getting the current value for an already defined integer input
c     parameter
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Name of the parameter.
c
c     Return value: (integer) The current parameter value.
c     ============
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
      integer           getinpint
      character*(*)     dirname
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      string
      integer           slen, i, dcode
      integer           commandcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the directive code.
c
      call strimcopy(dirname, 16, string, slen)
c
      dcode = commandcode(string(1:slen), slen, i)
c
      if ( ((dcode .ge. 1600) .and. (dcode .lt. 1800)) .or.
     +     (i .le. ncommands0) ) then
c
c       The specification is OK. Returning the corresponding value.
c
        getinpint = iidata(0, aditem(i))
c
      else
c
c       Invalid specification.
c
        call errprint(0, '$A38', 4, 'getinpint', ' ',
     +                0, 0, 0, 0.d0, string(1:slen))
      endif
c
      return
c
      end
c     --- End of routine getinpint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getinpswitch(dirname)
c
c     Getting the current value for an already defined logical input
c     parameter
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Name of the parameter.
c
c     Return value: (logical) The current parameter value.
c     ============
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
      logical           getinpswitch
      character*(*)     dirname
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      string
      integer           slen, i, dcode
      integer           commandcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the directive code.
c
      call strimcopy(dirname, 16, string, slen)
c
      dcode = commandcode(string(1:slen), slen, i)
c
      if ((dcode .ge. 1800) .and. (dcode .lt. 2000)) then
c
c       The specification is OK. Returning the corresponding value.
c
        getinpswitch = lidata(0, aditem(i))
c
      else
c
c       Invalid specification.
c
        call errprint(0, '$A38', 4, 'getinpswitch', ' ',
     +                0, 0, 0, 0.d0, string(1:slen))
      endif
c
      return
c
      end
c     --- End of routine getinpswitch
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getinpstring(dirname, string, slen)
c
c     Getting the current value for an already defined character string
c     input parameter
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Name of the parameter.
c     string.......... (output, character*(*)) The current parameter
c                      value. Its length must be at least 16
c                      characters.
c     slen............ (output, integer) The used length of string.
c                      On error, slen is negative.
c
c     Return value: (logical) The current parameter value.
c     ============
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
      character*(*)     dirname, string
      integer           slen
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, dcode
      integer           commandcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the directive code.
c
      call strimcopy(dirname, 16, string, slen)
c
      dcode = commandcode(string(1:slen), slen, i)
c
      if ((dcode .ge. 2000) .and. (dcode .lt. 2200)) then
c
c       The specification is OK. Returning the corresponding value.
c
        call nthword(sidata(0, aditem(i)),
     +               sidatastring(sidata(4, aditem(i)):
     +                            sidata(5, aditem(i))),
     +               sidatastring(sidata(3, aditem(i)):
     +                            sidata(3, aditem(i))),
     +               string, slen)
c
      else
c
c       Invalid specification.
c
        call errprint(0, '$A38', 4, 'getinpstring', ' ',
     +                0, 0, 0, 0.d0, string(1:slen))
        slen = -1
      endif
c
      return
c
      end
c     --- End of routine getinpswitch
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setglobalvar(setfromline, cdline, llen, i1, i2, irc)
c
c     Assigning a value to a global variable.
c
c     Written by: S. J. Sciutto, La Plata 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     setfromline..... (input, integer) If 0, the current value
c                      of the variable is taken from the command line;
c                      if 1 it is get from the OS environment;
c                      if 2 the rest of the command line is interpreted
c                      as an OS command whose output is assigned to
c                      the variable.
c     cdline.......... (input, character*(*)) The command line to
c                      process, which includes variable name and
c                      value to set.
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) On input these
c                      indices mark the position of the name of the
c                      global variable. On output, they mark the
c                      string used to set the variable.
c     irc............. (output, integer) Return code. 0 means
c                      normal return.
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
      integer           setfromline
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i11, i21, i, j, gvi
      integer           clen, clen1, oldclen, clendif
      character*(mxcdl) tmpname
      integer           tmplen, dynst, ids, jds
      integer           wordlocate
      logical           smatch
c
c     FIRST EXECUTABLE STATEMENT
c
c     Determining dynamic or static type. Default is dynamic.
c
      if (i2 .ge. i1) then
        dynst = wordlocate(cdline(i1:i2), 'Dynamic Static', ' ', 3)
        if (dynst .gt. 0) then
          call nextword(cdline, llen, i1, i2)
        else
          dynst = 1
        endif
      else
        call errprint(0, '$G02', idlerrsev, 'setglobalvar', ' ',
     +                0, 0, 0, 0.d0, cdline(1:llen))
        irc = 13
        return
      endif
c
      if (i2 .ge. i1) then
c
c       Non empty variable name.
c
        irc = 0
        tmpname = cdline(i1:i2)
        tmplen  = min(i2 - i1 + 1, mxcdl)
        i11     = i1
        i21     = i2
c
c       Checking that the new variables does not contain bracket
c       characters
c
        call glovarbracheck(tmpname, tmplen, idlerrsev, irc)
        if (irc .ne. 0) return
c
c       Positioning in the next word (value).
c
        call nextword(cdline, llen, i1, i2)
c
c       Evaluating value to set.
c
        if (setfromline .eq. 0) then
c
c         Getting value from the command line.
c
          if (i1 .le. llen) then
c
c           Checking if there is a "Conditional" clause.
c
            if (smatch(cdline(i1:i2), 'Conditional', 3)) then
              call condglovarset(cdline, llen, i1, i2,
     +                           longauxline, clen, irc)
              if (irc .ne. 0) then
                call errprint(0, '$G04', idlerrsev, 'setglobalvar',
     +                        ' ', 0, 0, 0, 0.d0, cdline(1:llen))
                return
              endif
            else
c
c             Normal (unconditional assignment).
c
              clen = llen - i1 + 1
              longauxline = cdline(i1:llen)
            endif
c
          else
            clen = 0
          endif
c
        else if (setfromline .eq. 1) then
c
c         Getting value from the OS environment.
c
          call importvar(cdline(i11:i21), longauxline, clen)
c
        else
c
c         Getting value from the output of a shell command.
c
          clen = 0
          if (i1 .le. llen) then
            call sysspawn(cdline(i1:llen), ' ', auxfn, irc)
            open(57, file = auxfn, status = 'OLD', err = 1050)
 1020       continue
            if (clen .le. 0) then
              clen1 = clen + 1
            else
              clen1 = clen + 2
            endif
            read(57, 2010, end = 1050, err = 1050)
     +                      longauxline(clen1:512)
 2010       format(a)
            do i = 512, clen, -1
              j = i
              if (longauxline(i:i) .ne. ' ') goto 1030
            enddo
 1030       continue
            clen = j
            if (clen .lt. 511) goto 1020
 1050       continue
            close(57, status = 'DELETE')
          endif
c
        endif
c
c       Checking if the variable exists.
c
        do ids = 1, 2
          jds = ids
          do i = 1, nglobar(ids)
            gvi = i
            if (tmpname .eq.
     +          globnam(i, ids)(1:globlen(i, ids))) goto 1010
          enddo
        enddo
c
c       Variable not found. Creating new variable.
c
        if (nglobar(dynst) .ge. mxglobar) then
          call errprint(0, '$G01', 4, 'setglobalvar', ' ',
     +                  0, 0, 0, 0.d0, cdline(1:llen))
        endif
c
        gvi             = nglobar(dynst) + 1
        nglobar(dynst)  = gvi
        thereareglobars = .true.
c            
        globnam(gvi, dynst)   = tmpname
        globlen(gvi, dynst)   = tmplen
        globdfend(gvi, dynst) = globdfend(gvi - 1, dynst)
c
        if (clen .gt. 0) then
          globdfend(gvi, dynst) = globdfend(gvi, dynst) + clen
          if (globdfend(gvi, dynst) .gt. mxgll) then
            call errprint(0, '$G01', 4, 'setglobalvar', ' ',
     +                    0, 0, 0, 0.d0, cdline(1:llen))
          endif
          globarstring(dynst)
     +       (globstrlen(dynst)+1:globdfend(gvi, dynst))
     +       = longauxline(1:clen)
        endif
c
        globstrlen(dynst) = globdfend(gvi, dynst)
        return
c
 1010   continue
c
c       The variable is already definded.
c
c       Checking type compatibility.
c
        if (dynst .ne. jds) then
          call errprint(0, '$G03', idlerrsev, 'setglobalvar', ' ',
     +                  0, 0, 0, 0.d0, cdline(1:llen))
          irc = 26
          return
        endif
c
c       Superseding its old value.
c
        oldclen = globdfend(gvi, jds) - globdfend(gvi - 1, jds)
        clendif = clen - oldclen
c
        if (clendif .lt. 0) then
          j = globdfend(gvi - 1, jds) + clen
          do i = globdfend(gvi, jds) + 1,
     +           globdfend(nglobar(jds), jds)
            j = j + 1
            globarstring(jds)(j:j) = globarstring(jds)(i:i)
          enddo
          globstrlen(jds) = j
        else if (clendif .gt. 0) then
          globstrlen(jds) = globstrlen(jds) + clendif
          if (globstrlen(jds) .gt. mxgll) then
            call errprint(0, '$G01', 4, 'setglobalvar', ' ',
     +                    0, 0, 0, 0.d0, cdline(1:llen))
          endif
          j          = globstrlen(jds)
          do i = globdfend(nglobar(jds), jds),
     +           globdfend(gvi, jds) + 1, -1
            globarstring(jds)(j:j) = globarstring(jds)(i:i)
            j = j - 1
          enddo
        endif
c
        do i = gvi, nglobar(jds)
          globdfend(i, jds) = globdfend(i, jds) + clendif
        enddo
c
c       Assigning the new value.
c
        if (clen .gt. 0) then
          globarstring(jds)
     +            (globdfend(gvi - 1, jds) + 1:globdfend(gvi, jds))
     +            = longauxline(1:clen)
        endif
c
      else
        call errprint(0, '$G02', idlerrsev, 'setglobalvar', ' ',
     +                0, 0, 0, 0.d0, cdline(1:llen))
        irc = 13
c
      endif
c
      return
      end
c     --- End of routine setglobalvar
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine delglobalvar(cdline, llen, i1, i2, irc)
c
c     Removing an already defined global variable.
c
c     Written by: S. J. Sciutto, La Plata 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line to
c                      process, which includes variable name.
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input, integer) Indices marking the position of
c                      the name of the global variable.
c     irc............. (output, integer) Return code. 0 means
c                      normal return.
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
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, gvi, oldclen
      character*(mxcdl) tmpname
      integer           tmplen, ids, jds
c
c     FIRST EXECUTABLE STATEMENT
c
      if (i2 .ge. i1) then
c
c       Non empty variable name.
c
        irc = 0
        tmpname = cdline(i1:i2)
        tmplen  = min(i2 - i1 + 1, mxcdl)
c
c       Checking if the variable exists.
c
        do ids = 1, 2
          jds = ids
          do i = 1, nglobar(ids)
            gvi = i
            if (tmpname .eq.
     +          globnam(i, ids)(1:globlen(i, ids))) goto 1010
          enddo
        enddo
c
c       Variable not found.
c
        call errprint(0, '*', 2, 'delglobalvar',
     +       'Global variable is undefined.',
     +        0, 0, 0, 0.d0, cdline(i1:i2))
        irc = 2
        return
c
 1010   continue
c
c       The variable is already definded. Removing.
c
        oldclen = globdfend(gvi, jds) - globdfend(gvi - 1, jds)
c
        j = globdfend(gvi - 1, jds)
        do i = globdfend(gvi, jds) + 1, globdfend(nglobar(jds), jds)
          j = j + 1
          globarstring(jds)(j:j) = globarstring(jds)(i:i)
        enddo
        globstrlen(jds) = j
c
        do i = gvi + 1, nglobar(jds)
          globdfend(i - 1, jds) = globdfend(i, jds) - oldclen
          globnam(i - 1, jds)   = globnam(i, jds)
          globlen(i - 1, jds)   = globlen(i, jds)
        enddo
c
        nglobar(jds)    = nglobar(jds) - 1
        thereareglobars = ((nglobar(1) + nglobar(2)) .gt. 0)
c
      else
        call errprint(0, '$G02', idlerrsev, 'delglobalvar', ' ',
     +                0, 0, 0, 0.d0, cdline(1:llen))
        irc = 13
c
      endif
c
      return
      end
c     --- End of routine delglobalvar
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine glovarbracheck(gvar, gvlen, errsev, irc)
c
c     Checking that global variable names do not contain the current
c     bracket characters.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c
c     Arguments:
c     =========
c
c     gvar............ (input, character*(*)) The global variable
c                      to check.
c     gvlen........... (input, integer) Length of gvar. If gvlen is
c                      set to zero or negative, then all the
c                      currently defined variables are checked.
c                      process, which includes variable name.
c     errsev.......... (input, integer) Error message severity.
c     irc............. (output, integer) Error code. 0 means normal
c                      return.
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
      character*(*)     gvar
      integer           gvlen, errsev, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, ids
      character*(mxcdl) tmpname
c
c     FIRST EXECUTABLE STATEMENT
c
      if (gvlen .gt. 0) then
c
c       Non empty variable name. Checking single variable.
c
        tmpname = gvar
        do i = 1, gvlen
          if ( (gvar(i:i) .eq. opbr) .or.
     +         (gvar(i:i) .eq. clbr) .or.
     +         (gvar(i:i) .eq. brec)      ) goto 3010
        enddo
c
      else
c
c       Checking all the currently defined variables.
c
        do ids = 1, 2
          do i = 1, nglobar(ids)
            tmpname = globnam(i, ids)
            do j = 1, globlen(i, ids)
              if ( (globnam(i, ids)(j:j) .eq. opbr) .or.
     +             (globnam(i, ids)(j:j) .eq. clbr) .or.
     +             (globnam(i, ids)(j:j) .eq. brec)      ) goto 3010
            enddo
          enddo
        enddo
c
      endif
c
      irc = 0
      return
c
 3010 continue
c
c     Error.
c
      if (errsev .gt. 0) then
        call errprint(0, '*', errsev, 'glovarbracheck',
     +       'Global variable name contains bracket character(s).',
     +       0, 0, 0, 0.d0, tmpname)
      endif
      irc = 8
      return
c
      end
c     --- End of routine glovarbracheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine condglovarset(cdline, llen, i1, i2,
     +                         rstring, rslen, irc)
c
c     Interpreting a conditional clause and assigning "true" or
c     "false" strings accordingly with the result of the logical
c     opeartion.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line to
c                      process, which includes conditional clause and
c                      value(s) to set.
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) On input these
c                      indices mark the position of the first word
c                      of the conditional clause. On output, they mark
c                      the last word scanned.
c     rstring......... (output, character*(*)) The output string.
c     rslen........... (output, integer) Length of "rstring".
c     irc............. (output, integer) Return code. 0 means
c                      normal return.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     cdline, rstring
      integer           llen, i1, i2, rslen, irc
c
c     Declaration of internal variables and arrays.
c
      integer           ic11, ic12, ic21, ic22, il1, il2
      logical           lcl
      double precision  fc1, fc2
c
c     FIRST EXECUTABLE STATEMENT
c
      irc = 11
c
c     Analyzing the clause.
c
      call nextword(cdline, llen, i1, i2)
      if (i1 .gt. i2) return
      ic11 = i1
      ic12 = i2
      call nextword(cdline, llen, i1, i2)
      if (i1 .gt. i2) return
      il1 = i1
      il2 = i2
      call nextword(cdline, llen, i1, i2)
      if (i1 .gt. i2) return
      ic21 = i1
      ic22 = i2
c
      if      (cdline(il1:il2) .eq. 'EQS') then
        lcl = (cdline(ic11:ic12) .eq. cdline(ic21:ic22))
      else if (cdline(il1:il2) .eq. 'NES') then
        lcl = (cdline(ic11:ic12) .ne. cdline(ic21:ic22))
      else
        read(cdline(ic11:ic12), *, err = 3010) fc1
        read(cdline(ic21:ic22), *, err = 3010) fc2
        if      (cdline(il1:il2) .eq. 'EQ') then
          lcl = (fc1 .eq. fc2)
        else if (cdline(il1:il2) .eq. 'NE') then
          lcl = (fc1 .ne. fc2)
        else if (cdline(il1:il2) .eq. 'GT') then
          lcl = (fc1 .gt. fc2)
        else if (cdline(il1:il2) .eq. 'GE') then
          lcl = (fc1 .ge. fc2)
        else if (cdline(il1:il2) .eq. 'LT') then
          lcl = (fc1 .lt. fc2)
        else if (cdline(il1:il2) .eq. 'LE') then
          lcl = (fc1 .le. fc2)
        else
          irc = 9
          return
        endif
      endif
c
c     Returning the result.
c
      il2 = i2
      call nextword(cdline, llen, i1, i2)
c
      if (i1 .gt. llen) then
        if (lcl) then
          rstring = 'True'
          rslen   = 4
        else
          rstring = 'False'
          rslen   = 5
        endif
      else
        ic11 = i1
        ic12 = llen
        ic21 = llen
        ic22 = -1
 1010   continue
        if (i1 .gt. i2) goto 1020
        if (cdline(i1:i2) .eq. 'Else') then
          ic12 = il2
          call nextword(cdline, llen, i1, i2)
          ic21 = i1
          ic22 = llen
        else
          il2 = i2
          call nextword(cdline, llen, i1, i2)
          goto 1010
        endif
 1020   continue
        rstring = ' '
        rslen  = 0
        if (lcl) then
          if (ic11 .le. ic12) then
            rstring = cdline(ic11:ic12)
            rslen  = ic12 - ic11 + 1
          endif
        else
          if (ic21 .le. ic22) then
            rstring = cdline(ic21:ic22)
            rslen  = ic22 - ic21 + 1
          endif
        endif
      endif
c
      irc = 0
      return
c
c     Error.
c
 3010 continue
      irc = 15
      return
c
      end
c     --- End of routine condglovarset
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine getglobal(gvname, sdynsw, gvval, valen)
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
c     gvname.......... (input, character*(*)) Global variable name.
c     sdynsw.......... (output, integer) Type of variable: 1 dynamic,
c                      2 static, 0 if the variable is undefined.
c     gvval........... (output, character*(*)) The string currently
c                      assigned to the variable. The calling program
c                      must ensure enough space to store the string.
c     valen........... (output, integer) Length of gvval. valen is
c                      negative for undefined variables.
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
      character*(*)     gvname, gvval
      integer           sdynsw, valen
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*(mxcdl) tmpname
      integer           i, ids, jds, gvi, i1, i2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the name of the variable.
c
      i2 = len(gvname)
      i  = 0
      call nextword(gvname, i2, i1, i)
c
      if (i1 .le. i2) then
c
        tmpname = gvname(i1:i2)
c
        do ids = 1, 2
          jds = ids
          do i = 1, nglobar(ids)
            gvi = i
            if (tmpname .eq. globnam(i, ids)(1:globlen(i, ids)))
     +         goto 1010
          enddo
        enddo
c
      endif
c
c     The variable does not exist.
c
      sdynsw = 0
      valen  = -1
      return
c
 1010 continue
c
c     Returning variable attributes.
c
      sdynsw = jds
c
      i1 = globdfend(gvi - 1, jds)
      i2 = globdfend(gvi, jds)
c
      if (i2 .gt. i1) then
        gvval = globarstring(jds)(i1+1:i2)
        valen = i2 - i1
      else
        gvval = ' '
        valen = 0
      endif
c
      return
      end
c     --- End of routine getglobal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
