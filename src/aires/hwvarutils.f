c
c     FILE: hwvarutils.f                    Creation date: 19/MAR/1997.
c                                       LAST MODIFICATION: 21/JUL/2003.
c
c     This file contains several routines to define and manage internal
c     input or output variables.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newinpreal(vartype, dirname, minamelen, longname, vid,
     +                      default, bdrykey, bdry1, bdry2)
c
c     Defining a new directive, associated to a real data item.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c
c     Arguments:
c     =========
c
c     vartype......... (input, integer) Type of variable: 1 Real number
c                      (no units). 2 Length. 3 Time. 4 Energy.
c                      Otherwise: Real number.
c     dirname......... (input, character*(*)) Name of the directive.
c                      Maximum length is 16 characters. No embedded
c                      blanks allowed.
c     minamelen....... (input, integer) Length of the minimum
c                      abbreviation for dirname.
c     longname........ (input, character*(*)) A "long" name for the
c                      directive (up to 27 characters), which will
c                      be used for printing (may contain blanks).
c     vid............. (input, integer) "Very important directive"
c                      switch. If vid is less than 2, then
c                      the directive is assumed not to be frequently
c                      set (it is better a parameter of the program),
c                      and so it will not be listed unless explicitly
c                      specified with the "InputListing Full"
c                      directive. Additionally, if vid is equal to 1,
c                      a warning message will be printed every time the
c                      corresponding variable is altered. If vid is
c                      greater or equal than 2, then the current value
c                      of the variable will always be listed.
c     default......... (input, real*8) Default value for the variable.
c     bdrykey......... (input, integer) Boundary key. 0 means that
c                      no boundary check is needed for the variable,
c                      any real number is a valid input. 1 means that
c                      the variable must be greater or equal than
c                      argument bdry1. 2 means that the variable must
c                      be less or equal than argument bdry2. 3 means
c                      that both 1 and 2 hold. If the boundary check
c                      is not passed then an error message is typed
c                      and the variable is assigned the default value.
c                      If bdrykey is negative or greater than 3, it is
c                      taken as zero.
c     bdry1, bdry2.... (input, real*8) Parameters used in connection
c                      with "bdrykey".
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
      integer           vartype
      character*(*)     dirname, longname
      integer           minamelen, vid, bdrykey
      real*8            default, bdry1, bdry2
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      string
      integer           slen, i, clen
      logical           ivstring
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the name of the variable.
c
      call strimcopy(dirname, 16, string, slen)
      ivstring = (slen .le. 0)
      do i = 1, slen
        if (string(i:i) .eq. ' ') ivstring = .true.
      enddo
      if (ivstring) call errprint(0, '$A35', 4, 'newinpreal', ' ',
     +                            0, 0, 0, 0.d0, string)
c
      if ((minamelen .le. 1) .or. (minamelen .gt. slen))
     +  call errprint(0, '$A36', 4, 'newinpreal', ' ',
     +  1, minamelen, 0, 0.d0, string(1:slen))
c
      do i = 1, ncommands
        clen = max(slen, minclen(i))
        if (string(1:clen) .eq. cname(i)(1:clen))
     +     call errprint(0, '$A35', 4, 'newinpreal',
     +                   'Identifier matches existing directive.',
     +                   0, 0, 0, 0.d0, string(1:clen))
      enddo
c
c     No match with previous directives.
c
c     Checking array space.
c
      if ((ncommands .ge. maxccodes) .or.
     +    (nfidata .ge. mxfidata)) call errprint(0, '$A37', 4,
     +                                  'newinpreal', ' ',
     +                                  0, 0, 0, 0.d0, string)
c
c     Input parameters checked. Assigning the new directive.
c
      ncommands = ncommands + 1
      nfidata   = nfidata + 1
c
      i = vartype
      if ((i .lt. 0) .or. (i .gt. 4)) i = 0
      lastdcode(i) = lastdcode(i) + 1
c
      cname(ncommands)      = string
      minclen(ncommands)    = minamelen
      clgname(ncommands)    = longname
      ccode(ncommands)      = lastdcode(i)
      aditem(ncommands)     = nfidata
      veryimpdir(ncommands) = (vid .gt. 1)
      wngonset(ncommands)   = (vid .eq. 1)
c
      fidata(0, nfidata)    = default
      fidata(2, nfidata)    = bdry1
      fidata(3, nfidata)    = bdry2
      fdbdry(nfidata)       = bdrykey
      if ((fdbdry(nfidata) .lt. 0) .or.
     +    (fdbdry(nfidata) .gt. 3)) fdbdry(nfidata) = 0
c
      return
c
      end
c     --- End of routine newinpreal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newinpint(dirname, minamelen, longname, vid,
     +                      default, bdrykey, bdry1, bdry2)
c
c     Defining a new directive, associated to an integer data item.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Name of the directive.
c                      Maximum length is 16 characters. No embedded
c                      blanks allowed.
c     minamelen....... (input, integer) Length of the minimum
c                      abbreviation for dirname.
c     longname........ (input, character*(*)) A "long" name for the
c                      directive (up to 27 characters), which will
c                      be used for printing (may contain blanks).
c     vid............. (input, integer) "Very important directive"
c                      switch. If vid is less than 2, then
c                      the directive is assumed not to be frequently
c                      set (it is better a parameter of the program),
c                      and so it will not be listed unless explicitly
c                      specified with the "InputListing Full"
c                      directive. Additionally, if vid is equal to 1,
c                      a warning message will be printed every time the
c                      corresponding variable is altered. If vid is
c                      greater or equal than 2, then the current value
c                      of the variable will always be listed.
c     default......... (input, integer) Default value for the variable.
c     bdrykey......... (input, integer) Boundary key. 0 means that
c                      no boundary check is needed for the variable,
c                      any integer number is a valid input. 1 means
c                      that the variable must be greater or equal than
c                      argument bdry1. 2 means that the variable must
c                      be less or equal than argument bdry2. 3 means
c                      that both 1 and 2 hold. If the boundary check
c                      is not passed then an error message is typed
c                      and the variable is assigned the default value.
c                      If bdrykey is negative or greater than 3, it is
c                      taken as zero.
c     bdry1, bdry2.... (input, integer) Parameters used in connection
c                      with "bdrykey".
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
      character*(*)     dirname, longname
      integer           minamelen, vid, bdrykey
      integer           default, bdry1, bdry2
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      string
      integer           slen, i, clen
      logical           ivstring
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the name of the variable.
c
      call strimcopy(dirname, 16, string, slen)
      ivstring = (slen .le. 0)
      do i = 1, slen
        if (string(i:i) .eq. ' ') ivstring = .true.
      enddo
      if (ivstring) call errprint(0, '$A35', 4, 'newinpint', ' ',
     +                            0, 0, 0, 0.d0, string)
c
      if ((minamelen .le. 1) .or. (minamelen .gt. slen))
     +  call errprint(0, '$A36', 4, 'newinpint', ' ',
     +  1, minamelen, 0, 0.d0, string(1:slen))
c
      do i = 1, ncommands
        clen = max(slen, minclen(i))
        if (string(1:clen) .eq. cname(i)(1:clen))
     +     call errprint(0, '$A35', 4, 'newinpint',
     +                   'Identifier matches existing directive.',
     +                   0, 0, 0, 0.d0, string(1:clen))
      enddo
c
c     No match with previous directives.
c
c     Checking array space.
c
      if ((ncommands .ge. maxccodes) .or.
     +    (niidata .ge. mxiidata)) call errprint(0, '$A37', 4,
     +                                  'newinpint', ' ',
     +                                  0, 0, 0, 0.d0, string)
c
c     Input parameters checked. Assigning the new directive.
c
      ncommands = ncommands + 1
      niidata   = niidata + 1
c
      lastdcode(5) = lastdcode(5) + 1
c
      cname(ncommands)      = string
      minclen(ncommands)    = minamelen
      clgname(ncommands)    = longname
      ccode(ncommands)      = lastdcode(5)
      aditem(ncommands)     = niidata
      veryimpdir(ncommands) = (vid .gt. 1)
      wngonset(ncommands)   = (vid .eq. 1)
c
      iidata(0, niidata)    = default
      iidata(2, niidata)    = bdry1
      iidata(3, niidata)    = bdry2
      idbdry(niidata)       = bdrykey
      if ((idbdry(niidata) .lt. 0) .or.
     +    (idbdry(niidata) .gt. 3)) idbdry(niidata) = 0
c
      return
c
      end
c     --- End of routine newinpint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newinpswitch(dirname, minamelen, longname, vid,
     +                        default0, default1)
c
c     Defining a new directive, associated to a logical switch.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Name of the directive.
c                      Maximum length is 16 characters. No embedded
c                      blanks allowed.
c     minamelen....... (input, integer) Length of the minimum
c                      abbreviation for dirname.
c     longname........ (input, character*(*)) A "long" name for the
c                      directive (up to 27 characters), which will
c                      be used for printing (may contain blanks).
c     vid............. (input, integer) "Very important directive"
c                      switch. If vid is less than 2, then
c                      the directive is assumed not to be frequently
c                      set (it is better a parameter of the program),
c                      and so it will not be listed unless explicitly
c                      specified with the "InputListing Full"
c                      directive. Additionally, if vid is equal to 1,
c                      a warning message will be printed every time the
c                      corresponding variable is altered. If vid is
c                      greater or equal than 2, then the current value
c                      of the variable will always be listed.
c     default0........ (input, logical) Default value for the variable,
c                      to be supplied in the case of missing
c                      specification.
c     default1........ (input, logical) Default value for the variable,
c                      to be used when the directive is used without
c                      "On" or "Off" specification.
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
      character*(*)     dirname, longname
      integer           minamelen, vid
      logical           default0, default1
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      string
      integer           slen, i, clen
      logical           ivstring
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the name of the variable.
c
      call strimcopy(dirname, 16, string, slen)
      ivstring = (slen .le. 0)
      do i = 1, slen
        if (string(i:i) .eq. ' ') ivstring = .true.
      enddo
      if (ivstring) call errprint(0, '$A35', 4, 'newinpswitch', ' ',
     +                            0, 0, 0, 0.d0, string)
c
      if ((minamelen .le. 1) .or. (minamelen .gt. slen))
     +  call errprint(0, '$A36', 4, 'newinpswitch', ' ',
     +  1, minamelen, 0, 0.d0, string(1:slen))
c
      do i = 1, ncommands
        clen = max(slen, minclen(i))
        if (string(1:clen) .eq. cname(i)(1:clen))
     +     call errprint(0, '$A35', 4, 'newinpswitch',
     +                   'Identifier matches existing directive.',
     +                   0, 0, 0, 0.d0, string(1:clen))
      enddo
c
c     No match with previous directives.
c
c     Checking array space.
c
      if ((ncommands .ge. maxccodes) .or.
     +    (nlidata .ge. mxlidata)) call errprint(0, '$A37', 4,
     +                                  'newinpswitch', ' ',
     +                                  0, 0, 0, 0.d0, string)
c
c     Input parameters checked. Assigning the new directive.
c
      ncommands = ncommands + 1
      nlidata   = nlidata + 1
c
      lastdcode(6) = lastdcode(6) + 1
c
      cname(ncommands)      = string
      minclen(ncommands)    = minamelen
      clgname(ncommands)    = longname
      ccode(ncommands)      = lastdcode(6)
      aditem(ncommands)     = nlidata
      veryimpdir(ncommands) = (vid .gt. 1)
      wngonset(ncommands)   = (vid .eq. 1)
c
      lidata(0, nlidata)    = default0
      lidata(2, nlidata)    = default1
c
      return
c
      end
c     --- End of routine newinpswitch
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newinpstring(dirname, minamelen, longname, vid,
     +                        options, separator,
     +                        minchar, default0, default1)
c
c     Defining a new directive, associated to an integer variable coded
c     as character strings.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c
c     Arguments:
c     =========
c
c     dirname......... (input, character*(*)) Name of the directive.
c                      Maximum length is 16 characters. No embedded
c                      blanks allowed.
c     minamelen....... (input, integer) Length of the minimum
c                      abbreviation for dirname.
c     longname........ (input, character*(*)) A "long" name for the
c                      directive (up to 27 characters), which will
c                      be used for printing (may contain blanks).
c     vid............. (input, integer) "Very important directive"
c                      switch. If vid is less than 2, then
c                      the directive is assumed not to be frequently
c                      set (it is better a parameter of the program),
c                      and so it will not be listed unless explicitly
c                      specified with the "InputListing Full"
c                      directive. Additionally, if vid is equal to 1,
c                      a warning message will be printed every time the
c                      corresponding variable is altered. If vid is
c                      greater or equal than 2, then the current value
c                      of the variable will always be listed.
c     options......... (input, character*(*)) List of valid options for
c                      the variable, a character string containing
c                      several words separated by a separator character.
c                      The first word correspond to the integer 1,
c                      the second to 2, etc.
c     separator....... (input, character*(*)) The character used to
c                      separate characters in the options string.
c     minchar......... (input, integer) Minimum number of characters
c                      that must be supplied when the directive
c                      contains a nonull string specification.
c     default0........ (input, character*(*)) Default value for the
c                      variable, to be supplied in the case of missing
c                      specification.
c     default1........ (input, logical) Default value for the variable,
c                      to be used when the directive is used without
c                      any string specification.
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
      character*(*)     dirname, longname, options, separator
      integer           minamelen, vid, minchar
      character*(*)     default0, default1
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*16      string
      character*1       sep
      integer           slen, i, clen, mnlen
      integer           olen, olast, nwords, ilast
      logical           ivstring
      integer           wordlocate
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the name of the variable.
c
      call strimcopy(dirname, 16, string, slen)
      ivstring = (slen .le. 0)
      do i = 1, slen
        if (string(i:i) .eq. ' ') ivstring = .true.
      enddo
      if (ivstring) call errprint(0, '$A35', 4, 'newinpstring', ' ',
     +                            0, 0, 0, 0.d0, string)
c
      if ((minamelen .le. 1) .or. (minamelen .gt. slen))
     +  call errprint(0, '$A36', 4, 'newinpstring', ' ',
     +  1, minamelen, 0, 0.d0, string(1:slen))
c
      do i = 1, ncommands
        clen = max(slen, minclen(i))
        if (string(1:clen) .eq. cname(i)(1:clen))
     +     call errprint(0, '$A35', 4, 'newinpstring',
     +                   'Identifier matches existing directive.',
     +                   0, 0, 0, 0.d0, string(1:clen))
      enddo
c
c     No match with previous directives.
c
c     Checking array and string space.
c
      if ((ncommands .ge. maxccodes) .or.
     +    (nsidata .ge. mxsidata)) call errprint(0, '$A37', 4,
     +                                  'newinpstring', ' ',
     +                                  0, 0, 0, 0.d0, string)
      olen  = len(options)
      olast = sidatalen + olen
c
      if (olast .ge. mxsil) call errprint(0, '$A37', 4,
     +                      'newinpstring',
     +                      'Insufficient string length',
     +                      0, 0, 0, 0.d0, string)

c
c     Checking the options.
c
      sep    = separator
      nwords = 0
      mnlen  = max(1, minchar)
      ilast  = 1
      do i = 1, olen
        if (options(i:i) .eq. sep) then
          nwords = nwords + 1
          if ((i - ilast) .lt. mnlen) goto 3010
          if (wordlocate(options(ilast:i-1), options, sep, mnlen)
     +        .ne. nwords) goto 3010
          ilast = i + 1
        endif
      enddo
      if (options(olen:olen) .ne. sep) then
        nwords = nwords + 1
        if ((olen - ilast + 1) .lt. mnlen) goto 3010
        if (wordlocate(options(ilast:olen), options, sep, mnlen)
     +      .ne. nwords) goto 3010
      endif
      if (nwords .le. 0) goto 3010
c
c     Input parameters checked. Assigning the new directive.
c
      ncommands = ncommands + 1
      nsidata   = nsidata + 1
c
      lastdcode(7) = lastdcode(7) + 1
c
      cname(ncommands)      = string
      minclen(ncommands)    = minamelen
      clgname(ncommands)    = longname
      ccode(ncommands)      = lastdcode(7)
      aditem(ncommands)     = nsidata
      veryimpdir(ncommands) = (vid .gt. 1)
      wngonset(ncommands)   = (vid .eq. 1)
c
      sidata(0, nsidata)    = wordlocate(default0, options, sep, mnlen)
      sidata(2, nsidata)    = wordlocate(default1, options, sep, mnlen)
      sidata(6, nsidata)    = mnlen
c
      sidatalen                         = sidatalen + 1
      sidata(3, nsidata)                = sidatalen
      sidatastring(sidatalen:sidatalen) = sep
      sidatalen                         = sidatalen + 1
      olast                             = olast + 1
      sidata(4, nsidata)                = sidatalen
      sidata(5, nsidata)                = olast
      sidatastring(sidatalen:olast)     = options
      sidatalen                         = olast
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '*', 4, 'newinpstring',
     +              'Invalid options string',
     +              0, 0, 0, 0.d0, dirname)
c
      end
c     --- End of routine newinpstring
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newoutreal(vartype, longname, viv, varid)
c
c     Defining a new output variable, associated to a real data item.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     vartype......... (input, integer) Type of variable: 1 Real number
c                      (no units). 2 Length. 3 Time. 4 Energy.
c                      Otherwise: Real number. In this version, only
c                      varype = 1 is supported.
c     longname........ (input, character*(*)) A name for the variable
c                      (up to 27 characters), which will be used for
c                      printing (may contain blanks).
c     viv............. (input, integer) "Very important variable"
c                      switch. If viv is less than 2, then
c                      the variable is assumed not to be frequently
c                      used, so it will not be printed in the summary
c                      unless explicitly specified with the
c                      "OutputListing Full" directive. If vid is
c                      greater or equal than 2, then the data
c                      associated with the variable will always be
c                      listed.
c     varid........... (output, integer) Integer label which identifies
c                      the defined variable. It should not be modified
c                      by the calling program.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           vartype, viv
      character*(*)     longname
      integer           varid
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking array space.
c
      if ((nallodata .ge. mxallodata) .or.
     +    (nfodata .ge. mxfodata)) call errprint(0, '$A37', 4,
     +                                  'newoutreal', ' ',
     +                                  0, 0, 0, 0.d0, longname)
c
c     Input parameters checked. Assigning the new variable.
c
      nallodata = nallodata + 1
      nfodata   = nfodata + 1
c
c      i = vartype    REPLACED BY i = 1 by now!!!!!
      i = 1
c      if ((i .lt. 0) .or. (i .gt. 4)) i = 0
c
      odataname(nallodata)    = longname
      odatatype(nallodata)    = i
      odataitem(nallodata)    = nfodata
      veryimpodata(nallodata) = (viv .gt. 1)
c
c     Zeroing the variable.
c
      do i = 1, 5
        fodata(i, nfodata) = 0
      enddo
      fodata(4, nfodata) = 5.d35
      fosamples(nfodata) = 0
c
c     Returing the variable identification.
c
      varid = nallodata
c
      return
c
      end
c     --- End of routine newoutreal
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newoutint(longname, viv, varid)
c
c     Defining a new output variable, associated to an integer data
c     item.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     longname........ (input, character*(*)) A name for the variable
c                      (up to 27 characters), which will be used for
c                      printing (may contain blanks).
c     viv............. (input, integer) "Very important variable"
c                      switch. If viv is less than 2, then
c                      the variable is assumed not to be frequently
c                      used, so it will not be printed in the summary
c                      unless explicitly specified with the
c                      "OutputListing Full" directive. If vid is
c                      greater or equal than 2, then the data
c                      associated with the variable will always be
c                      listed.
c     varid........... (output, integer) Integer label which identifies
c                      the defined variable. It should not be modified
c                      by the calling program.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           viv
      character*(*)     longname
      integer           varid
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking array space.
c
      if ((nallodata .ge. mxallodata) .or.
     +    (niodata .ge. mxiodata)) call errprint(0, '$A37', 4,
     +                                  'newoutint', ' ',
     +                                  0, 0, 0, 0.d0, longname)
c
c     Input parameters checked. Assigning the new variable.
c
      nallodata = nallodata + 1
      niodata   = niodata + 1
c
      odataname(nallodata)    = longname
      odatatype(nallodata)    = 5
      odataitem(nallodata)    = niodata
      veryimpodata(nallodata) = (viv .gt. 1)
c
c     Zeroing the variable.
c
      do i = 1, 3
        iodata(i, niodata) = 0
      enddo
      iodata(2, niodata) = 2147483647
      iosamples(niodata) = 0
c
c     Returing the variable identification.
c
      varid = nallodata
c
      return
c
      end
c     --- End of routine newoutint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine newoutswitch(longname, viv, varid)
c
c     Defining a new output variable, associated to a logical data
c     item.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     longname........ (input, character*(*)) A name for the variable
c                      (up to 27 characters), which will be used for
c                      printing (may contain blanks).
c     viv............. (input, integer) "Very important variable"
c                      switch. If viv is less than 2, then
c                      the variable is assumed not to be frequently
c                      used, so it will not be printed in the summary
c                      unless explicitly specified with the
c                      "OutputListing Full" directive. If vid is
c                      greater or equal than 2, then the data
c                      associated with the variable will always be
c                      listed.
c     varid........... (output, integer) Integer label which identifies
c                      the defined variable. It should not be modified
c                      by the calling program.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           viv
      character*(*)     longname
      integer           varid
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking array space.
c
      if ((nallodata .ge. mxallodata) .or.
     +    (nlodata .ge. mxlodata)) call errprint(0, '$A37', 4,
     +                                  'newoutswitch', ' ',
     +                                  0, 0, 0, 0.d0, longname)
c
c     Input parameters checked. Assigning the new variable.
c
      nallodata = nallodata + 1
      nlodata   = nlodata + 1
c
      odataname(nallodata)    = longname
      odatatype(nallodata)    = 6
      odataitem(nallodata)    = nlodata
      veryimpodata(nallodata) = (viv .gt. 1)
c
c     "Zeroing" the variable.
c
      lodata(nlodata) = .false.
c
c     Returing the variable identification.
c
      varid = nallodata
c
      return
c
      end
c     --- End of routine newoutswitch
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine outrealupdate(varid, instance)
c
c     Updating an already defined output real data item.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     varid........... (input, integer) Integer label which identifies
c                      the defined variable, as returned by the
c                      initializing routine.
c     instance........ (input-output, real*8) The real value to use for
c                      the variable statistical update. It is set to
c                      zero after being used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           varid
      real*8            instance
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking variable identifier.
c
      if ((varid .lt. nallodata0) .or. (varid .gt. nallodata)) then
        call errprint(0, '$B38', 4, 'outrealupdate', ' ',
     +                1, varid, 0, 0.d0, ' ')
      endif
c
c     Statistical update.
c
      i = odataitem(varid)
c
      fodata(2, i) = instance
      call statupdate(1, 5, fodata(1, i))
      fosamples(i) = fosamples(i) + 1
c
      instance = 0
c
      return
c
      end
c     --- End of routine outrealupdate
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine outintupdate(varid, instance)
c
c     Updating an already defined output integer data item.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     varid........... (input, integer) Integer label which identifies
c                      the defined variable, as returned by the
c                      initializing routine.
c     instance........ (input-output, integer) The integer value to use
c                      for the variable statistical update. It is set
c                      to zero after being used.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           varid
      integer           instance
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking variable identifier.
c
      if ((varid .lt. nallodata0) .or. (varid .gt. nallodata)) then
        call errprint(0, '$B38', 4, 'outintupdate', ' ',
     +                1, varid, 0, 0.d0, ' ')
      endif
c
c     Statistical update.
c
      i = odataitem(varid)
c
      iodata(1, i) = iodata(1, i) + instance
      if (instance .lt. iodata(2, i)) iodata(2, i) = instance
      if (instance .gt. iodata(3, i)) iodata(3, i) = instance
      iosamples(i) = iosamples(i) + 1
c
      instance = 0
c
      return
c
      end
c     --- End of routine outintupdate
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine outswitchset(varid, value)
c
c     Setting an already defined output logical data item.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     varid........... (input, integer) Integer label which identifies
c                      the defined variable, as returned by the
c                      initializing routine.
c     value........... (input, logical) The value used to set the
c                      variable.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           varid
      logical           value
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking variable identifier.
c
      if ((varid .lt. nallodata0) .or. (varid .gt. nallodata)) then
        call errprint(0, '$B38', 4, 'outswitchset', ' ',
     +                1, varid, 0, 0.d0, ' ')
      endif
c
c     Setting the variable
c
      lodata(odataitem(varid)) = value
c
      return
c
      end
c     --- End of routine outswitchset
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'hwvarutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
