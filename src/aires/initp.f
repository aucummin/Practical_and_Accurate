c
c     FILE: initp.f                         Creation date: 02/AUG/1996.
c                                       LAST MODIFICATION: 17/MAY/2004.
c
c     This file contains several input processing and initialization
c     routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine inputscan
c
c     Scanning the input file and setting the input parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2001, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*256     line
      integer           llen, ifirst, ilast, dcode, dindex, irc
      logical           getiline
      integer           commandcode
c
c     FIRST EXECUTABLE STATEMENT
c
      ninierrors = 0
c
c     Setting the input unit
c
      inpunit    = 5
      inpnesl    = 0
      nilines(0) = 0
      iprompt    = .false.
      promptison = .false.
      traceison  = .false.
      goto 1010
c
 1005 continue
c
c     Exiting or changing input file. Returning if all the files
c     were already scanned.
c
      if (dcode .eq. -07) then
c
c       "Exit" directive. Stopping.
c
        call putlog(0, .true., 'Stopping.')
        call airesshutdown
        stop
      endif
c
      if (inpunit .eq. 5) goto 1100
      close(inpunit)
      if (inpunit .eq. minmacunit) then
        inpunit = 5
        iprompt = promptison
      else
        inpunit = inpunit - 1
      endif
      inpnesl = inpnesl - 1
c
c     Loop over all input lines.
c
 1010 continue
      if (.not. getiline(.true., 176, line, llen)) goto 1005
c
c     If the line starts with a label mark it is ignored.
c
      if (line(1:1) .eq. labelchar) goto 1010
c
c     Parsing the line obtained.
c
      ilast = 0
      call nextword(line, llen, ifirst, ilast)
c
      dcode = commandcode(line(ifirst:ilast),
     +                    ilast - ifirst + 1, dindex)
c
      if (dcode .lt. 0) goto 1005
      if (dcode .eq. 0) then
        call errprint(0, '$A07', idlerrsev2, 'inputpar',
     +                line(1:llen), 0, 0, 0, 0.d0, ' ')
        if (.not. iprompt) ninierrors = ninierrors + 1
      else
        call commandparse(dcode, dindex,
     +                    line(1:llen), ilast, llen, irc)
        if (irc .lt. 0) goto 1005
        if ((irc .gt. 0) .and. (.not. iprompt))
     +    ninierrors = ninierrors + 1
      endif
c
      goto 1010
c
 1100 continue
c
c     Scanning was finished. Returning.
c
c     Checking the remark file.
c
      if (remark) close(rmkut)
c     MATIAS: This might be unnecessary, and could let me 
c     have one less file to modify. Consider removal.
c     Added by Matias Tueros Nov 2011 for IDL compatibility
c     check if fieldtool.inp exist and set the corresponding flag
c    
c      fieldtool=.false.
c 
c      open(unit=211,status='old',file='fieldtool.inp',err=300)
c
c      fieldtool=.true.
c
 300  continue
c     
c     End addition
      return
c
      end
c     --- End of routine inputscan
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine commandparse(cdcode, cdindex, cdline,
     +                        argstart, llen, irc)
c
c     Input commands parsing and processing.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2001, 2003, 2004.
c
c
c     Arguments:
c     =========
c
c     cdcode.......... (input, integer) Command code.
c     cdindex......... (input, integer) Command index.
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     argstart........ (input, integer) Position of the last
c                      character before the beginning of argument
c                      string (trailing part of cdline).
c     llen............ (input, integer) Length of cdline.
c     irc............. (output, integer) Return code.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'pclepar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           cdcode, cdindex, argstart, llen, irc
      character*(*)     cdline
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, i1, i2, iditem, jrc
      integer           itmp1, itmp2, itmp3, itmp4, itmp5
      integer           itmp6, itmp7, itmp8, itmp9
      double precision  ftmp(2), ftmp1, ftmp2, ftmp3
      equivalence       (ftmp(1), ftmp1), (ftmp(2), ftmp2)
      logical           ltmp1, ltmp2
      character*64      auxstring
      integer           searchlabel
c
      double precision  l2meters, t2sec, e2gev, angle2deg, b2nt
      external          l2meters, t2sec, e2gev, angle2deg, b2nt
      integer           wordlocate
      logical           smatch
c
      character*8       altkeys(4)
      integer           altklen(4)
      data              altkeys(1), altklen(1) / 'Relative', 1 /
      data              altkeys(2), altklen(2) / '%'       , 1 /
      data              altkeys(3), altklen(3) / 'g/cm2'   , 5 /
      data              altkeys(4), altklen(4) / 'Infinite', 3 /
c
c     FIRST EXECUTABLE STATEMENT
c
      irc = 0
c
c     Looking if there is a first command argument.
c     (This is needed for most commands).
c
      i2 = argstart
      call nextword(cdline, llen, i1, i2)
c
c     Selecting level of command.
c
c     Codes from    1 to  199: Basic and input management directives.
c     Codes from  200 to  399: Initial conditions and run control.
c     Codes from  400 to  599: Physical quantities.
c     Codes from  600 to  799: Ouput control.
c
c     Codes from  800 to  999: Numerical input parameters.
c     Codes from 1000 to 1199: Length input parameters.
c     Codes from 1200 to 1399: Time input parameters.
c     Codes from 1400 to 1599: Energy input parameters.
c     Codes from 1600 to 1799: Logical input parameters.
c
      if (cdcode .lt.  200) goto 10000
      if (cdcode .lt.  400) goto 20000
      if (cdcode .lt.  600) goto 30000
      if (cdcode .lt.  800) goto 40000
c
      iditem = aditem(cdindex)
c
      if (cdcode .lt. 1000) goto 51000
      if (cdcode .lt. 1200) goto 52000
      if (cdcode .lt. 1400) goto 53000
      if (cdcode .lt. 1600) goto 54000
      if (cdcode .lt. 1800) goto 55000
      if (cdcode .lt. 2000) goto 56000
      if (cdcode .lt. 2200) goto 57000
c
c     This point should never be reached.
c
      call errprint(0, '$A08', 4, 'commandparse', '(In section 0)',
     +              0, 0, 0, 0.d0, ' ')
      return
c
c
c     SECTION 1: Basic and input management directives.
c
10000 continue
c
      if (cdcode .eq. 010) then
c
c       Command code 010 corresponds to directive: Remark
c
        if ((i2 .ge. i1) .and. (cdline(i1:i1) .eq. labelchar)) then
c
c         Big remark option. Copying lines into remark file until
c         label is reached.
c
          irc = searchlabel(cdline(i1:i2), .true.)
          if (irc .ne. 0) call errprint(0, '$A11', 2, 'commandparse',
     +                                  cdline(i1:i2),
     +                                  0, 0, 0, 0.d0, ' ')
c
        else
c
c         One-line-only remark option.
c
          call rmkcopy(cdline(argstart+2:llen), llen - argstart - 1)
c
        endif
c
      else if (cdcode .eq. 012) then
c
c       Command code 012 corresponds to directive: SetGlobal
c
        call setglobalvar(0, cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 014) then
c
c       Command code 014 corresponds to directive: DelGlobal
c
        call delglobalvar(cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 016) then
c
c       Command code 016 corresponds to directive: Import
c
        call setglobalvar(1, cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 017) then
c
c       Command code 016 corresponds to directive: ImportShell
c
        call setglobalvar(2, cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 018) then
c
c       Command code 018 corresponds to directive: Brackets
c
c       Getting On-Off switch.
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    2, .true., idlerrsev, ltmp1, jrc)
c
        if (ltmp1) then
          if ((jrc .eq. 1) .or.
     +        ((jrc .eq. 0) .and. (i2 .eq. llen))) then
c
c           No brackets specified, using defaults.
c
            opbr = defopbr
            clbr = defclbr
            brec = labelchar
c
          else
c
c           Needs to read arguments. Getting first bracket.
c
            call getsgchar((jrc .le. 1), cdline, llen, i1, i2,
     +                     0, 0, itmp1, irc)
c
            if (irc .ne. 0) then
              call errprint(0, '$A12', idlerrsev, 'commandparse', ' ',
     +                      0, 0, 0, 0.d0, cdline(1:llen))
              return
            endif
c
c           Getting second bracket.
c
            call getsgchar(.true., cdline, llen, i1, i2,
     +                     0, 0, itmp2, irc)
c
            if ((irc .ne. 0) .or. (itmp1 .eq. itmp2)) then
              call errprint(0, '$A12', idlerrsev, 'commandparse', ' ',
     +                      0, 0, 0, 0.d0, cdline(1:llen))
              return
            endif
c
c           Getting bracket escape character.
c
            call getsgchar(.true., cdline, llen, i1, i2,
     +                     1, ichar(labelchar), itmp3, irc)
c
            ltmp2 = (irc .le. 1)
            if ((irc .gt. 2) .or.
     +          (ltmp2 .and.
     +           ((itmp3 .eq. itmp1) .or. (itmp3 .eq. itmp2)))) then
              call errprint(0, '$A12', idlerrsev, 'commandparse', ' ',
     +                      0, 0, 0, 0.d0, cdline(1:llen))
              irc = 2
              return
            endif
c
c           Assigning.
c
            opbr = char(itmp1)
            clbr = char(itmp2)
            if (ltmp2) brec = char(itmp3)
c
          endif
c
c         Checking that the already defined variables do not contain
c         such characters.
c
          call glovarbracheck(' ', 0, 4, irc)
c
        endif
c
c       Setting brackets switch.
c
        bracketson = ltmp1
c
      else if (cdcode .eq. 020) then
c
c       Command code 020 corresponds to directive: Skip
c
        if (i2 .ge. i1) then
          if (cdline(i1:i1) .ne. labelchar) then
            call errprint(0, '$A10', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          else
            irc = searchlabel(cdline(i1:i2), .false.)
            if (irc .ne. 0) call errprint(0, '$A11', idlerrsev,
     +                                    'commandparse',
     +                                    cdline(i1:i2),
     +                                    0, 0, 0, 0.d0, ' ')
          endif
        else
          call errprint(0, '$A10', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 6
        endif
c
      else if (cdcode .eq. 022) then
c
c       Command code 015 corresponds to directive: Input
c
        if (i2 .lt. i1) then
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 6
        else
          if (inpunit .ge. maxmacunit) then
            call errprint(0, '$A13', 4, 'commandparse',  ' ',
     +                    0, 0, 0, 0.d0, ' ')
            irc = 10
          else
c
c           Argument and nesting OK. Trying to open file.
c
            if (inpunit .eq. 5) then
              itmp1 = minmacunit
            else
              itmp1 = inpunit + 1
            endif
c
c           Searching the file.
c
            call lookforfile(cdline(i1:i2),
     +                       inputsearchpath, searchpathlen, pathsep,
     +                       ltmp1, auxfilestring, itmp2)
c
            if (ltmp1) then
c
c             The file was found. Opening.
c
              open(itmp1, file = auxfilestring, status = 'OLD',
     +                    err = 3020)
c
c             The file was successfully opened.
c
              inpunit          = itmp1
              iprompt          = .false.
              inpnesl          = inpnesl + 1
              nilines(inpnesl) = 0
c
            else
c
c             No file(s) found.
c
              goto 3020
c
            endif
          endif
        endif
c
      else if (cdcode .eq. 024) then
c
c       Command code 024 corresponds to directive: InputPath
c
        if (i1 .le. i2) then
c
          if (cdline(i1:i2) .eq. 'Append') then
            call nextword(cdline, llen, i1, i2)
          else
            searchpathlen = 0
          endif
c
          if (i1 .le. i2) then
c
c           Setting the search path.
c
            if (cdline(i1:i1) .eq. pathsep) then
              itmp1 = i1 + 1
            else
              itmp1 = i1
            endif
            itmp2 = itmp1
c
            do i = itmp1, i2
c
              if (cdline(i:i) .eq. pathsep) then
c
                auxfilestring = cdline(itmp2:i-1)
                itmp3 = i - itmp2
                call absdirset(auxfilestring, itmp3)
c
                if (itmp3 .gt. 0) then
                  itmp4 = searchpathlen + itmp3 + 1
c
                  if (itmp4 .le. 256) then
c
                    inputsearchpath(searchpathlen+1:256) =
     +                      auxfilestring(1:itmp3) // pathsep
                    searchpathlen = itmp4
c
                  else
                    call errprint(0, '$T16', idlerrsev, 'commandparse',
     +                            ' ', 0, 0, 0, 0.d0, cdline(i1:i2))
                    irc = 3
                  endif
                endif
                itmp2 = i + 1
              endif
            enddo
c
            if (cdline(i2:i2) .ne. pathsep) then
c
              auxfilestring = cdline(itmp2:i2)
              itmp3 = i2 - itmp2 + 1
              call absdirset(auxfilestring, itmp3)
c
              if (itmp3 .gt. 0) then
                itmp4 = searchpathlen + itmp3 + 1
c
                if (itmp4 .le. 256) then
c
                  inputsearchpath(searchpathlen+1:256) =
     +                    auxfilestring(1:itmp3) // pathsep
                  searchpathlen = itmp4
c
                else
                  call errprint(0, '$T16', idlerrsev, 'commandparse',
     +                          ' ', 0, 0, 0, 0.d0, cdline(i1:i2))
                  irc = 3
                endif
              endif
            endif
c
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 6
          endif
        else
c
c         InputPath with no arguments clears the search path.
c
          searchpathlen = 0
c
        endif
c
      else if (cdcode .eq. 025) then
c
c       Command code 025 corresponds to directive: Echo
c
        if (i1 .le. llen) then
          write(6, 2050) cdline(i1:llen)
        else
          write(6, 2050)
        endif
 2050   format(a)
c
      else if (cdcode .eq. 026) then
c
c       Command code 026 corresponds to directive: Trace
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, traceison, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 028) then
c
c       Command code 028 corresponds to directive: CheckOnly
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, inpcheckonly, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 029) then
c
c       Command code 029 corresponds to directive: StopOnError
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., 3, ltmp1, jrc)
        if (jrc .le. 1) then
          if (ltmp1) then
            idlerrsev = 4
          else
            idlerrsev = 3
          endif
          if (pgmcode .lt. 2000) then
            idlerrsev2 = idlerrsev
          else
            idlerrsev2 = 2
          endif
        else
          irc = jrc
        endif
c
      else if (cdcode .eq. 030) then
c
c       Command code 030 corresponds to directive: ForceInit
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, runforcein, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 032) then
c
c       Command code 032 corresponds to directive: Prompt
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, promptison, jrc)
        if (inpnesl .le. 0) iprompt = promptison
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 034) then
c
c       Command code 034 corresponds to directive: Help
c
        call helprint(cdline, llen, i1, i2)
c
      else if (cdcode .eq. 035) then
c
c       Command code 035 corresponds to directive: Help + Prompt On
c       (Actually '?' directive).
c
        call helprint(cdline, llen, i1, i2)
        promptison = .true.
        if (inpnesl .le. 0) iprompt = promptison
c
      else if (cdcode .eq. 040) then
c
c       Command code 040 corresponds to directive: ForceModelName
c
        call extmodelname(auxstring, itmp1)
c
        nforcextset = nforcextset + 1
        if (itmp1 .gt. 0)
     +    notforcext  = ( notforcext .or. (i1 .gt. i2) .or.
     +                    (auxstring(1:itmp1) .ne. cdline(i1:i2)) )
c
      else if (cdcode .eq. 042) then
c
c       Command code 042 corresponds to directive: Shell
c
        if (i1 .le. llen) then
          call sysspawn(cdline(i1:llen), ' ', ' ', irc)
        endif
c
      else
c
c       No more commands for this section.
c       This point should never be reached.
c
        call errprint(0, '$A08', 4, 'commandparse', '(In section 1)',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
      return
c
c     SECTION 2: Initial conditions and run control.
c
20000 continue
c
      if (cdcode .eq. 210) then
c
c       Command code 210 corresponds to directive: TaskName
c
        if (i2 .lt. i1) then
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 6
        else
c
          if (cdline(i1:i2) .eq. 'Append') then
            call nextword(cdline, llen, i1, i2)
          else
            tasknamelen = 0
          endif
c
          if (i2 .lt. i1) then
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 6
          else
c
c           Setting the task name.
c
            taskname(tasknamelen+1:64) = cdline(i1:i2)
c
            if (tasknamelen .le. 0) tasknameset = tasknameset + 1
            tasknamelen = tasknamelen + i2 - i1 + 1
c
            if (tasknamelen .gt. 64) then
              tasknamelen = 64
              call errprint(0, '$T15', 2, 'commandparse', ' ',
     +                      0, 0, 0, 0.d0, taskname(1:tasknamelen))
              irc = 1
            endif
c
c           Setting the version name, if given.
c
            call getnumber(.true., cdline, llen, i1, i2,
     +                     1, 0.0d0, ftmp1, itmp1)
            if (itmp1 .le. 1) then
              itmp2 = ftmp1
              if ((itmp2 .ge. 0) .and. (itmp2 .le. 999)) then
                tasknamever = itmp2
              else
                call errprint(0, '$A15', idlerrsev, 'commandparse',
     +                        cdline(1:llen), 1, itmp2, 0, 0.d0, ' ')
                irc = 4
              endif
            else
              call errprint(0, '$A16', idlerrsev, 'commandparse', ' ',
     +                      0, 0, 0, 0.d0, cdline(i1:i2))
              irc = itmp1
            endif
          endif
        endif
c
      else if (cdcode .eq. 212) then
c
c       Command code 212 corresponds to directive: TotalShowers
c
        call getnumber(.false., cdline, llen, i1, i2, 2, 0.d0,
     +                 ftmp1, irc)
c
        if (irc .eq. 0) then
c
          itmp1 = ftmp1
          if ((itmp1 .gt. 0) .and. (itmp1 .le. 759375) 
     +        .and. (itmp1 .eq. ftmp1)                 ) then
            mtotshowers = itmp1
            totshwset   = totshwset + 1
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else
          call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        endif
c
      else if (cdcode .eq. 213) then
c
c       Command code 213 corresponds to directive: FirstShowerNumber
c
        call getnumber(.false., cdline, llen, i1, i2, 2, 0.d0,
     +                 ftmp1, irc)
c
        if (irc .eq. 0) then
c
          itmp1 = ftmp1
          if ((itmp1 .gt. 0) .and. (itmp1 .le. 759375) 
     +        .and. (itmp1 .eq. ftmp1)                 ) then
            firstshowernon1 = itmp1 - 1
            firstshowerset  = firstshowerset + 1
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else
          call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        endif
c
      else if (cdcode .eq. 214) then
c
c       Command code 214 corresponds to directive: ShowersPerRun
c
c       Searching for the keyword "Infinite"
c
        if ((i1 .le. i2) .and.
     +      (cdline(i1:i2) .eq. altkeys(4)(1:min(8,
     +                                     max(i2 - i1 + 1,
     +                                         altklen(3))))))
     +  then
c
c         "Infinite" selected.
c
          runshowers = -1
          runshwset  = runshwset + 1
c
        else
c
c         Looking for a valid number.
c
          call getnumber(.false., cdline, llen, i1, i2, 2, 0.d0,
     +                   ftmp1, irc)
c
          if (irc .eq. 0) then
c
            itmp1 = ftmp1
            if (itmp1 .eq. ftmp1) then
              if (itmp1 .eq. 0) itmp1 = -1
              runshowers = itmp1
              runshwset  = runshwset + 1
            else
              call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = 8
            endif
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          endif
        endif
c
      else if (cdcode .eq. 216) then
c
c       Command code 216 corresponds to directive: MaxCpuTimePerRun
c
c       Searching for the keyword "Infinite"
c
        if ((i1 .le. i2) .and.
     +      (cdline(i1:i2) .eq. altkeys(4)(1:min(8,
     +                                     max(i2 - i1 + 1,
     +                                         altklen(3))))))
     +  then
c
c         "Infinite" selected.
c
          cpuperrun = -1
          cpurunset = cpurunset + 1
c
        else
c
c         Looking for a valid time specification.
c
          call nplusunit(t2sec, .false., cdline, llen, i1, i2,
     +                   0, altkeys, altklen, .false.,
     +                   0.0d0, ftmp1, itmp6, auxstring, itmp1, irc)
c
          if (irc .le. 3) then
c
c           Valid time (even if negative). Assigning.
c
            cpuperrun = ftmp1
            cpurunset = cpurunset + 1
c
            if (irc .ne. 0) call errprint(0, '$A21', 2, 'commandparse',
     +                                    cdline(1:llen),
     +                                    0, 0, 0, 0.d0,
     +                                    auxstring(1:itmp1))
            irc = 0
c
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          endif
        endif
c
      else if (cdcode .eq. 218) then
c
c       Command code 218 corresponds to directive: RunsPerProcess
c
c       Searching for the keyword "Infinite"
c
        if ((i1 .le. i2) .and.
     +      (cdline(i1:i2) .eq. altkeys(4)(1:min(8,
     +                                     max(i2 - i1 + 1,
     +                                         altklen(3))))))
     +  then
c
c         "Infinite" selected.
c
          processjobs    = -1
          processjobsset = processjobsset + 1
c
        else
c
c         Looking for a valid number.
c
          call getnumber(.false., cdline, llen, i1, i2, 2, 0.d0,
     +                   ftmp1, irc)
c
          if (irc .eq. 0) then
c
            itmp1 = ftmp1
            if (itmp1 .eq. ftmp1) then
              if (itmp1 .eq. 0) itmp1 = -1
              processjobs    = itmp1
              processjobsset = processjobsset + 1
            else
              call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = 8
            endif
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          endif
        endif
c
      else if (cdcode .eq. 220) then
c
c       Command code 220 corresponds to directive: DumpFile
c
c       (Not implemeted yet).
c
99999   continue
c
      else
c
c       No more commands for this section.
c       This point should never be reached.
c
        call errprint(0, '$A08', 4, 'commandparse', '(In section 2)',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
      return
c
c     SECTION 3: Physical data.
c
30000 continue
c
      if (cdcode .eq. 410) then
c
c       Command code 410 corresponds to directive: PrimaryParticle
c
        call getpclecode(.false., cdline, llen, i1, i2,
     +                   itmp1, itmp2, jrc)
c
        if (jrc .le. 4) then
          if (jrc .ne. 3) then
c
c           Single particle specified
c
            itmp4 = nshprimary + 1
            if (itmp4 .gt. maxshprimaries) then
              call errprint(0, '$A18', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              itmp3 = itmp4 + 1
            else
c
              shprimary(itmp4) = itmp2
              itmp3      = itmp4
              nshprimary = itmp4
c
            endif
c
          else
c
c           Particle group specified
c
            if (itmp1 .le. 0) return
            itmp4 = nshprimary + itmp1
            if (itmp4 .gt. maxshprimaries) then
              call errprint(0, '$A18', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              itmp3 = itmp4 + 1
            else
c
              itmp3 = nshprimary + 1
              call pglist(itmp2, itmp5, shprimary(itmp3))
c
              if (itmp5 .ne. itmp1) call errprint(0, '*', 4,
     +          'commandparse', 'Serious problem with pglist',
     +          0, 0, 0, 0.d0, cdline(1:llen))
c
              nshprimary = itmp4
c
            endif
          endif
c
c         Setting the weight(s).
c
          if (itmp3 .le. itmp4) then
c
            call getnumber(.true., cdline, llen, i1, i2,
     +                     2, 1.0d0, ftmp1, itmp1)
            if ((itmp1 .ge. 3) .or. (ftmp1 .le. 0)) then
              call errprint(0, '$A19', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0,
     +                      cdline(i1:i2))
              ftmp1 = 1
              irc   = 2
            endif
c
            do itmp1 = itmp3, itmp4
              shprimarywt(itmp1) = ftmp1
            enddo
c
          else
            irc = 8
          endif
c
        else
          call errprint(0, '$A17', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else if (cdcode .eq. 412) then
c
c       Command code 412 corresponds to directive: PrimaryEnergy
c
c       Looking for "pryenergymin".
c
        call nplusunit(e2gev, .false., cdline, llen, i1, i2,
     +                 0, altkeys, altklen, .false.,
     +                 0.0d0, ftmp1, itmp6, auxstring, itmp1, jrc)
c
        if (jrc .le. 3) then
c
          if (jrc .ne. 0) then
            call errprint(0, '$A22', 2, 'commandparse', cdline(1:llen),
     +                    0, 0, 0, 0.d0, auxstring(1:itmp1))
            irc = 2
          endif
c
c         Checking if the energy is within bounds.
c
          if ((ftmp1 .lt. superemin) .or. (ftmp1 .gt. hyperemax)) then
            call errprint(0, '$A20', idlerrsev, 'commandparse', ' ',
     +                    0, 0, 0, 0.d0, cdline(1:llen))
            irc = 8
c
          else
c
            pryenergymin = ftmp1
            pryeminset   = pryeminset + 1
c
c           Looking for "pryenergymax".
c
            call nplusunit(e2gev, .true., cdline, llen, i1, i2,
     +                     0, altkeys, altklen, .false.,
     +                     0.0d0, ftmp1, itmp6, auxstring, itmp1, jrc)
c
            if (jrc .le. 3) then
c
              if (jrc .ne. 0) then
                call errprint(0, '$A22', 2, 'commandparse',
     +                        cdline(1:llen),
     +                        0, 0, 0, 0.d0, auxstring(1:itmp1))
                irc = 2
              endif
c
c             Checking if the energy is within bounds.
c
              if ((ftmp1 .lt. superemin) .or. (ftmp1 .gt. hyperemax))
     +        then
                call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                        ' ', 0, 0, 0, 0.d0, cdline(1:llen))
                irc = 8
              else
c
                if (ftmp1 .ge. pryenergymin) then
                  fidata(1, 1) = ftmp1
                else
                  fidata(1, 1) = pryenergymin
                  pryenergymin = ftmp1
                endif
                fidatawaset(1) = pryeminset
c
c               Looking for "pryenergyslp".
c
                call getnumber(.true., cdline, llen, i1, i2, 0, 0.d0,
     +                         ftmp1, jrc)
c
                if (jrc .eq. 0) then
c
                  fidata(1, 2)   = ftmp1
                  fidatawaset(2) = pryeminset
c
                else if (jrc .gt. 2) then
                  call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                          cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                  irc = jrc
                endif
              endif
c
            else if (jrc .gt. 4) then
              call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
          endif
c
        else
          call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else if (cdcode .eq. 414) then
c
c       Command code 414 corresponds to directive: PrimaryZenAngle
c
c       Getting angle (must be in the range [0 deg, 90 deg)).
c
        call nplusunit(angle2deg, .false., cdline, llen, i1, i2,
     +                 0, altkeys, altklen, .false.,
     +                 0.d0, ftmp1, itmp6, auxstring, itmp5, jrc)
        if (jrc .eq. 3) jrc = 1
c
        if (jrc .le. 1) then
c
          if ((ftmp1 .ge. 0) .and. (ftmp1 .lt. 90.d0)) then
c
c           Setting both min and max angles, and distribution
c           switch.
c
            fidata(1, 3)   = ftmp1
            fidata(1, 4)   = ftmp1
            iidata(1, 1)   = 1
            fidatawaset(3) = fidatawaset(3) + 1
c
            if (jrc .eq. 1) then
              call errprint(0, '$R23', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
c           Looking for "pryzenithmax"
c
            call nplusunit(angle2deg, .true., cdline, llen, i1, i2,
     +                     0, altkeys, altklen, .false.,
     +                     0.d0, ftmp1, itmp6, auxstring, itmp5, jrc)
            if (jrc .eq. 3) jrc = 1
c
            if (jrc .le. 1) then
c
              if ((ftmp1 .ge. 0) .and. (ftmp1 .lt. 90.d0)) then
                if (ftmp1 .ge. fidata(1, 3)) then
                  fidata(1, 4) = ftmp1
                else
                  fidata(1, 4) = fidata(1, 3)
                  fidata(1, 3) = ftmp1
                endif
                fidatawaset(4) = fidatawaset(3)
c
                if (jrc .eq. 1) then
                  call errprint(0, '$R23', 2, 'commandparse',
     +                          cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                  irc = jrc
                endif
c
c               Looking for the probability distribution switch.
c
                call nextword(cdline, llen, i1, i2)
c
                if (i1 .le. i2) then
c
                  if (cdline(i1:i2) .eq. 'S') then
                    iidata(1, 1) = 1
                    iidatawaset(1) = fidatawaset(3)
                  else if ((cdline(i1:i2) .eq. 'SC') .or.
     +                     (cdline(i1:i2) .eq. 'CS')) then
                    iidata(1, 1) = 2
                    iidatawaset(1) = fidatawaset(3)
                  else
                    call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                            cdline(1:llen), 0, 0, 0, 0.d0,
     +                            cdline(i1:i2))
                    irc = 8
                  endif
                endif
c
              else
                call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                        cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                irc = 8
              endif
c
            else if (jrc .ne. 4) then
              call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
        else
          call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else if (cdcode .eq. 416) then
c
c       Command code 416 corresponds to directive: PrimaryAzimAngle
c
c       Getting angle (must be in the range ([-180 deg, 360 deg]).
c
        call nplusunit(angle2deg, .false., cdline, llen, i1, i2,
     +                 0, altkeys, altklen, .false.,
     +                 0.d0, ftmp1, itmp6, auxstring, itmp5, jrc)
        if (jrc .eq. 3) jrc = 1
c
        if (jrc .le. 1) then
c
          if ((ftmp1 .ge. -180.d0) .and. (ftmp1 .le. 360.d0)) then
c
            fidata(1, 5)   = ftmp1
            fidata(1, 6)   = ftmp1
            fidatawaset(5) = fidatawaset(5) + 1
c
            if (jrc .eq. 1) then
              call errprint(0, '$R23', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
c           Looking for "pryazimmax"
c
            call nplusunit(angle2deg, .true., cdline, llen, i1, i2,
     +                     0, altkeys, altklen, .false.,
     +                     0.d0, ftmp1, itmp6, auxstring, itmp5, jrc)
            if (jrc .eq. 3) jrc = 1
c
            if (jrc .le. 1) then
c
              if ((ftmp1 .ge. -180.d0) .and.
     +            (ftmp1 .le. 360.d0)) then
                if (ftmp1 .ge. fidata(1, 5)) then
                  fidata(1, 6) = ftmp1
                else
                  fidata(1, 6) = fidata(1, 5)
                  fidata(1, 5) = ftmp1
                endif
                fidatawaset(6) = fidatawaset(5)
c
                if (jrc .eq. 1) then
                  call errprint(0, '$R23', 2, 'commandparse',
     +                          cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                  irc = jrc
                else
                  call nextword(cdline, llen, i1, i2)
                endif
              else
                call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                        cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                irc = 8
              endif
            endif
c
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
c         Looking for "Magnetic" or "Geographic".
c
          if ((irc .le. 1) .and. (i1 .le. i2)) then
            auxstring = 'Magnetic'
            auxstring(21:40) = 'Geographic'
            itmp1 = min(20, i2 - i1 + 1)
c
            if (cdline(i1:i2) .eq. auxstring(1:itmp1)) then
              lidatawaset(1) = fidatawaset(5)
              lidata(1, 1)   = .false.
            else if (cdline(i1:i2) .eq. auxstring(21:20+itmp1)) then
              lidatawaset(1) = fidatawaset(5)
              lidata(1, 1)   = .true.
            else
              call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                      cdline(1:llen),
     +                      0, 0, 0, 0.d0, cdline(i1:i2))
              irc = 8
            endif
          endif
c
        else
          call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else if (cdcode .eq. 418) then
c
c       Command code 418 corresponds to directive: ThinningEnergy
c
c       Reading number and unit (presumably energy unit, but
c       the keyword "Relative", with minimum length 1, is also
c       valid).
c
        if (thinningon) then
c
          call nplusunit(e2gev, .false., cdline, llen, i1, i2,
     +                   1, altkeys(1), altklen(1), .true.,
     +                   0.0d0, ftmp1, itmp6, auxstring, itmp1, jrc)
c
          if ((jrc .eq. 0) .and. (ftmp1 .ge. 0)) then
c
c           Absolute energy specification.
c
            fidata(1, 7)   = ftmp1
            fidatawaset(7) = fidatawaset(7) + 1
c
          else if ((jrc .le. 2) .and. (ftmp1 .ge. 0)) then
c
c           Relative thinning specification.
c
            fidata(1, 7)   = - ftmp1
            fidatawaset(7) = fidatawaset(7) + 1
c
            if (jrc .eq. 1) then
              call errprint(0, '$A26', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
          else
            call errprint(0, '$A25', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = jrc
          endif
c
        else
          call errprint(0, '*', 2, 'commandparse',
     +         'Thinning is disabled. Directive ignored.',
     +         0, 0, 0, 0.d0, cdline(1:llen))
          irc = 2
        endif
c
      else if (cdcode .eq. 420) then
c
c       Command code 420 corresponds to directive: InjectionAltitude
c
c       And also to synonym: InjectionDepth
c
c       Reading number and unit (presumably length unit, but
c       the depth unit "g/cm2" is also valid).
c
c       Cannot convert from altitude to depth (or vice-versa) due to
c       the fact that the atmospheric model is still undefined.
c       Quantities pending conversion will be marked negative
c       and processed later.
c
        call nplusunit(l2meters, .false., cdline, llen, i1, i2,
     +                 1, altkeys(3), altklen(3), .true., 0.0d0,
     +                 ftmp1, itmp6, auxstring, itmp1, jrc)
c
        if (jrc .eq. 0) then
c
c         Valid length unit: The altitude was specified.
c
          if (ftmp1 .gt. 0) then
c
c           The altitude is valid.
c
            fidata(1, 8)   = ftmp1
            fidatawaset(8) = fidatawaset(8) + 1
            fidata(1, 9)   = -10
c
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else if (jrc .le. 2) then
c
c         Missing unit or valid depth specification.
c
          if (ftmp1 .ge. 0) then
c
c           The atmospheric depth is valid.
c
            fidata(1, 8)   = -10
            fidatawaset(8) = fidatawaset(8) + 1
            fidata(1, 9)   = ftmp1
c
            if (jrc .eq. 1) then
              call errprint(0, '$A24', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else if (cdcode .eq. 422) then
c
c       Command code 422 corresponds to directive: GroundAltitude
c
c       And also to synonym: GroundDepth
c
c       Reading number and unit (presumably length unit, but
c       the depth unit "g/cm2" is also valid).
c
c       Cannot convert from altitude to depth (or vice-versa) due to
c       the fact that the atmospheric model is still undefined.
c       Quantities pending conversion will be marked negative
c       and processed later.
c
        call nplusunit(l2meters, .false., cdline, llen, i1, i2,
     +                 1, altkeys(3), altklen(3), .true., 0.0d0,
     +                 ftmp1, itmp6, auxstring, itmp1, jrc)
c
        if (jrc .eq. 0) then
c
c         Valid length unit: The altitude was specified.
c
          if (ftmp1 .ge. 0) then
c
c           The altitude is valid.
c
            fidata(1, 10)   = ftmp1
            fidatawaset(10) = fidatawaset(10) + 1
            fidata(1, 11)   = -10
c
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else if (jrc .le. 2) then
c
c         Missing unit or valid depth specification.
c
          if (ftmp1 .gt. 0) then
c
c           The atmospheric depth is valid.
c
            fidata(1, 10)   = -10
            fidatawaset(10) = fidatawaset(10) + 1
            fidata(1, 11)   = ftmp1
c
            if (jrc .eq. 1) then
              call errprint(0, '$A24', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else if (cdcode .eq. 424) then
c
c       Command code 424 corresponds to directive: Atmosphere
c
        call getnumber(.false., cdline, llen, i1, i2, 2, 0.d0,
     +                 ftmp1, irc)
c
        if (irc .eq. 0) then
c
          itmp1 = ftmp1
          if (itmp1 .ge. 0) then
            atmoslabel = itmp1
            atmoslset  = atmoslset + 1
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else
          call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        endif
c
      else if (cdcode .eq. 426) then
c
c       Command code 426 corresponds to directive: GeomagneticField.
c
        itmp2 = 1
c
        if (i2 .ge. i1) then
c
c         Searching On-Off switch.
c
          if (cdline(i1:i2) .eq. 'Off') then
c
c           Geomagnetic field disabled.
c
            if (i2 .eq. llen) then
              iidata(1, 4)   = 0
              iidatawaset(4) = iidatawaset(4) + 1
              itmp2          = 0
              i2             = 0
            else
              irc = 12
            endif
c
          else if (cdline(i1:i2) .eq. 'On') then
c
c           Geomagnetic field enabled with default value.
c
            iidata(1, 4)   = 1
            iidatawaset(4) = iidatawaset(4) + 1
            itmp2          = 0
c
            call nextword(cdline, llen, i1, i2)
c
          else
c
c           Searching field strength.
c
            call nplusunit(b2nt, .false., cdline, llen, i1, i2,
     +                     0, altkeys, altklen, .true.,
     +                     0.d0, ftmp1, itmp6, auxstring, itmp5, jrc)
            if (jrc .eq. 3) jrc = 1
c
            if (jrc .le. 2) then
c
c             Absolute magnetic field was specified.
c
              iidata(1, 4)    = 1
              iidatawaset(4)  = iidatawaset(4) + 1
              itmp2           = 0
              fidata(1, 35)   = ftmp1
              fidatawaset(35) = iidatawaset(4)
c
              if (jrc .eq. 1) then
                call errprint(0, '$B23', 2, 'commandparse',
     +                        cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              endif
c
c             Searching for the inclination.
c
              call nplusunit(angle2deg, .true., cdline, llen, i1, i2,
     +                       0, altkeys, altklen, .false.,
     +                       0.d0, ftmp1, itmp6, auxstring, itmp5, jrc)
              if (jrc .eq. 3) jrc = 1
c
              if (jrc .le. 1) then
c
c               Absolute inclination was specified.
c
                fidata(1, 36)   = ftmp1
                fidatawaset(36) = fidatawaset(35)
c
                if (jrc .eq. 1) then
                  call errprint(0, '$R23', 2, 'commandparse',
     +                          cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                endif
c
c               Searching for the declination.
c
                call nplusunit(angle2deg, .true., cdline, llen, i1, i2,
     +                         0, altkeys, altklen, .false.,
     +                         0.d0, ftmp1, itmp6, auxstring,
     +                         itmp5, jrc)
                if (jrc .eq. 3) jrc = 1
c
                if (jrc .le. 1) then
c
c                 Absolute declination was specified.
c
                  fidata(1, 37)   = ftmp1
                  fidatawaset(37) = fidatawaset(36)
c
                  if (jrc .eq. 1) then
                    call errprint(0, '$R23', 2, 'commandparse',
     +                          cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                  else
                    call nextword(cdline, llen, i1, i2)
                  endif
c
                endif
              endif
              if (jrc .lt. 4) irc = jrc
            else
              if (jrc .lt. 5) irc = jrc
            endif
          endif
c
c         Searching for the fluctuation parameter.
c
          if ((irc .eq. 0) .and. (i1 .le. i2)) then
c
            auxstring = 'Fluctuations'
            i         = min(12, i2 - i1 + 1)
c
            if (cdline(i1:i2) .eq. auxstring(1:i)) then
c
              call nplusunit(b2nt, .true., cdline, llen, i1, i2,
     +                       2, altkeys(1), altklen(1), .false.,
     +                       0.d0, ftmp1, itmp6, auxstring,
     +                       itmp5, jrc)
              if (jrc .eq. 3) jrc = 1
c
              if ((jrc .le. 2) .and. (ftmp1 .ge. 0)) then
c
                if (jrc .eq. 2) then
c
c                 Relative fluctuation specification.
c
                  if (itmp6 .eq. 2) ftmp1 = ftmp1 / 100
                  fidata(1, 38) = -ftmp1
c
                else
c
c                 Absolute fluctuation specification.
c
                  fidata(1, 38) = ftmp1
c
                endif

                iidata(1, 4)    = 2
                iidatawaset(4)  = iidatawaset(4) + itmp2
                fidatawaset(38) = iidatawaset(4)
c
                if (jrc .eq. 1) then
                  call errprint(0, '$B23', 2, 'commandparse',
     +                          cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                  irc = jrc
                endif
c
              else
                call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                        cdline(1:llen), 0, 0, 1, ftmp3, ' ')
                irc = 8
              endif
c
            else
              call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                      cdline(1:llen),
     +                      0, 0, 0, 0.d0, cdline(i1:i2))
              irc = 6
            endif
c
          else if (irc .gt. 1) then
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          endif
        else
          call errprint(0, '$A10', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 6
        endif
c
      else if (cdcode .eq. 428) then
c
c       Command code 428 corresponds to directive: Site
c
        if (i2 .ge. i1) then
c
c         Scanning the site library to get matching site(s).
c
          itmp1 = min(16, i2 - i1 + 1)
          itmp2 = 0
c
          do i = 0, nlibsites
            if (cdline(i1:i2) .eq. sitename(i)(1:itmp1)) then
              itmp2 = itmp2 + 1
              itmp3 = i
            endif
          enddo
c
          if (itmp2 .le. 0) then
c
c           No matching names.
c
            call errprint(0, '$S11', idlerrsev, 'commandparse',
     +                    '(Unknown site)',
     +                    0, 0, 0, 0.d0, cdline(i1:i2))
            irc = 8
c
          else if (itmp2 .gt. 1) then
c
c           Ambiguous specification.
c
            call errprint(0, '$S11', idlerrsev, 'commandparse',
     +           '(Ambiguous specification. Supply more characters)',
     +           0, 0, 0, 0.d0, cdline(i1:i2))
            irc = 8
c
          else
c
c           Setting the site label.
c
            iidatawaset(3) = iidatawaset(3) + 1
            iidata(1, 3)   = itmp3
            irc            = 0
c
          endif
        else
          call errprint(0, '$A10', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 6
        endif
c
      else if (cdcode .eq. 430) then
c
c       Command code 430 corresponds to directive: AddSite
c
        if (i2 .ge. i1) then
c
          itmp1 = i1
          itmp2 = i2
c
c         Searching for the latitude.
c
          call nplusunit(angle2deg, .true., cdline, llen, i1, i2,
     +                   0, altkeys, altklen, .false.,
     +                   0.d0, ftmp1, itmp6, auxstring, itmp5, irc)
c
          if (irc .eq. 0) then
c
c           Searching for the longitude.
c
            call nplusunit(angle2deg, .true., cdline, llen, i1, i2,
     +                     0, altkeys, altklen, .false.,
     +                     0.d0, ftmp2, itmp6, auxstring, itmp5, irc)
c
            if (irc .eq. 0) then
c
c             Searching for the altitude.
c
              call nplusunit(l2meters, .true., cdline, llen, i1, i2,
     +                       1, altkeys(3), altklen(3), .true.,
     +                       0.d0, ftmp3, itmp6, auxstring, itmp5, jrc)
              if (jrc .eq. 3) jrc = 1
c
              if (jrc .le. 2) then
c
c               Altitude or depth was specified.
c
                if (jrc .ne. 0) then
c
c                 Depth in g/cm2 was specified.
c
                  if (ftmp3 .gt. 0) then
                    ftmp3 = - ftmp3
                  else
                    call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                            cdline(1:llen), 0, 0, 1, ftmp3, ' ')
                    irc = 8
                  endif
c
                  if (jrc .eq. 1) then
                    call errprint(0, '$A24', 2, 'commandparse',
     +                          cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                    irc = jrc
                  endif
                endif
c
              else
                irc = jrc
              endif
            else
              irc = 5
            endif
          else
            irc = 5
          endif
c
          if (irc .le. 1) then
c
c           Adding the site to the library.
c
            call addlibsite(cdline(itmp1:itmp2),
     +                      ftmp1, ftmp2, ftmp3, irc)
c
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          endif
c
        else
          call errprint(0, '$A10', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 6
        endif
c
      else if (cdcode .eq. 432) then
c
c       Command code 432 corresponds to directive: Date
c
c       Looking for the year.
c
        call getnumber(.false., cdline, llen, i1, i2,
     +                 0, 0.0d0, ftmp1, irc)
c
        if ((irc .eq. 0) .and. (ftmp1 .gt. 1000)) then
c
c         Checking for integer year.
c
          itmp1 = ftmp1
c
          if (itmp1 .eq. ftmp1) then
c
c           Integer year.
c
            ftmp3 = 1
c
c           Looking for month.
c
            call getnumber(.true., cdline, llen, i1, i2,
     +                     2, 1.0d0, ftmp2, jrc)
c
            if (jrc .eq. 0) then
              itmp1 = ftmp2
              if ((itmp1 .eq. ftmp2) .and.
     +            (itmp1 .ge. 1) .and. (itmp1 .le. 12)) then
c
c               Looking for day.
c
                call getnumber(.true., cdline, llen, i1, i2,
     +                         2, 1.0d0, ftmp3, jrc)
c
                if (jrc .eq. 0) then
                  itmp1 = ftmp3
                  if (.not. ((itmp1 .eq. ftmp3) .and.
     +                       (itmp1 .ge. 1) .and.
     +                       (itmp1 .le. 31))) then
                    call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                            cdline(1:llen), 0, 0, 0, 0.d0,
     +                            cdline(i1:i2))
                    irc = 8
                  endif
                else if (jrc .gt. 2) then
                  call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                          ' ', 0, 0, 0, 0.d0, cdline(i1:i2))
                  irc = 6
                endif
              else
                call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                        cdline(1:llen), 0, 0, 0, 0.d0,
     +                        cdline(i1:i2))
                irc = 8
              endif
            else if (jrc .gt. 2) then
              call errprint(0, '$A06', idlerrsev, 'commandparse', ' ',
     +                      0, 0, 0, 0.d0, cdline(i1:i2))
              irc = 6
            endif
c
c           Encoding as:
c           fdate = -(day + 32 month + 512 year)
c
            ftmp1 = - (512 * ftmp1 + 32 * ftmp2 + ftmp3)
c
          else
c
c           Floating point year. Checking that there are no more
c           arguments.
c
            if (i2 .lt. llen) then
              call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = 10
            endif
          endif
c
          if (irc .eq. 0) then
            fidatawaset(39) = fidatawaset(39) + 1
            fidata(1, 39)   = ftmp1
          endif
c
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, cdline(i1:i2))
          irc = 10
        endif
c
      else if (cdcode .eq. 434) then
c
c       Command code 434 corresponds to directive: AddSpecialParticle
c
        if ((i2 .ge. i1) .and. (i2 .lt. llen)) then
c
          itmp1 = i1
          itmp2 = i2
c
c         Searching for the macro specification.
c
          call nextword(cdline, llen, i1, i2)
c
c         Searching the file.
c
          call lookforfile(cdline(i1:i2),
     +                     inputsearchpath, searchpathlen, pathsep,
     +                     ltmp1, auxfilestring, itmp3)
          if (ltmp1) then
c
c           Checking existing names.
c
            call getpclecode(.false., cdline, llen, itmp1, itmp2,
     +                       i, itmp4, itmp5)
c
            if (itmp5 .ge. 8) then
c
c             The name is different to the existing ones.
c             Assigning the new "special" particle.
c
              itmp4 = i1
              itmp5 = i2
c
              call nextword(cdline, llen, i1, i2)     
c
              itmp6 = escmacropos(4, nescpcles)
              itmp7 = itmp5 - itmp4 + 1
              itmp8 = llen - i1 + 1
              itmp9 = itmp6 + itmp7 + itmp3 + itmp5
c
              if ((nescpcles .lt. nescodes) .and. (itmp9 .le. espms))
     +        then
c
                nescpcles              = nescpcles + 1
                lastescpcle            = nescpcles + pescode1 - 1
                escpclename(nescpcles) = cdline(itmp1:itmp2)
                escmacrover(nescpcles) = 0
                escmacrouse(nescpcles) = 0
c
                if ((itmp2 - itmp1) .gt. 15) then
                  call errprint(0, '*', 2, 'commandparse',
     +                 'Special particle name too long. Truncating.',
     +                 0, 0, 0, 0.d0, escpclename(nescpcles))
                endif
c
                itmp1                     = itmp6 + 1
                itmp2                     = itmp6 + itmp7
                escmacropos(1, nescpcles) = itmp1
                escmacropos(2, nescpcles) = itmp2
                escpclemacro(itmp1:espms) = cdline(itmp4:itmp5)
                itmp1                     = itmp2 + 1
                itmp2                     = itmp2 + itmp3
                escmacropos(3, nescpcles) = itmp2
                escpclemacro(itmp1:espms) = auxfilestring(1:itmp3)
                itmp1                     = itmp2 + 1
                itmp2                     = itmp2 + itmp8
                escmacropos(4, nescpcles) = itmp2
                if (itmp8 .gt. 0) then
                  escpclemacro(itmp1:espms) = cdline(i1:llen)
                endif
c
              else
                call errprint(0, '$S18', idlerrsev, 'commandparse',
     +               ' ', 0, 0, 0, 0.d0, cdline(1:llen))
                     irc = 6
              endif
            else
              call errprint(0, '$A17', idlerrsev, 'commandparse',
     +          '(Special particle name matches already existing one)',
     +          0, 0, 0, 0.d0, cdline(1:llen))
              irc = 8
            endif
          else
            call errprint(0, '$B14', idlerrsev, 'commandparse',
     +           ' ', 0, 0, 0, 0.d0, cdline(i1:i2))
            irc = 8
          endif
        else
          call errprint(0, '$A10', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 6
        endif
c
      else if (cdcode .eq. 436) then
c
c       Command code 436 corresponds to directive: SpecialParticLog
c
        call getnumber(.false., cdline, llen, i1, i2,
     +                 1, 1.d0, ftmp1, jrc)
c
        if (jrc .le. 1) then
c
          itmp1 = ftmp1
          if ((itmp1 .ge. 0) .and. (itmp1 .eq. ftmp1)) then
            sprimlog = min(itmp1, 2)
          else
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
c
        else
          call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        endif
c
c Added by Matias Tueros on Nov 2011 for ZHAireS IDL compatibility
      else if (cdcode .eq. 438) then
c
c       Command code 438 corresponds to directive: AddAntenna
c 
c       AddAntenna is handled by antennaparser, in fieldparser.f, to keep things as tidy as possible
        call antennaparser(cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 440) then
c
c       Command code 440 corresponds to directive: AddFrequency
c 
c       AddFrequency is handled by frequencyparser, in fieldparser.f, to keep things as tidy as possible
        call frequencyparser(cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 442) then
c
c       Command code 442 corresponds to directive: AddFraunhofrAngle
c 
c       AddFraunhofrAngle is handled by fraunhfrangleparser, in fieldparser.f, to keep things as tidy as possible
        call fraunhfrangleparser(cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 444) then
c
c       Command code 444 corresponds to directive: AddFraunhofrPhi
c 
c       AddFraunhofrPhi is handled by fraunhfrangleparser, in fieldparser.f, to keep things as tidy as possible
        call fraunhfrphiparser(cdline, llen, i1, i2, irc)
c
c End addition
c
      else if (cdcode .eq. 490) then
c
c       Command code 490 corresponds to directive: RandomSeed
c
        ltmp1 = .true.
        call getnumber(.false., cdline, llen, i1, i2, 0, 0.d0,
     +                 ftmp1, irc)
c
        if (irc .eq. 0) then
          if ((ftmp1 .le. 0) .or. (ftmp1 .ge. 1)) then
            ltmp1 = .false.
            call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          endif
        else
c
c         Searching the keyword 'GetFrom'
c
          if (i1 .le. i2) then
            if (smatch(cdline(i1:i2), 'GetFrom', 3)) then
c
c             Getting the number from an idf file.
c
              call nextword(cdline, llen, i1, i2)
c
              if (i1 .le. i2) then
                call rsdread(cdline(i1:i2), 3, ftmp1, irc)
                if (irc .gt. 0) then
                  ltmp1 = .false.
                  call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                 cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                endif
              else
                ltmp1 = .false.
                call errprint(0, '$A10', idlerrsev, 'commandparse',
     +               cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              endif
            else
              ltmp1 = .false.
              call errprint(0, '$A12', idlerrsev, 'commandparse',
     +             cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            endif
          else
            ltmp1 = .false.
            call errprint(0, '$A10', idlerrsev, 'commandparse',
     +           cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          endif
        endif
c
        if (ltmp1) then
          fidata(1, 50)   = ftmp1
          fidatawaset(50) = fidatawaset(50) + 1
        else
          irc = 8
        endif
c
      else
c
c       No more commands for this section.
c       This point should never be reached.
c
        call errprint(0, '$A08', 4, 'commandparse', '(In section 3)',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
      return
c
c     SECTION 4: Output control.
c
40000 continue
c
      if ((cdcode .eq. 610) .or. (cdcode .eq. 611)) then
c
c       Command code 610 corresponds to directive: InputListing
c       Command code 611 corresponds to directive: OutputListing
c
c       The options are Brief (default) and Full.
c
        ltmp2 = .true.
        if (i2 .lt. i1) then
c
c         No directive. Brief assumed.
c
          ltmp1 = .false.
c
        else if (cdline(i1:i2) .eq. 'Brief') then
c
          ltmp1 = .false.
c
        else if (cdline(i1:i2) .eq. 'Full') then
c
          ltmp1 = .true.
c
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc   = 4
          ltmp2 = .false.
        endif
c
        if (ltmp2) then
          if (cdcode .eq. 610) then
            fullinputlist  = ltmp1
          else
            fulloutputlist = ltmp1
          endif
        endif
c
      else if (cdcode .eq. 612) then
c
c       Command code 612 corresponds to directive: SaveInFile
c
        call saveinfile(.true., cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 614) then
c
c       Command code 614 corresponds to directive: SaveNotInFile
c
        call saveinfile(.false., cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 615) then
c
c       Command code 614 corresponds to directive: RecordObsLevels
c
        if ((cdline(i1:i2) .eq. 'Not') .or.
     +      (cdline(i1:i2) .eq. 'No' )) then
          ltmp1 = .false.
          call nextword(cdline, llen, i1, i2)
        else
          ltmp1 = .true.
        endif
c
        call recobslev(ltmp1, cdline, llen, i1, i2, irc)
c
      else if (cdcode .eq. 616) then
c
c       Command code 616 corresponds to directive: SeparateShowers.
c
c       Searching for the keyword "Off"
c
        if ((i1 .le. i2) .and.
     +      (cdline(i1:i2) .eq. 'Off')) then
c
c         "Off" selected.
c
          iidata(1, 2)   = -1
          iidatawaset(2) = iidatawaset(2) + 1
c
        else
c
c         Looking for a valid number.
c
          call getnumber(.false., cdline, llen, i1, i2, 1, 1.d0,
     +                   ftmp1, jrc)
c
          if (jrc .le. 1) then
c
            itmp1 = ftmp1
            if (itmp1 .eq. ftmp1) then
              iidata(1, 2)   = itmp1
              iidatawaset(2) = iidatawaset(2) + 1
            else
              call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = 8
            endif
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = jrc
          endif
        endif
c
      else if (cdcode .eq. 618) then
c
c       Command code 618 corresponds to directive: FileDirectory
c
        if (i1 .le. i2) then
c
c         Determining the type of directory to set.
c
          itmp1 = wordlocate(cdline(i1:i2),
     +            'Output Global Export Scratch All', ' ', 1)
c
          if ((itmp1 .gt. 0) .and. (itmp1 .le. (mxwdir + 1))) then
c
c           Searching the directory string.
c
            call nextword(cdline, llen, i1, i2)
c
            if (i1 .le. i2) then
c
c             Setting the corresponding directory(s).
c
              itmp2 = i2 - i1 + 1
              itmp3 = min(itmp2, 94)
c
              if (itmp1 .le. mxwdir) then
c
                wdirname(itmp1)    = cdline(i1:i2)
                wdirnamelen(itmp1) = itmp3
c
              else
c
                do itmp1 = 1, mxwdir - 1
                  wdirname(itmp1)    = cdline(i1:i2)
                  wdirnamelen(itmp1) = itmp3
                enddo
                itmp1 = 1
c
              endif
c
              if (itmp2 .gt. 94) then
                call errprint(0, '$T15', 2, 'commandparse', ' ',
     +                        0, 0, 0, 0.d0,
     +                        wdirname(itmp1)(1:wdirnamelen(itmp1)))
                irc = 2
              endif
c
            else
              call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                      cdline, 0, 0, 0, 0.d0, ' ')
              irc = 6
            endif
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline, 0, 0, 0, 0.d0, ' ')
            irc = 6
          endif
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline, 0, 0, 0, 0.d0, ' ')
          irc = 6
        endif
c
      else if (cdcode .eq. 620) then
c
c       Command code 620 corresponds to directive: ObservingLevels
c
c       Reading number of observing levels.
c
c       Cannot convert from altitude to depth (or vice-versa) due to
c       the fact that the atmospheric model is still undefined.
c       Quantities pending conversion will be marked negative
c       and processed later.
c
        call getnumber(.false., cdline, llen, i1, i2,
     +                 0, 0.d0, ftmp1, irc)
c
        itmp1 = abs(ftmp1)
        if ((irc .eq. 0) .and. (itmp1 .eq. ftmp1) .and.
     +      (itmp1 .ge. 4)) then
c
c         A positive integer was specified.
c
          if (itmp1 .le. mxobslevels) then
c
c           The number of specified observing levels is valid.
c
c           Looking if there are specifications for the
c           lowest and highest levels.
c
c           Reading first number and unit (presumably length unit,
c           but the depth unit "g/cm2" is also valid).
c
            call nplusunit(l2meters, .true., cdline, llen, i1, i2,
     +                     1, altkeys(3), altklen(3), .true., 0.0d0,
     +                     ftmp1, itmp6, auxstring, itmp2, jrc)
c
            if (jrc .eq. 0) then
c
c             Valid length unit: An altitude was specified.
c
              ltmp1      = .true.
              obszset(1) = .true.
c
            else if (jrc .eq. 2) then
c
c             Valid depth specification.
c
              ltmp1      = (ftmp1 .ge. 0)
              obszset(1) = .false.
c
              if (.not. ltmp1) then
                call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                        cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                irc = 4
              endif
c
            else if (jrc .eq. 4) then
c
c            No specification. Just "ObservingLevels <number>"
c
             obslevset     = obslevset + 1
             iobslevdat(1) = - itmp1
             ltmp1         = .false.
c
            else
              call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              ltmp1 = .false.
              irc   = jrc
            endif
c
            if (ltmp1) then
c
c             Reading second number and unit (presumably length unit,
c             but the depth unit "g/cm2" is also valid).
c
              call nplusunit(l2meters, .true., cdline, llen, i1, i2,
     +                       1, altkeys(3), altklen(3), .true., 0.0d0,
     +                       ftmp2, itmp6, auxstring, itmp2, jrc)
c
              if (jrc .eq. 0) then
c
c               Valid length unit: An altitude was specified.
c
                ltmp1      = .true.
                obszset(2) = .true.
c
              else if (jrc .le. 2) then
c
c               Missing unit or valid depth specification.
c
                ltmp1      = (ftmp2 .ge. 0)
                obszset(2) = .false.
c
                if (ltmp1) then
                  if (jrc .eq. 1) then
                    call errprint(0, '$A24', 2, 'commandparse',
     +                            cdline(1:llen),
     +                            0, 0, 0, 0.d0, ' ')
                    irc = jrc
                  endif
                else
                  call errprint(0, '$A20', idlerrsev,
     +                          'commandparse', cdline(1:llen),
     +                          0, 0, 0, 0.d0, ' ')
                  irc = 4
                endif
c
              else
                call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                        cdline(1:llen), 0, 0, 0, 0.d0, ' ')
                ltmp1 = .false.
                irc = jrc
              endif
c
              if (ltmp1) then
c
c               Everything seems OK. Assigning the observing levels
c               data.
c
                obslevset        = obslevset + 1
                iobslevdat(1)    = itmp1
                fobslevdat(1, 1) = ftmp1
                fobslevdat(1, 2) = ftmp2
c
              endif
            endif
c
          else
            call errprint(0, '$A40', idlerrsev, 'commandparse',
     +                    cdline(1:llen),
     +                    1, mxobslevels, 0, 0.d0, ' ')
            irc = 8
          endif
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 10
        endif
c
      else if (cdcode .eq. 622) then
c
c       Command code 622 corresponds to directive: ELimsTables
c
c       Reading first number and unit (presumably energy unit,
c       but the keyword "Relative" is also valid).
c
        call nplusunit(e2gev, .false., cdline, llen, i1, i2,
     +                 1, altkeys(1), altklen(1), .false., 0.0d0,
     +                 ftmp1, itmp6, auxstring, itmp2, jrc)
c
        ltmp1 = (ftmp1 .gt. 0)
c
        if (jrc .eq. 2) then
c
c         Valid relative specification.
c
          ftmp1 = -ftmp1
c
        else if (jrc .ne. 0) then
          ltmp1 = .false.
        endif
c
        if (ltmp1) then
c
c         Reading second number and unit (presumably energy unit,
c         but the keyword "Relative" is also valid).
c
          call nplusunit(e2gev, .true., cdline, llen, i1, i2,
     +                   1, altkeys(1), altklen(1), .true., 0.0d0,
     +                   ftmp2, itmp6, auxstring, itmp2, jrc)
c
          ltmp1 = (ftmp2 .gt. 0)
c
          if (jrc .eq. 2) then
c
c           Valid relative specification.
c
            ftmp2 = -ftmp2
c
          else if (jrc .eq. 1) then
            call errprint(0, '$A22', 2, 'commandparse', cdline(1:llen),
     +                    0, 0, 0, 0.d0, ' ')
            irc = jrc
c
          else if (jrc .gt. 2) then
            ltmp1 = .false.
          endif
c
          if (ltmp1) then
c
c           Everything is OK. Assigning the corresponding
c           energies.
c
            fidatawaset(12) = fidatawaset(12) + 1
            fidata(1, 12)   = ftmp1
            fidata(1, 13)   = ftmp2
c
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 4
          endif
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 8
        endif
c
      else if (cdcode .eq. 624) then
c
c       Command code 624 corresponds to directive: RLimsTables
c
c       Reading first number and unit (length unit).
c
        call nplusunit(l2meters, .false., cdline, llen, i1, i2,
     +                 0, altkeys, altklen, .false.,
     +                 5.0d1, ftmp1, itmp6, auxstring, itmp1, irc)
c
        if ((irc .eq. 0) .and. (ftmp1 .gt. 0)) then
c
c         Reading second number and unit (length unit).
c
          call nplusunit(l2meters, .true., cdline, llen, i1, i2,
     +                   0, altkeys, altklen, .false.,
     +                   2.0d3, ftmp2, itmp6, auxstring, itmp1, jrc)
c
          ltmp1 = (ftmp2 .ge. 0)
c
          if (((jrc .le. 1) .or. (jrc .eq. 4)) .and.
     +        (ftmp2 .gt. 0)) then
c
c           Valid specification.
c
            if (jrc .eq. 1) then
              call errprint(0, '$A23', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
c           Checking minimum and maximum.
c
            if (ftmp1 .lt. ftmp2) then
c
c             Everything is OK. Assigning the corresponding
c             lengths.
c
              fidatawaset(14) = fidatawaset(14) + 1
              fidata(1, 14)   = ftmp1
              fidata(1, 15)   = ftmp2
c
            else
              call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = 10
            endif
c
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 8
        endif
c
      else if (cdcode .eq. 626) then
c
c       Command code 626 corresponds to directive: RLimsFiles
c
c       Setting the file number.
c
        call setfileno(cdline, llen, i1, i2, idlerrsev, itmp2, irc)
        if (irc .ne. 0) return
c
c       Reading first number and unit (length unit).
c
        call nplusunit(l2meters, .true., cdline, llen, i1, i2,
     +                 0, altkeys, altklen, .false.,
     +                 1.0d2, ftmp1, itmp6, auxstring, itmp1, irc)
c
        if ((irc .eq. 0) .and. (ftmp1 .ge. 0)) then
c
c         Reading second number and unit (length unit).
c
          call nplusunit(l2meters, .true., cdline, llen, i1, i2,
     +                   0, altkeys, altklen, .false.,
     +                   1.2d4, ftmp2, itmp6, auxstring, itmp1, jrc)
c
          if (((jrc .le. 1) .or. (jrc .eq. 4)) .and.
     +        (ftmp2 .gt. 0)) then
c
c           Valid specification.
c
            if (jrc .eq. 1) then
              call errprint(0, '$A23', 2, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = jrc
            endif
c
c           Checking minimum and maximum.
c
            if (ftmp1 .lt. ftmp2) then
c
c             Everything is OK. Assigning the corresponding
c             distances.
c
              itmp2                = 14 + 2 * itmp2
              fidatawaset(itmp2)   = fidatawaset(itmp2) + 1
              fidata(1, itmp2)     = ftmp1
              fidata(1, itmp2 + 1) = ftmp2
c
            else
              call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                      cdline(1:llen), 0, 0, 0, 0.d0, ' ')
              irc = 10
            endif
c
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            irc = 8
          endif
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = 8
        endif
c
      else if (cdcode .eq. 628) then
c
c       Command code 628 corresponds to directive: StackInformation
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, stackinfo, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 630) then
c
c       Command code 630 corresponds to directive: PrintTables
c
        call tableprexp(cdline, llen, i1, i2, 1, ntableprt,
     +                  tableprt, tprtopt, irc)
c
      else if (cdcode .eq. 632) then
c
c       Command code 632 corresponds to directive: ExportTables
c
        call tableprexp(cdline, llen, i1, i2, 2, ntableexp,
     +                  tableexp, texpopt, irc)
c
      else if (cdcode .eq. 633) then
c
c       Command code 633 corresponds to directive: ExportPerShower
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, exportpershower, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 634) then
c
c       Command code 634 corresponds to directive: TableIndex
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, tableindex, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 636) then
c
c       Command code 636 corresponds to directive: Summary
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, sryison, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 637) then
c
c       Command code 636 corresponds to directive: TSSFile
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, tssison, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 638) then
c
c       Command code 638 corresponds to directive: LaTeX
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, latexsry, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else if (cdcode .eq. 640) then
c
c       Command code 640 corresponds to directive: CommentCharacter
c
c       This command allows to change the default comment character.
c       If the length of its parameter is one, then the parameter
c       is taken as the comment. Otherwise it is interpreted as
c       an ascii code (ranging from 0 to 255). This permits
c       specifying '#' as comment.
c
        call getsgchar(.false., cdline, llen, i1, i2,
     +                 0, 0, itmp1, irc)
c
        if (irc .eq. 0) then
          icommentchar = itmp1
        else
          call errprint(0, '$A12', idlerrsev, 'commandparse', ' ',
     +                  0, 0, 0, 0.d0, cdline(1:llen))
        endif
c
      else if (cdcode .eq. 642) then
c
c       Command code 642 corresponds to directive: ADFile
c
        call setdynswitch(.false., cdline, llen, i1, i2,
     +                    1, .true., idlerrsev, adfile, jrc)
        if (jrc .gt. 1) irc = jrc
c
      else
c
c       No more commands for this section.
c       This point should never be reached.
c
        call errprint(0, '$A08', 4, 'commandparse', '(In section 4)',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
      return
c
c     SECTION 5: Real, integer and logical parameters, "dynamically"
c                defined.
c
51000 continue
c
c     Real parameters.
c
      call getnumber(.false., cdline, llen, i1, i2, 0, 0.d0,
     +               ftmp1, irc)
c
      if (irc .eq. 0) then
c
c       A valid number was specified. Checking boundaries.
c
        call bdrycheck(fdbdry(iditem), fidata(2, iditem), ftmp1, irc)
        if (irc .eq. 0) then
          fidata(1, iditem)   = ftmp1
          fidatawaset(iditem) = fidatawaset(iditem) + 1
          if (wngonset(cdindex)) then
            call errprint(0, '$DS1', 2, 'commandparse',
     +                    clgname(cdindex),
     +                    0, 0, 2, fidata(0, iditem), ' ')
          endif
        else
          call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        endif
c
      else
        call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
      endif
      return
c
52000 continue
c
c     Quantities measured in length units.
c
      call nplusunit(l2meters, .false., cdline, llen, i1, i2,
     +               0, altkeys, altklen, .false.,
     +               0.0d0, ftmp1, itmp6, auxstring, itmp1, jrc)
c
      if (jrc .le. 3) then
c
        if (jrc .ne. 0) then
          call errprint(0, '$A23', 2, 'commandparse',
     +                  cdline(1:llen),
     +                  0, 0, 0, 0.d0, auxstring(1:itmp1))
          irc = jrc
        endif
c
c       A valid length was specified. Checking boundaries.
c
        call bdrycheck(fdbdry(iditem), fidata(2, iditem), ftmp1, jrc)
        if (jrc .eq. 0) then
          fidata(1, iditem)   = ftmp1
          fidatawaset(iditem) = fidatawaset(iditem) + 1
          if (wngonset(cdindex)) then
            call errprint(0, '$DS1', 2, 'commandparse',
     +                    clgname(cdindex),
     +                    0, 0, 2, fidata(0, iditem), ' ')
          endif
        else
          call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else
        call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        irc = jrc
      endif
      return
c
53000 continue
c
c     Quantities measured in time units.
c
      call nplusunit(t2sec, .false., cdline, llen, i1, i2,
     +               0, altkeys, altklen, .false.,
     +               0.0d0, ftmp1, itmp6, auxstring, itmp1, jrc)
c
      if (jrc .le. 3) then
c
        if (jrc .ne. 0) then
          call errprint(0, '$A21', 2, 'commandparse',
     +                  cdline(1:llen),
     +                  0, 0, 0, 0.d0, auxstring(1:itmp1))
          irc = jrc
        endif
c
c       A valid time was specified. Checking boundaries.
c
        call bdrycheck(fdbdry(iditem), fidata(2, iditem), ftmp1, jrc)
        if (jrc .eq. 0) then
          fidata(1, iditem)   = ftmp1
          fidatawaset(iditem) = fidatawaset(iditem) + 1
          if (wngonset(cdindex)) then
            call errprint(0, '$DS1', 2, 'commandparse',
     +                    clgname(cdindex),
     +                    0, 0, 2, fidata(0, iditem), ' ')
          endif
        else
          call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else
        call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        irc = jrc
      endif
      return
c
54000 continue
c
c     Quantities measured in energy units.
c
      call nplusunit(e2gev, .false., cdline, llen, i1, i2,
     +               0, altkeys, altklen, .false.,
     +               0.0d0, ftmp1, itmp6, auxstring, itmp1, jrc)
c
      if (jrc .le. 3) then
c
        if (jrc .ne. 0) then
          call errprint(0, '$A22', 2, 'commandparse',
     +                  cdline(1:llen),
     +                  0, 0, 0, 0.d0, auxstring(1:itmp1))
          irc = jrc
        endif
c
c       A valid energy was specified. Checking boundaries.
c
        call bdrycheck(fdbdry(iditem), fidata(2, iditem), ftmp1, jrc)
        if (jrc .eq. 0) then
          fidata(1, iditem)   = ftmp1
          fidatawaset(iditem) = fidatawaset(iditem) + 1
          if (wngonset(cdindex)) then
            call errprint(0, '$DS1', 2, 'commandparse',
     +                    clgname(cdindex),
     +                    0, 0, 2, fidata(0, iditem), ' ')
          endif
        else
          call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
          irc = jrc
        endif
c
      else
        call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        irc = jrc
      endif
      return
c
55000 continue
c
c     Integer parameters.
c
      call getnumber(.false., cdline, llen, i1, i2, 0, 0.d0,
     +               ftmp1, irc)
c
      if (irc .eq. 0) then
c
c       A valid number was specified. Checking boundaries.
c
        call ibdrycheck(idbdry(iditem), iidata(2, iditem), ftmp1,
     +                  itmp1, irc)
        if (irc .eq. 0) then
          iidata(1, iditem)   = itmp1
          iidatawaset(iditem) = iidatawaset(iditem) + 1
          if (wngonset(cdindex)) then
            call errprint(0, '$DS1', 2, 'commandparse',
     +                    clgname(cdindex),
     +                    2, iidata(0, iditem), 0, 0.d0, ' ')
          endif
        else
          call errprint(0, '$A20', idlerrsev, 'commandparse',
     +                  cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        endif
c
      else
        call errprint(0, '$A06', idlerrsev, 'commandparse',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
      endif
      return
c
56000 continue
c
c     Logical switch.
c
      call getswitch(.false., cdline, llen, i1, i2,
     +               1, lidata(2, iditem), ltmp1, itmp1)
c
      if (itmp1 .le. 1) then
c
c       Valid switch specification. Assigning.
c
        lidata(1, iditem)   = ltmp1
        lidatawaset(iditem) = lidatawaset(iditem) + 1
        if (wngonset(cdindex)) then
          call errprint(0, '$DS1', 2, 'commandparse',
     +                    clgname(cdindex), 0, 0, 0, 0.d0, ' ')
        endif
c
      else
        call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                cdline(1:llen), 0, 0, 0, 0.d0, cdline(i1:i2))
        irc = 2
      endif
      return
c
57000 continue
c
c     Character string variables.
c
      if (i1 .le. i2) then
c
c       Nonull string specification.
c
        if (i2 .ge. llen) then
c
c         Valid character string specification. Attempting to assign.
c
          itmp1 = wordlocate(cdline(i1:i2),
     +                       sidatastring(sidata(4, iditem):
     +                                    sidata(5, iditem)),
     +                       sidatastring(sidata(3, iditem):
     +                                    sidata(3, iditem)),
     +                       sidata(6, iditem))
c
          if (itmp1 .gt. 0) then
            ltmp1 = .true.
          else
            call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                    cdline(1:llen), 0, 0, 0, 0.d0, ' ')
            ltmp1 = .false.
          endif
c
        else
          call errprint(0, '$A10', idlerrsev, 'commandparse',
     +                  ' ', 0, 0, 0, 0.d0, cdline(1:llen))
          ltmp1 = .false.
        endif
c
      else if (sidata(2, iditem) .gt. 0) then
c
c       Null specification. Using the default.
c
        itmp1 = sidata(2, iditem)
        ltmp1 = .true.
c
      else
        call errprint(0, '$A12', idlerrsev, 'commandparse',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        ltmp1 = .false.
      endif
c
      if (ltmp1) then
        sidatawaset(iditem) = sidatawaset(iditem) + 1
        sidata(1, iditem)   = itmp1
        if (wngonset(cdindex)) then
          call errprint(0, '$DS1', 2, 'commandparse',
     +                  clgname(cdindex), 0, 0, 0, 0.d0, ' ')
        endif
      else
        irc   = 2
      endif
      return
c
c     ERROR MESSAGES.
c
 3020 continue
      call errprint(0, '$A14', idlerrsev, 'commandparse',
     +              cdline(i1:i2), 0, 0, 0, 0.d0, ' ')
      irc = 12
      return
c
      end
c     --- End of routine commandparse.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getiline(igblank, llenmax, line, llen)
c
c     Reading a data line from the input file and eliminating the
c     commented out areas, and all leading and trailing blanks or
c     special characters.
c     Only the first llenmax characters of each line are processed.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001, 2003;
c                                Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     igblank......... (input, logical) True if blank lines must
c                      be ignored. False if such lines need to be
c                      returned as valid ones; in this case also
c                      leading blanks are kept.
c     llenmax......... (input, integer) Maximum length of parameter
c                      line.
c     line............ (output, character*(*)) The line read,
c                      without commented out areas.
c     llen............ (output, integer) Position of the last nonblank
c                      character.
c
c     Return value: (logical) True if a line was successfully read.
c     ============  False otherwise. False isused to indicate end
c                   of file.
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
      logical           getiline
      logical           igblank
      integer           llenmax, llen
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, ilast, jlast, ilast0
      character*32      gvname
      integer           inest, nbset, oppos, nrep
      integer           clen, oldclen, clendif
      character*1       sch
      integer           mxbrset
      parameter         (mxbrset = 50)
      integer           rpbeg(mxbrset)
      integer           bsbeg(mxbrset)
      integer           bsend(mxbrset)
      logical           bsnor(mxbrset)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scanning the input file in search of a valid line.
c
 1010 continue
c
      if (iprompt) write(6, 2005) 'Aires IDL> '
 2005 format(a, $)
      read(inpunit, 2010, err = 3010, end = 1100) line
 2010 format(a)
      nilines(inpnesl) = nilines(inpnesl) + 1
c
c     Cleaning out comment marks and/or trailing blanks.
c
      call scleancomm(llenmax, line, ilast)
c
c     If the line is blank and blank lines must be ignored, then a new
c     line must be read in.
c
      if (ilast .le. 0) then
        if (igblank) then
          goto 1010
        else
          goto 1090
        endif
      endif
c
c     Typing out the command line if Trace is On.
c
      if (traceison) write(6, 2040) inpnesl, nilines(inpnesl),
     +                              line(1:ilast)
 2040 format(i2, ':', i4.4, 1x, a)
c
c     Performing global variable substitution, if necessary.
c
      if (thereareglobars .and. bracketson) then
c
        longauxline = line(1:ilast)
        ilast0      = ilast
c
        do inest = 1, 100
c
          nbset  = 0
          oppos = -1
c
          do i = 1, ilast
            sch = longauxline(i:i)
            if (sch .eq. opbr) then
              oppos = i
            else if (sch .eq. clbr) then
              if (oppos .gt. 0) then
                if (nbset .ge. mxbrset) goto 1056
                nbset = nbset + 1
                bsbeg(nbset) = oppos
                rpbeg(nbset) = oppos
                bsend(nbset) = i
                if (oppos .gt. 1) then
                  j = oppos - 1
                  if (longauxline(j:j) .ne. brec) then
                    bsnor(nbset) = .true.
                  else if (j .gt. 1) then
                    bsnor(nbset) = (longauxline(j-1:j-1) .eq. brec)
                    rpbeg(nbset) = j
                  else
                    bsnor(nbset) = .false.
                  endif
                else
                  bsnor(nbset) = .true.
                endif
              endif
              oppos = -1
            endif
          enddo
c
          nrep = 0
c
          do i = 1, nbset
            if (bsnor(i) .and. (bsend(i) .gt. (bsbeg(i) + 1))) then
              gvname = longauxline(bsbeg(i)+1:bsend(i)-1)
              call getglobal(longauxline(bsbeg(i)+1:bsend(i)-1),
     +                       j, auxline, clen)
              if (clen .ge. 0) then
c
                nrep    = nrep + 1
                oldclen = bsend(i) - rpbeg(i) + 1
                clendif = clen - oldclen
c
                if (clendif .lt. 0) then
                  j = rpbeg(i) + clen - 1
                  do k = bsend(i) + 1, ilast
                    j = j + 1
                    longauxline(j:j) = longauxline(k:k)
                  enddo
                  ilast = j
                else if (clendif .gt. 0) then
                  jlast = ilast + clendif
                  if (jlast .gt. 512) then
                    call errprint(0, '*', idlerrsev, 'getiline',
     +                   'Generated input line is too long. Ignoring.',
     +                   0, 0, 0, 0.d0, line(1:ilast0))
                    goto 1010
                  endif
                  j     = jlast
                  do k = ilast, bsend(i) + 1, -1
                    longauxline(j:j) = longauxline(k:k)
                    j = j - 1
                  enddo
                  ilast = jlast
                endif
c
                bsend(i) = bsend(i) + clendif
                do j = i + 1, nbset
                  bsbeg(j) = bsbeg(j) + clendif
                  rpbeg(j) = rpbeg(j) + clendif
                  bsend(j) = bsend(j) + clendif
                enddo
c
c               Replacing with the replacement string.
c
                if (clen .gt. 0) then
                  longauxline(rpbeg(i):bsend(i)) = auxline(1:clen)
                endif
              endif
            endif
          enddo
c
          if (nrep .gt. 0) then
c
c           Typing out the command line if Trace is On.
c
            if (traceison) write(6, 2050) longauxline(1:ilast)
c
          else
c
c           No more replacements. Changing "escaped" brackets.
c
            nrep = 0
            do i = 1, nbset
              if (.not. bsnor(i)) then
                nrep = nrep + 1
                do k = bsbeg(i), ilast
                  j         = k - 1
                  longauxline(j:j) = longauxline(k:k)
                enddo
                ilast   = j
c
                do j = i, nbset
                  bsbeg(j) = bsbeg(j) - 1
                  bsend(j) = bsend(j) - 1
                enddo
              endif
            enddo
c
c           Typing out the command line if Trace is On.
c
            if (traceison .and. (nrep .gt. 0))
     +        write(6, 2050) longauxline(1:ilast)
c
            line = longauxline(1:ilast)
            goto 1060
c
          endif
 2050     format(3x, '--->  ', a)
c
        enddo
c
c       Too many replacements... Seems like an infinite loop.
c
 1056   continue
        call errprint(0, '*', 4, 'getiline',
     +       'Too many global variable replacements in input line.',
     +       0, 0, 0, 0.d0, line(1:ilast0))
c
 1060   continue
c
c       The line can have length 0 after replacing. In this case we
c       need to read in another line if necessary.
c
        if (ilast .le. 0) then
          if (igblank) then
            goto 1010
          else
            goto 1090
          endif
        endif
c
      endif
c
      llen     = min(ilast, llenmax)
      getiline = .true.
c
      return
c
 1090 continue
c
c     Have read a blank line, and must return it as a valid input line.
c
      if (traceison) write(6, 2040) inpnesl, nilines(inpnesl)
      llen     = 0
      getiline = .true.
      return
c
 1100 continue
c
c     End of file reached.
c
      llen     = 0
      getiline = .false.
      return
c
 3010 continue
c
c     Error reading input file.
c
      call errprint(0, irerr, idlerrsev, 'getiline',
     .              ' ', 0, 0, 0, 0.d0, ' ')
c
      llen     = 0
      getiline = .false.
      return
c
      end
c     --- End of routine getiline.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function searchlabel(label, rmksw)
c
c     Reading data lines from the input file until the given label
c     is found.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     label........... (input, character*(*)) The label. It is
c                      assumed that it starts with the label mark
c                      character.
c     rmksw........... (input, logical) .true. if the skipped lines
c                      must be copied into the remarks file.
c
c     Return value: (integer) 0 if the label is found. -5 if the
c     ============  EOF condition is raised.
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
      integer           searchlabel
      character*(*)     label
      logical           rmksw
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*256     line
      integer           ifirst, ilast, clen
      logical           sksw, goon, traceisonsave
      logical           getiline
c
c     FIRST EXECUTABLE STATEMENT
c
      sksw          = (.not. rmksw)
      traceisonsave = traceison
      if (sksw) traceison = .false.
c
c     Scanning the input file in search of the given label.
c
 1010 continue
      if (.not. getiline(.false., 176, line, clen)) goto 1100
c
c     Searching for a label mark.
c
      if (clen .gt. 0) then
        ilast = 0
        call nextword(line, clen, ifirst, ilast)
        if (line(ifirst:ifirst) .eq. labelchar) goto 1020
      endif
c
c     Non label line. Checking if it is necessary to copy it
c     into the remarks file.
c
      if (rmksw) call rmkcopy(line, clen)
c
      goto 1010
c
 1020 continue
c
c     A label mark was found. Checking the entire label.
c
c     Null labels ('&') are not allowed, but they are silently
c     skipped.
c
      if (ilast .lt. ifirst) then
        goon = .true.
      else
        goon = (label .ne. line(ifirst:ilast))
      endif
c
      if (goon) then
c
c       The searched label is not in this line. Deciding if it
c       is necessary to copy it into the remarks file and
c       continuing to read the next line.
c
        if (rmksw) call rmkcopy(line, clen)
        goto 1010
c
      else
c
c       The label was found. Returning.
c
        if (traceisonsave .and. sksw)
     +    write(6, 2020) inpnesl, nilines(inpnesl),
     +                   line(ifirst:ilast)
 2020   format(i2, ':', i4.4, 1x, a)
c
        searchlabel = 0
        traceison   = traceisonsave
        return
c
      endif
c
 1100 continue
c
c     End of file reached.
c
      searchlabel = -5
      traceison   = traceisonsave
      return
c
 3010 continue
c
c     Error reading input file.
c
      call errprint(0, irerr, idlerrsev, 'searchlabel',
     .              ' ', 0, 0, 0, 0.d0, ' ')
c
      searchlabel = -8
      return
c
      end
c     --- End of routine searchlabel
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine rmkcopy(line, ilast0)
c
c     Appending a line into the remarks file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     line............ (input, character*(*)) The line to append.
c     ilast0.......... (input, integer) Last position of string "line"
c                      to scan.
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
      character*(*)     line
      integer           ilast0
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, ilast
      logical           nonblank
c
c     FIRST EXECUTABLE STATEMENT
c
      if (.not. remark) then
c
c       Opening the remarks file.
c
        open(rmkut, file = rmkfn, status = 'UNKNOWN',
     +              form = 'UNFORMATTED', err = 3010)
        remark = .true.
c
      endif
c
c     Copying the line (without trailing blanks or specials).
c     Lines are truncated to 75 characters (maximum).
c
      do i = min(ilast0, 75), 1, -1
        ilast = i
        if (nonblank(line(i:i))) goto 1010
      enddo
      ilast = 0
 1010 continue
c
      write(rmkut) ilast
      if (ilast .gt. 0) write(rmkut) line(1:ilast)
c
      return
c
c     ERROR MESSAGES.
c
 3010 continue
      call errprint(0, '$A09', 4, 'rmkcopy',
     .              ' ', 1, rmkut, 0, 0.d0, ' ')
      return
      end
c     --- End of routine rmkcopy
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setdynswitch(advance, cdline, llen, i1, i2,
     +                        usedefault, default, errsev,
     +                        dswitch, irc)
c
c     Setting a logical dynamical switch.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     advance......... (input, logical) Switch to decide whether or
c                      not invoke routine "nextword" to extract the
c                      first word from "string".
c     cdline.......... (input, character*(*)) The string to scan
c     llen............ (input, integer) String length, or last
c                      position to scan.
c     i1.............. (input-output, integer) Position of the first
c                      character of the given or to be localized word.
c     i2.............. (input-output, integer) Position of the last
c                      character of the given or to be localized word.
c     usedefault...... (input, integer) Switch to decide if a default
c                      value must be assigned when the input word
c                      is not given (empty string) or is invalid.
c                      0 means do not use default value. 1 use default
c                      only for missing specification. 2 use the
c                      default even in the case of an invalid
c                      specification.
c     default......... (input, logical) Default value to use. This
c                      argument is not used if "usedefault" is zero.
c     errsev.......... (input, integer) Error message severity.
c     dswitch......... (output, logical) The dynamical switch to set.
c     irc............. (output, integer) Return code. 0 means that a
c                      valid switch was specified and processed.
c                      1 means that no switch was specified and
c                      the default value was used. 2 means that no
c                      switch was specified and usedefault was zero.
c                      3 means that an invalid switch was specified
c                      and the default value was used to set the
c                      switch (usedefault >= 2). 4 is like 3 but in
c                      the case usedefault < 2.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           advance
      character*(*)     cdline
      integer           llen, i1, i2
      integer           usedefault, errsev, irc
      logical           default, dswitch
c
c     Declaration of internal variables and arrays.
c
      logical           ltmp1
c
c     FIRST EXECUTABLE STATEMENT
c
        call getswitch(advance, cdline, llen, i1, i2,
     +                 usedefault, default, ltmp1, irc)
c
        if ((irc .le. 1) .or. (usedefault .gt. 1)) then
          dswitch = ltmp1
        else
          call errprint(0, '$A12', errsev, 'setdynswitch',
     +                  cdline(1:llen),
     +                  0, 0, 0, 0.d0, cdline(i1:i2))
        endif
      return
c
      end
c     --- End of routine setdynswitch.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine tableprexp(cdline, llen, i1, i2, iprtexp,
     +                      ntables, tables, options, irc)
c
c     Setting TablePrint/TableExport options.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998, 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The string to scan
c     llen............ (input, integer) String length, or last
c                      position to scan.
c     i1.............. (input-output, integer) Position of the first
c                      character of the given or to be localized word.
c     i2.............. (input-output, integer) Position of the last
c                      character of the given or to be localized word.
c     iprtexp......... (input, integer) 1 for Print 2 for Export.
c     ntables......... (input-output, integer) Number of defined
c                      tables.
c     tables.......... (input-output, integer, array(mxprtexp)) Table
c                      array.
c     options......... (input-output, integer, array(mxprtexp)) Table
c                      options array.
c     irc............. (output, integer) Return code.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      character*(*)     cdline
      integer           llen, i1, i2, iprtexp
      integer           ntables
      integer           tables(mxprtexp)
      integer           options(mxprtexp)
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, l, itmp1, itmp2
      double precision  ftmp1
      integer           codopt, table1, table2
      character*8       opkey
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the "Clear" option.
c
      if (i1 .le. i2) then
        opkey = 'Clear'
        i     = max(2, min(8, i2 - i1 + 1))
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c         "Clear" option: setting ntables to zero.
c
          ntables = 0
          irc     = 0
          return
c
        endif
      endif
c
      irc = 5
c
c     Getting the first table code.
c
      call getnumber(.false., cdline, llen, i1, i2,
     +               0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
        itmp2 = ftmp1
        if ((itmp2 .eq. ftmp1) .and. (itmp2 .gt. 0)) then
          table1 = itmp2
        else
          call errprint(0, '$A20', idlerrsev, 'tableprexp',
     +                  cdline(1:llen),
     +                  1, itmp2, 0, 0.d0, ' ')
          return
        endif
c
c       Searching the second code.
c
        call getnumber(.true., cdline, llen, i1, i2,
     +                 0, 0.0d0, ftmp1, itmp1)
c
        if (itmp1 .eq. 0) then
          itmp2 = ftmp1
          if ((itmp2 .eq. ftmp1) .and. (itmp2 .gt. 0)) then
            table2 = itmp2
          else
            call errprint(0, '$A20', idlerrsev, 'tableprexp',
     +                    cdline(1:llen),
     +                    1, itmp2, 0, 0.d0, ' ')
            return
          endif
c
          call nextword(cdline, llen, i1, i2)
c
        else
c
c         No second number o no numeric specification.
c
          table2 = table1
c
        endif
c
c       Searching options.
c
        if (i1 .le. i2) then
c
          opkey = 'Options'
          itmp1 = min(7, i2 - i1 + 1)
c
          if (opkey(1:itmp1) .eq. cdline(i1:i2)) then
c
c           There are specified options.
c
            call nextword(cdline, llen, i1, i2)
            if (i1 .le. i2) then
              l = i2 - i1 + 1
            else
              l = 0
            endif
c
          else
            call errprint(0, '$A10', idlerrsev, 'tableprexp', ' ',
     +                    0, 0, 0, 0.d0, cdline(1:llen))
            return
          endif
        else
          l = 0
        endif
c
c       Encoding the options.
c
        call petopts(iprtexp, cdline(i1:i2), l, codopt, irc)
c
        if (irc .ne. 0) then
c
c         Unknown option.
c
          i = i1 + codopt - 1
          call errprint(0, '$A12', idlerrsev, 'tableprexp',
     +                  cdline(1:llen), 0, 0, 0, 0.d0,
     +                  cdline(i:i))
          return
c
        endif
c
c       Table code range and options are now known. Setting.
c
        itmp1  = min(table1, table2)
        table2 = max(table1, table2)
        table1 = itmp1
        itmp1  = ntables
c
        do i = 1, nlhtables
          if ((lhcoden(i) .ge. table1) .and.
     +        (lhcoden(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = lhcoden(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nlhtables
          if ((wlhcoden(i) .ge. table1) .and.
     +        (wlhcoden(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = wlhcoden(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nlhtables
          if ((lhcodee(i) .ge. table1) .and.
     +        (lhcodee(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = lhcodee(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nldtables
          if ((ldcoden(i) .ge. table1) .and.
     +        (ldcoden(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = ldcoden(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nldtables
          if ((wldcoden(i) .ge. table1) .and.
     +        (wldcoden(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = wldcoden(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nldtables
          if ((ldcodee(i) .ge. table1) .and.
     +        (ldcodee(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = ldcodee(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nldtables
          if ((wldcodee(i) .ge. table1) .and.
     +        (wldcodee(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = wldcodee(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, ntdtables
          if ((tdcoden(i) .ge. table1) .and.
     +        (tdcoden(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = tdcoden(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, npshtables
          if ((pshcode(i) .ge. table1) .and.
     +        (pshcode(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = pshcode(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nlitables
          if ((llcoden(i) .ge. table1) .and.
     +        (llcoden(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = llcoden(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nlitables
          if ((wllcoden(i) .ge. table1) .and.
     +        (wllcoden(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = wllcoden(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nlitables
          if ((llcodee(i) .ge. table1) .and.
     +        (llcodee(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = llcodee(i)
            options(ntables) = codopt
          endif
        enddo
        do i = 1, nlitables
          if ((licodee(i) .ge. table1) .and.
     +        (licodee(i) .le. table2)) then
            if (ntables .eq. mxprtexp) goto 3010
            ntables          = ntables + 1
            tables(ntables)  = licodee(i)
            options(ntables) = codopt
          endif
        enddo
c
        if (ntables .eq. itmp1) then
          call errprint(0, '*', 2, 'tableprexp',
     +    'No table(s) selected in TablePrint/Export directive.$' //
     +    'Check table codes available.',
     +    0, 0, 0, 0.d0, cdline(1:llen))
          irc = 2
        else
          irc = 0
        endif
c
      else
        call errprint(0, '$A10', idlerrsev, 'tableprexp', ' ',
     +                0, 0, 0, 0.d0, cdline(1:llen))
        irc = itmp1
      endif
      return
c
c     Too many tables selected.
c
 3010 continue
      call errprint(0, '*', idlerrsev, 'tableprexp',
     + 'Too many table(s) selected in TablePrint/Export directive(s).',
     + 0, 0, 0, 0.d0, cdline(1:llen))
      return
c
      end
c     --- End of routine tableprexp.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine helprint(cdline, llen, ifirst, ilast)
c
c     Printing some help information.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 2000, 2001.
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The Help directive line.
c     llen............ (input, integer) Length of string cdline.
c     ifirst, ilast... (input, integer) First and last character
c                      of first command argument. If there is no
c                      argument, then ilast < ifirst.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Number of columns in directive listing.
c
      integer           idlcols
      parameter         (idlcols = 4)
c
c     Declaration of arguments.
c
      character*(*)     cdline
      integer           llen, ifirst, ilast
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, j4, jr
      character*(mxcdl) dirstring(idlcols)
      character*1       hidden(idlcols)
      integer           i4(0:idlcols)
      logical           mainhelp, alsonovid
      logical           tablehelp, sitehelp
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the parameter.
c
      if (ifirst .le. ilast) then
        alsonovid = (cdline(ifirst:ilast) .eq. '*') .or.
     +              (cdline(ifirst:ilast) .eq. 'All') .or.
     +              (cdline(ifirst:ilast) .eq. 'all')
        tablehelp = (cdline(ifirst:ilast) .eq. 'Table') .or.
     +              (cdline(ifirst:ilast) .eq. 'table') .or.
     +              (cdline(ifirst:ilast) .eq. 'Tables') .or.
     +              (cdline(ifirst:ilast) .eq. 'tables')
        sitehelp  = (cdline(ifirst:ilast) .eq. 'Site') .or.
     +              (cdline(ifirst:ilast) .eq. 'site') .or.
     +              (cdline(ifirst:ilast) .eq. 'Sites') .or.
     +              (cdline(ifirst:ilast) .eq. 'sites')
        mainhelp  = (.not.(tablehelp .or. sitehelp))
      else
        alsonovid = .false.
        tablehelp = .false.
        sitehelp  = .false.
        mainhelp  = .true.
      endif
c
      write(6, 2010)
 2010 format(9a)
c
      if (mainhelp) then
c
c       General help.
c
        write(6, 2010) 'DIRECTIVES AVAILABLE:'
        write(6, 2010)
c
c       Listing the available directives.
c
        j = 0
        do i = 1, ncommands
          if (alsonovid .or. veryimpdir(i)) then
            j = j + 1
            obslevscrai(j) = i
          endif
        enddo
c
c       Sorting the entries.
c
        call subsort(j, cname, 1, mxcdl, obslevscrai(1))
c
        j4 = j / idlcols
        jr = j - idlcols * j4
c
        i4(0) = 0
        do i = 1, idlcols
          i4(i) = j4
        enddo
        do i = 1, jr
          i4(i) = i4(i) + 1
        enddo
        do i = 1, idlcols
          i4(i) = i4(i) + i4(i - 1)
        enddo
c
        do l = 1, j4
c
          do j = 1, idlcols
            i = obslevscrai(i4(j - 1) + l)
            dirstring(j) = cname(i)
            if (veryimpdir(i)) then
              hidden(j) = ' '
            else
              hidden(j) = '.'
            endif
          enddo
c
          write(6, 2020) (hidden(k), dirstring(k), k = 1, 4)
c
        enddo
 2020   format(4(2x, 2a))
c
        if (jr .gt. 0) then
c
          j4 = j4 + 1
          do j = 1, jr
            i = obslevscrai(i4(j - 1) + j4)
            dirstring(j) = cname(i)
            if (veryimpdir(i)) then
              hidden(j) = ' '
            else
              hidden(j) = '.'
            endif
          enddo
c
          write(6, 2020) (hidden(k), dirstring(k), k = 1, jr)
        endif
c
c       Typing additional information.
c
        write(6, 2010)
        write(6, 2010)
     +    'Directive names are case sensitive and may be abbreviated.'
        write(6, 2010)
     +    'For more details see the documentation.'
        write(6, 2010)
c
      else if (tablehelp) then
c
c       Help on tables.
c
        write(6, 2010) 'OUTPUT DATA TABLES:'
        write(6, 2010)
        write(6, 2010) '       Code   Name'
        write(6, 2010)
c
c       Listing the available tables.
c
        do i = 1, nlhtables
          write(6, 2050) i, lhcoden(i), lhnamen(i)
        enddo
        j = nlhtables
        do i = 1, nlhtables
          j = j + 1
          write(6, 2050) j, wlhcoden(i), wlhnamen(i)
        enddo
        do i = 1, nlhtables
          j = j + 1
          write(6, 2050) j, lhcodee(i), lhnamee(i)
        enddo
        do i = 1, nldtables
          j = j + 1
          write(6, 2050) j, ldcoden(i), ldnamen(i)
        enddo
        do i = 1, nldtables
          j = j + 1
          write(6, 2050) j, wldcoden(i), wldnamen(i)
        enddo
        do i = 1, nldtables
          j = j + 1
          write(6, 2050) j, ldcodee(i), ldnamee(i)
        enddo
        do i = 1, nldtables
          j = j + 1
          write(6, 2050) j, wldcodee(i), wldnamee(i)
        enddo
        do i = 1, ntdtables
          j = j + 1
          write(6, 2050) j, tdcoden(i), tdnamen(i)
        enddo
        do i = 1, npshtables
          j = j + 1
          write(6, 2050) j, pshcode(i), pshname(i)
        enddo
        do i = 1, nlitables
          j = j + 1
          write(6, 2050) j, llcoden(i), llnamen(i)
        enddo
        do i = 1, nlitables
          j = j + 1
          write(6, 2050) j, wllcoden(i), wllnamen(i)
        enddo
        do i = 1, nlitables
          j = j + 1
          write(6, 2050) j, llcodee(i), llnamee(i)
        enddo
        do i = 1, nlitables
          j = j + 1
          write(6, 2050) j, licodee(i), linamee(i)
        enddo
 2050   format(1x, i3, 3x, i4.4, 3x, a)
c
        write(6, 2010)
        write(6, 2010) ' Options for the PrintTable directive: '
        write(6, 2010) '    nmM (min/max), RS (RMS/Std. dev.),',
     +                 ' rdlL (raw/density/dln/dlog10)'
        write(6, 2010)
        write(6, 2010) ' Options for the ExportTable directive: '
        write(6, 2010) '    sh (supress/header), UxX ("excess" bins),',
     +                 ' KMGTPE (energy unit)'
        write(6, 2010) '    radlL (raw/slant/density/dln/dlog10),'
        write(6, 2010) '    op (obs. levels horizontal/perp. to axis)'
        write(6, 2010)
c
      else
c
c       Help on sites.
c
        write(6, 2010) 'SITE DATA LIBRARY:'
        write(6, 2010)
        write(6, 2010)
     +  ' Name                 Latitude    Longitude      Altitude'
        write(6, 2010)
c
c       Listing the available sites.
c
        do i = 0, nlibsites
          if (siteground(i) .ge. 0) then
            call lnice(siteground(i), 0, auxline, j)
          else
            call fltnice(-siteground(i), 0, auxline, j)
            j = j + 6
            auxline(j-5:j) = ' g/cm2'
          endif
          write(6, 2070) sitename(i), sitelat(i), sitelong(i),
     +                   auxline(1:j)
        enddo
 2070   format(1x, a, 2x, 2(f9.2, ' deg'), 4x, a)
c
        write(6, 2010)
c
      endif
c
      return
c
      end
c     --- End of routine helprint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine saveinfile(saveornot, cdline, llen, i1, i2, irc)
c
c     Processing directives SaveInFile or SaveNotInFile.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     saveornot....... (input, logical) True (false) to process
c                      directive SaveInFile (SaveNotInFile).
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Position of first and
c                      last character of the last word scanned.
c     irc............. (output, integer) Return code.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      logical           saveornot
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, file, np, ncodes, pgcode, jrc
c
c     FIRST EXECUTABLE STATEMENT
c
c     Directive is ignored if not called from simulation program(s).
c
      if (pgmcode .ge. 2000) return
c
c     Setting the file number.
c
      call setfileno(cdline, llen, i1, i2, idlerrsev, file, irc)
      if (irc .ne. 0) return
c
c     Assigning the particle(s) specified to the corresponding
c     save file.
c
      jrc = 0
      np  = 0
 1020 continue
      call getpclecode(.true., cdline, llen, i1, i2,
     +                 ncodes, pgcode, irc)
c
      if (irc .eq. 8) goto 1030
c
      np = np + 1
      if ((irc .le. 3) .and. (irc .ne. 2)) then
        if (irc .ne. 3) then
c
c         Single particle specified
c
          allpclesave(pgcode, file)  = saveornot
c
        else
c
c         Particle group specified
c
          if (ncodes .eq. -1) then
c
c           This corresponds to the "remaining particles" group.
c           In this command this is equivalent to "all"
c           particles.
c
            call errprint(0, '*', 2, 'saveinfile',
     +        'In directives SaveInFile or SaveNotInFile, ' //
     +        '"AnyOther" particle group$means "All" particles.',
     +        0, 0, 0, 0.d0, ' ')
            jrc = max(jrc, 2)
c
            do i = -maxpcle, maxncode
              if (allpgroup(1, i)) allpclesave(i, file) = saveornot
            enddo
c
          else if (ncodes .eq. 0) then
c
c           This corresponds to the empty group.
c           SaveInFile: No particles to be saved in this file.
c           SaveNotInFile: All particles to be saved in this file.
c           (In the first case an additional instruction for this
c           file should exist).
c
            do i = -maxpcle, maxncode
              if (allpgroup(1, i)) then
                allpclesave(i, file) = .not. saveornot
              endif
            enddo
c
          else if (pgcode .le. maxpgroup) then
c
c           A significant normal group was specified.
c
            do i = -maxpcle, maxncode
              if (allpgroup(pgcode, i)) then
                allpclesave(i, file) = saveornot
              endif
            enddo
c
          else
c
c           A "generic" group was specified.
c
            pgcode = pgcode - maxpgroup
            do i = 1, gengrpsize(pgcode)
              j = gengrppcle(i - 1, pgcode)
              allpclesave(j, file) = saveornot
            enddo
c
          endif
        endif
c
      else
        call errprint(0, '$A17', idlerrsev, 'saveinfile',
     +                cdline(1:llen), 0, 0, 0, 0.d0, cdline(i1:i2))
        jrc = 8
      endif
      goto 1020
c
 1030 continue
      if (np .le. 0) then
        call errprint(0, '$A17', idlerrsev, 'saveinfile',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        irc = 3
      else
        irc = jrc
      endif
c
      return
c
      end
c     --- End of routine saveinfile
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setfileno(cdline, llen, i1, i2, errsev, fileno, irc)
c
c     Getting the index associated to a given compressed file
c     identification string.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Position of first and
c                      last character of the word to analyse.
c     errsev.......... (input, integer) Error message severity.
c     fileno.......... (output, integer) The file index.
c     irc............. (output, integer) Return code.
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
      character*(*)     cdline
      integer           llen, i1, i2, errsev, irc
      integer           fileno
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      character*12      auxstring
c
c     FIRST EXECUTABLE STATEMENT
c
      fileno = 0
c
      if (i2 .ge. i1) then
c
c       Checking if the file name corresponds to a valid file.
c
        auxstring = cdline(i1:i2)
        j         = max(i2 - i1 + 1, 3)
c
        do i = 1, npofiles
          fileno = i
          if (auxstring(1:j) .eq. pofilext(i)(2:j+1)) return
        enddo
c
c       Invalid file specification.
c
        call errprint(0, '$F01', errsev, 'setfileno',
     +                cdline(1:llen), 0, 0, 0, 0.d0, cdline(i1:i2))
        irc = 8
c
      else
c
c       Missing file specification.
c
        call errprint(0, '$A10', errsev, 'setfileno',
     +                cdline(1:llen), 0, 0, 0, 0.d0, ' ')
        irc = 5
      endif
c
      return
c
      end
c     --- End of routine setfileno
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine recobslev(recornot, cdline, llen, i1, i2, irc)
c
c     Processing directive RecordObsLevels
c
c     Written by: S. J. Sciutto, La Plata 1998, 2003.
c
c
c     Arguments:
c     =========
c
c     recornot........ (input, logical) Setting to use (true by now).
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Position of first and
c                      last character of the last word scanned.
c     irc............. (output, integer) Return code.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      logical           recornot
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, i3, itmp1, itmp2
      double precision  ftmp1
      logical           alln, lnotset
      character*8       opkey
c
c     FIRST EXECUTABLE STATEMENT
c
c     Directive is ignored if not called from simulation program(s).
c
      if (pgmcode .ge. 2000) return
c
      if (i1 .le. i2) then
c
        i = max(2, min(8, i2 - i1 + 1))
c
c       Searching the "None" option.
c
        opkey = 'None'
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c         "None" option: Unsetting all levels.
c
          do i = 1, mxobslevels
            olfilesave(i) = (.not. recornot)
          enddo
c
          return
c
        else
c
c         Searching the "All" or "All/n" options.
c
          i3 = i2
          do j = i1, i2
            if (cdline(j:j) .eq. '/') goto 1010
            i3 = j
          enddo
          alln = .false.
          goto 1020
 1010     continue
          alln = .true.
          i = max(2, min(8, i3 - i1 + 1))
 1020     continue
c
          opkey = 'All'
c
          if (cdline(i1:i3) .eq. opkey(1:i)) then
c
c           "All" option.
c
            j = 1
            k = mxobslevels
c
            if (alln) then
c
c             "All/n" option. Reading n.
c
              if (i3 .ge. i2) goto 3010
              read(cdline(i3+2:i2), *, err = 3010) l
              if (l .eq. 0) goto 3010
              if (l .lt. 0) then
                i = mxobslevels
                j = 1
              endif
c
            else
c
c             Direct "All" option.
c
              l = 1
c
            endif
c
c           Setting the observing levels.
c
            do i = j, k, l
              olfilesave(i) = recornot
            enddo
c
            return
c
          endif
        endif
      endif
c
c     Searching numerical specifications.
c
c     Getting the first observing level
c
      call getnumber(.false., cdline, llen, i1, i2,
     +               0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
        itmp2 = ftmp1
        if ((itmp2 .eq. ftmp1) .and.
     +      (itmp2 .gt. 0) .and. (itmp2 .le. mxobslevels)) then
          j = itmp2
        else
          call errprint(0, '$A20', idlerrsev, 'recobslev',
     +                  cdline(1:llen),
     +                  1, itmp2, 0, 0.d0, ' ')
          return
        endif
c
c       Searching the last observing level.
c
        call getnumber(.true., cdline, llen, i1, i2,
     +                 0, 0.0d0, ftmp1, itmp1)
c
        if (itmp1 .eq. 0) then
          itmp2 = ftmp1
          if ((itmp2 .eq. ftmp1) .and.
     +        (itmp2 .gt. 0) .and. (itmp2 .le. mxobslevels)) then
            k = itmp2
          else
            call errprint(0, '$A20', idlerrsev, 'recobslev',
     +                    cdline(1:llen),
     +                    1, itmp2, 0, 0.d0, ' ')
            return
          endif
c
c         Searching the step.
c
          call getnumber(.true., cdline, llen, i1, i2,
     +                   0, 0.0d0, ftmp1, itmp1)
c
          if (itmp1 .eq. 0) then
            itmp2 = ftmp1
            if ((itmp2 .eq. ftmp1) .and. (itmp2 .ne. 0)) then
              l = itmp2
            else
              call errprint(0, '$A20', idlerrsev, 'recobslev',
     +                      cdline(1:llen),
     +                      1, itmp2, 0, 0.d0, ' ')
              return
            endif
c
          else
c
c           No step specification or no numeric specification.
c
            l = 1
c
          endif
c
        else
c
c         No second number or no numeric specification.
c
          k = j
          l = 1
c
        endif
c
        if (itmp1 .gt. 2) then
          call errprint(0, '$A20', idlerrsev, 'recobslev',
     +                  cdline(1:llen), 1, itmp2, 0, 0.d0, ' ')
          return
        endif
c
c       Setting the indicated observing levels
c
        lnotset = .true.
        do i = j, k, l
          olfilesave(i) = recornot
          lnotset = .false.
        enddo
c
        if (lnotset) then
c
          call errprint(0, '*', 2, 'recobslev',
     +    'No observing levels set with the following directive:',
     +    0, 0, 0, 0.d0, cdline(1:llen))
          irc = 2
c
        else
          irc = 0
        endif
c
      else
        call errprint(0, '$A12', idlerrsev, 'recobslev', ' ',
     +                0, 0, 0, 0.d0, cdline(1:llen))
        irc = itmp1
      endif
      return
c
c     Other error conditions.
c
 3010 continue
      call errprint(0, '$A12', idlerrsev, 'recobslev', ' ',
     +              0, 0, 0, 0.d0, cdline(1:llen))
      irc = 0
      return
c
      end
c     --- End of routine recobslev.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine bdrycheck(bdrykey, bdry12, x, irc)
c
c     Checking a number againsts the boundary conditions defined for
c     real input parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     bdrykey......... (input, integer) Boundary key. 0 means that
c                      no boundary check is needed for the variable,
c                      any real number is a valid input. 1 means that
c                      the variable must be greater or equal than
c                      argument bdry12(1). 2 means that the variable
c                      must be less or equal than argument bdry12(2).
c                      3 means that both 1 and 2 hold.
c     bdry12.......... (input, double precision, array(2)) Parameter
c                      used in connection with "bdrykey".
c     x............... (input, double precision) The number to check.
c     irc............. (output, integer) Return code. 0 means that
c                      the check was passed successfully.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           bdrykey, irc
      double precision  bdry12(2), x
c
c     FIRST EXECUTABLE STATEMENT
c
      irc = 0
      if (bdrykey .eq. 1) then
        if (x .lt. bdry12(1)) irc = 3
      else if (bdrykey .eq. 2) then
        if (x .gt. bdry12(2)) irc = 5
      else if (bdrykey .eq. 3) then
        if ((x .lt. bdry12(1)) .or. (x .gt. bdry12(2))) irc = 8
      endif
      return
c
      end
c     --- End of routine bdrycheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ibdrycheck(bdrykey, bdry12, x, ix, irc)
c
c     Checking a number againsts the boundary conditions defined for
c     integer input parameters.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     bdrykey......... (input, integer) Boundary key. 0 means that
c                      no boundary check is needed for the variable
c                      (the routine only checks that it is an integer),
c                      any integer number is a valid input. 1 means
c                      that the variable must be greater or equal than
c                      argument bdry12(1). 2 means that the variable
c                      must be less or equal than argument bdry12(2).
c                      3 means that both 1 and 2 hold.
c     bdry12.......... (input, integer, array(2)) Parameter used in
c                      connection with "bdrykey".
c     x............... (input, double precision) The number to check.
c     ix.............. (output, integer) x converted to an integer.
c     irc............. (output, integer) Return code. 0 means that
c                      the check was passed successfully.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           bdrykey, irc
      integer           bdry12(2)
      double precision  x
      integer           ix
c
c     FIRST EXECUTABLE STATEMENT
c
      irc = 0
c
      ix = x
c
      if (ix .ne. x) then
        irc = 10
      else if (bdrykey .eq. 1) then
        if (ix .lt. bdry12(1)) irc = 3
      else if (bdrykey .eq. 2) then
        if (ix .gt. bdry12(2)) irc = 5
      else if (bdrykey .eq. 3) then
        if ((ix .lt. bdry12(1)) .or. (ix .gt. bdry12(2))) irc = 8
      endif
      return
c
      end
c     --- End of routine ibdrycheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initp.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
