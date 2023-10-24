c
c     FILE: omsgmgr.f                       Creation date: 22/JUN/1996.
c                                       LAST MODIFICATION: 20/AUG/2003.
c
c     This file contains the output messages management routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine welcome(ou, cc, nlines)
c
c     Typing a welcome message to announce the program is running.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997; Fermilab 1999;
c                                La Plata 2001; Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     ou.............. (input, integer) Logical output unit.
c     cc.............. (input, character*1) If nonblank, this character
c                      is inserted before each written line (comment
c                      character).
c     nlines.......... (output, integer) Number of written lines.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
      include 'compildat.f'
      include 'initpar.f'
c
c     Declaration of arguments.
c
      integer           ou, nlines
      character*1       cc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*20      datistr
      character*38      hoststr
      integer           l
      character*4       ggstr
c
c     FIRST EXECUTABLE STATEMENT
c
 2010 format(a, 1x, 9a)
c
      if (cc .eq. ' ') then
        ggstr = '>>>>'
      else
        ggstr = cc // '>>>'
      endif
c
      write(ou, 2010) ggstr
c
c     Variable pgmcode specifies the calling main program.
c
      if (pgmcode .lt. 2000) then
c
c       Called from simulation program(s).
c
        datistr = 'AIRES'
        l       = 5
c
      else if (pgmcode .lt. 2200) then
c
c       Called from summary program(s).
c
        datistr = 'AIRES (Summary)'
        l       = 15
c
      else if (pgmcode .lt. 2500) then
c
c       Called from merge program(s).
c
        datistr = 'AIRES (Merge)'
        l       = 13
c
      else if (pgmcode .lt. 3000) then
c
c       Called from idf upgrade program(s).
c
        datistr = 'AIRES (Upgrade)'
        l       = 15
c
      else
        call errprint(0, '*', 4, 'welcome',
     +       'Invalid value for internal parameter "pgmcode"',
     +       0, 0, 0, 0.d0, ' ')
      endif
c
      write(ou, 2010) ggstr, 'This is ', datistr(1:l), ' version ',
     +                aires_version, ' (', aires_date, ')'
c
      hoststr = aires_cwho
      l = len(aires_cwho)
      if (l .gt. 38) then
        l = 38
        hoststr(36:38) = '...'
      endif
c
      write(ou, 2010) ggstr, '(Compiled by ', hoststr(1:l),
     +                ', date: ', aires_cdate, ')', aires_cstar
c
      hoststr = crhost
      l = crhostlen
      if (l .gt. 28) then
        l = 28
        hoststr(26:28) = '...'
      endif
      call dati(datistr)
c
      write(ou, 2010) ggstr, 'USER: ', cruser(1:cruserlen),
     +                     ', HOST: ', hoststr(1:l),
     +                     ', DATE: ', datistr(1:11)
c
      write(ou, 2010) ggstr
      write(ou, *)
c
      nlines = 6
c
      return
      end
c     --- End of routine welcome.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine aireslogo(ou, nlines)
c
c     Printing a character AIRES logo.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     ou.............. (input, integer) I/O unit where to print the
c                      logo.
c     nlines.......... (output, integer) Number of written lines.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ou, nlines
c
c     FIRST EXECUTABLE STATEMENT
c
 2010 format('>>>>', 7x, a, a1)
c
      write(ou, 2010)
      write(ou, 2010)
c
      write(ou, 2010)
     +' AAAAA    IIII   RRRRRR    EEEEEEE    SSSSS       ', '\\'
      write(ou, 2010)
     +'AAAAAAA   IIII   RRRRRRR   EEEEEEE   SSSSSSS       A'
      write(ou, 2010)
     +'AA   AA    II    RR   RR   EE        SS   SS        I'
      write(ou, 2010)
     +'AAAAAAA    II    RRRRRR    EEEE       SSS            R-shower'
      write(ou, 2010)
     +'AAAAAAA    II    RRRRR     EEEE         SSS          Extended'
      write(ou, 2010)
     +'AA   AA    II    RR  RR    EE        SS   SS         Simulations'
      write(ou, 2010)
     +'AA   AA   IIII   RR   RR   EEEEEEE   SSSSSSS         |'
      write(ou, 2010)
     +'AA   AA   IIII   RR   RR   EEEEEEE    SSSSS         / ',  '\\'
c
      write(ou, 2010)
      write(ou, 2010)
c
      write(ou, 2010)
     +'Departamento de Fisica, Universidad de La Plata, ARGENTINA.'
c
      write(ou, 2010)
c
      nlines = 14
c
      return
      end
c     --- End of routine aireslogo.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine openlog(access)
c
c     Opening the log file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2001.
c
c
c     Arguments:
c     =========
c
c     access.......... (input, character*(*)) The kind of access to use
c                      in the open statement ('SEQUENTIAL' or 'APPEND')
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
      character*(*)     access
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           irc
c
c     FIRST EXECUTABLE STATEMENT
c
      auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // lgfext
c
      if (access .eq. 'SEQUENTIAL') then
        open(7, file = auxfilestring, status = 'UNKNOWN',
     +          access = access, err = 3010)
      else
        call appendopen(7, auxfilestring, 'UNKNOWN', 'FORMATTED', irc)
        if (irc .ne. 0) goto 3010
      endif
c
      lgfisclosed = .false.
      return
c
 3010 continue
c
c     Error opening the log file. This is an error only for
c     the simulation programs.
c
      if (pgmcode .lt. 2000) then
c
c       If the call comes from minmaxou then it is not safe to make a
c       call to errprint since this routine might be the calling one
c       (recursive situation). In that case a simple message will be
c       printed.
c
        if (mmacutok) then
          call errprint(0, '$A09', 4, 'openlog', ' ',
     +                  1, 7, 0, 0.d0,
     +                  auxfilestring(1:leadingfnlen(2)+4))
        else
          write(6, 2010) '!!!! Panic! Fatal error opening log file.'
          write(6, 2010) '!!!! Stopping.'
 2010     format(a)
          call airesshutdown
          stop
        endif
      endif
c
      return
      end
c     --- End of routine openlog
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine closelog
c
c     Closing the log file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
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
c     FIRST EXECUTABLE STATEMENT
c
      close(7)
      lgfisclosed = .true.
c
      return
      end
c     --- End of routine closelog
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine putlog(ouflag, datiflag, message)
c
c     Writing messages into the log file(s).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997; Fermilab 1999.
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c     datiflag........ (input, logical) True means that the
c                      current date and time will be printed
c                      with the message.
c     message......... (input, character*(*)) The message text.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ouflag
      logical           datiflag
      character*(*)     message
c
c     Declaration of shared data.
c
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u, minu, maxu
      character*20      dts
c
c     FIRST EXECUTABLE STATEMENT
c
c     Selecting output units.
c
      call minmaxou(ouflag, minu, maxu)
c
c     Printing the message.
c
      if (datiflag) then
c
        call dati(dts)
        do u = minu, maxu
          write(u, 2010, err = 3010) '> ', dts, '. ', message
        enddo
c
      else
c
        do u = minu, maxu
          write(u, 2010, err = 3010) '  ', message
        enddo
c
      endif
 2010 format(4a)
c
      return
c
c     Error message.
c
 3010 continue
c
c     An error occurred during a write operation to the log file.
c     This is relevant only for the simulation programs.
c
      if ((pgmcode .lt. 2000) .or. (u .eq. minu)) then
        call errprint(0,  '$A09', 4, 'putlog', ' ',
     +       1, u, 0, 0.d0,
     +       'Error writing AIRES log file.')
      endif
c
      return
      end
c     --- End of routine putlog
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine clearerrmess
c
c     Internal routine to clear error message arrays.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'omsgpar.f'
c
c     Declaration of shared data.
c
      include 'omsgcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      errlastpos = 0
      nerrcodes  = 0
c
c     Setting parameters for the "dummy" message.
c
      errname(0)   = '*'
      deferrsev(0) = 1
c
      return
      end
c     --- End of routine clearerrmess
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine mtxtappnd(mflag, errorname, dsev, text)
c
c     Internal routine to manage error message data.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     mflag........... (input, integer) Message flag. If mflag .ge. 2
c                      then a new error code is set. If mflag .eq. 1
c                      a new text line for the current error code is
c                      begun; and if mflag .le. 0 the characters in
c                      parameter "text" are appended to the current
c                      text line. Parameters "dsev", "nidata" and
c                      "nfdata" are taken into account only when
c                      mflag .ge. 2.
c     errorname....... (input, character*(*)) Message name. Maximum 4
c                      characters.
c     dsev............ (input, integer) default severity.
c     text............ (input, character*(*)) Text to process.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'omsgpar.f'
c
c     Declaration of arguments.
c
      integer           mflag, dsev
      character*(*)     errorname, text
c
c     Declaration of shared data.
c
      include 'omsgcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, i2, l, ierrcode
      logical           isnewmessage, isbdychar
c
c     FIRST EXECUTABLE STATEMENT
c
      l  = len(text)
      i2 = errlastpos + l
c
      if (i2 .gt. errcl) then
c
c       Not enough room to store more error data.
c       The message is printed explicitly since errprint
c       is not ready at this moment.
c
        write(6, 2010) '===='
        write(6, 2010)
     +    '====  Not enough space to store error message named: ',
     +    errorname
        write(6, 2010)
     +    '====  Increase parameter "erravlen" and recompile.'
        write(6, 2010) '===='
        stop
c
      endif
 2010 format(5a)
c
c     Checking if this is a new message or not.
c
      isnewmessage = .false.
      do i = 0, nerrcodes
        ierrcode = i
        if (errname(i) .eq. errorname) goto 1010
      enddo
      isnewmessage = .true.
 1010 continue
c
c     Checking the existence of the boundary character within
c     message text.
c
      isbdychar = .false.
      do i = 1, l
        if (text(i:i) .eq. mtxtbdy) isbdychar = .true.
      enddo
      if (isbdychar) then
c
        write(6, 2010) '===='
        write(6, 2010)
     +    '====  Warning: The reserved boundary mark character "',
     +    mtxtbdy, '" is used within string:'
        write(6, 2010) '====  ', text
        write(6, 2010)
     +    '====  (Mark character is used to label line boundaries)'
        write(6, 2010) '===='
c
      endif
c
c     Switching accordingly with parameter mflag.
c
      if (mflag .ge. 2) then
c
c       New error code.
c
        if (isnewmessage) then
c
          if (nerrcodes .ge. maxerrcodes) then
c
c           Not enough room to store more error codes.
c
            write(6, 2010) '===='
            write(6, 2010)
     +      '====  Not enough space to store error message named: ',
     +      errorname
            write(6, 2010)
     +      '====  Increase parameter "maxerrcodes" and recompile.'
            write(6, 2010) '===='
            stop
c
          endif
c
c         Adding the error message data.
c
          nerrcodes = nerrcodes + 1
c
          errname(nerrcodes) = errorname
c
c         Checking the error default severity.
c
          if ((dsev .gt. 0) .and. (dsev .le. 4)) then
c
            deferrsev(nerrcodes) = dsev
c
          else
c
            write(6, 2010) '===='
            write(6, 2010)
     +      '====  Warning: Out of range value for default severity'
            write(6, 2010)
     +      '====  (Allowed values range from 1 to 4).'
            write(6, 2010)
     +      '====  Message name: ', errorname
            write(6, 2010)
     +      '====  Default severity is set to one'
            write(6, 2010) '===='
c
            deferrsev(nerrcodes) = 1
c
          endif
c
c         Appending the first text string.
c
          errbeg(nerrcodes) = errlastpos + 1
          errend(nerrcodes) = i2
          errormessages(errlastpos+1:i2) = text
          errlastpos = i2 + 1
c
        else
c
c         The message already exists.
c
          write(6, 2010) '===='
          write(6, 2010)
     +    '====  Error message "', errorname, '" already exists.'
          write(6, 2010)
     +    '====  Cannot store it as new message.'
          write(6, 2010) '===='
          stop
c
        endif
c
      else if (isnewmessage .or. (ierrcode .ne. nerrcodes)) then
c
c       Being here is an error. Text can be appended only to the
c       last defined message.
c
        write(6, 2010) '===='
        write(6, 2010)
     +    '====  Message "', errorname, '" is not defined yet,'
        write(6, 2010)
     +    '====  or it is not the last defined message.'
        write(6, 2010)
     +    '====  Cannot append additional text.'
        write(6, 2010) '===='
        stop
c
      else if (mflag .eq. 1) then
c
c       Appending a new line of text to the current message.
c
        errend(nerrcodes) = i2
        errormessages(errlastpos+1:i2) = text
        errlastpos = i2 + 1
c
      else
c
c       Appending text to the current line.
c
        errend(nerrcodes) = i2 - 1
        errormessages(errlastpos:i2-1) = text
        errlastpos = i2
c
      endif
c
c     Ending with a boundary mark.
c
      errormessages(errlastpos:errlastpos) = mtxtbdy
c
      return
      end
c     --- End of routine mtxtappnd
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine errprint(ouflag, errorname, sev, callrout, auxstring1,
     +                    nidata, idata, nfdata, fdata, auxstring2)
c
c     Printing error messages in standard way.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2001, 2002.
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c     errname......... (input, integer) Error name.
c     sev............. (input, integer) Error severity. 0 implies
c                      that the default severity (correspoding
c                      to the error code) will be used. 1 informative
c                      message, 2 warning, 3 error, >3 fatal error
c                      (process is stopped).
c     callrout........ (input, character*(*)) Name of calling routine.
c     auxstring1...... (input, character*(*)) Auxiliary string
c                      to pass additional text for the message (not
c                      always used).
c     nidata.......... (input, integer) Number of integer data to print
c                      with the message.
c     idata........... (input, integer, array(nidata)) Integer data to
c                      be printed.
c     nfdata.......... (input, integer) Number of real data to print
c                      with the message.
c     fdata........... (input, double precision, array(nfdata)) Real
c                      data to be printed.
c     auxstring2...... (input, character*(*)) Same as auxtring1
c                      but the message is enclosed between bars "/".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'omsgpar.f'
c
c     Declaration of arguments.
c
      integer           ouflag, sev, nidata, nfdata
      character*(*)     errorname
      character*(*)     callrout, auxstring1, auxstring2
      integer           idata(nidata)
      double precision  fdata(nfdata)
c
c     Declaration of shared data.
c
      include 'omsgcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ou, minu, maxu
      integer           i, i1, i2, isev, ierrcode
      character*20      datistr
      character*6       linehead(4)
      character*12      sevstring(4)
      integer           sevstrlen(4)
c
      data              linehead(1)   / 'Info  '  /
      data              linehead(2)   / 'wwww  '  /
      data              linehead(3)   / 'EEEE  '  /
      data              linehead(4)   / 'FFFF  '  /
      data              sevstring(1)  / 'Informative' /
      data              sevstring(2)  / 'Warning' /
      data              sevstring(3)  / 'Error' /
      data              sevstring(4)  / 'FATAL' /
      data              sevstrlen     / 11, 7, 5, 5 /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Selecting output units.
c
      call minmaxou(ouflag, minu, maxu)
c
c     Getting the error code.
c
      do i = 0, nerrcodes
        ierrcode = i
        if (errname(i) .eq. errorname) goto 1010
      enddo
c
c     The error name given is not in the table.
c
      do ou = minu, maxu
        write(ou, 2010) '===='
        write(ou, 2010)
     +    '====  Unknown message "', errorname,
     +    '". Assuming dummy ("*") message name.'
        write(ou, 2010) '===='
      enddo
      ierrcode = 0
c
 1010 continue
c
c     Setting the severity.
c
      if (sev .le. 0) then
        isev = deferrsev(ierrcode)
      else if (sev .gt. 4) then
        isev = 4
      else
        isev = sev
      endif
c
c     Printing the message.
c
      call dati(datistr)
c
      do ou = minu, maxu
        write(ou, 2010) linehead(isev)
        write(ou, 2010) linehead(isev), datistr, '. ',
     +                  sevstring(isev)(1:sevstrlen(isev)),
     +                  ' message from ', callrout
      enddo
c
      if (ierrcode .gt. 0) then
c
c       Printing the stored text of the message.
c
        do ou = minu, maxu
          i1 = errbeg(ierrcode)
          do i = errbeg(ierrcode), errend(ierrcode)
            if (errormessages(i:i) .eq. mtxtbdy) then
              write(ou, 2010) linehead(isev), errormessages(i1:i-1)
              i1 = i + 1
            endif
          enddo
          write(ou, 2010) linehead(isev),
     +                    errormessages(i1:errend(ierrcode))
        enddo
      endif
c
c       Printing the optional parts.
c
      do ou = minu, maxu
c
        if (auxstring1 .ne. ' ') then
c
          i1 = 1
          i2 = len(auxstring1)
          do i = 1, i2
            if (auxstring1(i:i) .eq. mtxtbdy) then
              write(ou, 2010) linehead(isev), auxstring1(i1:i-1)
              i1 = i + 1
            endif
          enddo
          write(ou, 2010) linehead(isev), auxstring1(i1:i2)
c
        endif
c
        if (nidata .gt. 0) then
          write(ou, 2020) linehead(isev), '>',
     +                    (idata(i), ',', i = 1, nidata - 1),
     +                    idata(nidata), '<'
        endif
c
        if (nfdata .gt. 0) then
          write(ou, 2030) linehead(isev), '>',
     +                    (fdata(i), ',', i = 1, nfdata - 1),
     +                    fdata(nfdata), '<'
        endif
c
        if (auxstring2 .ne. ' ') then
          write(ou, 2010) linehead(isev), '>', auxstring2, '<'
        endif
c
      enddo
c
 2010 format(8a)
 2020 format(2a, 5(i11, a))
 2030 format(2a, 1p, 5(g11.4, a))
c
c     Checking the fatal condition.
c
      if (isev .ge. 4) then
c
        do ou = minu, maxu
          write(ou, 2010) linehead(isev), 'Stopping.'
          write(ou, 2010) linehead(isev)
        enddo
        call airesshutdown
        stop
c
      else
c
        do ou = minu, maxu
          write(ou, 2010) linehead(isev)
        enddo
c
      endif
c
      return
      end
c     --- End of routine errprint
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine minmaxou(ouflag, minu, maxu)
c
c     Selecting output logical i/o units.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag: 0 or negative means unit 6 only,
c                      1 means unit 7 only, 2 means both units 6 and 7,
c                      3 means unit 8 only, ouflag > 8 means unit
c                      ouflag only and any other value means unit 6
c                      only.
c     minu, maxu...... (output, integer) Logical unit range.
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
      integer           ouflag, minu, maxu
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (ouflag .eq. 1) then
        minu = 7
        maxu = 7
      else if (ouflag .eq. 2) then
        minu = 6
        maxu = 7
      else if (ouflag .eq. 3) then
        minu = 8
        maxu = 8
      else if (ouflag .gt. 8) then
        minu = ouflag
        maxu = ouflag
      else
        minu = 6
        maxu = 6
      endif
c
c     Checking the status of the log file
c
      if (maxu .eq. 7) then
        if (lgfisclosed) then
          mmacutok = .false.
          call openlog('APPEND')
          mmacutok = .true.
          if (lgfisclosed) maxu = 6
        endif
      endif
c
      return
      end
c     --- End of routine minmaxou
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'omsgmgr.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
