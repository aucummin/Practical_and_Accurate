c
c     FILE: AiresSry.f                      Creation date: 12/NOV/1996.
c                                       LAST MODIFICATION: 23/NOV/2001.
c
c                         SUMMARY PROGRAM FILE
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
c
c          AIRESAIRESAIRESAIRESAIRESAIRESAIRESAIRESAIRESAIRES
c          I                                                A
c          R                \                               I
c          E                 \                              R
c          S                  A                             E
c          A                   I                            S
c          I                    R-shower                    A
c          R                    Extended                    I
c          E                    Simulations                 R
c          S                    |                           E
c          A                   / \                          S
c          I                                                A
c          RESAIRESAIRESAIRESAIRESAIRESAIRESAIRESAIRESAIRESAI
c
c
c     AIRES: A program for extended air shower simulations.
c
c     Written by: S. J. Sciutto
c                 Departamento de Fisica
c                 Universidad Nacional de La Plata
c                 C. C. 67 - 1900 La Plata
c                 ARGENTINA
c
c         E-mail: sciutto@fisica.unlp.edu.ar
c
c     La Plata, 1996, 1997, 1998, 1999; Fermilab 1999;
c     La Plata 2000, 2001.
c
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
      program AiresSry
c
c     Main program for creating summary files from existing .idf files.
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
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*176     tmpfn
      logical           exists
      character*6       asciibin
      integer           asciibinlen
      integer           i, iver, tmpfnlen, idfadf
c
c     FIRST EXECUTABLE STATEMENT
c
c     Program code.
c
      pgmcode = 2001
c
c     Getting the user and host names.
c
      call username(cruser, cruserlen, crhost, crhostlen)
c
c     Starting the error message system.
c
      call clearerrmess
      call errinit
c
c     Telling the world we're here.
c
      call welcome(6, ' ', i)
c
c     Some initializations needed before scanning the input data.
c
      call init0
c
c     Scanning the input data file.
c
      call putlog(0, .true., 'Reading data from standard input unit')
      call inputscan
c
c     Some initializations needed after scanning the input data.
c
      call init1
c
      if (runforcein) then
c
c       This parameter has nosense here.
c
        call errprint(0, '*', 1, 'AiresSry (MAIN)',
     +   'ForceInit directive ignored.', 0, 0, 0, 0.d0, ' ')
c
      endif
 2010 format(5a)
 2030 format(a, i3.3)
c
c     Setting the trailing file name string.
c
      iver         = tasknamever
      leadingf0    = taskname
      leadingf0len = tasknamelen
c
      if ((iver .gt. 0) .and. (iver .le. 999)) then
c
        do i = 1, mxwdir - 1
          write(leadingfn(i)(leadingfnlen(i)+1:160), 2030) '_', iver
          leadingfnlen(i) = leadingfnlen(i) + 4
        enddo
        leadingf0len = leadingf0len + 4
        leadingf0(leadingf0len-3:leadingf0len) =
     +            leadingfn(2)(leadingfnlen(2)-3:leadingfnlen(2))
c
      else if (iver .gt. 999) then
c
        do i = 1, mxwdir - 1
          write(leadingfn(i)(leadingfnlen(i)+1:160), 2010)
     +      '_INVALID_VERSION_NUMBER'
          leadingfnlen(i) = leadingfnlen(i) + 23
        enddo
        leadingf0len = leadingf0len + 23
        leadingf0(leadingf0len-22:leadingf0len) =
     +            leadingfn(2)(leadingfnlen(2)-22:leadingfnlen(2))
c
      endif
c
c     Searching for idf/adf files.
c
c     IDF is the first guess.
c
      tmpfn = leadingfn(2)(1:leadingfnlen(2)) // idfext
c
c     Checking the existence of the file.
c
      inquire (file = tmpfn, exist = exists)
c       
      if (exists) then
c
        idfadf = 1
c
      else
c
c       ADF is the second guess.
c
        tmpfn = leadingfn(2)(1:leadingfnlen(2)) // adfext
c
c       Checking the existence of the file.
c
        inquire (file = tmpfn, exist = exists)
c       
        if (exists) then
c
          idfadf = 2
c
        else
c
c         Neither the idf nor the adf file exist: It's an error here.
c
          call errprint(0, '*', 4, 'AiresSry (MAIN)',
     +     'Specified idf/adf file does not exist.', 0, 0, 0, 0.d0,
     +     leadingfn(2)(1:leadingfnlen(2)) // idfext // ' or ' //
     +     adfext)
          idfadf = -1
c
        endif
      endif
c
c     The file exists: Going on.
c       
      if (idfadf .eq. 1) then
c
c       Binary dump file processing.
c
c       Additional initializations, which include reading
c       of the idf file.
c
        call init3(0)
c
        asciibin    = 'Binary'
        asciibinlen = 6
c
      else
c
c       ASCII dump file processing.
c
c       Additional initializations, which include reading
c       of the adf file.
c
        call init3adf
c
        asciibin    = 'ASCII'
        asciibinlen = 5
c
      endif
c
      if (inpcheckonly) goto 3000
c
c     Opening the log file.
c
      call openlog('APPEND')
c
      call putlog(2, .true., 'Beginning of summary processing.')
      call putlog(2, .false., asciibin(1:asciibinlen) //
     +                        ' dump file successfully read.')
c
c     Completing the data initialization.
c
      call init4s
c
c     DOING THE SUMMARY.
c
      call putlog(2, .false., 'Writing summary/tss/export file(s).')
      tmpfn = 'Number of completed showers: '
      call intnice(lastshower, 0, tmpfn(30:176), tmpfnlen)
      tmpfnlen = tmpfnlen + 29
      call putlog(2, .false., tmpfn(1:tmpfnlen))
c
      call summary
c
      call putlog(2, .true., 'Summary completed.')
c
      if (sryison) then
        tmpfn = 'Summary file: ' // leadingfn(2)(1:leadingfnlen(2))
     +                           // sryfn
        tmpfnlen = leadingfnlen(2) + 22
        call putlog(0, .false., tmpfn(1:tmpfnlen))
      endif
c
c     End of Main program (summary).
c
      goto 1999
c
 3000 continue
c
c     End of processing when CheckOnly is enabled.
c
      call idatacheck(0)
c
      call errprint(0, '*', 2, 'AiresSry (MAIN)',
     + 'Some check diagnostics may not apply if the same input file$'
     + // 'is used with the simulation program.', 0, 0, 0, 0.d0, ' ')
c
      call putlog(0, .true.,
     +            'Stopping because of the CheckOnly flag.')
      write(6, 2010) '>>>>'
c
 1999 continue
c
c     Performing ordered shutdown.
c
      call airesshutdown
c
      end
c     --- End of main program.
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
c     End of file 'AiresSry.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
