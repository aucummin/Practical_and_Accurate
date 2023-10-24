c
c     FILE: Aires.f                         Creation date: 18/JUN/1996.
c                                       LAST MODIFICATION: 18/JUL/2003.
c
c                           MAIN PROGRAM FILE
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
c                 AIRES (AIRshower Extended Simulations).
C
c
c     A program to simulate extended air showers initiated after the
c     incidence of cosmic rays on the earth's atmosphere.
c
c     Written by: S. J. Sciutto
c                 Departamento de Fisica
c                 Universidad Nacional de La Plata
c                 C. C. 67 - 1900 La Plata
c                 ARGENTINA
c
c         E-mail: sciutto@fisica.unlp.edu.ar
c
c     La Plata 1996, 1997, 1998, 1999; Fermilab 1999;
c     La Plata 2000, 2001, 2002, 2003.
c
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
      program aires
c
c
c                         MAIN SIMULATION PROGRAM
c
c
      use iso_c_binding
      use grid
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  cpu02, runcputime, dummy
      character*176     tmpfn
      integer           jsts, trial
      character*16      stsmsg
      logical           exists, stopfile
      integer           i, ijob, irc
      integer           iver, iver0, arc, stoprc, tmpfnlen
      logical           defsta, taskover
c
c     FIRST EXECUTABLE STATEMENT
c
c     Program code.
c
      pgmcode = 1001
c
c     Getting the processor time for the beginning of the program,
c     the current date and time and who is running at which machine.
c
      call add_one
      call cputime(.true., cpu0(4), dummy)
      call dati(datistr0(4))
      call username(cruser, cruserlen, crhost, crhostlen)
c
c     NOTE: Routine "cputime" should always be used with a "false" flag
c           when called from outside this main program.
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
      call cioinit0w
      call defineallcio
      call initialremark
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
c     Branching between run initialization or continuation.
c
      iver = tasknamever
c
      if (runforcein) then
c
c       Task initialization must be forced.
c       Setting the version number for the files.
c       The search will continue until a nonexisting file
c       is found.
c
        iver0 = tasknamever
        if (tasknamever .eq. 0) then
c
          write(tmpfn, 2010) leadingfn(2)(1:leadingfnlen(2)),
     +                       idfext
          inquire (file = tmpfn, exist = exists)
          if (.not. exists) goto 1010
          iver0 = 1
c
        endif
c
c       Searching an appropiate version number.
c
        do i = iver0, 999
          iver = i
c
          write(tmpfn, 2020) leadingfn(2)(1:leadingfnlen(2)),
     +                       '_', i, idfext
          inquire (file = tmpfn, exist = exists)
          if (.not. exists) goto 1010
        enddo
c
c       Arrived to version 999, all of them exist!
c
        if (inpcheckonly) then
          i = 3
        else
          i = 4
        endif
        call errprint(0, '*', i, 'Aires (MAIN)',
     +   'All file versions up to 999 exist! ' //
     +   'Cannot create new output files.', 0, 0, 0, 0.d0, ' ')
c
        iver = 9999
c
      endif
 2010 format(5a)
 2020 format(2a, i3.3, a)
 2030 format(a, i3.3)
c
 1010 continue
c
c     Resetting the trailing file name string.
c
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
      tmpfnlen = leadingfnlen(2) + 1
      tmpfn    =  leadingfn(2)(1:leadingfnlen(2)) // idfext
c
c     Setting the status file (first time).
c
      if (.not. inpcheckonly) then
c
        call Aires_status(1, crhost(1:crhostlen),
     +                    cruser(1:cruserlen),
     +                    leadingf0(1:leadingf0len),
     +                    'Initializing', 0,
     +                    mtotshowers, lastshower, -1, trial)
c
c       We create also the directory file.
c
        call Aires_dirs(crhost(1:crhostlen), cruser(1:cruserlen),
     +                  leadingf0(1:leadingf0len),
     +                  randomfnh, wdirname, wdirnamelen)
c
      endif
c
c     Processing the internal dump file (idf).
c
c     Checking the existence of the file.
c
      inquire (file = tmpfn, exist = exists)
c       
      if (exists) then
c
c       The file exists: This is the continuation of an already
c                        initialized task.
c
c       Additional initializations, which include reading
c       of the idf file.
c
        if (inpcheckonly) then
          call putlog(0, .true.,
     +      'Reading input parameters from internal file.')
          call init3(0)
          goto 3000
        else
c
c         Opening the log file.
c
          call openlog('APPEND')
c
c         The particle stacks will be opened during the execution of
c         "init3". "init4" completes the initialization phase.
c
          call init3(2)
          call newprocess
          call init4
c
c         Checking status variables of the cio system.
c
          call ciocheckstatus
c
          call putlog(2, .true.,
     +      'Internal data file read. Continuing with current task.')
          tmpfn = 'Process number '
          call intnice(processnumber, 0, tmpfn(16:176), tmpfnlen)
          i = tmpfnlen + 37
          tmpfn(i-21:i) = ' (Last completed run: '
          call intnice(jobnumber, 0, tmpfn(i+1:176), tmpfnlen)
          tmpfnlen = tmpfnlen + i + 2
          tmpfn(tmpfnlen-1:tmpfnlen) = ').'
          call putlog(2, .false., tmpfn(1:tmpfnlen))
c
        endif
c
      else
c
c       The idf file does not exist: This is the beginning of a
c                                    new task.

        call defineusedcio
c
        tasknamever = iver
        if (inpcheckonly) goto 3000
c
c       Opening the log file.
c
        call openlog('SEQUENTIAL')
c
c       Copying the initial message to the log file.
c
        call welcome(7, ' ', i)
c
c       Checking the input data.
c
        call idatacheck(2)
c
        call putlog(0, .true., 'Beginning new task.')
c
c       Preparing the beginning of the process.
c
        call init2
c
c       Opening the particle stacks.
c
        if (.not. defsta(0, npstacks, 0, 0.d0,
     +            leadingfn(mxwdir)(1:leadingfnlen(mxwdir)))) then
          call errprint(2, '$A34', 4, 'Aires (MAIN)', ' ',
     +    0, 0, 0, 0.d0, ' ')
        endif
c
c       Completing the data initialization.
c
        call init4
c
c       Completing the definitions of the cio files, opening them and
c       writing the respective file headers.
c
        call cioinit1w
        call openallcio(0)
c
c       Stacking the first primary particle.
c
        call newprimary
c
c       Saving data in the internal dump file.
c
        call idfwrite
c
        call putlog(2, .true.,
     +              'Starting simulation of first shower.')
c
      endif
c
c     Checking the existence of aborted runs.
c
      if (trial .gt. 1) then
        write(tmpfn, 2040)
     +    'Detected previous aborted run. This is trial #',
     +    trial, '.'
 2040   format(a, i2, a)
        call putlog(2, .false., tmpfn(1:49))
      endif
c
c     Closing the cio files.
c
      call cioclose
c
c     Loop over runs within a process.
c
      do ijob = 1, processruns
c
        call newrun
c
c       Reopening the cio files and resetting the status variables
c       of the cio system.
c
        call openallcio(3)
        call ciormstatus
c
c       Closing the log file (temporarily)
c
        call closelog
c
c       INVOKING THE KERNEL.
c
        call Aires_status(2, crhost(1:crhostlen),
     +                    cruser(1:cruserlen),
     +                    leadingf0(1:leadingf0len), 'Running', 0,
     +                    mtotshowers, lastshower, -1, trial)
c
c       Calling the simulation steering routine.
c
        call scheduler(irc)
c
c       Control returned to Main program.
c
        call dati(datistr0(3))
        call cputime(.false., cpu02, runcputime)
        cpu0(1) = cpu0(1) + runcputime
c
        call Aires_status(2, crhost(1:crhostlen),
     +                    cruser(1:cruserlen),
     +                    leadingf0(1:leadingf0len), 'AfterRun', 0,
     +                    mtotshowers, lastshower, -11, trial)
c
c       Reopening the log file.
c
        call openlog('APPEND')
c
        tmpfn = 'End of run number '
        call intnice(jobnumber, 0, tmpfn(19:176), tmpfnlen)
        tmpfnlen = tmpfnlen + 19
        tmpfn(tmpfnlen:tmpfnlen) = '.'
c
c       Analysing the return code.
c
        arc      = abs(irc)
        taskover = (arc .eq. 0)
c
        if (taskover) then
c
c         Task completed. All showers were processed.
c         Completing the cio files.
c
          call completeallcio
c
          call putlog(2, .true., tmpfn(1:tmpfnlen))
c
          stsmsg = 'EndOfTask'
          jsts   = 9
          stoprc = 0
c
        else if (irc .ne. 1) then
c
c         Task not completed.
c
          if (irc .lt. 0) then
            stoprc   = 2
            stopfile = .false.
            call putlog(2, .true.,
     +        'Stop file detected. Processing stopped.')
          else
            stoprc   = 1
            stopfile = .true.
          endif
c
          call putlog(2, stopfile, tmpfn(1:tmpfnlen))
c
          tmpfn = 'Completed (total) showers: '
          call intnice(lastshower, 0, tmpfn(28:176), i)
          tmpfnlen = i + 29
          tmpfn(tmpfnlen-1:tmpfnlen) = ' ('
          call intnice(mtotshowers, 0, tmpfn(tmpfnlen+1:176), i)
          tmpfnlen = tmpfnlen + i + 1
          tmpfn(tmpfnlen:tmpfnlen) = ')'
          call putlog(2, .false., tmpfn(1:tmpfnlen))
c
          stsmsg = 'EndOfRun'
          jsts   = 8
c
          if (arc .ne. 2) then
c
c           Stopping without completion of a shower.
c
            call putlog(2, .false., 'Current shower is incomplete.')
          endif
c
        else
c
c         Return code from scheduler is 1. This means that there is
c         no work to do. Stopping.
c
          call putlog(2, .true.,
     +        'There is no work to do. All showers were simulated.')
c
          tmpfn = 'Total number of showers: '
          call intnice(mtotshowers, 0, tmpfn(26:176), tmpfnlen)
          tmpfnlen = tmpfnlen + 25
          call putlog(2, .false., tmpfn(1:tmpfnlen))
c
          call Aires_status(0, crhost(1:crhostlen),
     +                      cruser(1:cruserlen),
     +                      leadingf0(1:leadingf0len),
     +                      'EndOfTask', 0,
     +                      mtotshowers, lastshower, 9, i)
          goto 1100
c
        endif
c
        call cputime(.true., cpu0(4), runcputime)
        tmpfn = 'CPU time for this run: '
        call tnice(runcputime, 0, tmpfn(24:176), tmpfnlen)
        tmpfnlen = tmpfnlen + 23
        call putlog(2, .false., tmpfn(1:tmpfnlen))
c
c       Saving status variables of the cio system.
c
        call ciosavestatus
c
c       Closing the ciofiles (This implies also that the corresponding
c       scratch files will be effectively appended to the respective
c       main ones).
c
        call cioclose
c
c       Rewriting the internal dump file.
c
        call idfwrite
c
c       If control reaches this point, it means that the compressed
c       data files and idf file were written OK, so the status
c       information can be safely deleted.
c
        call ciormstatus
c
c       Updating the status file.
c
        call Aires_status(0, crhost(1:crhostlen),
     +                    cruser(1:cruserlen),
     +                    leadingf0(1:leadingf0len),
     +                    stsmsg(1:jsts), stoprc,
     +                    mtotshowers, lastshower, arc, i)
c
        if (irc .le. 0) goto 1100
        call dati(datistr0(4))
c
      enddo
 1100 continue
c
c     Closing the particle stacks.
c
      call closesta(0)
c
c     If the task is complete, write the adf file, perform final
c     statistics and write the output summary file.
c
      if (taskover) then
c
c       Writting the adf file.
c
        if (adfile) then
          call putlog(2, .true., 'Writing ASCII dump file.')
          call adfwrite
        endif
c
        call putlog(2, .true., 'Task completed.')
        tmpfn = 'Total number of showers: '
        call intnice(mtotshowers, 0, tmpfn(26:176), tmpfnlen)
        tmpfnlen = tmpfnlen + 25
        call putlog(2, .false., tmpfn(1:tmpfnlen))
c
        if (sryison) call putlog(2, .true., 'Writing summary file.')
        call summary
        call putlog(2, .true., 'End of processing.')
c
      call calculate_radio()
      call grid_statistics()
      endif
c
c     End of Main program.
c
      goto 1999
c
 3000 continue
c
c     End of processing when CheckOnly is enabled.
c
      call idatacheck(0)
c
      call putlog(0, .true.,
     +            'Stopping because the CheckOnly flag is On.')
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
c     End of file 'Aires.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
