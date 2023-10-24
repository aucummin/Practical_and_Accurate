c
c     FILE: Aires_status.f                  Creation date: 15/JUL/1996.
c                                       LAST MODIFICATION: 28/FEB/2000.
c
c     Routine to write a status file, useful to be processed by
c     the operating system.
c     NOTE: The format of the file may depended on the OS.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine Aires_status(rf, host, user, taskname, msg,
     +                        stoprc, totsh, lastsh, switch, abtrial)
c
c     Writing and (eventually) reading status information. The format
c     is compatible with the operating system (UNIX in this case).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1998, 2000.
c
c     Parameters:
c     ==========
c
c     rf.............. (input, integer) If 1 or 2 then the (eventually)
c                      existing status file is read to check if
c                      it correspond to a previous aborted run.
c                      If the read task name is equal to "taskname"
c                      then the trial number is read. If this operation
c                      ends successfully then if rf is 2 the number of
c                      trials is rewritten unchanged when positive,
c                      otherwise is set to 1; if rf is 1 it is
c                      incremented by 1 before rewritting it. If rf is
c                      zero the number of trials is set to zero; in any
c                      other case it is set to 1.
c     host............ (input, character*(*)) The name of the host
c                      where the program is running on.
c     user............ (input, character*(*)) The name of the user who
c                      is currently operating AIRES.
c     taskname........ (input, character*(*)) The complete task name.
c     msg............. (input, character*(*)) A text message.
c                      Usually contains the keywords: "Initializing",
c                      "Running", "AfterRun", "EndOfTask" or
c                      "EndOfRun".
c     stoprc.......... (input, integer) More information about the
c                      reasons to stop. This variable is 0 when msg
c                      is "Running", "AfterRun" or "EndOfTask". At an
c                      "EndOfRun" it can be 1 (no stop file found) or 2
c                      (stopping due to a stop file).
c     totsh........... (input, integer) The current total showers.
c     lastsh.......... (input, integer) Current value of the
c                      last completed shower.
c     switch.......... (input, integer) "Logical" switch.
c                      -1 means "Running", -11 means "AfterRun",
c                      0 means "Task completed", 2 means "Run ended
c                      after completing a shower", and 5 means "Run
c                      ended while processing a given shower".
c     abtrial......... (output, integer) The current value of the
c                      number of trials (1 if rc is not 1 or 2).
c                      If greater than one it means that the existing
c                      status file corresponds to an aborted run, and
c                      indicates the number of the current trial.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           rf
      character*(*)     host, user, taskname, msg
      integer           stoprc, totsh, lastsh, switch
      integer           abtrial
c
c     Declaration of internal variables and arrays.
c
      character*48      sline
      integer           i, j, trial
c
c     FIRST EXECUTABLE STATEMENT
c
      abtrial = 1
c
      if ((rf .eq. 1) .or. (rf .eq. 2)) then
c
c       Reading the old status file (if it exists),
c       and looking for the trial line.
c
        open(59, file = 'Aires.status', status = 'OLD', err = 1080)
c
c       Getting the task name.
c
 1010   continue
        read(59, 2010, err = 1080, end = 1080) sline
        do i = 14, 48
          j = i
          if (sline(i-13:i) .eq. 'Aires_TaskName') goto 1020
        enddo
        goto 1010
 1020   continue
c
c       If the task name is not equal to the current one, then
c       we ignore the status information.
c
        if (sline(j+3:min(48,j+len(taskname)+2)) .ne. taskname)
     +    goto 1080
c
c       Task names are equal. Reading number of trials.
c
 1040   continue
        read(59, 2010, err = 1080, end = 1080) sline
        do i = 11, 48
          j = i
          if (sline(i-10:i) .eq. 'Aires_Trial') goto 1050
        enddo
        goto 1040
 1050   continue
        read(sline(j+2:48), *, err = 1080) trial
c
        if (rf .eq. 1) trial = min(9, trial + 1)
        abtrial = max(1, trial)
c
 1080   continue
        close(59)
c
      else if (rf .eq. 0) then
c
        abtrial = 0
c
      endif
c
c     Opening the status file for writing.
c
      open(59, file = 'Aires.status', status = 'UNKNOWN', err = 3010)
c
      write(59, 2010, err = 3010)
     +          '# This is the status file. DO NOT EDIT.'
      write(59, 2010, err = 3010)
     +          'Aires_Host=''', host, ''''
      write(59, 2010, err = 3010)
     +          'Aires_UserName=''', user, ''''
      write(59, 2010, err = 3010)
     +          'Aires_TaskName=''', taskname, ''''
      write(59, 2010, err = 3010)
     +          'Aires_Msg=''', msg, ''''
      write(59, 2020, err = 3010)
     +          'Aires_StopRc=', stoprc
      write(59, 2020, err = 3010)
     +          'Aires_Trial=', abtrial
      write(59, 2030, err = 3010)
     +          'Aires_status=''', totsh, lastsh, switch, ''''
 2010 format(3a)
 2020 format(a, i1)
 2030 format(a, 3i10, a)
c
      close(59)
c
      if (abtrial .ge. 9) call errprint(0, '*', 4, 'Aires_status',
     +  'Too many aborted retrials.', 0, 0, 0, 0.d0, ' ')
c
      return
c
 3010 continue
c
c     Error message.
c
      call errprint(0, '*', 4, 'Aires_status',
     +     'Error opening or writing the status file.',
     +     0, 0, 0, 0.d0, ' ')
c
c
      end
c     --- End of routine Aires_status.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'Aires_status.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
