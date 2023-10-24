c
c     FILE: kernel1.f                       Creation date: 16/JUL/1996.
c                                       LAST MODIFICATION: 26/MAY/2003.
c
c     This file contains the simulation-steering routines, first part:
c     Main kernel routine (scheduler), and main stack processing
c     routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine scheduler(irc)
c
c     Steering the simulation.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2000.
c
c     Arguments:
c     =========
c
c     irc............. (output, integer) Return code. 0 Means that
c                      the task was completed: All showers were
c                      simulated. |irc| = 2 means that processing
c                      was stopped after finishing a shower, and
c                      |irc| = 5 means that processing was stopped
c                      during the simulation of a shower. Negative
c                      values indicate that the reason for stopping
c                      was the presence of a stop file.
c                      irc = 1 means that the routine was invoked
c                      with no work to do.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ishower, firstshower, endshower
      integer           jrc
      double precision  tmp1, tmp2
      integer           currpsize
      logical           splitok, notempty
      logical           stopcheck
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if there is work to do.
c
      irc = 1
      if (lastshower .ge. mtotshowers) return
c
c     Entering the nuclear part of the program.
c     This is the CPU intensive part of the simulation program,
c     so from now on all data check will be reduced to the
c     truly essential. It is assumed that if control reaches this
c     point, then all input parameters and internal data have
c     already been tested.
c
c     Checking the number of showers.
c
      firstshower = lastshower + 1
      if (checknsh) then
        endshower = min(lastshower + runshowers, mtotshowers)
      else
        endshower = mtotshowers
      endif
c
c     Main shower loop.
c
      do ishower = firstshower, endshower
c
c       Compressed file splitting.
c
        if (shcomplete) then
c
c         Processing starts with a new shower.
c
          call reopenallcio(lastshower, splitok)
c
c         Setting the primary for the new shower. This will also
c         mark the current shower as incomplete.
c
          call newprimary
c
c         If new files were actually opened, then the internal dump
c         file must be updated. If the dump file is successfully
c         written, then the cio status file will be removed.
c
          if (splitok) then
            call idfwrite
            call ciormstatus
          endif
c
        endif
c
c       Closing the log file (temporarily)
c
        if (.not. lgfisclosed) call closelog
c
c       Performing the simulation...
c
        currpsize  = 0
c
c       Stack processing loop.
c       The shower is processed until all the stacks are empty.
c
 1010   continue
c
c       Checking the stop condition.
c
        if (currpsize .gt. stopchsize1) then
          stopchsize2 = stopchsize2 + currpsize
          currpsize   = 0
          if (stopcheck(tmp1, jrc)) goto 1110
          tmp2   = tmp1 - dstime
          dstime = tmp1
          if ((tmp2 .lt. (0.7d0 * stopchsecs)) .or.
     +        (tmp2 .gt. (1.4d0 * stopchsecs))) then
            tmp2 = min(stopchsecs / max(0.1d0, tmp2), 2.5d0)
            stopchsize1 = stopchsize1 * tmp2
            stopchsize1 = max(stopchsize1, 1)
          endif
        endif
c
c       Processing the stacks.
c
        call scheduler0(currpsize, notempty)
c
c       If every stack is empty then the shower is finished.
c       Otherwise processing must continue.
c
        if (notempty) goto 1010
c
c       The shower is finished. Calling the final processing routine.
c       This will also label the shower as completed.
c
        stopchsize2 = stopchsize2 + currpsize
        call aftershower
c
c       Checking the stop condition.
c
        if (stopcheck(tmp1, jrc)) goto 1100
c
      enddo
c
c     Task/Run completed or CPU time or stop condition enabled at
c     the end of a shower.
c
 1100 continue
c
      if (lastshower .ge. mtotshowers) then
        irc = 0
      else
        if (jrc .ge. 0) then
          irc = 2
        else
          irc = -2
        endif
      endif
c
      return
 1110 continue
c
c     A stop condition was enabled within a shower.
c
      call cputime(.false., tmp1, tmp2)
      cpu0(3) = cpu0(3) + tmp1 - cpu0(4)
      cpu0(4) = tmp1
c
      if (jrc .lt. 0) then
        irc = -5
      else
        irc = 5
      endif
      return
c
      end
c     --- End of routine scheduler
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine adscansta(apspr, dpspr, stacknumber)
c
c     Processing (in advance and decay mode) a given nonempty stack.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 2000.
c
c     Arguments:
c     =========
c
c     apspr........... (external) Particle stack first processing
c                      routine (see explanations within the code
c                      of routine "scheduler0").
c     dpspr........... (external) Particle stack second processing
c                      routine (see explanations within the code
c                      of routine "scheduler0").
c     stacknumber..... (input, integer) The number of the stack to
c                      process.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      external          apspr, dpspr
      integer           stacknumber
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ilast
      logical           istrue, fillsta
      integer           nspartic, nsparti0, ilast0
      integer           nrem, nextrem(maxstaentries)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Preparing the stack for sequential processing.
c
      istrue   = fillsta(stacknumber, nspartic)
      ilast0   = last0staentr(stacknumber)
c
c     Setting value of current stack for internal processing.
c
      currstack = stacknumber
c
c     Invoking the first processing routine.
c
      call apspr(nspartic, chpsta(stabeg(stacknumber)))
c
c     Evaluating the status of the processed particles:
c     Longitudinal development, ground level reaching, etc.
c     Also, the fast particle stack is recompressed.
c
      call stackmoni1(ilast0, stacknumber, ilast)
c
c     Checking first interaction depth.
c
      if (fstintnotset) call setfirstint(stacknumber, ilast0, ilast)
c
c     The second processing routine is invoked only if there are
c     particles in the stack.
c
      if (ilast .gt. stabeg0(stacknumber)) then
c
        nspartic = ilast - stabeg0(stacknumber)
        nsparti0 = nspartic
c
        call dpspr(nspartic, chpsta(stabeg(stacknumber)),
     +             nrem, nextrem)
c
c       Checking and recompressing the fast particle stack.
c
        call stackmoni2(nsparti0, stacknumber, nrem, nextrem, ilast)
c
      endif
c
c     Compressing the last part of the fast stack
c     and restoring normal processing mode for the stack.
c
      call restorestack(stacknumber, ilast0, ilast)
c
      return
c
      end
c     --- End of routine adscansta
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine adscansta0(apspr, dpspr, stacknumber)
c
c     Processing (in advance and decay mode) a given nonempty stack,
c     without tracking the longitudinal shower development, which
c     is left to the model's routine.
c     This is useful for particles having complex propagation rules.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1998.
c
c     Arguments:
c     =========
c
c     apspr........... (external) Particle stack first processing
c                      routine (see explanations within the code
c                      of routine "scheduler").
c     dpspr........... (external) Particle stack second processing
c                      routine (see explanations within the code
c                      of routine "scheduler").
c     stacknumber..... (input, integer) The number of the stack to
c                      process.
c
c

c      use iso_c_binding
c      use grid
      implicit none
      
c      type(c_ptr) vec_c_ptr
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      external          apspr, dpspr
      integer           stacknumber
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ilast
      logical           istrue, fillsta
      integer           nspartic, nsparti0, ilast0
      integer           nrem, nextrem(maxstaentries)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting value of current stack for internal processing.
c
      currstack = stacknumber
c
c     Preparing the stack for sequential processing.
c
      istrue   = fillsta(stacknumber, nspartic)
      nsparti0 = nspartic
      ilast0   = last0staentr(stacknumber)
c
c     Invoking the first processing routine.
c
c      vec_c_ptr = get_vector_ptr()
      
c      print*, "Made it here"
c      print*, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
c      call apspr(nspartic, chpsta(stabeg(stacknumber)), nrem, 
c     +nextrem, vec_c_ptr)
     
      call apspr(nspartic, chpsta(stabeg(stacknumber)), nrem, nextrem)

c
c     Evaluating the status of the processed particles:
c     Only ground level reaching, looses and low energy.
c     Also, the fast particle stack is recompressed.
c
      call stackmoni0(nsparti0, stacknumber, nrem, nextrem, ilast)
c
c     Checking first interaction depth.
c
      if (fstintnotset) call setfirstint(stacknumber, ilast0, ilast)
c
c     The second processing routine is invoked only if there are
c     particles in the stack.
c
      if (ilast .gt. stabeg0(stacknumber)) then
c
        nspartic = ilast - stabeg0(stacknumber)
        nsparti0 = nspartic
c
        call dpspr(nspartic, chpsta(stabeg(stacknumber)),
     +             nrem, nextrem)
c
c       Checking and recompressing the fast particle stack.
c
        call stackmoni2(nsparti0, stacknumber, nrem, nextrem, ilast)
c
      endif
c
c     Compressing the last part of the fast stack
c     and restoring normal processing mode for the stack.
c
      call restorestack(stacknumber, ilast0, ilast)
c
      return
c

c      call grid_statistics(vec_c_ptr)
      end
c     --- End of routine adscansta0
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine restorestack(stacknumber, ilast0, ilast)
c
c     Resseting an already processed stack to operate in non sequential
c     mode.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2003.
c
c     Arguments:
c     =========
c
c     stacknumber..... (input, integer) Stack number.
c     ilast0.......... (input, integer) Position of last sequential
c                      record before processing the stack.
c     ilast........... (input-output, integer) Current position of last
c                      record.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           stacknumber, ilast0, ilast
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Compressing the last part of the fast stack, and setting the
c     new end point for the stack.
c
      if (ilast .lt. ilast0) then
        do i = ilast0 + 1, laststaentr(stacknumber)
          ilast = ilast + 1
          chpsta(ilast) = chpsta(i)
        enddo
        laststaentr(stacknumber) = ilast
      else
        ilast = laststaentr(stacknumber)
      endif
c
c     Restoring normal processing mode for the stack.
c
      nstaentr(stacknumber)       = ilast - stabeg0(stacknumber)
      stasize(stacknumber)        = nstaentr(stacknumber) +
     +                              nhstaentr(stacknumber)
      last0staentr(stacknumber)   = stabeg0(stacknumber)
      freestaentr(1, stacknumber) = totstasize(stacknumber)
      freestaentr(2, stacknumber) = totstasize(stacknumber) / 2
c
c     Checking the stack sizes and setting green lights.
c
      call stacklights
c
      return
c
      end
c     --- End of routine restorestack
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacklights
c
c     Checking the stack sizes and setting green lights.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
      include 'pstackpar.f'
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      integer           maxhentr0, maxsize0, imxs0
      integer           maxsize1, imxs1
      logical           greenlight0, greenlight1
c
c     FIRST EXECUTABLE STATEMENT
c
      maxhentr0   = 0
      maxsize0    = 0
      do i = 1, npsta
        if (nhstaentr(i) .gt. maxhentr0) maxhentr0 = nhstaentr(i)
        if (stasize(i) .gt. maxsize0) then
          imxs0       = i
          maxsize0    = stasize(i)
        endif
      enddo
c
      if (maxhentr0 .le. 0) then
c
c       The "hard" stacks are empty. All stacks can be scanned normally
c       (all green lights) if the first category ones do not possess
c       too many entries.
c
c       Checking the sizes of category 1 stacks.
c
        maxsize1    = 0
        greenlight0 = .true.
        do j = 1, nstafirstcat
          i = stafirst(j)
          greenlight0 = greenlight0 .and.
     +                  (stasize(i) .lt. bigstasize(i))
          if (stasize(i) .gt. maxsize1) then
            imxs1       = i
            maxsize1    = stasize(i)
          endif
        enddo
c
        if (greenlight0) then
c
c         Enabling all stacks
c
          do i = 1, npsta
            greenlight(i) = .true.
          enddo
c
          ngreencalls = ngreencalls + 1
          nredcalls   = 0
c
        else
c
c         There is a considerable amount of category 1 entries: Only
c         one (category 1) stack is processed.
c
          do i = 1, npsta
            greenlight(i) = .false.
          enddo
c
          greenlight(imxs1) = .true.
c
          nredcalls   = nredcalls + 1
          ngreencalls = 0
c
        endif
c
      else
c
c       There are nonempty "hard" stacks. Selecting a subset of
c       stacks for processing.
c
        greenlight0 = .false.
c
        do i = 1, npsta
          greenlight(i) = .false.
        enddo
c
        greenlight(imxs0) = .true.
c
        if (stacateg(imxs0) .le. 0) then
c
c         Checking the sizes of category 1 stacks, and determining
c         if they must be processed too.
c
          maxsize1    = 0
          greenlight1 = .true.
          do j = 1, nstafirstcat
            i = stafirst(j)
            if (stasize(i) .gt. maxsize1) then
              imxs1       = i
              maxsize1    = stasize(i)
              greenlight1 = (maxsize1 .lt. littlestasize(i))
            endif
          enddo
c
          if (greenlight1) then
c
c           Category 1 stacks are almost empty, they will be enabled
c           together with the largest category 0 stack.
c
            do i = 1, nstafirstcat
              greenlight(stafirst(i)) = .true.
            enddo
c
          else
c
c           There is a considerable amount of category 1 entries: Only
c           one category 1 stack is processed.
c
            greenlight(imxs1) = .true.
c
          endif
c
c         When the largest stack is category 0, the stack processing
c         level is reset to process less entries per call.
c
          if (nredcalls .gt. 50) then
            stalevkey = 8
            midstalev = nmidlev
            nredcalls = 0
          endif
c
        endif
c
        nredcalls   = nredcalls + 1
        ngreencalls = 0
c
      endif
c
c     Resetting the stack "processing level".
c
      if (nredcalls .gt. 0) then
        if (nredcalls .gt. 90) then
          stalevkey = 8
          midstalev = 0
        else if (stalevkey .le. 0) then
          stalevkey = 24 - (16 * midstalev) / nmidlev
          midstalev = max(0, midstalev - 1)
        else
          stalevkey = stalevkey - 1
        endif
      else
        if (ngreencalls .le. 3) then
          stalevkey = 8
          midstalev = nmidlev
        else if (stalevkey .le. 0) then
          stalevkey = 24 - (16 * midstalev) / nmidlev
          midstalev = max(0, midstalev - 1)
        else
          stalevkey = stalevkey - 1
        endif
      endif
c
      maxhentries = maxhentr0
c
      return
c
      end
c     --- End of routine stacklights
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function stopcheck(currcpu, irc)
c
c     Checking the stopping conditions.
c     is found.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     currcpu......... (output, double precision) Current job cpu time
c                      (sec).
c     irc............. (output, integer) Signal label. If there is no
c                      stop signal it is set to 0 (in this case, the
c                      returned value stopcheck = .false.). If there
c                      is a not urgent stop signal it is set to -1
c                      (stopcheck = .false.). -2 means a file stop
c                      signal (urgent) (stopcheck =.true.), and 5
c                      means CPU time stopping condition (stopcheck =
c                      .true.) with missing stop file.
c
c     Return value: (logical) True if there is a urgent stop signal
c     ============  enabled.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      logical           stopcheck
      double precision  currcpu
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      logical           exists
      double precision  tmp1
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the existence of a stop file.
c
      do i = 1, nstopfiles
        j = i
        inquire (file = stopfile(i), exist = exists)
        if (exists) goto 1010
      enddo
c
c     No stop files.
c
      irc    = 0
      exists = .false.
c
      goto 1020
 1010 continue
c
c     Stopping due to the existence of one of the STOP files.
c
      irc       = -2
      stopcheck = .true.
c
c     Reading the stop flag. If a negative integer was placed
c     within the stop file, that means a non urgent stop signal. If
c     the integer is not negative, is missing or invalid, it will be
c     taken as an urgent stop signal.
c
      open(57, file = stopfile(j), status = 'OLD', err = 1015)
      read(57, *, err = 1015, end = 1015) i
c
c     The stop flag was read, checking if it is negative.
c
      if (i .lt. 0) then
        irc       = -1
        stopcheck = .false.
      endif
c
 1015 continue
      close(57)
      if (stopcheck) return
c
 1020 continue
c
c     Checking CPU time.
c
      call cputime(.false., tmp1, currcpu)
      if (checkcpu) then
        if (currcpu .ge. cpuperrun) goto 1030
      endif
c
c     CPU limit not reached yet. The return code remains the one
c     coming from the stop file check.
c
      stopcheck = .false.
      return
c
 1030 continue
c
c     Stopping due to CPU time condition. If control reaches this point
c     with an existing stop file, it means a non urgent stop signal,
c     and therefore the return code must not be set here.
c
      if (.not. exists) irc = 5
      stopcheck = .true.
      return
c
      end
c     --- End of routine stopcheck
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'kernel1.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
