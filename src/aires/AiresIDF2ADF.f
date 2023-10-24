c
c     FILE: AiresIDF2ADF.f                  Creation date: 16/APR/1999.
c                                       LAST MODIFICATION: 20/AUG/2003.
c
c                    IDF TO ADF FILE CONVERTING PROGRAM
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
c     Fermilab 1999, La Plata 2000, 2003; Fermilab 2003.
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
      program AiresIDF2ADF
c
c     Main program for generating ASCII dump files (ADF) from internal
c     (binary) dump files (IDF).
c
c
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
      character*256     tmpfn
      logical           exists
      integer           irc, tmpfnlen, leadinglen
      integer           i, i1, i2
      logical           smatch
c
c     FIRST EXECUTABLE STATEMENT
c
c     Program code.
c
      pgmcode = 2602
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
 1005 continue
c
c     Some initializations needed before performing the upgrade.
c
      call init0
c
      lgfisclosed = .true.
      mmacutok    = .true.
c
c     Asking for the idf file name.
c
 1010 continue
      write(6, 2010) 'Enter task or idf file name (blank to exit).'
      write(6, 2005) 'Task/file name? '
 2005 format(a, $)
      read(5, 2010) tmpfn
 2010 format(a)
      call scleancomm(256, tmpfn, tmpfnlen)
c
      if (tmpfnlen .le. 0) goto 1100
c
c     Getting the file or task name.
c
      i2 = 0
      call nextword(tmpfn, tmpfnlen, i1, i2)
      if (tmpfn(i1:i2) .eq. comchar) goto 1010
c
      if (smatch(tmpfn(i1:i2), 'TaskName', 4)) then
c
c       Explicit task name specified.
c
        call nextword(tmpfn, tmpfnlen, i1, i2)
        if (i1 .gt. i2) goto 1100
        tmpfnlen = 0
        do i = i1, i2
          tmpfnlen = tmpfnlen + 1
          tmpfn(tmpfnlen:tmpfnlen) = tmpfn(i:i)
        enddo
c
        leadinglen = tmpfnlen
        tmpfn      = tmpfn(1:tmpfnlen) // idfext
        tmpfnlen   = leadinglen + 4
c
      else
c
c       File/task name specified.
c
        do i = tmpfnlen, 1, -1
          leadinglen = i
          if (tmpfn(i:i) .eq. '.') goto 1020
        enddo
c
c       No dot in the file/task name, appnding the extension.
c
        leadinglen = tmpfnlen
        tmpfn      = tmpfn(1:tmpfnlen) // idfext
        tmpfnlen   = leadinglen + 4
        goto 1030
c
 1020   continue
c
c       Extension explicitly specified.
c
        leadinglen = leadinglen - 1
c
 1030   continue
      endif
c
c     Checking the existence of the file.
c
      inquire(file = tmpfn, exist = exists)
c
      if (.not. exists) then
        call errprint(0, '*', 3, 'AiresIDF2ADF',
     +                'The specified file does not exist.',
     +                0, 0, 0, 0.d0, tmpfn(1:tmpfnlen))
        write(6, *)
        goto 1010
      endif
c
c     The file exists, reading it with idfread, and initializing
c     the necessary variables.
c     Notice that the stacks are not being read.
c
      leadingfnlen(2) = leadinglen
      leadingfn(2)    = tmpfn(1:leadinglen)
c
      call idfread(' ', 0, 3, irc)
c
      if (irc .ne. 0) then
        call errprint(0, '*', 2, 'AiresIDF2ADF',
     +                'Nonzero return code from "idfread".',
     +                1, irc, 0, 0.d0, ' ')
        goto 1010
      endif
c
c     Setting the "x" variables.
c
      wdirname(1)    = xwdirname
      wdirnamelen(1) = xwdirnamelen
      taskname       = xtaskname
      tasknamelen    = xtasknamelen
      tasknamever    = xtasknamever
      tasknameset    = xtasknameset
      mtotshowers    = xtotshowers
      runshowers     = xrunshowers
      cpuperrun      = xcpuperrun
      processjobs    = xprocessjobs
      totshwset      = xtotshwset
      runshwset      = xrunshwset
      cpurunset      = xcpurunset
      processjobsset = xprocessjobsset
c
c     Phase 4 initialization (summary only).
c
      call init4s
c
c     Setting the "original_version" string. The corresponding flag is
c     set to "true" so the string will not be set within "idfwrite",
c     also, the number of upgrades will be increased in 1.
c
      oversionset      = .true.
      original_version = idfversion
      nverchanges      = nverchanges + 1
c
c     Generating the ADF file. It will be written using the current
c     ADF format.
c
      call adfwrite
c
c     Upgrade complete.
c
      write(6, *)
      write(6, 2010) '====  ASCII dump file successfully written.'
      write(6, *)
c
c     Deleting internal files.
c
      if (lastshower .gt. 0) call rmfile(shzut, shzfn)
      if (remark) call rmfile(rmkut, rmkfn)
c
      goto 1005
c
 1100 continue
c
c     All done, exiting.
c
      write(6, 2010) 'Exiting.'
c
c     End of Main program (IDF2ADF).
c
      end
c     --- End of main program.
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
c     End of file 'AiresIDF2ADF.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
