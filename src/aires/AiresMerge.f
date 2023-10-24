c
c     FILE: AiresMerge.f                    Creation date: 16/AUG/2003.
c                                       LAST MODIFICATION: 24/FEB/2004.
c
c                       IDF/ADF FILE MERGING PROGRAM
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
c     Fermilab 2003; La Plata 2004.
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
      program AiresMerge
c
c     Main program for merging data from various IDF/ADF files onto
c     a single ADF file.
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
      include 'initmergecomm.f'
      include 'maincomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*256     iline
      integer           liline
      logical           merging
      character*32      mergelabel
      integer           icc
      integer           i, i1, i2
      character*128     mergeidlset
      integer           lidlset
      integer           wordlocate
c
      data              mergeidlset
     +  / 'MergeFiles|FileDirectory|TaskName|TotalShowers|Shell' /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Program code.
c
      pgmcode = 2201
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
c     Some initializations needed before starting the merging process.
c
      call basicstart0
c
      lgfisclosed = .true.
      mmacutok    = .true.
c
      nmergedfiles  = 0
      amtaskname    = taskname
      amtasknamelen = tasknamelen
      amwdirname    = ' '
      amwdirnamelen = 0
      amtotshwset   = 0
c
      call strim(-1, mergeidlset, lidlset)
c
c     Reading instrunctions.
c
      call putlog(0, .true.,
     +            'Reading instructions from standard input unit.')
c
      merging = .false.
c
 1010 continue
      read(5, 2010, err = 3010, end = 1100) iline
 2010 format(a)
      call scleancomm(256, iline, liline)
      if (liline .le. 0) goto 1010
c
      if ( (iline(1:liline) .eq. 'Exit') .or.
     +     (iline(1:liline) .eq. 'x')         ) goto 1120
      if (iline(1:liline) .eq. 'End')           goto 1100
c
      i2 = 0
      call nextword(iline, liline, i1, i2)
c
      if (merging) then
c
c       Within mergin process: Check for labels and/or files.
c
        if (iline(1:1) .eq. labelchar) then
          if (iline(1:i2) .eq. mergelabel) then
            merging = .false.
          endif
        else
          call mergefile(iline(1:i2))
        endif
c
      else
c
c       Check and process other instructions.
c
        if (iline(1:1) .eq. labelchar) goto 1010
c
        icc = wordlocate(iline(i1:i2), mergeidlset(1:lidlset), '|', 4)
c
        if (icc .le. 0) then
c
          call errprint(0, '$A07', 3, 'AiresMerge (MAIN)',
     +                  iline(1:liline), 0, 0, 0, 0.d0, ' ')
c
        else if (icc .eq. 1) then
c
c         Instruction code 1: MergeFiles
c
          call nextword(iline, liline, i1, i2)
c
          if (i2 .ge. i1) then
            if (iline(i1:i1) .eq. labelchar) then
              mergelabel = iline(i1:i2)
              merging    = .true.
            else
              call mergefile(iline(i1:i2))
            endif
          else
            call errprint(0, '$A10', 3, 'AiresMerge (MAIN)',
     +                    iline(1:liline), 0, 0, 0, 0.d0, ' ')
          endif
c
        else if (icc .eq. 2) then
c
c         Instruction code 2: Filedirectory
c
          call setiowdir(iline, liline, i1, i2)
c
        else if (icc .eq. 3) then
c
c         Instruction code 3: TaskName
c
          call setotaskname(iline, liline, i1, i2)
c
        else if (icc .eq. 4) then
c
c         Instruction code 4: TotalShowers
c
          call setototshowers(iline, liline, i1, i2)
c
        else if (icc .eq. 5) then
c
c         Instruction code 5: Shell
c
          call nextword(iline, liline, i1, i2)
          if (i1 .le. liline) then
            call sysspawn(iline(i1:liline), ' ', ' ', i)
          endif
c
        else
c
c         ... should never happen.
c
          call errprint(0, '$A08', 4, 'AiresMerge (MAIN)',
     +                  ' ', 0, 0, 0, 0.d0, ' ')
c
        endif
      endif
c
      goto 1010
 1100 continue
c
      if (merging) then
        call errprint(0, '*', 4, 'AiresMerge (MAIN)',
     +    'End of input data set reached while reading list of$' //
     +    'files to be merged. Stopping.',
     +    0, 0, 0, 0.d0, ' ')
      endif
c
      if (nmergedfiles .le. 0) then
        call errprint(0, '*', 2, 'AiresMerge (MAIN)',
     +    'No files to merge. Exiting.',
     +    0, 0, 0, 0.d0, ' ')
        goto 1120
      endif
c
c     All instructions already read in, and input files
c     successfully processed. Writing output ADF.
c
      call setmergedout
      call adfwrite
c
c     Merge complete.
c
      call putlog(0, .true.,
     +            'All files successfully merged into output ADF.')
      call intnice(nmergedfiles, 0, iline, liline)
      call putlog(0, .false., iline(1:liline) //
     +            ' file(s) successfully processed.')
c
 1120 continue
c
c     Deleting internal files.
c
      call rmfile(shzut, shzfn)
      call rmfile(shzut, amshzfn)
      call rmfile(rmkut, rmkfn)
c
c     All done, exiting.
c
      stop
c
c     Error exits.
c
 3010 continue
      call errprint(0, irerr, 4, 'AiresMerge (MAIN)',
     .              ' ', 0, 0, 0, 0.d0, ' ')
c
c     End of Main program (Merge).
c
      end
c     --- End of main program.
c
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c<===><===><===><===><===><===><===>*<===><===><===><===><===><===><===>
c
c     End of file 'AiresMerge.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
