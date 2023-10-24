c
c     FILE: pstackmgr.f                     Creation date: 12/JUN/1996.
c                                       LAST MODIFICATION: 01/SEP/2000.
c
c     This file contains the particle stack (buffer) management
c     routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function defsta(openflag, nsta, firstcl, sizew, basefn)
c
c     Defining and initializing the stacks to use.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     openflag........ (input, integer) Open-only flag: If
c                      openflag is positive, then all stacks are
c                      initialized as empty; If > 1 also stack
c                      categories are set. Otherwise the
c                      effect of this routine limits to
c                      opening the scratch files.
c     nsta............ (input, integer) The number of stacks to
c                      initialize. It must be greater than zero
c                      and not greater than the maximum (maxnsta).
c     firstcl......... (input, integer, array(nsta)) Integer key
c                      labelling the "first class" stacks (when
c                      positive), that is, those stacks whose size
c                      controls the "red lights" to temporarily
c                      stop processing not first class stacks in
c                      certain circumstances.
c     sizew........... (input, double precision, array(nsta)) Array
c                      containing weights to determine the relative
c                      size of the stacks.
c     basefn.......... (input, character*(*)) The leading part
c                      of the file names to use as hard stacks.
c
c     Return value: (logical) True if nsta is a valid parameter
c     ============  and the scratch files could be opened
c                   successfully. False otherwise.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      logical           defsta
      integer           openflag, nsta
      integer           firstcl(nsta)
      double precision  sizew(nsta)
      character*(*)     basefn
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           hbunit, i
      character*144     hstafile
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the input parameter
c
      defsta = ((nsta .gt. 0) .and. (nsta .le. maxnsta))
      if (.not. defsta) return
c
      npsta = nsta
c
      if (openflag .gt. 0) then
c
c       Initializing the fast stacks.
c
        if (openflag .gt. 1) call setstacat(nsta, firstcl)
c
c       Clearing stacks and setting their maximum sizes.
c
        call resizesta(nsta, sizew)
c
      endif
c
c     Opening the scratch i/o files.
c
      do i = 1, nsta
c
        hbunit = pstaiou0 + i
        write(hstafile, 2010) basefn, '.pstack', hbunit
 2010   format(2a, i3.3)
c
        close (unit = hbunit)
        open (unit = hbunit, file = hstafile, status = 'UNKNOWN',
     +        form = 'UNFORMATTED',
     +        access = 'DIRECT', recl = hpstarecl,
     +        err = 3010)
c
      enddo
c
      return
c
 3010 continue
c
c     Error opening the scratch files.
c
      call errprint(0, operr, 4, 'defsta',
     +              ' ', 1, hbunit, 0, 0.d0, ' ')
      defsta = .false.
      return
      end
c     --- End of routine defsta.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine closesta(keep)
c
c     Finishing stack operations. Closing the files.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     keep............ (input, integer) 0 means delete the scratch
c                      files. Otherwise keep them.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           keep
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
c     Closing the hard (disk) stacks.
c
      if (keep .eq. 0) then
        do i = 1, npsta
          close (unit = pstaiou0 + i, status = 'DELETE')
        enddo
      else
        do i = 1, npsta
          close (unit = pstaiou0 + i, status = 'KEEP')
        enddo
      endif
c
      return
      end
c     --- End of routine closesta.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setstacat(nsta, firstcl)
c
c     Setting (or resetting) the stack categories.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     nsta............ (input, integer) The number of stacks to
c                      initialize. It must be greater than zero
c                      and not greater than the maximum (maxnsta).
c     firstcl......... (input, integer, array(nsta)) Integer key
c                      labelling the "first class" stacks (when
c                      positive), that is, those stacks whose size
c                      controls the "red lights" to temporarily
c                      stop processing not first class stacks in
c                      certain circumstances.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           nsta
      integer           firstcl(nsta)
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
c     Setting stack categories.
c
      nstafirstcat = 0
c
      do i = 1, nsta
        if (firstcl(i) .gt. 0) then
          stacateg(i) = 1
          nstafirstcat = nstafirstcat + 1
          stafirst(nstafirstcat) = i
        else
          stacateg(i) = 0
        endif
      enddo
c
      return
      end
c     --- End of routine setstacat.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine resizesta(nsta, sizew)
c
c     Setting (or resetting) the maximum size of the stacks,
c     proportionally to given weights.
c     The stacks are cleared before performing this operation.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998.
c
c     Arguments:
c     =========
c
c     nsta............ (input, integer) The number of stacks to
c                      initialize. It must be greater than zero
c                      and not greater than the maximum (maxnsta).
c     sizew........... (input, double precision, array(nsta)) Array
c                      containing weights to determine the relative
c                      size of the stacks.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           nsta
      double precision  sizew(nsta)
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, totentries, itotsizen
      double precision  sow, midsta
c
c     FIRST EXECUTABLE STATEMENT
c
c     Emptying the stacks.
c
      stasize(0) = 0
      do i = 1, nsta
        stasize(i)    = 0
        maxstasize(i) = 0
        totalloc(i)   = 0
      enddo
c
c     Setting the maximum stack sizes accordingly with the given
c     weights.
c
      sow = 0
      do i = 1, nsta
        sow = sow + abs(sizew(i))
      enddo
c
      if (sow .le. 0) call errprint(0, '$A34', 4, 'resizesta',
     +                              'The stack weights sum zero.',
     +                              0, 0, nsta, sizew, ' ')
c
      sow = (maxstaentries - nsta * minstaentries) / sow
c
      if (sow .lt. 0) call errprint(0, '$A34', 4, 'resizesta',
     +                              'Not enough total size.',
     +                              1, maxstaentries, 0, 0.d0, ' ')
c
c     Setting stack sizes.
c
      totentries  = 0
      totstasize(0) = 0
      do i = 1, nsta - 1
        totstasize(i) = minstaentries + sow * abs(sizew(i)) + 0.5d0
        totentries    = totentries + totstasize(i)
      enddo
      totstasize(nsta) = maxstaentries - totentries
c
c     Defining stack boundaries and completing the stack clearing.
c
      supstasize  = 0
      maxhentries = 0
      staend(0)   = 0
c
      do i = 1, nsta
c
        if (totstasize(i) .gt. supstasize) supstasize = totstasize(i)
c
        stabeg(i) = staend(i - 1) + 1
        staend(i) = staend(i - 1) + totstasize(i)
        itotsizen = min(totstasize(i), maxentriesbig)
c
        midsta = 0.5d0 * itotsizen
c
        do j = 0, mxmidlev
          if (midsta .lt. minmidentries) midsta = minmidentries
          middlestaentr(i, j) = staend(i - 1) + midsta
          q3staentr(i, j)     = staend(i - 1) + 1.5d0 * midsta
          midsta              = midsta * midlevratio
        enddo
c
        stasize(i)        = 0
        nstaentr(i)       = 0
        freestaentr(1, i) = totstasize(i)
        freestaentr(2, i) = totstasize(i) / 2
        last0staentr(i)   = staend(i - 1)
        laststaentr(i)    = staend(i - 1)
        nhstaentr(i)      = 0
        nrhoper(i)        = 0
        nwhoper(i)        = 0
c
        littlestasize(i)  = littlesizefrac * itotsizen
        bigstasize(i)     = bigsizefrac * itotsizen
        littlestasize(i)  = max(littlestasize(i), 2 * minstaentries)
        j                 = littlestasize(i) + plushentries
        bigstasize(i)     = max(bigstasize(i), j)
c
      enddo
      stabeg(nsta + 1) = staend(nsta) + 1
c
c     Defining the number of significant processing levels.
c
      nmidlev = mxmidlev
      do j = mxmidlev, 1, -1
        do i = 1, nsta
          if ((middlestaentr(i, j) - stabeg0(i)) .gt. minmidentries)
     +       goto 1010
        enddo
        nmidlev = j
      enddo
 1010 continue
c
      nmidlev2 = log(0.12d0) / log(midlevratio) + 0.5d0
      nmidlev2 = max(0, min(nmidlev - 1, nmidlev2))
c
c     Setting the initial processing level and turning the green
c     lights on.
c
      do i = 1, nsta
        greenlight(i) = .true.
      enddo
c
      ngreencalls = 0
      nredcalls   = 0
      midstalev   = nmidlev
      stalevkey   = 10
c
      return
      end
c     --- End of routine resizesta.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine clearsta(ista)
c
c     Clearing a given stack (making it empty without closing the
c     scratch file).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     ista............ (input, integer) The stack to clear.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           ista
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      stasize(ista) = 0
      if (ista .le. 0) return
c
      stasize(ista)        = 0
      last0staentr(ista)   = stabeg0(ista)
      laststaentr(ista)    = stabeg0(ista)
      freestaentr(1, ista) = totstasize(ista)
      freestaentr(2, ista) = totstasize(ista) / 2
      nstaentr(ista)       = 0
      nhstaentr(ista)      = 0
      nrhoper(ista)        = 0
      nwhoper(ista)        = 0
c
      return
      end
c     --- End of routine clearsta.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacp2s(sequ, irc)
c
c     This subroutine writes all the stacks into a sequential
c     unformatted file. The sequential file is assumed to be
c     already opened, and it is not closed after completing
c     the operation.
c     The stacks are left unchanged.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999.
c
c     Arguments:
c     =========
c
c     sequ............ (input, integer) The logical unit corresponding
c                      to the unformatted sequential file.
c     irc............. (output, integer) Return code. 0 means
c                      successful return, 5 means an error while
c                      writing to the sequential unit. 8 means
c                      an error reading the direct file(s).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           sequ, irc
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           hbunit, i, j, k
      double precision  fpstaux(maxstalen)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing general parameters.
c
      write(sequ, err = 3020) npsta
c
c     Size limiting related variables.
c
      write(sequ, err = 3020) midstalev, stalevkey, maxhentries,
     +                        ngreencalls, nredcalls,
     +                        (greenlight(i), i = 1, npsta)
c
c     Copying the records to the sequential file.
c
      do i = 1, npsta
c
        write(sequ, err = 3020) nstaentr(i), nhstaentr(i),
     +                          totalloc(i), maxstasize(i),
     +                          nrhoper(i), nwhoper(i)
c
        if (nstaentr(i) .gt. 0) then
c
c         Copying the fast part of the stack.
c
          do k = stabeg(i), laststaentr(i)
            write (sequ, err = 3020)
     +            (fpsta(j, k), j = 1, maxstalen)
          enddo
        endif
c
        if (nhstaentr(i) .gt. 0) then
c
c         Copying the swapped part of the stack.
c
          hbunit  = pstaiou0 + i
          do k = 1, nhstaentr(i)
            read  (hbunit, rec = k, err = 3010)
     +            (fpstaux(j), j = 1, maxstalen)
            write (sequ, err = 3020)
     +            (fpstaux(j), j = 1, maxstalen)
          enddo
        endif
c
      enddo
c
      irc = 0
      return
c
 3010 continue
c
c     Error reading stack file.
c
      call errprint(0, rderr, 3, 'stacp2s',
     +              ' ', 1, hbunit, 0, 0.d0, ' ')
      irc = 8
      return
c
 3020 continue
c
c     Error writing sequential file.
c
      irc = 5
      return
c
      end
c     --- End of routine stacp2s
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stacp2d(sequ, irc)
c
c     This subroutine reads all the stacks from a sequential
c     unformatted file. The sequential file is assumed to be
c     already opened, and it is not closed after completing
c     the operation.
c     The stacks are cleared before reading data in.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000.
c
c     Arguments:
c     =========
c
c     sequ............ (input, integer) The logical unit corresponding
c                      to the unformatted sequential file.
c     irc............. (output, integer) Return code. 0 means
c                      successful return, 5 means an error while
c                      reading from the sequential unit. 8 means
c                      an error writing the direct file(s).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           sequ, irc
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           hbunit, i, j, k
      integer           firstp0, maxfr, recn
      double precision  fpstaux(maxstalen)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Reading general parameters.
c
      read(sequ, err = 3020, end = 3020) j
c
      if (j .ne. npsta) call errprint(0, '$A34', 4, 'stacp2d',
     +   'The number of stacks saved is NE to the allocated ones.',
     +   1, j, 0, 0.d0, ' ')
c
c     Size limiting related variables.
c
      read(sequ, err = 3020, end = 3020)
     +    midstalev, stalevkey, maxhentries,
     +    ngreencalls, nredcalls,
     +    (greenlight(i), i = 1, npsta)
c
c     Copying the records from the sequential file.
c
      do i = 1, npsta
c
c       Clearing the stack.
c
        call clearsta(i)
c
c       Reading the number of records.
c
        read(sequ, err = 3020, end = 3020) nstaentr(i), nhstaentr(i),
     +                                     totalloc(i), maxstasize(i),
     +                                     nrhoper(i), nwhoper(i)
c
        stasize(i)     = nstaentr(i) + nhstaentr(i)
        firstp0        = stabeg0(i)
        maxfr          = min(nstaentr(i), totstasize(i))
        laststaentr(i) = firstp0 + maxfr
        hbunit         = pstaiou0 + i
        recn           = 0
c
        if (nstaentr(i) .gt. 0) then
c
c         Reading the fast part of the stack.
c
          do k = 1, maxfr
            read (sequ, err = 3020, end = 3020)
     +           (fpsta(j, firstp0 + k), j = 1, maxstalen)
          enddo
c
c         If the number of "fast" records cannot be completely
c         stored due to (eventual) changes in the fast stack size,
c         they are written in the corresponding file.
c
          do k = maxfr + 1, nstaentr(i)
            recn = recn + 1
            read  (sequ, err = 3020, end = 3020)
     +            (fpstaux(j), j = 1, maxstalen)
            write (hbunit, rec = recn, err = 3010)
     +            (fpstaux(j), j = 1, maxstalen)
          enddo
          nstaentr(i) = maxfr
c
        endif
c
        if (nhstaentr(i) .gt. 0) then
c
c         Reading the swapped part of the stack.
c
          do k = 1, nhstaentr(i)
            recn = recn + 1
            read  (sequ, err = 3020, end = 3020)
     +            (fpstaux(j), j = 1, maxstalen)
            write (hbunit, rec = recn, err = 3010)
     +            (fpstaux(j), j = 1, maxstalen)
          enddo
c
        endif
c
        nhstaentr(i) = recn
c
      enddo
c
      irc = 0
      return
c
 3010 continue
c
c     Error writing stack file.
c
      call errprint(0, wrerr, 3, 'stacp2s',
     +              ' ', 1, hbunit, 0, 0.d0, ' ')
      irc = 8
      return
c
 3020 continue
c
c     Error reading sequential file.
c
      irc = 5
      return
c
      end
c     --- End of routine stacp2d
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function allstae(ista, nentry)
c
c     This function allocates data entries in stack ista, and marks
c     them as busy. Hard writting of the stack is done if needed.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999.
c
c     Arguments:
c     =========
c
c     ista............ (input, integer) The stack to use. No
c                      bound check is performed on this variable.
c     nentry.......... (input, integer) The number of data entries
c                      to allocate. This variable must be greater
c                      than zero.
c
c     Return value: (integer) An address between 0 and maxstaentries-1
c     ============  labelling the record previous to the first
c                   allocated one, that is, return_value + i is the
c                   address of data entry i (i = 1,...,nentry).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      integer           allstae
      integer           ista, nentry
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           hbunit, newlast, recn, i, j, mstart1
      integer           sbegr, mendr, nent0
c
c     FIRST EXECUTABLE STATEMENT
c
      mstart1 = laststaentr(ista)
      newlast = mstart1 + nentry
c
      if (newlast .gt. staend(ista)) then
c
        if (nentry .gt. freestaentr(1, ista))
     +     call errprint(0, alerr, 4, 'allstae',
     +                   ' ', 1, nentry, 0, 0.d0, ' ')
c
c       There is no enough space to allocate.
c       Room will be made by hard writing a part of the stack.
c
c       Writing the first half of the (non sequential part of the)
c       stack, or more if necessary.
c
        hbunit = pstaiou0 + ista
        recn   = nhstaentr(ista)
        sbegr  = last0staentr(ista) + 1
        nent0  = max(freestaentr(2, ista), nentry)
        mendr  = sbegr + nent0
c
        do i = sbegr, mendr - 1
          recn = recn + 1
          write (hbunit, rec = recn, err = 3010)
     +          (fpsta(j, i), j = 1, maxstalen)
        enddo
c
c       Shifting the remaining stack entries.
c
        do i = mendr, mstart1
          chpsta(i - nent0) = chpsta(i)
        enddo
c
        mstart1           = mstart1 - nent0
        nwhoper(ista)     = nwhoper(ista) + 1
        nhstaentr(ista)   = recn
        nstaentr(ista)    = mstart1 - stabeg0(ista)
        newlast           = mstart1 + nentry
c
      endif
c
c     Allocating the entries.
c
      totalloc(ista)    = totalloc(ista) + nentry
      laststaentr(ista) = newlast
      nstaentr(ista)    = nstaentr(ista) + nentry
      stasize(ista)     = stasize(ista) + nentry
      if (stasize(ista) .gt. maxstasize(ista))
     +   maxstasize(ista) = stasize(ista)
c
      allstae = mstart1
      return
c
 3010 continue
c
c     Error writing stack file.
c
      call errprint(0, wrerr, 0, 'allstae',
     +              ' ', 1, hbunit, 0, 0.d0, ' ')
      allstae = -1
      return
c
      end
c     --- End of routine allstae.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fillsta(ista, nseqentr)
c
c     Preparing the stack for sequential processing. Final filling
c     will be in the range indicated by the current processing level
c     (50%-75% for the default level 0) if possible. The reserved
c     region is reset to the final stack size. This region is assumed
c     to be contiguous.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998.
c
c     Arguments:
c     =========
c
c     ista............ (input, integer) The stack to use. No
c                      bound check is performed on this variable.
c     nseqentr........ (output, integer) The number of entries reserved
c                      for the sequential processing region.
c
c     Return value: (logical) True if the operation was
c     ============  successfully completed. False means that the
c                   stack is empty.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pstackpar.f'
c
c     Declaration of arguments.
c
      logical           fillsta
      integer           ista, nseqentr
c
c     Declaration of shared data.
c
      include 'pstackcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           hbunit, necessary, recn0
      integer           i, j, i0, i1, free
c
c     FIRST EXECUTABLE STATEMENT
c
      if (stasize(ista) .le. 0) then
        fillsta = .false.
        return
      else
        fillsta = .true.
      endif
c
c     Checking the current filling level.
c
      if (laststaentr(ista) .lt. middlestaentr(ista, midstalev)) then
c
c       Filling is below lower threshold. Reading from hard stack if
c       possible.
c
        necessary = min(q3staentr(ista, midstalev) - laststaentr(ista),
     +                  nhstaentr(ista))
c
        if (necessary .gt. 0) then
c
c         Reading from the file stack.
c
          hbunit  = pstaiou0 + ista
          recn0   = nhstaentr(ista) - necessary
          i0      = laststaentr(ista)
c
          do i = 1, necessary
            i1 = i0 + i
            read (hbunit, rec = recn0 + i, err = 3010)
     +           (fpsta(j, i1), j = 1, maxstalen)
          enddo
c
          laststaentr(ista) = i1
          nstaentr(ista)    = nstaentr(ista) + necessary
          nhstaentr(ista)   = recn0
          nrhoper(ista)     = nrhoper(ista) + 1
c
        endif
c
      endif
c
c     Now filling is (hopefully) greater than the lower threshold.
c     Preparing for sequential processing.
c
      last0staentr(ista)   = min(laststaentr(ista),
     +                           q3staentr(ista, midstalev))
      nseqentr             = last0staentr(ista) - stabeg0(ista)
      free                 = staend(ista) - last0staentr(ista)
      freestaentr(1, ista) = free
      freestaentr(2, ista) = free / 2
c
      return
c
 3010 continue
c
c     Error reading stack file.
c
      call errprint(0, rderr, 0, 'fillsta',
     +              ' ', 1, hbunit, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine fillsta.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'pstackmgr.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
