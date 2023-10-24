c
c     FILE: cioaux.f                        Creation date: 19/DEC/1996.
c                                       LAST MODIFICATION: 17/JUL/2003.
c
c     Some auxiliary routines used to manage the Aires cio system
c     during the air shower simulations.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine defineallcio
c
c     Checking the cio files needed for a program and marking them as
c     defined.
c     This routine is called after invoking "cioinit0w", that is
c     after initializing the cio system.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'pclepar.f'
      include 'ciopar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'pclecomm.f'
      include 'ciocomm.f'
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the files defined in the cio initialization step.
c     Special index variables are set in routine init4s.
c
      nciofiles = 0
c
      do i = 1, nhwciofiles
        if (ciofcateg(i) .gt. 0) then
          nciofiles   = nciofiles + 1
          ciofiles(i) = nciofiles
        else
          ciofiles(i) = 0
        endif
      enddo
c
c     Initializing the particle save flags, and indices labelling files
c     by "physical" meaning. By default, no particles are saved for
c     any file, except the ground particles file, where all particles
c     will be saved unless explicitly disabled by the user.
c
      do j = 1, nciofiles
        do i = -maxpcle, maxncode
          allpclesave(i, j) = .false.
        enddo
      enddo
c
c     Compressed output file indices.
c
      do i = 1, nciofiles
        if (pofilext(i) .eq. '.grdpcles') then
          gpcleciofile = i
        else if (pofilext(i) .eq. '.lgtpcles') then
          longiciofile = i
        else if (pofilext(i) .eq. '.pcletrace') then
          traceciofile = i
        endif
      enddo
c
c     Enabling particles to be saved into the ground particle file.
c
      do i = -maxpcle, maxncode
        allpclesave(i, gpcleciofile) = allpgroup(1, i)
      enddo
c
c     The first assumption is that all the files are going to be used.
c
      nciofilesu = nciofiles
      do i = 1, nciofiles
        ciofilesu(i) = i
      enddo
c
      return
      end
c     --- End of routine defineallcio
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine defineusedcio
c
c     Checking the cio files after having scanned the input file,
c     to definitively set the files to be used.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'pclepar.f'
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pclecomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           j, k
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the files to be used accordingly with the
c     related input variables: A file will not be opened if there
c     are no particles to be saved in it.
c
      nciofilesu = 0
      do j = 1, nciofiles
         anypinfile(j) = .false.
        do k = -maxpcle, maxncode
          if (allpclesave(k, j)) anypinfile(j) = .true.
        enddo
        if (anypinfile(j)) then
          nciofilesu = nciofilesu + 1
          ciofilesu(nciofilesu) = j
        endif
      enddo
c
      return
      end
c     --- End of routine defineusedcio
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine openallcio(newsw)
c
c     Opening all the defined cio files for writing.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     newsw........... (input, integer) New-old switch. If 0 or 1
c                      a new file is initialized, otherwise the file
c                      is tested for existence. Also if newsw is
c                      positive and not equal to 2, then a scratch file
c                      is opened for data writing, otherwise the main
c                      file remains open and positioned at the current
c                      end of file. If newsw is negative it is assumed
c                      that a scratch file is already opened and an
c                      alternative FORTRAN unit number (59) is used
c                      therefore.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           newsw
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     It is assumed that the initializing routine routine has already
c     been called.
c
c     Opening the defined files.
c
      do i = 1, nciofilesu
        call cioopenw(ciofilesu(i), newsw,
     +                leadingfn(mxwdir)(1:leadingfnlen(mxwdir)))
      enddo
c
      return
      end
c     --- End of routine openallcio
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine reopenallcio(splitidx, splitok)
c
c     Closing all compressed files and reopening them with new numbers
c     in the file names.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2000.
c
c
c     Arguments:
c     =========
c
c     splitidx........ (input, integer) Splitting index. If compressed
c                      file splitting is enabled, splitidx > 0 and
c                      if mod(splitidx, ciosplitevery) = 0, then all
c                      the compressed files are closed and reopened
c                      (with new file numbers in the name).
c     splitok......... (output, logical) True if the file splitting
c                      operation was actually performed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           splitidx
      logical           splitok
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the splitting condition.
c
      splitok = (ciofsplit .and. (nciofilesu .gt. 0) .and.
     +           (splitidx .gt. 0) .and.
     +           (mod(splitidx, ciosplitevery) .eq. 0))
c
      if (splitok) then
c
c       Files must be closed and reopened.
c
c       Saving first status variables of the cio system.
c
        call ciosavestatus
c
        do i = 1, nciofilesu
          call cioreopenw(ciofilesu(i), lastciopointer(i),
     +                    ciosplitevery, 1,
     +                    leadingfn(mxwdir)(1:leadingfnlen(mxwdir)))
        enddo
c
      endif
c
      return
      end
c     --- End of routine reopenallcio
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine markallcio
c
c     Saving the current pointers of all opened cio files.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
      integer           ciogeteofblock
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, nciofilesu
        lastciopointer(i) = ciogeteofblock(ciofilesu(i))
      enddo
c
      return
      end
c     --- End of routine markallcio
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine completeallcio
c
c     Hard writing the cio buffers up to the last record marked
c     (This is used at the end of a task to leave the cio files properly
c     finished).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, nciofilesu
        call cioflush(ciofilesu(i), lastciopointer(i))
      enddo
c
      return
      end
c     --- End of routine completeallcio
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine allciosave(nfiles, files, rectype,
     +                      nidat, intfields, realfields, iws)
c
c     Writing a record into all the defined cio files.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     nfiles.......... (input, integer) Number of files to write. If
c                      nfiles is zero or negative, then the record is
c                      saved in all available files.
c     files........... (input, integer, array(nfiles)) List of files
c                      where to save the record. Used only if nfiles
c                      is positive.
c     rectype......... (input, integer) Record type. Must be a record
c                      type present in all the defined files.
c     nidat........... (input, integer) The number of integer data
c                      entered in array "intfields".
c     intfields....... (input, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). No control is done
c                      on the integers ranges.
c     realfields...... (input, double precision, array(*)) Real fields
c                      of the record. Ranges are not checked.
c     iws............. (scratch, integer, array(*)) Working integer
c                      array. Its length must not be less than the
c                      length of "intfields".
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
      integer           nfiles, rectype, nidat
      integer           files(nfiles)
      integer           intfields(1)
      double precision  realfields(0:1)
      integer           iws(1)
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
      if (nfiles .le. 0) then
c
        do i = 1, nciofilesu
          do j = 1, nidat
            iws(j) = intfields(j)
          enddo
          call ciowriterecord(ciofilesu(i), rectype, iws, realfields)
        enddo
c
      else
c
        do i = 1, nfiles
          do j = 1, nidat
            iws(j) = intfields(j)
          enddo
          call ciowriterecord(files(i), rectype, iws, realfields)
        enddo
c
      endif
c
      return
      end
c     --- End of routine allciosave
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine allciosavewaf(nfiles, files, rectype,
     +                         nidat, intfields, realfields,
     +                         ntoadd, ftoadd, iws)
c
c     Writing a record with additional fields. into all the defined
c     cio files.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     nfiles.......... (input, integer) Number of files to write. If
c                      nfiles is zero or negative, then the record is
c                      saved in all available files.
c     files........... (input, integer, array(nfiles)) List of files
c                      where to save the record. Used only if nfiles
c                      is positive.
c     rectype......... (input, integer) Record type. Must be a record
c                      type present in all the defined files.
c     nidat........... (input, integer) The number of integer data
c                      entered in array "intfields".
c     intfields....... (input, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). No control is done
c                      on the integers ranges.
c     realfields...... (input, double precision, array(*)) Real fields
c                      of the record. Ranges are not checked.
c     ntoadd.......... (input, integer) Number of additional fields to
c                      add. If ntoadd is zero or negative, then
c                      no fields are added.
c     ftoadd.......... (input, double precision, array(ntoadd)) Array
c                      containing the values of the fields to be added.
c     iws............. (scratch, integer, array(*)) Working integer
c                      array. Its length must not be less than the
c                      length of "intfields".
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
      integer           nfiles, rectype, nidat, ntoadd
      integer           files(nfiles)
      integer           intfields(1)
      double precision  realfields(0:1)
      double precision  ftoadd(ntoadd)
      integer           iws(1)
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
      if (nfiles .le. 0) then
c
        do i = 1, nciofilesu
          do j = 1, nidat
            iws(j) = intfields(j)
          enddo
          call ciowriterecordwaf(ciofilesu(i), rectype, iws,
     +                           realfields, ntoadd, ftoadd)
        enddo
c
      else
c
        do i = 1, nfiles
          do j = 1, nidat
            iws(j) = intfields(j)
          enddo
          call ciowriterecordwaf(files(i), rectype, iws,
     +                           realfields, ntoadd, ftoadd)
        enddo
c
      endif
c
      return
      end
c     --- End of routine allciosavewaf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine endciosave(nidat, intfields, realfields,
     +                      inl, inh, iel, ieh, iws)
c
c     Writing a "end" of shower record into all the defined cio files.
c     A (type = 2) record is written into all the defined cio files.
c     It is assumed that the arrays "intfields" and "realfields" were
c     already set with the corresponding quantities, as defined in the
c     "end of shower" record definitions, except for the number of
c     excluded particles records which are set here. The indices for
c     such records are passed as parameters (inl, inh, iel, ieh).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997; Fermilab 1999.
c
c
c     Arguments:
c     =========
c
c     nidat........... (input, integer) The number of integer data
c                      entered in array "intfields".
c     intfields....... (input, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). No control is done
c                      on the integers ranges.
c     realfields...... (input-scratch, double precision, array(*)) Real
c                      fields of the record. Ranges are not checked.
c                      Elements inl, inh, iel, ieh of this array are
c                      set internally.
c     inl............. (input, integer) Index pointing to the "near
c                      excluded particle number" fields.
c     inh............. (input, integer) Index pointing to the "far
c                      excluded particle number" field.
c     iel............. (input, integer) Index pointing to the "energy
c                      of near excluded particles" fields.
c     ieh............. (input, integer) Index pointing to the "energy
c                      of far excluded particles" field.
c     iws............. (scratch, integer, array(*)) Working integer
c                      array. Its length must not be less than the
c                      length of "intfields".
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
c     include 'pstackpar.f'      (Included by 'pbuffpar.f')
c     include 'ciopar.f'         (Included by 'pbuffpar.f')
      include 'pbuffpar.f'
c
c     Declaration of arguments.
c
      integer           nidat, inl, inh, iel, ieh
      integer           intfields(1)
      double precision  realfields(1)
      integer           iws(1)
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, ifile
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing the end of shower record (rectype = 2) in every
c     compressed file in use.
c
      do i = 1, nciofilesu
        ifile = ciofilesu(i)
c
        if (ifile .gt. 1) then
          realfields(inl)     = noplowp(2, 1, ifile)
          realfields(inl + 1) = noplowp(2, 2, ifile)
          realfields(inh)     = nophighp(2, ifile)
          realfields(iel)     = eoplowp(2, 1, ifile)
          realfields(iel + 1) = eoplowp(2, 2, ifile)
          realfields(ieh)     = eophighp(2, ifile)
        else
          realfields(inl)     = ngndlowp(2, 1)
          realfields(inl + 1) = ngndlowp(2, 2)
          realfields(inh)     = ngndhighp(2)
          realfields(iel)     = egndlowp(2, 1)
          realfields(iel + 1) = egndlowp(2, 2)
          realfields(ieh)     = egndhighp(2)
        endif
c
        do j = 1, nidat
          iws(j) = intfields(j)
        enddo
c
        call ciowriterecord(ifile, 2, iws, realfields)
c
      enddo
c
      return
      end
c     --- End of routine endciosave
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine initciopbuf
c
c     Initializing the cio particle buffers and related variables.
c
c     Written by: S. J. Sciutto, La Plata 1997; Fermilab 1999.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
c     include 'pstackpar.f'      (Included by 'pbuffpar.f')
c     include 'ciopar.f'         (Included by 'pbuffpar.f')
      include 'pbuffpar.f'
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
c     Emptying the cio particle buffers.
c
      npgndbuff = 0
c
      do i = 2, nciofiles
        npopbuff(i) = 0
      enddo
c
c     Initializing the associated statistical data arrays.
c
      do j = 1, 5
        ngndlowp(j, 1)  = 0
        ngndlowp(j, 2)  = 0
        ngndhighp(j)    = 0
        egndlowp(j, 1)  = 0
        egndlowp(j, 2)  = 0
        egndhighp(j)    = 0
      enddo
      ngndlowp(4, 1)  = 1.0d35
      ngndlowp(4, 2)  = 1.0d35
      ngndhighp(4)    = 1.0d35
      egndlowp(4, 1)  = 1.0d35
      egndlowp(4, 2)  = 1.0d35
      egndhighp(4)    = 1.0d35
c
      do i = 2, nciofiles
        do j = 1, 5
          noplowp(j, 1, i)  = 0
          noplowp(j, 2, i)  = 0
          nophighp(j, i)    = 0
          eoplowp(j, 1, i)  = 0
          eoplowp(j, 2, i)  = 0
          eophighp(j, i)    = 0
        enddo
        noplowp(4, 1, i)  = 1.0d35
        noplowp(4, 2, i)  = 1.0d35
        nophighp(4, i)    = 1.0d35
        eoplowp(4, 1, i)  = 1.0d35
        eoplowp(4, 2, i)  = 1.0d35
        eophighp(4, i)    = 1.0d35
      enddo
c
      return
      end
c     --- End of routine initciopbuf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine updateciopbuf
c
c     Updating the cio particle buffers statistical variables.
c
c     Written by: S. J. Sciutto, La Plata 1997; Fermilab 1999.
c
c
      implicit none
c
c     Compilation parameters.
c
c     include 'kernelpar.f'      (Included by 'pstackpar.f')
c     include 'pstackpar.f'      (Included by 'pbuffpar.f')
c     include 'ciopar.f'         (Included by 'pbuffpar.f')
      include 'pbuffpar.f'
c
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
      include 'pgndbuffcomm.f'
      include 'opbuffcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
c     Numbers and energies of particles not included in the ground
c     particle file.
c
      call statupdate(2, 5, ngndlowp)
      call statupdate(1, 5, ngndhighp)
      call statupdate(2, 5, egndlowp)
      call statupdate(1, 5, egndhighp)
c
c     Number and energies of particles not included in other files.
c
      do i = 1, nciofilesu
        j = ciofilesu(i)
        if (j .gt. 1) then
          call statupdate(2, 5, noplowp(1, 1, j))
          call statupdate(1, 5, nophighp(1, j))
          call statupdate(2, 5, eoplowp(1, 1, j))
          call statupdate(1, 5, eophighp(1, j))
        endif
      enddo
c
      return
      end
c     --- End of routine updateciopbuf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cioaux.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
