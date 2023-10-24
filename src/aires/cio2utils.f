c
c     FILE: cio2utils.f                     Creation date: 16/JAN/1997.
c                                       LAST MODIFICATION: 19/JUL/2003.
c
c     Aires compressed i/o system (III): Some utilities to process
c     already created compressed files.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine croheaderinfo(ouflag, vrb, irc)
c
c     Printing a summary of the information contained in the header
c     of the last opened compressed file.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           ouflag, vrb, irc
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u, minu, maxu
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if there is an already opened file.
c
      if (lastopciofile2 .le. 0) then
        irc = 12
        if (vrb .gt. 0) then
          call errprint(0, '$CU1', max(3, vrb), 'croheaderinfo',
     +       ' ', 0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
c     Selecting output units.
c
      call minmaxou(ouflag, minu, maxu)
c
c     Printing the title information.
c
 2010 format(9a)
 2020 format(a, i2)
      do u = minu, maxu
        write(u, *)
        write(u, 2010) '>>>> FILE: ', pifiletitle(lastopciofile2)
        write(u, *)
      enddo
c
c     Printing a summary of the available info.
c
      call croheader1info(ouflag)
c
      irc = 0
      return
      end
c     --- End of routine croheaderinfo
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function crofileversion(fileid)
c
c     Returning the AIRES version used to write an already opened
c     compressed file.
c
c     Written by: S. J. Sciutto, La Plata 1998.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c
c     Return value: (integer) The corresponding version in integer
c     ============  format (for example 01040200 for version 1.4.2,
c                   01040201 for version 1.4.2a, etc.). If the file
c                   is not opened or if there is an error, then the
c                   return value is negative. 
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           crofileversion
      integer           fileid
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (cio2pen(fileid)) then
c
        crofileversion = rcversion(fileid)
c
      else
c
        crofileversion = - 1
c
      endif
c
      return
      end
c     --- End of routine crofileversion
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function thisairesversion()
c
c     Returning the current AIRES version.
c
c     Written by: S. J. Sciutto, Fermilab 1999.
c
c
c     Return value: (integer) The corresponding version in integer
c     ============  format (for example 01040200 for version 1.4.2,
c                   01040201 for version 1.4.2a, etc.).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
c
c     Declaration of arguments.
c
      integer           thisairesversion
c
c     FIRST EXECUTABLE STATEMENT
c
      thisairesversion = aires_version_no
c
      return
      end
c     --- End of routine thisairesversion
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crorecstruct(fileid, nrtype, nintf, nrealf, irc)
c
c     Getting information about the records of an already opened
c     compressed file.
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     nrtype.......... (output, integer) The highest record type
c                      defined for the file (record types range from
c                      zero to nrtype).
c     nintf........... (output, integer, array(0:nrtype)) Number
c                      of integer fields contained at each record type,
c                      for record types from zero to nrtype.
c     nrealf.......... (output, integer, array(0:nrtype)) Number
c                      of real fields contained at each record type,
c                      for record types from zero to nrtype.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           fileid, nrtype, irc
      integer           nintf(0:1)
      integer           nrealf(0:1)
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           irec, kfield
c
c     FIRST EXECUTABLE STATEMENT
c
      nrtype = 0
c
c     Checking if the file is already open.
c
      if (.not. cio2pen(fileid)) then
        irc = 12
        return
      endif
c
c     Total number of records.
c
      nrtype = nrectypes2(fileid)
c
c     Getting field info for each record.
c
      do irec = 0, nrectypes2(fileid)
c
c       Integer fields.
c
        nintf(irec) = cio2lfnoscal(irec, fileid)
c
c       Date-time fields (added to integer).
c
        kfield = nrecfield2(datiftype, irec, fileid)
        if (kfield .gt. 0) nintf(irec) = nintf(irec) + 6 * kfield
c
c       Real fields
c
        nrealf(irec) = totrecfields2(irec, fileid) - kfield -
     +                 (cio2lfnoscal(irec, fileid) + min(irec, 1))
c
      enddo
c
      irc = 0
      return
      end
c     --- End of routine crorecstruct
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crofileinfo(fileid, ouflag, vrb, irc)
c
c     Printing information about the records of an already opened
c     compressed file.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
      include 'versionpar.f'
c
c     Declaration of arguments.
c
      integer           fileid, ouflag, vrb, irc
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u, minu, maxu
      integer           i, j, ver1, ver2, ver3, verl
      character*10      version
      integer           irec, indx, ishift
      integer           ifield, ifield0, ifield1
      integer           jfield, kfield, lfield
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the file is already open.
c
      if (.not. cio2pen(fileid)) then
        irc = 12
        if (vrb .gt. 0) then
          call errprint(0, '$CI3', max(3, vrb), 'crofileinfo',
     +       'File is not open.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
c     Selecting output units.
c
      call minmaxou(ouflag, minu, maxu)
c
c     Printing the title information.
c
 2010 format(9a)
 2020 format(a, i2)
      do u = minu, maxu
        write(u, *)
        write(u, 2010) '>>>> FILE: ', pifiletitle(fileid)
      enddo
c
      if (rcversion(fileid) .ne. aires_version_no) then
c
c       The file was written using a different AIRES version.
c
        ver3 = rcversion(fileid) / 100
        verl = rcversion(fileid) - 100 * ver3
        ver2 = ver3 / 100
        ver3 = ver3 - 100 * ver2
        ver1 = ver2 / 100
        ver2 = ver2 - 100 * ver1
c
        write(version, 2025) ver1, ver2, ver3
 2025   format(2(i2, '.'), i2)
        j = 0
        do i = 1, 8
          if (version(i:i) .ne. ' ') then
            j = j + 1
            version(j:j) = version(i:i)
          endif
        enddo
c
        if (verl .gt. 0) then
          j = j + 1
          version(j:j) = char(ichar('a') + verl - 1)
        endif
c
        do u = minu, maxu
          write(u, *)
          write(u, 2010)
     +             '     ***** File written using AIRES version ',
     +             version(1:j)
        enddo
c
      endif
c
      do u = minu, maxu
        write(u, *)
        write(u, 2010) '>>>> RECORD STRUCTURE:'
        write(u, *)
        write(u, 2020) '     Number of defined record types:',
     +                 nrectypes2(fileid) + 1
      enddo
c
c     Printing record and field info.
c
      do irec = 0, nrectypes2(fileid)
c
        do u = minu, maxu
          write(u, *)
          write(u, 2030) '     RECORD TYPE', irec, ': ',
     +                   cio2recname(irec, fileid)
        enddo
 2030   format(a, i2, 2a)
c
        ishift  = min(irec, 1)
        ifield0 = ishift + 1
c
        do u = minu, maxu
          write(u, *)
          write(u, 2010) '     Integer fields'
        enddo
c
        ifield1 = cio2lfnoscal(irec, fileid) + ishift
        indx    = 0
c
        do ifield = ifield0, ifield1
          indx = indx + 1
          do u = minu, maxu
            write(u, 2040) '               (', indx, ')   ',
     +                     cio2fname(ifield, irec, fileid)
          enddo
        enddo
 2040   format(a, i2, 2a)
c
c       Analysing date-time fields.
c
        lfield = totrecfields2(irec, fileid)
        kfield = lfield - nrecfield2(datiftype, irec, fileid)
c
        if (lfield .gt. kfield) then
          indx = indx + 1
          do jfield = kfield + 1, lfield
            do u = minu, maxu
              write(u, 2050) '            (',
     +                       indx, '-', indx + 5, ')   ',
     +                       cio2fname(jfield, irec, fileid)
            enddo
            indx = indx + 6
          enddo
 2050     format(a, 2(i2, a), a)
        endif
c
        if (indx .le. 0) then
          do u = minu, maxu
            write(u, 2010) '               NONE'
          enddo
        endif
c
        do u = minu, maxu
          write(u, *)
          write(u, 2010) '     Real fields'
        enddo
c
        do ifield = ifield1 + 1, kfield
          indx = ifield - ifield1
          do u = minu, maxu
            write(u, 2040) '               (', indx, ')   ',
     +                     cio2fname(ifield, irec, fileid)
          enddo
        enddo
c
        if (kfield .le. ifield1) then
          do u = minu, maxu
            write(u, 2010) '               NONE'
          enddo
        endif
c
      enddo
c
      do u = minu, maxu
        write(u, *)
      enddo
c
      irc = 0
      return
      end
c     --- End of routine crofileinfo
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crorecninfo(fileid, poskey, ouflag, vrb, irc)
c
c     Printing information about the total number of records in a file.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     poskey.......... (input, integer) Positioning key. This parameter
c                      allows to control the file positioning after
c                      returning from this routine: If zero or negative
c                      the file remains positioned at the end of file
c                      point, if 1 at the beginning of data, and if
c                      greater than 1, at the position found before the
c                      call (This option may enlarge processing time
c                      for very long files).
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           fileid, poskey, ouflag, vrb, irc
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, nrtype, oldrecnumber, totrec, vrb0
      integer           u, minu, maxu
      integer           nrec(0:mxciortype)
      logical           ldy
      logical           crogotorec
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the file is already open.
c
      if (.not. cio2pen(fileid)) then
        irc = 12
        if (vrb .gt. 0) then
          call errprint(0, '$CI3', max(3, vrb), 'crorecninfo',
     +       'File is not open.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
      if (vrb .eq. 1) then
        vrb0 = 0
      else
        vrb0 = vrb
      endif
c
      oldrecnumber = cio2recread(fileid)
c
c     Rewinding the file to start from the first record.
c
      call crorewind(fileid, vrb0, irc)
      if (irc .ne. 0) return
c
c     Counting the records.
c
      call croreccount(fileid, vrb, nrtype, nrec, irc)
      if (irc .ne. 0) return
c
      totrec = 0
      do i = 0, nrtype
        totrec = totrec + nrec(i)
      enddo
c
c     Selecting output units.
c
      call minmaxou(ouflag, minu, maxu)
c
c     Printing the record information.
c
      do u = minu, maxu
        write(u, *)
        write(u, 2010) '>>>> FILE: ', pifiletitle(lastopciofile2)
        write(u, 2010)
        write(u, 2020) 'Record type and name', 'Number of records'
        write(u, *)
c
        do i = 0, nrtype
          write(u, 2030) i, cio2recname(i, fileid), nrec(i)
        enddo
        write(u, *)
        write(u, 2040) 'TOTAL', totrec
        write(u, *)
      enddo
 2010 format(9a)
 2020 format(5x, a, t53, a)
 2030 format(i6, 2x, a42, i15)
 2040 format(5x, a, t51, i15)
c
c     Repositioning the file.
c
      if (poskey .le. 0) then
        irc = 0
      else if (poskey .eq. 1) then
        call crorewind(fileid, vrb0, irc)
      else
        ldy = crogotorec(fileid, oldrecnumber, vrb0, irc)
      endif
c
      return
      end
c     --- End of routine croreccount
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function crofieldindex(fileid, rectype, fieldname, vrb, datype,
     +                       irc)
c
c     Returning the index corresponding to a given field within a
c     compressed file record.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     rectype......... (input, integer) Record type (0 for default
c                      record type).
c     fieldname....... (input, character*(*)) First characters of
c                      field name (enough characters must be provided
c                      to make an unambiguous specification).
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     datype.......... (output, integer) The data type that corresponds
c                      to the specified field: 1 for integer data,
c                      2 for date-time data, 3 for real data, and
c                      4 for dynamically added real data fields.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c     Return value: (integer) The field index. Zero if there was an
c     ============  error.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           crofieldindex
      integer           fileid, rectype, vrb, datype, irc
      character*(*)     fieldname
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           lfn, nfind
      integer           indx, jndx, indx0, ishift
      integer           ifield, ifield0, ifield1
      integer           jfield, kfield, lfield
c
      integer           nreserved
      parameter         (nreserved = 2)
c
      character*42      reserved(nreserved)
      data              reserved / 'Number of added fields',
     +                             'First added field'       /
c
c     FIRST EXECUTABLE STATEMENT
c
      crofieldindex = 0
c
c     Checking if the file is already open.
c
      if (.not. cio2pen(fileid)) then
        irc = 12
        if (vrb .gt. 0) then
          call errprint(0, '$CI3', max(3, vrb), 'crofieldindex',
     +       'File is not open.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
c     Checking record type.
c
      if ((rectype .lt. 0) .or. (rectype .gt. nrectypes2(fileid))) then
        irc = 10
        if (vrb .gt. 0) then
          call errprint(0, '*', max(3, vrb), 'crofieldindex',
     +       'The listed record type is not defined.',
     +       1, rectype, 0, 0.d0, pifiletitle(fileid))
        endif
        return
      endif
c
c     Searching for the given field name.
c
      lfn   = min(42, len(fieldname))
      nfind = 0
c
c     Analysing integer fields.
c
      ishift  = min(rectype, 1)
      ifield0 = ishift + 1
c
      ifield1 = cio2lfnoscal(rectype, fileid) + ishift
      indx    = 0
c
      do ifield = ifield0, ifield1
        indx = indx + 1
        if (fieldname(1:lfn) .eq.
     +      cio2fname(ifield, rectype, fileid)(1:lfn)) then
          nfind  = nfind + 1
          jndx   = indx
          datype = 1
        endif
      enddo
c
c     Analysing date-time fields.
c
      lfield = totrecfields2(rectype, fileid)
      kfield = lfield - nrecfield2(datiftype, rectype, fileid)
c
      if (lfield .gt. kfield) then
        indx = indx + 1
        do jfield = kfield + 1, lfield
          if (fieldname(1:lfn) .eq.
     +        cio2fname(jfield, rectype, fileid)(1:lfn)) then
            nfind  = nfind + 1
            jndx   = indx
            datype = 2
          endif
          indx  = indx + 6
        enddo
        indx = indx - 1
      endif
c
      indx0 = indx
      indx  = 0
c
c     Analysing real fields.
c
      do ifield = ifield1 + 1, kfield
        indx = ifield - ifield1
        if (fieldname(1:lfn) .eq.
     +      cio2fname(ifield, rectype, fileid)(1:lfn)) then
          nfind  = nfind + 1
          jndx   = indx
          datype = 3
        endif
      enddo
c
c     Analysing dynamically added fields.
c
      if (cio2dynrecty(rectype, fileid)) then
        if (fieldname(1:lfn) .eq. reserved(1)(1:lfn)) then
          nfind  = nfind + 1
          jndx   = indx0 + 1
          datype = 1
        else if (fieldname(1:lfn) .eq. reserved(2)(1:lfn)) then
          nfind  = nfind + 1
          jndx   = indx + 1
          datype = 4
        endif
      endif
c
c     Checking if the field was found.
c
      if (nfind .le. 0) then
        irc    = 8
        datype = 0
        if (vrb .gt. 0) then
          call errprint(0, '*', max(3, vrb), 'crofieldindex',
     +       'Field not found',
     +       0, 0, 0, 0.d0, fieldname(1:lfn))
        endif
        return
      else if (nfind .gt. 1) then
        irc    = 7
        datype = 0
        if (vrb .gt. 0) then
          call errprint(0, '*', max(3, vrb), 'crofieldindex',
     +       'Ambiguous field specification',
     +       0, 0, 0, 0.d0, fieldname(1:lfn))
        endif
        return
      else
c
c       The field was correctly specified. Setting the index.
c
        crofieldindex = jndx
c
        if (vrb .eq. 1) then
          call errprint(0, '*', 1, 'crofieldindex',
     +       'Field index successfully assigned.',
     +       1, jndx, 0, 0.d0, fieldname)
        endif
c
      endif
c
      irc = 0
      return
      end
c     --- End of routine crofieldindex
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine croreccount(fileid, vrb, nrtype, nrec, irc)
c
c     Counting the records of a compressed file starting from the
c     first non-read record.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     nrtype.......... (output, integer) The highest record type
c                      defined for the file (record types range from
c                      zero to nrtype).
c     nrec............ (output, integer, array(0:nrtype)) For each
c                      record type, the number of records found.
c                      Enough space for this array must be ensured by
c                      the calling program.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           fileid, vrb, nrtype, irc
      integer           nrec(0:1)
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, rectype
      logical           getcrorectype
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the file is already open.
c
      if (.not. cio2pen(fileid)) then
        irc = 12
        if (vrb .gt. 0) then
          call errprint(0, '$CI3', max(3, vrb), 'croreccount',
     +       'File is not open.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
      nrtype = nrectypes2(fileid)
c
      do i = 0, nrtype
        nrec(i) = 0
      enddo
c
c     Counting the records.
c
 1010 continue
      if (.not. getcrorectype(fileid, 0, i, rectype)) goto 1020
      nrec(rectype) = nrec(rectype) + 1
      goto 1010
c
 1020 continue
c
c     Error or EOF reached.
c
      if (rectype .lt. 0) then
        irc = 0
        if (vrb .eq. 1) then
          call errprint(0, '*', 1, 'croreccount',
     +       'AIRES compressed data file successfully scanned.',
     +       0, 0, 0, 0.d0, ' ')
        endif
      else
        irc = rectype
        if (vrb .gt. 0) then
          call errprint(0, '$CI3', max(3, vrb), 'croreccount',
     +       '(Error getting record type).',
     +       0, 0, 0, 0.d0, ' ')
        endif
      endif
c
      return
      end
c     --- End of routine croreccount
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function crorecnumber(fileid, vrb, irc)
c
c     Returning the current record number corresponding to a currently
c     opened compressed file.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c     Return value: (Integer) The record number. If the file is not
c     ============  ready (closed or end of file), then -1 is retuned.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           crorecnumber
      integer           fileid, vrb, irc
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      crorecnumber = -1
c
c     Checking if the file is already open.
c
      if (.not. cio2pen(fileid)) then
        irc = 3
        if (vrb .gt. 0) then
          call errprint(0, '$CI3', 2, 'crorecnumber',
     +       'File is not open.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
c     Returning the record number.
c
      crorecnumber = cio2recread(fileid)
      irc          = 0
c
      return
      end
c     --- End of routine crorecnumber.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getcrorectype(fileid, vrb, infield1, rectype)
c
c     Getting the record type of the next record in the cio file.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     infield1........ (output, integer) If rectype is zero, this
c                      variable contains the current value of the first
c                      (nonscaled) integer field of the record.
c                      Otherwise it is set to zero.
c     rectype......... (output, integer) Record type and return code.
c                      0 means that a default record was successfully
c                      read. i (i > 0) means that an alternative
c                      record of type i was successfully read. -1
c                      means that an end-of-file condition was get
c                      from the corresponding file.  Any other value
c                      indicates a reading error (rectype equals the
c                      system return code plus 10000).
c
c     Return value: (logical) True if a record was successfully read.
c     ============  False othwerise (EOF or I/O error).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      logical           getcrorectype
      integer           fileid, vrb, infield1, rectype
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           intfields(4)
      integer           lastchar, lastcha0, nadded
      logical           ciorebuffer
c
c     FIRST EXECUTABLE STATEMENT
c
      currciofile2 = fileid
c
c     Refreshing the buffer (if necessary).
c
      if (cio2reclast(fileid) .ge. cio2fillpoint(0, fileid)) then
c
c       The pointer arrived to the end of the buffer. Refreshing.
c
        getcrorectype = ciorebuffer(fileid, vrb, rectype)
        if (.not. getcrorectype) return
c
      endif
c
 1010 continue
      lastchar = cio2reclast(fileid)
      lastcha0 = lastchar
c
c     Getting the first type 1 field to determine the record type.
c
      call intintdecode4(1, cio2record1(fileid),
     +                   lastchar, intfields(1), intfields(3))
c
c     Analysing the record type.
c
      if (intfields(1) .lt. cio2firstalt(fileid)) then
c
c       It is a default record (this will happen most frequently).
c
        rectype = 0
c
c       In this case argument "infield1" is also set, and it
c       corresponds to a particle code.
c
        infield1 = cio2pcledecode(intfields(1))
c
        cio2reclast(fileid) = lastcha0 + cio2reclen(0, fileid)
        cio2recread(fileid) = cio2recread(fileid) + 1
        getcrorectype       = .true.
c
      else if (intfields(1) .lt. cio2nullcode(fileid)) then
c
c       It is an alternative record.
c
        rectype  = cio2nullcode(fileid) - intfields(1)
        infield1 = 0
c
c       Skipping dynamic part if necessary.
c
        if (cio2dynrecty(rectype, fileid)) then
          lastchar = lastcha0 + cio2reclen0(rectype, fileid)
          call intdecode2(1, cio2record1(fileid),
     +                    lastchar, nadded)
          lastchar = lastchar + dynadflen * nadded
          lastchar = 1 + (lastchar - lastcha0 - 1) /
     +                   cio2reclen(0, fileid)
          cio2reclast(fileid) = lastcha0 + lastchar *
     +                                     cio2reclen(0, fileid)
        else
          cio2reclast(fileid) = lastcha0 + cio2reclen(rectype, fileid)
        endif
c
        cio2recread(fileid) = cio2recread(fileid) + 1
        getcrorectype       = .true.
c
      else
c
c       It is a null record, which marks the end of an incompletely
c       filled block. It is necessary to refresh the buffer and retry.
c
        getcrorectype = ciorebuffer(fileid, vrb, rectype)
        if (getcrorectype) goto 1010
c
      endif
c
      cio2recprev(fileid) = lastcha0
c
      return
      end
c     --- End of routine getcrorectype.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function crorecfind(fileid, intype, vrb, infield1, rectype)
c
c     Reading records until getting a specified record type.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998, 2003.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     intype.......... (input, integer) Record type to find.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     infield1........ (output, integer) If intype is zero, this
c                      variable contains the current value of the first
c                      (nonscaled) integer field of the last scanned
c                      record. It is set to zero otherwise.
c     rectype......... (output, integer) Last scanned record type and
c                      return code. 0 means that a default record was
c                      successfully read. i (i > 0) means that an
c                      alternative record of type i was successfully
c                      read. -1 means that an end-of-file condition was
c                      get from the corresponding file.  Any other
c                      value indicates a reading error (rectype equals
c                      the system return code plus 10000).
c
c     Return value: (logical) True if the last record was successfully
c     ============  read. False othwerise (EOF or I/O error).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      logical           crorecfind
      integer           fileid, intype, vrb, infield1, rectype
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           intfields(4)
      integer           lastchar, lastcha0, nadded
      logical           ciorebuffer
c
c     FIRST EXECUTABLE STATEMENT
c
      currciofile2 = fileid
c
 1005 continue
c
c     Refreshing the buffer (if necessary).
c
      if (cio2reclast(fileid) .ge. cio2fillpoint(0, fileid)) then
c
c       The pointer arrived to the end of the buffer. Refreshing.
c
        crorecfind = ciorebuffer(fileid, vrb, rectype)
        if (.not. crorecfind) return
c
      endif
c
 1010 continue
      lastchar = cio2reclast(fileid)
      lastcha0 = lastchar
c
c     Getting the first set of type 1 fields to determine the record
c     type.
c
      call intintdecode4(1, cio2record1(fileid),
     +                   lastchar, intfields(1), intfields(3))
c
c     Analysing the record type.
c
      if (intfields(1) .lt. cio2firstalt(fileid)) then
c
c       It is a default record (this will happen most frequently).
c
        rectype = 0
c
        cio2reclast(fileid) = lastcha0 + cio2reclen(0, fileid)
        cio2recread(fileid) = cio2recread(fileid) + 1
c
        if (intype .ne. 0) goto 1005
c
c       In this case argument "infield1" is also set, and it
c       corresponds to a particle code.
c
        infield1 = cio2pcledecode(intfields(1))
c
      else if (intfields(1) .lt. cio2nullcode(fileid)) then
c
c       It is an alternative record.
c
        rectype = cio2nullcode(fileid) - intfields(1)
c
c       Skipping dynamic part if necessary.
c
        if (cio2dynrecty(rectype, fileid)) then
          lastchar = lastcha0 + cio2reclen0(rectype, fileid)
          call intdecode2(1, cio2record1(fileid),
     +                    lastchar, nadded)
          lastchar = lastchar + dynadflen * nadded
          lastchar = 1 + (lastchar - lastcha0 - 1) /
     +                   cio2reclen(0, fileid)
          cio2reclast(fileid) = lastcha0 + lastchar *
     +                                     cio2reclen(0, fileid)
        else
          cio2reclast(fileid) = lastcha0 + cio2reclen(rectype, fileid)
        endif
c
        cio2recread(fileid) = cio2recread(fileid) + 1
c
        if (intype .ne. rectype) goto 1005
c
        infield1 = 0
c
      else
c
c       It is a null record, which marks the end of an incompletely
c       filled block. It is necessary to refresh the buffer and retry.
c
        crorecfind = ciorebuffer(fileid, vrb, rectype)
        if (crorecfind) goto 1010
        return
c
      endif
c
      cio2recprev(fileid) = lastcha0
      crorecfind          = .true.
c
      return
      end
c     --- End of routine crorecfind.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function crogotorec(fileid, recnumber, vrb, irc)
c
c     Positioning the file after a given record.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     recnumber....... (input, integer) The record number. A negative
c                      value is taken as zero. If recnumber LE 0, the
c                      return code is always set to zero for
c                      successful operations.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means that the
c                      last scanned record record was of default type. i
c                      (i > 0) means that the last scanned record was an
c                      alternative record of type i. -1 means that an
c                      end-of-file condition was got from the
c                      corresponding file before reaching the indicated
c                      record.  Any other value indicates a reading
c                      error (irc equals the system return code plus
c                      10000).
c
c     Return value: (logical) True if the positioning was successfully
c     ============  done. False otherwise.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      logical           crogotorec
      integer           fileid, recnumber, vrb, irc
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, lastchar, vrb0
      logical           get1
      logical           getcrorectype
c
c     FIRST EXECUTABLE STATEMENT
c
      if (vrb .eq. 1) then
        vrb0 = 0
      else
        vrb0 = vrb
      endif
c
      irc = 0
c
c     Comparing the given record number with the current one.
c
      if (recnumber .eq. cio2recread(fileid)) then
c
c       The positioning is already OK. Re-reading the record to get
c       its record type.
c
        currciofile2 = fileid
        lastchar     = cio2recprev(fileid)
c
c       Getting the first type 1 field to determine the record type.
c
        call intintdecode4(1, cio2record1(fileid),
     +                     lastchar, i, j)
c
c       Analysing the record type.
c
        if (i .ge. cio2firstalt(fileid)) then
c
c         It is an alternative record.
c
          irc = cio2nullcode(fileid) - i
c
        endif
c
        crogotorec = .true.
c
        if (vrb .eq. 1) then
          call errprint(0, '*', 1, 'crogotorec',
     +       'File successfully positioned after given record number.',
     +       1, recnumber, 0, 0.d0, pifilename(fileid))
        endif
c
        return
c
      else if (recnumber .lt. cio2recread(fileid)) then
c
c       The given record number is smaller than the current one.
c       It is necessary to rewind the file before continuing.
c
        crogotorec = .false.
        call crorewind(fileid, vrb0, irc)
        if (irc .ne. 0) return
c
      endif
c
c     Skipping records until reaching the desired position.
c
      get1 = .true.
      do i = cio2recread(fileid) + 1, recnumber
        get1 = getcrorectype(fileid, vrb0, j, irc)
        if (.not. get1) goto 1010
      enddo
 1010 continue
c
      crogotorec = get1
c
      if (get1) then
        if (vrb .eq. 1) then
          call errprint(0, '*', 1, 'crogotorec',
     +       'File successfully positioned after given record number.',
     +       1, recnumber, 0, 0.d0, pifilename(fileid))
        endif
      endif
c
      return
      end
c     --- End of routine crogotorec.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function regetcrorecord(fileid, intfields, realfields, altrec,
     +                        vrb, irc)
c
c     Re-reading the current record. To be used jointly with
c     "getcrorectype" and "crorecfind"
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     intfields....... (output, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). The calling
c                      program must provide enough space for this
c                      array (The minimum dimension is the maximum
c                      number of fields that can appear in a record
c                      plus 1). Positions beyond the last integer
c                      fields are used as scratch working space.
c     realfields...... (output, double precision, array(*)) Real fields
c                      of the record. The calling program must provide
c                      enough space for this array.
c     altrec.......... (output, logical) True if the record is an
c                      alternative record. False if a default record
c                      was obtained.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means that a
c                      default record was successfully read.
c                      i (i > 0) means that an alternative record of
c                      type i was successfully read. -1 means that an
c                      end-of-file condition was get from the
c                      corresponding file.  Any other value indicates
c                      a reading error (irc equals the system return
c                      code plus 10000).
c
c     Return value: (logical) True if a record was successfully
c     ============  re-read. False othwerise (EOF or I/O error).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      logical           regetcrorecord
      integer           fileid, vrb, irc
      integer           intfields(1)
      double precision  realfields(0:1)
      logical           altrec
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      logical           getcrorecord
c
c     FIRST EXECUTABLE STATEMENT
c
c     Turning the pointer back one record position.
c
      cio2reclast(fileid) = cio2recprev(fileid)
      cio2recread(fileid) = cio2recread(fileid) - 1
c
c     Reading the record again.
c
      regetcrorecord = getcrorecord(fileid, intfields, realfields,
     +                              altrec, vrb, irc)
c
      return
      end
c     --- End of routine regetcrorecord.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine crorewind(fileid, vrb, irc)
c
c     "Rewinding" an already opened compressed file. The file is
c     positioned just before the first data record. It is assumed
c     that the file header is OK.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           fileid, vrb, irc
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ut
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the file is already open.
c
      if (.not. cio2pen(fileid)) then
        irc = 3
        if (vrb .gt. 0) then
          call errprint(0, '$CI3', 2, 'crorewind',
     +       'File is not open.',
     +       0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
      ut = minciout + fileid
c
c     System rewinding the file.
c
      rewind(ut)
c
c     Skipping all headers to position the file just before the
c     first data record. To this end all records are read in until
c     the final marker ('*+*+*') is found.
c
 1010 continue
      read(ut, 2010, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:5)
      call restring(ciorecord0(1:5))
      if (ciorecord0(1:5) .ne. '*+*+*') goto 1010
 2010 format(a)
c
c     Resetting the associated pointers and counters.
c
      cio2reclast(fileid) = cio2fillpoint(0, fileid)
      cio2recprev(fileid) = cio2reclast(fileid)
      cio2recread(fileid) = 0
c
      return
c
c     Error message.
c
 3010 continue
      if (vrb .gt. 0) then
        call errprint(0, '$CI3', max(3, vrb), 'opencrofile',
     +     'System return code is listed:',
     +     1, irc, 0, 0.d0, pifilename(fileid))
      endif
      irc = irc + 10000
      call cioclose1(fileid)
      return
c
      end
c     --- End of routine crorewind.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cio2utils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
