c
c     FILE: ciomgr1.f                       Creation date: 28/NOV/1996.
c                                       LAST MODIFICATION: 06/AUG/2003.
c
c     Aires compressed i/o system (I): Routines to create and write
c     the compressed files.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioinit0w
c
c     Initializing the predefined file intrinsic arrays of the i/o
c     system. This is necessary for output operations. Notice, however,
c     That the scaling variables are not set here, but in routine
c     cioinit1w. This is to allow dynamical setting (to be done just
c     before opening the file).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      integer           ifile, jfile, irec, iftype
      integer           ifield, jfield, nfield
      integer           totf, rlen, trlen, ncontrecd, recblocks
      integer           nblocks, lscal, lnscal
      integer           chks, chkf
c
c     FIRST EXECUTABLE STATEMENT
c
c     General initialization.
c
      call cioinit0
c
c     Setting default values for internal arrays.
c
      do ifile = 1, mxciofiles
c
        pofiletitle(ifile) = 'Aires compressed data file.'
        write(pofilext(ifile), 2010) '.cf', ifile
 2010   format(a, i2.2)
        nrectypes(ifile) = 0
c
        do irec = 0, mxciortype
c
          write(ciorecname(irec, ifile), 2020) 'RECORD ', ifield
 2020     format(a, i4.4)
c
          cioffscaled(irec, ifile) = 2
          ciodynfield(irec, ifile) = 0
          ciodynrecty(irec, ifile) = .false.
c
          do iftype = 1, mxcioftypes
            nrecfield(iftype, irec, ifile) = 0
          enddo
c
          do ifield = 1, mxciofields
c
            write(ciofname(ifield, irec, ifile), 2020) 'Field ', ifield
c
            ciofminv(ifield, irec, ifile) = 0
            ciofmaxv(ifield, irec, ifile) = 0
            ciofwsc0(ifield, irec, ifile) = 0
            ciofwsc1(ifield, irec, ifile) = 1
            ciofrsc0(ifield, irec, ifile) = 0
            ciofrsc1(ifield, irec, ifile) = 1
            cioflogs(ifield, irec, ifile) = .false.
          enddo
c
        enddo
      enddo
c
c     Defining the files to use and their formats.
c
      call ciodefinef(mxciofiles, mxciortype, mxcioftypes, mxciofields,
     +                npofiles, pofilext, pofiletitle, nrectypes,
     +                ciorecname, nrecfield, cioffscaled, cioflogs,
     +                ciofname, ciodynrecty)
c
c     Checking the initialized arrays for consistency, and completing
c     the evaluation of file definition parameters.
c
      if (npofiles .gt. mxciofiles) then
        call errprint(0, '$CI7', 4, 'cioinit0',
     +                'Too many compressed output files defined.',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
      do ifile = 1, npofiles
c
c       Checking the file extensions.
c
        if ((pofilext(ifile) .eq. lgfext) .or.
     +      (pofilext(ifile) .eq. idfext) .or.
     +      (pofilext(ifile) .eq. sryext) .or.
     +      (pofilext(ifile) .eq. texext)) then
          call errprint(0, '$CI7', 4, 'cioinit0',
     +    'Reserved file extension used for compressed file number:',
     +    1, ifile, 0, 0.d0, pofilext(ifile))
        endif
c
        do jfile = 1, ifile - 1
          if (pofilext(ifile) .eq. pofilext(jfile)) then
            call errprint(0, '$CI7', 4, 'cioinit0',
     +      'Compressed file name extension already in use.',
     +      0, 0, 0, 0.d0, pofilext(ifile))
          endif
        enddo
c
c       Base-name numbers are initially set to zero.
c
        pofileno(ifile) = 0
c
c       Checking the number of different records.
c
        if (nrectypes(ifile) .gt. mxciortype) then
          call errprint(0, '$CI7', 4, 'cioinit0',
     +    'Too many different records defined for compressed file.',
     +    1, nrectypes(ifile), 0, 0.d0, pofiletitle(ifile))
        endif
c
c       Checking the default record.
c
c       There must be unless one set of fields of type 1;
c       and no dynamically added fields.
c
        if (nrecfield(1, 0, ifile) .lt. 1) then
          call errprint(0, '$CI7', 4, 'cioinit0',
     +    'No type 1 fields defined for comp. file default record.',
     +    0, 0, 0, 0.d0, pofiletitle(ifile))
        endif
        if (ciodynrecty(0, ifile)) then
          call errprint(0, '$CI7', 4, 'cioinit0',
     +         'Attempt to define dynamic fields for default record.',
     +         0, 0, 0, 0.d0, ' ')
        endif
c
c
c       There must not be records of types beyond the maximum
c       record type allowed within the default record.
c
        do i = lastdefrecftype + 1, mxcioftypes
          if (nrecfield(i, 0, ifile) .gt. 0) then
            call errprint(0, '$CI7', 4, 'cioinit0',
     +      'Not permitted field type defined for the default record.',
     +      1, ciofieldid(i), 0, 0.d0, pofiletitle(ifile))
          endif
        enddo
c
        chks = 0
        chkf = 137
c
c       Evaluating the default record length.
c
        totf = 0
        rlen = 0
        do i = 1, lastdefrecftype
          chks = chks + chkf * nrecfield(i, 0, ifile)
          totf = totf + nrecfield(i, 0, ifile) * nsubfields(i)
          rlen = rlen + nrecfield(i, 0, ifile) * ciofieldlen(i)
        enddo
c
        if (rlen .gt. mxciorsize) then
          call errprint(0, '$CI7', 4, 'cioinit0',
     +    'Defined default record is too long.',
     +    1, rlen, 0, 0.d0, pofiletitle(ifile))
        endif
c
c       Defining the number of records that enter in a block and the
c       blocksize.
c
        recblocks              = mxciorsize / rlen
        ciobuflen(ifile)       = rlen * recblocks
        cioreclen(0, ifile)    = rlen
        ciofillpoint(0, ifile) = ciobuflen(ifile)
        totrecfields(0, ifile) = totf
c
c       End of default record section.
c
c       Checking the alternative records.
c
        do irec = 1, nrectypes(ifile)
c
c         The number of fields of type must be equal to or greater than
c         the corresponding number for the default record.
c
          if (nrecfield(1, irec, ifile) .lt. nrecfield(1, 0, ifile))
     +    then
            call errprint(0, '$CI7', 4, 'cioinit0',
     +      'Too few records of type 1 defined for alternative record.'
     +      , 1, nrecfield(1, irec, ifile), 0, 0.d0,
     +      pofiletitle(ifile))
          endif
c
          chkf = chkf * 11
c
c         Evaluating the record total length.
c
          totf  = 0
          trlen = 0
          do i = 1, mxcioftypes
            chks  = chks + chkf * nrecfield(i, irec, ifile)
            totf  = totf + nrecfield(i, irec, ifile) * nsubfields(i)
            trlen = trlen + nrecfield(i, irec, ifile)
     +                        * ciofieldlen(i)
          enddo
c
          cioreclen00(irec, ifile) = trlen
c
c         Fields than can be added dynamically.
c
          if (ciodynrecty(irec, ifile)) then
            if (totf .ge. mxciofields) then
              call errprint(0, '$CI7', 4, 'cioinit0',
     +        'Not enough space available to define dynamic fields.',
     +        0, 0, 0, 0.d0, ' ')
            endif
          endif
c
c         Evaluating the number of continuation records needed to
c         save the alternative record.
c
          ncontrecd  = (trlen - 1) / rlen
          nblocks    = ncontrecd + 1
c
          if (nblocks .gt. recblocks) then
            call errprint(0, '$CI7', 4, 'cioinit0',
     +      'Defined alternative record is too long. Record number:',
     +      1, irec, 0, 0.d0, pofiletitle(ifile))
          endif
c
          cioreclen(irec, ifile)    = rlen * nblocks
          ciofillpoint(irec, ifile) = ciobuflen(ifile) -
     +                                               rlen * ncontrecd
          totrecfields(irec, ifile) = totf
c
        enddo
c
c       End of alternative records section.
c
        ciochecksum(ifile) = chks
c
c       Analysing the different fields within each record type.
c
c       The first field of the alternative records is reserved
c       for the escape code.
c
        do irec = 1, nrectypes(ifile)
          ciofname(1, irec, ifile) = 'RESERVED FOR CONTROL CODE'
        enddo
c
c       Setting internal arrays and scaling coefficients.
c
        do irec = 0, nrectypes(ifile)
c
c         Evaluating scaling coefficients.
c
c         For fields of type one, only the second sub-field of each
c         set is scaled. The first subfields go ahead in the list
c         of non-scaled fields.
c
          ifield = nrecfield(1, irec, ifile)
          lnscal = nrecfield(1, irec, ifile)
          lscal  = 0
c
c         For the alternative record types, the first field is
c         the escape control field and is not available. In this
c         case one position must be subtracted from the position
c         pointers.
c
          if (irec .gt. 0) lnscal = lnscal - 1
c
c         This loop is for the set of second subfields only.
c
          startfield(1, irec, ifile) = ifield + 1
          do i = 1, nrecfield(1, irec, ifile)
            ifield = ifield + 1
            if (ifield .ge. cioffscaled(irec, ifile)) then
              lscal = lscal + 1
            else
              lnscal = lnscal + 1
            endif
          enddo
c
c         The subfields in the other types are ordered consecutively.
c
c         Integer coded fields.
c
          do iftype = 2, lastscaledtype1
c
            startfield(iftype, irec, ifile) = ifield + 1
c
            do i = 1, nrecfield(iftype, irec, ifile)
              do j = 1, nsubfields(iftype)
                ifield = ifield + 1
                if (ifield .ge. cioffscaled(irec, ifile)) then
                  lscal = lscal + 1
                else
                  lnscal = lnscal + 1
                endif
              enddo
            enddo
          enddo
c
          ciolfnoscal(irec, ifile) = lnscal
          ciolfscale1(irec, ifile) = lscal - 1
c
c         Floating point fields.
c
          jfield = 0
c
          do iftype = lastscaledtype1 + 1, lastscaledtype2
c
            startfield(iftype, irec, ifile) = jfield + 1
c
            nfield = nrecfield(iftype, irec, ifile) *
     +               nsubfields(iftype)
            ifield = ifield + nfield
            jfield = jfield + nfield
          enddo
c
          cionfscale2(irec, ifile) = jfield
c
c         Date and time fields starting position.
c         (Notice that the following positions do not correspond to
c         the same array).
c
          startfield(datiftype, irec, ifile) = lnscal + 1
c
        enddo
      enddo
c
c     Labelling compressed files as closed.
c
      do i = 1, mxciofiles
        ciopen(i)  = .false.
        cioscra(i) = .false.
      enddo
c
      mustcheckcio = .false.
c
      return
      end
c     --- End of routine cioinit0w
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioinit1w
c
c     Initializing the scaling variables associated to the compressed
c     file record fields defined in routine cioinit0w.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2003.
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
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      integer           ifile, irec, iftype, ifield
      integer           rlen, trlend, ncontrecd, nblocksd
c
c     FIRST EXECUTABLE STATEMENT
c
c     Invoking the scale setting procedure.
c
      ifile = npofiles
      call cioscaling(mxciofiles, mxciortype, mxcioftypes, mxciofields,
     +                ifile, ciofminv, ciofmaxv,
     +                ciofwsc0, ciofwsc1, ciofrsc0, ciofrsc1,
     +                ciodynfield)
c
c     Setting internal arrays and scaling coefficients.
c
      do ifile = 1, npofiles
        do irec = 0, nrectypes(ifile)
c
c         Evaluating scaling coefficients.
c
c         For fields of type one, only the second sub-field of each
c         set is scaled. The first subfields go ahead in the list
c         of non-scaled fields.
c         This loop is for the set of second subfields only:
c
          ifield = nrecfield(1, irec, ifile)
c
          do i = 1, nrecfield(1, irec, ifile)
            ifield = ifield + 1
            if (ifield .ge. cioffscaled(irec, ifile)) then
              call ciosetscale1(ifile, irec, ifield, 1)
            endif
          enddo
c
c         The subfields in the other types are ordered consecutively.
c
c         Integer coded fields.
c
          do iftype = 2, lastscaledtype1
c
            do i = 1, nrecfield(iftype, irec, ifile)
              do j = 1, nsubfields(iftype)
                ifield = ifield + 1
                if (ifield .ge. cioffscaled(irec, ifile)) then
                  call ciosetscale1(ifile, irec, ifield, iftype)
                endif
              enddo
            enddo
          enddo
c
c         Floating point fields.
c
          do iftype = lastscaledtype1 + 1, lastscaledtype2
c
            do i = 1, nrecfield(iftype, irec, ifile)
              do j = 1, nsubfields(iftype)
                ifield = ifield + 1
                call ciosetscale2(ifile, irec, ifield, iftype)
              enddo
            enddo
          enddo
c
c         Fields than can be added dynamically.
c
          if (ciodynrecty(irec, ifile)) then
c
            if (ciodynfield(irec, ifile) .le. 0) then
              call errprint(0, '$CI7', 4, 'cioinit1w',
     +        'Max. dynamically added fields is 0 for record number:',
     +        1, irec, 0, 0.d0, pofiletitle(ifile))
            endif
c
c           Re-evaluating the number of continuation records needed to
c           save the record.
c
            trlend    = cioreclen00(irec, ifile)
     +                      + dynadflen * ciodynfield(irec, ifile)
     +                      + dynadcountlen
            rlen      = cioreclen(0, ifile)
            ncontrecd = (trlend - 1) / rlen
            nblocksd  = ncontrecd + 1
c
            if ((nblocksd * rlen) .gt. ciobuflen(ifile)) then
              call errprint(0, '$CI7', 4, 'cioinit1w',
     +        'Defined alternative record is too long. Record number:',
     +        1, irec, 0, 0.d0, pofiletitle(ifile))
            endif
c
            ciofillpoint(irec, ifile) = ciobuflen(ifile) -
     +                                               rlen * ncontrecd
c
c           Setting the scaling factors for the added fields.
c
            ifield = ifield + 1
            call ciosetscale2(ifile, irec, ifield, dynadftype)
c
          endif
c
          ciodynfwcix(irec, ifile) = ifield
c
        enddo
      enddo
c
      return
      end
c     --- End of routine cioinit1w
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciosetscale1(ifile, irec, ifield, iftype)
c
c     Setting read and write scaling factors for compressed i/o coding
c     (integer numbers).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) The file being defined.
c     irec............ (input, integer) Record type.
c     ifield.......... (input, integer) Field index.
c     iftype.......... (input, integer) Field type.
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
      integer           ifile, irec, ifield, iftype
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  xmin, xmax, xx(2)
      equivalence       (xx(1), xmin), (xx(2), xmax)
      double precision  ymin, ymax
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scaling:
c
c     For writing:     x <- w1 * y + w0
c                      i <- wca * (y + wcb)
c     if x = xmin then i = 0, if x = xmax then i = imax
c
c     For reading:     x <- r1 * (rca * i + rcb) + r0
c     (The same x is reobtained when r1 = 1 and r0 = 0).
c
c     Checking minimum and maximum.
c
      xmin = ciofminv(ifield, irec, ifile)
      xmax = ciofmaxv(ifield, irec, ifile)
c
      if (xmin .eq. xmax) then
        call errprint(0, '$CI7', 4, 'ciosetscale1',
     +  'Invalid range for real field. Record and range are listed',
     +  1, irec, 2, xx, ciofname(ifield, irec, ifile))
      endif
c
      if (ciofwsc1(ifield, irec, ifile) .eq. 0) then
        call errprint(0, '$CI7', 4, 'ciosetscale1',
     +  'Zero multiplicative bias. Record is listed.',
     +  1, irec, 0, 0.d0, ciofname(ifield, irec, ifile))
      endif
c
c     Switching accordingly with the linear or log scale.
c
      if (cioflogs(ifield, irec, ifile)) then
c
c       Logarithmic scaling (y = w1 * log(x) + w0).
c
        if ((xmin .le. 0) .or. (xmax .le. 0)) then
          call errprint(0, '$CI7', 4, 'ciosetscale1',
     +    'Invalid range for log field. Record and range are listed',
     +    1, irec, 2, xx, ciofname(ifield, irec, ifile))
        endif
c
        xmin = log(xmin)
        xmax = log(xmax)
      endif
c
      ymin = (xmin - ciofwsc0(ifield, irec, ifile))
     +       / ciofwsc1(ifield, irec, ifile)
c
      ymax = (xmax - ciofwsc0(ifield, irec, ifile))
     +       / ciofwsc1(ifield, irec, ifile)
c
c     Coefficients for writing.
c     (Notice the 0.5 factor for rounding.)
c
      ciofwca(ifield, irec, ifile) = maxfieldint(iftype) /
     +                               (ymax - ymin)
      ciofwcb(ifield, irec, ifile) = (0.5d0 /
     +                                ciofwca(ifield, irec, ifile))
     +                               - ymin
c
c     Coefficients for reading.
c
      ciofrca(ifield, irec, ifile) = ciofrsc1(ifield, irec, ifile) *
     +                               (xmax - xmin) /
     +                               maxfieldint(iftype)
      ciofrcb(ifield, irec, ifile) = ciofrsc0(ifield, irec, ifile) +
     +                               ciofrsc1(ifield, irec, ifile) *
     +                               xmin
c
      return
      end
c     --- End of routine ciosetscale1.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciosetscale2(ifile, irec, ifield, iftype)
c
c     Setting read and write scaling factors for compressed i/o coding
c     (real numbers).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) The file being defined.
c     irec............ (input, integer) Record type.
c     ifield.......... (input, integer) Field index.
c     iftype.......... (input, integer) Field type.
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
      integer           ifile, irec, ifield, iftype
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  xmin, xmax, xx(2)
      equivalence       (xx(1), xmin), (xx(2), xmax)
      double precision  ymin, ymax, zmin, zmax
      logical           setpos
      double precision  fltmax
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scaling:
c
c     For writing:     x <- w1 * y + w0
c                      f <- wca * (y + wcb)
c     if |x| = |xmin| then f = 0, if |x| = |xmax| then f = fmax
c
c     For reading:     x <- r1 * (rca * f + rcb) + r0
c
c     Checking minimum and maximum.
c     If the maximum is negative then it is interpreted as maximum
c     for the entire (positive and negative) range of real numbers;
c     otherwise it is taken as maximum for positive numbers only.
c     Additionally, if cioflogs is true, it is interpreted that
c     the numbers to code are all positive, and therefore the range
c     of exponents will be larger (the sign field will be used
c     to enlarge the exponent field).
c
      xmax   = ciofmaxv(ifield, irec, ifile)
      xmin   = ciofminv(ifield, irec, ifile)
      setpos = (xmax .ge. 0)
      xmax   = abs(xmax)
      xmin   = abs(xmin)
c
      if (ciofwsc1(ifield, irec, ifile) .eq. 0) then
        call errprint(0, '$CI7', 4, 'ciosetscale2',
     +  'Zero multiplicative bias. Record is listed.',
     +  1, irec, 0, 0.d0, ciofname(ifield, irec, ifile))
      endif
c
      ymin = abs((xmin - ciofwsc0(ifield, irec, ifile))
     +           / ciofwsc1(ifield, irec, ifile))
c
      ymax = abs((xmax - ciofwsc0(ifield, irec, ifile))
     +           / ciofwsc1(ifield, irec, ifile))
c
      if (ymin .eq. ymax) then
        call errprint(0, '$CI7', 4, 'ciosetscale2',
     +  'Invalid range for flt. field. Record and range are listed',
     +  1, irec, 2, xx, ciofname(ifield, irec, ifile))
      endif
c
      if (setpos) then
        if (cioflogs(ifield, irec, ifile)) then
          fltmax = maxfieldfltx(iftype)
        else
          fltmax = maxfieldflt(iftype)
        endif
      else
        fltmax = - maxfieldfltn(iftype)
        ymin   = - ymin
        ymax   = - ymax
      endif
c
      zmin = ciofwsc1(ifield, irec, ifile) * ymin +
     +       ciofwsc0(ifield, irec, ifile)
c
      zmax = ciofwsc1(ifield, irec, ifile) * ymax +
     +       ciofwsc0(ifield, irec, ifile)
c
c     Coefficients for writing.
c     (The factor for rounding cannot be evaluated here. Instead,
c     it must be calculated after knowning the exponent of the number
c     to be coded).
c
      ciofwca(ifield, irec, ifile) = fltmax / (ymax - ymin)
      ciofwcb(ifield, irec, ifile) = - ymin
c
c     Coefficients for reading.
c
      ciofrca(ifield, irec, ifile) = ciofrsc1(ifield, irec, ifile) *
     +                               (zmax - zmin) / fltmax
      ciofrcb(ifield, irec, ifile) = ciofrsc0(ifield, irec, ifile) +
     +                               ciofrsc1(ifield, irec, ifile) *
     +                               zmin
c
      return
      end
c     --- End of routine ciosetscale2.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioopenw(file, newsw, basescrafn)
c
c     Opening compressed i/o files for writing.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2000, 2002,
c                                         2003.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
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
c     basescarfn...... (input, character*(*)) String to use as a
c                      base-name for the associated scratch file.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
      include 'versionpar.f'
c
c     Declaration of arguments.
c
      integer           file, newsw
      character*(*)     basescrafn
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           unit, i, j, k, verlen, irc
      character*12      intheader
      character*24      version
      character*12      filext
      character*36      headintegers
      character*4       errmess
      logical           chkr, fap
c
c     cdf specific header.
c
      character*32      cdfheader0, cdfheader
      parameter         (cdfheader0 = '--- AIRES cdf --- ')
      integer           cdfintheader0, cdfintheader
      parameter         (cdfintheader0 = 0)
c
c     FIRST EXECUTABLE STATEMENT
c
c     It is assumed that the initializing routine routine has already
c     been called.
c
c     Checking the file index.
c
      if ((file .le. 0) .or. (file .gt. npofiles)) then
        call errprint(0, '*', 4, 'cioopenw',
     +                'Out of range compressed file index.',
     +                1, file, 0, 0.d0, pofiletitle(file))
      endif
c
c     If newsw is negative the unit is the scratch unit 59.
c     If the file is already open, no action is taken unless
c     newsw is negative.
c
      if (newsw .ge. 0) then
        if (ciopen(file)) return
        unit = minciout + file
        fap  = ((newsw .eq. 0) .or. (newsw .eq. 2))
        chkr = .true.
      else
        unit = 59
        fap  = .true.
        chkr = .false.
      endif
c
      if (pofileno(file) .le. 0) then
        auxfilestring = leadingfn(1)(1:leadingfnlen(1)) //
     +                  pofilext(file)
      else if (pofileno(file) .lt. 10000) then
        write(auxfilestring, 2005) leadingfn(1)(1:leadingfnlen(1)),
     +                             '_s', pofileno(file),
     +                             pofilext(file)
 2005   format(2a, i4.4, a)
      else
        write(auxfilestring, 2006) leadingfn(1)(1:leadingfnlen(1)),
     +                             '_s', pofileno(file),
     +                             pofilext(file)
 2006   format(2a, i6.6, a)
      endif
c
c     Opening the file.
c
 2010 format(400a)
c
      errmess = '$CI1'
      if ((newsw .eq. 0) .or. (newsw .eq. 1)) then
c
c       Opening the file as an empty new file.
c
        open(unit, file = auxfilestring, status = 'UNKNOWN',
     +             err = 3010)
c
c       Writing the file header and the control character arrays.
c
        errmess = '$CI2'
        version = aires_version
        write(headintegers, 2020) pofileno(file),
     +                            codemax1, ciochecksum(file)
 2020   format(3i12)
c
        write(intheader, 2022) cdfintheader0
 2022   format(i10)
c
        write(unit, 2010, err = 3010)
     +        cdfheader0, intheader,
     +        version, pofilext(file), headintegers,
     +        (ciochrdigits(i), i = 0, codemax1),
     +        ((numalchr(i, j), i = 1, 2), j = 1, 3),
     +        (spechr(i), i = 1, 32)
c
c       Writing the file header.
c
        call cioputheaders(file, unit)
c
c       Initializing the respective data buffer as empty, and the
c       number of definitively written data blocks to zero.
c
        cioreclast(file) = 0
        ciorlastcp(file) = 0
        ciowblocks(file) = 0
c
      else
c
c       Opening an existing file to check or append data.
c
c       It will be first opened normally, and then the first record
c       will be read in, to check that the ascii sequences used
c       are the same that those used to initialize the file.
c
        open(unit, file = auxfilestring, status = 'OLD',
     +             err = 3020)
c
c       Reading the first record.
c
        errmess = '$CI3'
        read(unit, 2010, err = 3010, end = 3010)
     +       cdfheader, intheader, version, filext, headintegers,
     +       ciorecord0(1:codebase+38)
c
c       Checking header.
c
        cdfintheader = -999999
        read(intheader, *, err = 1010) i
        cdfintheader = i
 1010   continue
c
        if ((cdfheader .ne. cdfheader0) .or.
     +      (cdfintheader .ne. cdfintheader0)) then
          call errprint(0, '*', 4, 'cioopenw',
     +     'This file seems not to be a valid AIRES compressed file.',
     +     1, unit, 0, 0.d0, pofilext(file))
        endif
c
c       Checking version.
c
        call strim(24, version, verlen)
        call versioncheck(version(1:verlen), i, irc)
        if (irc .ne. 0) then
          if (irc .gt. 0) then
            i = 4
          else
            i = 3
          endif
          call errprint(0, '$CI4', i, 'cioopenw', ' ',
     +                  1, unit, 0, 0.d0, pofiletitle(file))
        endif
c
c       Checking that the opened file corresponds to the same
c       extension, file name number, coding base and checksum.
c
        read(headintegers, *) i, j, k
c
        if ((filext .ne. pofilext(file)) .or.
     +      (i .ne. pofileno(file))) then
          call errprint(0, '$CI5', 4, 'cioopenw', ' ',
     +                  1, i, 0, 0.d0, ciorecord0(1:12))
        endif
c
        if ((j .ne. codemax1) .or. (k .ne. ciochecksum(file))) then
          call errprint(0, '$CI8', 4, 'cioopenw', ' ',
     +                  1, unit, 0, 0.d0, ' ')
        endif
c
c       Checking the ascii arrays.
c
        do i = 0, codemax1
          k = i + 1
          if (ciorecord0(k:k) .ne. ciochrdigits(i)) goto 3020
        enddo
c
        do j = 1, 3
          do i = 1, 2
            k = k + 1
            if (ciorecord0(k:k) .ne. numalchr(i, j)) goto 3020
          enddo
        enddo
c
        do i = 1, 32
          k = k + 1
          if (ciorecord0(k:k) .ne. spechr(i)) goto 3020
        enddo
c
c       All header checks passed.
c       It is assumed that the data buffer is initialized
c       independently.
c
c       Performing integrity checks if necessary.
c
        if (chkr) then
c
          if (mustcheckcio) then
            call ciofcheck(file, unit)
          endif
c
c         Resetting some variables related to buffer data.
c
          ciorecord2(file) = ciorecord1(file)
          ciorlastcp(file) = cioreclast(file)
c
        endif
      endif
c
c     All done, the file is closed.
c
      close(unit)
c
c     Checking if the file must be reopened for appending data
c     or if a scratch file is opened instead.
c
      if (fap) then
c
c       Positioning at the end of the file.
c
        call appendopen(unit, auxfilestring, 'UNKNOWN', 'FORMATTED', i)
        if (i .ne. 0) goto 3020
        cioscra(file) = .false.
c
      else
c
c       Opening the scratch file.
c
        auxfilestring = basescrafn // pofilext(file)
c
        open(unit, file = auxfilestring, status = 'UNKNOWN',
     +             err = 3030)
        cioscra(file) = .true.
c
      endif
c
      ciopen(file) = .true.
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(2, errmess, 4, 'cioopenw', ' ',
     +              1, unit, 0, 0.d0, pofiletitle(file))
      return
c
 3020 continue
c
      call errprint(2, '$CI1', 4, 'cioopenw',
     +              '(Opening for appending data)',
     +              1, unit, 0, 0.d0,
     +              pofilext(file))
      return
c
 3030 continue
c
      call errprint(2, '$CI1', 4, 'cioopenw',
     +              '(Auxiliary scratch file).',
     +              1, unit, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine cioopenw
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioreopenw(file, lastosave, deltano,
     +                      newsw, basescrafn)
c
c     Reopening a given compressed file. The currently opened file is
c     flushed and closed, and a new file (with increased file number)
c     is opened.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) The index of the file to
c                      reopen.
c     lastosave....... (input, integer) Last position to save
c                      (parameter to be passed to flushing routine,
c                      see "cioflush"). If negative then all the buffer
c                      is saved. The non-saved part of the buffer
c                      remains within it.
c     deltano......... (input, integer) The increment to the file
c                      base-name number.
c     newsw........... (input, integer) New-old switch to use in the
c                      reopening call. If 0 or 1 a new file is
c                      initialized, otherwise the file is tested for
c                      existence. Also if newsw is positive and not
c                      equal to 2, then a scratch file is opened for
c                      data writing, otherwise the main file remains
c                      open and positioned at the current end of file.
c                      If newsw is negative it is assumed that a
c                      scratch file is already opened and an
c                      alternative FORTRAN unit number (59) is used
c                      therefore. This parameter should be equal to
c                      1 for normal operation with this routine.
c     basescrafn...... (input, character*(*)) String to use as a
c                      base-name for the associated scratch file.
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
      integer           file, lastosave, deltano, newsw
      character*(*)     basescrafn
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           rlast
c
c     FIRST EXECUTABLE STATEMENT
c
c     Flushing the corresponding buffer, and saving buffer size.
c
      call cioflush(file, lastosave)
      rlast = cioreclast(file)
c
c     Closing the currently open file.
c
      call ciosaveclose(file)
c
c     Increasing the file number.
c
      pofileno(file) = pofileno(file) + deltano
c
c     Reopening the file, and restoring the buffer size.
c
      call cioopenw(file, newsw, basescrafn)
      cioreclast(file) = rlast
      ciorlastcp(file) = rlast
c
      return
      end
c     --- End of routine cioreopenw
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioclose
c
c     Closing all compressed i/o files. Non-empty buffers are left
c     unchanged. To ensure that every record is effectively written
c     into the file, it is necessary to use routine "cioflush"
c     before closing the files.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
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
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ifile
c
c     FIRST EXECUTABLE STATEMENT
c
      do ifile = 1, npofiles
        call ciosaveclose(ifile)
      enddo
c
      return
      end
c     --- End of routine cioclose
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciosaveclose(ifile)
c
c     Closing a specified compressed i/o file. Non-empty buffers are
c     left unchanged. To ensure that every record is effectively
c     written into the file, it is necessary to use routine "cioflush"
c     before closing the files.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) The file being saved and
c                      closed.
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
      integer           ifile
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ut, nwblock
c
c     FIRST EXECUTABLE STATEMENT
c
      ut = minciout + ifile
c
c     If the file is open as a scratch file, it is
c     appendend to the corresponding master file.
c
      if (ciopen(ifile) .and. cioscra(ifile)) then
c
c       Opening temporarily the master file as unit 59.
c
        call cioopenw(ifile, -1, ' ')
c
c       Positioning the scratch file at the beginning.
c
        endfile(ut)
        rewind(ut)
c
c       Copying data from the scratch file.
c
        nwblock = 0
 1010   continue
        read(ut, 2010, err = 3010, end = 1020)
     +           ciorecord0(1:ciobuflen(ifile))
 2010   format(a)
        write(59, 2010, err = 3020) ciorecord0(1:ciobuflen(ifile))
        nwblock = nwblock + 1
        goto 1010
 1020   continue
        close(59)
c
c       The scratch file is deleted.
c
        close(ut, status = 'DELETE')
c
c       Updating the number of blocks effectively written.
c
        ciowblocks(ifile) = ciowblocks(ifile) + nwblock
c
      else
c
c       The file is just closed.
c
        close(ut)
c
      endif
c
      ciopen(ifile)  = .false.
      cioscra(ifile) = .false.
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$CI3', 4, 'ciosaveclose',
     +  '(Error reading the corresponding auxiliary scratch file).',
     +  1, ut, 0, 0.d0, pofiletitle(ifile))
      return
c
 3020 continue
      call errprint(0, '$CI2', 4, 'ciosaveclose',
     +  '(Error writing master file opened as scratch unit).',
     +  1, ut, 0, 0.d0, pofiletitle(ifile))
      return
c
      end
c     --- End of routine ciosaveclose
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioputheaders(ifile, ut)
c
c     Writing the header of compressed i/o files. This routine is
c     called at the beginning of a task.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     ut.............. (input, integer) The FORTRAN i/o unit number
c                      used in the open instruction.
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
      include 'ciocomm.f'
c
c     Declaration of arguments.
c
      integer           ifile, ut
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, nrec
c
c     FIRST EXECUTABLE STATEMENT
c
c     FIRST PART OF THE HEADER: Simulation program specific variables.
c
      call cioputheader1(ifile, ut)
c
 2001 format(400a)
 2010 format(16i11)
c
c     Header separator.
c
      write(ut, 2020, err = 3010)
     +          '*-*-* ', ifile, ' *-*-*'
 2020 format(a, i6, 3a)
c
c     SECOND PART OF THE HEADER: Data coding specifications.
c
c     Data about all cio files currently defined.
c
      write(ut, 2010, err = 3010) npofiles
      if (npofiles .gt. 0)
     +   write(ut, 2001, err = 3010) (pofilext(i), i = 1, npofiles)
c
c     Brief description of the file.
c
      write(ut, 2001, err = 3010) pofiletitle(ifile)
c
c     Some sizes of arrays.
c
      write(ut, 2010, err = 3010)
     +          mxciorsize, mxcioftypes, lastdefrecftype
c
c     Basic data related with coding.
c
      write(ut, 2010, err = 3010)
     +          codemax1h, lastscaledtype1, lastscaledtype2,
     +          (ciofieldlen(i), i = 1, mxcioftypes)
      write(ut, 2010, err = 3010)
     +          (ciofieldid(i), i = 1, mxcioftypes)
      write(ut, 2010, err = 3010)
     +          (nsubfields(i), i = 1, mxcioftypes)
c
      write(ut, 2010, err = 3010)
     +          (ciopclecodedata(i), i = 1, 3)
c
c     Record length, number of different records and related
c     variables.
c
      write(ut, 2010, err = 3010)
     +          ciobuflen(ifile), nrectypes(ifile)
c
      nrec = nrectypes(ifile)
c
      write(ut, 2010, err = 3010)
     +          (cioreclen(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (ciofillpoint(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (totrecfields(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (ciodynfield(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (ciodynfwcix(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (cioffscaled(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (ciolfnoscal(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (ciolfscale1(i, ifile), i = 0, nrec)
      write(ut, 2010, err = 3010)
     +          (cionfscale2(i, ifile), i = 0, nrec)
c
c     Record definitions record by record.
c
      do i = 0, nrec
c
        write(ut, 2001, err = 3010) ciorecname(i, ifile)
        write(ut, 2010, err = 3010)
     +            (nrecfield(j, i, ifile), j = 1, mxcioftypes)
        write(ut, 2010, err = 3010)
     +            (startfield(j, i, ifile), j = 1, mxcioftypes)
c
c       Field definitions.
c
        do j = 1, max(ciodynfwcix(i, ifile), totrecfields(i, ifile))
c
          write(ut, 2080, err = 3010)
     +              ciofname(j, i, ifile),
     +              ciofminv(j, i, ifile), ciofmaxv(j, i, ifile),
     +              ciofwsc0(j, i, ifile), ciofwsc1(j, i, ifile),
     +              ciofrsc0(j, i, ifile), ciofrsc1(j, i, ifile),
     +              ciofwca(j, i, ifile), ciofwcb(j, i, ifile),
     +              ciofrca(j, i, ifile), ciofrcb(j, i, ifile),
     +              cioflogs(j, i, ifile)
 2080     format(a, 1p, 10g24.14e3, l5)
c
        enddo
      enddo
c
c     End of file header.
c
      call dati(ciorecord0)
      write(ut, 2020, err = 3010)
     +          '*+*+* ', ifile, '      ', ciorecord0(1:20), ' *+*+*'
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$CI2', 4, 'cioputheaders', ' ',
     +              1, ut, 0, 0.d0, pofiletitle(ifile))
      return
c
      end
c     --- End of routine cioputheaders.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciowriterecord(file, rectype, intfields, realfields)
c
c     Writing a compressed record (any record type).
c
c     Written by: S. J. Sciutto, La Plata 1996, 2003.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     rectype......... (input, integer) Record type. Ranges from 0
c                      (default record) to the number of alternative
c                      records defined when initializing the
c                      corresponding file.
c     intfields....... (input, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). No control is done
c                      on the integers ranges.
c     realfields...... (input, double precision, array(0:*)) Real
c                      fields of the record. Ranges are not checked.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           file, rectype
      integer           intfields(1)
      double precision  realfields(0:1)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           lastchar, lastcha0
c
c     FIRST EXECUTABLE STATEMENT
c
c     Selecting accordingly with the record type.
c
      if (rectype .gt. 0) then
c
c       Alternative records.
c
c       Checking if the corresponding record is already filled.
c
        if (cioreclast(file) .ge. ciofillpoint(rectype, file)) then
          call cioshipgen(file)
        endif
c
        lastcha0 = cioreclast(file)
        lastchar = lastcha0
c
        call ciowritealtrec(file, rectype, intfields, realfields,
     +                      lastchar)
c
        if (ciodynfield(rectype, file) .gt. 0) then
c
c         No dynamical added fields written in this case.
c
          call intcode2(1, 0, ciorecord1(file), lastchar)
c
c         Updating position of last used buffer character.
c
          lastchar         = 1 + (lastchar - lastcha0 - 1) /
     +                           cioreclen(0, file)
          cioreclast(file) = lastcha0 + lastchar *
     +                                  cioreclen(0, file)
c
        else
c
c         Fixed length record type.
c
          cioreclast(file) = lastcha0 + cioreclen(rectype, file)
c
        endif
c
      else
c
c       Default record.
c
        call ciowritedefrec(file, intfields, realfields)
c
      endif
c
      return
      end
c     --- End of routine ciowriterecord.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciowriterecordwaf(file, rectype, intfields,
     +                             realfields, ntoadd, realtoadd)
c
c     Writing a compressed record (alternative record types) with
c     added fields. The correspondind record type must be defined
c     to accept such added fields. If it is not the case, this
c     routine executes similarly as "ciowriterecord", silently
c     ignoring the added fields.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     rectype......... (input, integer) Record type. Ranges from 0
c                      (default record) to the number of alternative
c                      records defined when initializing the
c                      corresponding file.
c     intfields....... (input, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). No control is done
c                      on the integers ranges.
c     realfields...... (input, double precision, array(0:*)) Real
c                      fields of the record. Ranges are not checked.
c     ntoadd.......... (input, integer) Number of records to add.
c     realtoadd....... (input, double precision, array(ntoadd)) Real
c                      fields to be added. Ranges are not checked.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           file, rectype
      integer           intfields(1)
      double precision  realfields(0:1)
      integer           ntoadd
      double precision  realtoadd(ntoadd)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, itoadd, lastchar, lastcha0, ki
      double precision  transflt(mxciofields + 600)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Selecting accordingly with the record type.
c
      if (rectype .gt. 0) then
c
c       Alternative records.
c
c       Checking if the corresponding record is already filled.
c
        if (cioreclast(file) .ge. ciofillpoint(rectype, file)) then
          call cioshipgen(file)
        endif
c
        lastcha0 = cioreclast(file)
        lastchar = lastcha0
c
        call ciowritealtrec(file, rectype, intfields, realfields,
     +                      lastchar)
c
c       Processing records to be added dynamically.
c
        if (ciodynrecty(rectype, file)) then
c
          itoadd   = min(max(ntoadd, 0), ciodynfield(rectype, file))
          ki       = ciodynfwcix(rectype, file)
c
          call intcode2(1, itoadd, ciorecord1(file), lastchar)
c
          do i = 1, itoadd
            transflt(i) = ciofwca(ki, rectype, file) *
     +                      (realtoadd(i) + ciofwcb(ki, rectype, file))
          enddo
c
          call fltcode5(itoadd, transflt, ciorecord1(file), lastchar)
c
c         Updating position of last used buffer character.
c
          lastchar         = 1 + (lastchar - lastcha0 - 1) /
     +                           cioreclen(0, file)
          cioreclast(file) = lastcha0 + lastchar *
     +                                  cioreclen(0, file)
c
        else
c
c         Fixed length record type.
c
          cioreclast(file) = lastcha0 + cioreclen(rectype, file)
c
        endif
c
      else
c
c       Default record.
c
        call ciowritedefrec(file, intfields, realfields)
c
      endif
c
      return
      end
c     --- End of routine ciowriterecord.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciowritealtrec(file, rectype, intfields, realfields,
     +                          lastchar)
c
c     Writing a compressed record (only alternative record type).
c
c     Written by: S. J. Sciutto, La Plata 1996, 2003.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     rectype......... (input, integer) Record type. Ranges from 1
c                      to the number of alternative records defined
c                      when initializing the corresponding file.
c                      Must not be zero.
c     intfields....... (input, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). No control is done
c                      on the integers ranges.
c     realfields...... (input, double precision, array(0:*)) Real
c                      fields of the record. Ranges are not checked.
c     lastchar........ (input-output, integer) As input: Position of
c                      first character of buffer to use, minus 1.
c                      As output: Last used character of buffer.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           file, rectype, lastchar
      integer           intfields(1)
      double precision  realfields(0:1)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, ki, jns
      integer           ist(mxcioftypes)
      integer           nfi(mxcioftypes)
      integer           transformed(mxciofields)
      double precision  transflt(mxciofields)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Encoding and saving the data (non default record case).
c
c     Data reordering and scaling.
c
c     Escape control field and first fields of type 1.
c
      transformed(1) = codemax1h - rectype
c
c     Non scaled fields.
c
      jns = ciolfnoscal(rectype, file)
      do i = 1, jns
        transformed(i + 1) = intfields(i)
      enddo
c
c     Scaling.
c
      jns = jns + 1
      j   = jns
      ki  = jns
      k   = cioffscaled(rectype, file)
      do i = 0, ciolfscale1(rectype, file)
        j  = j + 1
        ki = k + i
        transformed(j) = ciofwca(ki, rectype, file) *
     +                     (realfields(i) +
     +                      ciofwcb(ki, rectype, file))
      enddo
c
c     Scaling floating point fields.
c
      i = ciolfscale1(rectype, file)
      do j = 1, cionfscale2(rectype, file)
        ki = ki + 1
        transflt(j) = ciofwca(ki, rectype, file) *
     +                  (realfields(i + j) +
     +                     ciofwcb(ki, rectype, file))
      enddo
c
c     Starting positions.
c
      do i = 1, mxcioftypes
        ist(i) = startfield(i, rectype, file)
        nfi(i) = nrecfield(i, rectype, file)
      enddo
c
c     Data encoding.
c
c     Type 1 fields.
c
c     First type 1 integer field is a particle code (if it exists).
c
      if (nfi(1) .gt. 1) transformed(2) = transformed(2) + maxpcle
c
      call intintcode4(nfi(1),
     +                 transformed(1), transformed(ist(1)),
     +                 ciorecord1(file), lastchar)
c
c     Type 2 fields.
c
      call intintcode5(nfi(2), transformed(ist(2)),
     +                 ciorecord1(file), lastchar)
c
c     Type 3 fields.
c
      call intcode2(nfi(3), transformed(ist(3)),
     +              ciorecord1(file), lastchar)
c
c     Type 4 fields.
c
      call fltfltcode5(nfi(4), transflt(1),
     +                 ciorecord1(file), lastchar)
c
c     Type 5 fields.
c
      call fltcode2(nfi(5), transflt(ist(5)),
     +              ciorecord1(file), lastchar)
c
c     Type 6 fields.
c
      call fltcode5(nfi(6), transflt(ist(6)),
     +              ciorecord1(file), lastchar)
c
c     Type 7 fields.
c
      do i = 1, nfi(7)
        call daticode(intfields(jns), ciorecord1(file), lastchar)
        jns = jns + 6
      enddo
c
      return
      end
c     --- End of routine ciowritealtrec.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciowritedefrec(file, intfields, realfields)
c
c     Writing a compressed record (only default record type).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     intfields....... (input-scratch, integer, array(*)) Integer
c                      fields of the record. No control is done on the
c                      integers ranges. Positions beyond the last
c                      input field are used as scratch working space.
c                      Minimum dimension of this array = total number
c                      of record fields plus 1.
c     realfields...... (input, double precision, array(0:*)) Real
c                      fields of the record. Ranges are not checked.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'pclepar.f'
c
c     Declaration of arguments.
c
      integer           file
      integer           intfields(1)
      double precision  realfields(0:1)
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, ki, lastchar
      integer           ist(mxcioftypes)
      integer           nfi(mxcioftypes)
      double precision  transflt(mxciofields)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the corresponding record is already filled.
c
      if (cioreclast(file) .ge. ciofillpoint(0, file)) then
        call cioshipfull(file)
      endif
c
c     Encoding and saving the data.
c
c     Data reordering and scaling.
c
c     Escape control field is an input field in the default record (it
c     is treated as a particle code), and the last input position in
c     the integer array corresponds to the last non-scaled integer.
c     Further elements are used as working scratch space.
c
c     First integer field is a particle code.
c
      intfields(1) = intfields(1) + maxpcle
c
c     Scaling.
c
      k  = cioffscaled(0, file)
      ki = ciolfnoscal(0, file)
      do i = 0, ciolfscale1(0, file)
        ki = k + i
        intfields(ki) = ciofwca(ki, 0, file) *
     +                    (realfields(i) + ciofwcb(ki, 0, file))
      enddo
c
c     Scaling floating point fields.
c
      i = ciolfscale1(0, file)
      do j = 1, cionfscale2(0, file)
        ki = ki + 1
        transflt(j) = ciofwca(ki, 0, file) *
     +                  (realfields(i + j) + ciofwcb(ki, 0, file))
      enddo
c
c     Starting positions.
c
      do i = 1, lastdefrecftype
        nfi(i) = nrecfield(i, 0, file)
        ist(i) = startfield(i, 0, file)
      enddo
c
c     Data encoding.
c
      lastchar = cioreclast(file)
c
c     Type 1 fields.
c
      call intintcode4(nfi(1), intfields, intfields(ist(1)),
     +                 ciorecord1(file), lastchar)
c
c     Type 2 fields.
c
      call intintcode5(nfi(2), intfields(ist(2)),
     +                 ciorecord1(file), lastchar)
c
c     Type 3 fields.
c
      call intcode2(nfi(3), intfields(ist(3)),
     +              ciorecord1(file), lastchar)
c
c     Type 4 fields.
c
      call fltfltcode5(nfi(4), transflt(1),
     +                   ciorecord1(file), lastchar)
c
c     Type 5 fields.
c
      call fltcode2(nfi(5), transflt(ist(5)),
     +                ciorecord1(file), lastchar)
c
c     No fields of type greater than 5 in the default record.
c
      cioreclast(file) = lastchar
c
      return
      end
c     --- End of routine ciowritedefrec.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioshipgen(file)
c
c     Hard writing a compressed data buffer. If the record is not full,
c     then a null record is appended to mark the data end point.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
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
      integer           file
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if the record is full.
c
      if (cioreclast(file) .lt. ciofillpoint(0, file)) then
c
c       The record is not completely full. Marking the data end.
c
        call intintcode4(1, codemax1h, codemax1h,
     +                   ciorecord1(file), cioreclast(file))
      endif
c
c     Hard writing the record.
c
      call cioshipfull(file)
c
      return
      end
c     --- End of routine cioshipgen.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioshipfull(file)
c
c     Hard writing a (full) compressed data buffer.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
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
      integer           file
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ut
c
c     FIRST EXECUTABLE STATEMENT
c
      ut = minciout + file
c
      write(ut, 2010, err = 3010) ciorecord1(file)(1:ciobuflen(file))
 2010 format(a)
c
      cioreclast(file) = 0
c
      return
c
c     Error message.
c
 3010 continue
      call errprint(0, '$CI2', 4, 'cioshipfull', ' ',
     +              1, ut, 0, 0.d0, pofiletitle(file))
      return
      end
c     --- End of routine cioshipfull.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioflush(file, lastosave)
c
c     Flushing a compressed file buffer.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     lastosave....... (input, integer) Last position to save. If
c                      negative then all the buffer is saved. The
c                      non-saved part of the buffer remains within it.
c     
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
      integer           file, lastosave
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           newptr, oldptr
c
c     FIRST EXECUTABLE STATEMENT
c
      newptr = cioreclast(file)
      if (lastosave .lt. 0) then
        oldptr = cioreclast(file)
      else
        if ((lastosave .gt. newptr) .or.
     +      (lastosave .le. 0)) return
        oldptr = lastosave
      endif
c
c     Hard writing the data.
c
      ciorecord0       = ciorecord1(file)
      cioreclast(file) = oldptr
      call cioshipgen(file)
c
c     Resetting the buffer.
c
      if (newptr .gt. oldptr) then
        cioreclast(file) = newptr - oldptr
        ciorecord1(file) = ciorecord0(oldptr + 1 : newptr)
      else
        cioreclast(file) = 0
      endif
c
      return
      end
c     --- End of routine cioflush.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function ciogeteofblock(file)
c
c     Getting the last used position of the compressed file buffer.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     file............ (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c
c     Return value: (integer) The corresponding last position.
c     ============
c
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
      integer           ciogeteofblock
      integer           file
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      ciogeteofblock = cioreclast(file)
c
      return
      end
c     --- End of routine ciogeteofblock.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciocheckstatus
c
c     Checking the status of existing cio files. It is assumed that
c     this routine is invoked after setting relevant data with the
c     information contained within the IDF file.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
      include 'versionpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ifile
      integer           xversion, xdlfac
      integer           xnpofiles, xpofileno
      character*64      xpofiletitle
      character*12      xpofilext
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking the existence of the internal file. If it does not
c     exists, then it means that previous i/o operations ended
c     successfully.
c
      auxfilestring = leadingfn(1)(1:leadingfnlen(1)) // '.CIOInfo'
c
      inquire(file = auxfilestring, exist = mustcheckcio)
c
      if (mustcheckcio) then
c
c       The file exists. Opening and reading it.
c
        open(57, file = auxfilestring, status = 'OLD',
     +           form = 'UNFORMATTED', err = 3010)
c
        read(57, end = 3010, err = 3010) auxline(1:70)
c
c       Reading relevant status data.
c
        read(57, end = 3010, err = 3010)
     +            xversion, xdlfac, xnpofiles
c
        if ((aires_dlfac .ge. 0) .and.
     +      (xversion .ne. aires_version_no)) goto 3020
        if (xnpofiles .ne. npofiles) goto 3020
c
        if (xnpofiles .gt. 0) then
          do ifile = 1, xnpofiles
            read(57, end = 3010, err = 3010)
     +                xpofiletitle, xpofilext, xpofileno,
     +                cioreclast(ifile), ciorlastcp(ifile),
     +                ciowblocks(ifile), ciopen(ifile)
            if (xpofiletitle .ne. pofiletitle(ifile)) goto 3020
            if (xpofilext    .ne. pofilext(ifile))    goto 3020
            if (xpofileno    .ne. pofileno(ifile))    goto 3020
            if (ciorlastcp(ifile) .gt. 0) then
              read(57, end = 3010, err = 3010)
     +                  ciorecord2(ifile)(1:ciorlastcp(ifile))
            endif
          enddo
        endif
c
        close(57, err = 3010)
c
      endif
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A30', 4, 'ciocheckstatus', ' ',
     +              1, 57, 0, 0.d0, ' ')
      return
c
 3020 continue
      call errprint(0, '$A30', 4, 'ciocheckstatus',
     +              '(Parameter inconsistency)',
     +              1, 57, 0, 0.d0, ' ')
      return
      end
c     --- End of routine ciocheckstatus.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciosavestatus
c
c     Generating a temporary file containing relevant status variables
c     of the compressed i/o system
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
      include 'versionpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ifile
c
c     FIRST EXECUTABLE STATEMENT
c
c     Opening the internal file and writing its header.
c
      auxfilestring = leadingfn(1)(1:leadingfnlen(1)) // '.CIOInfo'
c
      open(57, file = auxfilestring, status = 'UNKNOWN',
     +         form = 'UNFORMATTED', err = 3010)
c
      auxline = '--- AIRES Internal Info File. DO NOT EDIT!! ---'
      write(57, err = 3010) auxline(1:70)
c
c     Writing relevant status data.
c
      write(57, err = 3010) aires_version_no, aires_dlfac, npofiles
c
      if (npofiles .gt. 0) then
        do ifile = 1, npofiles
          write(57, err = 3010) pofiletitle(ifile), pofilext(ifile),
     +                          pofileno(ifile),
     +                          cioreclast(ifile), ciorlastcp(ifile),
     +                          ciowblocks(ifile), ciopen(ifile)
          if (ciorlastcp(ifile) .gt. 0) then
            write(57, err = 3010)
     +                ciorecord2(ifile)(1:ciorlastcp(ifile))
          endif
        enddo
      endif
c
      close(57, err = 3010)
c
      return
c
c     Error message.
c
 3010 continue
      call errprint(0, '$A09', 4, 'ciosavestatus', ' ',
     +              1, 57, 0, 0.d0, ' ')
      return
      end
c     --- End of routine ciosavestatus.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciormstatus
c
c     Removing the internal info file used when updation cio files.
c
c     Written by: S. J. Sciutto, La Plata 2000, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'ciocomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      auxfilestring = leadingfn(1)(1:leadingfnlen(1)) // '.CIOInfo'
      call rmfile(57, auxfilestring)
      mustcheckcio = .false.
c
      return
      end
c     --- End of routine ciormstatus.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciofcheck(ifile, ut)
c
c     Scanning an already opened compressed file to ensure the proper
c     number of written data records. This routine is normally called
c     when recovering from a previous system crash.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     ut.............. (input, integer) The FORTRAN i/o unit number
c                      used in the open instruction.
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
      include 'ciocomm.f'
c
c     Declaration of arguments.
c
      integer           ifile, ut
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the end of the headers.
c
 1010 continue
      read(ut, 2010, end = 3020, err = 3010) ciorecord0
      if (ciorecord0(1:6) .ne. '*+*+* ') goto 1010
 2010 format(a)
c
      read(ciorecord0(7:12), *) i
      if (i .ne. ifile) goto 3020
c
c     Scanning the file up to the last record to keep.
c
      do i = 1, ciowblocks(ifile)
        read(ut, 2010, end = 3030, err = 3010)
     +                  ciorecord0(1:ciobuflen(ifile))
      enddo
c
c     Remarking the end of the file.
c
      endfile(ut, err = 3040)
c
c     Resetting related variables.
c
      ciorecord1(ifile) = ciorecord2(ifile)
      cioreclast(ifile) = ciorlastcp(ifile)
c
c     Logging the operation.
c
      call errprint(2, '$CC1', 1, 'ciofcheck', pofiletitle(ifile),
     +              1, ciowblocks(ifile), 0, 0.d0, ' ')
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$CI3', 4, 'ciofcheck', ' ',
     +              0, 0, 0, 0.d0, pofiletitle(ifile))
      return
c
 3020 continue
      call errprint(0, '$CI3', 4, 'ciofcheck',
     +              '(Invalid or incomplete header)',
     +              0, 0, 0, 0.d0, pofiletitle(ifile))
      return
c
 3030 continue
      call errprint(0, '$CI3', 4, 'ciofcheck',
     +              '(Premature end of file)',
     +              0, 0, 0, 0.d0, pofiletitle(ifile))
      return
c
 3040 continue
      call errprint(0, '$CI2', 4, 'ciofcheck',
     +              'Cannot write end of file mark.',
     +              1, ut, 0, 0.d0, pofiletitle(ifile))
      return
c
      end
c     --- End of routine ciofcheck.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciomgr1.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
