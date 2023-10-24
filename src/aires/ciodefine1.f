c
c     FILE: ciodefine1.f                    Creation date: 06/DEC/1996.
c                                       LAST MODIFICATION: 06/AUG/2003.
c
c     Compressed i/o system initializing parameters (I):
c
c     Ground particle file (standard or large record size).
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciodefine1(maxfiles, maxrectypes,
     +                      fieldtypes, maxfields, nfiles,
     +                      filext, filetitle,
     +                      nrectypes, recname, nrecfield, firscaled,
     +                      fieldlogs, fieldname, dynrecty,
     +                      filecateg)
c
c     Defining the format of records to use in compressed data file.
c     To define the format of the compressed data file records, it
c     is necessary to adequately set the arguments, which are the
c     record/field definition arrays.
c
c     Here, a "field" is a sector of the record which forms a data unit.
c     When writing a record, it is necessary to call a subroutine
c     whose parameters are of the form (..., intpars, realpars), where
c     intpars is an integer array while realpars is a real one. The
c     data is taken from these two array accordingly with the following
c     conventions:
c
c       1) Contents of "intpars":   1st type 1 subfield 1
c                                   2nd type 1 subfield 1
c                                   ...
c                                   1st nonscaled integer
c                                   ...
c                                   last nonscaled integer
c                                   first number of 1st date-time
c                                   5 following date-time numbers
c                                   first number of 2nd date-time
c                                   5 following date-time numbers
c                                   ...
c
c       1) Contents of "realpars":  1st scaled variable
c                                   2nd scaled variable
c                                   ...
c                                   1st floating point variable
c                                   2nd floating point variable
c                                   ...
c
c     For the default record, the first element of the integer array
c     corresponds to field number 1. For non default record types,
c     field number one is reserved for the escape control field, and
c     therefore the first element of the integer array is,
c     alternatively: (1) First subfield of second type 1 field, or,
c     (2) The first nonscaled field, or, (3) The first date-time
c     subfield, or, (4) It is not used at all.
c
c     The logical ordering of the fields is the following:
c
c             1st type 1 subfield 1
c             2nd type 1 subfield 1
c             ...
c             1st type 1 subfield 2
c             2nd type 1 subfield 2
c             ...
c             1st type 2 subfield 1
c             1st type 2 subfield 2
c             2nd type 2 subfield 1
c             2nd type 2 subfield 2
c             ...
c             1st type 3 field
c             2nd type 3 field
c             ...
c             1st type 4 subfield 1
c             1st type 4 subfield 2
c             2nd type 4 subfield 1
c             2nd type 4 subfield 2
c             ...
c             1st type 5 field
c             2nd type 5 field
c             ...
c             1st type 6 field
c             2nd type 6 field
c             ...
c             1st type 7 field (6 subfields of 1 byte each)
c             2nd type 7 field
c             ...
c
c     Notice the special sequencing for type 1 subfields.
c
c     It is important to notice also that for EVERY record, if there
c     are one or more subfields 1 of type 1 not used for escape control
c     characters, then the first of these fields is interpreted as
c     a "particle code" and transformed accordingly with particle
c     code coding-decoding rules.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999; La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     maxfiles........ (input, integer) Maximum number of compressed
c                      output data files that can be defined.
c     maxrectypes..... (input, integer) Maximum number of different
c                      record types that can be defined for each file
c                      (excluding the default record).
c     fieldtypes...... (input, integer) Number of available field
c                      types.
c     maxfields....... (input, integer) Maximum number of fields that
c                      can be defined for each record type.
c     nfiles.......... (input-output, integer) As input, it gives the
c                      number of previously defined files. This routine
c                      increments this parameter by one.
c     filext.......... (output, character*(*), array(maxfiles))
c                      Extension to be appended to the base file name
c                      which will uniquely specify the file. Default
c                      is '.cfnn' where nn is the compressed file
c                      number. Maximum length is 12 chars. Reserved
c                      strings are: '.lgf', '.sry', '.tex', '.idf'
c     filetitle....... (output, character*(*), array(maxfiles))
c                      File "title" string. Allows to place a brief
c                      explanation on the file. Maximum 64 chars.
c     nrectypes....... (output, integer, array(maxfiles)) The number
c                      of different record types for each file
c                      (excluding the default record).
c     recname......... (output, character*(*), array(0:maxrectypes,
c                      maxfiles)) Name of each record within each
c                      file. Maximum length: 42 chars.
c     nrecfield....... (output, integer, array(fieldtypes,
c                      0:maxrectypes, maxfiles)) Number of fields of
c                      the corresponding field type that are present
c                      in the defined record. Default is 0.
c     firscaled....... (output, integer, array(0:maxrectypes, maxfiles))
c                      Index of first field to be scaled (and hence
c                      converted to a real variable when reading it).
c                      Default is 2.
c     fieldlogs....... (output, logical, array(maxfields,
c                      0:maxrectypes, maxfiles)) For each field of
c                      type 1 to 3 within each record, this parameter
c                      define if the logarithm of the variable, rather
c                      than the variable itself, is used to evaluate
c                      the corresponding field data. For each field of
c                      type 4 or 5 the parameter defines whether the
c                      variable to code is always positive (fieldlogs
c                      true) in order to use an extended exponent
c                      range. For other field types the specification
c                      is meaningless. The default is .false., which
c                      corresponds to linear scaling (always positive
c                      numbers) for types 1 to 3 (4 or 5).
c     fieldname....... (output, character*(*), array(maxfields,
c                      0:maxrectypes, maxfiles)) Name of each field
c                      within each record. Maximum length: 42 chars.
c     dynrecty........ (output, logical, array(0:maxrectypes,
c                      maxfiles)) TRUE to enable dynamically added
c                      fields for the corresponding records.
c     filecateg....... (output, integer) An integer which defines the
c                      "category" of the file to be defined. It is
c                      an internal parameter used to switch among
c                      different alternatives for record definition.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'cioenable.f'
c
c     Declaration of arguments.
c
      integer           maxfiles, maxrectypes
      integer           fieldtypes, maxfields, nfiles
      integer           filecateg
      character*(*)     filext(maxfiles)
      character*(*)     filetitle(maxfiles)
      integer           nrectypes(maxfiles)
      character*42      recname(0:maxrectypes, maxfiles)
      integer           nrecfield(fieldtypes, 0:maxrectypes, maxfiles)
      integer           firscaled(0:maxrectypes, maxfiles)
      logical           fieldlogs(maxfields, 0:maxrectypes, maxfiles)
      character*42      fieldname(maxfields, 0:maxrectypes, maxfiles)
      logical           dynrecty(0:maxrectypes, maxfiles)
c
c     Declaration of internal variables and arrays.
c
      integer           rno, fno
      logical           norrec, bigrec
c
c     FIRST EXECUTABLE STATEMENT
c
c     I. STANDARD GROUND PARTICLE FILE (Standard or large record size).
c
c     Determining the file category.
c
      filecateg = cioenable_grdpcles
c
c     Normally, compressed files are defined only if their category is
c     positive; but the ground particle file MUST be ALWAYS defined.
c     If its category is zero or negative, it is taken as equal to 1.
c
      if (filecateg .le. 0) filecateg = 1
c
c     Defining the file.
c
c     The number of defined files is incremented by one.
c
      nfiles = nfiles + 1
c
c     If the file category is 1 then the format is set as "short"
c     (direction of motion data are not included); if it is set to 2
c     the format is "normal", and if the category is 3 then the format
c     is set as "long" format (including also particle creation depth
c     and last hadronic interaction depth).
c
      norrec = (filecateg .ge. 2)
      bigrec = (filecateg .ge. 3)
c
c     File name extension string (maximum 12 characters).
c     This string uniquely identifies the file.
c
      filext(nfiles) = '.grdpcles'
c
c     File title.
c
      if (bigrec) then
        filetitle(nfiles) =
     +            'Standard ground particle file (long record)'
      else
        if (norrec) then
          filetitle(nfiles) =
     +            'Standard ground particle file'
        else
          filetitle(nfiles) =
     +            'Standard ground particle file (short record)'
        endif
      endif
c
c     Defining record types.
c
c     0.- (Default) particle data record.
c     1.- Beginning of shower record.
c     2.- End of shower record.
c
      nrectypes(nfiles) = 2
      rno = 0
c
c
c     Defining the fields for the default record.
c     ------------------------------------------
c
      if (filecateg .eq. 1) then
        recname(rno, nfiles) = 'Ground particle record (short)'
      else if (filecateg .eq. 2) then
        recname(rno, nfiles) = 'Ground particle record'
      else
        recname(rno, nfiles) = 'Ground particle record (long)'
      endif
c
c     The default record defines the record length. All other records
c     will be stored using one or more blocks having the same length
c     of this default record.
c     Also, the default record must contain ONLY fields of type 1, 2
c     3, 4 or 5.
c
c     Description of the default fields.
c
c     Fields of type 1: Two integers in 1 1/2 + 2 1/2 bytes.
c     This is mandatory for the default record: 1 or more fields
c     of this kind.
c     Field 1 will be particle code (mandatory) and field 2 particle
c     energy (GeV).
c
      nrecfield(1, rno, nfiles) = 1
      fno = 0
c
c     Field number 1: Particle code. Can range between 0 and
c                     (3374-1) - 2 = 3371. The values 3372,...,3374
c                     are reserved for record type identification.
c
c     Corresponds to integer parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Particle code'
c
c     The following field is the first to be scaled.
c
      firscaled(rno, nfiles) = fno + 1
c
c     Field number 2: Energy. Stored as a scaled logarithm.
c
c     Corresponds to real parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Energy (GeV) (log)'
      fieldlogs(fno, rno, nfiles) = .true.
c
c     Fields of type 2: Two integers in 2 1/2 plus 2 1/2 bytes.
c     1 compound field (2 sub-fields):
c               (r (m), theta) (horizontal coordinates).
c               (fields 3 and 4)
c
      nrecfield(2, rno, nfiles) = 1
c
c     Field number 3: Horizontal distance from the shower axis. Stored
c                     as a scaled logarithm.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Distance from the core (m) (log)'
      fieldlogs(fno, rno, nfiles) = .true.
c
c     Field number 4: Polar angle of the (x, y) horizontal particle
c                     coordinates. Stored linearly scaled.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Ground plane polar angle (radians)'
c
c     The following two fields are not included in "short" format.
c
      if (norrec) then
c
c       Fields of type 3: One integer in 2 bytes.
c       2 fields: direction_x, direction_y. (fields 5 and 6)
c
        nrecfield(3, rno, nfiles) = 2
c
c       Field number 5: X component of the unitary vector pointing the
c                       direction of motion of the particle. Stored
c                       linearly scaled.
c
c       Corresponds to real parameter 4.
c
        fno = fno + 1
        fieldname(fno, rno, nfiles) =
     +           'Direction of motion (x component)'
c
c       Field number 6: Y component of the unitary vector pointing the
c                       direction of motion of the particle. Stored
c                       linearly scaled.
c
c       Corresponds to real parameter 5.
c
        fno = fno + 1
        fieldname(fno, rno, nfiles) =
     +           'Direction of motion (y component)'
c
      endif
c
c     Fields of type 4: Two real numbers in 2 1/2 plus 2 1/2 bytes.
c     1 compound field (2 sub-fields):
c               (arrival time delay (ns), particle weight)
c               (fields 7 and 8)
c
      nrecfield(4, rno, nfiles) = 1
c
c     Field number 7 (5s): Arrival time delay. Stored as a floating
c                          point number.
c
c     Corresponds to real parameter 6 (4s).
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Arrival time delay (ns)'
c
c     Field number 8 (6s): Weight. Stored as a strictly positive
c                          floating point number.
c
c     Corresponds to real parameter 7 (5s).
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Particle weight'
      fieldlogs(fno, rno, nfiles) = .true.
c
c     Fields of type 5: One real number in 2 bytes.
c     0 fields of this type for normal record.
c
      if (bigrec) then
c
c       The following two fields are defined only for the long
c       record version.
c
        nrecfield(5, rno, nfiles) = 2
c
c       Field number 9L: Particle creation depth. Stored as a floating
c                        point number.
c
c       Corresponds to real parameter 8L.
c
        fno = fno + 1
        fieldname(fno, rno, nfiles) =
     +           'Particle creation depth (g/cm2)'
c
c       Field number 10L: Last hadronic interaction depth. Stored as a
c                         floating point number.
c
c       Corresponds to real parameter 9L.
c
        fno = fno + 1
        fieldname(fno, rno, nfiles) =
     +           'Last hadronic interaction depth (g/cm2)'
c
      endif
c
c     No more fields for the default record.
c
c
c     Defining the fields for record type 1: Beginning of shower.
c     -------------------------------------
c
      rno = rno + 1
      recname(rno, nfiles) = 'Beginning of shower record'
c
c     Description of record type 1 fields.
c
c     Fields of type 1: Two integers in 1 1/2 + 2 1/2 bytes.
c     This is mandatory for every record different from the default
c     record: The same or more fields of type 1 than in the default
c     record.
c     There will be two such set of fields in this record.
c     1 1/2 fields: Field 1 cannot be used for non-default record since
c     it stores the escape code, and field 2 is the primary particle
c     code.
c     2 1/2 fields: Field 3 stores the shower number, and field 4 the
c     primary energy.
c
      nrecfield(1, rno, nfiles) = 2
      fno = 1
c
c     Field number 2: Primary particle code. Similar to field number
c                     1 of default record, but without the
c                     restrictions of the escape codes.
c
c     Corresponds to integer parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Primary particle code'
c
c     Field number 3: Shower number.
c
c     Corresponds to integer parameter 2.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Shower number'
c
c     The following field is the first to be scaled.
c
      firscaled(rno, nfiles) = fno + 1
c
c     Field number 4: Primary energy. Stored as a scaled logarithm.
c
c     Corresponds to real parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Primary energy (GeV) (log)'
      fieldlogs(fno, rno, nfiles) = .true.
c
c     Fields of type 2: Two integers in 2 1/2 plus 2 1/2 bytes.
c     1 compound field (2 sub-fields):
c               (Zenith angle, azimuth angle).
c               (fields 5 and 6)
c
      nrecfield(2, rno, nfiles) = 1
c
c     Field number 5: Zenith angle. Stored linearly scaled.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Primary zenith angle (deg)'
c
c     Field number 6: Azimuth angle. Stored linearly scaled.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Primary azimuth angle (deg)'
c
c     Fields of type 3: One integer in two bytes.
c     0 fields of this type.
c
c     Fields of type 4: Two real numbers in 2 1/2 plus 2 1/2 bytes.
c
c     1 set of fields: (Thinning energy, Depth of first interaction.)
c                      (fields 7 and 8)
c
      nrecfield(4, rno, nfiles) = 1
c
c     Field number 7: Thinning Energy. Stored as a strictly positive
c                     floating point number.
c
c     Corresponds to real parameter 4.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Thinning energy (GeV)'
      fieldlogs(fno, rno, nfiles) = .true.
c
c     Field number 8: Depth of first interaction. Stored as a
c                     floating point number.
c
c     Corresponds to real parameter 5.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'First interaction depth (g/cm2)'
c
c     Fields of type 5: One real number in 2 bytes.
c     0 fields of this type.
c
c     Fields of type 6: One real number in 5 bytes.
c
c     2 fields: Central injection altitude and global time shift
c     (fields 9 and 10).
c
      nrecfield(6, rno, nfiles) = 2
c
c     Field number 9: Central injection altitude. Stored as a floating
c                     point number.
c
c     Corresponds to real parameter 6.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Central injection altitude (m)'
c
c     Field number 10: Global time shift. Stored as a floating point
c                      point number.
c
c     Corresponds to real parameter 7.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Global time shift (sec)'
c
c     Fields of type 7: Date and time in 6 bytes.
c
c     1 field: Starting date and time (field 11).
c
      nrecfield(7, rno, nfiles) = 1
c
c     Field number 11: Starting date and time. Stored as a 6 character
c                      encoded date-time specification.
c
c     Corresponds to integer parameters 3 to 8.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Starting date and time'
c
c     No more fields for record type 1.
c
c
c     Defining the fields for record type 2: End of shower.
c     -------------------------------------
c
      rno = rno + 1
      recname(rno, nfiles) = 'End of shower record'
c
c     Description of record type 2 fields.
c
c     Fields of type 1: Two integers in 1 1/2 + 2 1/2 bytes.
c     This is mandatory for every record different from the default
c     record: The same or more fields of type 1 than in the default
c     record.
c     There will be one such set of fields in this record.
c     Field 1 cannot be used for non-default record since it stores
c     the escape code. Field 2 stores the shower number.
c
      nrecfield(1, rno, nfiles) = 1
      fno = 1
c
c     Field number 2: Shower number.
c
c     Corresponds to integer parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = fieldname(3, 1, nfiles)
c
c     Fields of type 2: Two integers in 2 1/2 plus 2 1/2 bytes.
c     0 fields of this type.
c
c     Fields of type 3: One integer in two bytes.
c     1 field: Return code from the Xmax, Nmax fitting routine.
c
      nrecfield(3, rno, nfiles) = 1
c
c     Field number 3: Xmax fit return code
c
c     Corresponds to integer parameter 2.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Xmax fit return code'
c
c     Fields of type 4: Two real numbers in 2 1/2 plus 2 1/2 bytes.
c     0 fields of this type.
c
c     Fields of type 5: One real number in 2 bytes.
c     0 fields of this type.
c
c     Fields of type 6: One real number in 5 bytes.
c     21 fields: total particles, lost particles, low energy particles,
c                ground particles, number of unphysical particles,
c                number of neutrinos, number of particles too near the
c                shower core, idem roughly sampled, idem too far. Depth
c                of shower maximum, charged particles at max, energy of
c                lost particles, energy of low-energy particles, energy
c                of ground particles, energy of unphysical particles,
c                energy of neutrinos, energy lost in the air, energy of
c                particles too near to the shower core, idem roughly
c                sampled, idem too far, cpu time used. (fields 4 to
c                24).
c
      nrecfield(6, rno, nfiles) = 21
c
c     The following field is the first to be scaled.
c
      firscaled(rno, nfiles) = fno + 1
c
c     Field number 4: Total particles. Stored as a floating point
c                     number.
c
c     Corresponds to real parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Total number of shower particles'
c
c     Field number 5: Lost particles. Stored as a floating point number
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Total number of lost particles'
c
c     Field number 6: Low energy particles. Stored as a floating
c                     point number
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Number of low energy particles'
c
c     Field number 7: Ground particles. Stored as a floating point
c                     number.
c
c     Corresponds to real parameter 4.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Number of particles reaching ground'
c
c     Field number 8: Unphysical particles. Stored as a floating point
c                     number.
c
c     Corresponds to real parameter 5.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Total number of unphysical particles'
c
c     Field number 9: No. of neutrinos. Stored as a floating point
c                     number.
c
c     Corresponds to real parameter 6.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Total number of neutrinos'
c
c     Field number 10. Number of particles that were not saved in the
c                      ground particle file because they were too near
c                      to the shower axis. Stored as a floating point
c                      number.
c
c     Corresponds to real parameter 7.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Particles too near to the shower core'
c
c     Field number 11. Number of particles that were processed by the
c                      rough sampling algorithm before deciding to
c                      save them in the ground particle file.
c                      Stored as a floating point number.
c
c     Corresponds to real parameter 8.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Particles in the rough sampling region'
c
c     Field number 12. Number of particles that were not saved in the
c                      ground particle file because they were too far
c                      from the shower axis. Stored as a floating point
c                      number.
c
c     Corresponds to real parameter 9.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Particles too far from the shower core'
c
c     Field number 13: Depth of shower maximum. Stored as a
c                      floating point number.
c
c     Corresponds to real parameter 10.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +          'Shower maximum depth (Xmax) (g/cm2)'
c
c     Field number 14: Charged particles at shower maximum. Stored as
c                      a floating point number.
c
c     Corresponds to real parameter 11.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +          'Total charged particles at shower maximum'
c
c     Field number 15: Energy of lost particles. Stored as a
c                      floating point number.
c
c     Corresponds to real parameter 12.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Energy of lost particles (GeV)'
c
c     Field number 16: Energy of low energetic particles. Stored
c                      as a floating point number (idem energy of
c                      lost particles).
c
c     Corresponds to real parameter 13.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Energy of low-energy particles (GeV)'
c
c     Field number 17: Energy of ground particles. Stored as a
c                      floating point number (idem energy of lost
c                      particles).
c
c     Corresponds to real parameter 14.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Energy of ground particles (GeV)'
c
c     Field number 18: Energy of unphysical particles. Stored as a
c                      floating point number.
c
c     Corresponds to real parameter 15.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Energy of unphysical particles (GeV)'
c
c     Field number 19: Energy of neutrinos. Stored as a floating point
c                      number.
c
c     Corresponds to real parameter 16.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Energy of neutrinos (GeV)'
c
c     Field number 20: Energy lost (by ionization). Stored as a
c                      floating point number.
c
c     Corresponds to real parameter 17.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Energy lost in the air (GeV)'
c
c     Field number 21. Energy of particles that were not saved in the
c                      ground particle file because they were too near
c                      to the shower axis. Stored as a floating point
c                      number.
c
c     Corresponds to real parameter 18.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Energy of particles too near to the core'
c
c     Field number 22. Energy of particles that undergo the rough
c                      sampling algorithm before being saved in the
c                      ground particle file.
c
c     Corresponds to real parameter 19.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Energy of roughly sampled particles'
c
c     Field number 23. Energy of particles that were not saved in the
c                      ground particle file because they were too far
c                      from the shower axis. Stored as a floating point
c                      number.
c
c     Corresponds to real parameter 20.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +         'Energy of particles too far from the core'
c
c     Field number 24: CPU time (1 shower). Stored as a floating
c                      point number.
c
c     Corresponds to real parameter 21.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'CPU time (sec)'
c
c     Fields of type 7: Date and time in 6 bytes.
c     1 field: Ending date. (field 25)
c
      nrecfield(7, rno, nfiles) = 1
c
c     Field number 25: Ending date and time. Stored as a 6 character
c                      encoded date-time specification.
c
c     Corresponds to integer parameters 3 to 8.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Ending date and time'
c
c     No more fields for record type 2.
c
c     Defining records associated with special primary particles
c     (this will add more record types to the already defined ones).
c
      call ciodefsprec(maxfiles, maxrectypes,
     +                 fieldtypes, maxfields, nfiles, rno,
     +                 nrectypes, recname, nrecfield, firscaled,
     +                 fieldlogs, fieldname, dynrecty)
c
c     No more definitions for ground particle file.
c
      return
      end
c     --- End of routine ciodefine1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioscaling1(filecateg, maxfiles, maxrectypes,
     +                       fieldtypes, maxfields, ifile,
     +                       fieldminv, fieldmaxv,
     +                       fieldwsc0, fieldwsc1,
     +                       fieldrsc0, fieldrsc1,
     +                       dynaddfields)
c
c     Setting minimum and maximum values and read and write linear
c     mappings for the scaled fields of the ground particle compressed
c     data file records (standard or large record size).
c
c     The different fields and records are as defined in routine
c     "ciodefine1"
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2002, 2003.
c
c
c     Arguments:
c     =========
c
c     filecateg....... (input, integer) An integer which defines the
c                      "category" of the file to be defined. It is
c                      an internal parameter used to switch among
c                      different alternatives for record definition.
c     maxfiles........ (input, integer) Maximum number of compressed
c                      output data files that can be defined.
c     maxrectypes..... (input, integer) Maximum number of different
c                      record types that can be defined for each file
c                      (excluding the default record).
c     fieldtypes...... (input, integer) Number of available field
c                      types.
c     maxfields....... (input, integer) Maximum number of fields that
c                      can be defined for each record type.
c     ifile........... (input, integer) Index of the file whose
c                      parameters are set.
c     fieldminv,
c     fieldmaxv....... (output, double precision, array(maxfields,
c                      0:maxrectypes, maxfiles)) For each field within
c                      each record, these parameters define the minimum
c                      (maximum) value that the corresponding variable
c                      can take. No default.
c     fieldwsc0,
c     fieldwsc1....... (output, double precision, array(maxfields,
c                      0:maxrectypes, maxfiles)) For each field within
c                      each record, these parameters define the
c                      shift-and-scale transformation (wsc0 + wsc1 * x,
c                      x the corresponding variable) to use before
c                      writing the data. The transformation will not
c                      affect the data reading. Default values are 0
c                      and 1, respectively.
c     fieldrsc0,
c     fieldrsc1....... (output, double precision, array(maxfields,
c                      0:maxrectypes, maxfiles)) For each field within
c                      each record, these parameters define the
c                      shift-and-scale transformation (rsc0 + rsc1 * x,
c                      x the corresponding variable) to use after
c                      reading the data. The transformation will not
c                      affect the data writing. Default values are 0
c                      and 1, respectively.
c     dynaddfields.... (output, integer, array(0:maxrectypes,
c                      maxfiles)) Maximum number of fields than can
c                      be added dynamically. Meaningful only for
c                      records with dynamically added fields enabled.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           filecateg
      integer           maxfiles, maxrectypes
      integer           fieldtypes, maxfields, ifile
      double precision  fieldminv(maxfields, 0:maxrectypes, maxfiles)
      double precision  fieldmaxv(maxfields, 0:maxrectypes, maxfiles)
      double precision  fieldwsc0(maxfields, 0:maxrectypes, maxfiles)
      double precision  fieldwsc1(maxfields, 0:maxrectypes, maxfiles)
      double precision  fieldrsc0(maxfields, 0:maxrectypes, maxfiles)
      double precision  fieldrsc1(maxfields, 0:maxrectypes, maxfiles)
      integer           dynaddfields(0:maxrectypes, maxfiles)
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, rno, fno
      double precision  tmp1
c
c     FIRST EXECUTABLE STATEMENT
c
c     Ground particle file.
c
c     Scaling factors for the default record.
c     --------------------------------------
c
      rno = 0
c
c     Field number 2 is first scaled field
c
      fno = 2
c
c     Field number 2: Energy. Stored as a scaled logarithm. The
c                     resulting integer lies between 0 and 759374.
c                     Minimum value is 0.8 * minumum stop energy,
c                     maximum is 1.05 * maximum primary energy.
c
c     Corresponds to real parameter 1.
c
      fieldminv(fno, rno, ifile) = 0.80d0 * mincutegy
      fieldmaxv(fno, rno, ifile) = 1.05d0 * pryenergymax
c
c     Field number 3: Horizontal distance from the shower axis. Stored
c                     as a scaled logarithm. The resulting integer
c                     lies between 0 and 759374.
c                     Minimum and maximum value can be read from input
c                     file (this helps to control file size).
c                     Notice that in this version the minimum value
c                     is 50 times smaller than the IDL specification.
c                     This enables the partial save zone.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = rminfile(gpcleciofile) / prsratio0
      fieldmaxv(fno, rno, ifile) = rmaxfile(gpcleciofile)
c
c     The value that will be actually evaluated is log(r ** 2), to
c     obtain log(r) it is necessary to divide the original logarithm
c     by 2 before writing the data. To obtain this the following
c     scale factor is set to 0.5 (the default value is 1):
c
      fieldwsc1(fno, rno, ifile) = 0.5d0
c
c     Field number 4: Polar angle of the (x, y) horizontal particle
c                     coordinates. Stored linearly scaled. The
c                     resulting integer lies between 0 and 759374.
c                     Minimum and maximum value are set to cover all
c                     possible angles.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      tmp1 = atan2(-1.d0, -1.d0)
      if (tmp1 .lt. 0) then
        fieldminv(fno, rno, ifile) = -pi
        fieldmaxv(fno, rno, ifile) =  pi
      else
        fieldminv(fno, rno, ifile) = 0.d0
        fieldmaxv(fno, rno, ifile) = twopi
      endif
c
      if (filecateg .gt. 1) then
c
c       The following two fields are not included in "short" format.
c
c       Field number 5: X component of the unitary vector pointing the
c                       direction of motion of the particle. Stored
c                       linearly scaled. The resulting integer lies
c                       between 0 and 50624.
c                       Minimum is -1 and maximum is 1.
c
c       Corresponds to real parameter 4.
c
        fno = fno + 1
        fieldminv(fno, rno, ifile) = -1
        fieldmaxv(fno, rno, ifile) =  1
c
c       Field number 6: Y component of the unitary vector pointing the
c                       direction of motion of the particle. Stored
c                       linearly scaled. The resulting integer lies
c                       between 0 and 50624.
c                       Minimum is -1 and maximum is 1.
c
c       Corresponds to real parameter 5.
c
        fno = fno + 1
        fieldminv(fno, rno, ifile) = -1
        fieldmaxv(fno, rno, ifile) =  1
c
      endif
c
c     Field number 7 (5s): Arrival time delay. Stored as a floating
c                          point number. Additionally, the internal
c                          time unit used in AIRES (such that c = 1 m /
c                          unit) is changed to nanoseconds before
c                          encoding the data. Maximum is set
c                          accordingly with RLimsFile setting. The
c                          negative value indicates that the maximum
c                          (which is the absolute value) also holds for
c                          negative times.
c
c     Corresponds to real parameter 6 (4s).
c
      tmp1 = abs(3 * ucspeedns * rmaxfile(gpcleciofile)
     +             * (1 + sin(pi180 * pryzenithmax)))
      tmp1 = max(tmp1, 0.00007d9)
c
      fno = fno + 1
      fieldwsc1(fno, rno, ifile) = ucspeedns
      fieldmaxv(fno, rno, ifile) = - tmp1
c
c     Field number 8 (6s):  Weight. Stored as a floating point number.
c                           Positive variable ranging from 1 to 10^15.
c
c     Corresponds to real parameter 7 (5s).
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = 1.d0
      fieldmaxv(fno, rno, ifile) = 1.d15
c
      if (filecateg .ge. 3) then
c
c       The following two fields are defined only for the long
c       record version.
c
c       Field number 9L: Particle creation depth. Stored as a floating
c                        point number. Maximum is 1500.
c
c       Corresponds to real parameter 8L.
c
c       Field number 10L: Last hadronic interaction depth. Stored as a
c                         floating point number. Maximum is 1500.
c
c       Corresponds to real parameter 9L.
c
c       Setting the maximum for both fields.
c
        do i = 1, 2
          fno = fno + 1
          fieldmaxv(fno, rno, ifile) = 1500.d0
        enddo
c
      endif
c
c     No more fields for the default record.
c
c
c     Scaling factors for the record type 1: Beginning of shower.
c     -------------------------------------
c
      rno = rno + 1
c
c     Field number 4 is first scaled field
c
      fno = 4
c
c     Field number 4: Primary energy. Stored as a scaled logarithm. The
c                     resulting integer lies between 0 and 759374.
c                     Minimum value is 0.5 * minumum primary energy,
c                     maximum is 1.05 * maximum primary energy.
c
c     Corresponds to real parameter 1.
c
      fieldminv(fno, rno, ifile) = pryenergymin / 2
      fieldmaxv(fno, rno, ifile) = 1.05d0 * pryenergymax
c
c     Field number 5: Zenith angle. Stored linearly scaled. The
c                     resulting integer lies between 0 and 759374.
c                     Minimum is 0 and maximum is 90.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = 0
      fieldmaxv(fno, rno, ifile) = 90
c
c     Field number 6: Azimuth angle. Stored linearly scaled. The
c                     resulting integer lies between 0 and 759374.
c                     Minimum is -180 and maximum is 180.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = -180
      fieldmaxv(fno, rno, ifile) =  180
c
c     Field number 7: Thinning Energy.  Stored as a strictly positive
c                     floating point number.
c                     Minimum value is minumum stop energy,
c                     maximum is maximum primary energy.
c
c     Corresponds to real parameter 4.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = mincutegy
      fieldmaxv(fno, rno, ifile) = pryenergymax
c
c     Field number 8: Depth of first interaction. Stored as a
c                     floating point number.
c                     Minimum value is 0, maximum is 10000.
c
c     Corresponds to real parameter 5.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1.d4
c
c     Field number 9: Central injection altitude. Stored as a floating
c                     point number.
c                     Maximum is 10^7 m.
c
c     Corresponds to real parameter 6.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1.d7
c
c     Field number 10: Global time shift. Stored as a floating point
c                      number.
c                      Maximum is 10 sec.
c
c     Corresponds to real parameter 7.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 10.d0
c
c     No more fields for record type 1.
c
c
c     Scaling factors for the record type 2: End of shower.
c     -------------------------------------
c
      rno = rno + 1
c
c     Field number 3 is last non-scaled field
c
      fno = 3
c
c     Field number 4: Total particles. Stored as a floating point
c                     number (up to 15 ** 112).
c                     Minimum value is 0, maximum is 10^35 (set
c                     below).
c
c     Corresponds to real parameter 1.
c
c     Field number 5: Lost particles. Stored as a floating point number
c                     (idem total particles).
c
c     Corresponds to real parameter 2.
c
c     Field number 6: Low energetic particles. Stored as a floating
c                     point number (idem total particles).
c
c     Corresponds to real parameter 3.
c
c     Field number 7: Ground particles. Stored as a floating point
c                     number (idem total particles).
c
c     Corresponds to real parameter 4.
c
c     Field number 8: Unphysical particles. Stored as a floating point
c                     number (idem total particles).
c
c     Corresponds to real parameter 5.
c
c     Field number 9: No. of neutrinos. Stored as a floating point
c                     number (idem total particles).
c
c     Corresponds to real parameter 6.
c
c     Field number 10. Number of particles that were not saved in the
c                      ground particle file because they were too near
c                      to the shower axis. Stored as a floating point
c                      number (idem total particles).
c
c     Corresponds to real parameter 7.
c
c     Field number 11. Number of particles that were processed by the
c                      rough sampling algorithm before deciding to
c                      save them in the ground particle file.
c                      Stored as a floating point number.
c
c     Corresponds to real parameter 8.
c
c     Field number 12. Number of particles that were not saved in the
c                      ground particle file because they were too far
c                      from the shower axis. Stored as a floating point
c                      number (idem total particles).
c
c     Corresponds to real parameter 9.
c
c     Setting maximum values for fields 4 to 12.
c
      do i = 1, 9
        fno = fno + 1
        fieldmaxv(fno, rno, ifile) = 1.d35
      enddo
c
c     Field number 13: Depth of shower maximum. Stored as a
c                      floating point number (up to 15 ** 112).
c                      Minimum value is 0, maximum is 10^10.
c
c     Corresponds to real parameter 10.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1.d10
c
c     Field number 14: Charged particles at shower maximum. Stored as
c                      a floating point number (up to 15 ** 112).
c                      Minimum value is 0, maximum is 10^25.
c
c     Corresponds to real parameter 11.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1.d35
c
c     Field number 15: Energy of lost particles. Stored as a
c                      floating point number (up to 15 ** 112).
c                      Minimum value is 0, maximum is
c                      2 * maximum primary energy.
c
c     Corresponds to real parameter 12.
c
c     Field number 16: Energy of low energetic particles. Stored
c                      as a floating point number (idem energy of
c                      lost particles).
c
c     Corresponds to real parameter 13.
c
c     Field number 17: Energy of ground particles. Stored as a
c                      floating point number (idem energy of lost
c                      particles).
c
c     Corresponds to real parameter 14.
c
c     Field number 18: Energy of unphysical particles. Stored as a
c                      floating point number (idem energy of lost
c                      particles).
c
c     Corresponds to real parameter 15.
c
c     Field number 19: Energy of neutrinos. Stored as a floating point
c                      number (idem energy of lost particles).
c
c     Corresponds to real parameter 16.
c
c     Field number 20: Energy lost (by ionization). Stored as a
c                      floating point number (idem energy of lost
c                      particles).
c
c     Corresponds to real parameter 17.
c
c     Field number 21. Energy of particles that were not saved in the
c                      ground particle file because they were too near
c                      to the shower axis. Stored as a floating point
c                      number (idem total particles).
c
c     Corresponds to real parameter 18.
c
c     Field number 22. Energy of particles that undergo the rough
c                      sampling algorithm before being saved in the
c                      ground particle file.
c
c     Corresponds to real parameter 19.
c
c     Field number 23. Energy of particles that were not saved in the
c                      ground particle file because they were too far
c                      from the shower axis. Stored as a floating point
c                      number (idem total particles).
c
c     Corresponds to real parameter 20.
c
c     Setting extreme values for fields 15 to 23.
c
      do i = 1, 9
        fno = fno + 1
        fieldminv(fno, rno, ifile) = 0
        fieldmaxv(fno, rno, ifile) = 2 * pryenergymax
      enddo
c
c     Field number 24: CPU time (1 shower). Stored as a floating
c                     point number (up to 15 ** 112).
c                     Minimum value is 0, maximum is 10^20.
c
c     Corresponds to real parameter 21.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1.d20
c
c     No more scalable fields for record type 2.
c
c     Scaling the fields corresponding to the records associated with
c     special primaries.
c
      call cioscalsprec(maxfiles, maxrectypes,
     +                  fieldtypes, maxfields, ifile, rno,
     +                  fieldminv, fieldmaxv,
     +                  fieldwsc0, fieldwsc1,
     +                  fieldrsc0, fieldrsc1,
     +                  dynaddfields)
c
c     No more records for ground particle file.
c
      return
      end
c     --- End of routine cioscaling1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciodefine1.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
