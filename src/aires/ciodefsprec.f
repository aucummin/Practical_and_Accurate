c
c     FILE: ciodefsprec.f                   Creation date: 27/OCT/1999.
c                                       LAST MODIFICATION: 06/AUG/2003.
c
c     Compressed i/o system initializing parameters (III):
c
c     Definition of records associated with the special particles.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciodefsprec(maxfiles, maxrectypes,
     +                       fieldtypes, maxfields, nfiles, rno,
     +                       nrectypes, recname, nrecfield, firscaled,
     +                       fieldlogs, fieldname, dynrecty)
c
c     Defining the format of records to use in compressed data files
c     to store data for the special primaries.
c     This routine must be called from inside the main record
c     definition routine.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
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
c     nfiles.......... (input, integer) The label of the file being
c                      defined.
c     rno............. (input-output, integer) As input this parameter
c                      gives the label of the last defined record.
c                      It is then increased by the number of new
c                      record types defined.
c     nrectypes....... (input-output, integer, array(maxfiles))
c                      The number of different record types for each
c                      file (excluding the default record).
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
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           maxfiles, maxrectypes
      integer           fieldtypes, maxfields, nfiles, rno
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
      integer           fno
c
c     FIRST EXECUTABLE STATEMENT
c
c     Increasing the number of defined record types.
c
      nrectypes(nfiles) = nrectypes(nfiles) + 2
c
c     Defining the fields for the first additional record.
c     ---------------------------------------------------
c
      rno = rno + 1
      recname(rno, nfiles) = 'External primary particle'
c
c     Description of the fields.
c
c     Fields of type 1: Two integers in 1 1/2 + 2 1/2 bytes.
c     This is mandatory for every record different from the default
c     record: The same or more fields of type 1 than in the default
c     record.
c     There will be two such set of fields in this record.
c     Field 1 cannot be used for non-default record since it stores
c     the escape code. Field 2 stores the shower number.
c
      nrecfield(1, rno, nfiles) = 2
      fno = 1
c
c     Field number 2: Particle code. Can range between 0 and
c                     (3374-1) - 4 = 3369. The values 3370,...,3374
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
c     Field number 3: Energy. Stored as a scaled logarithm.
c
c     Corresponds to real parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Energy (GeV) (log)'
      fieldlogs(fno, rno, nfiles) = .true.
c
c     Field number 4: X component of the unitary vector pointing the
c                     direction of motion of the particle. Stored
c                     linearly scaled.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +           'Direction of motion (x component)'
c
c     Fields of type 2: Two integers in 2 1/2 plus 2 1/2 bytes.
c     1 compound field (2 sub-fields):
c               (Uy, Uz).
c               (fields 5 and 6)
c
      nrecfield(2, rno, nfiles) = 1
c
c     Field number 5: Y component of the unitary vector pointing the
c                     direction of motion of the particle. Stored
c                     linearly scaled.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +           'Direction of motion (y component)'
c
c     Field number 6: Z component of the unitary vector pointing the
c                     direction of motion of the particle. Stored
c                     linearly scaled.
c
c     Corresponds to real parameter 4.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +           'Direction of motion (z component)'
c
c      Fields of type 3: One integer in 2 bytes.
c      0 fields of this type.
c
c     Fields of type 4: Two real numbers in 2 1/2 plus 2 1/2 bytes.
c     3 compound fields (6 sub-fields):
c               (X coordinate, Y coordinate)
c               (Z coordinate, Atmospheric depth).
c               (Injection time (ns), particle weight)
c               (fields 7 to 12)
c
      nrecfield(4, rno, nfiles) = 3
c
c     Field number 7: X coordinate of the primary particle. Stored
c                     as a floating point number.
c
c     Corresponds to real parameter 5.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'X coordinate (m)'
c
c     Field number 8: Y coordinate of the primary particle. Stored
c                     as a floating point number.
c
c     Corresponds to real parameter 6.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Y coordinate (m)'
c
c     Field number 9: Z coordinate of the primary particle. Stored
c                     as a floating point number.
c
c     Corresponds to real parameter 7.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Z coordinate (m)'
c
c     Field number 10: Injection depth. Stored as a floating point
c                      number.
c
c     Corresponds to real parameter 8.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Injection depth (g/cm2)'
c
c     Field number 11: Injection time. Stored as a floating point
c                      number.
c
c     Corresponds to real parameter 9.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Injection time (ns)'
c
c     Field number 12: Weight. Stored as a strictly positive
c                      floating point number.
c
c     Corresponds to real parameter 10.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Particle weight'
      fieldlogs(fno, rno, nfiles) = .true.
c
c     No more fields for the "External primary particle" record.
c
c
c     Defining the fields for the second additional record.
c     ---------------------------------------------------
c
      rno = rno + 1
      recname(rno, nfiles) = 'Special primary trailer record'
c
c     Description of record fields.
c
c     Fields of type 1: Two integers in 1 1/2 + 2 1/2 bytes.
c     This is mandatory for every record different from the default
c     record: The same or more fields of type 1 than in the default
c     record.
c     There will be one such set of fields in this record.
c     1 1/2 fields: Field 1 cannot be used for non-default record since
c     it stores the escape code, and field 2 is the version assigned
c     to the external module used to process the primaries.
c
      nrecfield(1, rno, nfiles) = 1
      fno = 1
c
c     Field number 2: Version of external module. Stored as an integer.
c
c     Corresponds to integer parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Version of external module.'
c
c     The following field is the first to be scaled.
c
      firscaled(rno, nfiles) = fno + 1
c
c     Fields of type 2: Two integers in 2 1/2 plus 2 1/2 bytes.
c     0 fields of this type.
c
c     Fields of type 3: One integer in two bytes.
c     0 fields of this type.
c
c     Fields of type 4: Two real numbers in 2 1/2 plus 2 1/2 bytes.
c     0 fields of this type.
c
c     Fields of type 5: One real number in 2 bytes.
c     0 fields of this type.
c
c     Fields of type 6: One real number in 5 bytes.
c
c     3 fields: Number of primary particles (weighted and unweighted),
c               and total energy of the primaries.
c     (fields 3 to 5).
c
      nrecfield(6, rno, nfiles) = 3
c
c     Field number 3: Number of primary particles. Stored as a floating
c                     point number.
c
c     Corresponds to real parameter 1.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Total number of primaries'
c
c     Field number 4: Unweighted Number of primary entries. Stored as a
c                     floating point number.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) = 'Unweighted primary entries'
c
c     Field number 5: Energy of primary particles. Stored as a floating
c                     point number.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldname(fno, rno, nfiles) =
     +                   'Total energy of primary particles (GeV)'
c
c     Fields of type 7: Date and time in 6 bytes.
c     0 fields of this type.
c
c     Enabling dynamically added fields.
c
      dynrecty(rno, nfiles) = .true.
c
c     No more fields for the "Special primary trailer record" record.
c
c     No more definitions for special primary associated records.
c
      return
      end
c     --- End of routine ciodefsprec
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioscalsprec(maxfiles, maxrectypes,
     +                        fieldtypes, maxfields, ifile, rno,
     +                        fieldminv, fieldmaxv,
     +                        fieldwsc0, fieldwsc1,
     +                        fieldrsc0, fieldrsc1,
     +                        dynaddfields)
c
c     Setting minimum and maximum values and read and write linear
c     mappings for the scaled fields of the special primary particle
c     data file records.
c
c     The different fields and records are as defined in routine
c     "cioscalsprec"
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
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
c     ifile........... (input, integer) Index of the file whose
c                      parameters are set.
c     rno............. (input-output, integer) As input this parameter
c                      gives the label of the last processed record.
c                      It is then increased by the number of new
c                      record types processed.
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
      integer           maxfiles, maxrectypes
      integer           fieldtypes, maxfields, ifile, rno
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
      integer           fno
      integer           getinpint
c
c     FIRST EXECUTABLE STATEMENT
c
c     Scaling factors for the "External primary particle" record.
c     ----------------------------------------------------------
c
      rno = rno + 1
c
c     Field number 3 is first scaled field
c
      fno = 3
c
c     Field number 3: Energy. Stored as a scaled logarithm. The
c                     resulting integer lies between 0 and 759374.
c                     Minimum value is the minimum cut energy;
c                     maximum is 1.01 * maximum primary energy.
c
c     Corresponds to real parameter 1.
c
      fieldminv(fno, rno, ifile) = mincutegy
      fieldmaxv(fno, rno, ifile) = 1.01d0 * pryenergymax
c
c     Field number 4: X component of the unitary vector pointing the
c                     direction of motion of the particle. Stored
c                     linearly scaled. The resulting integer lies
c                     between 0 and 759374.
c                     Minimum is -1 and maximum is 1.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = -1
      fieldmaxv(fno, rno, ifile) =  1
c
c     Field number 5: Y component of the unitary vector pointing the
c                     direction of motion of the particle. Stored
c                     linearly scaled. The resulting integer lies
c                     between 0 and 759374.
c                     Minimum is -1 and maximum is 1.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = -1
      fieldmaxv(fno, rno, ifile) =  1
c
c     Field number 6: Z component of the unitary vector pointing the
c                     direction of motion of the particle. Stored
c                     linearly scaled. The resulting integer lies
c                     between 0 and 759374.
c                     Minimum is -1 and maximum is 1.
c
c     Corresponds to real parameter 4.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = -1
      fieldmaxv(fno, rno, ifile) =  1
c
c     Field number 7: X coordinate of the primary particle, stored as a
c                     floating point number.
c                     Minimum value is 0, maximum is 2000 km. The
c                     negative value indicates that the maximum (which
c                     is the absolute value) also holds for negative
c                     coordinates.
c
c     Corresponds to real parameter 5.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = -2.d6
c
c     Field number 8: Y coordinate of the primary particle, stored as a
c                     floating point number.
c                     Minimum value is 0, maximum is 2000 km. The
c                     negative value indicates that the maximum (which
c                     is the absolute value) also holds for negative
c                     coordinates.
c
c     Corresponds to real parameter 6.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = -2.d6
c
c     Field number 9: Z coordinate of the primary particle, stored as a
c                     floating point number.
c                     Minimum value is 0, maximum is 2000 km. The
c                     negative value indicates that the maximum (which
c                     is the absolute value) also holds for negative
c                     coordinates.
c
c     Corresponds to real parameter 7.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = -2.d6
c
c     Field number 10: Injection depth. Stored as a floating point
c                      number. Maximum is 1500.
c
c     Corresponds to real parameter 8.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1500.d0
c
c     Field number 11: Injection time. Stored as a floating point
c                      number. Additionally, the internal time unit
c                      used in AIRES (such that c = 1 m / unit) is
c                      changed to nanoseconds before encoding the
c                      data. Maximum is set to 1000 km / c. The
c                      negative value indicates that the maximum
c                      (which is the absolute value) also holds for
c                      negative times.
c
c     Corresponds to real parameter 9.
c
      fno = fno + 1
      fieldwsc1(fno, rno, ifile) = ucspeedns
      fieldmaxv(fno, rno, ifile) = -1.d6 * ucspeedns
c
c     Field number 12: Weight. Stored as a strictly positive
c                      floating point number.
c                      Positive variable ranging from 1 to 10^15.
c
c     Corresponds to real parameter 10.
c
      fno = fno + 1
      fieldminv(fno, rno, ifile) = 1.d0
      fieldmaxv(fno, rno, ifile) = 1.d15
c
c     No more scalable fields for the "External primary particle"
c     record.
c
c
c     Scaling factors for the "Special primary trailer record".
c     --------------------------------------------------------
c
      rno = rno + 1
c
c     Field number 2 is last non-scaled field
c
      fno = 2
c
c     Field number 3: Number of primary particles. Stored as a floating
c                     point number (up to 15 ** 112).
c                     Minimum value is 0, maximum is 10^35.
c
c     Corresponds to real parameter 1.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1.d35
c
c     Field number 4: Unweighted Number of primary entries. Stored as a
c                     floating point number (up to 15 ** 112).
c                     Minimum value is 0, maximum is 10^35.
c
c     Corresponds to real parameter 2.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 1.d35
c
c     Field number 5: Energy of primary particles. Stored as a floating
c                     point number (up to 15 ** 112). Minimum value is 0,
c                     maximum is 2 * maximum primary energy.
c
c     Corresponds to real parameter 3.
c
      fno = fno + 1
      fieldmaxv(fno, rno, ifile) = 2 * pryenergymax
c
c     No more scalable fields for the "Special primary trailer record".
c
c     Maximum number of additional records than can be set dynamically,
c     and sccaling for all the added float fields.
c
      dynaddfields(rno, ifile)       = getinpint('SPMaxFieldsToAdd')
      fieldmaxv(fno + 1, rno, ifile) = 1.d100
c
c     No more special primary associated records.
c
      return
      end
c     --- End of routine cioscalsprec
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciodefsprec.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
