c
c     FILE: ciodefine.f                     Creation date: 04/JUL/1997.
c                                       LAST MODIFICATION: 06/AUG/2003.
c
c     Compressed i/o system initializing parameters (I): Defining the
c     parameters to initialize new output files.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciodefinef(maxfiles, maxrectypes, fieldtypes,
     +                      maxfields, nfiles, filext, filetitle,
     +                      nrectypes, recname, nrecfield, firscaled,
     +                      fieldlogs, fieldname, dynrecty)
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
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     nfiles.......... (output, integer) The number of defined files.
c     maxfiles........ (input, integer) Maximum number of compressed
c                      output data files that can be defined.
c     maxrectypes..... (input, integer) Maximum number of different
c                      record types that can be defined for each file
c                      (excluding the default record).
c     fieldtypes...... (input, integer) Number of available field
c                      types.
c     maxfields....... (input, integer) Maximum number of fields that
c                      can be defined for each record type.
c     nfiles.......... (output, integer) The number of defined files.
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
      integer           maxfiles, maxrectypes
      integer           fieldtypes, maxfields, nfiles
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
c     Declaration of shared data.
c
      include 'cioauxcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      nfiles = 0
c
c     I. Ground particle file.
c
      call ciodefine1(maxfiles, maxrectypes, fieldtypes,
     +                maxfields, nfiles, filext, filetitle,
     +                nrectypes, recname, nrecfield, firscaled,
     +                fieldlogs, fieldname, dynrecty, ciofcateg(1))
c
c     I. Longitudinal tracking particle file.
c
      call ciodefine2(maxfiles, maxrectypes, fieldtypes,
     +                maxfields, nfiles, filext, filetitle,
     +                nrectypes, recname, nrecfield, firscaled,
     +                fieldlogs, fieldname, dynrecty, ciofcateg(2))
c
c     nhwciofiles must be set equal to the number of file definition
c     subroutines called, and must not be greater than parameter
c     mxciofiles + 8.
c
      nhwciofiles = 2
c
      return
      end
c     --- End of routine ciodefinef
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioscaling(maxfiles, maxrectypes, fieldtypes,
     +                      maxfields, nfiles,
     +                      fieldminv, fieldmaxv,
     +                      fieldwsc0, fieldwsc1,
     +                      fieldrsc0, fieldrsc1,
     +                      dynaddfields)
c
c     Setting minimum and maximum values and read and write linear
c     mappings for the scaled fields of the compressed data file
c     records.
c
c     The different fields and records are as defined in routine
c     "ciodefinef"
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
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
c     nfiles.......... (input, integer) Number of defined files.
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
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      integer           maxfiles, maxrectypes
      integer           fieldtypes, maxfields, nfiles
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
      include 'cioauxcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ifile
c
c     FIRST EXECUTABLE STATEMENT
c
c     Setting the parameters for the defined files.
c
c     I. Ground particle file.
c
      ifile = 1
      call cioscaling1(ciofcateg(ifile),
     +                 maxfiles, maxrectypes, fieldtypes,
     +                 maxfields, ifile, fieldminv, fieldmaxv,
     +                 fieldwsc0, fieldwsc1, fieldrsc0, fieldrsc1,
     +                 dynaddfields)
c
c     I. Longitudinal tracking particle file.
c
      ifile = ifile + 1
      call cioscaling2(ciofcateg(ifile),
     +                 maxfiles, maxrectypes, fieldtypes,
     +                 maxfields, ifile, fieldminv, fieldmaxv,
     +                 fieldwsc0, fieldwsc1, fieldrsc0, fieldrsc1,
     +                 dynaddfields)
c
c     Checking consistency between number of defined files.
c
      if ((ifile .ne. nhwciofiles)) then
        call errprint(0, '*', 4, 'cioscaling',
     +                'Serious problem initializing the cio system.',
     +                0, 0, 0, 0.d0, ' ')
      endif
c
      return
      end
c     --- End of routine cioscaling
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciodefine.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
