c
c     FILE: errinit.f                       Creation date: 18/JUN/1996.
c                                       LAST MODIFICATION: 14/APR/2003.
c
c     This file contains the error messages initialization routine.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine errinit
c
c     Error message initialization.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2000,
c                                         2001, 2002, 2003.
c
c
      implicit none
c
c     FIRST EXECUTABLE STATEMENT
c
c     Here are the specifications for the error messages.
c     Message #1, #2, and so on.
c
c     Arguments for each message: 
c
c     1) Integer flag: 2 or greater means start of new error code;
c        1 append a new line of text to current error; 0 or less
c        means append text to the current line of the current error.
c        Argument 3 is used only if this argument is equal or greater
c        than 2 (beginning of new error).
c     2) Name of the error message. A string with maximum length 4.
c        Error names are case sensitive.
c     3) Default severity.
c     4) Text of message.
c
c                      Error code number 1:
      call mtxtappnd(2, '$A01', 4,
     + 'Error opening scratch i/o file. Stack number is listed.')
c
c                      Error code number 2:
      call mtxtappnd(2, '$A02', 4,
     + 'Error writing scratch i/o file. Stack number is listed.')
c
c                      Error code number 3:
      call mtxtappnd(2, '$A03', 4,
     + 'Error reading scratch i/o file. Stack number is listed.')
c
c                      Error code number 4:
      call mtxtappnd(2, '$A04', 4,
     + 'Cannot allocate so many stack positions at a time:')
c
c                      Error code number 5:
      call mtxtappnd(2, '$A05', 4,
     + 'Error reading standard input.')
c
c                      Error code number 6:
      call mtxtappnd(2, '$A06', 3,
     + 'Invalid integer or floating point number. Ignoring.')
c
c                      Error code number 7:
      call mtxtappnd(2, '$A07', 3,
     + 'Unknown input directive. Instruction ignored.')
c
c                      Error code number 8:
      call mtxtappnd(2, '$A08', 4,
     + 'Control should never have arrived to this point!')
c
c                      Error code number G01:
      call mtxtappnd(2, '$G01', 4,
     + 'No more space available to store global variable data.')
c
c                      Error code number G02:
      call mtxtappnd(2, '$G02', 3,
     + 'No name assigned to global variable. Directive ignored.')
c
c                      Error code number G03:
      call mtxtappnd(2, '$G03', 3,
     + 'Dynamic/Static incompatibility for already defined variable.')
      call mtxtappnd(1, '$G03', 3,
     + 'Directive ignored.')
c
c                      Error code number G02:
      call mtxtappnd(2, '$G04', 3,
     + 'Empty or invalid conditional clause. Directive ignored.')
c
c                      Error code number 9:
      call mtxtappnd(2, '$A09', 4,
     + 'Error opening and/or writing internal/output file. ')
      call mtxtappnd(0, '$A09', 4, 'Unit is listed.')
      call mtxtappnd(1, '$A09', 4,
     + 'Check disk quota or protection of corresponding directory.')
c
c                      Error code number 10:
      call mtxtappnd(2, '$A10', 3, 'Syntax error. Directive ignored.')
c
c                      Error code number 11:
      call mtxtappnd(2, '$A11', 3,
     + 'End of file reached while searching for label:')
c
c                      Error code number 12:
      call mtxtappnd(2, '$A12', 3,
     + 'Missing or invalid argument(s) in the following ')
      call mtxtappnd(0, '$A12', 3,
     + 'input directive')
      call mtxtappnd(1, '$A12', 3,
     + '(Argument(s) and/or directive are ignored):')
c
c                      Error code number 13:
      call mtxtappnd(2, '$A13', 4,
     + 'Maximum nesting level of "Input" directives reached.')
      call mtxtappnd(1, '$A13', 4,
     + 'I cannot open another additional input data file.')
c
c                      Error code number 14:
      call mtxtappnd(2, '$A14', 4,
     + 'Error opening input file named:')
c
c                      Error code number B14:
      call mtxtappnd(2, '$B14', 3,
     + 'Input or macro file not found.')
c
c                      Error code number 15:
      call mtxtappnd(2, '$A15', 3,
     +'Invalid Task version. Allowed values range from 0 to 999.')
c
c                      Error code number T15:
      call mtxtappnd(2, '$T15', 2,
     +'Task name/directory too long. Truncating.')
c
c                      Error code number T16:
      call mtxtappnd(2, '$T16', 3,
     +'Directory path too long. Ignoring last specifiaction.')
c
c                      Error code number 16:
      call mtxtappnd(2, '$A16', 3,
     + 'Invalid or missing numeric string. Number treated as zero.')
c
c                      Error code number 17:
      call mtxtappnd(2, '$A17', 3,
     + 'Invalid particle specification. Specification ignored.')
c
c                      Error code number 18:
      call mtxtappnd(2, '$A18', 3,
     + 'Too many primary particles specified. ')
      call mtxtappnd(0, '$A18', 3,
     + 'Statement skipped.')
c
c                      Error code number S18:
      call mtxtappnd(2, '$S18', 3,
     + 'Not enough space to add new special particle names. ')
      call mtxtappnd(0, '$S18', 3,
     + 'Statement skipped.')
c
c                      Error code number 19:
      call mtxtappnd(2, '$A19', 2,
     + 'Invalid weight factor specified. Taken as one.')
c
c                      Error code number 20:
      call mtxtappnd(2, '$A20', 3,
     + 'Numeric parameter(s) invalid or out of range.')
c
c                      Error code number 21:
      call mtxtappnd(2, '$A21', 2,
     + 'Time unit missing or invalid. "Seconds" assumed.')
c
c                      Error code number 22:
      call mtxtappnd(2, '$A22', 2,
     + 'Energy unit missing or invalid. "GeV" assumed.')
c
c                      Error code number 23:
      call mtxtappnd(2, '$A23', 2,
     + 'Length unit missing or invalid. "meters" assumed.')
c
c                      Error code number B23:
      call mtxtappnd(2, '$B23', 2,
     + 'Magnetic field unit missing or invalid. "nanoteslas" assumed.')
c
c                      Error code number R23:
      call mtxtappnd(2, '$R23', 2,
     + 'Angle unit missing or invalid. "degrees" assumed.')
c
c                      Error code number 24:
      call mtxtappnd(2, '$A24', 2,
     + 'Altitude/depth unit missing or invalid. "g/cm2" assumed.')
c
c                      Error code number 25:
      call mtxtappnd(2, '$A25', 3,
     + 'Thinning energy specification missing or invalid. ')
      call mtxtappnd(0, '$A25', 2,
     + 'Directive ignored.')
c
c                      Error code number 26:
      call mtxtappnd(2, '$A26', 2,
     + 'Thinning energy keyword missing. "Relative" assumed.')
c
c                      Error code number S11:
      call mtxtappnd(2, '$S11', 3,
     + 'Invalid site specification.')
c
c                      Error code number F01:
      call mtxtappnd(2, '$F01', 3,
     + 'Unknown file name extension. Directive ignored.')
c
c                      Error code number DS1:
      call mtxtappnd(2, '$DS1', 2,
     + 'The parameter associated with the following directive has')
      call mtxtappnd(1, '$DS1', 2,
     + 'been altered by input specification:')
c
c                      Error code number 72:
      call mtxtappnd(2, '$A72', 2,
     + 'One or more input directives were unsuccessfully processed.')
      call mtxtappnd(1, '$A72', 2,
     + 'The number of errors/warnings is listed.')
c
c                      Error code number 27:
      call mtxtappnd(2, '$A27', 3,
     + 'The following parameter was not set and no default value')
      call mtxtappnd(1, '$A27', 3,
     + 'will be provided.')
c
c                      Error code number 28:
      call mtxtappnd(2, '$A28', 2,
     + 'Multiple definitions for one or more input parameters.')
      call mtxtappnd(1, '$A28', 2,
     + 'Only last definition is taken into account.')
c
c                      Error code number 29:
      call mtxtappnd(2, '$A29', 4,
     + 'One or more fundamental parameters were not set ')
      call mtxtappnd(1, '$A29', 4,
     + '(they are marked with "(X)").')
      call mtxtappnd(1, '$A29', 4,
     + 'No default value is provided for such quantities.')
c
c                      Error code number 79:
      call mtxtappnd(2, '$A79', 4,
     + 'Selected primary energy is too low. Must be greater than')
      call mtxtappnd(1, '$A79', 4,
     + 'highest cut energy.')
      call mtxtappnd(1, '$A79', 4,
     + 'Selected energy and highest threshold are listed (GeV):')
c
c                      Error code number 30:
      call mtxtappnd(2, '$A30', 3,
     + 'Error opening or reading internal file. Unit is listed.')
c
c                      Error code number P30:
      call mtxtappnd(2, '$P30', 4,
     + 'Error processing external module interchange file.')
c
c                      Error code number 31:
      call mtxtappnd(2, '$A31', 3,
     + 'Incompatibility between internal parameters ')
      call mtxtappnd(0, '$A31', 3,
     + 'currently in effect')
      call mtxtappnd(1, '$A31', 3,
     + 'and corresponding ones saved in the internal dump file (IDF).')
c
c                      Error code number 32:
      call mtxtappnd(2, '$A32', 2,
     + 'Saved output file directory not equal to current one.')
c
c                      Error code number 33:
      call mtxtappnd(2, '$A33', 3,
     + 'Saved TaskName and/or version not equal to current ones.')
c
c                      Error code number 34:
      call mtxtappnd(2, '$A34', 4,
     + 'Serious problem when initializing the particle stacks.')
c
c                      Error code number 35:
      call mtxtappnd(2, '$A35', 4,
     + 'String invalid as a directive identifier.')
c
c                      Error code number 36:
      call mtxtappnd(2, '$A36', 4,
     + 'Minimum directive length must be in the range 2-16.')
c
c                      Error code number 37:
      call mtxtappnd(2, '$A37', 4,
     + 'Not enough array size to store new directives/variables.')
c
c                      Error code number 38:
      call mtxtappnd(2, '$A38', 4, 'Invalid input data directive.')
c
c                      Error code number B38:
      call mtxtappnd(2, '$B38', 4, 'Invalid variable identifier.')
c
c                      Error code number 39:
      call mtxtappnd(2, '$A39', 3,
     + 'Both minimum and maximum observing level depths are equal:')
c
c                      Error code number 40:
      call mtxtappnd(2, '$A40', 3,
     + 'Too many observing levels (maximum listed). ')
      call mtxtappnd(0, '$A40', 3,
     + 'Directive ignored.')
c
c                      Error code number 41:
      call mtxtappnd(2, '$A41', 4,
     + 'Inconsistency in injection and/or ground levels.')
      call mtxtappnd(1, '$A41', 4,
     + 'Injection and ground depths (g/cm2) and ')
      call mtxtappnd(0, '$A41', 4, 'altitudes (m) are listed:')
c
c                      Error code number 42:
      call mtxtappnd(2, '$A42', 4,
     + 'Invalid minimum and maximum limits for energy histograms.')
      call mtxtappnd(1, '$A42', 4,
     + 'Minimum and maximum energies (GeV) are listed:')
c
c                      Error code number 43:
      call mtxtappnd(2, '$A43', 2,
     + 'Primary or thinning energy lower than largest cut energy.')
      call mtxtappnd(1, '$A43', 2,
     + 'Resetting. Original and corrected values are listed (GeV):')
c
c                      Error code number 44:
      call mtxtappnd(2, '$A44', 4,
     + 'Missing or invalid particle specification for stack number:')
c
c                      Error code number 45:
      call mtxtappnd(2, '$A45', 4,
     + 'External model name inconsistency.')

c                      Error code '$CI1'
      call mtxtappnd(2, '$CI1', 4,
     + 'Error opening compressed output data file.')
c
c                      Error code '$CI2'
      call mtxtappnd(2, '$CI2', 4,
     + 'Error writing compressed output data file. Unit is listed.')
c
c                      Error code '$CI3'
      call mtxtappnd(2, '$CI3', 4,
     + 'Error reading compressed output data file.')
c
c                      Error code '$CI4'
      call mtxtappnd(2, '$CI4', 4,
     + 'Compressed output data file was not initialised with the')
      call mtxtappnd(1, '$CI4', 4,
     + 'current AIRES version. Unit and file extension are listed.')
c
c                      Error code '$CI5'
      call mtxtappnd(2, '$CI5', 4,
     + 'Not coincident extension or file number recorded in old ')
      call mtxtappnd(0, '$CI5', 4,
     + 'compressed')
      call mtxtappnd(1, '$CI5', 4,
     + 'data file. Recorded number and file extension are listed.')
c
c                      Error code '$CI8'
      call mtxtappnd(2, '$CI8', 4,
     + 'Base or checksum error in old compressed data file header.')
      call mtxtappnd(1, '$CI8', 4,
     + 'Unit is listed.')
c
c                      Error code '$CI9'
      call mtxtappnd(2, '$CI9', 3,
     + 'Serious problem initializing ascii arrays ')
      call mtxtappnd(0, '$CI9', 3,
     + 'from file header data.')
c
c                      Error code '$CI7'
      call mtxtappnd(2, '$CI7', 4,
     + 'Serious problem initializing the compressed i/o system.')
c
c                      Error code '$CC1'
      call mtxtappnd(2, '$CC1', 2,
     + 'Compressed file not properly closed (title is listed).')
      call mtxtappnd(1, '$CC1', 2,
     + 'Number of data blocks after recovery is listed.')
c
c                      Error code '$COF'
      call mtxtappnd(2, '$COF', 4,
     + 'Floating point overflow in compressed number encoding.')
c
c                      Error code '$CU1'
      call mtxtappnd(2, '$CU1', 3,
     + 'There is no file header information available.')
c
c                      Error code '$DF1'
      call mtxtappnd(2, '$DF1', 4,
     + 'Invalid AIRES version format in data file header.')
c
c                      Error code '$DF2'
      call mtxtappnd(2, '$DF2', 4,
     + 'Data file written with a newer AIRES version. Upgrade AIRES.')
c
c                      Error code '$I01'
      call mtxtappnd(2, '$I01', 1,
     + 'Operation successfully completed:')
c
c                      EXAMPLE:
c
c      call mtxtappnd(2, '$Ann', 3,
c     + 'This is an example of a call to mtxtappnd.')
c      call mtxtappnd(1, '$Ann', 3,
c     + 'Second line. In the first call the last number means')
c      call mtxtappnd(1, '$Ann', 3,
c     + 'Severity 3; ')
c      call mtxtappnd(0, '$Ann', 3,
c     + 'appended to 3rd line')
c
c     End of error messages section.
c
      return
      end
c     --- End of routine errinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'errinit.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
