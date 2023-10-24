c
c     FILE: cio2utilsfc.f                   Creation date: 07/AUG/1997.
c                                       LAST MODIFICATION: 17/SEP/1998.
c
c     Aires compressed i/o system (IIIc): Some utilities to process
c     already created compressed files from a C environment.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function fccrofieldindex(fileid, rectype, ifieldname, vrb,
     +                         datype, irc)
c
c     Returning the index corresponding to a given field within a
c     compressed file record. Special version called from C routine
c     "crofieldindexc".
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
c     rectype......... (input, integer) Record type (0 for default
c                      record type).
c     ifieldname...... (input, integer, array(*)) First characters of
c                      field name, encoded as an integer array of ascii
c                      codes.
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
c                      2 for date-time data, and 3 for real data.
c     irc............. (output, integer) Return code. 0 means
c                      successful return.
c
c     Return value: (integer) The field index. Zero if there was an
c     ============  error.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           fccrofieldindex
      integer           fileid, rectype, vrb, datype, irc
      integer           ifieldname(1)
c
c     Declaration of internal variables and arrays.
c
      character*42      fieldname
      integer           lfn
      integer           crofieldindex
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character srting.
c
      call cfchar(ifieldname, fieldname, lfn)
c
c     Invoking the original procedure.
c
      fccrofieldindex = crofieldindex(fileid, rectype,
     +                                fieldname(1:lfn),
     +                                vrb, datype, irc)
c
      return
      end
c     --- End of routine fccrofieldindex
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cio2utilsfc.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
