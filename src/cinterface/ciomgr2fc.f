c
c     FILE: ciomgr2fc.f                     Creation date: 28/MAY/1997.
c                                       LAST MODIFICATION: 02/JUL/2003.
c
c     Aires compressed i/o system (IIc): Auxiliary Routines to process
c     already created compressed files from a C environment.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcopencrofile(iwdir, ifilename, header1, logbase, vrb,
     +                         fileid, irc)
c
c     Opening compressed i/o files for reading. Special version called
c     from C routine "opencrofilec"
c
c     Written by: S. J. Sciutto, La Plata 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     iwdir........... (input, integer, array(*)) The name of the
c                      directory where the file is placed, encoded as
c                      an integer array of ascii codes. It defaults to
c                      the current directory when blank.
c     ifilename....... (input, integer, array(*))) The name of the file
c                      to open, encoded as an integer array of ascii
c                      codes.
c     header1......... (input, integer) Integer switch to select
c                      reading (ge 0) or skipping (lt 0) the first
c                      part of the header.
c     logbase......... (input, integer) Variable to control the
c                      logarithmically scaled fields of the file
c                      records. If logbase is less than 2, then
c                      the returned logarithms will be natural
c                      logarithms. Otherwise base "logbase" will be
c                      returned (decimal ones if logbase = 10).
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2 means that only error messages will
c                      be printed. vrb > 2 is similar to vrb = 2, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     fileid.......... (output, integer) File identification. This
c                      variable should not be changed by the calling
c                      program. It must be used as a parameter of
c                      the reading and closing routines in order to
c                      specify the opened file.
c     irc............. (output, integer) Return code. 0 means
c                      successful return. 1 means successful return
c                      obtained with a file that was written with a
c                      previous AIRES version. 10 means that the file
c                      could be opened normally, but that it seems not
c                      to be a valid Aires compressed data file, or is
c                      a corrupted file; 12 invalid file header; 14
c                      not enough size in some of the internal arrays;
c                      16 format incompatibilities. 20: too many
c                      compressed files already opened. Any other value
c                      indicates an opening / header-reading error (irc
c                      equals the system return code plus 10000).
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iwdir(1), ifilename(1)
      integer           header1, logbase, vrb, fileid, irc
c
c     Declaration of internal variables and arrays.
c
      character*180     wdir
      character*256     filename
      integer           lwd, lfn
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into character variables.
c
      call cfchar(iwdir, wdir, lwd)
      call cfchar(ifilename, filename, lfn)
c
c     Calling the original opening routine.
c
      call opencrofile(wdir(1:lwd), filename(1:lfn),
     +                 header1, logbase, vrb, fileid, irc)
c
      return
      end
c     --- End of routine fcopencrofile
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciomgr2fc.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
