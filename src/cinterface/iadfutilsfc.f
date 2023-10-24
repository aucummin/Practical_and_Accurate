c
c     FILE: iadfutilsfc.f                   Creation date: 26/OCT/2000.
c                                       LAST MODIFICATION: 02/JUL/2003.
c
c     Some utilities to process already created dump files from a C
c     environment.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine fcloadumpfile(iwdir, iitaskname, raw, vrb, irc)
c
c     Reading the dump file associated with a given task. Special
c     version called from C routine "getdumpfilec".
c
c     Written by: S. J. Sciutto, La Plata 2000, 2003.
c
c
c     Arguments:
c     =========
c
c
c     iwdir........... (input, integer, array(*)) The name of the
c                      directory where the file is placed, encoded
c                      as an integer array of ascii codes.
c     iitaskname...... (input, character*(*)) Task name, or dump file
c                      name, encoded as an integer array of ascii
c                      codes.
c     raw............. (input, integer) Integer switch to determine
c                      whether (raw = 0) or not (raw ne 0) final
c                      statistical calculations are applied to output
c                      observables.
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
c                      successful return. 8 means that no dump file
c                      (in the sequence taskname, taskname.adf,
c                      taskname.idf) exists. 12 means invalid file
c                      name. Other return codes
c                      come from the adf or idf read routines.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           iwdir(1)
      integer           iitaskname(1)
      integer           raw, vrb, irc
c
c     Declaration of internal variables and arrays.
c
      character*180     wdir
      character*64      itaskname
      integer           lwd, ltn
c
c     FIRST EXECUTABLE STATEMENT
c
c     Converting ascii codes into a character srting.
c
      call cfchar(iwdir, wdir, lwd)
      call cfchar(iitaskname, itaskname, ltn)
c
c     Invoking the original procedure.
c
      call loadumpfile(wdir(1:lwd), itaskname(1:ltn), raw, vrb, irc)
c
      return
      end
c     --- End of routine fcloadumpfile.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cio2utilsfc.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
