/*
   FILE: ciomgr2c.c                         Creation date: 28/MAY/1997.
                                        LAST MODIFICATION: 15/NOV/2006.

   C interface to the compressed output data file management routines.

*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

#include "fexten.h"
#include "str2iarray.h"

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void opencrofilec(wdir, filename, header1, logbase, vrb, fileid, irc)
/*
   Opening compressed i/o files for reading. C version.

   Written by S. J. Sciutto, La Plata 1997, 2000, 2001, 2003, 2006.


   Arguments:
   =========

   wdir............ (input, pointer to a string) The name of the
                    directory where the file is placed. It defaults to
                    the current directory when blank.
   filename........ (input, pointer to a string) The name of the file
                    to open.
   header1......... (input, pointer to int) Integer switch to select
                    reading (ge 0) or skipping (lt 0) the first
                    part of the header.
   logbase......... (input, pointer to int) Variable to control the
                    logarithmically scaled fields of the file
                    records. If logbase is less than 2, then
                    the returned logarithms will be natural
                    logarithms. Otherwise base "logbase" will be
                    returned (decimal ones if logbase = 10).
   vrb............. (input, pointer to int) Verbosity control. If *vrb
                    is zero or negative then no error/informative
                    messages are printed; error conditions are
                    communicated to the calling program via the
                    return code. If *vrb is positive error messages
                    will be printed: *vrb = 1 means that messages
                    will be printed even with successful operations.
                    *vrb = 2 means that only error messages will
                    be printed. *vrb > 2 is similar to *vrb = 2, but
                    with the additional action of stopping the
                    program if a fatal error takes place.
   fileid.......... (output, pointer to int) File identification. This
                    variable should not be changed by the calling
                    program. It must be used as a parameter of
                    the reading and closing routines in order to
                    specify the opened file.
   irc............. (output, pointer to int) Return code. 0 means
                    successful return. 1 means successful return
                    obtained with a file that was written with a
                    previous AIRES version. 10 means that the file
                    could be opened normally, but that it seems not
                    to be a valid Aires compressed data file, or is
                    a corrupted file; 12 invalid file header; 14
                    not enough size in some of the internal arrays;
                    16 format incompatibilities. 20: too many
                    compressed files already opened. Any other value
                    indicates an opening / header-reading error (*irc
                    equals the system return code plus 10000).

*/

/*
   Declarations of arguments.
*/
  char    *wdir, *filename;
  int     *header1, *logbase, *vrb, *fileid, *irc;

/*
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int     iwdirx[180] = {0}, *iwdir;
  int     ifilenamex[256] = {0}, *ifilename;
  void    str2iarray();

#if FUNDERSCORE == _
     void  fcopencrofile_();
#else
     void  fcopencrofile();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the strings to arrays. */

  iwdir           = &iwdirx[0];
  iwdirx[179]     = IARREND;
  ifilename       = &ifilenamex[0];
  ifilenamex[255] = IARREND;

  str2iarray(wdir, iwdir);
  str2iarray(filename, ifilename);

  /*
     Calling the open routine.
  */

#if FUNDERSCORE == _
     fcopencrofile_
#else
     fcopencrofile
#endif
                     (iwdir, ifilename, header1, logbase, vrb,
                      fileid, irc);

}
/* End of opencrofilec. */

/* End of file ciomgr2c.c             */

/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
