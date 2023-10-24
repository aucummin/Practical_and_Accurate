/*
   FILE: iadfutilsc.c                       Creation date: 26/OCT/2000.
                                        LAST MODIFICATION: 25/NOV/2006.

   C interface to the dump file management utility routines.

*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

#include "fexten.h"
#include "str2iarray.h"

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void loadumpfilec(wdir, itaskname, raw, vrb, irc)
/*
   Reading the dump file associated with a given task.
 
   Written by: S. J. Sciutto, La Plata 2000, 2001, 2003, 2006.
 
 
   Arguments:
   =========

   wdir............ (input, string) The name of the directory
                    where the file is placed. It defaults to the
                    current directory when blank.
   itaskname....... (input, string) Task name, or dump file
                    name.
   raw............. (input, pointer to int) Integer switch to determine
                    whether (raw = 0) or not (raw ne 0) final
                    statistical calculations are applied to output
                    observables.
   vrb............. (input, pointer to int) Verbosity control. If *vrb
                    is zero or negative then no error/informative
                    messages are printed; error conditions are
                    communicated to the calling program via the
                    return code. If *vrb is positive error messages
                    will be printed: *vrb = 1 means that messages
                    will be printed even with successful operations.
                    *vrb = 2,3 means that only error messages will
                    be printed. *vrb > 3 is similar to *vrb = 3, but
                    with the additional action of stopping the
                    program if a fatal error takes place.
   irc............. (output, pointer to int) Return code. 0 means
                    successful return. 8 means that no dump file
                    (in the sequence taskname, taskname.adf,
                    taskname.idf) exists. 12 means invalid file
                    name. Other return codes come from the adf or
                    idf read routines.

*/

/* 
   Declaration of arguments.
*/
  char   *wdir, *itaskname;
  int    *raw, *vrb, *irc;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    iwdirx[180] = {0}, *iwdir;
  int    iitasknamex[65] = {0}, *iitaskname;
  void   str2iarray();

#if FUNDERSCORE == _
     void  fcloadumpfile_();
#else
     void  fcloadumpfile();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the field name string to array. */

  iwdir           = &iwdirx[0];
  iwdirx[179]     = IARREND;
  iitaskname      = &iitasknamex[0];
  iitasknamex[64] = IARREND;

  str2iarray(wdir, iwdir);
  str2iarray(itaskname, iitaskname);

  /*
     Calling the index assignment routine.
  */
#if FUNDERSCORE == _
         fcloadumpfile_
#else
         fcloadumpfile
#endif
                       (iwdir, iitaskname, raw, vrb, irc);

}
/* End of loadumpfilec. */

/* End of file cio2utilsc.c           */
/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
