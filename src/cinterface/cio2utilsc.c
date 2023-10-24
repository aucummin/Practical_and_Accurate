/*
   FILE: cio2utilsc.c                       Creation date: 07/AUG/1997.
                                        LAST MODIFICATION: 15/NOV/2006.

   C interface to the compressed output data file management utility
   routines.

*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

#include "fexten.h"
#include "str2iarray.h"

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

int crofieldindexc(fileid, rectype, fieldname, vrb, datype, irc)
/*
   Returning the index corresponding to a given field within a
   compressed file record. C version.
 
   Written by: S. J. Sciutto, La Plata 1997, 1998, 2001, 2003, 2006.
 
 
   Arguments:
   =========

   fileid.......... (input, pointer to int) Compressed file number.
                    This parameter identifies the file to be used for
                    reading, and is set by the file opening routine.
   rectype......... (input, pointer to int) Record type (0 for default
                    record type).
   fieldname....... (input, pointer to a string) First characters of
                    field name (enough characters must be provided to
                    make an unambiguous specification).
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
   datype.......... (output, pointer to int) The data type that
                    corresponds to the specified field: 1 for integer
                    data, 2 for date-time data, and 3 for real data.
   irc............. (output, pointer to int) Return code. 0 means
                    successful return.
 
   Return value: (integer) The field index. Zero if there was an
   ============  error.
*/

/* 
   Declaration of arguments.
*/
  int    *fileid, *rectype;
  char   *fieldname;
  int    *vrb, *datype, *irc;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    ifieldnamex[42] = {0}, *ifieldname;
  void   str2iarray();

#if FUNDERSCORE == _
     int  fccrofieldindex_();
#else
     int  fccrofieldindex();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the field name string to array. */

  ifieldname      = &ifieldnamex[0];
  ifieldnamex[41] = IARREND;

  str2iarray(fieldname, ifieldname);

  /*
     Calling the index assignment routine.
  */
  return(
#if FUNDERSCORE == _
         fccrofieldindex_
#else
         fccrofieldindex
#endif
                          (fileid, rectype, ifieldname, vrb,
                           datype, irc));

}
/* End of crofieldindexc. */

/* End of file cio2utilsc.c           */
/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
