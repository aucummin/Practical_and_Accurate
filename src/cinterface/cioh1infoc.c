/*
   FILE: cioh1infoc.c                       Creation date: 18/JUN/1998.
                                        LAST MODIFICATION: 02/JUL/2003.

   C interface to the compressed output data file management utility
   routines. II, routines to head the first header of the file..

*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

#include "fexten.h"
#include "str2iarray.h"

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void crotaskidc(tskname, tsknamelen, tskversion, startdate)
/*
     Getting task name and date from an already read compressed file
     header.

     Written by: S. J. Sciutto, Fermilab 1999; La Plata 2001.


     Arguments:
     =========

     tskname......... (output, pointer to a string) Task name (maximum
                      length is 64 characters).
     tsknamelen...... (output, pointer to int) Length of task name.
     tskversion...... (output, pointer to int) Task version.
     startdate....... (output, pointer to a string) Task starting date,
                      in the format "dd/mm/yyyy hh:mm:ss".

*/

/* 
   Declaration of arguments.
*/
  char   *tskname, *startdate;
  int    *tsknamelen, *tskversion;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    itskname[65], istartdate[21];
  void   iarray2str();

#if FUNDERSCORE == _
     void  fccrotaskid_();
#else
     void  fccrotaskid();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /*  Calling the internal fortran routine, returning integers.    */

#if FUNDERSCORE == _
         fccrotaskid_
#else
         fccrotaskid
#endif
                   (&itskname[0], tsknamelen, tskversion,
                    &istartdate[0]);

  /* Converting arrays into strings. */

  iarray2str(&itskname[0], tskname);
  iarray2str(&istartdate[0], startdate);

}
/* End of crotaskidc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

int idlcheckc(idldir)
/*
     Checking a string to see if it matches any of the IDL
     instructions currently defined, that is, the ones corresponding
     to the last opened compressed file.

     Written by: S. J. Sciutto, La Plata 1998, 2001, 2003.


     Arguments:
     =========

     idldir.......... (input, pointer to a string) The IDL directive.
                      Abbreviations are accepted accordingly with the
                      usual abbreviation rules.

     Return value: (integer) If an error occurs, then the returned
     ============  value will be negative. Other return values are the
                   following:
                   0 The string does not match any of the currently
                     valid IDL instructions.
                   1 The string matches a directive belonging to the
                     "basic" instruction set with no parameter(s)
                     associated with it, for example "Help".
                   2 The string matches a directive belonging to the
                     "basic" instruction set. If there is a parameter
                     associated with the directive, then it can be
                     obtained by means of routine "croinputdata0".
                   4 The directive corresponds to a real input
                     parameter. The parameter can be retrieved by
                     means of function "getinpreal".
                   6 The directive corresponds to an integer input
                     parameter. The parameter can be retrieved by
                     means of function "getinpint".
                   8 The directive corresponds to a logical input
                     parameter. The parameter can be retrieved by
                     means of function "getinpswitch".
*/

/* 
   Declaration of arguments.
*/
  char   *idldir;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    iidldirx[24], *iidldir;
  void   str2iarray();

#if FUNDERSCORE == _
     int  fcidlcheck_();
#else
     int  fcidlcheck();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the directive name string to array. */

  iidldir      = &iidldirx[0];
  iidldirx[23] = IARREND;

  str2iarray(idldir, iidldir);

  /*
     Calling the index assignment routine.
  */
  return(
#if FUNDERSCORE == _
         fcidlcheck_
#else
         fcidlcheck
#endif
                   (iidldir));

}
/* End of idlcheckc. */

/* End of file cioh1infoc.c           */
/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
