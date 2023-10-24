/*
   FILE: speilibc.c                         Creation date: 01/NOV/1999.
                                        LAST MODIFICATION: 02/MAR/2001.

   C interface to the auxiliary routines for "special primary
   particle" processing.

*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

#include "fexten.h"

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void sprimnamec(pname, pnamelen)
/*
   Getting the name of the special primary particle specified in the
   corresponding IDL instruction. Special version called from C
   routine "sprimnamec".

   Written by: S. J. Sciutto, La Plata 1999, 2001.


   Arguments:
   =========

   pname........... (output, pointer to a string) Particle name. The
                    calling program must provide enough space to
                    store this string.
   pnamelen........ (output, pointer to int) Lenght of the particle name
                    string.

*/
/*
   Declaration of arguments.
*/
  char   *pname;
  int    *pnamelen;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    ipname[17];
  void   iarray2str();

#if FUNDERSCORE == _
     void fcsprimname_();
#else
     void fcsprimname();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /*
     Calling the string assignment routine.
  */
#if FUNDERSCORE == _
         fcsprimname_
#else
         fcsprimname
#endif
                        (&ipname[0], pnamelen);

  /* Converting array(s) into string(s). */

  iarray2str(&ipname[0], pname);

}
/* End of sprimnamec. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void speitaskc(taskn, tasklen, tver)
/*
   Getting the task name and version corresponding to the current
   shower. Special version called from C routine speitaskc

   Written by: S. J. Sciutto, La Plata 1999, 2001.


   Arguments:
   =========

   taskn........... (output, pointer to a string) Task name. The
                    calling program must provide enough space to
                    store this string.
   tasklen......... (output, integer) Lenght of the task name.
   tver............ (output, integer) Task name version.

*/
/*
   Declaration of arguments.
*/
  char   *taskn;
  int    *tasklen, *tver;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    itaskn[65];
  void   iarray2str();

#if FUNDERSCORE == _
     void fcspeitask_();
#else
     void fcspeitask();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /*
     Calling the string assignment routine.
  */
#if FUNDERSCORE == _
         fcspeitask_
#else
         fcspeitask
#endif
                        (&itaskn[0], tasklen, tver);

  /* Converting array(s) into string(s). */

  iarray2str(&itaskn[0], taskn);

}
/* End of speitaskc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void speigetmodnamec(mn, mnlen, mnfull, mnfullen)
/*
   Getting the name of the module invoked by the simulation
   program. Special version called from C routine speigetmodnamec.
c
   Written by: S. J. Sciutto, La Plata 1999, 2001.
c
c
   Arguments:
   =========
c
   mn.............. (output, pointer to a string) Name of external
                    module. The calling program must provide enough
                    space to store this array.
   mnlen........... (output, pointer to int) Lenght of string "mn".
   mnfull.......... (output, pointer to a string) Full name of
                    external module (Will be different of "imn" if
                    the module was placed within one of the
                    InputPath directories). The calling program
                    must provide enough space to store this array.
   mnfullen........ (output, pointer to int) Lenght of string "mnfull".

*/
/*
   Declaration of arguments.
*/
  char   *mn, mnfull;
  int    *mnlen, *mnfullen;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    imn[177], imnfull[513];
  void   iarray2str();

#if FUNDERSCORE == _
     void fcspeigetmodname_();
#else
     void fcspeigetmodname();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /*
     Calling the string assignment routine.
  */
#if FUNDERSCORE == _
         fcspeigetmodname_
#else
         fcspeigetmodname
#endif
                        (&imn[0], mnlen, &imnfull[0], mnfullen);

  /* Converting array(s) into string(s). */

  iarray2str(&imn[0], mn);
  iarray2str(&imnfull[0], mnfull);

}
/* End of speigetmodname. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void speigetparsc(parstring, pstrlen)
/*
   Getting the parameter string specified in the corresponding
   IDL instruction. Special version called from C routine
   speigetparsc.

   Written by: S. J. Sciutto, La Plata 1999.


   Arguments:
   =========

   parstring....... (output, pointer to a string) Parameter string.
                    The calling program must provide enough space to
                    store this string.
   pstrlen......... (output, pointger to int) Lenght of the parameter
                    string. Zero if there are no parameters.

*/
/*
   Declaration of arguments.
*/
  char   *parstring;
  int    *pstrlen;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    iparstring[177];
  void   iarray2str();

#if FUNDERSCORE == _
     void fcspeigetpars_();
#else
     void fcspeigetpars();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /*
     Calling the string assignment routine.
  */
#if FUNDERSCORE == _
         fcspeigetpars_
#else
         fcspeigetpars
#endif
                        (&iparstring[0], pstrlen);

  /* Converting array(s) into string(s). */

  iarray2str(&iparstring[0], parstring);

}
/* End of speigetparsc. */

/* End of file speilibc.c             */
/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
