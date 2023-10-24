/*
   FILE: initutilsc.c                       Creation date: 20/MAR/1998.
                                        LAST MODIFICATION: 15/NOV/2004.

   C interface to the auxiliary routines for input parameter processing.
   This file covers all routines corresponding to AIRES source files
   initutils.f and initutils2.f

*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

#include "fexten.h"
#include "str2iarray.h"

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

double getinprealc(dirname)
/*
   Getting the current value for an already defined real input
   parameter. C version.

   Written by: S. J. Sciutto, La Plata 1998, 2001, 2003, 2006.
 

   Arguments:
   =========

   dirname......... (input, pointer to a string) Name of the parameter.

   Return value: (double) The current parameter value.
   ============
*/

/* 
   Declaration of arguments.
*/
  char   *dirname;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    idirnamex[20] = {0}, *idirname;
  void   str2iarray();

#if FUNDERSCORE == _
     double fcgetinpreal_();
#else
     double fcgetinpreal();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the directive name string to array. */

  idirname      = &idirnamex[0];
  idirnamex[19] = IARREND;

  str2iarray(dirname, idirname);

  /*
     Calling the index assignment routine.
  */
  return(
#if FUNDERSCORE == _
         fcgetinpreal_(idirname)
#else
         fcgetinpreal(idirname)
#endif
        );

}
/* End of getinprealc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

int getinpintc(dirname)
/*
   Getting the current value for an already defined integer input
   parameter. C version.

   Written by: S. J. Sciutto, La Plata 1998, 2001, 2003, 2006.
 

   Arguments:
   =========

   dirname......... (input, pointer to a string) Name of the parameter.

   Return value: (integer) The current parameter value.
   ============
*/

/* 
   Declaration of arguments.
*/
  char   *dirname;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    idirnamex[20] = {0}, *idirname;
  void   str2iarray();

#if FUNDERSCORE == _
     int fcgetinpint_();
#else
     int fcgetinpint();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the directive name string to array. */

  idirname      = &idirnamex[0];
  idirnamex[19] = IARREND;

  str2iarray(dirname, idirname);

  /*
     Calling the index assignment routine.
  */
  return(
#if FUNDERSCORE == _
         fcgetinpint_(idirname)
#else
         fcgetinpint(idirname)
#endif
        );

}
/* End of getinpintc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

int getinpswitchc(dirname)
/*
   Getting the current value for an already defined logical input
   parameter. C version.

   Written by: S. J. Sciutto, La Plata 1998, 2001, 2003, 2006.
 

   Arguments:
   =========

   dirname......... (input, pointer to a string) Name of the parameter.

   Return value: (integer) The current parameter value. Note that
   ============  FORTRAN logical data is treated as integer in C.
*/

/* 
   Declaration of arguments.
*/
  char   *dirname;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    idirnamex[20] = {0}, *idirname;
  void   str2iarray();

#if FUNDERSCORE == _
     int fcgetinpswitch_();
#else
     int fcgetinpswitch();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the directive name string to array. */

  idirname      = &idirnamex[0];
  idirnamex[19] = IARREND;

  str2iarray(dirname, idirname);

  /*
     Calling the index assignment routine.
  */
  return(
#if FUNDERSCORE == _
         fcgetinpswitch_(idirname)
#else
         fcgetinpswitch(idirname)
#endif
        );

}
/* End of getinpswitchc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void getinpstringc(dirname, string, slen)
/*
   Getting the current value for an already defined string input
   parameter. C version.

   Written by: S. J. Sciutto, Fermilab 1999; La Plata 2001, 2003, 2006.
 

   Arguments:
   =========

   dirname......... (input, pointer to a string) Name of the parameter.
   string.......... (output, pointer to a sring) The current parameter
                    value. The calling routine must provide enough
                    space to store it.
   slen............ (output, pointer to int) Length of "string".

*/

/* 
   Declaration of arguments.
*/
  char   *dirname, *string;
  int    *slen;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    idirnamex[20] = {0}, *idirname;
  int    istring[256];
  void   str2iarray(), iarray2str();

#if FUNDERSCORE == _
     void fcgetinpstring_();
#else
     void fcgetinpstring();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the directive name string to array. */

  idirname      = &idirnamex[0];
  idirnamex[19] = IARREND;

  str2iarray(dirname, idirname);

  /*
     Calling the string assignment routine.
  */
#if FUNDERSCORE == _
         fcgetinpstring_
#else
         fcgetinpstring
#endif
                        (idirname, &istring[0], slen);

  /* Converting arrays into strings. */

  iarray2str(&istring[0], string);

}
/* End of getinpstringc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void getglobalc(gvname, sdynsw, gvval, valen)
/*
   Getting the current value of a global variable. C version.

   Written by: S. J. Sciutto, La Plata 2001, 2003, 2006.
 

   Arguments:
   =========

   gvname.......... (input, pointer to a string) Global variable name.
   sdynsw.......... (output, pointer to int) Type of variable: 1 dynamic,
                    2 static, 0 if the variable is undefined.
   gvval........... (output, pointer to a string) The string currently
                    assigned to the variable. The calling program
                    must ensure enough space to store the string.
   valen........... (output, pointer to int) Length of gvval. valen is
                    negative for undefined variables.

*/

/* 
   Declaration of arguments.
*/
  char   *gvname, *gvval;
  int    *sdynsw, *valen;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    igvnamex[20] = {0}, *igvname;
  int    igvval[512];
  void   str2iarray(), iarray2str();

#if FUNDERSCORE == _
     void fcgetglobal_();
#else
     void fcgetglobal();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the variable's name string to array. */

  igvname      = &igvnamex[0];
  igvnamex[19] = IARREND;

  str2iarray(gvname, igvname);

  /*
     Calling the string assignment routine.
  */
#if FUNDERSCORE == _
         fcgetglobal_
#else
         fcgetglobal
#endif
                        (igvname, sdynsw, &igvval[0], valen);

  /* Converting arrays into strings. */

  iarray2str(&igvval[0], gvval);

}
/* End of getglobalc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

int getintfromglobalc(gvname, defval, irc)
/*
   Interpreting a global variable as an integer number, and
   retrieving it. C version.

   Written by: S. J. Sciutto, La Plata 2001, 2003, 2006.
 

   Arguments:
   =========

   gvname.......... (input, pointer to a string) Global variable name.
   defval.......... (input, pointer to int) Default value to use if the
                    global variable is undefined, or not convertible
                    to an integer number.
   irc............. (output, pointer to int) Return code. Zero means that
                    the return value was successfully read in from
                    the global variable. irc = 1 means that the
                    global variable is not defined. irc = 3 means
                    that the string associated with the global
                    variable cannot be converted to integer.

*/

/* 
   Declaration of arguments.
*/
  char   *gvname;
  int    *defval;
  int    *irc;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    igvnamex[20] = {0}, *igvname;
  void   str2iarray();

#if FUNDERSCORE == _
     int fcgetintfromglobal_();
#else
     int fcgetintfromglobal();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the variable's name string to array. */

  igvname      = &igvnamex[0];
  igvnamex[19] = IARREND;

  str2iarray(gvname, igvname);

  /*
     Calling the number assignment routine.
  */

  return(
#if FUNDERSCORE == _
           fcgetintfromglobal_
#else
           fcgetintfromglobal
#endif
                               (igvname, defval, irc)
         );

}
/* End of getintfromglobalc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

double getrealfromglobalc(gvname, defval, irc)
/*
   Interpreting a global variable as a floating point number, and
   retrieving it. C version.

   Written by: S. J. Sciutto, La Plata 2001, 2003, 2006.
 

   Arguments:
   =========

   gvname.......... (input, pointer to a string) Global variable name.
   defval.......... (input, pointer to double) Default value to use if the
                    global variable is undefined, or not convertible
                    to an integer number.
   irc............. (output, pointer to int) Return code. Zero means that
                    the return value was successfully read in from
                    the global variable. irc = 1 means that the
                    global variable is not defined. irc = 3 means
                    that the string associated with the global
                    variable cannot be converted to integer.

*/

/* 
   Declaration of arguments.
*/
  char   *gvname;
  double *defval;
  int    *irc;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    igvnamex[20] = {0}, *igvname;
  void   str2iarray();

#if FUNDERSCORE == _
     double fcgetrealfromglobal_();
#else
     double fcgetrealfromglobal();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the variable's name string to array. */

  igvname      = &igvnamex[0];
  igvnamex[19] = IARREND;

  str2iarray(gvname, igvname);

  /*
     Calling the number assignment routine.
  */

  return(
#if FUNDERSCORE == _
           fcgetrealfromglobal_
#else
           fcgetrealfromglobal
#endif
                                (igvname, defval, irc)
         );

}
/* End of getrealfromglobalc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void setglobalc(gvname, vval, irc)
/*

   Setting a global variable. This routine is primarily designed to be
   used within a special particle module. C version.

   Written by: S. J. Sciutto, La Plata 2003, 2006.
 

   Arguments:
   =========

   gvname.......... (input, pointer to a string) Global variable name.
   vval.... ....... (input, pointer to a string) Value to set.
   irc............. (output, pointer to int) Return code. Zero means that
                    the variable has been set successfully.

*/

/* 
   Declaration of arguments.
*/
  char   *gvname;
  char   *vval;
  int    *irc;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    igvnamex[20] = {0}, *igvname;
  int    ivvalx[256] = {0}, *ivval;
  void   str2iarray();

#if FUNDERSCORE == _
     void fcsetglobal_();
#else
     void fcsetglobal();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the variable's name and value strings to array. */

  igvname      = &igvnamex[0];
  igvnamex[19] = IARREND;
  ivval        = &ivvalx[0];
  ivvalx[255]  = IARREND;

  str2iarray(gvname, igvname);
  str2iarray(vval, ivval);

  /*
     Calling the variable setting routine.
  */

#if FUNDERSCORE == _
           fcsetglobal_
#else
           fcsetglobal
#endif
                        (igvname, ivval, irc);

}
/* End of setglobalc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void setintglobalc(gvname, vval, irc)
/*

   Setting an integer value as a dynamic global variable. This
   routine is primarily designed to be used within a special
   particle module. C version.

   Written by: S. J. Sciutto, La Plata 2003, 2006.
 

   Arguments:
   =========

   gvname.......... (input, pointer to a string) Global variable name.
   vval.... ....... (input, pointer to int) Integer value to set.
   irc............. (output, pointer to int) Return code. Zero means that
                    the variable has been set successfully.

*/

/* 
   Declaration of arguments.
*/
  char   *gvname;
  int    *vval;
  int    *irc;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    igvnamex[20] = {0}, *igvname;
  void   str2iarray();

#if FUNDERSCORE == _
     void fcsetintglobal_();
#else
     void fcsetintglobal();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the variable's name string to array. */

  igvname      = &igvnamex[0];
  igvnamex[19] = IARREND;

  str2iarray(gvname, igvname);

  /*
     Calling the variable setting routine.
  */

#if FUNDERSCORE == _
           fcsetintglobal_
#else
           fcsetintglobal
#endif
                           (igvname, vval, irc);

}
/* End of setintglobalc. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void setrealglobalc(gvname, vval, irc)
/*

   Setting a real value as a dynamic global variable. This
   routine is primarily designed to be used within a special
   particle module. C version.

   Written by: S. J. Sciutto, La Plata 2003, 2006.
 

   Arguments:
   =========

   gvname.......... (input, pointer to a string) Global variable name.
   vval.... ....... (input, pointer to double) Real value to set.
   irc............. (output, pointer to int) Return code. Zero means that
                    the variable has been set successfully.

*/

/* 
   Declaration of arguments.
*/
  char   *gvname;
  double *vval;
  int    *irc;
/* 
  ROUTINE'S BODY.
*/
{
  /*
     Declaration of internal variables and arrays.
  */
  int    igvnamex[20] = {0}, *igvname;
  void   str2iarray();

#if FUNDERSCORE == _
     void fcsetrealglobal_();
#else
     void fcsetrealglobal();
#endif

  /*
     EXECUTABLE STATEMENTS.
  */

  /* Converting the variable's name string to array. */

  igvname      = &igvnamex[0];
  igvnamex[19] = IARREND;

  str2iarray(gvname, igvname);

  /*
     Calling the variable setting routine.
  */

#if FUNDERSCORE == _
           fcsetrealglobal_
#else
           fcsetrealglobal
#endif
                            (igvname, vval, irc);

}
/* End of setrealglobalc. */

/* End of file initutilsc.c           */
/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
