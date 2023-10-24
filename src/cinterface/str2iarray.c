/*
   FILE: str2iarray.c                       Creation date: 28/MAY/1997.
                                        LAST MODIFICATION: 02/JUL/2003.

   Character string management routine for C-FORTRAN interface.

*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/
/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

#include "str2iarray.h"

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void str2iarray(instr, icharray)
/*
   Converting a string into an integer array containing the
   respective ascii codes and ending with 0.

   This conversion eases C-FORTRAN interface when there are
   FORTRAN string arguments.

   Written by S. J. Sciutto, La Plata 1997, 2003.


   Arguments:
   =========

   instr........... (input, pointer to string) String to be converted.
   icharray........ (output, pointer to int array) Array of ASCII
                    codes, ending with 0.

*/

/*
   Declarations of arguments.
*/
  char    *instr;
  int     *icharray;

/*
  ROUTINE'S BODY.
*/
{
  while((*instr != '\0') && (*icharray != IARREND))
  {
    *icharray = *instr;
    ++icharray; ++instr;
  }
  *icharray = 0;
}
/* End of str2iarray. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

void iarray2str(icharray, outstr)
/*
   Converting a string into an integer array containing the
   respective ascii codes and ending with 0.

   This conversion eases C-FORTRAN interface when there are
   FORTRAN string arguments.

   Written by S. J. Sciutto, Fermilab 1999.


   Arguments:
   =========

   icharray........ (input, pointer to int array) Array of ASCII
                    codes, ending with 0.
   outstr.......... (output, pointer to string) Converted string.

*/

/*
   Declarations of arguments.
*/
  int     *icharray;
  char    *outstr;

/*
  ROUTINE'S BODY.
*/
{
  while(*icharray != 0)
  {
    *outstr = *icharray;
    ++outstr; ++icharray;
  }
  *outstr = '\0';
}
/* End of str2iarray. */

/*---**---**---**---**---**---**---***---**---**---**---**---**---**---*/

/* End of file str2iarray.c  */



/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
