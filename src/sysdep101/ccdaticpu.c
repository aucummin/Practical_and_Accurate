/*
   FILE: ccdaticpu.c                        Creation date: 18/JUN/1996.
                                        LAST MODIFICATION: 28/OCT/1998.

   C date, time and cputime routines. Work under LINUX OS (DEC ALPHA version).

*/
#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

int ccidati___(yr, mo, da, hr, mi, sc)
  long          *yr, *mo, *da, *hr, *mi, *sc;

/* Current date and time as integer numbers. */
/* This routine works around a "bug" in the FORTRAN/C GNU compilers
   in their ALPHA-Linux version.             */

/* Written by S. J. Sciutto, La Plata 1998.  */
{
  time_t        tsecs, t1;
  struct tm     *timedat;
 
  t1      = time(&tsecs);
  timedat = localtime(&tsecs);

  *yr = (*timedat).tm_year + 1900;
  *mo = (*timedat).tm_mon + 1;
  *da = (*timedat).tm_mday;
  *hr = (*timedat).tm_hour;
  *mi = (*timedat).tm_min;
  *sc = (*timedat).tm_sec;

  return(0);
}

double cpusec_()
/* User cpu time in seconds. */
/* Written by S. J. Sciutto, La Plata 1996, 1997. */
{
  int           rc;
  struct rusage tbuf;

  rc = getrusage(RUSAGE_SELF, &tbuf);
  return(tbuf.ru_utime.tv_sec + tbuf.ru_stime.tv_sec +
         1.0e-6 * (tbuf.ru_utime.tv_usec + tbuf.ru_stime.tv_usec));
}

/* End of file ccdaticpu.c             */
/* This source file is part of AIRES 2.8.4a distribution. */

/* Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006  */
