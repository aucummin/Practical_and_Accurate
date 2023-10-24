/*
   FILE: ccdaticpu.c                        Creation date: 18/JUN/1996.
                                        LAST MODIFICATION: 22/MAR/2005.

   C date, time and cputime routines.
   Work also under LINUX OS, 64 bit processors.

*/
#include <stdio.h>
#include <time.h>
#include <sys/resource.h>
#include <unistd.h>

int ccidati___(int *idati)
/* Current date and time as integer numbers.             */
/* Written by S. J. Sciutto, La Plata 1996, 2001, 2004.  */
{
  time_t tsecs, t1;
  struct tm     *timedat;
 
  t1      = time(&tsecs);
  timedat = localtime(&tsecs);

  *idati     = (*timedat).tm_year + 1900;
  *(idati+1) = (*timedat).tm_mon + 1;
  *(idati+2) = (*timedat).tm_mday;
  *(idati+3) = (*timedat).tm_hour;
  *(idati+4) = (*timedat).tm_min;
  *(idati+5) = (*timedat).tm_sec;

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
