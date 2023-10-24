c
c     FILE: sysutils.f                      Creation date: 25/NOV/1996.
c                                       LAST MODIFICATION: 18/JUN/2003.
c
c     Some standard utility routines.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine dati(datistr)
c
c     Current date and time in the format:
c
c            dd/Mmm/yyyy hh:mm:ss
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c     Arguments:
c     =========
c
c     datistr......... (output, character(*)) The string containing
c                      the current date and time.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     datistr
c
c     Declaration of internal variables and arrays.
c
      integer           idati(6)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Calling the numerical date and time routine.
c     This routine may contain non standard code. It returns date
c     and time as an integer array with six elements: year, month,
c     day, hour, minutes and seconds.
c
      call intdati(idati)
c
c     The numerical date and time information is formatted in a
c     string of the form: "dd/Mmm/yyyy hh:mm:ss"
c
      call datifmt(idati, datistr)
c
      return
      end
c     --- End of routine dati.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine datifmt(idati, datistr)
c
c     Converting numerical date and time into a string of the form:
c
c            dd/Mmm/yyyy hh:mm:ss
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c     Arguments:
c     =========
c
c     idati........... (input, integer, array(6)) Array containing
c                      the following data:
c                         idati(1) = year (yyyy).
c                         idati(2) = month (1 to 12)
c                         idati(3) = day (1 to 31)
c                         idati(4) = hour (1 to 24)
c                         idati(5) = minutes (0 to 59)
c                         idati(6) = seconds (0 to 59)
c     datistr......... (output, character(*)) The string containing
c                      the current date and time.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           idati(6)
      character*(*)     datistr
c
c     Declaration of internal variables and arrays.
c
      character*3       month(12)
c
      data              month / 'Jan', 'Feb', 'Mar', 'Apr',
     +                          'May', 'Jun', 'Jul', 'Aug',
     +                          'Sep', 'Oct', 'Nov', 'Dec'  /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing the date and time string.
c
      write(datistr, 2010)
     +      idati(3), '/', month(idati(2)), '/', idati(1),
     +      ' ', idati(4), ':', idati(5), ':', idati(6)
c
 2010 format(i2.2, 3a, i4.4, 3(a, i2.2))
c
      return
      end
c     --- End of routine datifmt.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function cvtfltdate(fdate, idate)
c
c     Extracting year, month, and day from a floating point date in the
c     format used to record event dates.
c     If the date is positive, the mantissa gives the fraction of the
c     year. Otherwise, its absolute value is interpreted in the format
c          
c                 |fdate| = day + 32 * month + 512 * year
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c     Arguments:
c     =========
c
c     fdate........... (input, double precision) Floating point date.
c     idate........... (output, integer, array(3)) Array containing
c                      the following data:
c                         idate(1) = year (yyyy).
c                         idate(2) = month (1 to 12)
c                         idate(3) = day (1 to 31)
c
c     Return value: (double precision) The date in true floating
c     ============  point format (year.fraction_of_year).
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  cvtfltdate
      double precision  fdate
      integer           idate(3)
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      integer           ydays, yday, adays
      double precision  ffdate
c
      integer           monthd(12)
c
      data              monthd / 31, 28, 31, 30, 31, 30,
     +                           31, 31, 30, 31, 30, 31  /
c
c     FIRST EXECUTABLE STATEMENT
c
      monthd(2) = 28
      ydays     = 365
c
      if (fdate .ge. 0) then
c
c       Date is given as a floating point number.
c
        idate(1) = fdate
c
        if ((idate(1) .ne. 1900) .and. (mod(idate(1), 4) .eq. 0)) then
          ydays     = 366
          monthd(2) = 29
        endif
c
        yday = (fdate - idate(1)) * ydays
        yday = yday + 1
        adays = 0
        do i = 1, 12
          j = i
          adays = adays + monthd(i)
          if (yday .le. adays) goto 1010
        enddo
 1010   continue
        idate(2) = j
        idate(3) = yday - adays + monthd(j)
c
        cvtfltdate = fdate
c
      else
c
c       Date given in format: day + 32 * month + 512 * year
c
        ffdate   = abs(fdate)
        idate(1) = ffdate / 512
        ffdate   = ffdate - idate(1) * 512
        idate(2) = ffdate / 32
        idate(3) = ffdate - idate(2) * 32
c
        if ((idate(1) .ne. 1900) .and. (mod(idate(1), 4) .eq. 0)) then
          ydays = 366
          monthd(2) = 29
        endif
c
        ffdate = 0
        do i = 1, idate(2) - 1
          ffdate = ffdate + monthd(i)
        enddo
        ffdate = ffdate + idate(3)
c
        cvtfltdate = idate(1) + (ffdate - 0.5d0) / ydays
c
      endif
c
      return
      end
c     --- End of routine cvtfltdate.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine rmfile(unit, fname)
c
c     Deleting a file (The file is opened and closed with the 'DELETE'
c     specification).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     unit............ (input, integer) The logical unit to use.
c     fname........... (input, character*(*)) The file name.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           unit
      character*(*)     fname
c
c     FIRST EXECUTABLE STATEMENT
c
c     Opening and closing the file to delete it.
c
      close (unit)
      open  (unit, file = fname, status = 'OLD', err = 1010)
      close (unit, status = 'DELETE')
      return
 1010 continue
      close (unit)
      return
c
      end
c     --- End of routine rmfile
c
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine lookforfile(fname, pathstring, pslen, pathsep,
     +                       exists, fullname, fullnamelen)
c
c     Checking for the existence of a given file. The file is searched
c     in the patstring and current directories.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2003.
c
c
c     Arguments:
c     =========
c
c     fname........... (input, character*(*)) The file name.
c     pathstring...... (input, character*(*)) Set of directories to
c                      search for the existence of the file.
c                      Directories are separated by character
c                      "pathsep".
c     pslen........... (input, integer) Length of string "pathstring".
c     pathsep......... (input, character*1) Separator character.
c     exists.......... (output, logical) True if the file is found.
c     fullname........ (output, character*(*)) Full name
c                      (directory + file name) of the file. This
c                      variable is set only when "exists" is true.
c     fullnamelen..... (output, integer) Length of "fullname". This
c                      variable is set only when "exists" is true.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     fname, pathstring, fullname
      integer           pslen, fullnamelen
      character*1       pathsep
      logical           exists
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
c     Searching the file in the input path
c
      j = 1
      do i = 1, pslen
        if (pathstring(i:i) .eq. pathsep) then
          fullname = pathstring(j:i-1) // fname
          inquire(file = fullname, exist = exists)
          if (exists) goto 1010
          j = i + 1
        endif
      enddo
c
c     No file(s) found in the path. Trying the current working
c     directory.
c
      fullname = fname 
      inquire(file = fullname, exist = exists)
      if (exists) goto 1010
c
c     The file does not exist.
c
      fullname    = ' '
      fullnamelen = 0
      return
c
 1010 continue
c
c     The file exists.
c
      call strim(-1, fullname, fullnamelen)
      return
c
      end
c     --- End of routine lookforfile
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'sysutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
