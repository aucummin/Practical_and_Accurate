c
c     FILE: conversion.f                    Creation date: 25/JUN/1996.
c                                       LAST MODIFICATION: 27/AUG/2003.
c
c     Unit conversion and management.

c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setunits
c
c     Initializing the unit conversion system, and related variables.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Length units.
c
      luname(1) = 'cm'
      luname(2) = 'm'
      luname(3) = 'km'
      luname(4) = 'in'
      luname(5) = 'ft'
      luname(6) = 'yd'
      luname(7) = 'mi'
c
      lconvtomt(0) = 1.00d00
      lconvtomt(1) = 0.01d00
      lconvtomt(2) = 1.00d00
      lconvtomt(3) = 1.00d03
      lconvtomt(4) = 2.54d-2
      lconvtomt(5) =   12 * lconvtomt(4)
      lconvtomt(6) =    3 * lconvtomt(5)
      lconvtomt(7) = 5280 * lconvtomt(5)
c
      do i = 0, nlunits
        lconvfrommt(i) = 1.d0 / lconvtomt(i)
      enddo
c
      lnicedefunit = 2
      do i = 0, nlunits0
        lnicemax(i) = 10.d0 ** (3 * i - 3)
      enddo
c
c     Time units.
c
      tuname(1) = 'ns'
      tuname(2) = 'sec'
      tuname(3) = 'min'
      tuname(4) = 'hr'
c
      tconvtosec(0) = 1.00d00
      tconvtosec(1) = 1.00d-9
      tconvtosec(2) = 1.00d00
      tconvtosec(3) = 60.0d00
      tconvtosec(4) = 3.60d03
c
      do i = 0, ntunits
        tconvtons(i)    = 1.0d9 * tconvtosec(i)
        tconvfromsec(i) = 1.0d0 / tconvtosec(i)
        tconvfromns(i)  = 1.0d0 / tconvtons(i)
      enddo
c
c     Energy units.
c
      euname( 1) = 'eV'
      euname( 2) = 'keV'
      euname( 3) = 'MeV'
      euname( 4) = 'GeV'
      euname( 5) = 'TeV'
      euname( 6) = 'PeV'
      euname( 7) = 'EeV'
      euname( 8) = 'ZeV'
      euname( 9) = 'YeV'
      euname(10) = 'KeV'
      euname(11) = 'J'
c
      econvtogev( 0) = 1.0d+00
      econvtogev( 1) = 1.0d-09
      econvtogev( 2) = 1.0d-06
      econvtogev( 3) = 1.0d-03
      econvtogev( 4) = 1.0d+00
      econvtogev( 5) = 1.0d+03
      econvtogev( 6) = 1.0d+06
      econvtogev( 7) = 1.0d+09
      econvtogev( 8) = 1.0d+12
      econvtogev( 9) = 1.0d+15
      econvtogev(10) = 1.0d-06
      econvtogev(11) = 1.0d-09 / protoncharge
c
      do i = 0, neunits
        econvfromgev(i) = 1.d0 / econvtogev(i)
      enddo
c
      enicedefunit = 1
      enicemax(0)  = 1.d-10
      do i = 1, neunits0
        enicemax(i) = 10.d0 ** (3 * i - 9)
      enddo
c
c     Angle units.
c
      auname(1) = 'deg'
      auname(2) = 'rad'
c
      aconvtodeg(0) = 1.00d00
      aconvtodeg(1) = 1.00d00
      aconvtodeg(2) = 180 / pi
c
      do i = 0, naunits
        aconvfromdeg(i) = 1.d0 / aconvtodeg(i)
      enddo
c
c     Magnetic B field units.
c
      buname(1) = 'nT'
      buname(2) = 'uT'
      buname(3) = 'mT'
      buname(4) = 'T'
      buname(5) = 'uG'
      buname(6) = 'mG'
      buname(7) = 'Gs'
      buname(8) = 'gm'
c
      bconvtont(0) = 1.0d+00
      bconvtont(1) = 1.0d+00
      bconvtont(2) = 1.0d+03
      bconvtont(3) = 1.0d+06
      bconvtont(4) = 1.0d+09
      bconvtont(5) = 1.0d-01
      bconvtont(6) = 1.0d+02
      bconvtont(7) = 1.0d+05
      bconvtont(8) = 1.0d+00
c
      do i = 0, nbunits
        bconvfromnt(i) = 1.d0 / bconvtont(i)
      enddo
c
      bnicedefunit = 4
      bnicemax(0)  = 0.1d0
      do i = 1, nbunits0
        bnicemax(i) = 10.d0 ** (3 * i)
      enddo
c
      return
c
      end
c     --- End of routine setunits
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine nplusunit(uconv, advance,
     +                     string, slen, wfirst, wlast,
     +                     naltunit, altunit, minalen,
     +                     defunit, default,
     +                     ovalue, uuniti, unitused, ulen, irc)
c
c     Extracting (optionally) the first word from a character string,
c     and reading it (and the subsequent word for the unit) as a
c     {number, unit} specification, where "unit" can be either a
c     unit compatible with the conversion routine "uconv" or the
c     alternative unit "altunit".
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     uconv........... (input, double precision, external) An external
c                      function to use to convert units. The functions
c                      l2meters, t2sec, e2gev, etc. in this
c                      package are suitable ones.
c     advance......... (input, logical) Switch to decide whether or
c                      not invoke routine "nextword" to extract the
c                      first word from "string".
c     string.......... (input, character*(*)) The string to scan
c     slen............ (input, integer) String length, or last
c                      position to scan.
c     wfirst.......... (input-output, integer) Position of the first
c                      character of the given or to be localized word.
c     wlast........... (input-output, integer) Position of the last
c                      character of the given or last position scanned.
c     naltunit........ (input, integer) The number of alternative units
c                      specified (may be zero).
c     altunit......... (input, character*(*), array(naltunit)) The
c                      alternative unit(s).
c     minalen......... (input, character*(*), array(naltunit))
c                      Mininum length for the alternative unit(s).
c     defunit......... (input, logical) When no unit is specified,
c                      defunit is used to convert the numerical value.
c                      If true, it is returned unchanged. Otherwise
c                      it is converted using the default unit of
c                      routine uconv.
c     default......... (input, double precision) Default value to be
c                      used when the return code is 4 or 5.
c     ovalue.......... (output, double precision) The value obtained,
c                      expressed in the corresponding unit (specified
c                      within the external function uconv).
c     uuniti.......... (integer, output) If the return code is 2, then
c                      this parameter returns the index of the
c                      alternative unit that was used. Otherwise zero
c                      is returned.
c     unitused........ (character*(*), output) The string used for
c                      the unit (even if it is invalid. Or blank if
c                      the unit is missing.
c     ulen............ (output, integer) Length of strin unitused.
c     irc............. (output, integer) Return code. 0 means that a
c                      valid length (number and unit) was specified
c                      and processed.
c                      1 means that a number without unit was
c                      specified. The default unit was used.
c                      2 means that one of the alternative units was
c                      explicitly specified.
c                      3 means that a valid number but an invalid
c                      unit were specified. The default unit was
c                      used instead.
c                      4 means that no number was specified.
c                      5 Invalid floating point number.
c
c
      implicit none
c
c     Declaration of arguments.
c
      external          uconv
      double precision  uconv
      logical           advance
      character*(*)     string
      integer           slen, wfirst, wlast
      integer           naltunit
      character*(*)     altunit(naltunit)
      integer           minalen(naltunit)
      logical           defunit
      double precision  default, ovalue
      integer           uuniti, ulen, irc
      character*(*)     unitused
c
c     Declaration of internal variables and arrays.
c
      integer           i, l, l0
      double precision  tmp1, tmp2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Getting the number
c
      call getnumber(advance, string, slen, wfirst, wlast,
     +               2, default, tmp1, irc)
c
      if (irc .eq. 0) then
c
c       Looking for the corresponding unit.
c
        call nextword(string, slen, wfirst, wlast)
c
        if (wfirst .le. wlast) then
c
          unitused = string(wfirst:wlast)
          ulen     = wlast - wfirst + 1
c
c         Checking if the unit is any of the alternative one(s)
c
          if (naltunit .gt. 0) then
c
            l0 = len(altunit(1))
            do i = 1, naltunit
              l = i
              if (string(wfirst:wlast) .eq.
     +            altunit(i)(1: min(l0, max(ulen, minalen(i)))))
     +           goto 1010
            enddo
          endif
c
c         No alternative specified. Using the conversion routine
c
          uuniti = 0
          tmp2   = uconv(tmp1, string(wfirst:wlast), irc)
c
          if (irc .le. 1) then
            ovalue = tmp2
          else
c
c           Invalid unit. Using the default unit.
c
            if (defunit) then
              ovalue   = tmp1
            else
              ovalue   = uconv(tmp1, ' ', irc)
            endif
            irc = 3
c
          endif
c
        else
c
c         No unit specified.
c
          unitused = ' '
          ulen     = 1
c
          if (defunit) then
            ovalue   = tmp1
            irc      = 1
          else
            ovalue   = uconv(tmp1, ' ', irc)
          endif
c
        endif
c
      else
c
c       Invalid or missing number. Returning the default.
c
        ovalue   = default
        unitused = ' '
        ulen     = 1
c
        if (irc .gt. 1) then
          irc = 5
        else
          irc = 4
        endif
c
      endif
      return
c
 1010 continue
c
c     An alternative unit or keyword was specified.
c
      ovalue = tmp1
      uuniti = l
      irc    = 2
      return
c
      end
c     --- End of routine nplusunit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function l2meters(length, unit, irc)
c
c     Converting length to meters.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     length.......... (input, double precision) The length to convert.
c     unit............ (input, character*(*)) The length unit.
c                      Default unit is "m".
c     irc............. (output, integer) Return code. 0 means that a
c                      valid unit was specified and processed.
c                      1 means that no unit was specified and
c                      the default value was used. 3 means that an
c                      invalid unit was specified, and the default
c                      one was used to convert the given value.
c
c     Return value: (double precision) The converted length in meters.
c     ============
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  l2meters
      double precision  length
      character*(*)     unit
      integer           irc
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  convunit
c
c     FIRST EXECUTABLE STATEMENT
c
      l2meters = convunit(length, unit,
     +                    nlunits, luname, lconvtomt, irc)
      return
c
      end
c     --- End of routine l2meters.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function t2sec(time, unit, irc)
c
c     Converting time to seconds.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     time............ (input, double precision) The time to convert.
c     unit............ (input, character*(*)) The time unit.
c                      Default unit is "sec".
c     irc............. (output, integer) Return code. 0 means that a
c                      valid unit was specified and processed.
c                      1 means that no unit was specified and
c                      the default value was used. 3 means that an
c                      invalid unit was specified, and the default
c                      one was used to convert the given value.
c
c     Return value: (double precision) The converted time in seconds.
c     ============
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  t2sec
      double precision  time
      character*(*)     unit
      integer           irc
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  convunit
c
c     FIRST EXECUTABLE STATEMENT
c
      t2sec = convunit(time, unit,
     +                 ntunits, tuname, tconvtosec, irc)
      return
c
      end
c     --- End of routine t2sec.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function e2gev(energy, unit, irc)
c
c     Converting energy to GeV (10**9 eV).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     energy.......... (input, double precision) The energy to convert.
c     unit............ (input, character*(*)) The energy unit.
c                      Default unit is "GeV".
c     irc............. (output, integer) Return code. 0 means that a
c                      valid unit was specified and processed.
c                      1 means that no unit was specified and
c                      the default value was used. 3 means that an
c                      invalid unit was specified, and the default
c                      one was used to convert the given value.
c
c     Return value: (double precision) The converted energy in GeV.
c     ============
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  e2gev
      double precision  energy
      character*(*)     unit
      integer           irc
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  convunit
c
c     FIRST EXECUTABLE STATEMENT
c
      e2gev = convunit(energy, unit, neunits, euname, econvtogev, irc)
c
      return
c
      end
c     --- End of routine e2gev.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function angle2deg(angle, unit, irc)
c
c     Converting angles to degrees.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     angle........... (input, double precision) The angle to convert.
c     unit............ (input, character*(*)) The length unit.
c                      Default unit is "deg".
c     irc............. (output, integer) Return code. 0 means that a
c                      valid unit was specified and processed.
c                      1 means that no unit was specified and
c                      the default value was used. 3 means that an
c                      invalid unit was specified, and the default
c                      one was used to convert the given value.
c
c     Return value: (double precision) The converted angle in degrees.
c     ============
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  angle2deg
      double precision  angle
      character*(*)     unit
      integer           irc
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  convunit
c
c     FIRST EXECUTABLE STATEMENT
c
      angle2deg = convunit(angle, unit,
     +                     naunits, auname, aconvtodeg, irc)
      return
c
      end
c     --- End of routine angle2deg.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function b2nt(bfield, unit, irc)
c
c     Converting magnetic B field to nanoteslas.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     bfield.......... (input, double precision) The field to convert.
c     unit............ (input, character*(*)) The B field unit.
c                      Default unit is "nanotesla".
c     irc............. (output, integer) Return code. 0 means that a
c                      valid unit was specified and processed.
c                      1 means that no unit was specified and
c                      the default value was used. 3 means that an
c                      invalid unit was specified, and the default
c                      one was used to convert the given value.
c
c     Return value: (double precision) The converted field in nt.
c     ============
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  b2nt
      double precision  bfield
      character*(*)     unit
      integer           irc
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  convunit
c
c     FIRST EXECUTABLE STATEMENT
c
      b2nt = convunit(bfield, unit, nbunits, buname, bconvtont, irc)
      return
c
      end
c     --- End of routine angle2deg.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function convunit(value, unit, nlist, unitlist, convfact, irc)
c
c     Converting "value", expressed in units "unit", to a given
c     unit.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     value........... (input, double precision) The value to convert.
c     unit............ (input, character*(*)) The unit corresponding
c                      to value.
c     nlist........... (input, integer) The number of available
c                      units.
c     unitlist........ (input, character*(*), array(nlist)) The names
c                      of the known units.
c     convfact........ (input, double precision, array(0:nlist)) The
c                      conversion factors. Element 0 corresponds to the
c                      default unit.
c     irc............. (output, integer) Return code. 0 means that a
c                      valid unit was specified and processed.
c                      1 means that no unit was specified and
c                      the default value was used. 3 means that an
c                      invalid unit was specified, and the default
c                      one was used to convert the given value.
c
c     Return value: (double precision) The converted value, expressed
c     ============  in the corresponding unit.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  convunit
      double precision  value
      character*(*)     unit
      integer           nlist
      character*(*)     unitlist(nlist)
      double precision  convfact(0:nlist)
      integer           irc
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u, uu
c
c     FIRST EXECUTABLE STATEMENT
c
      if (unit .eq. ' ') then
c
c       Missing unit. Assuming the default unit.
c
        irc      = 1
        convunit = value * convfact(0)
c
      else
c
c       Analysing the given unit.
c
        do u = 1, nlist
          uu = u
          if (unit .eq. unitlist(u)) goto 1010
        enddo
c
c       Invalid unit. Using the default.
c
        irc      = 3
        convunit = value * convfact(0)
        return
c
 1010   continue
c
c       A valid unit was specified.
c
        irc      = 0
        convunit = value * convfact(uu)
c
      endif
      return
c
      end
c     --- End of routine convunit.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine unice(number, nicedefunit, nnunits, nicemax, uname,
     +                 convfrom, nustring, nuslen)
c
c     Expressing a given quantity (length, energy, etc) (given in the
c     internally used unit) in convenient format for printing.
c     It is assumed that the available units scale at most as powers of
c     1000.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     number.......... (input, double precision) The number to process.
c     nicedefunit..... (input, integer) Label of the unit to use for
c                      very small or very large quantities.
c     nnunits......... (input, integer) Number of available units.
c     nicemax......... (input, double precision, array(0:nnunits) The
c                      ranges of use of the available units.
c     uname........... (input, character*(*), array(nnunits)) Names
c                      of available units.
c     convfrom........ (input, double precision, array(nnunits)
c                      Conversion factors.
c     nustring........ (output, character*(*)) A string containing
c                      the formatted number plus the unit used. The
c                      calling program must provide enough space for
c                      this string.
c     nuslen.......... (output, integer) Position of last nonblank
c                      character of "nustring".
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  number
      integer           nicedefunit, nnunits
      double precision  nicemax(0:nnunits)
      character*(*)     uname(nnunits)
      double precision  convfrom(0:nnunits)
      character*(*)     nustring
      integer           nuslen
c
c     Declaration of internal variables and arrays.
c
      integer           u, uu, j
      double precision  anumber, n0
c
c     FIRST EXECUTABLE STATEMENT
c
c     Selecting unit and format accordingly with magnitude.
c
      anumber = abs(number)
c
      if (anumber .eq. 0) then
c
        uu = nicedefunit
        write(nustring, 2005) '0.0000 ', uname(uu)
 2005   format(2a)
c
      else if ((anumber .lt. nicemax(0)) .or.
     +         (anumber .ge. nicemax(nnunits))) then
c
c       Value out of range. Using exponential notation in a
c       given default unit.
c
        uu = nicedefunit
        write(nustring, 2010) number * convfrom(uu), uname(uu)
 2010   format(1p, e11.4, 1x, a)
c
      else
c
c       Selecting appropiate unit.
c
        do u = 1, nnunits
          uu = u
          if (anumber .lt. nicemax(u)) goto 1010
        enddo
c
c       This point should never be reached.
c
 1010   continue
c
c       Writing the number. It is assumed that consecutive
c       units represent (at most) increments of magnitude 10^3.
c
        n0      = number * convfrom(uu)
        anumber = abs(n0)
c
        if (anumber .lt. 10.d0) then
          write(nustring, 2020) n0, uname(uu)
 2020     format(f7.4, 1x, a)
        else if (anumber .lt. 100.d0) then
          write(nustring, 2030) n0, uname(uu)
 2030     format(f7.3, 1x, a)
        else
          write(nustring, 2040) n0, uname(uu)
 2040     format(f7.2, 1x, a)
        endif
c
      endif
c
c     Eliminating leading decimal point and leading and trailing
c     blanks.
c
      nuslen = len(nustring)
      call snumtrim(nustring, nuslen)
c
c     Padding with blanks in the case of shorter unit names.
c
      j = len(uname(uu))
      if (uname(uu)(j:j) .eq. ' ') then
        nuslen = nuslen + 1
        nustring(nuslen:nuslen) = ' '
      endif
c
      return
      end
c     --- End of routine unice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine lnice(length, neginf, lstring, lslen)
c
c     Expressing a length (given in m) in convenient format for
c     printing.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     length.......... (input, double precision) The length to process
c                      (in m).
c     neginf.......... (input, integer) If positive, then the
c                      string returned by routine altstring(neginf,..)
c                      is returned for negative lengths. Otherwise
c                      the number is processed normally without regard
c                      to its sign.
c     lstring......... (output, character*(*)) A string containing
c                      the formatted length plus the unit used.
c                      Minimum length of this string: 9, maximum: 14
c     lslen........... (output, integer) Position of last nonblank
c                      character of "lstring".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  length
      integer           neginf
      character*(*)     lstring
      integer           lslen
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((length .lt. 0) .and. (neginf .gt. 0)) then
c
        call altstring(neginf, lstring, lslen)
c
      else
c
c       Selecting unit and format accordingly with magnitude.
c
        call unice(length, lnicedefunit, nlunits0, lnicemax, luname,
     +             lconvfrommt, lstring, lslen)
c
      endif
c
      return
      end
c     --- End of routine lnice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine tnice(tim, neginf, tstring, tslen)
c
c     Expressing a time (given in seconds) in the format:
c
c                hhhh hr mm min ss.ss sec
c     or
c                days.dd days
c
c     when the time is greater or equal than 1000 hours.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2003; Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     tim............. (input, double precision) The time to process
c                      (in sec).
c     neginf.......... (input, integer) If positive, then the
c                      string returned by routine altstring(neginf,..)
c                      is returned for negative times. Otherwise the
c                      number is processed normally without regard to
c                      its sign.
c     tstring......... (output, character*(*)) A string containing
c                      the formatted time. Maximum length is 30.
c     tslen........... (output, integer) Position of last nonblank
c                      character of "tstring".
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  tim
      integer           neginf
      character*(*)     tstring
      integer           tslen
c
c     Declaration of internal variables and arrays.
c
      logical           tisneg
      integer           hr, mn, sc, cc
      double precision  pt, fsc
      double precision  days, years
c
c     FIRST EXECUTABLE STATEMENT
c
      tisneg = (tim .lt. 0)
c
      if (tisneg .and. (neginf .gt. 0)) then
c
        call altstring(neginf, tstring, tslen)
c
      else
c
        pt  = abs(tim)
c
        if (pt .ge. 86400000.0d0) then
c
          years = tim / 31536000.0d0
          write(tstring, 2015) years, ' yr'
          tslen = 18
c
        else if (pt .ge. 3600000.0d0) then
c
          days = tim / 86400.0d0
          write(tstring, 2015) days, ' days'
          tslen = 20
c
        else if (pt .ge. 10.0d0) then
c
          pt  = pt + 0.005d0
          mn  = pt / 60
          fsc = pt - mn * 60
          hr  = mn / 60
          mn  = mn - hr * 60
          sc  = fsc
          cc  = 100 * (fsc - sc)
c
          if (hr .gt. 0) then
            if (tisneg) hr = -hr
            if ((sc .eq. 0) .and. (cc .eq. 0)) then
              if (mn .eq. 0) then
                write(tstring, 2010) hr, ' hr'
                tslen = 13
              else
                write(tstring, 2010) hr, ' hr ', mn, ' min'
                tslen = 20
              endif
            else
              write(tstring, 2010) hr, ' hr ', mn, ' min ',
     +                             sc, '.', cc, ' sec'
              tslen = 30
            endif
          else if (mn .gt. 0) then
            if (tim .lt. 0) mn = -mn
            if ((sc .eq. 0) .and. (cc .eq. 0)) then
              write(tstring, 2010) mn, ' min'
              tslen = 14
            else
              write(tstring, 2010) mn, ' min ',
     +                             sc, '.', cc, ' sec'
              tslen = 24
            endif
          else
            if (tim .lt. 0) sc = -sc
            write(tstring, 2010) sc, '.', cc, ' sec'
            tslen = 17
          endif
 2010     format(i10, a, 3(i2.2, a))
 2015     format(f15.3, a)
c
        else if (pt .ge. 1.0d-3) then
c
          write(tstring, 2020) tim, ' sec'
 2020     format(f7.4, a)
          tslen = 11
c
        else
c
          write(tstring, 2030) tim, ' sec'
 2030     format(1p, e11.4, a)
          tslen = 15
c
        endif
      endif
c
c     Removing leading and trailing blanks.
c
      call strim(0, tstring, tslen)
c
      return
      end
c     --- End of routine tnice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine enice(energy, negrel, estring, eslen)
c
c     Expressing an energy (given in GeV) in convenient format for
c     printing.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     energy.......... (input, double precision) The energy to process
c                      (in GeV).
c     negrel.......... (input, integer) If positive, then the
c                      string returned by routine altstring(negrel,..)
c                      is returned for negative energies. Otherwise
c                      the number is processed normally without regard
c                      to its sign.
c     estring......... (output, character*(*)) A string containing
c                      the formatted energy plus the unit used.
c                      Minimum length of this string: 11,
c                      maximum: 20
c     eslen........... (output, integer) Position of last nonblank
c                      character of "estring".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  energy
      integer           negrel
      character*(*)     estring
      integer           eslen
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u
      double precision  denergy
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((energy .lt. 0) .and. (negrel .gt. 0)) then
c
        denergy = abs(energy)
        write(estring, 2004) denergy
 2004   format(1p, g11.4)
        call strim(11, estring, u)
        u = u + 1
        estring(u:u) = ' '
        call altstring(negrel, estring(u+1:20), eslen)
        eslen = eslen + u
c
c       Eliminating leading decimal point and leading and trailing
c       blanks.
c
        call snumtrim(estring, eslen)
c
      else
c
c       Selecting unit and format accordingly with magnitude.
c
        call unice(energy, enicedefunit, neunits0, enicemax, euname,
     +             econvfromgev, estring, eslen)
c
      endif
c
      return
      end
c     --- End of routine enice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine escale(energy, unit, ulen, scaledir, scaleinv)
c
c     Setting scaling factors to convert energies expressed in GeV
c     into other units, so that the resulting numbers are suitable
c     for printing in f format.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c
c     Arguments:
c     =========
c
c     energy.......... (input, double precision) The energy to process
c                      (in GeV), usually it is the maximum energy to
c                      deal with.
c     unit............ (output, character*(*)) The new energy unit.
c     ulen............ (output, integer) The length of string unit.
c     scaledir........ (output, double precision) Direct scaling
c                      factor. Converts from GeV to the new unit.
c     scaleinv........ (output, double precision) Inverse scaling
c                      factor. Converts from the new unit to GeV.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  energy
      character*(*)     unit
      integer           ulen
      double precision  scaledir, scaleinv
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           itmp1, itmp2
c
c     FIRST EXECUTABLE STATEMENT
c
      if (energy .ne. 0) then
c
c       Searching one energy unit such that the absolute value of the
c       given energy is less than 10000 in that unit.
c       This can be ensured for energy in the range 0 to 10^28 eV.
c
        itmp1 = log10(abs(energy))
        itmp2 = (itmp1 + 10) / 3
        itmp2 = max(1, itmp2)
        itmp2 = min(neunits0, itmp2)
c
      else
        itmp2 = 1
      endif
c
      unit     = euname(itmp2)
      ulen     = 3
      if (itmp2 .eq. 1) ulen = 2
c
      scaledir = econvfromgev(itmp2)
      scaleinv = econvtogev(itmp2)
c
      return
      end
c     --- End of routine escale.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine bnice(bfield, negrel, bstring, bslen)
c
c     Expressing a magnetic B field (in nT) in convenient format for
c     printing.
c
c     Written by: S. J. Sciutto, La Plata 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     bfield.......... (input, double precision) The field to process
c                      (in nT).
c     negrel.......... (input, integer) If positive, then the
c                      string returned by routine altstring(negrel,..)
c                      is returned for negative fields. Otherwise
c                      the number is processed normally without regard
c                      to its sign.
c     bstring......... (output, character*(*)) A string containing
c                      the formatted magnetic field plus the unit used.
c                      Minimum length of this string: 10,
c                      maximum: 19
c     bslen........... (output, integer) Position of last nonblank
c                      character of "bstring".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'unitspar.f'
c
c     Declaration of arguments.
c
      double precision  bfield
      integer           negrel
      character*(*)     bstring
      integer           bslen
c
c     Declaration of shared data.
c
      include 'unitscomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u
      double precision  dbf
c
c     FIRST EXECUTABLE STATEMENT
c
      if ((bfield .lt. 0) .and. (negrel .gt. 0)) then
c
        dbf = abs(bfield)
        write(bstring, 2004) dbf
 2004   format(1p, g11.4)
        call strim(11, bstring, u)
        u = u + 1
        bstring(u:u) = ' '
        call altstring(negrel, bstring(u+1:19), bslen)
        bslen = bslen + u
c
c       Eliminating leading decimal point and leading and trailing
c       blanks.
c
        call snumtrim(bstring, bslen)
c
      else
c
c       Selecting unit and format accordingly with magnitude.
c
        call unice(bfield, bnicedefunit, nbunits0, bnicemax, buname,
     +             bconvfromnt, bstring, bslen)
c
      endif
c
      return
      end
c     --- End of routine bnice.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'conversion.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
