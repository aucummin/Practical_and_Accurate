c
c     FILE: summaryutils.f                  Creation date: 11/NOV/1997.
c                                       LAST MODIFICATION: 18/JUN/2003.
c
c     Final statistics and output.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine outsry
c
c     Printing a summary of output variables.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, ls
      double precision  ftmp
      logical           nothing
      character*8       auxauxline
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking if there is anything to print.
c
      if (nallodata .lt. nallodata0) return
c
      if (fulloutputlist) then
        nothing = .false.
      else
        nothing = .true.
        do i = nallodata0, nallodata
          if (veryimpodata(i)) nothing = .false.
        enddo
      endif
c
      if (nothing) return
c
c     Printing the summary of output data.
c
      call putsry(1, '>>>>          SOME ADDITIONAL OUTPUT DATA')
c
c     Floating point data.
c
      nothing = .true.
      do i = nallodata0, nallodata
        if ((fulloutputlist .or. veryimpodata(i)) .and.
     +      (odatatype(i) .le. 4)
     +     ) then
c
          if (nothing) then
            write(auxline, 2010) '       Variable',
     +            '     Mean        RMS Err.      Stddv.'
            call putsry(-1, auxline)
            j = 0
            nothing = .false.
          endif
c
          k = odataitem(i)
          call stmomts(fosamples(k), 1, 5, fodata(1, k))
          call strim(27, odataname(i), ls)
          write(auxline, 2020) odataname(i)(1:ls),
     +                         (fodata(l, k), l = 1, 3)
          call putsry(j, auxline)
          write(auxline, 2022) (fodata(l, k), l = 4, 5)
          call putsry(9, auxline)
          j = 9
        endif
      enddo
 2010 format(1x, a27, 3a)
 2020 format(1x, a27, 1p, g16.6, 2g12.3)
 2022 format(33x, 1p, '(Min =', g13.4, ', Max =', g13.4, ')')
c
c     Integer data.
c
      nothing = .true.
      do i = nallodata0, nallodata
        if ((fulloutputlist .or. veryimpodata(i)) .and.
     +      (odatatype(i) .eq. 5)
     +     ) then
c
          if (nothing) then
            write(auxline, 2010) '       Variable',
     +            '     Average     Minimum    Maximum'
            call putsry(-1, auxline)
            j = 0
            nothing = .false.
          endif
c
          k = odataitem(i)
          ftmp = iodata(1, k)
          if (iosamples(k) .gt. 0) ftmp = ftmp / iosamples(k)
          call strim(27, odataname(i), ls)
          write(auxline, 2030) odataname(i)(1:ls), ftmp,
     +                      (iodata(l, k), l = 2, 3)
          call putsry(j, auxline)
          j = 9
        endif
      enddo
 2030 format(1x, a27, f14.2, 2i11)
c
c     Logical data.
c
      nothing = .true.
      do i = nallodata0, nallodata
        if ((fulloutputlist .or. veryimpodata(i)) .and.
     +      (odatatype(i) .eq. 6)
     +     ) then
c
          if (nothing) then
            write(auxline, 2010) '       Variable', '  Value'
            call putsry(-1, auxline)
            j = 0
            nothing = .false.
          endif
c
          k = odataitem(i)
          call swnice(lodata(k), auxauxline, l)
          call strim(27, odataname(i), ls)
          write(auxline, 2010) odataname(i)(1:ls), '  ',
     +                         auxauxline(1:l)
          call putsry(j, auxline)
          j = 9
        endif
      enddo
c
      return
      end
c     --- End of routine outsry
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine gettcode(tcode, nktables, codearray, tindex)
c
c     Getting a kind-specific table (histogram) index.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     tcode........... (input, integer) The table code.
c     nktables........ (input, integer) The number of available tables
c                      of the specific kind.
c     codearray....... (input, integer, array(nktables)) Array
c                      containing all the table codes.
c     tindex.......... (output, integer) The location of "tcode" within
c                      "codearray".
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           tcode, nktables, tindex
      integer           codearray(nktables)
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, nktables
        tindex = i
        if (codearray(i) .eq. tcode) return
      enddo
c
c     The table code was not found. It is an error.
c
      call errprint(0, '*', 4, 'gettcode', 'Unknown table code.',
     +              1, tcode, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine gettcode.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine multiexport
c
c     Building internal table to control multiple exports of the same
c     table.
c
c     Written by: S. J. Sciutto, La Plata 2000.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           m0, m1, itab, itab0, jtab0, multidx
c
c     FIRST EXECUTABLE STATEMENT
c
      m0            = ichar('a')
      m1            = ichar('z')
      expmultich0   = m0
      expmultidx(1) = m0
      m0            = m0 - 1
c
      do itab0 = 2, ntableexp
        itab = tableexp(itab0)
        do jtab0 = itab0 - 1, 1, -1
          multidx = expmultidx(jtab0)
          if (tableexp(jtab0) .eq. itab) goto 1010
        enddo
        multidx = m0
 1010   continue
        expmultidx(itab0) = min(multidx + 1, m1)
      enddo
c
      return
      end
c     --- End of routine multiexport.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine tprintset(fstbin, lstbin, ld1, histo, plotmaxmin,
     +                     irms, linelen, yscale, abname, abfmt, orfmt,
     +                     hdecades, unity, lunity, yscalef)
c
c     Printing tables with graphical character plotting:
c     Setting variable bounds
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     fstbin, lstbin.. (input, integer) Indices of the first and last
c                      histogram bin, respectively.
c     ld1............. (input, integer) Leading dimension of array
c                      histo.
c     histo........... (input, double precision, array(ld1, nbins))
c                      Histogram data. Elements (i, *) represent,
c                      for i going from 1 to 5: mean, rms error
c                      of the mean, std. deviation, minimum and
c                      maximum.
c     plotmaxmin...... (input, integer) If zero or negative, maximum
c                      and minimum are not plotted; if one, maximum and
c                      minimum are plotted with "<" and ">" symbols;
c                      if greater than one these quantities are also
c                      tabulated.
c     irms............ (input, integer) 2 (3) if the RMS error (std.
c                      deviation) will be used for the error bars.
c     linelen......... (input, integer) The number of characters in
c                      one line.
c     yscale.......... (external, subroutine) Routine used to set
c                      the "y" scaling factor.
c     abname.......... (input, character*(*)) Name of the abscissa.
c     abfmt........... (input, integer) The number of decimal positions
c                      in the abscissa field. If negative, then g
c                      format is used.
c     orfmt........... (input, integer) The number of decimal positions
c                      in the "means" field. If negative, then g format
c                      is used.
c     hdecades........ (input, integer) Half the number of decades to
c                      represent in the lateral character plot. All
c                      data from the maximum value down to
c                      maximum / (10 ** (2 * hdecades)) are plotted.
c                      If hdecades is less than 1, it is taken as 1.
c     unity........... (output, character*(*)) Unit used for the "y"
c                      data.
c     lunity.......... (output, integer) Length of string unity.
c     yscalef......... (output, double precision) "Y" data scaling
c                      factor.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           fstbin, lstbin, ld1
      double precision  histo(ld1, fstbin:lstbin)
      integer           plotmaxmin
      integer           irms
      integer           linelen
      external          yscale
      character*(*)     abname
      integer           abfmt, orfmt, hdecades
      character*(*)     unity
      integer           lunity
      double precision  yscalef
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
      integer           k0, fmtlen, fd3len
      integer           hdec
      double precision  fdec, z, ymax, scalei
      character*24      f2mm
c
      double precision  scale(12)
      save              scale
c
      data              scale / 0.1d0, 0.2d0, 0.4d0, 0.5d0,
     +                          0.8d0, 1.0d0, 2.0d0, 4.0d0,
     +                          5.0d0, 8.0d0, 1.0d1, 2.0d1 /
c
c     FIRST EXECUTABLE STATEMENT
c
      plotmm  = (plotmaxmin .eq. 1)
      printmm = (plotmaxmin .gt. 1)
      rmss    = irms
c
c     Defining the "Y" scale.
c
c     Evaluating the maximum and minimum "y"
c
      ymax = -1.d45
      do i = fstbin, lstbin
        z = histo(1, i) + histo(irms, i)
        if (z .gt. ymax) ymax = z
      enddo
      if (plotmm) then
        do i = fstbin, lstbin
          if (histo(5, i) .gt. ymax) ymax = histo(5, i)
        enddo
      endif
      if (ymax .le. 0) ymax = 1
c
      call yscale(ymax, unity, lunity, yscalef, scalei)
c
c     Setting the table format.
c
c     Bin field, always i5. Abscissas field, with number of decimal
c     figures given as input parameter. Same for means field.
c     Error field, always f10.2. Min and max field, always f12.2 and
c     f10.2
c
      if ((abfmt .gt. 0) .and. (abfmt .lt. 4)) then
        i = 10 + abfmt
      else
        i = 14
      endif
c
      if (printmm) then
        f2mm = ', 2(9x, ''...''))'
      else
        f2mm = ')'
      endif
c
      if (abfmt .ge. 0) then
        if (orfmt .ge. 0) then
          write(tfmt1, 2010) '(i5, f', i, '.', abfmt, ', f',
     +                       orfmt - i + 22, '.', orfmt,
     +                       ', f10.2, 2f12.2)'
          write(tfmt2, 2020) '(i5, f', i, '.', abfmt, ', ',
     +                       orfmt - i + 17,
     +                       'x, ''.....'', 7x, ''...''', f2mm
        else
          write(tfmt1, 2010) '(i5, f', i, '.', abfmt, ', 1p, g',
     +                       -orfmt - i + 22, '.', -orfmt,
     +                       ', g10.2, 2g12.3)'
          write(tfmt2, 2020) '(i5, f', i, '.', abfmt, ', ',
     +                       -orfmt - i + 13,
     +                       'x, ''.....'', 7x, ''...''', f2mm
        endif
      else
        if (orfmt .ge. 0) then
          write(tfmt1, 2010) '(i5, 1p, g', i, '.', -abfmt, ', 0p, f',
     +                       orfmt - i + 22, '.', orfmt,
     +                       ', f10.2, 2f12.2)'
          write(tfmt2, 2020) '(i5, 1p, g', i, '.', -abfmt, ', ',
     +                       orfmt - i + 17,
     +                       'x, ''.....'', 7x, ''...''', f2mm
        else
          write(tfmt1, 2010) '(i5, 1p, g', i, '.', -abfmt, ', g',
     +                       -orfmt - i + 22, '.', -orfmt,
     +                       ', g10.2, 2g12.3)'
          write(tfmt2, 2020) '(i5, 1p, g', i, '.', -abfmt, ', ',
     +                       -orfmt - i + 13,
     +                       'x, ''.....'', 7x, ''...''', f2mm
        endif
      endif
 2010 format(a, i2.2, a, i1, a, i2.2, a, i1, 2a)
 2020 format(a, i2.2, a, i1, a, i2.2, 2a)
c
      fd3len = abs(orfmt) + 8
      fmtlen = fd3len + 29
c
c     ih1 is the position of the first graphic character.
c
      ih1 = fmtlen + 4
c
      if ((ih1 + 22) .gt. linelen) call errprint(0, '*', 4,
     +   'tprintset',
     +   'Insuficient line width to plot a histogram.',
     +   1, ih1 + 22, 0, 0.d0, ' ')
c
c     ic2, ic3, and ic4 mark initially the end of columns.
c
      ic2 = 19
      ic3 = ic2 + fd3len
      ic4 = ic3 + 10
      ic5 = ic4 + 12
      ic6 = ic5 + 12
      ihl = linelen
c
c     Setting the abscissa's label.
c
      i = len(abname)
      if (i .ge. 12) then
        abscname = abname
      else
        abscname = ' '
        abscname(1+(12-i)/2:12) = abname
      endif
c
c     Correcting position of 'Mean', 'Mininum' and 'Maximum'
c
      ic3 = ic3 - (fd3len - 4) / 2
      ic5 = ic5 - 1
      ic6 = ic6 - 1
c
c     Setting the scaling for the axis tick labels.
c
      hdec = max(1, hdecades)
      fdec = 2 * hdec
      ymax = yscalef * ymax
      z    = log10(ymax)
      k0   = z
      ca   = (ihl - ih1) / fdec
      cb   = ih1 - ca * (z - fdec)
      if (z .lt. 0) k0 = k0 - 1
      s1 = k0
      do i = 12, 1, -1
        s2  = log10(scale(i)) + s1
        is1 = ca * s2 + cb
        if (is1 .lt. ihl) goto 1010
      enddo
 1010 continue
c
      s1      = s2
      s2      = s1 - hdec
      is2     = ca * s2 + cb
      is3     = min(is1 + 6, ihl)
      cb      = cb + ca * log10(yscalef)
      scaledy = yscalef
      ystart  = 10 ** (((ih1 + 1.d-3) - cb) / ca)
c
      return
      end
c     --- End of routine tprintset
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine tprinthead(line)
c
c     Printing tables with graphical character plotting:
c     Printing table head. The lines printed are also appended to the
c     running page header.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     line............ (scratch, character*(*)) Working line.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           j
c
c     FIRST EXECUTABLE STATEMENT
c
c     Printing the header.
c
      call setsryhead(.true., ' ')
c
      if (.not. printmm) then
        line = ' '
        write(line(is2-2:is2+6), 2010) 10 ** s2
        write(line(is3-8:is3), 2010) 10 ** s1
 2010   format(1p, g9.2)
c
        call putsry(0, line)
        call setsryhead(.true., line)
      endif
c
      line            = '  Bin  '// abscname
      line(ic3-3:ic3) = 'Mean'
      if (rmss .eq. 3) then
        line(ic4-8:ic4) = 'Std. dev.'
      else
        line(ic4-7:ic4) = 'RMS err.'
      endif
c
      if (printmm) then
c
        line(ic5-6:ic5) = 'Minimum'
        line(ic6-6:ic6) = 'Maximum'
c
        call putsry(0, line)
        call setsryhead(.true., line)
        call putsry(9, ' ')
        call setsryhead(.true., ' ')
c
      else
c
        line(is1:is1) = '|'
        line(is2:is2) = '|'
c
        call putsry(9, line)
        call setsryhead(.true., line)
c
        line          = ' '
        line(ih1:ih1) = '+'
        do j = ih1 + 1, ihl
          line(j:j) = '='
        enddo
        line(ih1+4:ih1+12) = 'LOG-SCALE'
        call putsry(9, line)
        call setsryhead(.true., line)
c
      endif
c
      return
      end
c     --- End of routine tprinthead
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine plothisto(fstbin, lstbin, ld1, histo, fbin, xscale,
     +                     line)
c
c     Character plotting of histograms.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     fstbin, lstbin.. (input, integer) Indices of the first and last
c                      histogram bin, respectively.
c     ld1............. (input, integer) Leading dimension of array
c                      histo.
c     histo........... (input, double precision, array(ld1, nbins))
c                      Histogram data. Elements (i, *) represent,
c                      for i going from 1 to 5: mean, rms error
c                      of the mean, std. deviation, minimum and
c                      maximum.
c     fbin............ (external, returns double precision) External
c                      function to manage the abscissas: fbin(i)
c                      returns the abscissa of bin i.
c     xscale.......... (input, double precision) Scaling factor for the
c                      abscissas to tabulate.
c     line............ (scratch, character*(*)) Working line.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           fstbin, lstbin, ld1
      double precision  histo(ld1, fstbin:lstbin)
      external          fbin
      double precision  fbin
      double precision  xscale
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Tabulating and plotting the histogram.
c
      do i = fstbin, lstbin
        call plotline(i, histo(1, i), fbin, xscale, line)
      enddo
c
      return
      end
c     --- End of routine plothisto
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine plotline(nbin, hdata, fbin, xscale, line)
c
c     Character plotting of one histogram line.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     nbin............ (input, integer) Bin number.
c     hdata........... (input, double precision, array(5)) Histogram
c                      data. Elements (i) represent, for i going from 1
c                      to 5: mean, rms error of the mean, standard
c                      deviation, minimum and maximum.
c     fbin............ (external, returns double precision) External
c                      function to manage the abscissas: fbin(i)
c                      returns the abscissa of bin i.
c     xscale.......... (input, double precision) Scaling factor for the
c                      abscissas to tabulate.
c     line............ (scratch, character*(*)) Working line.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           nbin
      double precision  hdata(5)
      external          fbin
      double precision  fbin
      double precision  xscale
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           j, j1, j2
      double precision  x, y, delta_y, yp
c
c     FIRST EXECUTABLE STATEMENT
c
c     Tabulating and plotting the histogram line
c
      x       = xscale  * fbin(nbin)
      y       = hdata(1)
      delta_y = hdata(rmss)
c
      if (printmm) then
c
c       Full tabulation.
c
        write(line, tfmt1) nbin, x, scaledy * y, scaledy * delta_y,
     +                     scaledy * hdata(4), scaledy * hdata(5)
c
      else
c
c       Brief tabulation plus lateral plot.
c
        write(line, tfmt1) nbin, x, scaledy * y, scaledy * delta_y
c
        line(ih1:ihl) = ' '
        line(ih1:ih1) = '|'
c
        yp = y + delta_y
        if (yp .ge. ystart) then
          j2 = ca * log10(yp) + cb
          if (j2 .gt. ihl) j2 = ihl
          yp = max(ystart, y - delta_y)
          j1 = ca * log10(yp) + cb
          do j = j1, j2
            line(j:j) = '-'
          enddo
        endif
c
        if (plotmm) then
          if (hdata(4) .ge. ystart) then
            j = ca * log10(hdata(4)) + cb
            if (j .le. ihl) line(j:j) = '<'
          endif
          if (hdata(5) .ge. ystart) then
            j = ca * log10(hdata(5)) + cb
            if (j .le. ihl) line(j:j) = '>'
          endif
        endif
c
        if (y .ge. ystart) then
          j = ca * log10(y) + cb
          if (j .le. ihl) line(j:j) = 'o'
        endif
c
      endif
c
      call putsry(9, line)
c
      return
      end
c     --- End of routine plotline
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine plotnulline(nbin, fbin, xscale, line)
c
c     Character plotting of one histogram line: Case of a "null" line,
c     that is, a line where only the abscissa datum is meaningful.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     nbin............ (input, integer) Bin number.
c     fbin............ (external, returns double precision) External
c                      function to manage the abscissas: fbin(i)
c                      returns the abscissa of bin i.
c     xscale.......... (input, double precision) Scaling factor for the
c                      abscissas to tabulate.
c     line............ (scratch, character*(*)) Working line.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           nbin
      external          fbin
      double precision  fbin
      double precision  xscale
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  x
c
c     FIRST EXECUTABLE STATEMENT
c
c     Tabulating and plotting the histogram line
c
      x = xscale * fbin(nbin)
c
      write(line, tfmt2) nbin, x
c
      if (.not. printmm) then
        line(ih1:ihl) = ' '
        line(ih1:ih1) = '|'
      endif
c
      call putsry(9, line)
c
      return
      end
c     --- End of routine plotnulline
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine plotaltline(cc, binstr, xbin, hdata, fbin, xscale,
     +                       line)
c
c     Printing alternative line similarly to the plot lines.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     cc.............. (input, integer) "Carriage control" (Passed to
c                      routine "putsry"
c     binsrt.......... (input, character*(*)) String that is printed
c                      in place of the bin number.
c     xbin............ (input, integer) Substitute for the bin number.
c     hdata........... (input, double precision, array(5)) Histogram
c                      data. Elements (i) represent, for i going from 1
c                      to 5: mean, rms error of the mean, standard
c                      deviation, minimum and maximum.
c     fbin............ (external, returns double precision) External
c                      function to manage the abscissas: fbin(i)
c                      returns the abscissa of bin i.
c     xscale.......... (input, double precision) Scaling factor for the
c                      abscissas to tabulate.
c     line............ (scratch, character*(*)) Working line.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           cc
      character*(*)     binstr
      integer           xbin
      double precision  hdata(5)
      external          fbin
      double precision  fbin
      double precision  xscale
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  x, y, delta_y
c
c     FIRST EXECUTABLE STATEMENT
c
      tfmt1(1:3) = '(a5'
c
c     Printing the data line
c
      x       = xscale * fbin(xbin)
      y       = hdata(1)
      delta_y = hdata(rmss)
c
      if (printmm) then
c
c       Full tabulation.
c
        write(line, tfmt1) binstr, x, scaledy * y, scaledy * delta_y,
     +                     scaledy * hdata(4), scaledy * hdata(5)
c
      else
c
c       Brief tabulation.
c
        write(line, tfmt1) binstr, x, scaledy * y, scaledy * delta_y
c
      endif
c
      call putsry(cc, line)
c
      return
      end
c     --- End of routine plotaltline
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine plotaltnulline(cc, binstr, xbin, fbin, xscale, line)
c
c     Printing alternative line similarly to the plot lines:
c     Case of a "null" line, that is, a line where only the abscissa
c     datum is meaningful.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     cc.............. (input, integer) "Carriage control" (Passed to
c                      routine "putsry"
c     binsrt.......... (input, character*(*)) String that is printed
c                      in place of the bin number.
c     xbin............ (input, integer) Substitute for the bin number.
c     fbin............ (external, returns double precision) External
c                      function to manage the abscissas: fbin(i)
c                      returns the abscissa of bin i.
c     xscale.......... (input, double precision) Scaling factor for the
c                      abscissas to tabulate.
c     line............ (scratch, character*(*)) Working line.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           cc
      character*(*)     binstr
      integer           xbin
      external          fbin
      double precision  fbin
      double precision  xscale
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  x
c
c     FIRST EXECUTABLE STATEMENT
c
      tfmt2(1:3) = '(a5'
c
c     Printing the data line
c
      x       = xscale * fbin(xbin)
      write(line, tfmt2) binstr, x
c
      call putsry(cc, line)
c
      return
      end
c     --- End of routine plotaltnulline
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine exporthead(ut, singlesh, shnum, tcode, tname,
     +                      nxtitle, xtitle, xname, eunit)
c
c     Printing a header for the exported tables.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1998, 1999;
c                                Fermilab 1999.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c     singlesh........ (input, logical) TRUE if exporting single shower
c                      data, FALSE for average data.
c     shnum........... (input, integer) Shower number. This varible
c                      is used only when "singlesh" is TRUE.
c     tcode........... (input, integer) Code of the table being
c                      exported.
c     tname........... (input, character*(*)) Table name.
c     nxtitle......... (input, integer) Number of auxiliary title
c                      lines (To be placed under table name).
c     xtitle.......... (input, character*(*)), array(nxtitle))
c                      Auxiliary title lines.
c     xname........... (input, character*(*)) Abscissa's name
c     eunit........... (input, integer) Energy unit label
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'unitspar.f'
      include 'versionpar.f'
c
c     Declaration of arguments.
c
      integer           ut, shnum, tcode, eunit, nxtitle
      logical           singlesh
      character*(*)     tname, xname
      character*(*)     xtitle(nxtitle)
c
c     Declaration of shared data.
c
      include 'maincomm.f'
      include 'initcomm.f'
      include 'unitscomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*1       cc
      integer           i, j, k
      character*32      lstr
c
c     FIRST EXECUTABLE STATEMENT
c
      cc = char(icommentchar)
c
      write(ut, 2010) cc
 2010 format(5a)
      if (singlesh) then
        write(ut, 2010) cc, '   AIRES Export file (single shower data)'
      else
        write(ut, 2010) cc, '   AIRES Export file'
      endif
      write(ut, 2010) cc
c
      call dati(auxline)
      write(ut, 2010) cc, '   DATE: ', auxline(1:20),
     +                    '; AIRES version ', aires_version
      write(ut, 2010) cc, '   USER: ', cruser(1:cruserlen),
     +                    '; HOST: ', crhost(1:crhostlen)
c
      write(ut, 2010) cc
      write(ut, 2010) cc, '   Task name         : ',
     +                    taskname(1:tasknamelen)
      if (tasknamever .gt. 0) then
        write(ut, 2020) cc, '   Version           :', tasknamever
      endif
 2020 format(2a, i10)
      write(ut, 2010) cc
      write(ut, 2010) cc, '   Task starting date: ', datistr0(1)
c
      if (sryprog) then
        if (nverchanges .gt. 0) then
          write(ut, 2010) cc, '  Origin. AIRES vers.: ',
     +                    original_version
          if (idfvnecurrent) then
            write(ut, 2010) cc, '    Dump file version: ', idfversion
          endif
        else if (idfvnecurrent) then
          write(ut, 2010) cc, '   AIRES version used: ', idfversion
        endif
      endif
c
      write(ut, 2020) cc, '   Number of showers : ', lastshower
c
      write(ut, 2010) cc
      write(ut, 2010) cc
      write(ut, 2030) cc, '   TABLE ', tcode, ': ', tname
 2030 format(2a, i4.4, 2a)
c
      if (nxtitle .gt. 0) then
        do i = 1, nxtitle
          write(ut, 2010) cc, '               ', xtitle(i)
        enddo
      endif
c
      if (singlesh) then
        write(ut, 2010) cc
        write(ut, 2020) cc, '   SHOWER NUMBER ', shnum
        write(ut, 2010) cc
      endif
c
      write(ut, 2010) cc
      write(ut, 2010) cc, '   Units used:'
      write(ut, 2010) cc
      write(ut, 2010) cc, '         Depth   --- g/cm2'
      write(ut, 2010) cc, '         Length  --- m'
      write(ut, 2010) cc, '         Time    --- ns'
      write(ut, 2010) cc, '         Angle   --- deg'
      write(ut, 2010) cc, '         Energy  --- ', euname(eunit)
      write(ut, 2010) cc
      write(ut, 2010) cc, '   Columns:'
      write(ut, 2010) cc
      if (tcode .lt. 5000) then
        if (singlesh) then
          write(ut, 2010) cc, '         1 Bin #, 2 ', xname,
     +                        ', 3 Observable'
        else
          write(ut, 2010) cc, '         1 Bin #, 2 ', xname,
     +                        ', 3 Mean, 4 RMS Error,'
          write(ut, 2010) cc,
     +                 '         5 Std. Dev., 6 Minimum, 7 Maximum.'
        endif
      else
        i = 1
        k = len(xname)
        lstr = '         1 Shower #, 2, 3, ...: '
        do j = 2, len(xname)
          k = j
          if (xname(j:j) .eq. '$') then
            write(ut, 2010) cc, lstr, xname(i:j-1)
            i    = j + 1
            lstr = ' '
          endif
        enddo
        if (k .ge. i) write(ut, 2010) cc, lstr, xname(i:k)
      endif
      write(ut, 2010) cc
      write(ut, 2010) cc, '   >>>>>>>>'
      write(ut, 2010) cc
c
      return
      end
c     --- End of routine exporthead
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function linbin(binindex)
c
c     Tabulating linear bins.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     binindex........ (input, integer) The bin index.
c
c     Return value: (double precision) The corresponding abscissa.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  linbin
      integer           binindex
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      linbin = linbinca * binindex + linbincb
c
      return
      end
c     --- End of routine linbin
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function logbin(binindex)
c
c     Tabulating logarithmic bins.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     binindex........ (input, integer) The bin index.
c
c     Return value: (double precision) The corresponding abscissa.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      double precision  logbin
      integer           binindex
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      logbin = exp(logbinca * binindex + logbincb)
c
      return
      end
c     --- End of routine logbin
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine dstransf(ld, nbinsp1, rawdata, outdata)
c
c     Converting a lateral distribution into density distribution.
c     The bin boundaries are set using shared variables.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1999.
c
c     Arguments:
c     =========
c
c     ld.............. (input, integer) Leading dimension of arrays
c                      rawdata and outdata.
c     nbinsp1......... (input, integer) Number of bins plus 1.
c     rawdata......... (input, double precision, array(ld, 0:nbinsp1))
c                      Input data.
c     outdata......... (output, double precision, array(ld, 0:nbinsp1))
c                      Transformed data.
c     binindex........ (input, integer) The bin index.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           ld, nbinsp1
      double precision  rawdata(ld, 0:nbinsp1)
      double precision  outdata(ld, 0:nbinsp1)
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      double precision  tmp1, tmp2, tmp3
      double precision  logbin
c
c     FIRST EXECUTABLE STATEMENT
c
      tmp1 = 0
      do i = 1, nbinsp1
        tmp2 = pi * (logbin(i) ** 2)
        tmp3 = 1.d0 / (tmp2 - tmp1)
        do j = 1, ld
          outdata(j, i - 1) = tmp3 * rawdata(j, i - 1)
        enddo
        tmp1 = tmp2
      enddo
      do j = 1, ld
        outdata(j, nbinsp1) = 0
      enddo
c
      return
      end
c     --- End of routine dstransf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine lgtransf(ld, nbinsp1, uselog10, rawdata, outdata)
c
c     Converting a lateral (energy) distribution into dN/dln(R) or
c     dN/dlog10(R) (dN/dln(E) or dN/dlog10(E)) distribution.
c     The bin boundaries are set using shared variables.
c
c     Written by: S. J. Sciutto, La Plata 1997, 1999.
c
c     Arguments:
c     =========
c
c     ld.............. (input, integer) Leading dimension of arrays
c                      rawdata and outdata.
c     nbinsp1......... (input, integer) Number of bins plus 1.
c     uselog10........ (input, logical) True to normalize with decimal
c                      logarithms. Natural logarithms normalization
c                      otherwise.
c     rawdata......... (input, double precision, array(5, 0:nbinsp1))
c                      Input data.
c     outdata......... (output, double precision, array(5, 0:nbinsp1))
c                      Transformed data.
c     binindex........ (input, integer) The bin index.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           ld, nbinsp1
      logical           uselog10
      double precision  rawdata(ld, 0:nbinsp1)
      double precision  outdata(ld, 0:nbinsp1)
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
      double precision  ufbina
c
c     FIRST EXECUTABLE STATEMENT
c
      ufbina = 1.d0 / logbinca
      if (uselog10) ufbina = ln10 * ufbina
c
      do i = 2, nbinsp1
        do j = 1, ld
          outdata(j, i - 1) = ufbina * rawdata(j, i - 1)
        enddo
      enddo
      do j = 1, ld
        outdata(j, 0)       = 0
        outdata(j, nbinsp1) = 0
      enddo
c
      return
      end
c     --- End of routine dstransf
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine putsry(cc, line)
c
c     Writing a line into the summary file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c     Arguments:
c     =========
c
c     cc.............. (input, integer) "Carriage control" (more
c                      or less like in old FORTRAN: 0 leave a
c                      previous blank line, 1, begin a new page,
c                      -n leave n+1 blank lines, any other value,
c                      just print).
c     line............ (input, character*(*)) The line to print.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of arguments.
c
      integer           cc
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, itmp
c
c     FIRST EXECUTABLE STATEMENT
c
c     Processing carriage control and number of lines.
c
      if (cc .eq. 1) then
c
c       Beginning of a new page.
c
        currsryline = ntexlines + 1
c
      else if (cc .le. 0) then
c
c       Leaving blank lines.
c
        itmp        = currsryline + 1
        currsryline = itmp + abs(cc)
c
        if (currsryline .lt. ntexlines) then
          do i = itmp, currsryline
            write(8, *, err = 3010)
          enddo
        endif
        if (line .eq. ' ') return
c
      endif
c
c     If the page is full, begin a new one.
c
      if (currsryline .ge. ntexlines) then
c
c       New page.
c
        currsrypage = currsrypage + 1
c
        if (latexsry) then
c
          write(8, 2010, err = 3010) bs, 'end{verbatim}'
          write(8, 2010, err = 3010) bs, 'clearpage'
          write(8, 2020, err = 3010) '% AIRES summary.',
     +                               'Page', currsrypage
 2020     format(a, 52x, a, i5.4)
c
          write(8, 2010, err = 3010) bs, 'begin{verbatim}'
c
c         Printing the page header.
c
          do i = 1, nsryhead
            write(8, 2010, err = 3010) sryhead(i)(1:sryhlen(i))
          enddo
          currsryline = nsryhead
c
        else
          do i = 1, 3
            write(8, *, err = 3010)
          enddo
          currsryline = 3
        endif
c
        if (line .eq. ' ') return
c
      endif
c
c     Printing the line.
c
c     Eliminating trailing blanks.
c
      do i = len(line), 1, -1
        itmp = i
        if (line(i:i) .ne. ' ') goto 1010
      enddo
 1010 continue
c
      currsryline = currsryline + 1
      write(8, 2010, err = 3010) line(1:itmp)
 2010 format(4a)
c
      return
c
c     Error message.
c
 3010 continue
      call errprint(0, '$A09', 4, 'putsry', ' ',
     +              1, 9, 0, 0.d0, ' ')
c
      end
c     --- End of routine putsry
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine setsryhead(append, line)
c
c     Inserting a line in the running page header.
c
c     Written by: S. J. Sciutto, La Plata 1997.
c
c     Arguments:
c     =========
c
c     append.......... (input, logical) "true" if the line is to be
c                      appended to the current header (maximum 10
c                      header lines). "false" to insert first line.
c     line............ (input, character*(*)) The line to insert in
c                      the page header.
c
c
      implicit none
c
c     Declaration of arguments.
c
      logical           append
      character*(*)     line
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, itmp
c
c     FIRST EXECUTABLE STATEMENT
c
      if (.not. append) nsryhead = 0
c
c     Eliminating trailing blanks.
c
      do i = len(line), 1, -1
        itmp = i
        if (line(i:i) .ne. ' ') goto 1010
      enddo
 1010 continue
c
c     Adding the line.
c
      nsryhead = nsryhead + 1
      sryhead(nsryhead) = line
      sryhlen(nsryhead) = itmp
c
      return
      end
c     --- End of routine setsryhead
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine texfile(hdati)
c
c     Writing a LaTeX file that will allow processing the summary
c     output with this text processor.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2001.
c
c     Arguments:
c     =========
c
c     hdati........... (input, character*(*)) Date and time
c                      specification to place in the page header.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of arguments.
c
      character*(*)     hdati
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           auxlen, aux2, i, j
      logical           texspecial
c
c     FIRST EXECUTABLE STATEMENT
c
c     Opening the LaTeX file
c
      auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // texfn
      open(9, file = auxfilestring, status = 'UNKNOWN', err = 3010)
c
c     Writing its contents.
c
      write(9, 2010, err = 3010) '%%'
 2010 format(12a)
c
      auxline = '%% AIRES LaTeX file. Creation date: '
      write(9, 2010, err = 3010) auxline(1:56), hdati
      write(9, 2010, err = 3010) '%%'
c
      write(9, 2010, err = 3010) bs, 'documentclass[11pt]{article}'
      write(9, 2010, err = 3010) bs, 'setlength{', bs,
     +                           'topmargin}{0cm}'
c
      call intnice(texheighmm, 0, auxline, auxlen)
      write(9, 2010, err = 3010) bs, 'setlength{', bs, 'textheight}{',
     +                           auxline(1:auxlen), 'mm}'
c
      write(9, 2010, err = 3010) bs, 'setlength{', bs,
     +                           'textwidth}{163mm}'
      write(9, 2010, err = 3010) bs, 'setlength{', bs,
     +                           'evensidemargin}{0mm}'
      write(9, 2010, err = 3010) bs, 'setlength{', bs,
     +                           'oddsidemargin}{0mm}'
      write(9, 2010, err = 3010) bs, 'pagestyle{plain}'
      write(9, 2010, err = 3010) bs, 'catcode`@=11'
      write(9, 2010, err = 3010) bs, 'renewcommand', bs,
     +                           '@thehead{', bs, 'hss'
      write(9, 2010, err = 3010) bs, 'normalsize', bs, 'rm'
      write(9, 2010, err = 3010) bs, 'vtop{', bs, 'hsize=',
     +                           bs, 'textwidth'
      write(9, 2010, err = 3010) bs, 'baselineskip=14truept'
      write(9, 2010, err = 3010) bs, 'lineskip=1.5truept',
     +                           bs, 'lineskiplimit=1.5truept'
      write(9, 2010, err = 3010) bs, 'hbox to', bs, 'hsize{',
     +                           bs, 'hdstring', bs, 'hfil'
      write(9, 2010, err = 3010) hdati, '}%'
      write(9, 2010, err = 3010) '{', bs, 'baselineskip=0pt'
      write(9, 2010, err = 3010) bs, 'hbox to', bs, 'hsize{', bs,
     +                           'hss', bs, 'vrule height0.25truept'
      write(9, 2010, err = 3010) 'width', bs, 'textwidth depth0pt',
     +                           bs, 'hss}}}}%'
      write(9, 2010, err = 3010) bs, 'catcode`@=12'
c
      write(9, 2010, err = 3010) bs, 'def', bs,
     +                           'hdstring{AIRES summary'
      write(9, 2010, err = 3010) bs, 'hfil{', bs, 'bf'
c
      call scontain(taskname, tasknamelen, 36, auxline, auxlen)
c
c     Protecting TeX special characters that may be present in the
c     task name.
c
      aux2 = 50
      do i = 1, auxlen
        if (texspecial(auxline(i:i), j)) then
          aux2 = aux2 + 1
          auxline(aux2:aux2) = bs
        endif
        aux2 = aux2 + 1
        auxline(aux2:aux2) = auxline(i:i)
      enddo
c
      write(9, 2010, err = 3010) auxline(51:aux2)
c
      if (tasknamever .ne. 0) then
        call intnice(tasknamever, 0, auxline, auxlen)
        write(9, 2010, err = 3010) '(', auxline(1:auxlen), ')'
      endif
c
      write(9, 2010, err = 3010) '}}'
c
      write(9, 2010, err = 3010) bs, 'begin{document}'
      call fltnice(texbaselinemm, 0, auxline, auxlen)
      write(9, 2010, err = 3010) bs, 'baselineskip=',
     +                           auxline(1:auxlen), 'mm'
      write(9, 2010, err = 3010) bs, 'lineskip=1pt',
     +                           bs, 'lineskiplimit=1pt'
c
      write(9, 2010, err = 3010) bs, 'input{',
     +                           leadingf0(1:leadingf0len),
     +                           sryfn, '}'
c
      write(9, 2010, err = 3010) bs, 'end{document}'
c
      close(9)
c
      return
c
c     Error message.
c
 3010 continue
      call errprint(0, '$A09', 4, 'texfile',
     +              auxfilestring(1:leadingfnlen(2)+8),
     +              1, 9, 0, 0.d0, ' ')
c
      end
c     --- End of routine texfile.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine intfileskip(ut, mark)
c
c     Reading integer numbers from an unformatted file until finding a
c     specified mark.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN unit associated with
c                      the file.
c     mark............ (input, integer) The number to find.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ut, mark
c
c     Declaration of internal variables and arrays.
c
      integer           record1
c
c     FIRST EXECUTABLE STATEMENT
c
 1010 continue
      read(ut, err = 3010, end = 3010) record1
      if (record1 .ne. mark) goto 1010
c
      return
c
c     Error message.
c
 3010 continue
      call errprint(0, '$A30', 4, 'intfileskip', ' ',
     +              1, ut, 0, 0.d0, ' ')
c
      end
c     --- End of routine intfileskip.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'summaryutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
