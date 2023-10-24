c
c     FILE: summaryf.f                      Creation date: 14/JUL/1996.
c                                       LAST MODIFICATION: 20/AUG/2003.
c
c     Final statistics and output.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine summary
c
c     Final statistics and writing summary file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2001,
c                                         2003.
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
      include 'maincomm.f'
      include 'initcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      if (lastshower .lt. 1) call errprint(0, '*', 4, 'summary',
     +  'There are no simulated showers. Cannot make the summary.',
     +  0, 0, 0, 0.d0, ' ')
c
      sryprog = (pgmcode .ge. 2000)
c
c     FINAL STATISTICS FOR HISTOGRAMS.
c
      call table2(lastshower)
c
      if (sryison) then
c
c       Final statistics for stack and other observables.
c
        call momstxobs(lastshower)
c
c       Printing the basic summary.
c
        call summary0
c
c       Printing the different tables
c
        if (ntableprt .gt. 0) call tprints
c
c       Index of tables.
c
        if (tableindex) call idxprint
c
c       Closing the summary file.
c
        call putsry(-1, '>>>>          END OF SUMMARY.')
c
        if (latexsry) then
          write(8, 2010, err = 3010) bs, 'end{verbatim}'
        else
          write(8, *, err = 3010)
        endif
 2010   format(4a)
        close(8)
c
      endif
c
c     WRITING THE TSS FILE.
c
      if (tssison) call tssfile
c
c     EXPORTING THE DIFFERENT TABLES.
c
      if (ntableexp .gt. 0) call texports
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A09', 4, 'summary', ' ',
     +              1, 8, 0, 0.d0, ' ')
      return
      end
c     --- End of routine summary.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine summary0
c
c     Writing the basic output summary.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2001, 2003;
c                                Fermilab 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c     include 'kernelpar.f'        (Included by 'pstackpar.f')
      include 'pstackpar.f'
      include 'pclepar.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'kernelcomm.f'
      include 'pstackcomm.f'
      include 'pclecomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      external          fltscale, escale
      integer           i, j, k
      character*20      srydati
      integer           flen1, slen1, slen2
      character*64      sryauxstr1, sryauxstr2
      character*80      line
      double precision  ufmain, xmax, nmax, x1(2), sumofsq
      integer           fitrc
      integer           itmp1, itmp2, itmp3, itmp4
      double precision  tmp1
      integer           ifmt
      character*72      f2130(3)
      double precision  scald, scali
      logical           fhead
      character*9       bl9
      data              bl9 / '         ' /
c
c     FIRST EXECUTABLE STATEMENT
c
c     PREPARING THE OUTPUT SUMMARY FILES.
c
      call dati(srydati)
c
 2010 format(4a)
c
c     Opening the summary file. Looking for a new file.
c
      sryfn = sryext
      texfn = texext
      slen1 = 0
      auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // sryext
      inquire (file = auxfilestring, exist = fhead)
      if (.not. fhead) goto 1010
c
      slen1 = 4
      do i = 1, 99
        write(sryfn, 2012) '_s', i, sryext
        write(texfn, 2012) '_s', i, texext
 2012   format(a, i2.2, a)
        auxfilestring = leadingfn(2)(1:leadingfnlen(2)) // sryfn
        inquire (file = auxfilestring, exist = fhead)
        if (.not. fhead) goto 1010
      enddo
c
c     All versions from 0 to 99 are in use! Using version 99
c     nevertheless.
c
      call errprint(0, '*', 2, 'summary0',
     + 'All summary versions from 0 to 99 exist! Forcing version 99.',
     + 0, 0, 0, 0.d0, ' ')
c
 1010 continue
c
      flen1 = leadingfnlen(2) + slen1 + 4
c
c     Opening the file.
c
      open(8, file = auxfilestring, status = 'UNKNOWN',
     +        access = 'SEQUENTIAL', err = 3010)
c
c     Setting the current line and page numbers.
c
      currsryline = 0
      currsrypage = 1
      nsryhead    = 0
c
c     Creating the LaTeX file
c
      if (latexsry) then
c
        call texfile(srydati)
        write(8, 2010, err = 3010) bs, 'clearpage'
        write(8, 2010, err = 3010) bs, 'begin{verbatim}'
c
      else
        call putsry(0, ' ')
      endif
c
c     WRITING THE SUMMARY.
c
c     The summary starts with AIRES logo.
c
      call aireslogo(8, i)
      currsryline = currsryline + i
c      
c     Followed by the ZHAIRES logo      
c
      call zhaireslogo(8, i)
      currsryline = currsryline + i
c      
c     The text of the welcome message will be put here.
c
      call welcome(8, ' ', i)
      currsryline = currsryline + i
c
c     Completing the first page.
c
 2020 format(a, a27, ': ', 3a)
 2030 format(a, a27, ': ', 4i10)
c
      if (tasknamever .eq. 0) then
        call scontain(taskname, tasknamelen, 42, auxline, slen2)
        write(line, 2020) bl9, 'Task Name', auxline(1:slen2)
        call putsry(-2, line)
      else
        call intnice(tasknamever, 0, sryauxstr1, slen1)
        if (tasknamelen .le. (29 - slen1)) then
          write(line, 2020) bl9,
     +                      'Task Name', taskname(1:tasknamelen),
     +                      '    Version: ', sryauxstr1(1:slen1)
          call putsry(-2, line)
        else
          call scontain(taskname, tasknamelen, 42, auxline, slen2)
          write(line, 2020) bl9, 'Task Name', auxline(1:slen2)
          call putsry(-2, line)
          write(line, 2020) bl9, 'Task Version', sryauxstr1(1:slen1)
          call putsry(0, line)
        endif
      endif
c
      write(line, 2020) bl9, 'Current date and time',
     +                  srydati
      call putsry(-1, line)
c
      write(line, 2020) bl9, 'Processing started at date',
     +                  datistr0(1)
      call putsry(0, line)
c
      if (lastshower .lt. mtotshowers) then
        write(line, 2020) bl9, 'Last run ended at date',
     +                    datistr0(3)
        call putsry(0, line)
        write(line, 2020) bl9, 'Last shower completion date',
     +                    datistr0(2)
        call putsry(0, line)
        write(line, 2030) bl9, 'Completed (total) showers',
     +                    lastshower, mtotshowers
        call putsry(0, line)
      else
        write(line, 2020) bl9, 'Processing ended at date',
     +                    datistr0(2)
        call putsry(0, line)
        write(line, 2030) bl9, 'Total number of showers',
     +                    mtotshowers
        call putsry(0, line)
      endif
c
      write(line, 2030) bl9, 'Number of processes', processnumber
      call putsry(0, line)
      write(line, 2030) bl9, 'Number of runs', jobnumber
      call putsry(0, line)
c
      call tnice(cpu0(1), 1, sryauxstr1, slen1)
      write(line, 2020) bl9, 'Total CPU time', sryauxstr1(1:slen1)
      call putsry(0, line)
c
      call tnice(cpu0(2) / lastshower, 0, sryauxstr1, slen1)
      write(line, 2020) bl9, 'Avg. CPU time per shower',
     +                  sryauxstr1(1:slen1)
      call putsry(0, line)
c
      if (sryprog) then
c
        if ((idfcruser(1:idfcruserlen) .ne. cruser(1:cruserlen)) .or.
     +      (idfcrhost(1:idfcrhostlen) .ne. crhost(1:crhostlen))
     +     ) then
c
          call scontain(idfcrhost, idfcrhostlen, 41 - idfcruserlen,
     +                  auxline, slen2)
          write(line, 2020) bl9, 'Idf file written by',
     +                      idfcruser(1:idfcruserlen), '@',
     +                      auxline(1:slen2)
          call putsry(0, line)
        endif
c
        if (nverchanges .gt. 0) then
          write(line, 2020) bl9, 'Original AIRES version',
     +                           original_version
          call putsry(0, line)
          if (idfvnecurrent) then
            write(line, 2020) bl9, 'Dump file version',
     +                             idfversion
            call putsry(0, line)
          endif
        else if (idfvnecurrent) then
          write(line, 2020) bl9, 'AIRES version used',
     +                           idfversion
          call putsry(0, line)
        endif
c
        if (adfnmergedfiles .gt. 0) then
          write(line, 2030) bl9, 'Number of merged dump files',
     +                      adfnmergedfiles
          call putsry(0, line)
        endif
c
      endif
c
c     Printing the summary of input data.
c
      call putsry(1,
     +     '>>>>                        SUMMARY OF INPUT DATA')
      call putsry(9,
     +     '>>>>                        ---------------------')
c
      call inpsry(3)
c
c     Printing the Output Summary.
c
      call putsry(1,
     +     '>>>>                           OUTPUT SUMMARY')
      call putsry(9,
     +     '>>>>                           --------------')
c
      ufmain = 1.d0 / lastshower
 2110 format(a, a27, ': ', f12.2, 3i10)
 2112 format(a, a27, ': ', 2i6)
c
c     Defining the formats to use in floating point number printing.
c
 2129 format(1x, a, i2)
 2130 format(1x, a25 , ': ', f9.3, 4f9.2)
 2131 format(1x, a25)
c
      f2130(1) = '(1x, a25 , '': '', f9.6, 4f9.5)'
      f2130(2) = '(1x, a25 , '': '', f9.5, 4f9.4)'
      f2130(3) = '(1x, a25 , '': '', f9.3, 4f9.2)'
c
c     Stack statistics.
c
      if (stackinfo) then
c
        call putsry(-1, '>>>>          STACK USAGE')
c
        write(line, 2030) bl9, 'Total number of stacks', npstacks
        call putsry(0, line)
        itmp1 = (maxstaentries * maxstalen) / 128 + 0.5d0
        write(line, 2030) bl9, 'Total size (entries, KB)',
     +                         maxstaentries, itmp1
        call putsry(9, line)
c
        do i = 1, npstacks
c
          write(line, 2112) bl9, 'Stack number and category',
     +                      i, stacateg(i)
          call putsry(0, line)
          write(line, 2020) bl9, 'Name of stack',
     +                      psta_n(i)
          call putsry(9, line)
          write(line, 2020) bl9, 'Processed by routine(s)',
     +                      psta_rn(1, i), ' ', psta_rn(2, i)
          call putsry(9, line)
          write(line, 2020) bl9, 'Model(s) used',
     +                      psta_model_n(i)
          call putsry(9, line)
          write(line, 2110) bl9, 'Avg. calls per shower',
     +                      ufmain * callcounter(1, i)
          call putsry(9, line)
          tmp1  = ufmain * avgtotsize(i)
          itmp1 = tmp1 * maxstalen / 128 + 0.5d0
          write(line, 2110) bl9, 'Avg. size (entries, KB)',
     +                      tmp1, itmp1
          call putsry(9, line)
          write(line, 2110) bl9, 'Peak size (avg, min, max)',
     +                      ufmain * peakstsize(1, i),
     +                      (peakstsize(j, i), j = 2, 3)
          call putsry(9, line)
          write(line, 2110) bl9, 'Hard write (avg, min, max)',
     +                      ufmain * hardswrite(1, i),
     +                      (hardswrite(j, i), j = 2, 3)
          call putsry(9, line)
          write(line, 2110) bl9, 'Hard read (avg, min, max)',
     +                      ufmain * hardsread(1, i),
     +                      (hardsread(j, i), j = 2, 3)
          call putsry(9, line)
          itmp3 = procentries(2, i) + 0.5d0
          itmp4 = procentries(3, i) + 0.5d0
          write(line, 2110) bl9, 'Tot entries (avg, min, max)',
     +                      ufmain * procentries(1, i),
     +                      itmp3, itmp4
          call putsry(9, line)
c
        enddo
c
        itmp2 = 1
c
      else
c
        line = '>>>>          STACK DESCRIPTION: ' //
     +         'Names and average sizes (entries, KB).'
        call putsry(-1, line)
c
        itmp2 = 0
        do i = 1, npstacks
          tmp1  = ufmain * avgtotsize(i)
          itmp1 = tmp1 * maxstalen / 128 + 0.5d0
          write(line, 2115) i, psta_n(i), tmp1, itmp1
          call putsry(itmp2, line)
          itmp2 = 9
        enddo
 2115   format(i6, 2x, a, f13.2, i10)
c
        itmp2 = -1
c
      endif
c
c     Summary of particles and energies per stack.
c
c     Particles:
c
      call putsry(itmp2,
     +  '>>>>          PER SHOWER BALANCE OF PARTICLES')
c
c     Evaluating the maximum number and setting the scale factor.
c
      tmp1  = abs(max(totpcles(3, 0), totpcles(5, 0)))
      call fltscale(tmp1, sryauxstr1, slen1, scald, scali)
c
      if (scald .ne. 1) then
c
c       A scaling factor is needed.
c
        line =
     +    ' All particle numbers are expressed in units of '
     +    // sryauxstr1(1:slen1) // ' particles.'
        call putsry(-1, line)
      endif
c
      line = '                      Item     Mean' //
     +       '   RMS Err.   Stddv.    Min.     Max.'
      call putsry(-1, line)
c
c     Setting the format to use.
c
      tmp1 = scald * tmp1
      if (tmp1 .lt. 10) then
        ifmt = 1
      else if (tmp1 .lt. 100) then
        ifmt = 2
      else
        ifmt = 3
      endif
c
c     Printing the particle information.
c
      do i = 1, npstacks
        write(line, 2129) 'Stack', i
        call putsry(0, line)
        write(line, f2130(ifmt)) 'Particles stacked',
     +                           (scald * totpcles(j, i), j = 1, 5)
        call putsry(9, line)
        write(line, f2130(ifmt)) 'Particles lost',
     +                           (scald * nplost(j, i), j = 1, 5)
        call putsry(9, line)
        write(line, f2130(ifmt)) 'Low-E particles',
     +                           (scald * nplowe(j, i), j = 1, 5)
        call putsry(9, line)
        write(line, f2130(ifmt)) 'Pcles. that reached gnd.',
     +                           (scald * nprgnd(j, i), j = 1, 5)
        call putsry(9, line)
      enddo
c
      write(line, *) 'Totals:'
      call putsry(0, line)
      write(line, f2130(ifmt)) 'Particles stacked',
     +                        (scald * totpcles(j, 0), j = 1, 5)
      call putsry(9, line)
      write(line, f2130(ifmt)) 'Particles lost',
     +                         (scald * nplost(j, 0), j = 1, 5)
      call putsry(9, line)
      write(line, f2130(ifmt)) 'Low-E particles',
     +                         (scald * nplowe(j, 0), j = 1, 5)
        call putsry(9, line)
      write(line, f2130(ifmt)) 'Pcles. that reached gnd.',
     +                         (scald * nprgnd(j, 0), j = 1, 5)
      call putsry(9, line)
c
      call putsry(-1,
     +  ' Other particles not processed in stacks.')
c
      write(line, f2130(ifmt)) 'Neutrinos',
     +                         (scald * nneutrino(j), j = 1, 5)
      call putsry(0, line)
      write(line, f2130(ifmt)) 'Unphysical particles',
     +                         (scald * nnotap(j), j = 1, 5)
      call putsry(9, line)
c
c
c     Energies:
c
      call putsry(1,
     +  '>>>>          PER SHOWER BALANCE OF ENERGY')
c
c     Setting the scale factor.
c
      scali = aveprim(1)
      scald = 1.d0 / scali
c
      call enice(scali, -1, auxline, itmp2)
      call fltnice(1.d9 * scali, -1, auxline(101:176), itmp1)
      call putsry(-1,
     +  ' All energies are expressed in units of the average primary')
      line =
     +  ' energy: ' // auxline(1:itmp2) //
     +  ' (' // auxline(101:100+itmp1) // ' eV).'
      call putsry(9, line)
c
      line = '                      Item      Mean' //
     +       '   RMS Err.   Stddv.     Min.     Max.'
      call putsry(-1, line)
c
c     Printing the particle information.
c
      do i = 1, npstacks
        write(line, 2129) 'Stack', i
        call putsry(0, line)
        write(line, 2140) 'Medium losses',
     +                    (scald * eloss(j, i), j = 1, 5)
        call putsry(9, line)
        write(line, 2140) 'Particles lost',
     +                    (scald * elost(j, i), j = 1, 5)
        call putsry(9, line)
        write(line, 2140) 'Low-E particles',
     +                    (scald * elowe(j, i), j = 1, 5)
        call putsry(9, line)
        write(line, 2140) 'Pcles. that reached gnd.',
     +                    (scald * eprgnd(j, i), j = 1, 5)
        call putsry(9, line)
      enddo
c
 2140 format(1x, a25 , ': ', f10.7, 4f9.5)
c
      write(line, *) 'Totals:'
      call putsry(0, line)
c
      write(line, 2140) 'Primary energy',
     +                  (scald * aveprim(j), j = 1, 5)
      call putsry(0, line)
c
      write(line, 2140) 'Medium losses',
     +                  (scald * eloss(j, 0), j = 1, 5)
      call putsry(0, line)
      write(line, 2140) 'Particles lost',
     +                  (scald * elost(j, 0), j = 1, 5)
      call putsry(9, line)
      write(line, 2140) 'Low-E particles',
     +                  (scald * elowe(j, 0), j = 1, 5)
        call putsry(9, line)
      write(line, 2140) 'Pcles. that reached gnd.',
     +                  (scald * eprgnd(j, 0), j = 1, 5)
      call putsry(9, line)
c
      call putsry(-1,
     +  ' Other particles not processed in stacks.')
c
      write(line, 2140) 'Neutrinos',
     +                  (scald * eneutrino(j), j = 1, 5)
      call putsry(0, line)
      write(line, 2140) 'Unphysical particles',
     +                  (scald * enotap(j), j = 1, 5)
      call putsry(9, line)
c
c     Printing special primary information.
c
      if (nescpcles .gt. 0) then
c
        call putsry(1,
     +    '>>>>          SPECIAL PRIMARY PARTICLE SUMMARY')
c
        call putsry(-1, ' Defined primary particle(s)')
c
        line = '    Name              Associated external module' //
     +         '                        Version'
        call putsry(0, line)
c
        itmp2 = 0
        do i = 1, nescpcles
          itmp1 = escmacropos(3, i) - escmacropos(2, i)
          call scontain(escpclemacro(escmacropos(2, i) + 1:
     +                               escmacropos(3, i)),
     +                  itmp1, 49, auxline, slen2)
          write(line, 2150) i, escpclename(i), auxline(1:slen2),
     +                         escmacrover(i)
          call putsry(itmp2, line)
          itmp2 = 9
        enddo
 2150   format(i3, 1x, a16 , 2x, a, t73, i6.6)
c
        call putsry(-1,
     + ' Primary particle generation (avg, min, max, # of calls)')
c
        itmp2 = 0
        do i = 1, nescpcles
          if (escmacrouse(i) .le. 0) then
            nsprimpart(2, i) = 0
          endif
          write(line, 2160) escpclename(i),
     +                      nsprimpart(1, i) /
     +                      dfloat(max(escmacrouse(i), 1)),
     +                      (nsprimpart(j, i), j = 2, 3),
     +                      escmacrouse(i)
          call putsry(itmp2, line)
          itmp2 = 9
        enddo
 2160   format(4x, a, f12.2, 3i12)
c
      endif
c
      call putsry(1,
     +  '>>>>          PARTICLES REACHING GROUND LEVEL')
c
c     Evaluating the maximum number and setting the scale factor.
c
      tmp1  = abs(max(lhistn(3, nobslevelsp1, nlhtables),
     +                lhistn(5, nobslevelsp1, nlhtables)))
      call fltscale(tmp1, sryauxstr1, slen1, scald, scali)
c
      if (scald .ne. 1) then
c
c       A scaling factor is needed.
c
        line =
     +    '     All particle numbers are expressed in units of '
     +    // sryauxstr1(1:slen1) // ' particles.'
        call putsry(-1, line)
      endif
c
      line = '                           Mean' //
     +       '   RMS Err.   Stddv.    Min.     Max.'
      call putsry(-1, line)
c
c     Setting the format to use.
c
      tmp1 = scald * tmp1
      if (tmp1 .lt. 10) then
        sryauxstr2 = '(5x, a , 2x, f10.7, 4f9.6)'
      else if (tmp1 .lt. 100) then
        sryauxstr2 = '(5x, a , 2x, f10.6, 4f9.5)'
      else
        sryauxstr2 = '(5x, a , 2x, f10.4, 4f9.3)'
      endif
c
c     Printing the particle information.
c
      do i = 1, ngrdprint
        k = grdporder(i)
        write(line, sryauxstr2) lhpclen(k),
     +                          (scald * lhistn(j, nobslevelsp1, k),
     +                          j = 1, 5)
        call putsry(grdpspa(i), line)
      enddo
c
      call putsry(1,
     +  '>>>>          UNWEIGHTED GROUND PARTICLE ENTRIES')
c
c     Evaluating the maximum number and setting the scale factor.
c
      tmp1  = abs(max(wlhistn(3, nobslevelsp1, nlhtables),
     +                wlhistn(5, nobslevelsp1, nlhtables)))
      call fltscale(tmp1, sryauxstr1, slen1, scald, scali)
c
      if (scald .ne. 1) then
c
c       A scaling factor is needed.
c
        line =
     +    '     All particle entry numbers are expressed in units of '
     +    // sryauxstr1(1:slen1) // ' entries.'
        call putsry(-1, line)
      endif
c
      line = '                           Mean' //
     +       '   RMS Err.   Stddv.    Min.     Max.'
      call putsry(-1, line)
c
c     Setting the format to use.
c
      tmp1 = scald * tmp1
      if (tmp1 .lt. 10) then
        sryauxstr2 = '(5x, a , 2x, f10.7, 4f9.6)'
      else if (tmp1 .lt. 100) then
        sryauxstr2 = '(5x, a , 2x, f10.6, 4f9.5)'
      else
        sryauxstr2 = '(5x, a , 2x, f10.4, 4f9.3)'
      endif
c
c     Printing the particle information.
c
      do i = 1, ngrdprint
        k = grdporder(i)
        write(line, sryauxstr2) lhpclen(k),
     +                          (scald * wlhistn(j, nobslevelsp1, k),
     +                          j = 1, 5)
        call putsry(grdpspa(i), line)
      enddo
c
c
      call putsry(1,
     +  '>>>>          ENERGY OF GROUND LEVEL PARTICLES')
c
c     Evaluating the maximum energy and setting the scale factor.
c
      tmp1  = abs(max(lhiste(3, nobslevelsp1, nlhtables),
     +                lhiste(5, nobslevelsp1, nlhtables)))
      call escale(tmp1, sryauxstr1, slen1, scald, scali)
c
      line = '     All energies are expressed in ' //
     +       sryauxstr1(1:slen1) // '.'
      call putsry(-1, line)
c
      line = '                           Mean' //
     +       '   RMS Err.   Stddv.    Min.     Max.'
      call putsry(-1, line)
c
c     Setting the format to use.
c
      tmp1 = scald * tmp1
      if (tmp1 .lt. 10) then
        sryauxstr2 = '(5x, a , 2x, f10.7, 4f9.5)'
      else if (tmp1 .lt. 100) then
        sryauxstr2 = '(5x, a , 2x, f10.6, 4f9.4)'
      else
        sryauxstr2 = '(5x, a , 2x, f10.4, 4f9.2)'
      endif
c
c     Printing the energy information.
c
      do i = 1, ngrdprint
        k = grdporder(i)
        write(line, sryauxstr2) lhpclen(k),
     +                          (scald * lhiste(j, nobslevelsp1, k),
     +                          j = 1, 5)
        call putsry(grdpspa(i), line)
      enddo
c
c     Other shower parameters.
c
      call putsry(1, '>>>>          OTHER SHOWER PARAMETERS')
c
      line = '                               Mean' //
     +       '   RMS Err.   Stddv.    Min.     Max.'
      call putsry(-1, line)
c
      write(line, 2130) 'Vt. first interact. depth',
     +                  (fstintdp(j, 1), j = 1, 5)
      call putsry(0, line)
      write(line, 2130) 'Sl. first interact. depth',
     +                  (fstintdp(j, 2), j = 1, 5)
      call putsry(9, line)
      write(line, 2131) '(Depths in g/cm2)'
      call putsry(9, line)
c
      fhead = .false.
c
      if (shxsamples .gt. 0) then
c
        call putsry(-1, '     Shower maximum estimation:')
c
        write(line, 2130) 'Vt. depth of max. (g/cm2)',
     +                    (shxmax(j, 1), j = 1, 5)
        call putsry(0, line)
        write(line, 2130) 'Sl. depth of max. (g/cm2)',
     +                    (shxmax(j, 2), j = 1, 5)
        call putsry(9, line)
c
c       Evaluating the magnitude of NXmax and setting the scale
c       factor.
c
        tmp1 = abs(max(shnmax(3), shnmax(5)))
        call fltscale(tmp1, sryauxstr1, slen1, scald, scali)
c
c       Setting the format to use.
c
        tmp1 = scald * tmp1
        if (tmp1 .lt. 10) then
          ifmt = 1
        else if (tmp1 .lt. 100) then
          ifmt = 2
        else
          ifmt = 3
        endif
c
        write(line, f2130(ifmt)) 'Charged pcles. at maximum',
     +                           (scald * shnmax(j), j = 1, 5)
        call putsry(9, line)
        if (scald .ne. 1) then
          sryauxstr2 = '(Times ' // sryauxstr1(1:slen1) // ')'
          write(line, 2131) sryauxstr2(1:slen1+8)
          call putsry(9, line)
        endif
c
        fhead = .true.
      endif
c
c     Fitting Xmax and Nmax on the "average shower"
c
      if (lastshower .gt. 2) then
c
c       No error analisis implemented yet!
c
        call xmaxfit(1, x1(1), x1(2), xmax, nmax, sumofsq, fitrc)
c
        if (fitrc .eq. 0) then
c
          call putsry(0, '     Estimation on the "average shower":')
c
          write(line, 2130) 'Vt. depth of max. (g/cm2)', xmax
          call putsry(0, line)
c
c         Setting the scale factor for Nmax.
c
          call fltscale(nmax, sryauxstr1, slen1, scald, scali)
c
c         Setting the format to use.
c
          tmp1 = abs(scald * nmax)
          if (tmp1 .lt. 10) then
            ifmt = 1
          else if (tmp1 .lt. 100) then
            ifmt = 2
          else
            ifmt = 3
          endif
c
          write(line, f2130(ifmt)) 'Charged pcles. at maximum',
     +                             scald * nmax
          call putsry(9, line)
          if (scald .ne. 1) then
            sryauxstr2 = '(Times ' // sryauxstr1(1:slen1) // ')'
            write(line, 2131) sryauxstr2(1:slen1+8)
            call putsry(9, line)
          endif
c
          fhead = .true.
        endif
      endif
c
      if (fhead) then
        call putsry(0,
     +    '     The fits were done with the Levenberg-Marquardt')
        call putsry(9,
     +    '     nonlinear least-squares fitting algorithm, modelling')
        call putsry(9,
     +    '     shower profiles with a 4-parameter Gaisser-Hillas' //
     +    ' function.')
c
        call intnice(shxsamples, 0, sryauxstr1, slen1)
        line =
     +    '     Number of showers with converged fits: ' //
     +  sryauxstr1(1:slen1)
        call putsry(0, line)
      else
        call putsry(-1, '     Shower maximum: No data available.')
      endif
c
c     Printing the predefined output variables.
c
      call outsry
c
c     End of basic summary.
c
      nsryhead = 0
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A09', 4, 'summary0',
     +              auxfilestring(1:flen1),
     +              1, 8, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine summary0.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine idxprint
c
c     Printing the index of tables.
c
c     Written by: S. J. Sciutto, La Plata 1999, 2001.
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
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k
      character*80      line
c
c     FIRST EXECUTABLE STATEMENT
c
      call putsry(1, '>>>>          TABLE INDEX.')
      call setsryhead(.false., 'Table index (continued)')
      call setsryhead(.true., ' ')
      call putsry(-1, '       Code   Name')
      call setsryhead(.true., '       Code   Name')
      call setsryhead(.true., ' ')
c
c     Listing the available tables.
c
      k = 0
      do i = 1, nlhtables
        write(line, 2250) i, lhcoden(i), lhnamen(i)
        call putsry(k, line)
        k = 9
      enddo
      j = nlhtables
c
      k = 0
      do i = 1, nlhtables
        j = j + 1
        write(line, 2250) j, wlhcoden(i), wlhnamen(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nlhtables
        j = j + 1
        write(line, 2250) j, lhcodee(i), lhnamee(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nldtables
        j = j + 1
        write(line, 2250) j, ldcoden(i), ldnamen(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nldtables
        j = j + 1
        write(line, 2250) j, wldcoden(i), wldnamen(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nldtables
        j = j + 1
        write(line, 2250) j, ldcodee(i), ldnamee(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nldtables
        j = j + 1
        write(line, 2250) j, wldcodee(i), wldnamee(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, ntdtables
        j = j + 1
        write(line, 2250) j, tdcoden(i), tdnamen(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, npshtables
        j = j + 1
        write(line, 2250) j, pshcode(i), pshname(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nlitables
        j = j + 1
        write(line, 2250) j, llcoden(i), llnamen(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nlitables
        j = j + 1
        write(line, 2250) j, wllcoden(i), wllnamen(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nlitables
        j = j + 1
        write(line, 2250) j, llcodee(i), llnamee(i)
        call putsry(k, line)
        k = 9
      enddo
c
      k = 0
      do i = 1, nlitables
        j = j + 1
        write(line, 2250) j, licodee(i), linamee(i)
        call putsry(k, line)
        k = 9
      enddo
c
 2250 format(1x, i3, 3x, i4.4, 3x, a)
c
      nsryhead = 0
c
      return
      end
c     --- End of routine idxprint.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine tprints
c
c     Including the selected tables into the summary file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      external          linbin, logbin, fltscale, escale
      double precision  linbin, logbin
      integer           i, j, k, l, i1, i2
      integer           flen1, slen1, slen2
      character*64      sryauxstr1, sryauxstr2
      character*80      line
      integer           ipars(5)
      integer           fitrc, shprcode
      equivalence       (fitrc, ipars(1))
      equivalence       (shprcode, ipars(2))
      double precision  xpars(5), sumofsq
      integer           klast, knmax, kxmaxv
      integer           ngt, kfields, rfields
      integer           itmp1, itmp2
      double precision  tmp1
      double precision  auxtable(5, 0:tdbinsp1)
      double precision  auxtablel(5, mxobslevelsp1)
      double precision  auxtable2(5 * mxobslevelsp1)
      equivalence       (obslevscra0, auxtable, auxtablel, auxtable2)
      character*64      auxtname
      double precision  scald, scali
      integer           itab0, itab, jtab, jtab1
      integer           iopt(4)
c
      character*16      pentries(2, 2)
      integer           lpentries(2, 2), ipe
c
      data  pentries(1, 1), lpentries(1, 1) / 'particle'      ,  8 /
      data  pentries(2, 1), lpentries(2, 1) / ' particles'    , 10 /
      data  pentries(1, 2), lpentries(1, 2) / 'particle entry', 14 /
      data  pentries(2, 2), lpentries(2, 2) / ' entries'      ,  8 /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Printing the different tables.
c
 2210 format(a, i4.4, 2a)
c
      do itab0 = 1, ntableprt
c
        itab = tableprt(itab0)
c
c       Selecting options: iopt(1) --> (0 plot br, 1 plot full,
c                                       2 table)
c                          iopt(2) --> (2 RMS err, 3 stddev.)
c                          iopt(3) --> (0 normal, 1 density, 2 ln,
c                                       3 log10)
c
        itmp1 = tprtopt(itab0)
        do i = 1, 3
          itmp2   = itmp1 / 11
          iopt(i) = itmp1 - 11 * itmp2
          itmp1   = itmp2
        enddo
c
c       Selecting the table type.
c
        if ((itab .lt. 1500) .or.
     +      ((itab .ge. 7000) .and. (itab .lt. 7500)))
     +  then
c
c         Longitudinal histograms: Particles.
c
c         Searching the kind-specific index, and getting data in the
c         cases of normal or unweighted tables.
c
          if (itab .lt. 1300) then
c
c           Normal tables.
c
            call gettcode(itab, nlhtables, lhcoden, jtab)
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = lhistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = lhnamen(jtab)
            ipe      = 1
c
          else if (itab .lt. 1500) then
c
c           Unweighted tables.
c
            call gettcode(itab, nlhtables, wlhcoden, jtab)
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = wlhistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = wlhnamen(jtab)
            ipe      = 2
c
          else if (itab .lt. 7300) then
c
c           Low energy particles tables.
c
            call gettcode(itab, nlitables, llcoden, jtab)
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = llhistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = llnamen(jtab)
            ipe      = 1
c
          else
c
c           Unweighted low energy particle tables.
c
            call gettcode(itab, nlhtables, wllcoden, jtab)
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = wllhistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = wllnamen(jtab)
            ipe      = 2
c
          endif
c
          linbinca = 1.d0 / obslevca
          linbincb = - obslevcb / obslevca
c
          write(line, 2210) 'TABLE ', itab, ': ', auxtname
          nsryhead = 0
          call putsry(1, line)
          write(line, 2210) 'Table ', itab, ' (continued)'
          call setsryhead(.false., line)
c
          call tprintset(1, nobslevels, 5, auxtablel,
     +                   iopt(1), iopt(2), 80, fltscale, 'Depth g/cm2',
     +                   1, 2, 2, sryauxstr1, slen1, tmp1)
c
          if (tmp1 .ne. 1) then
c
c           A particle scaling factor is needed.
c
            line = '  All ' //
     +             pentries(1, ipe)(1:lpentries(1, ipe)) //
     +             ' numbers are expressed in units of ' //
     +             sryauxstr1(1:slen1) //
     +             pentries(2, ipe)(1:lpentries(2, ipe)) // '.'
            call putsry(-1, line)
c
          else
            call putsry(0, ' ')
          endif
c
          call tprinthead(line)
c
          call plothisto(1, nobslevels, 5, auxtablel,
     +                   linbin, 1.d0, line)
c
        else if ((itab .lt. 2000) .or.
     +           ((itab .ge. 7500) .and. (itab .lt. 8000)))
     +  then
c
c         Longitudinal histograms: Energy.
c
c         Searching the kind-specific index, and getting data in the
c         cases of normal, low, or deposited energy tables.
c
          if (itab .lt. 2000) then
c
c           Normal tables.
c
            call gettcode(itab, nlhtables, lhcodee, jtab)
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = lhiste(j, i, jtab)
              enddo
            enddo
c
            auxtname = lhnamee(jtab)
c
          else if (itab .lt. 7800) then
c
c           Energy of lowe particles tables.
c
            call gettcode(itab, nlitables, llcodee, jtab)
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = llhiste(j, i, jtab)
              enddo
            enddo
c
            auxtname = llnamee(jtab)
c
          else
c
c           Depoasited energy tables.
c
            call gettcode(itab, nlitables, licodee, jtab)
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = lihiste(j, i, jtab)
              enddo
            enddo
c
            auxtname = linamee(jtab)
c
          endif
c
          linbinca = 1.d0 / obslevca
          linbincb = - obslevcb / obslevca
c
          write(line, 2210) 'TABLE ', itab, ': ', auxtname
          nsryhead = 0
          call putsry(1, line)
          write(line, 2210) 'Table ', itab, ' (continued)'
          call setsryhead(.false., line)
c
          call tprintset(1, nobslevels, 5, auxtablel,
     +                   iopt(1), iopt(2), 80, escale, 'Depth g/cm2',
     +                   1, 4, 1, sryauxstr1, slen1, tmp1)
c
          line = '  All energies are expressed in ' //
     +           sryauxstr1(1:slen1) // '.'
          call putsry(-1, line)
c
          call tprinthead(line)
c
          call plothisto(1, nobslevels, 5, auxtablel,
     +                   linbin, 1.d0, line)
c
        else if (itab .lt. 2500) then
c
c         Lateral distribution histograms: Radial.
c
c         Searching the kind-specific index, and getting data in the
c         cases of normal or unweighted tables.
c
          if (itab .lt. 2300) then
c
c           Normal tables.
c
            call gettcode(itab, nldtables, ldcoden, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = rthistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = ldnamen(jtab)
            ipe      = 1
c
          else
c
c           Unweighted tables.
c
            call gettcode(itab, nldtables, wldcoden, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = wrthistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = wldnamen(jtab)
            ipe      = 2
c
          endif
c
          logbinca = factrthni
          logbincb = rhni0
c
          write(line, 2210) 'TABLE ', itab, ': ', auxtname
          nsryhead = 0
          call putsry(1, line)
          write(line, 2210) 'Table ', itab, ' (continued)'
          call setsryhead(.false., line)
c
          if (iopt(3) .eq. 0) then
c
c           Raw distributions (particles).
c
            itmp1 = 2
c
          else if (iopt(3) .eq. 1) then
c
c           Density distributions (particles/m2).
c
            call putsry(9, '            (Density distribution)')
c
            call dstransf(5, nttabinsp1, auxtable, auxtable)
c
            itmp1 = -4
c
          else if (iopt(3) .eq. 2) then
c
c           dN/dln(R) distributions.
c
            call putsry(9, '            (dN/dln(R) distribution)')
c
            call lgtransf(5, nttabinsp1, .false., auxtable, auxtable)
c
            itmp1 = -4
c
          else
c
c           dN/dlog10(R) distributions.
c
            call putsry(9, '            (dN/dlog10(R) distribution)')
c
            call lgtransf(5, nttabinsp1, .true., auxtable, auxtable)
c
            itmp1 = -4
c
          endif
c
          call tprintset(1, nttabins, 5, auxtable(1, 1),
     +                   iopt(1), iopt(2), 80, fltscale, 'R (m)',
     +                   1, itmp1, 1, sryauxstr1, slen1, tmp1)
c
          if (tmp1 .ne. 1) then
c
c           A particle scaling factor is needed.
c
            if (iopt(3) .eq. 1) then
              line = '  All densities are expressed in units of ' //
     +               sryauxstr1(1:slen1) //
     +               pentries(2, ipe)(1:lpentries(2, ipe)) // '/m2.'
            else
              line = '  All ' //
     +               pentries(1, ipe)(1:lpentries(1, ipe)) //
     +               ' numbers are expressed in units of ' //
     +               sryauxstr1(1:slen1) //
     +               pentries(2, ipe)(1:lpentries(2, ipe)) // '.'
            endif
            call putsry(-1, line)
c
          else if (iopt(3) .eq. 1) then
c
            line = '  All densities are expressed in units of' //
     +             pentries(2, ipe)(1:lpentries(2, ipe)) // '/m2.'
            call putsry(-1, line)
c
          else
            call putsry(0, ' ')
          endif
c
          call tprinthead(line)
c
          call plothisto(1, nttabins, 5, auxtable(1, 1),
     +                   logbin, 1.d0, line)
          if (iopt(3) .le. 1) then
            call plotaltline(0, '  <', 1, auxtable(1, 0),
     +                       logbin, 1.d0, line)
          endif
          if (iopt(3) .eq. 0) then
            call plotaltline(9, '  >', nttabinsp1,
     +                       auxtable(1, nttabinsp1),
     +                       logbin, 1.d0, line)
          endif
c
        else if (itab .lt. 3000) then
c
c         Energy distributions histograms.
c
c         Searching the kind-specific index.
c
c         Searching the kind-specific index, and getting data in the
c         cases of normal or unweighted tables.
c
          if (itab .lt. 2800) then
c
c           Normal tables.
c
            call gettcode(itab, nldtables, ldcodee, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = rthiste(j, i, jtab)
              enddo
            enddo
c
            auxtname = ldnamee(jtab)
            ipe      = 1
c
          else
c
c           Unweighted tables.
c
            call gettcode(itab, nldtables, wldcodee, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = wrthiste(j, i, jtab)
              enddo
            enddo
c
            auxtname = wldnamee(jtab)
            ipe      = 2
c
          endif
c
          logbinca = factrthei
          logbincb = rhei0
c
          call escale(emaxhis, sryauxstr2, slen2, scald, scali)
          do i = slen2, 1, -1
            sryauxstr2(i+3:i+3) = sryauxstr2(i:i)
          enddo
          sryauxstr2(1:3) = 'E ('
          slen2 = slen2 + 4
          sryauxstr2(slen2:slen2) = ')'
c
          write(line, 2210) 'TABLE ', itab, ': ', auxtname
          nsryhead = 0
          call putsry(1, line)
          write(line, 2210) 'Table ', itab, ' (continued)'
          call setsryhead(.false., line)
c
          if (iopt(3) .eq. 2) then
c
c           dN/dln(E) distributions.
c
            call putsry(9, '            (dN/dln(E) distribution)')
c
            call lgtransf(5, nttabinsp1, .false., auxtable, auxtable)
c
          else if (iopt(3) .eq. 3) then
c
c           dN/dlog10(E) distributions.
c
            call putsry(9, '            (dN/dlog10(E) distribution)')
c
            call lgtransf(5, nttabinsp1, .true., auxtable, auxtable)
c
          endif
c
          call tprintset(1, nttabins, 5, auxtable(1, 1),
     +                   iopt(1), iopt(2), 80, fltscale,
     +                   sryauxstr2(1:slen2), -4, 2, 1,
     +                   sryauxstr1, slen1, tmp1)
c
          if (tmp1 .ne. 1) then
c
c           A particle scaling factor is needed.
c
            line = '  All ' //
     +             pentries(1, ipe)(1:lpentries(1, ipe)) //
     +             ' numbers are expressed in units of ' //
     +             sryauxstr1(1:slen1) //
     +             pentries(2, ipe)(1:lpentries(2, ipe)) // '.'
            call putsry(-1, line)
c
          else
            call putsry(0, ' ')
          endif
c
          call tprinthead(line)
c
          call plothisto(1, nttabins, 5, auxtable(1, 1),
     +                   logbin, scald, line)
          call plotaltline(0, '  <', 1, auxtable(1, 0),
     +                     logbin, scald, line)
          call plotaltline(9, '  >', nttabinsp1,
     +                     auxtable(1, nttabinsp1), logbin,
     +                     scald, line)
c
        else if (itab .lt. 4000) then
c
c         Time distribution tables.
c
c         Searching the kind-specific index.
c
          call gettcode(itab, ntdtables, tdcoden, jtab)
c
          logbinca = factrthni
          logbincb = rhni0
c
          write(line, 2210) 'TABLE ', itab, ': ', tdnamen(jtab)
          nsryhead = 0
          call putsry(1, line)
          write(line, 2210) 'Table ', itab, ' (continued)'
          call setsryhead(.false., line)
c
          call tprintset(1, nttabins, 5, rthistt(1, 1, jtab),
     +                   iopt(1), iopt(2), 80, fltscale, 'R (m)',
     +                   1, -4, 3, sryauxstr1, slen1, tmp1)
c
c         Time unit used.
c
          itmp1 = 1.d0 / tmp1 + 0.5d0
          if (itmp1 .eq. 1) then
            sryauxstr1 = 'nanoseconds.'
            slen1      = 12
          else if (itmp1 .eq. 1000) then
            sryauxstr1 = 'microseconds.'
            slen1      = 13
          else if (itmp1 .eq. 1000000) then
            sryauxstr1 = 'miliseconds.'
            slen1      = 12
          else
            sryauxstr1 = '???.'
            slen1      = 4
          endif
c
          line =
     +      '  All times are expressed in ' // sryauxstr1(1:slen1)
          call putsry(-1, line)
c
          call tprinthead(line)
c
c         Printing only the significative lines.
c
          do k = 1, nttabins
            if (rtsampl(k, jtab) .gt. 0) then
              call plotline(k, rthistt(1, k, jtab), logbin, 1.d0,
     +                      line)
            else
              call plotnulline(k, logbin, 1.d0, line)
            endif
          enddo
          if (rtsampl(0, jtab) .gt. 0) then
            call plotaltline(0, '  <', 1, rthistt(1, 0, jtab),
     +                       logbin, 1.d0, line)
          else
            call plotaltnulline(0, '  <', 1, logbin, 1.d0, line)
          endif
          if (rtsampl(nttabinsp1, jtab) .gt. 0) then
            call plotaltline(9, '  >', nttabinsp1,
     +                       rthistt(1, nttabinsp1, jtab), logbin,
     +                       1.d0, line)
          else
            call plotaltnulline(9, '  >', nttabinsp1, logbin,
     +                          1.d0, line)
          endif
c
        else
c
c         "Per shower" tables.
c
c         Searching the kind-specific index.
c
          call gettcode(itab, npshtables, pshcode, jtab)
c
          write(line, 2210) 'TABLE ', itab, ': ', pshname(jtab)
          nsryhead = 0
          call putsry(1, line)
          write(line, 2210) 'Table ', itab, ' (continued)'
          call setsryhead(.false., line)
          call setsryhead(.true., ' ')
c
          if (.not. saveshowerdata) then
            line = '  NO DATA AVAILABLE.'
            call putsry(-1, line)
            goto 1250
          endif
c
c         Opening the file that contains shower-per-shower data.
c
          open(shzut, file = shzfn, status = 'OLD',
     +         form = 'UNFORMATTED', err = 3020)
c
          read(shzut, err = 3020, end = 3020) i, klast, ngt,
     +                                        kfields, rfields
          if (pershowertables) call intfileskip(shzut, -99966)
c
          rfields = rfields - klast - 1
c
          j = i / 1000000
          i = i - 1000000 * j
          if (i .eq. 114) then
            knmax   = klast - 1
            kxmaxv  = klast - 2
          else
            knmax   = klast
            kxmaxv  = klast - 1
          endif
c
          i = firstshowernon1
c
          if (jtab .lt. ngt) then
c
c           Printing ground particles list (versus shower number).
c
            line = '  Shower number    Number of particles' //
     +             '     Energy     Unweighted entries'
            call putsry(-1, line)
            call putsry(9, ' ')
            call setsryhead(.true., line)
            call setsryhead(.true., ' ')
c
            jtab1 = 3 * ngt - 1
c
 1200       continue
            read(shzut, err = 3020, end = 1240)
     +                  (ipars(l), l = 1, kfields),
     +                  (xpars(l), l = 1, klast), sumofsq,
     +                  (auxtable2(j), j = 1, rfields)
            if (pershowertables) call intfileskip(shzut, -99966)
            i = i + 1
c
            write(line, 2460) i,
     +                        (auxtable2(jtab + j), j = 0, jtab1, ngt)
            call putsry(9, line)
            goto 1200
c
          else
c
            i1 = 3 * ngt + 1
            i2 = i1 + 4
            do j = rfields + 1, i2
              auxtable2(j) = 0
            enddo
c
            jtab1 = jtab - ngt
c
            if (jtab1 .eq. 1) then
c
c             Printing Xmax and Nmax (versus shower number).
c
              line = '  Shower number    Depth of max. (Xmax)  ' //
     +               '  Charged pcles at max. (Nmax)'
              call putsry(-1, line)
              call putsry(9, ' ')
              call setsryhead(.true., line)
              call setsryhead(.true., ' ')
c
 1210         continue
              read(shzut, err = 3020, end = 1240)
     +                    (ipars(l), l = 1, kfields),
     +                    (xpars(l), l = 1, klast), sumofsq,
     +                    (auxtable2(j), j = 1, rfields)
              if (pershowertables) call intfileskip(shzut, -99966)
              i = i + 1
c
              if (fitrc .eq. 0) then
                write(line, 2470) i, xpars(kxmaxv), xpars(knmax)
              else
                write(line, 2480) i
              endif
              call putsry(9, line)
              goto 1210
c
            else if (jtab1 .eq. 2) then
c
c             Printing X1 and Eprim (versus shower number).
c
              line = '  Shower number    Depth of first int.  ' //
     +               '       Primary Energy (GeV)'
              call putsry(-1, line)
              call putsry(9, ' ')
              call setsryhead(.true., line)
              call setsryhead(.true., ' ')
c
              i2 = i1 + 1
c
 1220         continue
              read(shzut, err = 3020, end = 1240)
     +                    (ipars(l), l = 1, kfields),
     +                    (xpars(l), l = 1, klast), sumofsq,
     +                    (auxtable2(j), j = 1, rfields)
              if (pershowertables) call intfileskip(shzut, -99966)
              i = i + 1
c
              write(line, 2470) i, auxtable2(i1), auxtable2(i2)
              call putsry(9, line)
              goto 1220
c
            else
c
c             Printing zenith and azimuth angles (versus shower
c             number).
c
              line = '  Shower number      Zenith angle (deg)' //
     +               '    Azimuth angle (deg)    Primary code'
              call putsry(-1, line)
              call putsry(9, ' ')
              call setsryhead(.true., line)
              call setsryhead(.true., ' ')
c
              i1 = i1 + 3
              i2 = i1 + 1
c
 1230         continue
              read(shzut, err = 3020, end = 1240)
     +                    (ipars(l), l = 1, kfields),
     +                    (xpars(l), l = 1, klast), sumofsq,
     +                    (auxtable2(j), j = 1, rfields)
              if (pershowertables) call intfileskip(shzut, -99966)
              i = i + 1
c
              write(line, 2472) i, auxtable2(i1), auxtable2(i2),
     +                          shprcode
              call putsry(9, line)
              goto 1230
c
            endif
          endif
 2460     format(i11, 1p, g23.5, 2g18.5)
 2470     format(i11, 1p, g24.5, g27.5)
 2472     format(i11, 1p, g26.5, g23.5, i15)
 2480     format(i11, 1p, 14x, '....', 23x, '....')
c
 1240     continue
          close(shzut)
          if (i .lt. lastshower) then
            call errprint(0, '$A30', 3, 'tprints',
     +      '("Per shower" data file. Premature end of file reached)',
     +      1, shzut, 0, 0.d0, ' ')
          endif
c
 1250     continue
c
        endif
      enddo
c
      nsryhead = 0
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A09', 4, 'tprints',
     +              auxfilestring(1:flen1),
     +              1, 8, 0, 0.d0, ' ')
      return
c
 3020 continue
c
      call errprint(0, '$A30', 4, 'tprints',
     +              ' ', 1, shzut, 0, 0.d0, ' ')
c
      end
c     --- End of routine tprints.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine texports
c
c     Exporting output tables
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999;
c                                La Plata 2000, 2001, 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      external          logbin
      double precision  logbin
      integer           i, j, l, i1, i2
      integer           flen1
      integer           klast, knmax, kxmax, kxmaxv, kxmaxs
      integer           kx0, klambda
      integer           ipars(5)
      integer           fitrc, shprcode
      equivalence       (fitrc, ipars(1))
      equivalence       (shprcode, ipars(2))
      double precision  xpars(5), sumofsq
      integer           kfields, rfields
      integer           itmp1, itmp2, ngt
      double precision  auxtable(5, 0:tdbinsp1)
      double precision  auxtablel(5, mxobslevelsp1)
      double precision  auxtable2(5 * mxobslevelsp1)
      equivalence       (obslevscra0, auxtable, auxtablel, auxtable2)
      double precision  oldepths0(mxobslevelsp1)
      double precision  obslevslant(mxobslevelsp1)
      equivalence       (obslevscra0(1, 6), oldepths0)
      equivalence       (obslevscra0(1, 7), obslevslant)
      character*64      auxtname
      character*3       grdgt
      integer           igrdgt
      double precision  fener, ftmp1
      integer           itab0, itab, jtab, jtab1
      integer           iopt(5)
      character*1       cmmchar, trchar
      logical           fhead, thislant, thisnorm, thisnormgt
c
c     FIRST EXECUTABLE STATEMENT
c
c     Exporting tables is always done together with the summary.
c
c     Checking for multiple exports of the same table.
c
      call multiexport
c
 2010 format(4a)
c
      cmmchar   = char(icommentchar)
      flen1     = leadingfnlen(3) + 6
      xslantcal = .true.
c
      do itab0 = 1, ntableexp
c
        itab = tableexp(itab0)
c
c       File name.
c
        if (expmultidx(itab0) .eq. expmultich0) then
          trchar = ' '
        else
          trchar = char(expmultidx(itab0))
        endif
c
        write(auxfilestring, 2310) leadingfn(3)(1:leadingfnlen(3)),
     +                             '.t', itab, trchar
 2310   format(2a, i4.4, a)
c
c       Selecting options: iopt(1) --> (0 no header, 1 commented head)
c                          iopt(2) --> (0 no "border" bins,
c                                       1 commented, 2 not commented.)
c                          iopt(3) --> (0 normal (raw) data,
c                                       1 density distributions,
c                                       2 dN/dln distributions,
c                                       3 dN/dlog10 disributions,
c                                       4 slant depths).
c                          iopt(4) --> (0 to 5, KeV, MeV, TeV, PeV,
c                                       EeV)
c                          iopt(5) --> (0 horiz. obs. lev.,
c                                       1 normal obs. planes.)
c
        itmp1 = texpopt(itab0)
        do i = 1, 5
          itmp2   = itmp1 / 11
          iopt(i) = itmp1 - 11 * itmp2
          itmp1   = itmp2
        enddo
c
        fhead = (iopt(1) .gt. 0)
        fener = 1000.d0 ** (2 - iopt(4))
        iopt(4) = iopt(4) + 2
c
c       Selecting the table type.
c
        if ((itab .lt. 2000) .or.
     +      (itab .ge. 7000) .and. (itab .lt. 8000))
     +  then
c
c         Longitudinal histograms.
c
          thislant   = (iopt(3) .eq. 4)
          thisnorm   = (iopt(5) .eq. 1)
          thisnormgt = (.not. thisnorm)
          grdgt      = 'GRD'
          igrdgt     = nobslevelsp1
c
          if (thislant) then
c
            if (xslantcal) then
              call olv2slant(nobslevelsp1, obslevdepth, 0.0d0,
     +                       varzendis, pryzenithmin, pryzenithmax,
     +                       groundz, obslevslant)
              xslantcal = .false.
            endif
c
            do i = 1, nobslevelsp1
              oldepths0(i) = obslevslant(i)
            enddo
c
          else
c
            do i = 1, nobslevelsp1
              oldepths0(i) = obslevdepth(i)
            enddo
c
          endif
c
c         Searching the kind-specific index, and getting data in the
c         cases of normal, unweighted or energy tables.
c
          if (itab .lt. 1300) then
c
c           Normal tables.
c
            call gettcode(itab, nlhtables, lhcoden, jtab)
c
            auxtname = lhnamen(jtab)
            if (thisnorm) jtab = jtab + mxlhtable
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = lhistn(j, i, jtab)
              enddo
            enddo
c
          else if (itab .lt. 1500) then
c
c           Unweighted tables.
c
            call gettcode(itab, nlhtables, wlhcoden, jtab)
c
            auxtname = wlhnamen(jtab)
            if (thisnorm) jtab = jtab + mxlhtable
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = wlhistn(j, i, jtab)
              enddo
            enddo
c
          else if (itab .lt. 2000) then
c
c           Energy tables.
c
            call gettcode(itab, nlhtables, lhcodee, jtab)
c
            auxtname = lhnamee(jtab)
c
c           Notice that option 'p' has no effect here.
c
            thisnorm   = .false.
            thisnormgt = .true.
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = fener * lhiste(j, i, jtab)
              enddo
            enddo
c
          else if (itab .lt. 7300) then
c
c           Number of lowe particles tables.
c
            call gettcode(itab, nlitables, llcoden, jtab)
c
            grdgt      = ' GT'
            igrdgt     = nobslevels
            thisnormgt = .true.
            auxtname   = llnamen(jtab)
            if (thisnorm) jtab = jtab + mxlitable
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = llhistn(j, i, jtab)
              enddo
            enddo
c
          else if (itab .lt. 7500) then
c
c           Unweighted number of lowe particles tables.
c
            call gettcode(itab, nlitables, wllcoden, jtab)
c
            grdgt      = ' GT'
            igrdgt     = nobslevels
            thisnormgt = .true.
            auxtname   = wllnamen(jtab)
            if (thisnorm) jtab = jtab + mxlitable
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = wllhistn(j, i, jtab)
              enddo
            enddo
c
          else if (itab .lt. 7800) then
c
c           Energy of low-e particles tables.
c
            call gettcode(itab, nlitables, llcodee, jtab)
c
            grdgt      = ' GT'
            igrdgt     = nobslevels
            thisnormgt = .true.
            auxtname   = llnamee(jtab)
            if (thisnorm) jtab = jtab + mxlitable
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = fener * llhiste(j, i, jtab)
              enddo
            enddo
c
          else
c
c           Deposited energy tables.
c
            call gettcode(itab, nlitables, licodee, jtab)
c
            grdgt      = ' GT'
            igrdgt     = nobslevels
            thisnormgt = .true.
            auxtname   = linamee(jtab)
c
c           Notice that option 'p' has no effect here.
c
            thisnorm = .false.
c
            do i = 1, nobslevelsp1
              do j = 1, 5
                auxtablel(j, i) = fener * lihiste(j, i, jtab)
              enddo
            enddo
c
          endif
c
c         The table exists, opening the file.
c
          open(8, file = auxfilestring, status = 'UNKNOWN',
     +         access = 'SEQUENTIAL', err = 3010)
c
 2320     format(i5, 1p, g14.6, g15.6, 4g11.3)
 2330     format(a1, a4, 1p, g14.6, g15.6, 4g11.3)
c
          if (fhead) then
            if (thislant) then
              if (thisnorm) then
                call exporthead(8, .false., 0, itab, auxtname,
     +            1, 'Reference planes normal to the shower axis.',
     +            'Slant depth of obs. level', iopt(4))
              else
                call exporthead(8, .false., 0, itab, auxtname,
     +                          0, ' ', 'Slant depth of obs. level',
     +                          iopt(4))
              endif
            else
              if (thisnorm) then
                call exporthead(8, .false., 0, itab, auxtname,
     +            1, 'Reference planes normal to the shower axis.',
     +            'Depth of obs. level', iopt(4))
              else
                call exporthead(8, .false., 0, itab, auxtname,
     +                          0, ' ', 'Depth of obs. level',
     +                          iopt(4))
              endif
            endif
          endif
c
          if ((iopt(2) .eq. 1) .and. thisnormgt) then
            write(8, 2010, err = 3010) cmmchar
            write(8, 2330, err = 3010) cmmchar, grdgt,
     +                     oldepths0(igrdgt),
     +                     (auxtablel(j, nobslevelsp1), j = 1, 5)
            write(8, 2010, err = 3010) cmmchar
          endif
c
          do i = 1, nobslevels
            write(8, 2320, err = 3010) i, oldepths0(i),
     +                     (auxtablel(j, i), j = 1, 5)
          enddo
c
          if ((iopt(2) .eq. 2) .and. thisnormgt) then
            write(8, 2320, err = 3010) nobslevelsp1,
     +                     oldepths0(nobslevelsp1),
     +                     (auxtablel(j, nobslevelsp1), j = 1, 5)
          endif
c
        else if (itab .lt. 2500) then
c
c         Lateral distribution histograms: Radial.
c
c         Searching the kind-specific index, and getting data in the
c         cases of normal or unweighted tables.
c
          if (itab .lt. 2300) then
c
c           Normal tables.
c
            call gettcode(itab, nldtables, ldcoden, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = rthistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = ldnamen(jtab)
c
          else
c
c           Unweighted tables.
c
            call gettcode(itab, nldtables, wldcoden, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = wrthistn(j, i, jtab)
              enddo
            enddo
c
            auxtname = wldnamen(jtab)
c
          endif
c
c         The table exists, opening the file.
c
          open(8, file = auxfilestring, status = 'UNKNOWN',
     +         access = 'SEQUENTIAL', err = 3010)
c
          logbinca = factrthni
          logbincb = rhni0
c
          if (iopt(3) .eq. 0) then
c
c           Raw distributions (particles).
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, auxtname, 0, ' ',
     +                        'R (distance to the core)', iopt(4))
            endif
c
          else if (iopt(3) .eq. 1) then
c
c           Density distributions (particles/m2).
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, auxtname, 1,
     +                        'Density distribution (particles/m2).',
     +                        'R (distance to the core)', iopt(4))
            endif
c
            call dstransf(5, nttabinsp1, auxtable, auxtable)
c
          else if (iopt(3) .eq. 2) then
c
c           dN/dln(R) distributions.
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, auxtname, 1,
     +                        'dN/dln(R) distribution.',
     +                        'R (distance to the core)', iopt(4))
            endif
c
            call lgtransf(5, nttabinsp1, .false., auxtable, auxtable)
c
          else
c
c           dN/dlog10(R) distributions.
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, auxtname, 1,
     +                        'dN/dlog10(R) distribution.',
     +                        'R (distance to the core)', iopt(4))
            endif
c
            call lgtransf(5, nttabinsp1, .true., auxtable, auxtable)
c
          endif
c
          itmp1 = 1
          itmp2 = nttabins
          if (iopt(2) .eq. 1) then
            write(8, 2010, err = 3010) cmmchar
            write(8, 2330, err = 3010) cmmchar, '  <', logbin(1),
     +                     (auxtable(j, 0), j = 1, 5)
            write(8, 2330, err = 3010) cmmchar, '  >',
     +                     logbin(nttabinsp1),
     +                     (auxtable(j, nttabinsp1), j = 1, 5)
            write(8, 2010, err = 3010) cmmchar
          else if (iopt(2) .eq. 2) then
            itmp1 = 0
            itmp2 = nttabinsp1
          endif
c
          do i = itmp1, itmp2
            write(8, 2320, err = 3010) i, logbin(max(1, i)),
     +                     (auxtable(j, i), j = 1, 5)
          enddo
c
        else if (itab .lt. 3000) then
c
c         Energy distributions histograms.
c
c         Searching the kind-specific index, and getting data in the
c         cases of normal or unweighted tables.
c
          if (itab .lt. 2800) then
c
c           Normal tables.
c
            call gettcode(itab, nldtables, ldcodee, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = rthiste(j, i, jtab)
              enddo
            enddo
c
            auxtname = ldnamee(jtab)
c
          else
c
c           Unweighted tables.
c
            call gettcode(itab, nldtables, wldcodee, jtab)
c
            do i = 0, nttabinsp1
              do j = 1, 5
                auxtable(j, i) = wrthiste(j, i, jtab)
              enddo
            enddo
c
            auxtname = wldnamee(jtab)
c
          endif
c
c         The table exists, opening the file.
c
          open(8, file = auxfilestring, status = 'UNKNOWN',
     +         access = 'SEQUENTIAL', err = 3010)
c
          logbinca = factrthei
          logbincb = rhei0
c
          if (iopt(3) .le. 1) then
c
c           Raw distributions (particles).
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, auxtname, 0, ' ',
     +                        'Energy', iopt(4))
            endif
c
          else if (iopt(3) .eq. 2) then
c
c           dN/dln(E) distributions.
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, auxtname, 1,
     +                        'dN/dln(E) distribution.',
     +                        'Energy', iopt(4))
            endif
c
            call lgtransf(5, nttabinsp1, .false., auxtable, auxtable)
c
          else
c
c           dN/dlog10(E) distributions.
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, auxtname, 1,
     +                        'dN/dlog10(E) distribution.',
     +                        'Energy', iopt(4))
            endif
c
            call lgtransf(5, nttabinsp1, .true., auxtable, auxtable)
c
          endif
c
          itmp1 = 1
          itmp2 = nttabins
          if (iopt(2) .eq. 1) then
            write(8, 2010, err = 3010) cmmchar
            write(8, 2330, err = 3010) cmmchar, '  <',
     +                     fener * logbin(1),
     +                     (auxtable(j, 0), j = 1, 5)
            write(8, 2330, err = 3010) cmmchar, '  >',
     +                     fener * logbin(nttabinsp1),
     +                     (auxtable(j, nttabinsp1), j = 1, 5)
            write(8, 2010, err = 3010) cmmchar
          else if (iopt(2) .eq. 2) then
            itmp1 = 0
            itmp2 = nttabinsp1
          endif
c
          do i = itmp1, itmp2
            write(8, 2320, err = 3010) i, fener * logbin(max(1, i)),
     +                     (auxtable(j, i), j = 1, 5)
          enddo
c
        else if (itab .lt. 4000) then
c
c         Time distribution tables.
c
c         Searching the kind-specific index.
c
          call gettcode(itab, ntdtables, tdcoden, jtab)
c
c         The table exists, opening the file.
c
          open(8, file = auxfilestring, status = 'UNKNOWN',
     +         access = 'SEQUENTIAL', err = 3010)
c
          if (fhead) call exporthead(8, .false., 0, itab, tdnamen(jtab),
     +                               0, ' ',
     +                               'R (distance to the core)',
     +                               iopt(4))
c
          logbinca = factrthni
          logbincb = rhni0
c
          itmp1 = 1
          itmp2 = nttabins
          if (iopt(2) .eq. 1) then
c
            write(8, 2010, err = 3010) cmmchar
            if (rtsampl(0, jtab) .gt. 0) then
              write(8, 2330, err = 3010) cmmchar, '  <', logbin(1),
     +                       (rthistt(j, 0, jtab), j = 1, 5)
            else
              write(8, 2350, err = 3010) cmmchar, '  <', logbin(1)
            endif
            if (rtsampl(nttabinsp1, jtab) .gt. 0) then
              write(8, 2330, err = 3010) cmmchar, '  >',
     +                       logbin(nttabinsp1),
     +                       (rthistt(j, nttabinsp1, jtab), j = 1, 5)
            else
              write(8, 2350, err = 3010) cmmchar, '  <',
     +                       logbin(nttabinsp1)
            endif
            write(8, 2010, err = 3010) cmmchar
c
          else if (iopt(2) .eq. 2) then
            itmp1 = 0
            itmp2 = nttabinsp1
          endif
c     
 2340     format(i5, 1p, g14.6, 7x, '....', 3x, 4(4x, '....', 3x))
 2350     format(a1, a4, 1p, g14.6, 7x, '....', 3x, 4(4x, '....', 3x))
c
          do i = itmp1, itmp2
            if (rtsampl(i, jtab) .gt. 0) then
              write(8, 2320, err = 3010) i, logbin(max(1, i)),
     +                       (rthistt(j, i, jtab), j = 1, 5)
             else
              write(8, 2340, err = 3010) i, logbin(i)
            endif
          enddo
c
        else
c
c         "Per shower" tables.
c
c         Searching the kind-specific index.
c
          call gettcode(itab, npshtables, pshcode, jtab)
c
c         The table exists, opening the file.
c
          open(8, file = auxfilestring, status = 'UNKNOWN',
     +         access = 'SEQUENTIAL', err = 3010)
c
c         Opening the file that contains shower-per-shower data.
c
          open(shzut, file = shzfn, status = 'OLD',
     +         form = 'UNFORMATTED', err = 3020)
c
          read(shzut, err = 3020, end = 3020) i, klast, ngt,
     +                                        kfields, rfields
          if (pershowertables) call intfileskip(shzut, -99966)
c
          rfields = rfields - klast - 1
c
          j       = i / 1000000
          i       = i - 1000000 * j
          kx0     = 1
          klambda = 2
          if (i .eq. 114) then
            knmax   = klast - 1
            kxmaxv  = klast - 2
            kxmaxs  = klast
          else
            knmax   = klast
            kxmaxv  = klast - 1
            kxmaxs  = kxmaxv
            if (i .eq. 103) then
              klambda  = 5
              xpars(5) = 70
            endif
          endif
c
          thislant = (iopt(3) .eq. 4)
c
          i = firstshowernon1
c
          if (jtab .le. ngt) then
c
c           Exporting ground particles list (versus shower number).
c
            if (fhead) then
              call exporthead(8, .false., 0, itab, pshname(jtab),
     +        0, ' ',
     +        'Number of pcles., Energy, Unweighted$particle entries.',
     +        iopt(4))
            endif
c
            if (.not. saveshowerdata) then
              write(8, 2010, err = 3010) cmmchar,
     +                                   '        NO DATA AVAILABLE.'
              goto 1140
            endif
c
            jtab1 = 3 * ngt - 1
c
 1100       continue
            read(shzut, err = 3020, end = 1140)
     +                  (ipars(l), l = 1, kfields),
     +                  (xpars(l), l = 1, klast), sumofsq,
     +                  (auxtable2(j), j = 1, rfields)
            if (pershowertables) call intfileskip(shzut, -99966)
            i = i + 1
c
            write(8, 2360, err = 3010) i,
     +                     (auxtable2(jtab + j), j = 0, jtab1, ngt)
            goto 1100
c
          else
c
            i1 = 3 * ngt + 1
            i2 = i1 + 3
            do j = rfields + 1, i2
              auxtable2(j) = 0
            enddo
c
            jtab1 = jtab - ngt
c
            if (jtab1 .eq. 1) then
c
c             Exporting Xmax and Nmax (versus shower number).
c
              if (fhead) then
                if (thislant) then
                  call exporthead(8, .false., 0, itab,
     +              pshname(jtab), 1,
     +              '(SLANT atmospheric depths along shower axis)',
     +              'Xmax, Nmax, X0, lambda, SumOfSqr, Ret. code.',
     +              iopt(4))
                else
                  call exporthead(8, .false., 0, itab,
     +              pshname(jtab), 0, ' ',
     +              'Xmax, Nmax, X0, lambda, SumOfSqr, Ret. code.',
     +              iopt(4))
                endif
              endif
c
              if (.not. saveshowerdata) then
                write(8, 2010, err = 3010) cmmchar,
     +                                     '        NO DATA AVAILABLE.'
                goto 1140
              endif
c
              if (thislant) then
                kxmax = kxmaxs
              else
                kxmax = kxmaxv
              endif
c
 1110         continue
              read(shzut, err = 3020, end = 1140)
     +                    (ipars(l), l = 1, kfields),
     +                    (xpars(l), l = 1, klast), sumofsq,
     +                    (auxtable2(j), j = 1, rfields)
              if (pershowertables) call intfileskip(shzut, -99966)
              i = i + 1
c
              ftmp1 = xpars(kxmax) / xpars(kxmaxv)
c
              write(8, 2370, err = 3010) i,
     +              xpars(kxmax), xpars(knmax),
     +              ftmp1 * xpars(kx0), ftmp1 * xpars(klambda),
     +              sumofsq, fitrc
              goto 1110
c
            else if (jtab1 .eq. 2) then
c
c             Exporting X1 and Eprim (versus shower number).
c
              if (fhead) then
                if (thislant) then
                  call exporthead(8, .false., 0, itab,
     +                 pshname(jtab), 0, ' ', 'X1 (slant), Eprim',
     +                 iopt(4))
                else
                  call exporthead(8, .false., 0, itab,
     +                 pshname(jtab), 0, ' ', 'X1, Eprim',
     +                 iopt(4))
                endif
              endif
c
              if (.not. saveshowerdata) then
                write(8, 2010, err = 3010) cmmchar,
     +                                     '        NO DATA AVAILABLE.'
                goto 1140
              endif
c
              i2 = i1 + 1
              if (thislant) i1 = i2 + 1
c
 1120         continue
              read(shzut, err = 3020, end = 1140)
     +                    (ipars(l), l = 1, kfields),
     +                    (xpars(l), l = 1, klast), sumofsq,
     +                    (auxtable2(j), j = 1, rfields)
              if (pershowertables) call intfileskip(shzut, -99966)
              i = i + 1
c
              write(8, 2360, err = 3010) i,
     +              auxtable2(i1), fener * auxtable2(i2)
              goto 1120
c
            else
c
c             Exporting zenith and azimuth angles
c             (versus shower number).
c
              if (fhead) then
                call exporthead(8, .false., 0, itab,
     +               pshname(jtab), 0, ' ',
     +               'zenith, azimuth, primcode',
     +               iopt(4))
              endif
c
              if (.not. saveshowerdata) then
                write(8, 2010, err = 3010) cmmchar,
     +                                     '        NO DATA AVAILABLE.'
                goto 1140
              endif
c
              i1 = i1 + 3
              i2 = i1 + 1
c
 1130         continue
              read(shzut, err = 3020, end = 1140)
     +                    (ipars(l), l = 1, kfields),
     +                    (xpars(l), l = 1, klast), sumofsq,
     +                    (auxtable2(j), j = 1, rfields)
              if (pershowertables) call intfileskip(shzut, -99966)
              i = i + 1
c
              write(8, 2362, err = 3010) i,
     +              auxtable2(i1), auxtable2(i2), shprcode
              goto 1130
c
            endif
          endif
 2360     format(i6, 1p, 3g15.6)
 2362     format(i6, 1p, 2g15.6, i7)
 2370     format(i6, 1p, 2g14.6, 3g12.4, i6)
c
 1140     continue
          close(shzut)
c
        endif
c
        if (iopt(1) .eq. 1) then
          write(8, 2010, err = 3010) cmmchar
        else
          write(8, *, err = 3010)
        endif
c
        close(8)
      enddo
c
c     EXPORTING SINGLE SHOWER TABLES.
c
c     NOTE: This will overwrite a part of the table arrays, so
c     this step should be executed only after having saved the
c     corresponding data.
c
      if (exportpershower) call texportshowers
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A09', 4, 'texports',
     +              auxfilestring(1:flen1),
     +              1, 8, 0, 0.d0, ' ')
      return
c
 3020 continue
c
      call errprint(0, '$A30', 4, 'texports',
     +              ' ', 1, shzut, 0, 0.d0, ' ')
c
      end
c     --- End of routine texports.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine texportshowers
c
c     Exporting single shower output tables.
c
c     WARNING: This routine partially overwrites the table arrays.
c
c     Written by: S. J. Sciutto, La Plata 1999; Fermilab 1999;
c                                La Plata 2000, 2001.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'hdatacomm.f'
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      external          logbin
      double precision  logbin
      integer           i, j, k, l, k0
      integer           flen1, nfam
      integer           i0, lshr, kfields, rfields, ishower, jshower
      logical           lgrp5, lfam4
      integer           itmp1, itmp2
      double precision  auxtable(0:tdbinsp1)
      equivalence       (obslevscra0, auxtable)
      double precision  auxtablel(mxobslevelsp1)
      equivalence       (obslevscra0, auxtablel)
      double precision  oldepths0(mxobslevelsp1)
      double precision  obslevslant(mxobslevelsp1)
      equivalence       (obslevscra0(1, 8), oldepths0)
      equivalence       (obslevscra0(1, 9), obslevslant)
      character*64      auxtname
      character*3       grdgt
      integer           igrdgt
      double precision  fener, prevzen, thiszen
      integer           itab0, itab, jtab, izen
      integer           iopt(5)
      character*1       cmmchar, trchar
      logical           fhead, thislant, thisnorm, thisnormgt
c
c     FIRST EXECUTABLE STATEMENT
c
c     Exporting tables is always done together with the summary,
c     and just after ending it.
c
c     Checking if the individual tables have been saved.
c
      if (.not. pershowertables) then
        if (ntableexp .gt. 0) then
          call errprint(0, '*', 2, 'texportshowers',
     +         'No individual shower tables available.$' //
     +         'Shower table export instruction ignored.',
     +         0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
c     Downloading the single shower tables.
c
c     Opening the corresponding internal file.
c
      open(shzut, file = shzfn, status = 'OLD',
     +            form = 'UNFORMATTED', err = 3020)
c
c     Reading the file header.
c
      read(shzut, err = 3020, end = 3020)
     +     i0, i, lshr, kfields, rfields, nfam,
     +     (obslevscrai(j), j = 22, 26)
c
      izen      = rfields - 1
      prevzen   = -1440
c
      j = 31
      do i = 1, nfam
        read(shzut, err = 3020, end = 3020)
     +               (obslevscrai(j + l), l = 1, 10)
        j = j + 10
      enddo
      read(shzut, err = 3020, end = 3020) i
c
c     SINGLE SHOWER PROCESSING.
c
 2010 format(4a)
c
      cmmchar = char(icommentchar)
      flen1   = leadingfnlen(3) + 12
c
c     Backwards compatibility:
c
c     1) Needs to determine if there are longitudinal tables for normal
c        observing levels.
c
      lgrp5 = (obslevscrai(31 + 2) .eq. 5)
c
c     2) Needs to determine if there are deposited energy and related
c        tables.
c
      lfam4 = (nfam .eq. 4)
c
      do ishower = 1, lastshower
        jshower = firstshowernon1 + ishower
c
c       Shower head.
c
        read(shzut, err = 3020, end = 3020)
     +       (obslevscraj(k), k = 1, kfields),
     +       (obslevscra1(k), k = 1, rfields)
        read(shzut, err = 3020, end = 3020)
     +       (obslevscrai(k), k = 1, 10)
        if (obslevscrai(1) .ne. ishower) goto 3020
c
        thiszen   = obslevscra1(izen)
        xslantcal = (thiszen .ne. prevzen)
c
c       Reading the shower tables.
c
c       Longitudinal histograms.
c
        do k = 1, nlhtables
c
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
          do i = 1, nobslevelsp1
            lhistn(2, i, k) = obslevscra4(i)
          enddo
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
          do i = 1, nobslevelsp1
            lhiste(2, i, k) = obslevscra4(i)
          enddo
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
          do i = 1, nobslevelsp1
            wlhistn(2, i, k) = obslevscra4(i)
          enddo
c
          if (lgrp5) then
            read(shzut, err = 3020, end = 3020)
     +                   (obslevscra4(i), i = 1, nobslevelsp1)
            do i = 1, nobslevelsp1
              lhistn(2, i, k + mxlhtable) = obslevscra4(i)
            enddo
            read(shzut, err = 3020, end = 3020)
     +                   (obslevscra4(i), i = 1, nobslevelsp1)
            do i = 1, nobslevelsp1
              wlhistn(2, i, k + mxlhtable) = obslevscra4(i)
            enddo
          else
            do i = 1, nobslevelsp1
              lhistn(2, i, k + mxlhtable)  = lhistn(2, i, k)
              wlhistn(2, i, k + mxlhtable) = wlhistn(2, i, k)
            enddo
          endif
c
        enddo
c
c       Lateral distribution histograms.
c
        do k = 1, nldtables
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            rthistn(2, i, k) = obslevscra4(i)
          enddo
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            rthiste(2, i, k) = obslevscra4(i)
          enddo
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            wrthistn(2, i, k) = obslevscra4(i)
          enddo
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            wrthiste(2, i, k) = obslevscra4(i)
          enddo
        enddo
c
c       Time distribution histograms.
c
        do k = 1, ntdtables
          read(shzut, err = 3020, end = 3020)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            rthistt(2, i, k) = obslevscra4(i)
          enddo
        enddo
c
c       Deposited energy and related histograms.
c
        if (lfam4) then
c
          do l = 0, 1
            do k0 = 1, nlitables
              k = k0 + l * mxlitable
c
              read(shzut, err = 3020, end = 3020)
     +                     (obslevscra4(i), i = 1, nobslevelsp1)
              do i = 1, nobslevelsp1
                llhistn(2, i, k) = obslevscra4(i)
              enddo
              read(shzut, err = 3020, end = 3020)
     +                     (obslevscra4(i), i = 1, nobslevelsp1)
              do i = 1, nobslevelsp1
                llhiste(2, i, k) = obslevscra4(i)
              enddo
              read(shzut, err = 3020, end = 3020)
     +                     (obslevscra4(i), i = 1, nobslevelsp1)
              do i = 1, nobslevelsp1
                wllhistn(2, i, k) = obslevscra4(i)
              enddo
c
            enddo
          enddo
c
          do k = 1, nlitables
            read(shzut, err = 3020, end = 3020)
     +                   (obslevscra4(i), i = 1, nobslevelsp1)
            do i = 1, nobslevelsp1
              lihiste(2, i, k) = obslevscra4(i)
            enddo
          enddo
c
        else
          do l = 0, 1
            do k0 = 1, nlitables
              k = k0 + l * mxlitable
              do i = 1, nobslevelsp1
                llhistn(2, i, k)  = 0
                llhiste(2, i, k)  = 0
                wllhistn(2, i, k) = 0
              enddo
            enddo
          enddo
c
          do k = 1, nlitables
            do i = 1, nobslevelsp1
              lihiste(2, i, k) = 0
            enddo
          enddo
        endif
c
        read(shzut, err = 3020, end = 3020) i
c
c       EXPORTING THE TABLES FOR THE CURRENT SHOWER.
c
        do itab0 = 1, ntableexp
c
          itab = tableexp(itab0)
c
c         File name.
c
          if (expmultidx(itab0) .eq. expmultich0) then
            trchar = ' '
          else
            trchar = char(expmultidx(itab0))
          endif
c
          if (jshower .lt. 10000) then
            write(auxfilestring, 2310) leadingfn(3)(1:leadingfnlen(3)),
     +                                 '_s', jshower,
     +                                 '.t', itab, trchar
          else
            write(auxfilestring, 2311) leadingfn(3)(1:leadingfnlen(3)),
     +                                 '_s', jshower,
     +                                 '.t', itab, trchar
          endif
 2310     format(a, 2(a, i4.4), a)
 2311     format(2a, i6.6, a, i4.4, a)
c
c         Selecting options: iopt(1) --> (0 no header, 1 commented head)
c                            iopt(2) --> (0 no "border" bins,
c                                         1 commented, 2 not commented.)
c                            iopt(3) --> (0 normal (raw) data,
c                                         1 density distributions,
c                                         2 dN/dln distributions,
c                                         3 dN/dlog10 disributions,
c                                         4 slant depths).
c                            iopt(4) --> (0 to 5, KeV, MeV, TeV, PeV,
c                                         EeV)
c                            iopt(5) --> (0 horiz. obs. lev.,
c                                         1 normal obs. planes.)
c
          itmp1 = texpopt(itab0)
          do i = 1, 5
            itmp2   = itmp1 / 11
            iopt(i) = itmp1 - 11 * itmp2
            itmp1   = itmp2
          enddo
c
          fhead = (iopt(1) .gt. 0)
          fener = 1000.d0 ** (2 - iopt(4))
          iopt(4) = iopt(4) + 2
c
c         Selecting the table type.
c
          if ((itab .lt. 2000) .or.
     +        (itab .ge. 7000) .and. (itab .lt. 8000))
     +    then
c
c           Longitudinal histograms.
c
            thislant   = (iopt(3) .eq. 4)
            thisnorm   = (iopt(5) .eq. 1)
            thisnormgt = (.not. thisnorm)
            grdgt      = 'GRD'
            igrdgt     = nobslevelsp1
c
            if (thislant) then
c
              if (xslantcal) then
                call olv2slant(nobslevelsp1, obslevdepth, 0.0d0,
     +                         0, thiszen, thiszen,
     +                         groundz, obslevslant)
                xslantcal = .false.
                prevzen   = thiszen
              endif
c
              do i = 1, nobslevelsp1
                oldepths0(i) = obslevslant(i)
              enddo
c
            else
c
              do i = 1, nobslevelsp1
                oldepths0(i) = obslevdepth(i)
              enddo
c
            endif
c
c           Searching the kind-specific index, and getting data in the
c           cases of normal, unweighted or energy tables.
c
            if (itab .lt. 1300) then
c
c             Normal tables.
c
              call gettcode(itab, nlhtables, lhcoden, jtab)
c
              auxtname = lhnamen(jtab)
              if (thisnorm) jtab = jtab + mxlhtable
c
              do i = 1, nobslevelsp1
                auxtablel(i) = lhistn(2, i, jtab)
              enddo
c
            else if (itab .lt. 1500) then
c
c             Unweighted tables.
c
              call gettcode(itab, nlhtables, wlhcoden, jtab)
c
              auxtname = wlhnamen(jtab)
              if (thisnorm) jtab = jtab + mxlhtable
c
              do i = 1, nobslevelsp1
                auxtablel(i) = wlhistn(2, i, jtab)
              enddo
c
            else if (itab .lt. 2000) then
c
c             Energy tables.
c
              call gettcode(itab, nlhtables, lhcodee, jtab)
c
              auxtname = lhnamee(jtab)
c
c             Notice that option 'p' has no effect here.
c
              thisnorm   = .false.
              thisnormgt = .true.
c
              do i = 1, nobslevelsp1
                auxtablel(i) = fener * lhiste(2, i, jtab)
              enddo
c
            else if (itab .lt. 7300) then
c
c             Number of lowe particles tables.
c
              call gettcode(itab, nlitables, llcoden, jtab)
c
              grdgt      = ' GT'
              igrdgt     = nobslevels
              thisnormgt = .true.
              auxtname   = llnamen(jtab)
              if (thisnorm) jtab = jtab + mxlitable
c
              do i = 1, nobslevelsp1
                auxtablel(i) = llhistn(2, i, jtab)
              enddo
c
            else if (itab .lt. 7500) then
c
c             Unweighted number of lowe particles tables.
c
              call gettcode(itab, nlitables, wllcoden, jtab)
c
              grdgt      = ' GT'
              igrdgt     = nobslevels
              thisnormgt = .true.
              auxtname   = wllnamen(jtab)
              if (thisnorm) jtab = jtab + mxlitable
c
              do i = 1, nobslevelsp1
                auxtablel(i) = wllhistn(2, i, jtab)
              enddo
c
            else if (itab .lt. 7800) then
c
c             Energy of low-e particles tables.
c
              call gettcode(itab, nlitables, llcodee, jtab)
c
              grdgt      = ' GT'
              igrdgt     = nobslevels
              thisnormgt = .true.
              auxtname   = llnamee(jtab)
              if (thisnorm) jtab = jtab + mxlitable
c
              do i = 1, nobslevelsp1
                auxtablel(i) = fener * llhiste(2, i, jtab)
              enddo
c
            else
c
c             Deposited energy tables.
c
              call gettcode(itab, nlitables, licodee, jtab)
c
              grdgt      = ' GT'
              igrdgt     = nobslevels
              thisnormgt = .true.
              auxtname   = linamee(jtab)
c
c             Notice that option 'p' has no effect here.
c
              thisnorm = .false.
c
              do i = 1, nobslevelsp1
                auxtablel(i) = fener * lihiste(2, i, jtab)
              enddo
c
            endif
c
c           The table exists, opening the file.
c
 2320       format(i5, 1p, g14.6, g15.6)
 2330       format(a1, a4, 1p, g14.6, g15.6)
c
            open(8, file = auxfilestring, status = 'UNKNOWN',
     +           access = 'SEQUENTIAL', err = 3010)
c
            if (fhead) then
              if (thislant) then
                if (thisnorm) then
                  call exporthead(8, .true., jshower, itab, auxtname,
     +              1, 'Reference planes normal to the shower axis.',
     +              'Slant depth of obs. level', iopt(4))
                else
                  call exporthead(8, .true., jshower, itab, auxtname,
     +                            0, ' ', 'Slant depth of obs. level',
     +                            iopt(4))
                endif
              else
                if (thisnorm) then
                  call exporthead(8, .true., jshower, itab, auxtname,
     +              1, 'Reference planes normal to the shower axis.',
     +              'Depth of obs. level', iopt(4))
                else
                  call exporthead(8, .true., jshower, itab, auxtname,
     +                            0, ' ', 'Depth of obs. level',
     +                            iopt(4))
                endif
              endif
            endif
c
            if ((iopt(2) .eq. 1) .and. thisnormgt) then
              write(8, 2010, err = 3010) cmmchar
              write(8, 2330, err = 3010) cmmchar, grdgt,
     +                       oldepths0(igrdgt),
     +                       auxtablel(nobslevelsp1)
              write(8, 2010, err = 3010) cmmchar
            endif
c
            do i = 1, nobslevels
              write(8, 2320, err = 3010) i, oldepths0(i),
     +                       auxtablel(i)
            enddo
c
            if ((iopt(2) .eq. 2) .and. thisnormgt) then
              write(8, 2320, err = 3010) nobslevelsp1,
     +                       oldepths0(nobslevelsp1),
     +                       auxtablel(nobslevelsp1)
            endif
c
          else if (itab .lt. 2500) then
c
c           Lateral distribution histograms: Radial.
c
c           Searching the kind-specific index, and getting data in the
c           cases of normal or unweighted tables.
c
            if (itab .lt. 2300) then
c
c             Normal tables.
c
              call gettcode(itab, nldtables, ldcoden, jtab)
c
              do i = 0, nttabinsp1
                auxtable(i) = rthistn(2, i, jtab)
              enddo
c
              auxtname = ldnamen(jtab)
c
            else
c
c             Unweighted tables.
c
              call gettcode(itab, nldtables, wldcoden, jtab)
c
              do i = 0, nttabinsp1
                auxtable(i) = wrthistn(2, i, jtab)
              enddo
c
              auxtname = wldnamen(jtab)
c
            endif
c
c           The table exists, opening the file.
c
            open(8, file = auxfilestring, status = 'UNKNOWN',
     +           access = 'SEQUENTIAL', err = 3010)
c
            logbinca = factrthni
            logbincb = rhni0
c
            if (iopt(3) .eq. 0) then
c
c             Raw distributions (particles).
c
              if (fhead) then
                call exporthead(8, .true., jshower,
     +                          itab, auxtname, 0, ' ',
     +                          'R (distance to the core)', iopt(4))
              endif
c
            else if (iopt(3) .eq. 1) then
c
c             Density distributions (particles/m2).
c
              if (fhead) then
                call exporthead(8, .true., jshower, itab, auxtname, 1,
     +                          'Density distribution (particles/m2).',
     +                          'R (distance to the core)', iopt(4))
              endif
c
              call dstransf(1, nttabinsp1, auxtable, auxtable)
c
            else if (iopt(3) .eq. 2) then
c
c             dN/dln(R) distributions.
c
              if (fhead) then
                call exporthead(8, .true., jshower, itab, auxtname, 1,
     +                          'dN/dln(R) distribution.',
     +                          'R (distance to the core)', iopt(4))
              endif
c
              call lgtransf(1, nttabinsp1, .false., auxtable, auxtable)
c
            else
c
c             dN/dlog10(R) distributions.
c
              if (fhead) then
                call exporthead(8, .true., jshower, itab, auxtname, 1,
     +                          'dN/dlog10(R) distribution.',
     +                          'R (distance to the core)', iopt(4))
              endif
c
              call lgtransf(1, nttabinsp1, .true., auxtable, auxtable)
c
            endif
c
            itmp1 = 1
            itmp2 = nttabins
            if (iopt(2) .eq. 1) then
              write(8, 2010, err = 3010) cmmchar
              write(8, 2330, err = 3010) cmmchar, '  <', logbin(1),
     +                                   auxtable(0)
              write(8, 2330, err = 3010) cmmchar, '  >',
     +                                   logbin(nttabinsp1),
     +                                   auxtable(nttabinsp1)
              write(8, 2010, err = 3010) cmmchar
            else if (iopt(2) .eq. 2) then
              itmp1 = 0
              itmp2 = nttabinsp1
            endif
c
            do i = itmp1, itmp2
              write(8, 2320, err = 3010) i, logbin(max(1, i)),
     +                                   auxtable(i)
            enddo
c
          else if (itab .lt. 3000) then
c
c           Energy distributions histograms.
c
c           Searching the kind-specific index, and getting data in the
c           cases of normal or unweighted tables.
c
            if (itab .lt. 2800) then
c
c             Normal tables.
c
              call gettcode(itab, nldtables, ldcodee, jtab)
c
              do i = 0, nttabinsp1
                auxtable(i) = rthiste(2, i, jtab)
              enddo
c
              auxtname = ldnamee(jtab)
c
            else
c
c             Unweighted tables.
c
              call gettcode(itab, nldtables, wldcodee, jtab)
c
              do i = 0, nttabinsp1
                auxtable(i) = wrthiste(2, i, jtab)
              enddo
c
              auxtname = wldnamee(jtab)
c
            endif
c
c           The table exists, opening the file.
c
            open(8, file = auxfilestring, status = 'UNKNOWN',
     +           access = 'SEQUENTIAL', err = 3010)
c
            logbinca = factrthei
            logbincb = rhei0
c
            if (iopt(3) .le. 1) then
c
c             Raw distributions (particles).
c
              if (fhead) then
                call exporthead(8, .true., jshower,
     +                          itab, auxtname, 0, ' ',
     +                          'Energy', iopt(4))
              endif
c
            else if (iopt(3) .eq. 2) then
c
c             dN/dln(E) distributions.
c
              if (fhead) then
                call exporthead(8, .true., jshower, itab, auxtname, 1,
     +                          'dN/dln(E) distribution.',
     +                          'Energy', iopt(4))
              endif
c
              call lgtransf(1, nttabinsp1, .false., auxtable, auxtable)
c
            else
c
c             dN/dlog10(E) distributions.
c
              if (fhead) then
                call exporthead(8, .true., jshower, itab, auxtname, 1,
     +                          'dN/dlog10(E) distribution.',
     +                          'Energy', iopt(4))
              endif
c
              call lgtransf(1, nttabinsp1, .true., auxtable, auxtable)
c
            endif
c
            itmp1 = 1
            itmp2 = nttabins
            if (iopt(2) .eq. 1) then
              write(8, 2010, err = 3010) cmmchar
              write(8, 2330, err = 3010) cmmchar, '  <',
     +                                   fener * logbin(1),
     +                                   auxtable(0)
              write(8, 2330, err = 3010) cmmchar, '  >',
     +                                   fener * logbin(nttabinsp1),
     +                                   auxtable(nttabinsp1)
              write(8, 2010, err = 3010) cmmchar
            else if (iopt(2) .eq. 2) then
              itmp1 = 0
              itmp2 = nttabinsp1
            endif
c
            do i = itmp1, itmp2
              write(8, 2320, err = 3010) i, fener * logbin(max(1, i)),
     +                                   auxtable(i)
            enddo
c
          else if (itab .lt. 4000) then
c
c           Time distribution tables.
c
c           Searching the kind-specific index.
c
            call gettcode(itab, ntdtables, tdcoden, jtab)
c
c           The table exists, opening the file.
c
            open(8, file = auxfilestring, status = 'UNKNOWN',
     +           access = 'SEQUENTIAL', err = 3010)
c
            if (fhead) call exporthead(8, .true., jshower, itab,
     +                                 tdnamen(jtab), 0, ' ',
     +                                 'R (distance to the core)',
     +                                 iopt(4))
c
            logbinca = factrthni
            logbincb = rhni0
c
            itmp1 = 1
            itmp2 = nttabins
            if (iopt(2) .eq. 1) then
c
              write(8, 2010, err = 3010) cmmchar
              write(8, 2330, err = 3010) cmmchar, '  <', logbin(1),
     +                                   rthistt(2, 0, jtab)
              write(8, 2330, err = 3010) cmmchar, '  >',
     +                                   logbin(nttabinsp1),
     +                                   rthistt(2, nttabinsp1, jtab)
              write(8, 2010, err = 3010) cmmchar
c
            else if (iopt(2) .eq. 2) then
              itmp1 = 0
              itmp2 = nttabinsp1
            endif
c     
            do i = itmp1, itmp2
              write(8, 2320, err = 3010) i, logbin(max(1, i)),
     +                                   rthistt(2, i, jtab)
            enddo
c
          else
c
c           The remaining tables (>5000) are always "per shower",
c           they are exported in routine texports.
c
            iopt(1) = -99
c
          endif
c
          if (iopt(1) .eq. 1) then
            write(8, 2010, err = 3010) cmmchar
          else if (iopt(1) .ge. 0) then
            write(8, *, err = 3010)
          endif
c
          close(8)
        enddo
      enddo
c
      close(shzut)
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$A09', 4, 'texportshowers',
     +              auxfilestring(1:flen1),
     +              1, 8, 0, 0.d0, ' ')
      return
c
 3020 continue
c
      call errprint(0, '$A30', 4, 'texportshowers',
     +              ' ', 1, shzut, 0, 0.d0, ' ')
c
      end
c     --- End of routine texportshowers.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'summaryf.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
