c
c     FILE: inisry.f                        Creation date: 11/JUL/1996.
c                                       LAST MODIFICATION: 18/JUN/2003.
c
c     Checking the input data after scanning the input file.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine inpsry(ouflag)
c
c     Printing input data summary.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 2003.
c
c     Arguments:
c     =========
c
c     ouflag.......... (input, integer) Logical output unit(s)
c                      selection flag. See explanation in
c                      subroutine "minmaxou".
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'versionpar.f'
      include 'initpar.f'
      include 'kernelpar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           ouflag
c
c     Declaration of shared data.
c
      include 'maincomm.f'
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'ciocomm.f'
      include 'cioauxcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           u, minu, maxu
      character*80      auxstring
      integer           iauxv(6)
      integer           i, j, k, aux1, aux2, aux3, aux4
      equivalence       (aux1, iauxv(1))
      equivalence       (aux2, iauxv(2))
      equivalence       (aux3, iauxv(3))
      equivalence       (aux4, iauxv(4))
      logical           ltmp1, numn, intn, lgsw
      double precision  ftmp1
      integer           idlcheck
      double precision  cvtfltdate
c
      character*9       defsymprint(-1:1)
c
      data              defsymprint / '     (X) ',
     +                                '     (D) ',
     +                                '         ' /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Selecting output units.
c
      call minmaxou(ouflag, minu, maxu)
c
 2010 format(9a)
c
c     Printing the remarks.
c
      if (remark) then
c
        do u = minu, maxu
c
          write(u, *)
          write(u, 2010) '>>>>'
          write(u, 2010) '>>>>          REMARKS.'
          write(u, 2010) '>>>>'
          write(u, *)
c
        enddo
c
c       Opening the remarks file.
c
        open(rmkut, file = rmkfn, status = 'OLD',
     +              form = 'UNFORMATTED', err = 1020)
c
 1010   continue
        read(rmkut, err = 1020, end = 1030) aux1
c
        if (aux1 .gt. 0) then
c
          read(rmkut, err = 1020, end = 1030) auxstring(1:aux1)
          do u = minu, maxu
            write(u, 2010) '     ', auxstring(1:aux1)
          enddo
c
        else
c
          do u = minu, maxu
            write(u, *)
          enddo
c
        endif
        goto 1010
c
 1020   continue
c
c       I/O error.
c
        call errprint(ouflag, '$A30', 3, 'inpsry', ' ',
     +                1, rmkut, 0, 0.d0, ' ')
c
 1030   continue
        close(rmkut)
c
      endif
c
c     Printing out summary information on the input data.
c
      do u = minu, maxu
c
        write(u, *)
        write(u, 2010) '>>>>'
        write(u, 2010) 
     +    '>>>>          PARAMETERS AND OPTIONS IN EFFECT.'
        write(u, 2010) '>>>>'
        if (usedefault) then
          write(u, 2010) 
     +      '>>>> "(D)" indicates that the corresponding ',
     +      'default value is being used.'
          write(u, 2010) '>>>>'
        endif
c
        write(u, *)
c
 2020   format(a, a27, ': ', 3a)
 2021   format(a, a27, '  ', 3a)
 2025   format(a, a27, ': ', a10, 2a)
 2030   format(a, a27, ': ', 3i10)
c
        if (tasknamever .eq. 0) then
          call scontain(taskname, tasknamelen, 42, auxline, aux2)
          write(u, 2020) defsymprint(min(1, tasknameset)),
     +                   'Task Name', auxline(1:aux2)
        else
          call intnice(tasknamever, 0, auxstring, aux1)
          if (tasknamelen .le. (29 - aux1)) then
            write(u, 2020) defsymprint(min(1, tasknameset)),
     +                     'Task Name', taskname(1:tasknamelen),
     +                     '    Version: ', auxstring(1:aux1)
          else
            call scontain(taskname, tasknamelen, 42, auxline, aux2)
            write(u, 2020) defsymprint(min(1, tasknameset)),
     +                     'Task Name', auxline(1:aux2)
            write(u, 2020) defsymprint(min(1, tasknameset)),
     +                     'Task Version', auxstring(1:aux1)
          endif
        endif
c
c       Run control parameters and file names are printed only for
c       simulation and summary programs.
c
        if (pgmcode .lt. 3000) then
c
          write(u, *)
          write(u, 2010) '     ', 'RUN CONTROL:'
c
          if (totshwset .gt. 0) then
            write(u, 2030) defsymprint(1),
     +                     'Total number of showers', mtotshowers
          else
            write(u, 2025) defsymprint(-1),
     +                     'Total number of showers', 'NONE'
          endif
c
          if (firstshowerset .gt. 0) then
            write(u, 2030) defsymprint(1),
     +                     'First shower number', firstshowernon1 + 1
          endif
c
          call intnice(runshowers, 1, auxstring, aux1)
          write(u, 2025) defsymprint(min(1, runshwset)),
     +                   'Showers per run', auxstring(1:aux1)
c
          call intnice(processjobs, 1, auxstring, aux1)
          write(u, 2025) defsymprint(min(1, processjobsset)),
     +                   'Runs per process', auxstring(1:aux1)
c
          call tnice(cpuperrun, 1, auxstring, aux1)
          if (aux1 .le. 10) then
            write(u, 2025) defsymprint(min(1, cpurunset)),
     +                     'CPU time per run', auxstring(1:aux1)
          else
            write(u, 2020) defsymprint(min(1, cpurunset)),
     +                     'CPU time per run', auxstring(1:aux1)
          endif
c
c         Printing file information.
c
          write(u, *)
          write(u, 2010) '     ', 'FILE NAMES:'
c
          call scontain(leadingf0, leadingf0len, 38, auxstring, aux1)
          write(u, 2020) defsymprint(1),
     +                   'Log file',
     +                   auxstring(1:aux1), lgfext
c
          write(u, 2020) defsymprint(1),
     +                   'Binary dump file',
     +                   auxstring(1:aux1), idfext
c
          if (adfile) then
            write(u, 2020) defsymprint(1),
     +                     'ASCII dump file',
     +                     auxstring(1:aux1), adfext
          endif
c
          ltmp1 = (ciosplitevery .gt. 0)
          if (ltmp1) then
            call scontain(leadingf0, leadingf0len, 24, auxstring, aux1)
            aux1 = aux1 + 6
            auxstring(aux1-5:aux1) = '_sNNNN'
          else
            call scontain(leadingf0, leadingf0len, 30, auxstring, aux1)
          endif
          if (nciofilesu .eq. 1) then
            write(u, 2020) defsymprint(1),
     +                     'Compressed data file',
     +                     auxstring(1:aux1), pofilext(ciofilesu(1))
          else if (nciofilesu .gt. 1) then
            write(u, 2020) defsymprint(1),
     +                     'Compressed data files',
     +                     auxstring(1:aux1), pofilext(ciofilesu(1))
            do i = 2, nciofilesu
              write(u, 2021) defsymprint(1), ' ',
     +                       auxstring(1:aux1), pofilext(ciofilesu(i))
            enddo
          else
            write(u, 2020) defsymprint(1),
     +                     'Compressed data files',
     +                     'NONE'
          endif
          if (ltmp1 .and. (nciofilesu .gt. 0)) then
            write(u, 2030) defsymprint(1),
     +                     'Showers per compressed file',
     +                     ciosplitevery
          endif
c
          if (ntableexp .gt. 0) then
            call scontain(leadingf0, leadingf0len, 36, auxstring, aux1)
            write(u, 2020) defsymprint(1),
     +                     'Table export file(s)',
     +                     auxstring(1:aux1), '.tNNNN'
            if (idlcheck('PerShowerData') .eq. 10) then
              call getinpstring('PerShowerData', auxstring, aux2)
              if (exportpershower .and.
     +            (auxstring(1:aux2) .eq. 'Full')) then
                call scontain(leadingf0, leadingf0len, 30,
     +                        auxstring, aux1)
                write(u, 2021) defsymprint(1), ' ',
     +                         auxstring(1:aux1), '_sNNNN.tNNNN'
              endif
            endif
          endif
c
          if (sryison) then
            call scontain(leadingf0, leadingf0len, 34, auxstring, aux1)
            write(u, 2020) defsymprint(1),
     +                     'Output summary file',
     +                     auxstring(1:aux1), sryfn
            if (latexsry) then
              write(u, 2020) defsymprint(1),
     +                       'LaTeX output file',
     +                       auxstring(1:aux1), texfn
            endif
          endif
c
          if (tssison) then
            call scontain(leadingf0, leadingf0len, 34, auxstring, aux1)
            write(u, 2020) defsymprint(1),
     +                     'Task Summary Script file',
     +                     auxstring(1:aux1), tssext
          endif
c
          if (wdirnamelen(1) .gt. 0) then
            if (wdirnamelen(2) .gt. 0) then
              ltmp1 = (wdirname(2)(1:wdirnamelen(2)) .eq.
     +                 wdirname(1)(1:wdirnamelen(1)))
            else
              ltmp1 = .true.
            endif
            call scontain(wdirname(1), wdirnamelen(1),
     +                    42, auxstring, aux1)
            if (ltmp1) then
              write(u, 2020) defsymprint(1),
     +                       'Output file directory',
     +                       auxstring(1:aux1)
            else
              write(u, 2020) defsymprint(1),
     +                       'Compressed file directory',
     +                       auxstring(1:aux1)
              call scontain(wdirname(2), wdirnamelen(2),
     +                      42, auxstring, aux1)
              write(u, 2020) defsymprint(1),
     +                       'Global file directory',
     +                       auxstring(1:aux1)
            endif
          else
            if (wdirnamelen(2) .gt. 0) then
              call scontain(wdirname(2), wdirnamelen(2),
     +                      42, auxstring, aux1)
              write(u, 2020) defsymprint(1),
     +                       'Global file directory',
     +                       auxstring(1:aux1)
            endif
          endif
c
          if ((pgmcode .ge. 2000) .and.
     +        (idfversion .ne. aires_version)) then
            write(u, *)
            write(u, 2020) defsymprint(1),
     +                     'Dump file AIRES version', idfversion
            if (nverchanges .gt. 0) then
              write(u, 2020) defsymprint(1),
     +                     'Original AIRES version', original_version
            endif
          endif
c
        endif
c
        write(u, *)
        write(u, 2010) '     ', 'BASIC PARAMETERS:'
c
        call strimcopy(sitename(isite), 16, auxstring, aux1)
        write(u, 2020) defsymprint(min(1, iidatawaset(3))),
     +                 'Site', auxstring(1:aux1)
        write(auxstring, 2105)
     +        '(Lat:', sitelat(isite),
     +        ' deg. Long:',  sitelong(isite), ' deg.)'
        write(u, 2021) defsymprint(1), ' ', auxstring(1:37)
 2105   format(a, f7.2, a, f8.2, a)
c
        if (eventdate .lt. 0) then
          do i = 1, 6
            iauxv(i) = 0
          enddo
          ftmp1 = cvtfltdate(eventdate, iauxv)
          call datifmt(iauxv, auxstring)
          aux1 = 11
        else
          call fltnice(eventdate, 0, auxstring, aux1)
        endif
        write(u, 2020) defsymprint(min(1, fidatawaset(39))),
     +                 'Date', auxstring(1:aux1)
        write(u, *)
c
        if (nshprimary .eq. 1) then
c
          call pnice(2, shprimary(1), auxstring, aux1)
          write(u, 2020) defsymprint(1),
     +                   'Primary particle', auxstring(1:aux1)
c
        else if (nshprimary .gt. 1) then
c
          call pnice(2, shprimary(1), auxstring, aux1)
          write(auxstring(21:40), 2100) 'Weight =',
     +                                  shprimarywt(1)
          write(u, 2020) defsymprint(1),
     +                   'Primary particles', auxstring(1:40)
          do i = 2, nshprimary
            call pnice(2, shprimary(i), auxstring, aux1)
            write(auxstring(21:40), 2100) 'Weight =',
     +                                    shprimarywt(i)
            write(u, 2021) defsymprint(1),
     +                     ' ', auxstring(1:40)
          enddo
c
        else
          write(u, 2025) defsymprint(-1),
     +                   'Primary particle', 'NONE'
        endif
 2100   format(a, 1p, g12.4)
c
        if (pryeminset .gt. 0) then
c
          call enice(pryenergymin, 0, auxstring, aux1)
c
          if (pryenergymax .le. pryenergymin) then
c
            write(u, 2020) defsymprint(1),
     +                     'Primary energy', auxstring(1:aux1)
          else
c
            do i = aux1, 1, -1
              auxstring(i+5:i+5) = auxstring(i:i)
            enddo
            auxstring(1:5) = 'From '
            aux1 = aux1 + 9
            auxstring(aux1-3:aux1) = ' to '
            call enice(pryenergymax, 0,
     +                 auxstring(aux1+1:64), aux2)
            aux1 = aux1 + aux2
            write(u, 2020) defsymprint(1),
     +                     'Primary energy', auxstring(1:aux1)
            aux1 = aux1 + aux2 + 10
            write(auxstring, 2100) '(Gamma = '
            call fltnice(pryenergyslp, 0, auxstring(10:80), aux1)
            aux1 = aux1 + 10
            auxstring(aux1:aux1) = ')'
            write(u, 2021) defsymprint(1),
     +                     ' ', auxstring(1:aux1)
c
          endif
        else
          write(u, 2025) defsymprint(-1),
     +                   'Primary energy', 'NONE'
        endif
c
        aux1 = min(1, fidatawaset(3))
        if (pryzenithmax .gt. pryzenithmin) then
          write(auxstring, 2110) 'From', pryzenithmin,
     +                           ' deg to', pryzenithmax, ' deg'
          write(u, 2020) defsymprint(aux1),
     +                   'Primary zenith angle', auxstring(1:31)
          if (varzendis .eq. 2) then
            auxstring = 'Sine Cosine'
            aux2      = 11
          else
            auxstring = 'Sine'
            aux2      = 4
          endif
          write(u, 2020) defsymprint(aux1),
     +                   'Zenith angle distribution',
     +                   auxstring(1:aux2)
        else
          write(auxstring, 2120) pryzenithmin, ' deg'
          write(u, 2020) defsymprint(aux1),
     +                   'Primary zenith angle', auxstring(1:12)
        endif
 2110   format(a, 2(f8.2, a))
 2120   format(f8.2, a)
c
        if (pryazimmax .gt. pryazimmin) then
          write(auxstring, 2110) 'From', pryazimmin,
     +                           ' deg to',  pryazimmax, ' deg'
          aux1 = 31
        else
          write(auxstring, 2120) pryazimmin, ' deg'
          aux1 = 12
        endif
        write(u, 2020) defsymprint(min(1, fidatawaset(5))),
     +                 'Primary azimuth angle', auxstring(1:aux1)
        if (geognorth) then
          auxstring = 'Geographic'
          aux1      = 10
        else
          auxstring = 'Local magnetic'
          aux1      = 14
        endif
        write(u, 2020) defsymprint(min(1, lidatawaset(1))),
     +                 'Zero azimuth direction',
     +                  auxstring(1:aux1), ' north'
c
        if (thinningon) then
          call enice(ethinpar, 2, auxstring, aux1)
        else
          auxstring = 'NONE'
          aux1      = 4
        endif
        write(u, 2020) defsymprint(min(1, fidatawaset(7))),
     +                 'Thinning energy', auxstring(1:aux1)
c
        call lnice(injz, 0, auxstring, aux1)
        if (auxstring(aux1-1:aux1-1) .eq. ' ') then
          aux1 = aux1 + 1
          auxstring(aux1:aux1) = ' '
        endif
        aux1 = aux1 + 2
        auxstring(aux1-1:aux1) = ' ('
        call fltnice(injdepth, 0, auxstring(aux1+1:80), aux2)
        aux2 = aux1 + aux2 + 7
        auxstring(aux2-6:aux2) = ' g/cm2)'
        write(u, 2020) defsymprint(min(1, fidatawaset(8))),
     +                 'Injection altitude', auxstring(1:aux2)
c
        call lnice(groundz, 0, auxstring, aux1)
        if (auxstring(aux1-1:aux1-1) .eq. ' ') then
          aux1 = aux1 + 1
          auxstring(aux1:aux1) = ' '
        endif
        aux1 = aux1 + 2
        auxstring(aux1-1:aux1) = ' ('
        call fltnice(groundepth, 0, auxstring(aux1+1:80), aux2)
        aux2 = aux1 + aux2 + 7
        auxstring(aux2-6:aux2) = ' g/cm2)'
        write(u, 2020) defsymprint(min(1, fidatawaset(10))),
     +                 'Ground altitude', auxstring(1:aux2)
c
        call lnice(obslevmaxz, 0, auxstring, aux1)
        if (auxstring(aux1-1:aux1-1) .eq. ' ') then
          aux1 = aux1 + 1
          auxstring(aux1:aux1) = ' '
        endif
        aux1 = aux1 + 2
        auxstring(aux1-1:aux1) = ' ('
        call fltnice(obslevmind, 0, auxstring(aux1+1:80), aux2)
        aux2 = aux1 + aux2 + 7
        auxstring(aux2-6:aux2) = ' g/cm2)'
        write(u, 2020) defsymprint(min(1, obslevset)),
     +                 'First obs. level altitude', auxstring(1:aux2)
c
        call lnice(obslevminz, 0, auxstring, aux1)
        if (auxstring(aux1-1:aux1-1) .eq. ' ') then
          aux1 = aux1 + 1
          auxstring(aux1:aux1) = ' '
        endif
        aux1 = aux1 + 2
        auxstring(aux1-1:aux1) = ' ('
        call fltnice(obslevmaxd, 0, auxstring(aux1+1:80), aux2)
        aux2 = aux1 + aux2 + 7
        auxstring(aux2-6:aux2) = ' g/cm2)'
        write(u, 2020) defsymprint(min(1, obslevset)),
     +                 'Last obs. level altitude', auxstring(1:aux2)
c
        write(u, 2130) defsymprint(min(1, obslevset)),
     +                 'Obs. levels and depth step',
     +                  nobslevels, obslevstep, ' g/cm2'
 2130   format(a, a27, ': ', i10, f10.3, a)
c
        if (geobswitch .le. 0) then
          write(u, 2020) defsymprint(min(1, iidatawaset(4))),
     +                   'Geomagnetic field', 'Off'
        else
          call bnice(geob, 0, auxstring, aux1)
          write(u, 2020) defsymprint(min(1, iidatawaset(4))),
     +                   'Geomagnetic field',
     +                   'Intensity: ', auxstring(1:aux1)
          write(auxstring, 2110) 'I:', geobi,
     +                           ' deg. D:', geobd, ' deg'
          aux1 = 30
          write(u, 2021) defsymprint(min(1, fidatawaset(36))), ' ',
     +                   auxstring(1:aux1)
          if (geobswitch .eq. 2) then
            call bnice(geobfluc, 2, auxstring, aux1)
            write(u, 2021) defsymprint(1), ' ',
     +                     'Fluctuation: ', auxstring(1:aux1)
          endif
        endif
c
        call enice(eminhis, 0, auxstring, aux1)
        aux1 = aux1 + 4
        auxstring(aux1-3:aux1) = ' to '
        call enice(emaxhis, 0, auxstring(aux1+1:80), aux2)
        aux2 = aux1 + aux2
        write(u, 2020) defsymprint(min(1, fidatawaset(12))),
     +                 'Table energy limits', auxstring(1:aux2)
c
        call lnice(rminhis, 0, auxstring, aux1)
        aux1 = aux1 + 4
        auxstring(aux1-3:aux1) = ' to '
        call lnice(rmaxhis, 0, auxstring(aux1+1:80), aux2)
        aux2 = aux1 + aux2
        write(u, 2020) defsymprint(min(1, fidatawaset(14))),
     +                 'Table radial limits', auxstring(1:aux2)
c
        if (nciofilesu .gt. 0) then
          j = ciofilesu(1)
          k = 14 + 2 * j
          call lnice(fidata(0, k), 0, auxstring, aux1)
          aux1 = aux1 + 4
          auxstring(aux1-3:aux1) = ' to '
          call lnice(fidata(0, k + 1), 0, auxstring(aux1+1:80), aux2)
          aux2 = aux1 + aux2 + 2
          call strimcopy(pofilext(j), 12, auxstring(aux2:80), aux1)
          auxstring(aux2-1:aux2) = ' ('
          aux2 = aux2 + aux1
          auxstring(aux2:aux2) = ')'
          write(u, 2020) defsymprint(min(1, fidatawaset(k))),
     +                   'Output file radial limits', auxstring(1:aux2)
          do i = 2, nciofilesu
            j = ciofilesu(i)
            k = 14 + 2 * j
            call lnice(fidata(0, k), 0, auxstring, aux1)
            aux1 = aux1 + 4
            auxstring(aux1-3:aux1) = ' to '
            call lnice(fidata(0, k + 1), 0, auxstring(aux1+1:80), aux2)
            aux2 = aux1 + aux2 + 2
            call strimcopy(pofilext(j), 12, auxstring(aux2:80), aux1)
            auxstring(aux2-1:aux2) = ' ('
            aux2 = aux2 + aux1
            auxstring(aux2:aux2) = ')'
            write(u, 2021)
     +        defsymprint(min(1, fidatawaset(k))), ' ',
     +        auxstring(1:aux2)
          enddo
        endif
c
c       Printing "not-hardwired" parameter info.
c
        if (ncommands .ge. ncommands0) then
c
          write(u, 2020)
          if (fullinputlist) then
            write(u, 2010) '     ',
     +                     'ADDITIONAL PARAMETERS (Full list):'
          else
            write(u, 2010) '     ', 'ADDITIONAL PARAMETERS:'
          endif
c
          do i = ncommands0, ncommands
c
c           Determining the kind of magnitude.
c
            aux1 = (ccode(i) - 600) / 200
            numn = (aux1 .le. 4)
            intn = (aux1 .eq. 5)
            lgsw = (aux1 .eq. 6)
            j    = aditem(i)
c
            if (numn) then
              aux4  = min(1, fidatawaset(j))
            else if (intn) then
              aux4  = min(1, iidatawaset(j))
            else if (lgsw) then
              aux4  = min(1, lidatawaset(j))
            else
              aux4  = min(1, sidatawaset(j))
            endif
c
            if (fullinputlist .or. veryimpdir(i) .or.
     +          (aux4 .gt. 0)) then
c
              if (numn) then
c
                ftmp1 = fidata(0, j)
c
                if (aux1 .eq. 1) then
                  call fltnice(ftmp1, 0, auxstring, aux2)
                else if (aux1 .eq. 2) then
                  call lnice(ftmp1, 0, auxstring, aux2)
                else if (aux1 .eq. 3) then
                  call tnice(ftmp1, 0, auxstring, aux2)
                else
                  call enice(ftmp1, 0, auxstring, aux2)
                endif
c
              else if (intn) then
c
                call intnice(iidata(0, j), 0, auxstring, aux2)
c
              else if (lgsw) then
c
                call swnice(lidata(0, j), auxstring, aux2)
c
              else
c
                call nthword(sidata(0, j),
     +                       sidatastring(sidata(4, j):
     +                                    sidata(5, j)),
     +                       sidatastring(sidata(3, j):
     +                                    sidata(3, j)),
     +                       auxstring, aux2)
c
              endif
c
              call strim(27, clgname(i), aux3)
              write(u, 2020) defsymprint(aux4),
     +                       clgname(i)(1:aux3), auxstring(1:aux2)
c
            endif
          enddo
        endif
c
c       Printing additional information.
c
        write(u, *)
        write(u, 2010) '     ', 'MISCELLANEOUS:'
c
        if ((inputrseed .gt. 0) .and. (inputrseed .lt. 1)) then
          write(auxstring, 2200) inputrseed
 2200     format(f14.12)
          aux2 = 14
          call snumtrim(auxstring, aux2)
          write(u, 2020) defsymprint(min(1, fidatawaset(50))),
     +                   'Seed of random generator',
     +                   auxstring(1:aux2)
        else
          write(u, 2020) defsymprint(min(1, fidatawaset(50))),
     +                   'Seed of random generator', 'Automatic'
        endif
c
        write(u, 2020) defsymprint(min(1, atmoslset)),
     +                 'Atmospheric model', atmosmodel
c
c       End of summary of input variables.
c
c       Matias: ZHAireS status Compatibility with IDL
c        write(*,*) "Calling ZHAIRESTATUS from inisry",u
        call zhairestatus(u,i)   
c
      enddo
c
      return
      end
c     --- End of routine inpsry
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'inisry.f'
c     This source file is part of AIRES 2.8.4a distribution.
c     modified with ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
