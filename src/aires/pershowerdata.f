c
c     FILE: pershowerdata.f                 Creation date: 19/AUG/2003.
c                                       LAST MODIFICATION: 19/AUG/2003.
c
c     Routines related with shower per shower data.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine inipershowerdata
c
c     Initializing per shower data internal file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 2000, 2001.
c                                Fermilab 2003.
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
      integer           i, j, k, l, m, n, o
      character*16      shdata
c
c     FIRST EXECUTABLE STATEMENT
c
      call getinpstring('PerShowerData', shdata, i)
c
      saveshowerdata  = (shdata(1:i) .ne. 'None')
      pershowertables = (shdata(1:i) .eq. 'Full')
c
      open(shzut, file = shzfn, status = 'UNKNOWN',
     +     form = 'UNFORMATTED', err = 3010)
c
c     The index i identifies the model user for shower profile
c     parameterization. 114 refers to Gaisser-Hillas function with 4
c     free parameters, plus an additiona data item to store slant
c     Xmax.
c     If the index i is greater than 1000000, that means that the
c     tables at each shower will also be saved.
c     When shower data are saved, the following indices indicate
c     the number of items to be stored:
c     The second index contains the number of parameters used (plus one
c     reserved for slant Xmax).
c     The third index is the number of integer data items written at
c     each shower head record.
c     The fourth index indicating the number of floating point
c     fields that come after the integer data items.
c     The fifth index indicates the number of table families,
c     which is 0 when no tables are saved.
c
      i = 114
      j =   5
      k =   2
      l = nshff * nlhtables + j + 6
      o = 0
c
      if (pershowertables) then
c
        i = i + 1000000
        m = 4
c
        write(shzut, err = 3010) i, j, nlhtables, k, l, m,
     +                           (o, n = 1, 5)
c
c       Writing table info (m = 4 families)
c
c       1. Longitudinal tables (5 groups).
c
        i = 5
        j = 1
        write(shzut, err = 3010) nlhtables, i, j, nobslevelsp1,
     +                           (o, n = 1, 6)
c
c       2. Lateral and energy distribution tables (4 groups).
c
        i = 4
        j = 0
        write(shzut, err = 3010) nldtables, i, j, nttabinsp1,
     +                           (o, n = 1, 6)
c
c       3. Time distribution tables (1 group).
c
        i = 1
        write(shzut, err = 3010) ntdtables, i, j, nttabinsp1,
     +                           (o, n = 1, 6)
c
c       4. Deposited energy and related tables (7 groups).
c
        i = 7
        j = 1
        write(shzut, err = 3010) nlitables, i, j, nobslevelsp1,
     +                           (o, n = 1, 6)
c
c       End of header mark.
c
        i = -99966
        write(shzut, err = 3010) i
c
      else
c
c       If PerShowerData is set to "None", no data are saved.
c
        if (.not. saveshowerdata) then
          j = 0
          k = 0
          l = 0
        endif
c
        write(shzut, err = 3010) i, j, nlhtables, k, l,
     +                           (o, n = 1, 6)
c
      endif
c
      close(shzut)
c
      return
c
c     Error messages.
c
 3010 continue
c
      call errprint(0, '$A09', 4, 'inipershowerdata',
     +              ' ', 1, shzut, 0, 0.d0, ' ')
c
      end
c     --- End of routine inipershowerdata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine addpershowerdata(primcode, primegy, primzen, primazim,
     +                            primx1, primx1sl,
     +                            x0, lambda, xmax, nmax, sumofsq, irc)
c
c     End of shower histogram updating.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999;
c                                Fermilab 1999; La Plata 2000, 2001;
c                                Fermilab 2003.
c
c
c     Arguments:
c     =========
c
c     primcode........ (input, integer) Primary code.
c     primegy......... (input, double precision) Primary energy (GeV).
c     primzen......... (input, double precision) Primary zenith angle
c                      (deg).
c     primazim........ (input, double precision) Primary azimuth angle
c                      (deg).
c     primx1.......... (input, double precision) Vertical depth of
c                      first interaction (g/cm2).
c     primx1sl........ (input, double precision) Slant depth of
c                      first interaction (g/cm2).
c     x0, lambda...... (input, double precision) Two of the four
c                      parameters of the shower profile fit.
c     xmax............ (input, double precision, array(2)) Fitted
c                      position of the shower maximum (g.cm2) (1:
c                      vertical, 2: slant). If no fit was possible,
c                      then the the value coming from a direct
c                      estimation from the MC data is returned.
c     nmax............ (input, double precision) Estimated number of
c                      charged particles at the shower maximum. If no
c                      fit was possible, then the value coming from a
c                      direct estimation from the MC data is returned.
c     sqsum........... (input, double precision) The resulting sum of
c                      weighted squares.
c     irc............. (input, integer) Xmax fit return code. Zero
c                      means that the fit was successfully completed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           primcode
      double precision  primegy, primzen, primazim
      double precision  primx1, primx1sl
      double precision  x0, lambda, xmax(2), nmax, sumofsq
      integer           irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, k, k0, l
c
c     FIRST EXECUTABLE STATEMENT
c
c     Saving xmax and related data.
c     3 families of data are currently being written (see hdatapar.f).
c
      call appendopen(shzut, shzfn, 'UNKNOWN', 'UNFORMATTED', k)
      if (k .ne. 0) goto 3010
c
      write(shzut, err = 3010)
     +       irc, primcode,
     +       x0, lambda, xmax(1), nmax, xmax(2), sumofsq,
     +       (lhistn(2, nobslevelsp1, k), k = 1, nlhtables),
     +       (lhiste(2, nobslevelsp1, k), k = 1, nlhtables),
     +       (wlhistn(2, nobslevelsp1, k), k = 1, nlhtables),
     +       primx1, primegy, primx1sl, primzen, primazim
c
c     Saving individual shower tables.
c
      if (pershowertables) then
c
        i = 0
        write(shzut, err = 3010) currshower, (i, k = 1, 9)
c
c       Longitudinal histograms.
c
        do k = 1, nlhtables
c
          do i = 1, nobslevelsp1
            obslevscra4(i) = lhistn(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
          do i = 1, nobslevelsp1
            obslevscra4(i) = lhiste(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
          do i = 1, nobslevelsp1
            obslevscra4(i) = wlhistn(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
c
          do i = 1, nobslevelsp1
            obslevscra4(i) = lhistn(2, i, k + mxlhtable)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
          do i = 1, nobslevelsp1
            obslevscra4(i) = wlhistn(2, i, k + mxlhtable)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
c
        enddo
c
c       Lateral and energy distribution histograms.
c
        do k = 1, nldtables
          do i = 0, nttabinsp1
            obslevscra4(i) = rthistn(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            obslevscra4(i) = rthiste(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            obslevscra4(i) = wrthistn(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
          do i = 0, nttabinsp1
            obslevscra4(i) = wrthiste(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
        enddo
c
c       Time distribution histograms.
c
        do k = 1, ntdtables
          do i = 0, nttabinsp1
            obslevscra4(i) = ucspeedns * rthistt(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 0, nttabinsp1)
        enddo
c
c       Deposited energy and related histograms.
c
        do l = 0, mxlitable, mxlitable
          do k0 = 1, nlitables
            k = k0 + l
c
            do i = 1, nobslevelsp1
              obslevscra4(i) = llhistn(2, i, k)
            enddo
            write(shzut, err = 3010)
     +                   (obslevscra4(i), i = 1, nobslevelsp1)
            do i = 1, nobslevelsp1
              obslevscra4(i) = llhiste(2, i, k)
            enddo
            write(shzut, err = 3010)
     +                   (obslevscra4(i), i = 1, nobslevelsp1)
            do i = 1, nobslevelsp1
              obslevscra4(i) = wllhistn(2, i, k)
            enddo
            write(shzut, err = 3010)
     +                   (obslevscra4(i), i = 1, nobslevelsp1)
          enddo
        enddo
c
        do k = 1, nlitables
          do i = 1, nobslevelsp1
            obslevscra4(i) = lihiste(2, i, k)
          enddo
          write(shzut, err = 3010)
     +                 (obslevscra4(i), i = 1, nobslevelsp1)
        enddo
c
        i = -99966
        write(shzut, err = 3010) i
c
      endif
c
      close(shzut)
c
      return
c
c     Error messages.
c
 3010 continue
c
      call errprint(0, '$A09', 4, 'addpershowerdata',
     +              ' ', 1, shzut, 0, 0.d0, ' ')
c
      return
      end
c     --- End of routine addpershowerdata
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'pershowerdata.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
