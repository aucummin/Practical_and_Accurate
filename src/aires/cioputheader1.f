c
c     FILE: cioputheader1.f                 Creation date: 17/MAR/1997.
c                                       LAST MODIFICATION: 02/MAR/2001.
c
c     This file contain the simulation program specific intructions to
c     write data into the header of the compressed files.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioputheader1(ifile, ut)
c
c     Writing the simulation program specific part of the header of
c     compressed i/o files. This routine is called at the beginning of
c     a task.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2001.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) Compressed file number. Ranges
c                      from 1 to the number of defined files.
c     ut.............. (input, integer) The FORTRAN i/o unit number
c                      used in the open instruction.
c
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'pclepar.f'
      include 'kernelpar.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'pclecomm.f'
      include 'kernelcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of arguments.
c
      integer           ifile, ut
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, i1, i2
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing basic data, and main input data arrays.
c
 2001 format(400a)
 2010 format(16i11)
 2020 format(1p, 9g24.14e3)
c
      write(ut, 2010, err = 3010) cruserlen, crhostlen,
     +                            wdirnamelen(1), tasknamelen,
     +                            tasknamever, tasknameset
c
      write(ut, 2001, err = 3010) datistr0(1), cruser, crhost,
     +                            compildate, compilwho, compilstar
c
      if (wdirnamelen(1) .gt. 0)
     +   write(ut, 2001, err = 3010) wdirname(1)(1:wdirnamelen(1))
      if (tasknamelen .gt. 0)
     +   write(ut, 2001, err = 3010) taskname(1:tasknamelen)
c
      write(ut, 2030, err = 3010) nshprimary, atmoslabel,
     +                            usedefaults, pryenergymin
 2030 format(2i11, l5, 1p, 9g24.14e3)
 2035 format(i11, 1p, g24.14e3)
c
      if (nshprimary .gt. 0) then
        do i = 1, nshprimary
          write(ut, 2035, err = 3010) shprimary(i),
     +                                shprimarywt(i)
        enddo
      endif
c
c     Input directive names and related data.
c
      write(ut, 2010, err = 3010) mxcdl, ncommands, ncommands0
      write(ut, 2010, err = 3010)
     +                nfidata, niidata, nlidata,
     +                nsidata, sidatalen,
     +                (i, j = 1, 6)
      write(ut, 2010, err = 3010)
     +                nfidata0, niidata0, nlidata0,
     +                nsidata0, sidatalen0,
     +                (i, j = 1, 6)
c
      if (ncommands .gt. 0) then
        do i = 1, ncommands
          write(ut, 2100, err = 3010)
     +                    cname(i), clgname(i),
     +                    ccode(i), minclen(i), aditem(i),
     +                    veryimpdir(i), wngonset(i)
        enddo
      endif
 2100 format(2a, 3i11, 2l5)
c
c     Main input data arrays.
c
      if (nfidata .gt. 0) then
        do i = 1, nfidata
          write(ut, 2040, err = 3010) fidatawaset(i), fdbdry(i),
     +                                (fidata(j, i), j = 0, 3)
        enddo
      endif
 2040 format(2i11, 1p, 4g24.14e3)
c
      if (niidata .gt. 0) then
        do i = 1, niidata
          write(ut, 2010, err = 3010) iidatawaset(i), idbdry(i),
     +                                (iidata(j, i), j = 0, 3)
        enddo
      endif
c
      if (nlidata .gt. 0) then
        do i = 1, nlidata
          write(ut, 2050, err = 3010) lidatawaset(i),
     +                                (lidata(j, i), j = 0, 2)
        enddo
      endif
 2050 format(i11, 4l5)
c
      if (nsidata .gt. 0) then
        do i = 1, nsidata
          write(ut, 2010, err = 3010) sidatawaset(i),
     +                                (sidata(j, i), j = 0, 6)
        enddo
        write(ut, 2001, err = 3010) sidatastring(1:sidatalen)
      endif
c
c     Site library.
c
      write(ut, 2010, err = 3010) nlibsites
c
      if (nlibsites .gt. 0) then
        do i = 0, nlibsites
          write(ut, 2055, err = 3010) sitename(i), sitenlen(i),
     +                                sitelat(i), sitelong(i),
     +                                siteground(i)
        enddo
      endif
 2055 format(a, i11, 1p, 3g24.14e3)
c
c     Saving data related with "Special" particles.
c
      write(ut, 2010, err = 3010) nescpcles
c
      if (nescpcles .gt. 0) then
        write(ut, 2010, err = 3010) lastescpcle, nescodes,
     +                              pescode1, pescode2, espms
        do i = 1, nescpcles
          write(ut, 2350, err = 3010) escpclename(i),
     +                                escmacrover(i),
     +                                escmacrouse(i),
     +                                (escmacropos(j, i), j = 1, 4),
     +                                (nsprimpart(j, i), j = 1, 3)
        enddo
 2350   format(a16, 9i11)
        write(ut, 2001, err = 3010)
     +            escpclemacro(1:escmacropos(4, nescpcles))
      endif
c
c     Saving global variables.
c
      write(ut, 2010) nglobar(1), nglobar(2)
c
      do j = 1, 2
        if (nglobar(j) .gt. 0) then
          do i = 1, nglobar(j)
            write(ut, 2360, err = 3010)
     +                globnam(i, j), globlen(i, j)
          enddo
 2360     format(a, i11)
          do i = 0, nglobar(j)
            write(ut, 2010, err = 3010) globdfend(i, j)
          enddo
          write(ut, 2010, err = 3010) globstrlen(j)
          if (globstrlen(j) .gt. 0) then
            write(ut, 2001, err = 3010)
     +                globarstring(j)(1:globstrlen(j))
          endif
        endif
      enddo
c
c     Remarks.
c
      write(ut, 2060, err = 3010) remark
 2060 format(l5)
      if (remark) then
c
c       Opening the remarks file.
c
        open(rmkut, file = rmkfn, status = 'OLD',
     +              form = 'UNFORMATTED', err = 3020)
c
 1010   continue
        read(rmkut, err = 3020, end = 1020) i
        write(ut, 2010, err = 3010) i
c
        if (i .gt. 0) then
          read(rmkut, err = 3020, end = 3020) auxline(1:i)
          write(ut, 2001, err = 3010) auxline(1:i)
        endif
        goto 1010
 1020   continue
        close(rmkut)
        i = -99999
        write(ut, 2010, err = 3010) i
c
      endif
c
c     Observing levels data.
c
      write(ut, 2010, err = 3010)
     +          (iobslevdat(i), i = 0, 1), obslevset, nobslevelsp1
      write(ut, 2020, err = 3010)
     +          ((fobslevdat(i, j), i = 0, 1), j = 1, 2)
      write(ut, 2020, err = 3010)
     +          obslevstep, obslevminz, obslevmaxz
c
c     Markers for observing levels that are recorded into the
c     longitudinal file(s).
c
      if (nobslevels .gt. 0) then
        do i1 = 1, nobslevels, 50
          i2 = min(nobslevels, i1 + 49)
          write(ut, 2070, err = 3010) (olfilesave(i), i = i1, i2)
        enddo
      endif
 2070 format(50l2)
c
c     Writing a record of zeros to allocate some spare fields.
c
      i = 0
      write(ut, 2010, err = 3010) (i, j = 1, 10)
c
      return
c
c     Error messages.
c
 3010 continue
      call errprint(0, '$CI2', 4, 'cioputheader1', ' ',
     +              1, ut, 0, 0.d0, ' ')
      return
c
 3020 continue
c
      call errprint(0, '$A09', 4, 'cioputheader1',
     +              ' ', 1, rmkut, 0, 0.d0, ' ')
      return
c
      end
c     --- End of routine cioputheader1.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'cioputheader1.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
