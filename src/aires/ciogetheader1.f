c
c     FILE: ciogetheader1.f                 Creation date: 17/MAR/1997.
c                                       LAST MODIFICATION: 03/MAY/2004.
c
c     Aires compressed i/o system (II): Routines to process already
c     created compressed files.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciogetheader1(ifile, ut, irc)
c
c     Reading and processing the simulation program specific part of
c     the compressed i/o headers.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998, 1999, 2001,
c                                         2004.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) Compressed file number.
c     ut.............. (input, integer) FORTRAN logical unit connected
c                      to the file.
c     irc............. (output, integer) Return code. Zero means normal
c                      return. The error codes are like the ones
c                      explained in routine "opencrofile".
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
      include 'ciopar.f'
      include 'cio2par.f'
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'pclecomm.f'
      include 'kernelcomm.f'
      include 'cio2comm.f'
      include 'hdatacomm.f'
c
c     Declaration of arguments.
c
      integer           ifile, ut, irc
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, i1, i2, imxcdl
      integer           itmp0, itmp1, itmp2, itmp3
c
      logical           after120, after140, after155
      logical           after215, after23C, after269
c
c     FIRST EXECUTABLE STATEMENT
c
c     Backwards compatibility stuff.
c
      after120 = (rcversion(ifile) .gt. 01020000)
      after140 = (rcversion(ifile) .gt. 01040000)
      after155 = (rcversion(ifile) .gt. 01050500)
      after215 = (rcversion(ifile) .gt. 02010500)
      after23C = (rcversion(ifile) .gt. 02031200)
      after269 = (rcversion(ifile) .gt. 02060900)
c
c     Reading basic data, and main input data arrays.
c
 2001 format(400a)
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         auxline(1:66)
      call restring(auxline(1:66))
      read(auxline(1:66), *, err = 3030)
     +        cruserlen, crhostlen, wdirnamelen(1), tasknamelen,
     +        tasknamever, tasknameset
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         datistr0(1), cruser, crhost,
     +         cofcdate(ifile), cofcwho(ifile), cofcstar(ifile)
c
      call restring(cofcdate(ifile))
      call restring(cofcwho(ifile))
      call restring(cofcstar(ifile))
c
      call restring(datistr0(1))
      call restring(cruser)
      call restring(crhost)
c
      if (wdirnamelen(1) .gt. 0) then
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           wdirname(1)(1:wdirnamelen(1))
        call restring(wdirname(1)(1:wdirnamelen(1)))
      endif
      if (tasknamelen .gt. 0) then
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           taskname(1:tasknamelen)
        call restring(taskname(1:tasknamelen))
      endif
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         auxline(1:51)
      call restring(auxline(1:51))
      read(auxline(1:51), *, err = 3030)
     +         nshprimary, atmoslabel, usedefaults, pryenergymin
c
      if (nshprimary .gt. 0) then
        do i = 1, nshprimary
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:35)
          call restring(auxline(1:35))
          read(auxline(1:35), *, err = 3030)
     +                           shprimary(i), shprimarywt(i)
        enddo
      endif
c
c     Input directive names and related data.
c
      if (after155) then
c
c       New format with string variables.
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:33)
        call restring(auxline(1:33))
        read(auxline(1:33), *, err = 3030)
     +                          imxcdl, ncommands, ncommands0
        if (imxcdl .gt. mxcdl) goto 3010
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:121)
        call restring(auxline(1:121))
        read(auxline(1:110), *, err = 3030)
     +                nfidata, niidata, nlidata,
     +                nsidata, sidatalen
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:121)
        call restring(auxline(1:121))
        read(auxline(1:110), *, err = 3030)
     +                nfidata0, niidata0, nlidata0,
     +                nsidata0, sidatalen0
c
      else
c
c       Old (AIRES 1.4.2) format.
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:99)
        call restring(auxline(1:99))
        read(auxline(1:99), *, err = 3030)
     +                          imxcdl, ncommands, ncommands0,
     +                          nfidata, nfidata0,
     +                          niidata, niidata0,
     +                          nlidata, nlidata0
        nsidata    = 0
        nsidata0   = 0
        sidatalen  = 0
        sidatalen0 = 0
c
      endif
c
      if (ncommands .gt. 0) then
        do i = 1, ncommands
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:imxcdl+70)
          call restring(auxline(1:imxcdl+70))
          cname(i) = auxline(1:imxcdl)
          clgname(i) = auxline(imxcdl+1:imxcdl+27)
          read(auxline(imxcdl+28:imxcdl+70), *, err = 3030)
     +                             ccode(i), minclen(i), aditem(i),
     +                             veryimpdir(i), wngonset(i)
        enddo
      endif
c
c     Main input data arrays.
c
      if (nfidata .gt. 0) then
        do i = 1, nfidata
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:118)
          call restring(auxline(1:118))
          read(auxline(1:118), *, err = 3030)
     +                               fidatawaset(i), fdbdry(i),
     +                               (fidata(j, i), j = 0, 3)
        enddo
      endif
c
      if (niidata .gt. 0) then
        do i = 1, niidata
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:66)
          call restring(auxline(1:66))
          read(auxline(1:66), *, err = 3030)
     +                               iidatawaset(i), idbdry(i),
     +                               (iidata(j, i), j = 0, 3)
        enddo
      endif
c
      if (nlidata .gt. 0) then
        do i = 1, nlidata
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:26)
          call restring(auxline(1:26))
          read(auxline(1:26), *, err = 3030)
     +             lidatawaset(i), (lidata(j, i), j = 0, 2)
        enddo
      endif
c
      if (nsidata .gt. 0) then
        do i = 1, nsidata
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:88)
          call restring(auxline(1:88))
          read(auxline(1:88), *, err = 3030)
     +             sidatawaset(i), (sidata(j, i), j = 0, 6)
        enddo
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           sidatastring(1:sidatalen)
        call restring(sidatastring(1:sidatalen))
      endif
c
      if (.not. after269) then
        thinningon = .true.
      endif
c
c     Site library.
c
      if (after120) then
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:11)
        call restring(auxline(1:11))
        read(auxline(1:11), *, err = 3030) nlibsites
c
        if (nlibsites .gt. 0) then
          do i = 0, nlibsites
            read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +               auxline(1:99)
            call restring(auxline(1:99))
            sitename(i) = auxline(1:16)
            read(auxline(17:99), *, err = 3030)
     +               sitenlen(i), sitelat(i), sitelong(i),
     +               siteground(i)
          enddo
        endif
      else
        nlibsites     = 0
        sitename(0)   = ' '
        sitenlen(0)   = 1
        sitelat(0)    = 0
        sitelong(0)   = 0
        siteground(0) = 0
      endif
c
c     Saving data related with "Special" particles.
c
      if (after215) then
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:11)
        call restring(auxline(1:11))
        read(auxline(1:11), *, err = 3030) nescpcles
c
        if (nescpcles .gt. 0) then
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:55)
          call restring(auxline(1:55))
          read(auxline(1:55), *, err = 3030)
     +             lastescpcle, itmp0, itmp1, itmp2, itmp3
          if (nescpcles .gt. itmp0)   goto 3010
          if (lastescpcle .gt. itmp2) goto 3010
          if (itmp1 .ne. pescode1)    goto 3010
          do i = 1, nescpcles
            read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +               auxline(1:115)
            call restring(auxline(1:115))
            escpclename(i) = auxline(1:16)
            read(auxline(17:115), *, err = 3030)
     +               escmacrover(i), escmacrouse(i),
     +               (escmacropos(j, i), j = 1, 4),
     +               (nsprimpart(j, i), j = 1, 3)
          enddo
          if (escmacropos(4, nescpcles) .gt. espms) goto 3010
            read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +               escpclemacro(1:escmacropos(4, nescpcles))
            call restring(escpclemacro(1:escmacropos(4, nescpcles)))
        endif
c
      else
        nescpcles   = 0
        lastescpcle = pescode1 - 1
      endif
c
      escmacropos(4, 0) = 0
c
c     Reading global variables.
c
      if (after23C) then
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:22)
        call restring(auxline(1:22))
        read(auxline(1:22), *, err = 3030) nglobar(1), nglobar(2)
c
        do j = 1, 2
          if (nglobar(j) .gt. 0) then
            if (nglobar(j) .gt. mxglobar) goto 3010
            do i = 1, nglobar(j)
              read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +                   globnam(i, j)(1:imxcdl), auxline(1:11)
              call restring(globnam(i, j))
              call restring(auxline(1:11))
              read(auxline(1:11), *, err = 3030) globlen(i, j)
            enddo
            do i = 0, nglobar(j)
              read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +                   auxline(1:11)
              call restring(auxline(1:11))
              read(auxline(1:11), *, err = 3030) globdfend(i, j)
            enddo
            read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +                 auxline(1:11)
            call restring(auxline(1:11))
            read(auxline(1:11), *, err = 3030) globstrlen(j)
            if (globstrlen(j) .gt. 0) then
              read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +                   globarstring(j)(1:globstrlen(j))
              call restring(globarstring(j)(1:globstrlen(j)))
            endif
          endif
        enddo
      else
        nglobar(1) = 0
        nglobar(2) = 0
      endif
c
      thereareglobars = ((nglobar(1) + nglobar(2)) .gt. 0)
c
c     Remarks.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         auxline(1:5)
      call restring(auxline(1:5))
      read(auxline(1:5), *, err = 3030) remark
c
      if (remark) then
c
c       Opening the remarks file.
c
        close(rmkut)
        open(rmkut, file = rmkfn, status = 'UNKNOWN',
     +              form = 'UNFORMATTED', err = 3020)
c
 1010   continue
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:11)
        call restring(auxline(1:11))
        read(auxline(1:11), *, err = 3030) i
        if (i .eq. -99999) goto 1020
        write(rmkut, err = 3020) i
c
        if (i .gt. 0) then
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             auxline(1:i)
          call restring(auxline(1:i))
          write(rmkut, err = 3020) auxline(1:i)
        endif
        goto 1010
 1020   continue
        close(rmkut)
c
      endif
c
c     Observing levels data.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         auxline(1:44)
      call restring(auxline(1:44))
      read(auxline(1:44), *, err = 3030)
     +         (iobslevdat(i), i = 0, 1), obslevset, nobslevelsp1
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         auxline(1:96)
      call restring(auxline(1:96))
      read(auxline(1:96), *, err = 3030)
     +         ((fobslevdat(i, j), i = 0, 1), j = 1, 2)
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         auxline(1:72)
      call restring(auxline(1:72))
      read(auxline(1:72), *, err = 3030)
     +         obslevstep, obslevminz, obslevmaxz
c
c     Markers for observing levels that are recorded into the
c     longitudinal file(s).
c
      if (nobslevels .gt. 0) then
c
        if (after140) then
          do i1 = 1, nobslevels, 50
            read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +               auxline(1:100)
            call restring(auxline(1:100))
            i2 = min(nobslevels, i1 + 49)
            read(auxline(1:100), 2070, err = 3030)
     +               (olfilesave(i), i = i1, i2)
          enddo
        else
          do i = 1, nobslevels
            olfilesave(i) = .true.
          enddo
        endif
      endif
 2070 format(50l2)
c
c     Skipping record of zeros (spare fields for future use).
c
      if (after120) then
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           auxline(1:110)
      endif
c
c     Setting "usedefaultd" and related variables.
c
      usedefaultd    = (tasknameset .le. 0)
      usedefault     = usedefaults .or. usedefaultd
      totshwset      = 1
      runshwset      = 1
      cpurunset      = 1
      processjobsset = 1
      pryeminset     = 1
c
c     No more header1 records.
c
      irc = 0
c
c     Calling the atmospheric model initializing routine.
c     (By default, the atmospheric model is set to the model used in
c     the last file opened).
c
      call atmosinit(max(1, atmoslabel), atmosmodel)
c
      return
c
c     Error messages.
c
 3010 continue
      close(ut)
      irc   = irc + 10000
      ifile = -1
      return
c
 3020 continue
c
      call errprint(0, '$A09', 4, 'ciogetheader1',
     +              ' ', 1, rmkut, 0, 0.d0, ' ')
      return
c
 3030 continue
      irc = 10
      return
c
      end
c     --- End of routine ciogetheader1.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciogetheader1.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
