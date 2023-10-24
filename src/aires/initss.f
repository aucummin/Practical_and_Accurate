c
c     FILE: initss.f                        Creation date: 23/NOV/2001.
c                                       LAST MODIFICATION: 18/JUN/2003.
c
c     Printing input data information in tss format.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine inptss(ut)
c
c     Writing a task summary script file.
c
c     Written by: S. J. Sciutto, La Plata 2001, 2002, 2003.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) Logical output unit, assumed as
c                      already opened.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'kernelpar.f'
      include 'ciopar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           ut
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'kernelcomm.f'
      include 'ciocomm.f'
      include 'cioauxcomm.f'
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*180     auxstring
      character*64      tssauxstr
      integer           nremlines
      integer           iauxv(6)
      integer           i, j, k, aux1, aux2, aux3
      equivalence       (aux1, iauxv(1))
      equivalence       (aux2, iauxv(2))
      equivalence       (aux3, iauxv(3))
      logical           numn, intn, lgsw
      double precision  ftmp1
      double precision  cvtfltdate
c
c     FIRST EXECUTABLE STATEMENT
c
c     Writing the summary of input data.
c
 2010 format(a, i6.6)
c
c     Writing the remarks.
c
      if (remark) then
c
        call writetsscomm(ut, 'Remarks')
c
c       Opening the remarks file.
c
        open(rmkut, file = rmkfn, status = 'OLD',
     +              form = 'UNFORMATTED', err = 1020)
c
        nremlines = 0
c
 1010   continue
        read(rmkut, err = 1020, end = 1030) aux1
        nremlines = nremlines + 1
c
        write(tssauxstr, 2010) 'Remark', nremlines
c
        if (aux1 .gt. 0) then
          read(rmkut, err = 1020, end = 1030) auxstring(1:aux1)
          call writetssstr(ut, tssauxstr(1:12), auxstring(1:aux1))
        else
          call writetssstr(ut, tssauxstr(1:12), ' ')
        endif
        goto 1010
c
 1020   continue
c
c       I/O error.
c
        call errprint(ut, '$A30', 3, 'initss', ' ',
     +                1, rmkut, 0, 0.d0, ' ')
c
 1030   continue
        close(rmkut)
c
        call writetssint(ut, 'RemarkLines', nremlines, 0)
c
      endif
c
      call writetsscomm(ut, 'Basic Input Parameters.')
c
      call strimcopy(sitename(isite), 16, auxstring, aux1)
      call writetssstr(ut, 'Site', auxstring(1:aux1))
      call writetssflt(ut, 'SiteLatitude', sitelat(isite), 0)
      call writetssflt(ut, 'SiteLongitude', sitelong(isite), 0)
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
      call writetssstr(ut, 'EventDate', auxstring(1:aux1))
c
      call writetsscomm(ut, ' ')
      call writetssint(ut, 'NumberOfDifferentPrimaries', nshprimary, 0)
c
      if (nshprimary .eq. 1) then
        call pnice(0, shprimary(1), auxstring, aux1)
        call writetssstr(ut, 'PrimaryParticle', auxstring(1:aux1))
        call writetssint(ut, 'PrimaryParticleCode', shprimary(1), 0)
      else if (nshprimary .gt. 1) then
        do i = 1, nshprimary
          call pnice(0, shprimary(i), auxstring, aux1)
          write(tssauxstr, 2010) 'PrimaryParticle', i
          call writetssstr(ut, tssauxstr(1:21), auxstring(1:aux1))
          write(tssauxstr, 2010) 'PrimaryParticleCode', i
          call writetssint(ut, tssauxstr(1:25), shprimary(i), 0)
          write(tssauxstr, 2010) 'PrimaryWeight', i
          call writetssflt(ut, tssauxstr(1:19), shprimarywt(i), 0)
        enddo
      else
        call writetssstr(ut, 'PrimaryParticle', 'NONE')
        call writetssint(ut, 'PrimaryParticleCode', 0, 0)
      endif
c
      if (pryeminset .gt. 0) then
        if (pryenergymax .le. pryenergymin) then
          call writetssflt(ut, 'PrimaryEnergy', pryenergymin, 0)
        else
          call writetssflt(ut, 'PrimaryEnergyFrom', pryenergymin, 0)
          call writetssflt(ut, 'PrimaryEnergyTo', pryenergymax, 0)
          call writetssflt(ut, 'SpectrumGamma', pryenergyslp, 0)
c
        endif
      else
        call writetssstr(ut, 'PrimaryEnergy', 'NONE')
      endif
c
      if (pryzenithmax .gt. pryzenithmin) then
        call writetssflt(ut, 'PrimaryZenAngleFrom', pryzenithmin, 0)
        call writetssflt(ut, 'PrimaryZenAngleTo', pryzenithmax, 0)
        if (varzendis .eq. 2) then
          auxstring = 'Sine-Cosine'
          aux1      = 11
        else
          auxstring = 'Sine'
          aux1      = 4
        endif
        call writetssstr(ut, 'ZenithAngleDistribution',
     +                      auxstring(1:aux1))
      else
        call writetssflt(ut, 'PrimaryZenAngle', pryzenithmin, 0)
      endif
c
      if (pryazimmax .gt. pryazimmin) then
        call writetssflt(ut, 'PrimaryAzimAngleFrom', pryazimmin, 0)
        call writetssflt(ut, 'PrimaryAzimAngleTo', pryazimmax, 0)
      else
        call writetssflt(ut, 'PrimaryAzimAngle', pryazimmin, 0)
      endif
      if (geognorth) then
        auxstring = 'Geographic'
        aux1      = 10
      else
        auxstring = 'Magnetic'
        aux1      = 8
      endif
      call writetssstr(ut, 'ZeroAzimDirection', auxstring(1:aux1))
c
      call writetsssw(ut, 'Thinning', thinningon)
      if (thinningon) then
        if (ethinpar .gt. 0) then
          call writetssflt(ut, 'ThinningEnergyAbs', ethinpar, 0)
        else
          call writetssflt(ut, 'ThinningEnergyRel', abs(ethinpar), 0)
        endif
      endif
c
      call writetssflt(ut, 'InjectionAltitude', injz, 0)
      call writetssflt(ut, 'InjectionDepth', injdepth, 0)
c
      call writetssflt(ut, 'GroundAltitude', groundz, 0)
      call writetssflt(ut, 'GroundDepth', groundepth, 0)
c
      call writetssflt(ut, 'FirstOLevAltitude', obslevmaxz, 0)
      call writetssflt(ut, 'FirstOLevDepth', obslevmind, 0)
      call writetssflt(ut, 'LastOLevAltitude', obslevminz, 0)
      call writetssflt(ut, 'LastOLevDepth', obslevmaxd, 0)
c
      call writetssint(ut, 'ObservingLevels', nobslevels, 0)
      call writetssflt(ut, 'OLDepthStep', obslevstep, 0)
c
      call writetsssw(ut, 'GeomagneticDeflections', (geobswitch .gt. 0))
      call writetssflt(ut, 'GeomagneticField', geob, 0)
      call writetssflt(ut, 'GeomagneticInclination', geobi, 0)
      call writetssflt(ut, 'GeomagneticDeclination', geobd, 0)
      call writetssflt(ut, 'GeomagneticFluctuation', geobfluc, 0)
c
      call writetssflt(ut, 'TableEnergyMin', eminhis, 0)
      call writetssflt(ut, 'TableEnergyMax', emaxhis, 0)
c
      call writetssflt(ut, 'TableRadialMin', rminhis, 0)
      call writetssflt(ut, 'TableRadialMax', rmaxhis, 0)
c
      call writetssint(ut, 'ParticleFiles', nciofilesu, 0)
c
      if (nciofilesu .gt. 0) then
        do i = 1, nciofilesu
          j = ciofilesu(i)
          k = 14 + 2 * j
          write(tssauxstr, 2010) 'ExtensionPFile', i
          call strimcopy(pofilext(j), 12, auxstring, aux1)
          call writetssstr(ut, tssauxstr(1:20), auxstring(2:aux1))
          write(tssauxstr, 2010) 'RadialMinPFile', i
          call writetssflt(ut, tssauxstr(1:20), fidata(0, k), 0)
          write(tssauxstr, 2010) 'RadialMaxPFile', i
          call writetssflt(ut, tssauxstr(1:20), fidata(0, k + 1), 0)
        enddo
      endif
c
c     Printing "not-hardwired" parameter info.
c
      if (ncommands .ge. ncommands0) then
c
        call writetsscomm(ut, 'Additional parameters (full list).')
c
        do i = ncommands0, ncommands
c
c         Determining the kind of magnitude.
c
          aux1 = (ccode(i) - 600) / 200
          numn = (aux1 .le. 4)
          intn = (aux1 .eq. 5)
          lgsw = (aux1 .eq. 6)
          j    = aditem(i)
c
          call strim(mxcdl, cname(i), aux2)
c
          if (numn) then
            call writetssflt(ut, cname(i)(1:aux2), fidata(0, j), 0)
          else if (intn) then
            call writetssint(ut, cname(i)(1:aux2), iidata(0, j), 0)
          else if (lgsw) then
            call writetsssw(ut, cname(i)(1:aux2), lidata(0, j))
          else
            call nthword(sidata(0, j),
     +                   sidatastring(sidata(4, j):sidata(5, j)),
     +                   sidatastring(sidata(3, j):sidata(3, j)),
     +                   auxstring, aux3)
            call writetssstr(ut, cname(i)(1:aux2), auxstring(1:aux3))
          endif
c
        enddo
      endif
c
c     Miscellaneous parameters.
c
      call writetsscomm(ut, 'Miscellaneous.')
c
      if ((inputrseed .gt. 0) .and. (inputrseed .lt. 1)) then
        write(auxstring, 2200) inputrseed
 2200   format(f14.12)
        aux2 = 14
        call snumtrim(auxstring, aux2)
        call writetssstr(ut, 'RandomSeed', auxstring(1:aux2))
      else
        call writetssstr(ut, 'RandomSeed', 'Automatic')
      endif
c
      call strimcopy(atmosmodel, 42, auxstring, aux1)
      call writetssstr(ut, 'Atmosphere', auxstring(1:aux1))
c
c     End of summary of input variables.
c
      return
      end
c     --- End of routine inptss.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'initss.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
