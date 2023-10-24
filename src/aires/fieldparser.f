c
c     FILE: fieldparser.f                                                                      Creation date: 18/4/2012
c                                                                                         LAST MODIFICATION:  28/5/2012
c
c      This file should have the routines that deal with setting (and possibly removing)
c      antennas, frequencies, viewing angles and viewing azimuths for the radio calculation
c      In principle, i will provide routines that will add one by one, and possibly a line and a circle
c      If it is easy to do, i will also have a clearAll.
c

c      double precision  freq(maxnu) !maxnu es el numero maximo de frecuencias posibles
c      double precision  frqcom(maxnu)
c      double precision  theta(maxj)
c      double precision  phi(maxk)

c      double precision xant(maxna) !maxna es el maximo numero de antenas posible
c      double precision yant(maxna) 
c      double precision zant(maxna)      
c      integer          numax       !numax es el maximo actual de freq
c      integer          kmax        !kmax es el maximo actual de phi!
c      integer          jend(maxnu) !
c      integer          jmax        !kmax es el maximo actual de angles, que es ademas el maximo de thetas
c      integer          namax       !namax es el maximo actual de numero de antena
c======================================================================================================================
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
      subroutine  antennaparser(cdline, llen, i1, i2, irc)
c
c     Processing directive AddAntenna
c
c     Written by: M. J. Tueros, Malargüe (in the bus), november 2011 .
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Position of first and
c                      last character of the last word scanned.
c     irc............. (output, integer) Return code.0= ok, 1= error , 2=ignored
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'hdatacomm.f'
      include 'fieldcomm.f'     
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, i3, itmp1, itmp2, nant
      double precision  ftmp1, x1, y1, z1, x2, y2, z2
      logical           alln, lnotset
      character*8       opkey
      double precision  x,y,z,modulus, increment, phi0, radius
      double precision  pi
      parameter         (pi = 3.1415926535897932385d0) 
c
c     FIRST EXECUTABLE STATEMENT
c
c     Directive is ignored if not called from simulation program(s).
c
      if (pgmcode .ge. 2000) return
c     test for fieldtool
      open(unit=311,status='old',file='fieldtool.inp',err=300)
c
      fieldtool=.true.
      close(311)
300   continue
c      
c     honor directive only if there is no fieldtool.inp (for back compatibility)      
      if (fieldtool) then
        write(*,*) "wwww"
        write(*,*) "wwww  fieldtool.inp is present, honoring it"
        write(*,*) "wwww  directive: ",cdline
        write(*,*) "wwww  will be ignored"
        write(*,*) "wwww"
        irc=2     
        return
      endif
c
c      write(*,*) "in antenna parser:",cdline
c
c
      if (i1 .le. i2) then
c
        i = max(2, min(8, i2 - i1 + 1))
c
c       Searching the "None" option.
c
        opkey = 'None'
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c          write(*,*) "cdline",cdline(i1:i2)
c
c         "None" option: Unsetting all antennas.
c
          do i = 1, maxna
            xant(i) = 0
            yant(i) = 0
            zant(i) = 0 
          enddo
c         update the antenna counter
          namax=0 
c
          irc=0
c          write(*,*) "sali por aca 120"
        return 
c
        endif !for the None test
c
c        
c       Look for the Line option         
c
        opkey = 'Line'
        i = max(2, min(8, i2 - i1 + 1))
c
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c         "Line" option. It expects x1 y1 z1 x1 y2 z2 Nant
c
c       Reading the options for the line (ver como hacer esto en fortran, usar getnumber y similares de stringutils.f), para ir recorrinedo cdline
          read(cdline(i1+5:llen), *, err = 3010) x1,y1,z1,x2,y2,z2,nant
c          write(*,*) "cdline ",cdline(i1+5:i2),x1,y1,z1,x2,y2,z2,nant
c
c       Cheking that there is enough room to set the antennas
          if(namax+nant.gt.maxna) then 
            goto 3020
          else if(nant.lt.2) then
            goto 3030
          else
c         calculate the distance and the versor
            modulus=(x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1)
            modulus=sqrt(modulus)
            x=(x2-x1)/modulus
            y=(y2-y1)/modulus
            z=(z2-z1)/modulus
            increment=modulus/(nant-1)
 
            do i = 1, nant
              xant(namax+i)=x1+x*increment*(i-1)
              yant(namax+i)=y1+y*increment*(i-1)
              zant(namax+i)=z1+z*increment*(i-1)  
            enddo
c           update the antenna counter
            namax=namax+nant 
c      write(*,*) "Line created:",namax,xant(1:namax),yant(1:namax)
          endif
c
          irc=0
c          write(*,*) "sali por aca 162"
          return
        endif !end line specificationline
c
        opkey = 'Ring'
        i = max(2, min(8, i2 - i1 + 1))
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c       "Ring" option. It expects x, y, z, radius [m], phi0 [deg] and nant
c
c       Reading the options for the line (ver como hacer esto en fortran, usar getnumber y similares de stringutils.f), para ir recorrinedo cdline
          read(cdline(i1+5:llen),*,err=3010) x1,y1,z1,radius,phi0,nant
c       Cheking that there is enough room to set the antennas
          if(namax+nant.gt.maxna) then 
            goto 3020
          elseif(nant.lt.2) then
            goto 3030
          else
c       Creating the ring
            phi0=phi0*pi/180.0d0  
            increment=2.0d0*pi/(nant)
            do i = 1, nant
              xant(namax+i)=x1+radius*cos(phi0+increment*(i-1))
              yant(namax+i)=y1+radius*sin(phi0+increment*(i-1))
              zant(namax+i)=z1
            enddo
c           update the antenna counter
            namax=namax+nant 

c      write(*,*) "Ring created:",namax,xant(1:namax),yant(1:namax)
          endif !del else
          irc=0
c          write(*,*) "sali por aca 195"
          return          
        endif !del ring  
      
      endif !del keyword search
c
c     If we are here, no keywords matched, thus we asume we are receiving an individual antenna.
c
c     Getting the X
c
      call getnumber(.false., cdline, llen, i1, i2,
     +               0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
       x1=ftmp1
      else
          call errprint(0, '$A20', idlerrsev, 'antennaparser',
     +                  cdline(1:llen),
     +                  1, itmp2, 0, 0.d0, ' ')
       irc=1
c       write(*,*) "sali por aca 213"
       return
      endif
c
c     Getting the y
c
      call getnumber(.true., cdline, llen, i1, i2,
     +                 0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
       y1=ftmp1
      else
        call errprint(0, '$A20', idlerrsev, 'antennaparser',
     +                    cdline(1:llen),
     +                    1, itmp2, 0, 0.d0, ' ')
        irc=1
c        write(*,*) "sali por aca 228"
        return
      endif
c
c     Getting the z
c
      call getnumber(.true., cdline, llen, i1, i2,
     +                 0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
       z1=ftmp1
      else
        call errprint(0, '$A20', idlerrsev, 'antennaparser',
     +                    cdline(1:llen),
     +                    1, itmp2, 0, 0.d0, ' ')
        irc=1
        write(*,*) "sali por aca 243"
        return
      endif
c
c     Setting the indicated antenna
c
c     Cheking that there is enough room to set the antennas
      if(namax+1.gt.maxna) then 
        goto 3020
      else 
        xant(namax+1)=x1
        yant(namax+1)=y1
        zant(namax+1)=z1
        namax=namax+1
c      write(*,*) "Antenna added:",namax,xant(1:namax),yant(1:namax)
      endif
      irc=0
c
c      write(*,*) "sali por aca 260"
      return
c
c     Other error conditions.
c
 3010 continue
      call errprint(0, '$A12', idlerrsev, 'antennaparser', ' ',
     +              0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 269"
      return
c
 3020 continue
      call errprint(0, '$A12', idlerrsev, 'antennaparser', 
     + 'Not enough antenna slots left to comply with directive:',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 277"
      return
c
 3030 continue
      call errprint(0, '$A12', idlerrsev, 'antennaparser', 
     + 'You must specify at least 2 antennas for this directive',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 285"
      return
 
      end
c     --- End of routine antennaparser.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
      subroutine  frequencyparser(cdline, llen, i1, i2, irc)
c
c     Processing directive AddFrequency. Frequencies are expected in Mhz
c
c     Written by: M. J. Tueros, Malargüe (in bed), november 2011 .
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Position of first and
c                      last character of the last word scanned.
c     irc............. (output, integer) Return code.0= ok, 1= error , 2=ignored
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'hdatacomm.f'
      include 'fieldcomm.f'     
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, i3, itmp1, itmp2, nfreq
      double precision  ftmp1, fmin, fmax
      logical           alln, lnotset
      character*8       opkey
      double precision  logfreq, increment, logfmax,logfmin
c
c     FIRST EXECUTABLE STATEMENT
c
c     Directive is ignored if not called from simulation program(s).
c
      if (pgmcode .ge. 2000) return
c      
c     test for fieldtool
      open(unit=311,status='old',file='fieldtool.inp',err=300)
c
      fieldtool=.true.
      close(311)
300   continue
c     honor directive only if there is no fieldtool.inp (for back compatibility)      
      if (fieldtool) then
        write(*,*)"wwww"
        write(*,*)"wwww  fieldtool.inp is present, honoring it"
        write(*,*)"wwww  directive: ",cdline
        write(*,*)"wwww  will be ignored"
        write(*,*)"wwww"
        irc=2     
        return
      endif
c
c      write(*,*) "in frequency parser:",cdline
c
      if (i1 .le. i2) then
c
        i = max(2, min(8, i2 - i1 + 1))
c
c       Searching the "None" option.
c
        opkey = 'None'
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c         "None" option: Unsetting all frequencies.
c
          do i = 1, maxnu
           	freq(i)=0 
          enddo
c       update the antenna counter
          numax=0 
          irc=0
c      write(*,*) "sali por aca 388"
          return 
        endif !for the None test
c
c        
c       Look for the Log option         
c
        opkey = 'Log'
        i = max(2, min(8, i2 - i1 + 1))
c
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c         "Log" option. It expects fmin fmax Nfreq
c
c       Reading the options for the line (ver como hacer esto en fortran, usar getnumber y similares de stringutils.f), para ir recorrinedo cdline
          read(cdline(i1+4:llen), *, err = 3010) fmin,fmax,nfreq
c       Cheking that there is enough room to set the frequencies
          if(numax+nfreq.gt.maxnu) then 
            goto 3020
          else if(nfreq.lt.2) then
            goto 3030
          else
c         calculate the distance and the versor
            logfmin=log(fmin)
            logfmax=log(fmax)
            increment=(log(fmax)-log(fmin))/(nfreq-1)
c 
            do i = 1, nfreq
              logfreq=logfmin+(i-1)*increment
              freq(numax+i)=exp(logfreq)
            enddo
c           update the frequency counter
            numax=numax+nfreq 
          endif
c
          irc=0
c      write(*,*) "sali por aca 425"
          return
        endif !end line specificationline
c
        opkey = 'Linear'
        i = max(2, min(8, i2 - i1 + 1))
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c       "Linear" option. It expects fmin, fmax, and nfreq
c
c       Reading the options for the line (ver como hacer esto en fortran, usar getnumber y similares de stringutils.f), para ir recorrinedo cdline
          read(cdline(i1+7:llen),*,err=3010) fmin,fmax,nfreq
c       Cheking that there is enough room to set the antennas
          if(numax+nfreq.gt.maxnu) then 
            goto 3020
          elseif(nfreq.lt.2) then
            goto 3030
          else
c       Creating the frequencies
            increment=(fmax-fmin)/(nfreq-1)
            do i = 1, nfreq
              freq(numax+i)=fmin+(i-1)*increment
            enddo
c           update the frequency counter
            numax=numax+nfreq 
          endif !del else
          irc=0
c      write(*,*) "sali por aca 453"
          return
        endif !del linear
      
      endif !del keyword search
c
c     If we are here, no keywords matched, thus we asume we are receiving an individual frequency.
c
c     Getting the freq
c
      call getnumber(.false., cdline, llen, i1, i2,
     +               0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
c
c     Setting the indicated frequency
c
c     Cheking that there is enough room to set the frequency
        if(numax+1.gt.maxnu) then 
          goto 3020
        else 
          freq(numax+1)=ftmp1
          numax=numax+1
        endif
        irc=0
c
c      write(*,*) "sali por aca 479"
        return
c
      else
        call errprint(0, '$A20', idlerrsev, 'frequencyparser',
     +                  cdline(1:llen),
     +                  1, itmp2, 0, 0.d0, ' ')
       irc=1
c      write(*,*) "sali por aca 487"
       return
      endif
c
c     Other error conditions.
c
 3010 continue
      call errprint(0, '$A12', idlerrsev, 'frequencyparser', ' ',
     +              0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 497"
      return
c
 3020 continue
      call errprint(0, '$A12', idlerrsev, 'frequencyparser', 
     + 'Not enough frequency slots left to comply with directive:',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 505"
      return
c
 3030 continue
      call errprint(0, '$A12', idlerrsev, 'frequencyparser', 
     + 'You must specify at least 2 frequencies for this directive',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 513"
      return
 
      end
c     --- End of routine frequencyparser.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
      subroutine  fraunhfrangleparser(cdline, llen, i1, i2, irc)
c
c     Processing directive AddFraunhofrAngle. Angles are expected in degrees
c     and left in degrees. The initialization function leaves them in degrees
c
c
c     Written by: M. J. Tueros, Malargüe, november 2011 .
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Position of first and
c                      last character of the last word scanned.
c     irc............. (output, integer) Return code.0= ok, 1= error , 2=ignored
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'hdatacomm.f'
      include 'fieldcomm.f'     
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, i3, itmp1, itmp2, nangles
      double precision  ftmp1, amin, amax
      logical           alln, lnotset
      character*8       opkey
      double precision  increment
c
c     FIRST EXECUTABLE STATEMENT
c
c     Directive is ignored if not called from simulation program(s).
c
      if (pgmcode .ge. 2000) return
c     
c     test for fieldtool
      open(unit=311,status='old',file='fieldtool.inp',err=300)
c
      fieldtool=.true.
      close(311)
300   continue
c
c     honor directive only if there is no fieldtool.inp (for back compatibility)      
      if (fieldtool) then
        write(*,*) "wwww"
        write(*,*) "wwww  fieldtool.inp is present, honoring it"
        write(*,*) "wwww  directive: ",cdline
        write(*,*) "wwww  will be ignored"
        write(*,*) "wwww"
        irc=2     
        return
      endif

c
c      write(*,*) "in fraunangle parser:",cdline
c
      if (i1 .le. i2) then
c
        i = max(2, min(8, i2 - i1 + 1))
c
c       Searching the "None" option.
c
        opkey = 'None'
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c         "None" option: Unsetting all angles.
c
          do i = 1, maxj
           angles(i)=0 
          enddo
c         update the angles counter
          jmax=0 
          irc=0
          return 
c      write(*,*) "sali por aca 610"
        endif !for the None test
c
c        
        opkey = 'Linear'
        i = max(2, min(8, i2 - i1 + 1))
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c      write(*,*) "procesing Linear"
c
c       "Linear" option. It expects amin, amax, and nangles
c
c       Reading the options for the line (ver como hacer esto en fortran, usar getnumber y similares de stringutils.f), para ir recorrinedo cdline
          read(cdline(i1+7:llen),*,err=3010) amin,amax,nangles
c        write(*,*) "amin ",amin," amax ",amax," nangles ",nangles
c       Cheking that there is enough room to set the angles
          if(jmax+nangles.gt.maxj) then 
            goto 3020
          elseif(nangles.lt.2) then
            goto 3030
          else
c       Creating the angles
            increment=(amax-amin)/(nangles-1)
            do i = 1, nangles
              angles(jmax+i)=amin+(i-1)*increment
            enddo
c           update the angle counter
            jmax=jmax+nangles 
          endif !del else
          irc=0
c      write(*,*) "sali por aca 639"
          return
        endif !del linear  
      
      endif !del keyword search
c
c     If we are here, no keywords matched, thus we asume we are receiving an individual angle.
c
c     Getting the angle
c
      call getnumber(.false., cdline, llen, i1, i2,
     +               0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
c
c     Setting the indicated angle
c
c     Cheking that there is enough room to set angle
        if(jmax+1.gt.maxj) then 
          goto 3020
        else 
          angles(jmax+1)=ftmp1
          jmax=jmax+1
        endif
        irc=0
c
c      write(*,*) "sali por aca 665"
        return
c
      else
        call errprint(0, '$A20', idlerrsev, 'fraunhfrangleparser',
     +                  cdline(1:llen),
     +                  1, itmp2, 0, 0.d0, ' ')
        irc=1
c      write(*,*) "sali por aca 673"
        return
      endif
c
c     Other error conditions.
c
 3010 continue
      call errprint(0, '$A12', idlerrsev, 'fraunhfrangleparser', ' ',
     +              0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 683"
      return
c
 3020 continue
      call errprint(0, '$A12', idlerrsev, 'fraunhfrangleparser', 
     + 'Not enough angle slots left to comply with directive:',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 691"
      return
c
 3030 continue
      call errprint(0, '$A12', idlerrsev, 'fraunhfrangleparser', 
     + 'You must specify at least 2 angles for this directive',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 699"
      return
 
      end
c     --- End of routine fraunhfrangleparser.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
      subroutine  fraunhfrphiparser(cdline, llen, i1, i2, irc)
c
c     Processing directive AddFraunhofrPhi. Angles are expected in degrees
c     and left in degrees, as then the initialization function converts them to rad
c     MATIAS: It might be desired to modifiy the initialization to leave the input untouched
c
c     Written by: M. J. Tueros, Malargüe, november 2011 .
c
c
c     Arguments:
c     =========
c
c     cdline.......... (input, character*(*)) The command line
c                      (complete)
c     llen............ (input, integer) Length of cdline.
c     i1, i2.......... (input-output, integer) Position of first and
c                      last character of the last word scanned.
c     irc............. (output, integer) Return code.0= ok, 1= error , 2=ignored
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      character*(*)     cdline
      integer           llen, i1, i2, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'maincomm.f'
      include 'hdatacomm.f'
      include 'fieldcomm.f'     
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, i3, itmp1, itmp2, nangles
      double precision  ftmp1, amin, amax
      logical           alln, lnotset
      character*8       opkey
      double precision  increment
c
c     FIRST EXECUTABLE STATEMENT
c
c     Directive is ignored if not called from simulation program(s).
c
      if (pgmcode .ge. 2000) return
c      
c     test for fieldtool
      open(unit=311,status='old',file='fieldtool.inp',err=300)
c
      fieldtool=.true.
      close(311)
300   continue
c
c     honor directive only if there is no fieldtool.inp (for back compatibility)      
      if (fieldtool) then
        write(*,*) "wwww"
        write(*,*) "wwww  fieldtool.inp is present, honoring it"
        write(*,*) "wwww  directive: ",cdline
        write(*,*) "wwww  will be ignored"
        write(*,*) "wwww"
        irc=2     
        return
      endif
c
c      write(*,*) "en fraunhfrphiparser:",cdline
c
      if (i1 .le. i2) then
c
        i = max(2, min(4, i2 - i1 + 1))
c
c       Searching the "None" option.
c
        opkey = 'None'
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c         "None" option: Unsetting all angles.
c
          do i = 1, maxk
           phi(i)=0 
          enddo
c         update the angles counter
          kmax=0 
          irc=0
c        write(*,*) "sali por aca 797"
        return 

        endif !for the None test
c
c        
c
        opkey = 'Linear'
        i = max(2, min(8, i2 - i1 + 1))
c
        if (cdline(i1:i2) .eq. opkey(1:i)) then
c
c       "Linear" option. It expects amin, amax, and nangles
c
c       Reading the options for the line (ver como hacer esto en fortran, usar getnumber y similares de stringutils.f), para ir recorrinedo cdline
          read(cdline(i1+7:llen),*,err=3010) amin,amax,nangles
c       Cheking that there is enough room to set the angles
          if(kmax+nangles.gt.maxk) then 
            goto 3020
          elseif(nangles.lt.2) then
            goto 3030
          else
c       Creating the angles
            increment=(amax-amin)/(nangles-1)
            do i = 1, nangles
              phi(kmax+i)=amin+(i-1)*increment
            enddo
c           update the angle counter
            kmax=kmax+nangles 
          endif !del else
          irc=0
c          write(*,*) "sali por aca 826"
          return
        endif !del linear  
      
      endif !del keyword search
c
c     If we are here, no keywords matched, thus we asume we are receiving an individual angle.
c
c     Getting the angle
c
      call getnumber(.false., cdline, llen, i1, i2,
     +               0, 0.0d0, ftmp1, itmp1)
c
      if (itmp1 .eq. 0) then
c
c     Setting the indicated angle
c
c     Cheking that there is enough room to set angle
        if(kmax+1.gt.maxk) then 
          goto 3020
        else  
          phi(kmax+1)=ftmp1
          kmax=kmax+1
        endif
        irc=0
c
c       write(*,*) "sali por aca 852"
        return
c
      else
        call errprint(0, '$A20', idlerrsev, 'fraunhfrphiparser',
     +                  cdline(1:llen),
     +                  1, itmp2, 0, 0.d0, ' ')
       irc=1
c      write(*,*) "sali por aca 860"
       return
      endif
c
c     Other error conditions.
c
 3010 continue
      call errprint(0, '$A12', idlerrsev, 'fraunhfrphiparser', ' ',
     +              0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 870"
      return
c
 3020 continue
      call errprint(0, '$A12', idlerrsev, 'fraunhfrphiparser', 
     + 'Not enough angle slots left to comply with directive:',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 878"
      return
c
 3030 continue
      call errprint(0, '$A12', idlerrsev, 'fraunhfrphiparser', 
     + 'You must specify at least 2 angles for this directive',
     +  0, 0, 0, 0.d0, cdline(1:llen))
      irc = 1
c      write(*,*) "sali por aca 886"
      return
 
      end
c     --- End of routine fraunhfrphiparser.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->

      MODULE Dyn_Array
      !---------------------------------------------------------------------
      !
      !  Module containing definitions needed to dynamically allocate 
      !  the values of an array 
      !
      !---------------------------------------------------------------------

      implicit none
c
      save
c     The definitons below replace the common blocks       
c      common           /field_sum/ zsum2, zsum2_x, zsum2_y, zsum2_z, 
c     +     asum_x, asum_y, asum_z
c      common           /field_sum_fr/ zsum2_x_fr, zsum2_y_fr, 
c     +     zsum2_z_fr, asum_x_fr, asum_y_fr, asum_z_fr
c
c     Field matrix
c      complex*16        zsum2(maxj,maxk,maxnu)
c      complex*16        zsum2_x(maxj,maxk,maxnu)
c      complex*16        zsum2_y(maxj,maxk,maxnu)
c      complex*16        zsum2_z(maxj,maxk,maxnu)
      complex*16, DIMENSION(:), allocatable :: zsum2(:,:,:)
      complex*16, DIMENSION(:), allocatable :: zsum2_x(:,:,:)
      complex*16, DIMENSION(:), allocatable :: zsum2_y(:,:,:)
      complex*16, DIMENSION(:), allocatable :: zsum2_z(:,:,:)

c      complex*16        zsum2_x_fr(maxna,maxnu)
c      complex*16        zsum2_y_fr(maxna,maxnu)
c      complex*16        zsum2_z_fr(maxna,maxnu)
      complex*16, DIMENSION(:), allocatable :: zsum2_x_fr(:,:)
      complex*16, DIMENSION(:), allocatable :: zsum2_y_fr(:,:)
      complex*16, DIMENSION(:), allocatable :: zsum2_z_fr(:,:)

c     Vector potential matrix
c      double precision  asum_x(maxt,maxj,maxk)
c      double precision  asum_y(maxt,maxj,maxk)
c      double precision  asum_z(maxt,maxj,maxk)
      double precision, DIMENSION(:), allocatable :: asum_x(:,:,:)
      double precision, DIMENSION(:), allocatable :: asum_y(:,:,:)
      double precision, DIMENSION(:), allocatable :: asum_z(:,:,:)
c
c      double precision  asum_x_fr(maxt,maxna)
c      double precision  asum_y_fr(maxt,maxna)
c      double precision  asum_z_fr(maxt,maxna)
      double precision, DIMENSION(:), allocatable :: asum_x_fr(:,:) 
      double precision, DIMENSION(:), allocatable :: asum_y_fr(:,:)
      double precision, DIMENSION(:), allocatable :: asum_z_fr(:,:)


      END MODULE Dyn_Array
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->     
c     
      subroutine fieldmemalloc()
c
c     Allocating memmory for the arrays used by the ZHS algorithms.
c     NOTE: This is FORTRAN90. this routine makes this version of ZHAireS 
c     incompatible with FORTRAN77, but much more efficient in memmory usage.
c
c     Written by: M. J. Tueros, Santiago de Compostela 2012.
c
c     Arguments:
c     =========
c
c     None (all input values should have already been initialized)
c
c
      use Dyn_Array
      implicit none
c
c     Compilation parameters.
c
c
c     Declaration of shared data.
c
      include 'fieldcomm.f'
c
c     Declaration of arguments.
c
c     
c     Declaration of internal variables
      integer ierr,nu,k,j,na,t 
      integer newtotal,oldtotal
c
c     FIRST EXECUTABLE STATEMENT
c
c      complex*16        zsum2(maxj,maxk,maxnu)
c      complex*16        zsum2_x(maxj,maxk,maxnu)
c      complex*16        zsum2_y(maxj,maxk,maxnu)
c      complex*16        zsum2_z(maxj,maxk,maxnu)
      oldtotal=0
      newtotal=0
      write(*,*) "     allocating jmax",jmax,"kmax",kmax,"numax",numax,
     +                                  "namax",namax,"ntbins",ntbins
c      write(*,*) "instead of  maxj",maxj,"maxk",maxk,"maxnu",maxnu,
c     +                                  "maxna",maxna,"maxt",maxt
c      write(*,*) "alloc zsum2",jmax,kmax,numax,":",jmax*numax*kmax
      if(calcfreq) then 
        allocate(zsum2(jmax,kmax,numax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating zsum2"
c
c      write(*,*) "alloc zsum2_x",jmax,kmax,numax,":",jmax*numax*kmax
        allocate(zsum2_x(jmax,kmax,numax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating zsum2_x"
c
c      write(*,*) "alloc zsum2_y",jmax,kmax,numax,":",jmax*numax*kmax
        allocate(zsum2_y(jmax,kmax,numax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating zsum2_y"
c
c       write(*,*) "alloc zsum2_z",jmax,kmax,numax,":",jmax*numax*kmax
        allocate(zsum2_z(jmax,kmax,numax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating zsum2_z"
        newtotal=newtotal+4*jmax*kmax*numax
      endif
      oldtotal=oldtotal+maxj*maxk*maxnu
c
c      complex*16        zsum2_x_fr(maxna,maxnu)
c      complex*16        zsum2_y_fr(maxna,maxnu)
c      complex*16        zsum2_z_fr(maxna,maxnu)
c      write(*,*) "alloc zsum2_x_fr",namax,numax,":",namax*numax
      if(calcfreqfres) then
        allocate(zsum2_x_fr(namax,numax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating zsum2_x_fr"
c
c       write(*,*) "alloc zsum2_y_fr",namax,numax,":",namax*numax
        allocate(zsum2_y_fr(namax,numax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating zsum2_y_fr"
c
c        write(*,*) "alloc zsum2_z_fr",namax,numax,":",namax*numax
        allocate(zsum2_z_fr(namax,numax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating zsum2_z_fr"
        newtotal=newtotal+3*namax*numax
      endif
      oldtotal=oldtotal+3*maxna*maxnu
c      double precision  asum_x(maxt,maxj,maxk)
c      double precision  asum_y(maxt,maxj,maxk)
c      double precision  asum_z(maxt,maxj,maxk)
      if(calctime) then
c      write(*,*) "alloc asum_x",ntbins,jmax,kmax,":",ntbins*jmax*kmax
        allocate(asum_x(ntbins,jmax,kmax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating asum_x"
c
c      write(*,*) "alloc asum_y",ntbins,jmax,kmax,":",ntbins*jmax*kmax
        allocate(asum_y(ntbins,jmax,kmax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating asum_y"
c
c      write(*,*) "alloc asum_z",ntbins,jmax,kmax,":",ntbins*jmax*kmax
        allocate(asum_z(ntbins,jmax,kmax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating asum_z"
        newtotal=newtotal+3*ntbins*jmax*kmax
      endif
      oldtotal=oldtotal+3*maxt*maxj*maxk
c
c      double precision  asum_x_fr(maxt,maxna)
c      double precision  asum_y_fr(maxt,maxna)
c      double precision  asum_z_fr(maxt,maxna)
      if(calctimefres) then
c        write(*,*) "alloc asum_x_fr",ntbins,namax,":",ntbins*namax
        allocate(asum_x_fr(ntbins,namax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating asum_x_fr"
c
c        write(*,*) "alloc asum_y_fr",ntbins,namax,":",ntbins*namax
        allocate(asum_y_fr(ntbins,namax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating asum_y_fr"
c
c        write(*,*) "alloc asum_z_fr",ntbins,namax,":",ntbins*namax
        allocate(asum_z_fr(ntbins,namax),stat=ierr) !stat= allows for an error code return
        if(ierr.ne.0) stop "error allocating asum_z_fr"
        newtotal=newtotal+3*ntbins*namax
       endif
       oldtotal=oldtotal+3*maxt*maxna
c
c     Initializing zsum
c      write(*,*) "testing allocation by zeroing things"
      if(calcfreq) then
        do j=1,jmax
           do nu=1,numax
              do k=1,kmax
                zsum2(j,k,nu)=(0.d0,0.d0)
                zsum2_x(j,k,nu)=(0.d0,0.d0)
                zsum2_y(j,k,nu)=(0.d0,0.d0)
                zsum2_z(j,k,nu)=(0.d0,0.d0)
              enddo
           enddo
        enddo
      endif
c
c     Initializing asum_x,y,z
      if(calctime) then
        do t=1,ntbins
           do j=1,jmax
              do k=1,kmax
                 asum_x(t,j,k)=0.d0
                 asum_y(t,j,k)=0.d0
                 asum_z(t,j,k)=0.d0
              enddo
           enddo
        enddo
      endif
c
c     Initializing asum_xyz_fr 
      if(calctimefres) then
        do t=1,ntbins
           do na=1,namax
              asum_x_fr(t,na)=0.d0
              asum_y_fr(t,na)=0.d0
              asum_z_fr(t,na)=0.d0
           enddo
        enddo
      endif
c
c     Initializing zsum2_xyz_fr
      if(calcfreqfres) then
        do nu=1,numax
           do na=1,namax
              zsum2_x_fr(na,nu)=(0.d0,0.d0)
              zsum2_y_fr(na,nu)=(0.d0,0.d0)
              zsum2_z_fr(na,nu)=(0.d0,0.d0)
           enddo
        enddo
      endif
c     
      write(*,*) "ZHAireS:Dynamic memmory allocated succsesfully"
c      write(*,*) "previous ZHS version would use",
c     +                               oldtotal*8.0/(1024*1024),"MB"
      write(*,*) "the arrays require ",newtotal*8.0/(1024*1024),"MB"
      end
c     --- End of routine fieldmemalloc.                                       
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
      subroutine fieldmemdealloc()                                 
c     De-Allocating memmory for the arrays used by the ZHS algorithms.
c     NOTE: This is FORTRAN90. this routine makes this version of ZHAireS 
c     incompatible with FORTRAN77, but much more efficient in memmory usage.
c
c     Written by: M. J. Tueros, Santiago de Compostela 2012.
c
c     SPECIAL NOTE: SINCE THE ARRAY INITIALIZATION IS DONE ONLY ONCE, MEMORY
C     IS NOT BEING DEALLOCATED AND THE ARRAY IS ZEROED AFTER THE FILES ARE WRITEN
c
c     Arguments:
c     =========
c
c     None (all input values should have already been initialized)
c
      use Dyn_Array
      implicit none
c
c     Compilation parameters.
c
c
c
c     Declaration of shared data.
c
      include 'fieldcomm.f'
c
c     Declaration of arguments.
c
c     
c     Declaration of internal variables
      integer ierr 
c
c     FIRST EXECUTABLE STATEMENT
c      complex*16        zsum2(maxj,maxk,maxnu)
c      complex*16        zsum2_x(maxj,maxk,maxnu)
c      complex*16        zsum2_y(maxj,maxk,maxnu)
c      complex*16        zsum2_z(maxj,maxk,maxnu)
      if(calcfreq) then
        DEALLOCATE( zsum2, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating zsum2***"
        DEALLOCATE( zsum2_x, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating zsum2_x***"
        DEALLOCATE( zsum2_y, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating zsum2_y***"
        DEALLOCATE( zsum2_z, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating zsum2_z***"
      endif
c
c      complex*16        zsum2_x_fr(maxna,maxnu)
c      complex*16        zsum2_y_fr(maxna,maxnu)
c      complex*16        zsum2_z_fr(maxna,maxnu)
      if(calcfreqfres) then
        DEALLOCATE( zsum2_x_fr, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating zsum2_x_fr***"
        DEALLOCATE( zsum2_y_fr, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating zsum2_y_fr***"
        DEALLOCATE( zsum2_z_fr, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating zsum2_z_fr***"
      endif
c
c      double precision  asum_x(maxt,maxj,maxk)
c      double precision  asum_y(maxt,maxj,maxk)
c      double precision  asum_z(maxt,maxj,maxk)
      if(calctime) then
        DEALLOCATE( asum_x, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating asum_x***"
        DEALLOCATE( asum_y, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating asum_y***"
        DEALLOCATE( asum_z, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating asum_z***"
      endif
c
c      double precision  asum_x_fr(maxt,maxna)
c      double precision  asum_y_fr(maxt,maxna)
c      double precision  asum_z_fr(maxt,maxna)
      if(calctimefres) then
        DEALLOCATE( asum_x_fr, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating asum_x_fr***"
        DEALLOCATE( asum_y_fr, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating asum_y_fr***"
        DEALLOCATE( asum_z_fr, STAT = ierr)
        IF (ierr.ne.0) STOP "*** Trouble deallocating asum_z_fr***"
      endif
c
      write(*,*) "memory deallocated sucsessfully"
      end
c     --- End of routine fieldmemdealloc. 
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->                             
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->

      subroutine zhairestatus(ou, nlines)
c
c     Printing the current zhaires configuration (list antennas, frequencies angles, time bins, etc)
c
c     Written by: M. J. Tueros, Santiago de Compostela 2011.
c
c
c     Arguments:
c     =========
c
c     ou.............. (input, integer) I/O unit where to print the
c                      logo.
c     nlines.......... (output, integer) Number of written lines.
c
c
      implicit none
c
c     Compilation parameters.
c
c
c
c     Declaration of shared data.
c
      include 'fieldcomm.f'
c     for injz
      include 'initpar.f'
      include 'initcomm.f'      
c
c     Declaration of arguments.
c
      integer  na, ou
c     
c     Declaration of internal variables
      integer      nlines
      character*80 line
c
c     FIRST EXECUTABLE STATEMENT
c
c      write(*,*) "zhaireStatus writing to",ou,calcfield
c      write(ou,*) "this is ZHAireS Status writing to",ou,calcfield
c     6 standard output,7 lgf file, standard output, 8, sry file
c
      nlines=0
      if(calcfield) then !radio is on
c
        write(ou,*) " "
        write(ou,*) "   ZHAireS CONFIGURATION:"
        write(ou,*) " "
        nlines=nlines+3

        if(fieldtool) then
          write(ou,*)" Atention!:"
          write(ou,*)" fieldtool.inp file was present, honoring it"
          write(ou,*)" to ensure backwards compatibility."
          write(ou,*)" ZHAireS configuration from Aires input file"
          write(ou,*)" was ignored. This is the current configuration:"
          write(ou,*)" "      
          nlines=nlines+6
        endif
c
c     Refraction index
        write(ou,*) " "
        write(ou,*) "      Refraction index at sea level:",ref_n
        nlines=nlines+3
        if(usevarn.eqv..false.) then
        write(ou,*) "      Refraction index is constant at all heights"
          nlines=nlines+1
        endif
c
c energy loss
        if(usezhseloss) then 
          write(ou,*)" "
          write(ou,*)"      Using ZHS ICE energy deposit"
          nlines=nlines+2
        else 
          write(ou,*)" "
          write(ou,*)"      Using AIRES standard energy deposit"
          nlines=nlines+2
        endif
c
c include/exclude hadrons
        if(countpimu) then 
          write(ou,*) " "
          write(ou,*) "      Including the emission from hadrons"
          write(ou,*) "        fpicut=",fpicut,"MeV"
          write(ou,*) "        fmucut=",fmucut,"MeV"
          write(ou,*) "        fpcut=",fpcut,"MeV"
          nlines=nlines+5
        else 
          write(ou,*)" "
          write(ou,*) "      Ignoring the emission from hadrons"
          nlines=nlines+2
        endif

        if(calcfreqfres) then
          write(ou,*)""
          write(ou,*)"      Fresnel in the frequency domain ENABLED"
          nlines=nlines+2
        endif

        if(calctimefres) then
          write(ou,*)" "
          write(ou,*)"      Fresnel in the time domain ENABLED"
          nlines=nlines+2
        endif

        if(calcfreqfres.or.calctimefres) then !print the antenna info
          if(namax<1) then
          write(ou,*)"WARNING: You specified to do calculations in the"
          write(ou,*)"fresnel regime but no antennas were defined"
          write(ou,*)"use the AddAntena command"
          nlines=nlines+3
          else 
            write(ou,*)" "
            write(ou,*)"       Antennas used for the Fresnel formalism"
            write(ou,*)"       (with height expressed above sea level)"
            write(ou,*)"       Antenna|   X [m]   |   Y [m]   |   Z [m]
     +  |  t0 [ns]" 
            nlines=nlines+3
            do na=1,namax
          write(ou,1001) na,xant(na),yant(na),injz-zant(na),dt(na)*1.d9
              nlines=nlines+1 
            enddo
          endif
        endif
 1001   format('      ',i3,'   ',F10.2,'   ',F10.2,'   ',F10.2,
     +         '   ',F10.2)

        if(calcfreqfres.or.calcfreq) then !print the frequency info
          if(numax<1) then
            write(ou,*)"WARNING: You specified to do calculations in"
            write(ou,*)"frequency but no frequencies were defined"
            write(ou,*)"use the AddFrequency command"
            nlines=nlines+3          
          else
            write(ou,*)" "
            write(ou,*)"      Requested Frequencies"
            write(ou,*)"      Frequency | [Mhz] "
            nlines=nlines+3
            do na=1,numax
              write(ou,1002) na,freq(na)
              nlines=nlines+1 
            enddo
          endif
 1002   format('      ',i3,'     ',F10.2)      
        endif


        if(calcfreq) then
          write(ou,*)" "
          write(ou,*)"      Fraunhofer in the frequency domain ENABLED"
          nlines=nlines+2
        endif

        if(calctime) then
          write(ou,*)" "
          write(ou,*)"      Fraunhofer in the time domain ENABLED"
          nlines=nlines+2
        endif

        if(calcfreq.or.calctime) then !print fraunhofer angle info
          if(jmax<1) then
            write(ou,*)"WARNING: You specified to do calculations in"
            write(ou,*)"the fraunhofer domain but no angles were"
            write(ou,*)"defined. Use the AddFraunhofrAngle command"
            nlines=nlines+3                     
          endif
          if(kmax<1) then
            write(ou,*)"WARNING: You specified to do calculations in"
            write(ou,*)"the fraunhofer domain but no azimuth angles"
            write(ou,*)"were defined. Use the AddFraunhofrPhi command"
            nlines=nlines+3                     
          endif
          if(jmax>0.and.kmax>0) then      
            write(ou,*)" "
            write(ou,*)"      Angles for Fraunhofer formalism"
            write(ou,*)"      Number of Zentih Angles [Deg]:",jmax
            nlines=nlines+3
            do na=1,jmax
              write(ou,1003) na,angles(na)
              nlines=nlines+1 
            enddo
            write(ou,*)"      Number of Azimuth Angles [Deg]:",kmax
            nlines=nlines+1
            do na=1,kmax
              write(ou,1003) na,phi(na)*180.0/3.1415926
              nlines=nlines+1 
            enddo
          endif
 1003   format('   ',i3,'     ',F6.2)      
        endif
 
        if(calctime.or.calctimefres) then !print time bining info
          write(ou,*) " "
          write(ou,1004)"      Time bin size:",dt_bin_ns,"ns"
          nlines=nlines+2
        endif
 1004   format(a,F6.2,a)      
       return
      else
c      
c      write(ou,*) "fieldtool.inp was not found ()"
      nlines = 0 !radio computation is not enabled
c
      endif
      return
      end
c     --- End of routine zhairestatus.                                       
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
      subroutine zhaireslogo(ou, nlines)
c
c     Printing a character ZHAireS logo.
c
c     Written by: M. J. Tueros, Santiago de Compostela 2011.
c
c
c     Arguments:
c     =========
c
c     ou.............. (input, integer) I/O unit where to print the
c                      logo.
c     nlines.......... (output, integer) Number of written lines.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ou, nlines
c
c     FIRST EXECUTABLE STATEMENT
c
c
cwith
c
c ZZZZZ  H  H    AA   I RRR   EEEE  SSSS     \ \ \
c    Z   H  H   A  A  I R  R  E     S         \ \ \
c   Z    HHHH   AAAA  I RRR   EE    SSSS     ------- 
c  Z     H  H   A  A  I R R   E        S beta / / /  T 
c ZZZZZ  H  H   A  A  I R  R  EEEE  SSSS v28 / / /  T T
c 
c                                                 inside!
c 
 2010 format('>>>>', 7x, a, a1)
c
      write(ou, 2010)
c
      write(ou, 2010)
     +'with'
      write(ou, 2010)
     +''
      write(ou, 2010)
     +'ZZZZZ  H  H    AA   I  RRR   EEEE  SSSS     \ \ \'
      write(ou, 2010)
     +'   Z   H  H   A  A  I  R  R  E     S         \ \ \'
      write(ou, 2010)
     +'  Z    HHHH   AAAA  I  RRR   EE    SSSS     -------'
      write(ou, 2010)
     +' Z     H  H   A  A  I  R R   E        S beta / / /  T'
      write(ou, 2010)
     +'ZZZZZ  H  H   A  A  I  R  R  EEEE  SSSS v28 / / /  T T'
      write(ou, 2010)
     +''
      write(ou, 2010)
     +'                                                 inside!'
c
      write(ou, 2010)
      write(ou, 2010)
     +'Departamento de Fisica de Partículas, USC, ESPAÑA'
      write(ou, 2010)
c
      nlines = 13
c
      return
      end
c     --- End of routine zhaireslogo.                                       
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c     End of file 'fieldcomm.f'
c     This source file is part of ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
