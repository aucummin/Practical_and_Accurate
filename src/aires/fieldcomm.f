c
c     FILE: fieldcomm.f                                                                        Creation date: 18/6/2009
c                                                                                         LAST MODIFICATION:  28/5/2012
c
c     This file contains the ZHAireS common variables and compilation parameters.
c
c======================================================================================================================
c     material variables
c======================================================================================================================
      double precision  rho,ref_n,factor_t,ns,kr
c     Set variable n model parameter kr: n(h)=ns*exp(-kr*h)
      parameter(kr=-0.1218)
c     angle conversion
      double precision radegr
c     Fraunhoffer inside track variables
      double precision fraulambda
      parameter (fraulambda=0.3) !300MHz->1m  1GHz->0.3m  3GHz->0.1m
c     pi+-, mu+- and p/p-bar cuts for field calculation
      double precision fpicut,fmucut,fpcut
c
      common           /material/ rho, ref_n, factor_t,ns
      common           /conversion/ radegr
      common           /fieldcuts/  fpicut,fmucut,fpcut
c======================================================================================================================
c Default Time Window (this is what will be used by Fraunhofer & Fresnel)
c======================================================================================================================
c The t0 for each antenna will be calculated automatically assuming a plane shower front traveling at the speed of 
c light. "starttime" and "endtime" set the time window arround t0 for each antenna
c
c If you find that your pulses are out of the window, enlarge it and recompile. Note that the bigger the window, 
c the bigger the memmory requirements and the output file size will be.
c If you find yourself with long, unusefull tails, reduce the window and recompile. Smaler time windows also run faster.
c
c If you change this numbers, keep in mind that you should not exceed the maximum dimension that an array can have in your system.
c The biggest arrays in the code can have size maxj*maxk*maxt, maxna*maxt, maxj*maxk*maxnu and maxna*maxnu
c Depending on the architecture, this maximum is arround 2-3E9 (the 2Gb limit)
c
c The output is still produced in "absolute time", where 0 is the time at wich the shower core hits the ground 
c======================================================================================================================
c  
      double precision starttime,endtime   
      parameter        (starttime=-200.d0)
      parameter        (endtime=5800.d0)
c
c     time_min, time_max for time domain plot
      double precision time_min,time_max
      integer maxthalf
      common  /timewindow/ ntbins,maxthalf,time_min,time_max
c======================================================================================================================
c     Output format and control
c     These format fields control the output. You can change it to get more/less significant figures.
c     This will affect the output file size 
c======================================================================================================================
      logical empoutput
c     this variable enables the old, big .EMP output, that is more gnuplot friendly
      parameter (empoutput=.false.)
c
c     freq fresnel (original 2I5, 3F14.6, I4, F17.3, 1P, 4E18.10):(145 caracteres)
c     showern,na,xant(na),yant(na),zant(na),nu,freq(nu),aradius_fr(na,nu),aradius_x_fr(na,nu), aradius_y_fr(na,nu),
c     aradius_Z_fr(na,nu)
      character freqfresfmt*50
      parameter (freqfresfmt = "(2I4, 3F10.2, I4, es11.3, 4es11.3)") ! passed to (97 caracteres)
c
c     time fresnel  (original 2I5, 3F14.6, F15.5, 1P, 8E18.10):(211 caracteres) 
c     showern,na,xant(na),yant(na),zant(na),time,asum_fr(t,na),asum_x_fr(t,na),asum_y_fr(t,na),asum_z_fr(t,na),ef_fr,
c     ef_x_fr,ef_y_fr,ef_z_fr
      CHARACTER timefresfmt*50
      parameter (timefresfmt = "(2I4, 3F11.2, F10.2, 8es11.3)") !passed to (129 caracteres)
c
c     freq fraunhoffer (original 3I4, 2F14.6, I4, F11.3, 1P, 8E18.10):(199 caracteres)
c     showern,j,k,theta(j)*radegr,phi(k)*radegr,nu,freq(nu),aradius(j,k,nu),aphase(j,k,nu),aradius_x(j,k,nu),
c     aphase_x(j,k,nu),aradius_y(j,k,nu),aphase_y(j,k,nu), aradius_z(j,k,nu),aphase_z(j,k,nu)
      character freqfraufmt*50
      parameter (freqfraufmt = "(3I4, 2F7.2, I4, es11.3, 8es11.3)") ! passed to (129 caracteres)
c
c     time fraunhoffer  (original 3I4, 2F14.6, F15.5, 1P, 8E18.10):(199 caracteres) 
c     showern,j,k,theta(j)*radegr,phi(k)*radegr,time,asum(t,j,k),asum_x(t,j,k),asum_y(t,j,k),asum_z(t,j,k),ef(k),
c     ef_x(k),ef_y(k),ef_z(k)
      CHARACTER timefraufmt*50
      parameter (timefraufmt = "(3I4, 2F7.2, F11.2, 8es11.3)") !passed to (127 caracteres)
c
c======================================================================================================================
c program logic controls
c======================================================================================================================
c     Flag for calculating tracklenght: true-On false-Off
      logical           trackflag
c     Flag for actually calculating the fields (empsum calls)
      logical           calcfield
c     Flag for freq domain fraunhoffer calculation
      logical           calcfreq
c     Flag for time domain fraunhoffer calculation
      logical           calctime
c     Flag for freq domain fresnel calculation
      logical           calcfreqfres
c     Flag fot time domain fresnel calculation
      logical           calctimefres
c     Flag for using ZHS energy loss: true-ZHS false-TIERRAS
      logical           usezhseloss
c     Flag for counting tracks of mu+- and pi+-: true-ZHS false-TIERRAS
      logical           countpimu
c     Flag for using variable n
      logical           usevarn
c     common definition
      common           /field_f/ trackflag,usezhseloss,countpimu,
     + calcfield,calcfreq,calctime,calcfreqfres,calctimefres,usevarn
c     Matias:added for IDL compatibility
      logical fieldtool
      common /fieldtoolc/ fieldtool
c     end addition
c======================================================================================================================
c     maximum matrix size parameters
c======================================================================================================================
      integer           maxj,maxnu,maxk,maxt,maxna
      parameter        (maxj=1)     !was 100, but fraunhoffer is discouraged in this version
      parameter        (maxnu=100)  !was 200
      parameter        (maxk=1)     !was 5, but fraunhoffer is discouraged in this version
      parameter        (maxna=200)  !was ~240
      parameter        (maxt=60000) !was 120k (to be able to sample the default 6us time window in up to 0.02 ns!)
c
      integer          numax !actual maximum number of freq
      integer          kmax  !actual maximum number of phi angles for fraunhf
      integer          jend(maxnu) 
      integer          jmax  !actual maximum for theta angles for fraunhf
      integer          namax !actual maxumum for number of antennas
      integer          ntbins!actual maximum number of time bins   
c
c======================================================================================================================
c     antenna, angles and frequency grid variables
c======================================================================================================================
      double precision  freq(maxnu)   !this cannot be allocated dynamically, as it is needed during directives parsing
      double precision  frqcom(maxnu) !?this variable is never used
      double precision  theta(maxj)   !this cannot be allocated dynamically, as it is needed during directives parsing
      double precision  phi(maxk)     !this cannot be allocated dynamically, as it is needed during directives parsing 
      double precision  angles(maxj)  !this cannot be allocated dynamically, as it is needed during directives parsing
      double precision  dt_bin_s, dt_bin_ns
c     antenna matrix
      double precision xant(maxna)    !this cannot be allocated dynamically, as it is needed during directives parsing
      double precision yant(maxna)    !this cannot be allocated dynamically, as it is needed during directives parsing
      double precision zant(maxna)    !this cannot be allocated dynamically, as it is needed during directives parsing
c     antenna critical visibility parameters
      double precision parcrit(maxna)
      double precision Rtg !Rt+groundalt
      double precision h0ant(maxna) !antenna height above ground
      double precision dcrit(maxna) !dist from axis to antenna horizon (X)
      double precision minparcrit
c     array for time window shift
      double precision dt(maxna)      
c     commons
      common           /grid/ freq,frqcom,theta,phi,jend,numax,kmax,
     + jmax,namax,xant,yant,zant,dt,parcrit,minparcrit,Rtg,h0ant,dcrit,
     + angles
      common           /timegrid/ dt_bin_s, dt_bin_ns
c======================================================================================================================
c     track variables
c======================================================================================================================
      double precision tracksum,sumdeltaz,sumabdeltaz
      double precision excesssum,excesssumz,excessabsumz
c     for pions and muons alone
      double precision pimutracksum,pimusumdeltaz,pimusumabdeltaz
      double precision pimuexcesssum,pimuexcesssumz,pimuexcessabsumz
c     for separate particle types
c     protons
      double precision ptracksum,psumdeltaz,psumabdeltaz
c     anti-protons
      double precision pbartracksum,pbarsumdeltaz,pbarsumabdeltaz
c     electrons
      double precision emtracksum,emsumdeltaz,emsumabdeltaz
c     positrons
      double precision eptracksum,epsumdeltaz,epsumabdeltaz
c     mu plus
      double precision muptracksum,mupsumdeltaz,mupsumabdeltaz
c     mu minus
      double precision mumtracksum,mumsumdeltaz,mumsumabdeltaz
c     pi plus
      double precision piptracksum,pipsumdeltaz,pipsumabdeltaz
c     pi minus
      double precision pimtracksum,pimsumdeltaz,pimsumabdeltaz
c     common definitions
      common           /tracksums/ tracksum,sumdeltaz,sumabdeltaz,
     + excesssum,excesssumz,excessabsumz,pimutracksum,pimusumdeltaz,
     + pimusumabdeltaz,pimuexcesssum,pimuexcesssumz,pimuexcessabsumz,
     + ptracksum,psumdeltaz,psumabdeltaz,pbartracksum,pbarsumdeltaz,
     + pbarsumabdeltaz,emtracksum,emsumdeltaz,emsumabdeltaz,eptracksum,
     + epsumdeltaz,epsumabdeltaz,muptracksum,mupsumdeltaz,
     + mupsumabdeltaz,mumtracksum,mumsumdeltaz,mumsumabdeltaz,
     + piptracksum,pipsumdeltaz,pipsumabdeltaz,pimtracksum,
     + pimsumdeltaz,pimsumabdeltaz

c======================================================================================================================
c     speedup definitions (check if this could go to the dynamic array module)
c======================================================================================================================
      double precision  sintet(maxj)
      double precision  sintet2(maxj)
      double precision  costet(maxj)
      double precision  costet2(maxj)
      double precision  sinmu(maxj)
      double precision  cosmu(maxj)
      double precision  csmu_1(maxj)
      double precision  facfrq(maxnu)
      double precision  sinphi(maxk)
      double precision  cosphi(maxk)
c
      common           /speedup/ sintet,sintet2,costet,costet2,sinmu,
     +     csmu_1,cosmu,facfrq,sinphi,cosphi
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c     End of file 'fieldcomm.f'
c     This source file is part of ZHAireS betav0.28r16 distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
