c
c     FILE: pclecomm.f                      Creation date: 26/JUN/1996.
c                                       LAST MODIFICATION: 14/FEB/2002.
c
c     This file contains the particle codification related
c     definitions.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2000, 2001,
c                                         2002.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Properties of elementary particles.
c
      integer           pcleq(-maxpcle:maxpcle)
      double precision  pclemass(-maxpcle:maxpcle)
      double precision  pclelife(-maxpcle:maxpcle)
      double precision  cpclelife(-maxpcle:maxpcle)
      double precision  cutenergy(-maxpcle:maxpcle, 2)
      double precision  pfreeegy(-maxpcle:maxpcle)
      character*8       pclename(-maxpcle:maxpcle)
c
      common            /pcledata_c/ pclemass,
     +                               pclelife, cpclelife,
     +                               cutenergy, pfreeegy,
     +                               pclename, pcleq
c
c     Names of chemical elements.
c
      character*2       chemname(maxnucz)
      integer           chemdefn(maxnucz)
c
      common            /chemname_c/ chemdefn, chemname
c
c     Nuclear properties.
c
      integer           nucz(minncode:maxncode)
      integer           nucn(minncode:maxncode)
      integer           nuca(minncode:maxncode)
      double precision  nucmass(minncode:maxncode)
      double precision  nuclife(minncode:maxncode)
      double precision  cnuclife(minncode:maxncode)
      double precision  nucutenergy
c
      common            /nucdata0/ nucmass,
     +                             nuclife, cnuclife,
     +                             nucutenergy, nucz, nucn, nuca
c
c     Groups of particles.
c
      character*8       gengrpname(maxgengroup)
      integer           gengrppcle(-1:mxgengrpsize1, maxgengroup)
      integer           gengrpsize(maxgengroup)
      logical*1         allpgroup(maxpgroup, -maxpcle:maxncode)
c
      common            /pclesh_c1/ gengrpname, gengrppcle,
     +                              gengrpsize, allpgroup
c
c     Additional names for particles or nuclei.
c
      integer           psyncode(maxpsyn), pgsize(-1:maxpgroup)
      character*16      psynonym(maxpsyn), pgname(-1:maxpgroup)
c
      common            /psyn_c/ psyncode, pgsize, psynonym, pgname
c
c     Data related with "Special" particles (related with multiple
c     primaries).
c
      integer           nescpcles, lastescpcle
      character*16      escpclename(nescodes)
      character*(espms) escpclemacro
      integer           escmacropos(4, 0:nescodes)
c
      common            /escpcles_c0/ nescpcles, lastescpcle,
     +                                escpclename,
     +                                escmacropos, escpclemacro
c
      integer           escmacrover(nescodes)
      integer           escmacrouse(nescodes)
      integer           nsprimpart(3, nescodes)
c
      common            /escpcles_c0/ escmacrover, escmacrouse,
     +                                nsprimpart
c
c     Arrays for particle stack determination. Notice that no
c     distinction is made from elementary particles and nuclei.
c
      integer           allpclesta0(-maxpcle:maxncode)
      integer           allpclesta1(-maxpcle:maxncode)
c
      common            /pclesta01_c/ allpclesta0, allpclesta1
c
c     Also single arrays are used for the compressed file saving
c     data.
c
      logical*1         pcleresample(-maxpcle:maxncode)
      logical*1         allpclesave(-maxpcle:maxncode, 3)
      logical*1         anypinfile(3)
c
      common            /pclesave_sw/ pcleresample,
     +                                allpclesave, anypinfile
c
c     And for longitudinal, transversal, deposited energy, and time
c     distribution histogram determination.
c
      integer           allpclelh(-maxpcle:maxncode)
      integer           allpcleli(-maxpcle:maxncode)
      integer           allpcleth(-maxpcle:maxncode)
      integer           allpcleuh(-maxpcle:maxncode)
c
      common            /pcleh01_c/ allpclelh, allpcleli,
     +                              allpcleth, allpcleuh
c
c     Low energy decays related data.
c
      logical           forcelowedecay, forceloweannih
c
      common            /pclelowe_c/ forcelowedecay, forceloweannih
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file 'pclecomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
