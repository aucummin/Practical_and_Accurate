c
c     FILE: spipar.f                        Creation date: 15/OCT/1999.
c                                       LAST MODIFICATION: 20/OCT/1999.
c
c     Parameters for the "Special" primary particle external interface.
c
c     Written by: S. J. Sciutto, La Plata 1999.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     spiifile (spiofile) is the name of the main-to-slave
c     (slave-to-main) communication file.
c
      character*(*)     spiifile, spiofile
c
      parameter         (spiifile = '_AIRESSpecialParticle_.M2SFile')
      parameter         (spiofile = '_AIRESSpecialParticle_.S2MFile')
c
c     File header.
c
      character*24      spiifileheader0, spiofileheader0
      integer           spiintheader0
c
      parameter         (spiifileheader0 = '--- AIRES spii --- ')
      parameter         (spiofileheader0 = '--- AIRES spio --- ')
      parameter         (spiintheader0 = 0)
c
c     Particle codes with special meanings ("escape" codes).
c
      integer           injpointcode
c
      parameter         (injpointcode = -900000)
c
c     Miscellaneous.
c
      integer           speidonenumber
c
      parameter         (speidonenumber = 7438715)
c
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'spipar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
