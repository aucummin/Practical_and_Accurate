c
c     FILE: pbuffpar.f                      Creation date: 03/DEC/1996.
c                                       LAST MODIFICATION: 06/MAR/2002.
c
c     This file contains the ground particle buffers related
c     parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2002.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     The parameters defined for the particle stacks are needed here:
c
      include 'pstackpar.f'
c
c     (This also implies inclusion of parameter file 'kernelpar.f').
c
c     Cio related parameters are also needed:
c
      include 'ciopar.f'
c
c     gndbuffsize is the ground particle buffer size.
c     opbuffsize is the size of other buffers related with cio files.
c     nopauxi (nopauxf) is the number of auxiliary integer (real)
c     fields for these other buffers.
c
      integer           gndbuffsize, opbuffsize, nopauxi, nopauxf
c
      parameter         (gndbuffsize = 300)
      parameter         (opbuffsize  = 600)
      parameter         (nopauxi     = 2)
      parameter         (nopauxf     = 1)
c
c     mxobuffers is the maximum number of particle buffers, excluding
c     the ground particle buffer.
c
      integer           mxobuffers
c
      parameter         (mxobuffers = mxciofiles - 1)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'pbuffpar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
