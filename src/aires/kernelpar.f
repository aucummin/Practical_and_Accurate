c
c     FILE: kernelpar.f                     Creation date: 14/JUL/1996.
c                                       LAST MODIFICATION: 17/JUL/2003.
c
c     Kernel compile parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2003.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     npstacks is the number of particle stacks.
c
      integer           npstacks, npstacksp1
      parameter         (npstacks = 4)
      parameter         (npstacksp1 = npstacks + 1)
c
c     stopchsecs is the number of seconds to wait between subsequent
c     calls to routine stopcheck (actually only a rough estimation).
c     The current setting of 100 seconds will normally be adequate.
c
      double precision  stopchsecs
      parameter         (stopchsecs = 100.d0)
c
c     Parameters for shower bounding box evaluation.
c
c     dmbox (meters) is the indicative size for the shower box (half).
c     It defines the scale for bounding box calculations, and is equal
c     to the actual bound used for vertical showers.
c     dmboxsf is the inverse of the maximum scaling factor for dmbox
c     due to obliquity.
c     dmboxdepth0 (g/cm2) is an atmospheric depth setting the depth
c     scale for enlargement of the size of the basic rectangle.
c     rxysh0 (meters) is the fixed increase to add to the injection
c     position to be used as extreme for the bounding rectangle.
c
      double precision  dmbox, dmboxsf, dmboxdepth0, rxysh0
c
      parameter         (dmbox       = 15000.d0)
      parameter         (dmboxsf     = 0.2500d0)
      parameter         (dmboxdepth0 = 9.5000d0)
      parameter         (rxysh0      = 5000.0d0)
c
c     maximum number of internal variables for special primaries.
c
      integer           mxintintvar, mxintfltvar
c
      parameter         (mxintintvar = 12)
      parameter         (mxintfltvar = 50)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'kernelpar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
