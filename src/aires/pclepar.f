c
c     FILE: pclepar.f                       Creation date: 26/JUN/1996.
c                                       LAST MODIFICATION: 05/JUL/2002.
c
c     This file contains the particle codification related
c     parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2002.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     maxpcle is the largest elementary particle code.
c
      integer           maxpcle
      parameter         (maxpcle = 31)
c
c     maxpgroup is the number of particle groups that are defined
c               (applies both to elementary particles and nuclei).
c     maxgengroup is the number of "generic name groups" (E.g. "muons").
c     mxgengrpsize is the maximum size of generic groups
c     maxpsyn is the number of synonyms for particle/nucleus names.
c
      integer           maxpgroup, maxgengroup, mxgengrpsize, maxpsyn
      parameter         (maxpgroup    =  6)
      parameter         (maxgengroup  = 12)
      parameter         (mxgengrpsize =  4)
      parameter         (maxpsyn      = 44)
c
c     pescode1 and pescode2 define the "particle" codes reserved for
c     escape codes (used for multiple primaries).
c     nescodes is the number of escape codes available.
c     espms is the number of characters available for storing macro
c     and parameter data.
c
      integer           pescode1, nescodes, pescode2, espms
      parameter         (pescode1 = 80, nescodes = 10)
      parameter         (espms = 2048)
      parameter         (pescode2 = pescode1 + nescodes - 1)
c
c     nuccod0, nuccod1 and nuccod2 are the parameters to be used in the
c     nucleus coding formula: (A is the nucleus mass number and Z its
c     charge):
c          nucleus code = (nuccod0 + nuccod2) + (nuccod1 - 2) * Z + A
c
c     maxnucz is the maximum value of z allowed.
c     nuccod0 must be larger than pescode2.
c
      integer           nuccod0, nuccod1, nuccod2
      parameter         (nuccod0 = 100, nuccod1 = 32, nuccod2 = 8)
      integer           maxnucz
      parameter         (maxnucz = 36)
c
c     adelim is the delimiter used to separate chemical name from
c            mass number.
c
      character*1       adelim
      parameter         (adelim = '^')
c
c     Some derived parameters related to particle/nucleus encoding.
c     NOTICE: minncode MUST be greater than maxpcle
c
      integer           mxgengrpsize1
      parameter         (mxgengrpsize1 = mxgengrpsize - 1)
      integer           minncode, maxncode
      parameter         (minncode = nuccod0 + nuccod2 + nuccod1 - 1)
      parameter         (maxncode = nuccod0 +
     +                              nuccod1 * maxnucz + nuccod1 - 1)
c
      integer           syngroupcode, syngrouplim
      parameter         (syngroupcode = maxncode + 2)
      parameter         (syngrouplim  = syngroupcode + 10)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'pclepar.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
