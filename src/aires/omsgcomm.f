c
c     FILE: omsgcomm.f                      Creation date: 22/JUN/1996.
c                                       LAST MODIFICATION: 15/NOV/1996.
c
c     This file contains the output message routines common
c     definitions.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     Shared variables and arrays for error message management.
c
      integer           nerrcodes, errlastpos
      character*(errcl) errormessages
      character*4       errname(0:maxerrcodes)
      integer           errbeg(maxerrcodes)
      integer           errend(maxerrcodes)
      integer           deferrsev(0:maxerrcodes)
c
      common            /error_c/nerrcodes, errlastpos, errname,
     +                           errbeg, errend, deferrsev,
     +                           errormessages
c
co---oo---oo---oo---oo---oo---oo---o*o---oo---oo---oo---oo---oo---oo---o
c
c     End of file omsgcomm.f
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
