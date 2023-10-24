c
c     FILE: omsgpar.f                       Creation date: 22/JUN/1996.
c                                       LAST MODIFICATION: 02/AUG/1996.
c
c     This file contains the output message routines related
c     parameters.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     maxerrcodes is the maximum number of error messages.
c     erravlen is the error message text average length.
c     mtxtbdy is the character used to mark line boundaries;
c             it should not be used within text of messages.
c
      integer           maxerrcodes, erravlen
      parameter         (maxerrcodes = 95)
      parameter         (erravlen    = 80)
      character*1       mtxtbdy
      parameter         (mtxtbdy = '$')
c
c     Other derived parameters:
c
      integer           errcl
      parameter         (errcl = erravlen * maxerrcodes)
c
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c[---][---][---][---][---][---][---]*[---][---][---][---][---][---][---]
c
c     End of file 'omsgpar.f'

c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
