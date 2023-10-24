c
c     FILE: maincomm.f                      Creation date: 15/AUG/1996.
c                                       LAST MODIFICATION: 19/AUG/2003.
c
c     This file contains some shared variables for program organization.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997; Fermilab 2003.
c
co===oo===oo===oo===oo===oo===oo===o*o===oo===oo===oo===oo===oo===oo===o
co===oo===oo===oo===oo===oo===oo===o*o===oo===oo===oo===oo===oo===oo===o
c
c     Program code and related variables.
c
      integer           pgmcode, idfverlen
      character*24      idfversion
      logical           idfvnecurrent
      character*11      idfcdate
      character*48      idfcwho
      character*8       idfcstar
      character*24      idfcruser
      character*64      idfcrhost
      integer           idfcruserlen, idfcrhostlen
      integer           adfnmergedfiles
c
      common            /main_c1/ idfversion, idfverlen,
     +                            idfvnecurrent, pgmcode
      common            /main_c2/ idfcwho, idfcstar, idfcdate
      common            /main_c3/ idfcruserlen, idfcrhostlen,
     +                            idfcruser, idfcrhost
      common            /main_c4/ adfnmergedfiles
c
co===oo===oo===oo===oo===oo===oo===o*o===oo===oo===oo===oo===oo===oo===o
co===oo===oo===oo===oo===oo===oo===o*o===oo===oo===oo===oo===oo===oo===o
c
c     End of file 'maincomm.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
