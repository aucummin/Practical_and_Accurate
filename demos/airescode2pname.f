c
c     File airescode2pname.f: Converting AIRES particle codes to
c                             particle names.
c
c     Last modified: 18/Jun/2003.
c
c     ------------------------------------------------------------------
c     TO COMPILE THIS FORTRAN PROGRAM:
c
c     This program contains calls to some of the routines included in
c     the AIRES object library (generally named libAires.a). Therefore,
c     the library must be specified as an object library in the link
c     step of the compilation process. In the file "config" you can set
c     then name and placement of this library. If your home directory
c     is, say, "/myhome", the default full path for this library is
c     "/myhome/aires/lib/libAires.a". If "f77" is the command you use
c     to compile FORTRAN programs, then the command:
c
c       f77 airescode2pname.f -L/myhome/aires/lib -lAires
c
c     will do the work in the default case. In some systems it may be
c     necessary to put some qualifiers to the original "f77" commands.
c     If plain "f77" failed, try one of the following commands using
c     one or more qualifiers:
c
c       f77 -O5  ....                             for DEC Alpha FORTRAN.
c       f77 +U77 +O3  ....                        for HP FORTRAN.
c       f77 -qstrict -qcharlen=10000 -O3  ....    for IBM FORTRAN.
c       f77 -O3  ....                             for SGI FORTRAN.
c       f77 -native -O4  ....                     for SUN FORTRAN.
c     ------------------------------------------------------------------
c
      program airescode2pname
c
c     Converting particle names (read from stdin) to particle codes
c     (written to stdout).
c
      implicit none
c
      character*512     iline, oline
      integer           ilen, olen, polen, axl
      character*80      auxline
      integer           i1, i2, pcode, irc
c
c
c     Initializing the AIRES system.
c
      call ciorinit(0, 0, 0, irc)
c
c     Getting input line
c
      read(5, 2010, end = 3010, err = 3010) iline
 2010 format(a)
c
      call strim(-1, iline, ilen)
      if (ilen .le. 0) goto 3010
c
      i2 = 0
      olen = 0
c
 1010 continue
      call nextword(iline, ilen, i1, i2)
      if (i1 .gt. i2) goto 1100
      read(iline(i1:i2), *, err = 1100) pcode
      call pnice(0, pcode, auxline, axl)
      if (axl .gt. 0) then
        polen = olen + 1
        olen  = polen + axl
        oline(polen:olen) = ' ' // auxline(1:axl)
      endif
      goto 1010
 1100 continue
c
      if (olen .gt. 1) then
        print 2010, oline(2:olen)
      else
        print 2010, 'ERROR'
      endif
c
 3010 continue
      end
c
c     End of file airescode2pname.f
