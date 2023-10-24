c
c     FILE: atmosphere2.f                   Creation date: 05/AUG/1996.
c                                       LAST MODIFICATION: 13/MAY/2005.
c
c     Description of the atmosphere. II. Atomic composition.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function airtarsample()
c
c     Random sampling of the mass number of an atmospheric nucleus.
c     Element fractions are calculated on the basis of nuclear
c     cross section factors (proportional to A^0.91).
c
c     Written by: S. J. Sciutto, La Plata 2005.
c
c
c     Return value: (integer) A sample of the nucleus mass number.
c     ============
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           airtarsample
c
c     Declaration of parameters and shared data.
c
      include 'atmosdata.f'
c
c     Declaration of internal variables and arrays.
c
      double precision  rn
      double precision  urandom
c
c     FIRST EXECUTABLE STATEMENT
c
      rn = urandom()
      if (rn .lt. cfrnuc1) then
        airtarsample = anuca1
      else if (rn .lt. cfrnuc2) then
        airtarsample = anuca2
      else
        airtarsample = anuca3
      endif
c
      return
      end
c     --- End of routine airtarsample
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'atmosphere2.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
