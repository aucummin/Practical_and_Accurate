c
c     FILE: nucfragmodel.f                  Creation date: 15/FEB/2002.
c                                       LAST MODIFICATION: 13/MAY/2005.
c
c     AIRES built-in algorithm for nuclear fragmentation.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine nucfragment(massa, nucz, pwt, kenucleon, up, atarget,
     +                       plol, maxsec, ldf, nsec, nfrag, efrag,
     +                       seccode, esec, upsec, iws, fws)
c
c
c     A simple model for nuclear fragmentation, based in the algorithms
c     implemented in Hillas's MOCCA program.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2002, 2005.
c
c
c     Arguments:
c     =========
c
c     massa.......... (input, integer) Projectile mass number.
c     nucz........... (input, integer) Projectile charge number.
c     pwt............ (input, double precision) Particle weight.
c     kenucleon...... (input, double precision) The average free energy
c                     of the projectile's nucleons.
c     up............. (input, double precision, array(3)) Unitary
c                     vector marking the direction of the initial
c                     impulse.
c     atarget........ (input, integer) Target nucleus mass number.
c     plol........... (input, integer) Last observing level crossed
c                     by the primary particle.
c     maxsec......... (input, integer) Size of secondary arrays.
c     ldf............ (input, integer) Leading dimension of array
c                     upsec.
c     nsec........... (output, integer) Total number of generated
c                     secondaries.
c     nfrag.......... (output, integer) Total number of secondary
c                     nuclear fragments and elastically scattered
c                     nucleons.
c     efrag.......... (output, double precision) Total energy of the
c                     emerging fragments.
c     seccode........ (output, integer, array(maxsec)) Particle codes
c                     of the generated secondaries. seccode(i),
c                     i = 1,...,nfrag, contains the codes of the
c                     secondary nuclear fragments and elastically
c                     scattered nucleons. For i = nfrag+1,...,nsec,
c                     the array contains the codes of the secondaries
c                     generated inelastically.
c     esec........... (output, double precision, array(maxsec))
c                     Energies of the generated secondaries,
c                     ordered similarly as "seccode".
c     upsec.......... (output, double precision, array(ldf, maxsec))
c                     Unitary vectors marking the directions of the
c                     emerging secondaries, ordered similarly as
c                     "seccode".
c     iws............ (scratch, integer, array(maxsec)) Integer working
c                     space.
c     fws............ (scratch, double precision, array(maxsec)) Real
c                     working space.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclecodes.f'
c
c     Declaration of arguments.
c
      integer           massa, nucz
      double precision  pwt, kenucleon
      double precision  up(3)
      integer           atarget, plol, maxsec, ldf, nsec, nfrag
      double precision  efrag
      integer           seccode(maxsec)
      double precision  esec(maxsec), upsec(ldf, maxsec)
      integer           iws(maxsec)
      double precision  fws(maxsec)
c
c     Declaration of internal variables and arrays.
c
      integer           i, n, z, nsec1, nseci
      integer           ndetached, nbound, nelastic, ninelastic
      integer           nalpha, lost, n1, n2, n3
      double precision  ke
      double precision  urandom
      integer           nuclcode
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating the number of emerging fragments and nucleons.
c 
      ndetached = 1.5d0 * (massa ** (2 / 3.d0))
      if (massa .gt. 15) ndetached = ndetached * (0.5d0 + urandom())
      lost = (1 + ndetached) / 2
      lost = 2 * lost
      ninelastic = 0.5d0 + 0.77d0 * lost
      nelastic   = lost - ninelastic
      nbound     = massa - lost
c
      if ((nbound .gt. 23) .or. (nbound .eq. 8)) then
        nalpha = 2
      else if (nbound .gt. 15) then
        nalpha = 1
      else
        nalpha = 0
      endif
c
      nbound = nbound - 4 * nalpha
c
c     Generating the new secondaries.
c
      nsec  = 0
      efrag = 0
c
      n1 = protoncode
      n2 = neutroncode
c
c     New nucleons generated.
c
      do n = 1, nelastic
        nseci          = nsec + n
        seccode(nseci) = n1
        esec(nseci)    = kenucleon
        n3             = n1
        n1             = n2
        n2             = n3
      enddo
      nsec  = nsec + nelastic
      efrag = efrag + kenucleon * nelastic
c
c     Alpha particles.
c
      ke = 4 * kenucleon
      do n = 1, nalpha
        nseci          = nsec + n
        seccode(nseci) = alphapcode
        esec(nseci)    = ke
      enddo
      nsec  = nsec + nalpha
      efrag = efrag + nalpha * ke
c
      if (nbound .gt. 0) then
c
c       There is also an emerging nucleus, with
c       z + n = nbound.
c
        z     = nbound / 2.153d0
        if (z .lt. 1) z = 1
        n     = nbound - z
        ke    = nbound * kenucleon
        nsec  = nsec + 1
        efrag = efrag + ke
c
        seccode(nsec) = nuclcode(z, n, i)
        esec(nsec)    = ke
c
      endif
c
      nfrag = nsec
c
c     All the emerging nucleons and fragments move in the direction
c     of the primary
c
      do n = 1, nfrag
        do i = 1, 3
          upsec(i, n) = up(i)
        enddo
      enddo
c
c     Secondaries from inelastic scattering.
c
      do n = 1, ninelastic
        nsec1 = nsec + 1
c
        call hnucoll(.true., n1, pwt, kenucleon,
     +               up, plol, maxsec - nsec, ldf,
     +               nseci, seccode(nsec1), esec(nsec1),
     +               upsec(1, nsec1), iws, fws)
c
        nsec = nsec + nseci
c
        if (nsec .gt. (maxsec - 10))
     +    call errprint(2, 'NSEC', 4, 'nucfragment', ' ',
     +                  1, nsec, 0, 0.d0, ' ')
c
        n3 = n1
        n1 = n2
        n2 = n3
      enddo
c
      return
      end
c     --- End of routine nucfragment
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'nucfragmodel.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
