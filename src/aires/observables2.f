c
c     FILE: observables2.f                  Creation date: 05/JUN/2003.
c                                       LAST MODIFICATION: 07/JUN/2003.
c
c     This file contains several routines to perform statistical
c     calculations.
c     Part II: Auxiliary routines for final statistical calculations,
c              and miscellaneous related procedures.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine iniprtexpopts
c
c     Setting the print/export options, their defaults and related
c     data.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
c     Print options:  n  No min/max plots.
c                     m  Min/Max plots.
c                     M  Min/Max tables.
c                     R  Use RMS errors.
c                     S  Use standard deviations.
c                     r  Normal lateral distribution.
c                     d  Density lateral distributions.
c                     l  ln normalized lateral/egy distributions.
c                     L  log10 normalized lateral/egy distributions.
c
c     option1 (nmM   <-> 012)  default 0
c     option2 (RS    <-> 23)   default 3
c     option3 (rdlL  <-> 0123) default 0
c
      pexpopstr(1)  = 'nmMRSrdlL'
      npexpoplen(1) = 9
c
c     Settings for 'nmM'
c
      do i = 1, 3
        pexpoptidx(i, 1)   = 1
        pexpopequiv(i, 1)  = i - 1
      enddo
c
      pexpdefopt(1, 1)  = 0
c
c     Settings for 'RS'
c
      do i = 4, 5
        pexpoptidx(i, 1)   = 2
        pexpopequiv(i, 1)  = i - 2
      enddo
c
      pexpdefopt(2, 1)  = 3
c
c     Settings for 'rdlL'
c
      do i = 6, 9
        pexpoptidx(i, 1)   = 3
        pexpopequiv(i, 1)  = i - 6
      enddo
c
      pexpdefopt(3, 1)  = 0
c
c     Export options:  s  Supress file header
c                      h  Include file header.
c                      U  Do not include "border" bins.
c                      x  Include "border" bins as comments.
c                      X  Include "border" bins within the data.
c                      r  Normal lateral distribution.
c                      d  Density lateral distributions.
c                      l  ln normalized lateral/egy distributions.
c                      L  log10 normalized lateral/egy distributions.
c                      a  Depths expressed as slant depths.
c                      p  Obs. lev. perpendicular to shower axis.
c                      o  Horizontal observing levels.
c                      K  Express energies in KeV
c                      M  Express energies in MeV
c                      G  Express energies in GeV
c                      T  Express energies in TeV
c                      P  Express energies in PeV
c                      E  Express energies in EeV
c                      o  Horizontal observing levels.
c                      p  Obs. lev. perpendicular to shower axis.
c
c     option1 (sh      <-> 01)     default 1
c     option2 (UxX     <-> 012)    default 1
c     option3 (rdlLa   <-> 01234)  default 0
c     option4 (KMGTPE  <-> 012345) default 2
c     option5 (op      <-> 01)     default 0
c
      pexpopstr(2)  = 'shUxXrdlLaKMGTPEop'
      npexpoplen(2) = 18
c
c     Settings for 'sh'
c
      do i = 1, 2
        pexpoptidx(i, 2)   = 1
        pexpopequiv(i, 2)  = i - 1
      enddo
c
      pexpdefopt(1, 2)   = 1
c
c     Settings for 'UxX'
c
      do i = 3, 5
        pexpoptidx(i, 2)  = 2
        pexpopequiv(i, 2) = i - 3
      enddo
c
      pexpdefopt(2, 2)   = 1
c
c     Settings for 'rdlLa'
c
      do i = 6, 10
        pexpoptidx(i, 2)  = 3
        pexpopequiv(i, 2) = i - 6
      enddo
c
      pexpdefopt(3, 2)   = 0
c
c     Settings for 'KMGTPE'
c
      do i = 11, 16
        pexpoptidx(i, 2)  = 4
        pexpopequiv(i, 2) = i - 11
      enddo
c
      pexpdefopt(4, 2)   = 2
c
c     Settings for 'op'
c
      do i = 17, 18
        pexpoptidx(i, 2)   = 5
        pexpopequiv(i, 2)  = i - 17
      enddo
c
      pexpdefopt(5, 2)   = 0
c
      return
      end
c     --- End of routine iniprtexpopts
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine petopts(iprtexp, opstr, oplen, codopt, irc)
c
c     Analysing s given options string for printing/exporting tables.
c
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     iprtexp......... (input, integer) 1 for Print 2 for Export.
c     opstr........... (input, character*(*)) Options string.
c     oplen........... (input, integer) Length of string "opstr".
c     codopt.......... (output, integer) Encoded options.
c     irc............. (output, integer) Return code. Zero means
c                      that the options were successfully encoded.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
c
c     Declaration of arguments.
c
      integer           iprtexp, oplen
      character*(*)     opstr
      integer           codopt, irc
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l
      integer           inopt(mxpexopt)
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, mxpexopt
        inopt(i) = pexpdefopt(i, iprtexp)
      enddo
c
      if ((oplen .gt. 0) .and. (opstr .ne. ' ')) then
c
        l = npexpoplen(iprtexp)
        do i = 1, oplen
          do j = 1, l
            k = j
            if (pexpopstr(iprtexp)(j:j) .eq. opstr(i:i)) goto 1010
          enddo
c
c         Unknown option.
c
          codopt = i
          irc    = 4
          return
c
 1010     continue
c
c         Valid option.
c
          inopt(pexpoptidx(k, iprtexp)) = pexpopequiv(k, iprtexp)
c
        enddo
c
      endif
c
c       Encoding the options.
c
      codopt = 0
      do i = mxpexopt, 1, -1
        codopt = inopt(i) + 11 * codopt
      enddo
c
      irc = 0
      return
      end
c     --- End of routine petopts
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine table2(nshowers)
c
c     Final statistical calculations for histogram data.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999; La Plata 2001.
c
c
c     Arguments:
c     =========
c
c     nshowers........ (input, integer) The number of showers currently
c                      completed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'hdatapar.f'
      include 'constants.f'
c
c     Declaration of arguments.
c
      integer           nshowers
c
c     Declaration of shared data.
c
      include 'hdatacomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, k0, l
c
c     FIRST EXECUTABLE STATEMENT
c
c     Longitudinal histograms.
c
      do l = 0, mxlhtable, mxlhtable 
        do k0 = 1, nlhtables
          k = k0 + l
          call stmomts(nshowers, nobslevelsp1, 5, lhistn(1, 1, k))
          call stmomts(nshowers, nobslevelsp1, 5, lhiste(1, 1, k))
          call stmomts(nshowers, nobslevelsp1, 5, wlhistn(1, 1, k))
        enddo
      enddo
c
c     Deposited energy and related histograms.
c
      do l = 0, mxlitable, mxlitable 
        do k0 = 1, nlitables
          k = k0 + l
          call stmomts(nshowers, nobslevelsp1, 5, llhistn(1, 1, k))
          call stmomts(nshowers, nobslevelsp1, 5, llhiste(1, 1, k))
          call stmomts(nshowers, nobslevelsp1, 5, wllhistn(1, 1, k))
          call stmomts(nshowers, nobslevelsp1, 5, lihiste(1, 1, k))
        enddo
      enddo
c
c     Shower maximum.
c     Here the number of samples is taken from variable "shxsamples"
c
      call stmomts(shxsamples, 2, 5, shxmax)
      call stmomts(shxsamples, 1, 5, shnmax)
c
c     Lateral distribution histograms.
c
      do k = 1, nldtables
        call stmomts(nshowers, nttabinsp2, 5, rthistn(1, 0, k))
        call stmomts(nshowers, nttabinsp2, 5, rthiste(1, 0, k))
        call stmomts(nshowers, nttabinsp2, 5, wrthistn(1, 0, k))
        call stmomts(nshowers, nttabinsp2, 5, wrthiste(1, 0, k))
      enddo
c
c     Time distribution histograms.
c     Here the number of data samples is not "nshowers". Instead
c     is given by the array rtsampl and varies from bin to bin.
c     Also, times are converted to nanoseconds.
c
      do k = 1, ntdtables
        do j = 0, nttabinsp1
          do i = 1, 5
            rthistt(i, j, k) = ucspeedns * rthistt(i, j, k)
          enddo
          call stmomts(rtsampl(j, k), 1, 5, rthistt(1, j, k))
        enddo
      enddo
c
      return
      end
c     --- End of routine table2
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine momstxobs(nshowers)
c
c     Final statistical calculations for stack observables,
c     longitudinal development observables, and related quantities.
c
c     Written by: S. J. Sciutto, La Plata 2003.
c
c
c     Arguments:
c     =========
c
c     nshowers........ (input, integer) The number of showers currently
c                      completed.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'kernelpar.f'
c
c     Declaration of arguments.
c
      integer           nshowers
c
c     Declaration of shared data.
c
      include 'kernelcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Particles discriminated by stack.
c
      call stmomts(nshowers, npstacksp1, 5, totpcles)
      call stmomts(nshowers, npstacksp1, 5, nplost)
      call stmomts(nshowers, npstacksp1, 5, nplowe)
      call stmomts(nshowers, npstacksp1, 5, nprgnd)
c
c     Energies discriminated by stack.
c
      call stmomts(nshowers, npstacksp1, 5, eloss)
      call stmomts(nshowers, npstacksp1, 5, elost)
      call stmomts(nshowers, npstacksp1, 5, elowe)
      call stmomts(nshowers, npstacksp1, 5, eprgnd)
c
c     Particles not processed in stacks and their energies.
c
      call stmomts(nshowers, 1, 5, nneutrino)
      call stmomts(nshowers, 1, 5, nnotap)
      call stmomts(nshowers, 1, 5, eneutrino)
      call stmomts(nshowers, 1, 5, enotap)
c
c     Primary energy.
c
      call stmomts(nshowers, 1, 5, aveprim)
c
c     Depth of first interaction.
c
      call stmomts(nshowers, 2, 5, fstintdp)
c
      return
      end
c     --- End of routine momstxobs
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'observables2.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
