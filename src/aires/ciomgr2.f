c
c     FILE: ciomgr2.f                       Creation date: 09/DEC/1996.
c                                       LAST MODIFICATION: 05/OCT/2006.
c
c     Aires compressed i/o system (II): Routines to process already
c     created compressed files.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciorinit(inilevel, codsys, vrb, irc)
c
c     Initializing the Aires compressed i/o system for reading data.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2000.
c
c
c     Arguments:
c     =========
c
c     inilevel........ (input, integer) Initialization switch.
c                      If inilevel is zero or negative, all needed
c                      initialization routines are called. If positive
c                      only the cio system is initialized (The other
c                      routines must be called within the invoking
c                      program, before calling ciorinit: 1 = complete
c                      cio initialization, > 1 ==> only particle
c                      coding initialization.
c     codsys.......... (input, integer) Particle coding system
c                      identification. This variable permits selecting
c                      among several particle coding systems. The
c                      menu of available systems is explained within
c                      subroutine "ciosetpclecoding" (actually codsys
c                      is passed to this routine). The default
c                      coding system is the Aires system with decimal
c                      nuclear coding (A + 100 * Z). The default
c                      can be obtained setting codsys to a negative
c                      value.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return. 1 means that an invalid
c                      particle coding system was specified by codsys
c                      (default used).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           inilevel, codsys, vrb, irc
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
      include 'maincomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
      pgmcode = 3001
c
c     Checking initialization level.
c
      if (inilevel .le. 1) then
c
        if (inilevel .le. 0) then
c
c         Performing additional initializations.
c
          call clearerrmess
          call errinit
          call basicstart0
c
        endif
c
c       Basic initialization of the cio system.
c
        call cioinit0
c
      endif
c
c     Particle decoding initialization.
c
      call ciosetpclecoding(codsys, vrb, irc)
c
      if ((irc .eq. 0) .and. (vrb .eq. 1)) then
        call errprint(0, '*', 1, 'ciorinit',
     +       'Compressed i/o system successfully initialized.',
     +       0, 0, 0, 0.d0, ' ')
      endif
c
c     Labelling compressed files as closed.
c
      call cioclose
c
c     Labelling the index of the "last read head" file to null.
c
      lastheadfile2 = -1
c
      return
      end
c     --- End of routine ciorinit
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciorshutdown
c
c     Terminating (in an ordered fashion) a compressed file analysis
c     session.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2000.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     FIRST EXECUTABLE STATEMENT
c
c     Closing all cio files.
c
      call cioclose
c
c     Removing (if exists) the temporary files.
c
      call rmfile(rmkut, rmkfn)
      call rmfile(shzut, shzfn)
c
      return
      end
c     --- End of routine ciorshutdown
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciosetpclecoding(codsys, vrb, irc)
c
c     Initializing the particle coding arrays for the compressed i/o
c     system: Arrays to convert encoded codes into user level ones.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1998;
c                                Fermilab 1999;
c                                La Plata 1999, 2000, 2002, 2003, 2006.
c
c
c     Arguments:
c     =========
c
c     codsys.......... (input, integer) Particle coding system
c                      identification (for input only). This variable
c                      selects among several coding systems available:
c
c                        0  Aires internal coding system.
c                        1  Aires internal coding for elementary
c                           particles and decimal nuclear notation
c                           (A + 100 * Z).
c                        4  Particle Data Group coding system (Phys.
c                           Rev. D 45 (1992) S1). (*)
c                        5  CORSIKA program particle coding system.
c                        6  GEANT particle coding system.
c                        8  SIBYLL particle coding system. (**)
c                        9  Hillas style particle coding system. (**)
c                        -  Any other value is equivalent to
c                           codsys = 1.
c
c                      (*) For nuclei the notation:
c                          code = A + 100000 * Z, is used.
c                      (**) For nuclei the notation:
c                           code = A + 100 * Z, is used.
c
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means
c                      successful return. 1 means that an invalid
c                      particle coding system was specified by codsys
c                      (default used).
c
      implicit none
c
c     Compilation parameters.
c
      include 'pclepar.f'
      include 'pclecodes.f'
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           codsys, vrb, irc
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, a, n, z, zfac
      logical           decimalnuc, notairescode
      integer           geantnuc
      integer           nuclcode
c
c     FIRST EXECUTABLE STATEMENT
c
      irc        = 0
      currcodsys = codsys
c
      do i = 0, codemax1h
        cio2pcledecode(i) = -999999
      enddo
c
c     Decimal nuclear codes unless otherwise specified.
c
      decimalnuc = .true.
      zfac       = 100
c
c     Not AIRES codes assumed unless otherwise specified.
c
      notairescode = .true.
c
      if (codsys .eq. 0) then
c
c       Aires particle coding.
c
        do i = -maxpcle, maxncode
          cio2pcledecode(maxpcle + i) = i
        enddo
c
        decimalnuc   = .false.
        notairescode = .false.
c
      else if (codsys .eq. 5) then
c
c       Particle Data Group coding system (April 2002 revision).
c       K. Hagiwara, et al., Phys. Rev. D, 66 (2002) 010001-1.
c       (Website: pdg.lbl.gov).
c
        cio2pcledecode(maxpcle - gammacode)        =    22
        cio2pcledecode(maxpcle + gammacode)        =    22
        cio2pcledecode(maxpcle + positroncode)     =   -11
        cio2pcledecode(maxpcle + electroncode)     =    11
        cio2pcledecode(maxpcle + nuecode)          =    12
        cio2pcledecode(maxpcle + mupluscode)       =   -13
        cio2pcledecode(maxpcle + muminuscode)      =    13
        cio2pcledecode(maxpcle + numucode)         =    14
        cio2pcledecode(maxpcle + taupluscode)      =   -15
        cio2pcledecode(maxpcle + tauminuscode)     =    15
        cio2pcledecode(maxpcle + nutaucode)        =    16
        cio2pcledecode(maxpcle + pizerocode)       =   111
        cio2pcledecode(maxpcle + pipluscode)       =   211
        cio2pcledecode(maxpcle + piminuscode)      =  -211
        cio2pcledecode(maxpcle + k0lcode)          =   130
        cio2pcledecode(maxpcle + k0scode)          =   310
        cio2pcledecode(maxpcle + kpluscode)        =   321
        cio2pcledecode(maxpcle + kminuscode)       =  -321
        cio2pcledecode(maxpcle + etacode)          =   221
        cio2pcledecode(maxpcle + lambdacode)       =  3122
        cio2pcledecode(maxpcle + lambdabcode)      = -3122
        cio2pcledecode(maxpcle + sigma0code)       =  3212
        cio2pcledecode(maxpcle + sigma0bcode)      = -3212
        cio2pcledecode(maxpcle + sigmapluscode)    =  3222
        cio2pcledecode(maxpcle + sigmaplusbcode)   = -3222
        cio2pcledecode(maxpcle + sigmaminuscode)   =  3112
        cio2pcledecode(maxpcle + sigmaminusbcode)  = -3112
        cio2pcledecode(maxpcle + xi0code)          =  3322
        cio2pcledecode(maxpcle + xi0bcode)         = -3322
        cio2pcledecode(maxpcle + ximinuscode)      =  3312
        cio2pcledecode(maxpcle + ximinusbcode)     = -3312
        cio2pcledecode(maxpcle + omegaminuscode)   =  3334
        cio2pcledecode(maxpcle + omegaminusbcode)  = -3334
        cio2pcledecode(maxpcle + neutroncode)      =  2112
        cio2pcledecode(maxpcle + nbarcode)         = -2112
        cio2pcledecode(maxpcle + protoncode)       =  2212
        cio2pcledecode(maxpcle + pbarcode)         = -2212
c
c       Nuclei are coded in the form code = A + 100000 * Z
c
        zfac = 100000
c
      else if ((codsys .eq. 5) .or. (codsys .eq. 6)) then
c
c       CORSIKA program particle coding system (D. Heck, J. Knapp, J.N.
c       Capdevielle, G. Schatz, and T. Thouw, Forschungszentrum
c       Karlsruhe Report FZKA 6019 (1998)), or GEANT particle coding
c       system (CERN Program library Long Writeup W5013).
c       The codes for the tau particles are extensions to the
c       original coding system.
c       CORSIKA and GEANT coding differ in the neutrinos,
c       Cherenkov photons and nuclear codes:
c       CORSIKA has decimal codes: code = A + 100 * Z; in GEANT
c       nuclei coding is not biunivocal: AIRES(Z, N) goes to
c       GEANT(Z), except for deuterium, tritium, He^3, alpha particle,
c       Li^6, Be^7 and B^10.
c
        cio2pcledecode(maxpcle - gammacode)        = 50
        cio2pcledecode(maxpcle + gammacode)        =  1
        cio2pcledecode(maxpcle + positroncode)     =  2
        cio2pcledecode(maxpcle + electroncode)     =  3
        cio2pcledecode(maxpcle + nuecode)          =  4
        cio2pcledecode(maxpcle + mupluscode)       =  5
        cio2pcledecode(maxpcle + muminuscode)      =  6
        cio2pcledecode(maxpcle + numucode)         =  4
        cio2pcledecode(maxpcle + taupluscode)      = 86
        cio2pcledecode(maxpcle + tauminuscode)     = 87
        cio2pcledecode(maxpcle + nutaucode)        =  4
        cio2pcledecode(maxpcle + pizerocode)       =  7
        cio2pcledecode(maxpcle + pipluscode)       =  8
        cio2pcledecode(maxpcle + piminuscode)      =  9
        cio2pcledecode(maxpcle + k0lcode)          = 10
        cio2pcledecode(maxpcle + k0scode)          = 16
        cio2pcledecode(maxpcle + kpluscode)        = 11
        cio2pcledecode(maxpcle + kminuscode)       = 12
        cio2pcledecode(maxpcle + etacode)          = 17
        cio2pcledecode(maxpcle + lambdacode)       = 18
        cio2pcledecode(maxpcle + lambdabcode)      = 26
        cio2pcledecode(maxpcle + sigma0code)       = 20
        cio2pcledecode(maxpcle + sigma0bcode)      = 28
        cio2pcledecode(maxpcle + sigmapluscode)    = 19
        cio2pcledecode(maxpcle + sigmaplusbcode)   = 29
        cio2pcledecode(maxpcle + sigmaminuscode)   = 21
        cio2pcledecode(maxpcle + sigmaminusbcode)  = 27
        cio2pcledecode(maxpcle + xi0code)          = 22
        cio2pcledecode(maxpcle + xi0bcode)         = 30
        cio2pcledecode(maxpcle + ximinuscode)      = 23
        cio2pcledecode(maxpcle + ximinusbcode)     = 31
        cio2pcledecode(maxpcle + omegaminuscode)   = 24
        cio2pcledecode(maxpcle + neutroncode)      = 13
        cio2pcledecode(maxpcle + nbarcode)         = 25
        cio2pcledecode(maxpcle + protoncode)       = 14
        cio2pcledecode(maxpcle + pbarcode)         = 15
c
c       Nuclei and other codes different in CORSIKA.
c
        if (codsys .eq. 5) then
c
c         CORSIKA coding.
c
          cio2pcledecode(maxpcle - gammacode) = 9900
          cio2pcledecode(maxpcle + nuecode)   =   66
          cio2pcledecode(maxpcle - nuecode)   =   67
          cio2pcledecode(maxpcle + numucode)  =   68
          cio2pcledecode(maxpcle - numucode)  =   69
c
        else
c
c         GEANT coding.
c
          decimalnuc = .false.
c
          do i = minncode, maxncode
            call nucldecode(i, z, n, a)
            if (n .ge. 0) then
              if (z .ge. 5) then
                geantnuc = z + 61
              else if (z .eq. 4) then
                geantnuc = 64
              else if (z .eq. 3) then
                geantnuc = 62
             else if (z .eq. 2) then
                geantnuc = 47
              else
                geantnuc = 14
              endif
              cio2pcledecode(maxpcle + i) = geantnuc
            endif
          enddo
c
c         Deuterium, tritium, He^3, alpha particle, Li^6, Be^9 and
c         B^10.
c
          cio2pcledecode(maxpcle + nuclcode(1, 1, i)) = 45
          cio2pcledecode(maxpcle + nuclcode(1, 2, i)) = 46
          cio2pcledecode(maxpcle + nuclcode(2, 1, i)) = 49
          cio2pcledecode(maxpcle + nuclcode(2, 2, i)) = 47
          cio2pcledecode(maxpcle + nuclcode(3, 3, i)) = 61
          cio2pcledecode(maxpcle + nuclcode(4, 3, i)) = 63
          cio2pcledecode(maxpcle + nuclcode(5, 5, i)) = 65
c
       endif
c
      else if (codsys .eq. 8) then
c
c       SIBYLL particle codes (with decimal nuclei).
c
        cio2pcledecode(maxpcle - gammacode)     =   1
        cio2pcledecode(maxpcle + gammacode)     =   1
        cio2pcledecode(maxpcle + positroncode)  =   2
        cio2pcledecode(maxpcle + electroncode)  =   3
        cio2pcledecode(maxpcle + mupluscode)    =   4
        cio2pcledecode(maxpcle + muminuscode)   =   5
        cio2pcledecode(maxpcle + pizerocode)    =   6
        cio2pcledecode(maxpcle + pipluscode)    =   7
        cio2pcledecode(maxpcle + piminuscode)   =   8
        cio2pcledecode(maxpcle + kpluscode)     =   9
        cio2pcledecode(maxpcle + kminuscode)    =  10
        cio2pcledecode(maxpcle + k0lcode)       =  11
        cio2pcledecode(maxpcle + k0scode)       =  12
        cio2pcledecode(maxpcle + protoncode)    =  13
        cio2pcledecode(maxpcle + pbarcode)      = -13
        cio2pcledecode(maxpcle + neutroncode)   =  14
        cio2pcledecode(maxpcle + nbarcode)      = -14
        cio2pcledecode(maxpcle + nuecode)       =  15
        cio2pcledecode(maxpcle + numucode)      =  16
        cio2pcledecode(maxpcle + nutaucode)     =  17
c
c       Extensions to Sybill original coding:
c
        cio2pcledecode(maxpcle + taupluscode)      =  20
        cio2pcledecode(maxpcle + tauminuscode)     = -20
        cio2pcledecode(maxpcle + etacode)          =  22
        cio2pcledecode(maxpcle + lambdacode)       =  25
        cio2pcledecode(maxpcle + lambdabcode)      = -25
        cio2pcledecode(maxpcle + sigma0code)       =  26
        cio2pcledecode(maxpcle + sigma0bcode)      = -26
        cio2pcledecode(maxpcle + sigmapluscode)    =  27
        cio2pcledecode(maxpcle + sigmaplusbcode)   = -27
        cio2pcledecode(maxpcle + sigmaminuscode)   =  28
        cio2pcledecode(maxpcle + sigmaminusbcode)  = -28
        cio2pcledecode(maxpcle + xi0code)          =  29
        cio2pcledecode(maxpcle + xi0bcode)         = -20
        cio2pcledecode(maxpcle + ximinuscode)      =  31
        cio2pcledecode(maxpcle + ximinusbcode)     = -31
        cio2pcledecode(maxpcle + omegaminuscode)   =  32
c
      else if (codsys .eq. 9) then
c
c       Hillas style particle coding (Extended to match all the
c       particles known by Aires).
c
        cio2pcledecode(maxpcle - gammacode)     =  1
        cio2pcledecode(maxpcle + gammacode)     =  1
        cio2pcledecode(maxpcle + electroncode)  = -2
        cio2pcledecode(maxpcle + positroncode)  =  2
        cio2pcledecode(maxpcle + mupluscode)    =  3
        cio2pcledecode(maxpcle + muminuscode)   = -3
        cio2pcledecode(maxpcle + pipluscode)    =  4
        cio2pcledecode(maxpcle + piminuscode)   = -4
        cio2pcledecode(maxpcle + pizerocode)    =  5
        cio2pcledecode(maxpcle + protoncode)    =  7
        cio2pcledecode(maxpcle + pbarcode)      = -7
        cio2pcledecode(maxpcle + neutroncode)   =  6
        cio2pcledecode(maxpcle + nbarcode)      = -6
c
c       All neutrinos are mapped onto 0
c
        cio2pcledecode(maxpcle + nuecode)       =  0
        cio2pcledecode(maxpcle + numucode)      =  0
        cio2pcledecode(maxpcle + nutaucode)     =  0
c
c       Extensions to Hillas original coding:
c
c       Tau particles.
c
        cio2pcledecode(maxpcle + taupluscode)      =  10
        cio2pcledecode(maxpcle + tauminuscode)     = -10
c
c       Kaons, eta, and strange baryons.
c
        cio2pcledecode(maxpcle + kpluscode)        =  11
        cio2pcledecode(maxpcle + kminuscode)       = -11
        cio2pcledecode(maxpcle + k0scode)          =  12
        cio2pcledecode(maxpcle + k0lcode)          =  13
        cio2pcledecode(maxpcle + etacode)          =  14
        cio2pcledecode(maxpcle + lambdacode)       =  20
        cio2pcledecode(maxpcle + lambdabcode)      = -20
        cio2pcledecode(maxpcle + sigma0code)       =  21
        cio2pcledecode(maxpcle + sigma0bcode)      = -21
        cio2pcledecode(maxpcle + sigmapluscode)    =  22
        cio2pcledecode(maxpcle + sigmaplusbcode)   = -22
        cio2pcledecode(maxpcle + sigmaminuscode)   = -23
        cio2pcledecode(maxpcle + sigmaminusbcode)  =  23
        cio2pcledecode(maxpcle + xi0code)          =  24
        cio2pcledecode(maxpcle + xi0bcode)         = -24
        cio2pcledecode(maxpcle + ximinuscode)      = -26
        cio2pcledecode(maxpcle + ximinusbcode)     =  26
        cio2pcledecode(maxpcle + omegaminuscode)   = -28
c
      else
c
c       Aires particle coding with decimal nuclear coding.
c       (This option is the default).
c
        currcodsys = 1
c
        do i = -maxpcle, minncode
          cio2pcledecode(maxpcle + i) = i
        enddo
c
        notairescode = .false.
c
        if (codsys .ne. 1) then
          irc = 1
          if (vrb .gt. 0) then
            call errprint(0, '*', 2, 'ciosetpclecoding',
     +      'Unknown particle coding system. Using the default (1).',
     +      1, codsys, 0, 0.d0, ' ')
          endif
        endif
c
      endif
c
c     Decimal nuclear notation (code = A + zfac * Z, where zfac
c     is a power of 10).
c
      if (decimalnuc) then
        do i = minncode, maxncode
          call nucldecode(i, z, n, a)
          if (n .ge. 0) then
            cio2pcledecode(maxpcle + i) = a + zfac * z
          endif
        enddo
      endif
c
c     Escape codes for special particles are the same for
c     every coding system different to the AIRES system.
c
      if (notairescode) then
        do i = pescode1, pescode2
          cio2pcledecode(maxpcle + i) = i - (pescode1 + 99999)
        enddo
      endif
c
      cioinitlock = min(abs(cioinitlock), 100000) + 201
c
      return
      end
c     --- End of routine ciosetpclecoding
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine opencrofile(wdir, filename, header1, logbase, vrb,
     +                       fileid, irc)
c
c     Opening compressed i/o files for reading.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2000, 2001, 2003.
c
c
c     Arguments:
c     =========
c
c     wdir............ (input, character*(*)) The name of the directory
c                      where the file is placed. It defaults to the
c                      current directory when blank.
c     filename........ (input, character*(*)) The name of the file to
c                      open.
c     header1......... (input, integer) Integer switch to select
c                      reading (ge 0) or skipping (lt 0) the first
c                      part of the header.
c     logbase......... (input, integer) Variable to control the
c                      logarithmically scaled fields of the file
c                      records. If logbase is less than 2, then
c                      the returned logarithms will be natural
c                      logarithms. Otherwise base "logbase" will be
c                      returned (decimal ones if logbase = 10).
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     fileid.......... (output, integer) File identification. This
c                      variable should not be changed by the calling
c                      program. It must be used as a parameter of
c                      the reading and closing routines in order to
c                      specify the opened file.
c     irc............. (output, integer) Return code. 0 means
c                      successful return. 1 means successful return
c                      obtained with a file that was written with a
c                      previous AIRES version. 10 means that the file
c                      could be opened normally, but that it seems not
c                      to be a valid Aires compressed data file, or is
c                      a corrupted file; 12 invalid file header; 14
c                      not enough size in some of the internal arrays;
c                      16 format incompatibilities. 20: too many
c                      compressed files already opened. 300 < irc < 400
c                      indicates a version incompatibility (when
c                      processing files written with other AIRES
c                      version) or invalid version field (corrupt
c                      header). Any other value indicates an opening /
c                      header-reading error (irc equals the system
c                      return code plus 10000).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      character*(*)     wdir, filename
      integer           header1, logbase, vrb, fileid, irc
c
c     Declaration of shared data.
c
      include 'initcomm.f'
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           unit, i, j, k, verlen, ifil, xcodemax1
      integer           iiversion, jrc
      character*12      intheader
      character*24      version
      character*36      headintegers
c
c     cdf specific header.
c
      character*32      cdfheader0, cdfheader
      parameter         (cdfheader0 = '--- AIRES cdf --- ')
      integer           cdfintheader0, cdfintheader
      parameter         (cdfintheader0 = 0)
c
c     FIRST EXECUTABLE STATEMENT
c
c     Checking proper initialization of the cio system. If no previous
c     initialization is detected, then it is initialized here using
c     the default decoding system.
c
      if (cioinitlock .ne. 4167) then
        if (cioinitlock .eq. 3966) then
          call ciosetpclecoding(-1, -1, i)
        else
          call ciorinit(-1, -1, -1, i)
        endif
      endif
c
c     Searching an unused channel.
c
      do i = 1, mxcio2files
        fileid = i
        if (.not. cio2pen(i)) goto 1010
      enddo
c
c     All the available i/o channels are already open.
c
      irc    = 20
      fileid = -1
      if (vrb .gt. 0) then
        call errprint(0, '$CI1', max(3, vrb), 'opencrofile',
     +     'All the available i/o channels are already in use.',
     +     0, 0, 0, 0.d0, ' ')
      endif
      return
 1010 continue
c
c     I/O channel already defined. Opening the file.
c
      unit = minciout + fileid
      ifil = -2
      irc  = 10
c
c     Setting the complete filename.
c
      ciorecord0(001:200) = wdir
      ciorecord0(201:400) = filename
      call strim(200, ciorecord0(001:200), i)
      call strim(200, ciorecord0(201:400), j)
      call absfnset(ciorecord0(1:i), i, ciorecord0(201:j + 200), j,
     +              auxfilestring, k)
c
c     Opening the file.
c
      open(unit, file = auxfilestring, status = 'OLD',
     +           iostat = irc, err = 3010)
c
 2010 format(400a)
c
c     The file was successfully opened.
c
      ifil = -1
c
c     Reading the first record.
c
      read(unit, 2010, iostat = irc, err = 3020, end = 3030)
     +     cdfheader, intheader, version,
     +     pifilext(fileid), headintegers,
     +     (cio2chrdigits(i, fileid), i = 0, codemax1),
     +     ((numalchr2(i, j, fileid), i = 1, 2), j = 1, 3),
     +     (spechr2(i, fileid), i = 1, 32)
c
c     Before starting the checks, it is necessary to initialize the
c     corresponding ascii arrays.
c
c     Setting the corresponding internal variables.
c
      call cioinitr(cio2chrdigits(0, fileid), numalchr2(1, 1, fileid),
     +              spechr2(1, fileid),
     +              normalcharseq2(fileid), normalprtchar2(fileid),
     +              cio2intdigits(-256, fileid),
     +              ascarrayprt2(-256, fileid),
     +              vrb, irc)
c
      if (irc .ne. 0) then
        if (vrb .gt. 0) then
          call errprint(0, '$CI9', max(3, vrb), 'opencrofile',
     +       'File:', 0, 0, 0, 0.d0,
     +       filename(1:min(65, len(filename))))
        endif
        return
      endif
c
c     Internal variables set. Now it is possible to adequately
c     transform the input data, in the case the characters returned
c     differ from the original ones (due to the fact that the file
c     was written in a different machine).
c
      currciofile2 = fileid
c
      call restring(cdfheader)
      call restring(intheader)
      call restring(version)
      call restring(pifilext(fileid))
      call restring(headintegers)
c
      cdfintheader = -999999
      read(intheader, *, err = 1020) i
      cdfintheader = i
 1020 continue
c
c     Checking header.
c
      if ((cdfheader .ne. cdfheader0) .or.
     +    (cdfintheader .ne. cdfintheader0)) goto 3040
c
c     Checking version.
c
      call strim(24, version, verlen)
      call versioncheck(version(1:verlen), iiversion, jrc)
      rcversion(fileid) = iiversion
c
      if (jrc .ne. 0) then
c
c       Current and input version are not equal.
c
        if (jrc .gt. 100) then
c
c         Invalid version format.
c
          irc = 360
          if (vrb .gt. 0) then
            call errprint(0, '$DF1', max(3, vrb), 'opencrofile',
     +           filename(1:min(65, len(filename))),
     +           0, 0, 0, 0.d0, version(1:verlen))
          endif
          return
c
        else if (jrc .gt. 10) then
c
c         File corresponds to a future version of AIRES.
c
          irc = 350
          if (vrb .gt. 0) then
            call errprint(0, '$DF2', max(3, vrb), 'opencrofile',
     +           filename(1:min(65, len(filename))),
     +           0, 0, 0, 0.d0, version(1:verlen))
          endif
          return
c
        else
c
c         File was written with a previous AIRES version.
c         Applying backwards compatibility.
c
          if (iiversion .gt. 01000003) then
c
c           Files created with Aires versions posterior to 1.0.0c can be
c           processed normally.
c
            jrc = 1
c
          else
c
c           Version incompatibility.
c
            if (vrb .gt. 0) then
              call errprint(0, '$CI1', max(3, vrb), 'opencrofile',
     +           'Incompatibility with current Aires version.',
     +            0, 0, 0, 0.d0, ' ')
            endif
            irc = 300 + jrc
            return
c
          endif
        endif
c
      else
c
c       File was written with the same (current) AIRES version.
c       Applying "null" backwards compatibility.
c       (Nothing by now).
c
        jrc = 0
c
      endif
c
c     Checking encoding base.
c
      read(headintegers, *, iostat = irc, err = 3020) i, xcodemax1, j
      if (xcodemax1 .ne. codemax1) goto 3040
c
c     All basic checks passed.
c     Reading the complete file header.
c
      call ciogetheaders(fileid, unit, header1, irc)
      if (irc .ne. 0) goto 3040
c
      lastopciofile2 = fileid
      lastheadfile2  = fileid
c
c     The header data was completely read. Adapting the different
c     variables for record processing.
c
      call cioinit2(fileid, logbase)
c
c     Heading data completely processed. The file is ready for
c     record processing.
c
      cio2pen(fileid)     = .true.
      pifilename(fileid)  = auxfilestring
      cio2reclast(fileid) = cio2fillpoint(0, fileid)
      cio2recprev(fileid) = cio2reclast(fileid)
      cio2recread(fileid) = 0
      irc = jrc
c
      if (vrb .eq. 1) then
        call errprint(0, '*', 1, 'opencrofile',
     +     'AIRES compressed data file successfully opened:',
     +     0, 0, 0, 0.d0,
     +     filename(1:min(65, len(filename))))
      endif
c
      return
c
c     I/O error exit.
c
 3010 continue
      close(unit)
      if (vrb .gt. 0) then
        call errprint(0, '$CI1', max(3, vrb), 'opencrofile',
     +     'System return code is listed:',
     +     1, irc, 0, 0.d0,
     +     filename(1:min(65, len(filename))))
      endif
      irc    = irc + 10000
      fileid = ifil
      return
c
 3020 continue
      irc = irc + 10000
 3022 continue
      close(unit)
      if (vrb .gt. 0) then
        call errprint(0, '$CI3', max(3, vrb), 'opencrofile',
     +     'System return code is listed:',
     +     1, irc - 10000, 0, 0.d0,
     +     filename(1:min(65, len(filename))))
      endif
      fileid = ifil
      return
c
 3030 continue
      close(unit)
      if (vrb .gt. 0) then
        call errprint(0, '$CI3', max(3, vrb), 'opencrofile',
     +     'Premature end of file.',
     +     0, 0, 0, 0.d0,
     +     filename(1:min(65, len(filename))))
      endif
      irc    = irc + 10000
      fileid = ifil
      return
c
 3040 continue
      lastopciofile2 = -1
      lastheadfile2  = -1
      if (irc .ge. 10000) goto 3022
      close(unit)
      irc = 10
      if (vrb .gt. 0) then
        call errprint(0, '*', max(3, vrb), 'opencrofile',
     +     'Missing or invalid compressed file header data.',
     +     0, 0, 0, 0.d0,
     +     filename(1:min(65, len(filename))))
      endif
      return
c
      end
c     --- End of routine opencrofile
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioclose
c
c     Closing compressed i/o files.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ifile, ut
c
c     FIRST EXECUTABLE STATEMENT
c
      do ifile = 1, mxcio2files
        ut = minciout + ifile
        close(ut)
        cio2pen(ifile) = .false.
      enddo
c
      lastopciofile2 = -1
c
      return
      end
c     --- End of routine cioclose
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioclose1(fileid)
c
c     Closing a given compressed i/o file.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) File identification.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      integer           fileid
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ut
c
c     FIRST EXECUTABLE STATEMENT
c
      ut = minciout + fileid
      close(ut)
      cio2pen(fileid) = .false.
      lastopciofile2  = -1
c
      return
      end
c     --- End of routine cioclose1
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioinitr(fchrdigits, fnumalchr, fspechr,
     +                    fnormalcharseq, fnormalprtchar,
     +                    fintdigits, fascarrayprt, vrb, irc)
c
c     Initializing ascii arrays and related data to process old files,
c     eventually written in other machines.
c
c     Written by: S. J. Sciutto, La Plata 1996, 2001.
c
c
c     Arguments:
c     =========
c
c     fchrdigits...... (input, character*1, array(0:codemax1)) The
c                      sequence of ascii characters used to encode the
c                      (base codebase) digits, as read from the old
c                      file.
c     fnumalchr....... (input, character*1, array(2, 3)) The sequences
c                      of alphanumeric ascii characters (see routine
c                      cioinit0), as read from the old file.
c     fspechr......... (input, character*1, array(32)) The sequences
c                      of special printable ascii characters (see
c                      routine cioinit0), as read from the old file.
c     fnormalcharseq.. (output, logical) .true. if the character set
c                      read from the file is identical to the current
c                      machine's one.
c     fnormalprtchar.. (output, logical) .true. if the printable
c                      characters read from the old file are coincident
c                      with the current machine's ones.
c     fintdigits...... (output, integer, array(-256:255)) Number
c                      decoding integer array.
c                      (fintdigits(ichar(fchrdigits(i))) is equal to i,
c                      i = 0,...,codemax1).
c     fascarrayprt.... (output, character*1, array(-256:255)) Sequence
c                      of characters to decode strings of the old file.
c                      If fnormalprtchar is true, the elements
c                      fascarryprt(i) are equal to char(i).
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. Zero means normal
c                      return.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
c
c     Declaration of arguments.
c
      character*1       fchrdigits(0:codemax1)
      character*1       fnumalchr(2, 3)
      character*1       fspechr(32)
      logical           fnormalcharseq, fnormalprtchar
      integer           fintdigits(-256:255)
      character*1       fascarrayprt(-256:255)
      integer           vrb, irc
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, l, i1, i2
      integer           range0, range1
      integer           numalseq(2, 3)
      integer           speseq(32)
c
c     The ascii character data were already read from the
c     corresponding file header (arrays "fchrdigits", "fnumalchr" and
c     "spechr"). We are going to set the array of integer "digits" and
c     test if the character set of the current machine matchs the one
c     used when writing the file. Notice that the negative indices are
c     necessary to support systems where the "ichar" function returns
c     negative values (the usual case is when ichar return values in
c     the range [-128, 127], instead of [0, 255]).
c
      irc = 12
c
      do i = -256, 255
        fintdigits(i) = 0
      enddo
c
      do i = 0, codemax1
        fintdigits(ichar(fchrdigits(i))) = i
      enddo
c
c     Checking if all the digits were set.
c
      j = 0
      do i = -256, 255
        j = j + fintdigits(i)
      enddo
c
      if (j .ne. ((codebase * codemax1) / 2)) then
        if (vrb .gt. 0) then
          call errprint(0, '$CI9', max(3, vrb), 'cioinitr',
     +      'Incomplete or invalid encoded digit character set.',
     +      0, 0, 0, 0.d0, ' ')
        endif
        return
      endif
c
c     Comparing the sequence with the current machine's one.
c
      fnormalcharseq = .true.
      do i = -256, 255
        if (fintdigits(i) .ne. ciointdigits(i)) then
          fnormalcharseq = .false.
        endif
      enddo
c
c     Analysing the printable characters.
c
      do i = -256, 255
        fascarrayprt(i) = ascarrayprt(i)
      enddo
c
c     Setting the integer arrays.
c
      do j = 1, 3
        do i = 1, 2
          numalseq(i, j) = fintdigits(ichar(fnumalchr(i, j)))
        enddo
      enddo
c
      do i = 1, 32
        speseq(i) = fintdigits(ichar(fspechr(i)))
      enddo
c
c     Setting the printable character array.
c
      fnormalprtchar = .true.
c
      do j = 1, 3
c
        range0 = abs(numalseq0(2, j) - numalseq0(1, j))
        range1 = abs(numalseq(2, j) - numalseq(1, j))
c
        if (range0 .ne. range1) then
          if (vrb .gt. 0) then
            call errprint(0, '$CI9', max(3, vrb), 'cioinitr',
     +      'Invalid range when setting ascii sequence number:',
     +      1, j, 0, 0.d0, ' ')
          endif
          return
        endif
c
        if (numalseq0(1, j) .le. numalseq0(2, j)) then
          i1 = 1
        else
          i1 = -1
        endif
c
        if (numalseq(1, j) .le. numalseq(2, j)) then
          i2 = 1
        else
          i2 = -1
        endif
c
        k = numalseq(1, j)
        do i = numalseq0(1, j), numalseq0(2, j), i1
          l = ichar(fchrdigits(k))
          if (l .ne. i) fnormalprtchar = .false.
          fascarrayprt(l) = char(i)
          k = k + i2
        enddo
c
      enddo
c
      do i = 1, 32
        k = ichar(fchrdigits(speseq(i)))
        l = speseq0(i)
        if (k .ne. l) fnormalprtchar = .false.
        fascarrayprt(k) = char(l)
      enddo
c
      irc = 0
      return
      end
c     --- End of routine cioinitr.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cio2zero(ifile)
c
c     Initializing some internal arrays. (To be done before reading
c     a file header).
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) Compressed file number.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of arguments.
c
      integer           ifile
c
c     Declaration of internal variables and arrays.
c
      integer           i, j
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 0, mxciortype
        nrecfield2(1, i, ifile) = 1
        do j = 2, mxcioftypes
          nrecfield2(j, i, ifile) = 0
        enddo
      enddo
c
      return
c
      end
c     --- End of routine cio2zero.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine cioinit2(ifile, logbase)
c
c     Some internal initializations to do after reading a compressed
c     file header.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2002, 2003.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) Compressed file number.
c     logbase......... (input, integer) Variable to control the
c                      logarithmically scaled fields of the file
c                      records. If logbase is less than 2, then
c                      the returned logarithms will be natural
c                      logarithms. Otherwise base "logbase" will be
c                      returned (decimal ones if logbase = 10).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of arguments.
c
      integer           ifile, logbase
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, ftype, trlen
      double precision  factor
c
c     FIRST EXECUTABLE STATEMENT
c
c     Evaluating some internal arrays needed to process the different
c     records.
c
      do i = 0, nrectypes2(ifile)
        nrecfield2(0, i, ifile)  = nrecfield2(1, i, ifile) -
     +                             nrecfield2(1, 0, ifile)
        startfield2(0, i, ifile) = nrecfield2(1, 0, ifile)
        cio2dynrecty(i, ifile)   = (cio2dynfield(i, ifile) .gt. 0)
      enddo
c
c     Evaluating exact lengths of records (without added fields).
c
      do i = 0, nrectypes2(ifile)
c
        trlen = 0
        do j = 1, mxcioftypes
          trlen = trlen + nrecfield2(j, i, ifile) * ciofieldlen(j)
        enddo
c
        cio2reclen0(i, ifile) = trlen
c
      enddo
c
c     The starting positions of floating point fields are shifted
c     to place them directly in the output vector.
c
      do i = 0, nrectypes2(ifile)
        do j = lastscaledtype1 + 1, lastscaledtype2
          startfield2(j, i, ifile) = startfield2(j, i, ifile) +
     +                               cio2lfscale1(i, ifile)
        enddo
      enddo
c
c     The starting positions for alternative record types must be
c     disminished in 1 because of the elimination of the control field.
c
      do i = 1, nrectypes2(ifile)
        do j = 1, lastscaledtype1
          startfield2(j, i, ifile) = startfield2(j, i, ifile) - 1
        enddo
        do j = lastscaledtype2 + 1, mxcioftypes
          startfield2(j, i, ifile) = startfield2(j, i, ifile) - 1
        enddo
      enddo
c
      cio2firstalt(ifile) = cio2nullcode(ifile) - nrectypes2(ifile)
c
c     Modifying the scaling factors of the logarithmically scaled
c     quantities, and setting the always positive floating point
c     numbers switchs.
c
      if (logbase .gt. 1) then
        filelbcf(ifile) = log(dfloat(logbase))
        factor          = 1.d0 / filelbcf(ifile)
      else
        filelbcf(ifile) = 1.d0
        factor          = 1.d0
      endif
      filelogbase(ifile) = logbase
c
      do i = 0, nrectypes2(ifile)
c
        ftype = 1
        k     = 0
        do j = 1, totrecfields2(i, ifile)
c
 1010     continue
          if (k .lt. (nrecfield2(ftype, i, ifile) * nsubfields(ftype)))
     +    then
            k = k + 1
          else
            ftype = ftype + 1
            k     = 0
            if (ftype .le. mxcioftypes) goto 1010
          endif
c
          if (ftype .le. lastscaledtype1) then
            if (cio2flogs(j, i, ifile)) then
              cio2frca(j, i, ifile) = factor * cio2frca(j, i, ifile)
              cio2frcb(j, i, ifile) = factor * cio2frcb(j, i, ifile)
            endif
          else
            if (ftype .eq. fltflt5ftype) then
              cio2alwpff5(k, i, ifile) = cio2flogs(j, i, ifile)
            else if (ftype .eq. flt2ftype) then
              cio2alwpf2(k, i, ifile)  = cio2flogs(j, i, ifile)
            endif
          endif
        enddo
      enddo
c
      return
c
      end
c     --- End of routine cioinit2.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine ciogetheaders(ifile, ut, header1, irc)
c
c     Reading and processing compressed i/o headers.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 1999, 2002, 2003.
c
c
c     Arguments:
c     =========
c
c     ifile........... (input, integer) Compressed file number.
c     ut.............. (input, integer) FORTRAN logical unit connected
c                      to the file.
c     header1......... (input, integer) Integer switch to select
c                      reading (ge 0) or skipping (lt 0) the first
c                      part of the header.
c     irc............. (output, integer) Return code. Zero means normal
c                      return. The error codes are like the ones
c                      explained in routine "opencrofile".
c
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of shared data.
c
      include 'ciocomm.f'
      include 'cio2comm.f'
c
c     Declaration of arguments.
c
      integer           ifile, ut, header1, irc
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, nrec, nrec111
      integer           xciorsize, xcioftypes, xlastdefrecftype
      integer           xlastscaledtype1, xlastscaledtype2
      integer           itmp1(mxcioftypes)
c
      logical           after269
c
c     FIRST EXECUTABLE STATEMENT
c
c     It is assumed that all the ascii arrays corresponding to the
c     indicated file are already properly set.
c
      currciofile2 = ifile
c
c     FIRST PART OF THE HEADER: Simulation program specific variables.
c                               This section is read only if parameter
c                               "header1" is not negative.
c
      if (header1 .ge. 0) then
c
        call ciogetheader1(ifile, ut, irc)
        if (abs(irc) .gt. 1) return
c
c       Header separator.
c
c       The next record contains the string '*-*-*' together with the
c       file number used when writing the header. These data are not
c       used here, but the string is checked to be secure that we have
c       arrived to the end of the first part of the header
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           ciorecord0(1:17)
        call restring(ciorecord0(1:17))
        if (ciorecord0(1:5) .ne. '*-*-*') return
c
      else
c
c       Skipping the first part of the header. To do this, all lines
c       are read until detecting the separator (se above).
c
 1010   continue
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           ciorecord0(1:5)
        call restring(ciorecord0(1:5))
        if (ciorecord0(1:5) .ne. '*-*-*') goto 1010
c
      endif
 2001 format(400a)
c
c     SECOND PART OF THE HEADER: Data coding specifications.
c
c     Backwards compatibility stuff.
c
      after269 = (rcversion(ifile) .gt. 02060900)
c
c     Data about all cio files currently defined.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:11)
      call restring(ciorecord0(1:11))
      read(ciorecord0(1:11), *, err = 3030)
     +        npifiles(ifile)
c
      if (npifiles(ifile) .gt. 0) then
        j = 12 * npifiles(ifile)
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           ciorecord0(1:j)
        call restring(ciorecord0(1:j))
        pifiledefno(ifile) = -99
        do i = 1, npifiles(ifile)
          j = 12 * i
          if (ciorecord0(j-11:j) .eq. pifilext(ifile)) then
            pifiledefno(ifile) = i
          endif
        enddo
        irc = 12
        if (pifiledefno(ifile) .lt. 0) return
      endif
c
c     Brief description of the file.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         pifiletitle(ifile)
      call restring(pifiletitle(ifile))
c
c     Some sizes of arrays.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:33)
      call restring(ciorecord0(1:33))
      read(ciorecord0(1:33), *, err = 3030)
     +         xciorsize, xcioftypes, xlastdefrecftype
c
c     Size checking.
c
      irc = 14
      if (xciorsize .gt. mxciorsize) return
      if (xcioftypes .gt. mxcioftypes) return
c
      irc = 16
      if (xlastdefrecftype .ne. lastdefrecftype) return
c
c     Zeroing some internal arrays.
c
      call cio2zero(ifile)
c
c     Basic data related with coding.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:11*(xcioftypes+3))
      call restring(ciorecord0(1:11*(xcioftypes+3)))
      read(ciorecord0(1:11*(xcioftypes+3)), *, err = 3030)
     +         cio2nullcode(ifile),
     +         xlastscaledtype1, xlastscaledtype2,
     +         (itmp1(i), i = 1, xcioftypes)
c
      irc = 18
      if (cio2nullcode(ifile) .gt. codemax1h) return
      if (xlastscaledtype1 .ne. lastscaledtype1) return
      if (xlastscaledtype2 .ne. lastscaledtype2) return
      do i = 1, xcioftypes
        if (itmp1(i) .ne. ciofieldlen(i)) return
      enddo
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:11*xcioftypes)
      call restring(ciorecord0(1:11*xcioftypes))
      read(ciorecord0(1:11*xcioftypes), *, err = 3030)
     +         (itmp1(i), i = 1, xcioftypes)
c
      irc = 20
      do i = 1, xcioftypes
        if (itmp1(i) .ne. ciofieldid(i)) return
      enddo
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:11*xcioftypes)
      call restring(ciorecord0(1:11*xcioftypes))
      read(ciorecord0(1:11*xcioftypes), *, err = 3030)
     +         (itmp1(i), i = 1, xcioftypes)
c
      irc = 22
      do i = 1, xcioftypes
        if (itmp1(i) .ne. nsubfields(i)) return
      enddo
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:33)
      call restring(ciorecord0(1:33))
      read(ciorecord0(1:33), *, err = 3030)
     +         (itmp1(i), i = 1, 3)
c
      irc = 24
      if (itmp1(1) .le. 0) return
      if (itmp1(1) .gt. ciopclecodedata(1)) return
      if (itmp1(2) .ne. ciopclecodedata(2)) return
      if (itmp1(3) .gt. ciopclecodedata(3)) return
      if (itmp1(1) .ge. itmp1(2)) return
      if (itmp1(2) .ge. itmp1(3)) return
c
c     Record length, number of different records and related
c     variables.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:22)
      call restring(ciorecord0(1:22))
      read(ciorecord0(1:22), *, err = 3030)
     +         cio2buflen(ifile), nrectypes2(ifile)
c
      nrec    = nrectypes2(ifile)
      nrec111 = 11 * (nrec + 1)
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:nrec111)
      call restring(ciorecord0(1:nrec111))
      read(ciorecord0(1:nrec111), *, err = 3030)
     +         (cio2reclen(i, ifile), i = 0, nrec)
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:nrec111)
      call restring(ciorecord0(1:nrec111))
      read(ciorecord0(1:nrec111), *, err = 3030)
     +         (cio2fillpoint(i, ifile), i = 0, nrec)
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:nrec111)
      call restring(ciorecord0(1:nrec111))
      read(ciorecord0(1:nrec111), *, err = 3030)
     +         (totrecfields2(i, ifile), i = 0, nrec)
c
      if (after269) then
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           ciorecord0(1:nrec111)
        call restring(ciorecord0(1:nrec111))
        read(ciorecord0(1:nrec111), *, err = 3030)
     +           (cio2dynfield(i, ifile), i = 0, nrec)
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           ciorecord0(1:nrec111)
        call restring(ciorecord0(1:nrec111))
        read(ciorecord0(1:nrec111), *, err = 3030)
     +           (cio2dynfwcix(i, ifile), i = 0, nrec)
c
      else
        do i = 0, nrec
          cio2dynfield(i, ifile) = 0
          cio2dynfwcix(i, ifile) = totrecfields2(i, ifile)
        enddo
      endif
c
      irc = 14
      do i = 0, nrec
        if (totrecfields2(i, ifile) .gt. mxciofields) return
        if (cio2dynfwcix(i, ifile) .gt. mxciofields) return
      enddo
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:nrec111)
      call restring(ciorecord0(1:nrec111))
      read(ciorecord0(1:nrec111), *, err = 3030)
     +         (cio2ffscaled(i, ifile), i = 0, nrec)
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:nrec111)
      call restring(ciorecord0(1:nrec111))
      read(ciorecord0(1:nrec111), *, err = 3030)
     +         (cio2lfnoscal(i, ifile), i = 0, nrec)
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:nrec111)
      call restring(ciorecord0(1:nrec111))
      read(ciorecord0(1:nrec111), *, err = 3030)
     +         (cio2lfscale1(i, ifile), i = 0, nrec)
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:nrec111)
      call restring(ciorecord0(1:nrec111))
      read(ciorecord0(1:nrec111), *, err = 3030)
     +         (cio2nfscale2(i, ifile), i = 0, nrec)
c
c     Record definitions record by record.
c
      do i = 0, nrec
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           cio2recname(i, ifile)
        call restring(cio2recname(i, ifile))
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           ciorecord0(1:11*xcioftypes)
        call restring(ciorecord0(1:11*xcioftypes))
        read(ciorecord0(1:11*xcioftypes), *, err = 3030)
     +           (nrecfield2(j, i, ifile), j = 1, xcioftypes)
c
        read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +           ciorecord0(1:11*xcioftypes)
        call restring(ciorecord0(1:11*xcioftypes))
        read(ciorecord0(1:11*xcioftypes), *, err = 3030)
     +           (startfield2(j, i, ifile), j = 1, xcioftypes)
c
c       Field definitions.
c
        do j = 1, max(cio2dynfwcix(i, ifile), totrecfields2(i, ifile))
c
          read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +             ciorecord0(1:287)
          call restring(ciorecord0(1:287))
          cio2fname(j, i, ifile) = ciorecord0(1:42)
          read(ciorecord0(43:287), *, err = 3030)
     +              cio2fminv(j, i, ifile), cio2fmaxv(j, i, ifile),
     +              cio2fwsc0(j, i, ifile), cio2fwsc1(j, i, ifile),
     +              cio2frsc0(j, i, ifile), cio2frsc1(j, i, ifile),
     +              cio2fwca(j, i, ifile), cio2fwcb(j, i, ifile),
     +              cio2frca(j, i, ifile), cio2frcb(j, i, ifile),
     +              cio2flogs(j, i, ifile)
c
        enddo
      enddo
c
c     End of file header.
c
c     The next record contains the string '*+*+*' together with the
c     file number used when writing the header and the header creation
c     date. These data are not used here, but the string is checked to
c     be secure that we have arrived to the end of the header.
c
      read(ut, 2001, iostat = irc, end = 3010, err = 3010)
     +         ciorecord0(1:44)
      call restring(ciorecord0(1:44))
      irc = 2
      if (ciorecord0(1:5) .ne. '*+*+*') return
c
c     No more header records.
c
      irc = 0
c
      return
c
c     Error messages.
c
 3010 continue
      close(ut)
      irc   = irc + 10000
      ifile = -1
      return
c
 3030 continue
      irc = 10
      return
c
      end
c     --- End of routine ciogetheaders.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function getcrorecord(fileid, intfields, realfields, altrec,
     +                      vrb, irc)
c
c     Reading a record from a compressed data file already opened.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2003.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     intfields....... (output, integer, array(*)) Integer fields
c                      of the record. This includes the non-scaled
c                      quantities and (at the end) the date-time
c                      specification(s) (if any). The calling
c                      program must provide enough space for this
c                      array (The minimum dimension is the maximum
c                      number of fields that can appear in a record
c                      plus 1). Positions beyond the last integer
c                      fields are used as scratch working space.
c     realfields...... (output, double precision, array(0:*)) Real
c                      fields of the record. The calling program must
c                      provide enough space for this array.
c     altrec.......... (output, logical) True if the record is an
c                      alternative record. False if a default record
c                      was obtained.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means that a
c                      default record was successfully read.
c                      i (i > 0) means that an alternative record of
c                      type i was successfully read. -1 means that an
c                      end-of-file condition was got from the
c                      corresponding file.  Any other value indicates
c                      a reading error (irc equals the system return
c                      code plus 10000).
c
c     Return value: (logical) True if a record was successfully read.
c     ============  False othwerise (EOF or I/O error).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      logical           getcrorecord
      integer           fileid, vrb, irc
      integer           intfields(1)
      double precision  realfields(0:1)
      logical           altrec
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           i, i0, j, k, ki, nadded
      integer           lastchar, lastcha0, jns
      integer           ist(0:mxcioftypes)
      integer           nfi(0:mxcioftypes)
      integer           raw(mxciofields)
      logical           ciorebuffer
c
c     FIRST EXECUTABLE STATEMENT
c
      currciofile2 = fileid
c
c     Refreshing the buffer (if necessary).
c
      if (cio2reclast(fileid) .ge. cio2fillpoint(0, fileid)) then
c
c       The pointer arrived to the end of the buffer. Refreshing.
c
        getcrorecord = ciorebuffer(fileid, vrb, irc)
        if (.not. getcrorecord) return
c
      endif
c
      do i = 1, lastdefrecftype
        nfi(i) = nrecfield2(i, 0, fileid)
        ist(i) = startfield2(i, 0, fileid)
      enddo
c
 1010 continue
      lastchar = cio2reclast(fileid)
      lastcha0 = lastchar
c
c     Getting the first set of type 1 fields to determine the record
c     type.
c
      call intintdecode4(nfi(1), cio2record1(fileid),
     +                   lastchar, intfields(1), intfields(ist(1)))
c
c     Analysing the record type.
c
      if (intfields(1) .lt. cio2firstalt(fileid)) then
c
c       It is a default record (this will happen most frequently).
c
        irc    = 0
        altrec = .false.
c
c       In the default record there are no more records of type 1.
c       Getting the remainding fields.
c
c       Type 2 fields.
c
        call intintdecode5(nfi(2), cio2record1(fileid), lastchar,
     +                     intfields(ist(2)))
c
c       Type 3 fields.
c
        call intdecode2(nfi(3), cio2record1(fileid), lastchar,
     +                  intfields(ist(3)))
c
c       Type 4 fields.
c
        call fltfltdecode5(nfi(4), cio2alwpff5(1, 0, fileid),
     +                     cio2record1(fileid), lastchar,
     +                     realfields(ist(4)))
c
c       Type 5 fields.
c
        call fltdecode2(nfi(5), cio2alwpf2(1, 0, fileid),
     +                  cio2record1(fileid), lastchar,
     +                  realfields(ist(5)))
c
c       No fields of type greater than 5 in the default record.
c
c       Integer field number 1 is a particle code. Decoding it.
c
        intfields(1) = cio2pcledecode(intfields(1))
c
c       Inverse scaling to reobtain the real fields.
c
c       Integer coded real numbers.
c
        k  = cio2ffscaled(0, fileid)
        ki = k - 1
        do i = 0, cio2lfscale1(0, fileid)
          ki = k + i
          realfields(i) = cio2frca(ki, 0, fileid) * intfields(ki) +
     +                    cio2frcb(ki, 0, fileid)
        enddo
c
c       Floating point numbers.
c
        i0 = cio2lfscale1(0, fileid)
        do j = 1, cio2nfscale2(0, fileid)
          i  = i0 + j
          ki = ki + 1
          realfields(i) = cio2frca(ki, 0, fileid) * realfields(i) +
     +                    cio2frcb(ki, 0, fileid)
        enddo
c
        cio2reclast(fileid) = lastchar
        cio2recread(fileid) = cio2recread(fileid) + 1
        getcrorecord        = .true.
c
      else if (intfields(1) .lt. cio2nullcode(fileid)) then
c
c       It is an alternative record.
c
        irc    = cio2nullcode(fileid) - intfields(1)
        altrec = .true.
c
c       Reordering the data already read.
c       The first field is eliminated since it is the control code.
c
        jns = nfi(1)
        do i = 2, jns
          intfields(i - 1) = intfields(i)
        enddo
        do i = 1, jns
          raw(i) = intfields(jns + i)
        enddo
c
        do i = 0, mxcioftypes
          ist(i) = startfield2(i, irc, fileid)
          nfi(i) = nrecfield2(i, irc, fileid)
        enddo
c
c       Getting the remainding fields.
c
c       Remaining type 1 fields.
c
        call intintdecode4(nfi(0), cio2record1(fileid),
     +                     lastchar, intfields(jns), raw(jns + 1))
c
c       Rearranging integer fields again.
c
        jns = nfi(1)
        do i = 1, nfi(1)
          intfields(jns) = raw(i)
          jns = jns + 1
        enddo
c
c       Type 1 Integer field number 1 is a particle code (if it
c       exists). Decoding it.
c
        if (nfi(1) .gt. 1) intfields(1) = cio2pcledecode(intfields(1))
c
c       Type 2 fields.
c
        call intintdecode5(nfi(2), cio2record1(fileid), lastchar,
     +                     intfields(ist(2)))
c
c       Type 3 fields.
c
        call intdecode2(nfi(3), cio2record1(fileid), lastchar,
     +                  intfields(ist(3)))
c
c       Type 4 fields.
c
        call fltfltdecode5(nfi(4), cio2alwpff5(1, irc, fileid),
     +                     cio2record1(fileid), lastchar,
     +                     realfields(ist(4)))
c
c       Type 5 fields.
c
        call fltdecode2(nfi(5), cio2alwpf2(1, irc, fileid),
     +                  cio2record1(fileid), lastchar,
     +                  realfields(ist(5)))
c
c       Type 6 fields.
c
        call fltdecode5(nfi(6), cio2record1(fileid), lastchar,
     +                  realfields(ist(6)))
c
c       Inverse scaling to reobtain the real fields.
c
c       Integer coded real numbers.
c
        k   = cio2ffscaled(irc, fileid)
        jns = k - 1
        ki  = jns
        do i = 0, cio2lfscale1(irc, fileid)
          ki  = k + i
          realfields(i) = cio2frca(ki, irc, fileid) *
     +                                          intfields(ki - 1) +
     +                    cio2frcb(ki, irc, fileid)
        enddo
c
c       Floating point numbers.
c
        i0 = cio2lfscale1(irc, fileid)
        do j = 1, cio2nfscale2(irc, fileid)
          i0 = i0 + 1
          ki = ki + 1
          realfields(i0) = cio2frca(ki, irc, fileid) * realfields(i0)
     +                     + cio2frcb(ki, irc, fileid)
        enddo
c
c       Type 7 fields.
c
        do j = 1, nfi(7)
          call datidecode(cio2record1(fileid), lastchar,
     +                    intfields(jns))
          jns = jns + 6
        enddo
c
c       Getting and inverse scaling dynamically added fields.
c
        if (cio2dynrecty(irc, fileid)) then
c
          call intdecode2(1, cio2record1(fileid), lastchar, nadded)
c
          intfields(jns) = nadded
c
          if (nadded .gt. 0) then
            call fltdecode5(nadded, cio2record1(fileid), lastchar,
     +                      realfields(i0 + 1))
            ki = cio2dynfwcix(irc, fileid)
            do j = 1, nadded
              i = i0 + j
              realfields(i) = cio2frca(ki, irc, fileid) * realfields(i)
     +                        + cio2frcb(ki, irc, fileid)
            enddo
          endif
c
          i                   = 1 + (lastchar - lastcha0 - 1) /
     +                               cio2reclen(0, fileid)
          cio2reclast(fileid) = lastcha0 + i * cio2reclen(0, fileid)
c
        else
c
          cio2reclast(fileid) = lastcha0 + cio2reclen(irc, fileid)
c
        endif
c
        cio2recread(fileid) = cio2recread(fileid) + 1
        getcrorecord        = .true.       
c
      else
c
c       It is a null record, which marks the end of an incompletely
c       filled block. It is necessary to refresh the buffer and retry.
c
        getcrorecord = ciorebuffer(fileid, vrb, irc)
        if (getcrorecord) goto 1010
c
      endif
c
      cio2recprev(fileid) = lastcha0
c
      return
      end
c     --- End of routine getcrorecord.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function ciorebuffer(fileid, vrb, irc)
c
c     Refreshing the buffer of a compressed data file.
c
c     Written by: S. J. Sciutto, La Plata 1996.
c
c
c     Arguments:
c     =========
c
c     fileid.......... (input, integer) Compressed file number. This
c                      parameter identifies the file to be used for
c                      reading, and is set by the file opening
c                      routine.
c     vrb............. (input, integer) Verbosity control. If vrb is
c                      zero or negative then no error/informative
c                      messages are printed; error conditions are
c                      communicated to the calling program via the
c                      return code. If vrb is positive error messages
c                      will be printed: vrb = 1 means that messages
c                      will be printed even with successful operations.
c                      vrb = 2,3 means that only error messages will
c                      be printed. vrb > 3 is similar to vrb = 3, but
c                      with the additional action of stopping the
c                      program if a fatal error takes place.
c     irc............. (output, integer) Return code. 0 means that
c                      the corresponding buffer was successfully
c                      refreshed. -1 means that an end-of-file
c                      condition was obtained from the corresponding
c                      file.  Any other value indicates a reading
c                      error (irc equals the system return code plus
c                      10000).
c
c     Return value: (logical) True if a record was successfully read.
c     ============  False othwerise (EOF or I/O error).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'ciopar.f'
      include 'cio2par.f'
c
c     Declaration of arguments.
c
      logical           ciorebuffer
      integer           fileid, vrb, irc
c
c     Declaration of shared data.
c
      include 'cio2comm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           ut
c
c     FIRST EXECUTABLE STATEMENT
c
c     Refreshing the buffer.
c
      ut = minciout + fileid
c
      read(ut, 2010, iostat = irc, end = 1010, err = 3010)
     +         cio2record1(fileid)(1:cio2buflen(fileid))
 2010 format(a)
c
      cio2reclast(fileid) = 0
      cio2recprev(fileid) = 0
c
      irc         = 0
      ciorebuffer = .true.
      return
c
 1010 continue
c
c     End of file reached.
c
      cio2reclast(fileid) = cio2buflen(fileid)
      cio2recprev(fileid) = cio2reclast(fileid)
c
      irc         = -1
      ciorebuffer = .false.
c
      if (vrb .gt. 0) then
        call errprint(0, '$CI3', 2, 'ciorebuffer',
     +     'End of file reached.',
     +     0, 0, 0, 0.d0, pifiletitle(fileid))
      endif
c
      return
c
c     I/O Error.
c
 3010 continue
c
      if (vrb .gt. 0) then
        call errprint(0, '$CI3', max(3, vrb), 'ciorebuffer',
     +     'System return code is listed:',
     +     1, irc, 0, 0.d0, pifiletitle(fileid))
      endif
c
      ciorebuffer = .false.
      irc         = irc + 10000
c
      return
c
      end
c     --- End of routine ciorebuffer.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'ciomgr2.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
