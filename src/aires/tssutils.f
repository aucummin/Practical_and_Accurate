c
c     FILE: tssutils.f                      Creation date: 23/NOV/2001.
c                                       LAST MODIFICATION: 07/MAY/2002.
c
c     Auxiliary routines related with the "task summary script" file.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine writetsshead(ut)
c
c     Writing a header for the tss file.
c
c     Written by: S. J. Sciutto, La Plata 2002.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'idfhead.f'
      include 'versionpar.f'
c
c     Declaration of arguments.
c
      integer           ut
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*1      cc
      integer          i
c
c     FIRST EXECUTABLE STATEMENT
c
      cc = char(icommentchar)
c
      write(ut, 2010) (cc, i = 1, 4), tssheader0,
     +                'version ', aires_version
 2010 format(10a)
c
      call welcome(ut, cc, i)
c
      return
      end
c     --- End of routine writetsshead.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine writetsscomm(ut, commstr)
c
c     Writing a comment line to the tss file.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c     commstr......... (input, character*(*)) The comment string.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ut
      character*(*)     commstr
c
c     Declaration of shared data.
c
      include 'srycomm.f'
c
c     Declaration of internal variables and arrays.
c
      character*1      cc
c
c     FIRST EXECUTABLE STATEMENT
c
      cc = char(icommentchar)
c
      write(ut, 2010) cc
 2010 format(3a)
c
      if (commstr .eq. ' ') return
c
      write(ut, 2010) cc, ' ', commstr
      write(ut, 2010) cc
c
      return
      end
c     --- End of routine writetsscomm.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine writetssint(ut, varname, value, negalt)
c
c     Writing an integer variable into the tss file.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c     varname......... (input, character*(*)) Name of the variable.
c     value........... (input, integer) Current value.
c     negalt.......... (input, integer) If positive, then the
c                      string returned by routine altstring(negalt,..)
c                      is returned for negative numbers. Otherwise the
c                      number is processed normally without regard to
c                      its sign (see routine intnice).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of arguments.
c
      integer           ut, negalt
      character*(*)     varname
      integer           value
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           slen
c
c     FIRST EXECUTABLE STATEMENT
c
      call intnice(value, negalt, auxline, slen)
      write(ut, 2010) varname, ' = ', auxline(1:slen)
 2010 format(3a)
c
      return
      end
c     --- End of routine writetsint.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine writetssflt(ut, varname, value, negalt)
c
c     Writing a float variable into the tss file.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c     varname......... (input, character*(*)) Name of the variable.
c     value........... (input, double precision) Current value.
c     negalt.......... (input, integer) If positive, then the
c                      string returned by routine altstring(negalt,..)
c                      is returned for negative numbers. Otherwise the
c                      number is processed normally without regard to
c                      its sign (see routine intnice).
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of arguments.
c
      integer           ut, negalt
      character*(*)     varname
      double precision  value
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           slen
c
c     FIRST EXECUTABLE STATEMENT
c
      call fltnice(value, negalt, auxline, slen)
      write(ut, 2010) varname, ' = ', auxline(1:slen)
 2010 format(3a)
c
      return
      end
c     --- End of routine writetssflt.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine writetsssw(ut, varname, value)
c
c     Writing a logical variable into the tss file.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c     varname......... (input, character*(*)) Name of the variable.
c     value........... (input, logical) Current value.
c
c
      implicit none
c
c     Compilation parameters.
c
      include 'initpar.f'
c
c     Declaration of arguments.
c
      integer           ut
      character*(*)     varname
      logical           value
c
c     Declaration of shared data.
c
      include 'initcomm.f'
c
c     Declaration of internal variables and arrays.
c
      integer           slen
c
c     FIRST EXECUTABLE STATEMENT
c
      call swnice(value, auxline, slen)
      write(ut, 2010) varname, ' = ', auxline(1:slen)
 2010 format(3a)
c
      return
      end
c     --- End of routine writetsssw.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine writetssstr(ut, varname, string)
c
c     Writing a string variable into the tss file.
c
c     Written by: S. J. Sciutto, La Plata 2001.
c
c     Arguments:
c     =========
c
c     ut.............. (input, integer) FORTRAN i/o unit.
c     varname......... (input, character*(*)) Name of the variable.
c     string.......... (input, character*(*)) Current value.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ut
      character*(*)     varname, string
c
c     FIRST EXECUTABLE STATEMENT
c
      if (string .ne. ' ') then
        write(ut, 2010) varname, ' = ', string
      else
        write(ut, 2010) varname, ' ='
      endif
 2010 format(3a)
c
      return
      end
c     --- End of routine writetsstr.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'tssutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
