c
c     FILE: absfnset.f                      Creation date: 22/JUN/1996.
c                                       LAST MODIFICATION: 05/APR/2002.
c
c     Routines to set absolute directory and file names.
c     NOTE: The code of these routines may depend on the operating
c           system.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine absfnset(dir, dirlen, fn, fnlen, file, filelen)
c
c     Giving the complete pathname of a file, given the directory
c     name and the file name.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999.
c
c     Parameters:
c     ==========
c
c     dir............. (input-output, character*(*)) String
c                      containing directory info. May be edited
c                      accordingly with OS needs.
c     dirlen.......... (input-output, integer) Length of dir.
c     fn.............. (input-output, character*(*)) String
c                      containing the file name. May be edited
c                      accordingly with OS needs.
c     fnlen........... (input-output, integer) Length of fn.
c     file............ (output, character*(*)) The complete
c                      file identification string.
c     filelen......... (output, integer) Length of file.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     dir, fn, file
      integer           dirlen, fnlen, filelen
c
c     FIRST EXECUTABLE STATEMENT
c
      call absdirset(dir, dirlen)
c
      if (dirlen .gt. 0) then
c
c       A directory was explicitly given.
c
        file    = dir(1:dirlen) // fn(1:fnlen)
        filelen = dirlen + fnlen
c
      else
c
c       No directory is specified.
c
        file    = fn(1:fnlen)
        filelen = fnlen
c
      endif
c
      return
      end
c     --- End of routine absfnset.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine absdirset(dir, dirlen)
c
c     Setting a directory name to allow concatenation with file names
c     to obtain absolute file specifications.
c     The header '~' must be interpreted as the user's main (HOME)
c     directory.
c
c     The code of this routine is OS dependent.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1999, 2000, 2002.
c
c     Parameters:
c     ==========
c
c     dir............. (input-output, character*(*)) String
c                      containing directory info. May be edited
c                      accordingly with OS needs.
c     dirlen.......... (input-output, integer) Length of dir.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     dir
      integer           dirlen
c
c     Declaration of internal variables and arrays.
c
      integer           i, j, k, hl
      character*132     auxstring
c
c     FIRST EXECUTABLE STATEMENT
c
      if (dirlen .gt. 0) then
c
c       A directory was explicitly given.
c
c       The following transformation works well for UNIX
c       filesystems:
c
        if (dir(dirlen:dirlen) .ne. '/') then
          dirlen = dirlen + 1
          dir(dirlen:dirlen) = '/'
        endif
c
c       Translating leading ~/ if necessary.
c
        if (dir(1:2) .eq. '~/') then
c
          call sysspawn('rm -f __Aires_ENV.tmp; echo $HOME',
     +                  ' ', '__Aires_ENV.tmp', i)
          if (i .ne. 0) goto 3010
          open(57, file = '__Aires_ENV.tmp', status = 'OLD',
     +             err = 3010)
          read(57, 2010, err = 3010) auxstring
 2010     format(a)
          close(57, status = 'DELETE')
c
          hl = 132
          call strim(auxstring, hl)
          if (hl .le. 0) goto 3010
          if (auxstring(1:1) .ne. '/') goto 3010
c
          k      = dirlen
          dirlen = dirlen + hl - 1
          j      = dirlen
          do i = k, 2, -1
            dir(j:j) = dir(i:i)
            j = j - 1
          enddo
c
          dir(1:j) = auxstring(1:hl)
c
        else if (dir(1:dirlen) .eq. './') then
c
c         The "current directory" ("./") is set as null string.
c
          dirlen = 0
          dir    = ' '
c
        endif
c
      endif
      return
c
c     Error message.
c
 3010 continue
      call errprint(0, '*', 4, 'absdirset',
     +     'Cannot determine HOME directory.',
     +     0, 0, 0, 0.d0, ' ')
c
c
      end
c     --- End of routine absdirset.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'absfnset.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
