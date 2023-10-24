c
c     FILE: Aires_dirs.f                    Creation date: 28/FEB/2000.
c                                       LAST MODIFICATION: 28/FEB/2000.
c
c     Routine to write a list of current AIRES directories, useful to
c     be processed by the operating system.
c     NOTE: The format of the file may depended on the OS.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine Aires_dirs(host, user, taskname, rfn, fdirs, fdirlens)
c
c     Writing a list of current AIRES directories. The format of the
c     file is compatible with the operating system (UNIX in this case).
c
c     Written by: S. J. Sciutto, La Plata 1996, 1998, 2000.
c
c     Parameters:
c     ==========
c
c     host............ (input, character*(*)) The name of the host
c                      where the program is running on.
c     user............ (input, character*(*)) The name of the user who
c                      is currently operating AIRES.
c     taskname........ (input, character*(*)) The complete task name.
c     rfn............. (input, character*(*)) Unique header for
c                      internal (scratch) files.
c     fdirs........... (input, character*(*), array(4)) Directory
c                      names. Directories correspond, respectively,
c                      to AIRES "compressed output", "global",
c                      "export", and "scratch" directories.
c     fdirlens........ (input, integer, array(4)) Length of directory
c                      names.
c
c
      implicit none
c
c     Declaration of arguments.
c
      character*(*)     host, user, taskname, rfn
      character*(*)     fdirs(4)
      integer           fdirlens(4)
c
c     Declaration of internal variables and arrays.
c
      integer           i, i1, i2
c
      integer           dil
      parameter         (dil = 32)
      character*(dil)   dirkey
c
      data              dirkey / 'Output Global Export Scratch' /
c
c     FIRST EXECUTABLE STATEMENT
c
c     Opening the dirs file for writing.
c
      open(59, file = 'Aires.dirs', status = 'UNKNOWN', err = 3010)
c
      write(59, 2010, err = 3010)
     +          '# This is the AIRES dirs file. DO NOT EDIT.'
      write(59, 2010, err = 3010)
     +          'Aires_DHost=''', host, ''''
      write(59, 2010, err = 3010)
     +          'Aires_DUserName=''', user, ''''
      write(59, 2010, err = 3010)
     +          'Aires_DTaskName=''', taskname, ''''
 2010 format(5a)
c
      write(59, 2010, err = 3010)
     +          'Aires_DRandomfn=''', rfn, ''''
c
c     Writing the directory names.
c
      i1 = -1
      i2 = 0
c
      do i = 1, 4
c
        call nextword(dirkey, dil, i1, i2)
c
        if (fdirlens(i) .gt. 0) then
          write(59, 2010)
     +            'Aires_D', dirkey(i1:i2), '=''',
     +            fdirs(i)(1:fdirlens(i)), ''''
        else
          write(59, 2010)
     +            'Aires_D', dirkey(i1:i2), '='
        endif
c
      enddo
c
      close(59)
      return
c
 3010 continue
c
c     Error message.
c
      call errprint(0, '*', 4, 'Aires_dirs',
     +     'Error opening or writing the AIRES dirs file.',
     +     0, 0, 0, 0.d0, ' ')
c
c
      end
c     --- End of routine Aires_dirs.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'Aires_dirs.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
