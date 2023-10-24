c
c     FILE: mergeciofiles.f                 Creation date: 04/DEC/2006.
c                                       LAST MODIFICATION: 04/DEC/2006.
c
c     A utility program to merge two or more AIRES compressed files.
c
c     Written by: S. J. Sciutto, La Plata 2006.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      program mergeciofiles
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
      integer           irc, lifile, lofile
      character*512     ifile, ofile
      character*1200    line
      integer           ciochann
      logical           newok
      logical           ciorebuffer
c
c     FIRST EXECUTABLE STATEMENT
c
c     Initializing the cio system.
c
      call ciorinit(0, 0, 0, irc)
c
c     Reading Output file name.
c
 2010 format(5a)
 1010 continue
      read(5, 2010, end = 3010) ofile
      call strim(-1, ofile, lofile)
      if (lofile .le. 0) goto 1010
      if (ofile(1:1) .eq. '#') goto 1010
c
c     Reading first input file.
c
 1020 continue
      read(5, 2010, end = 3020) ifile
      call strim(-1, ifile, lifile)
      if (lifile .le. 0) goto 1020
      if (ifile(1:1) .eq. '#') goto 1020
c
c     Copying first input file to output file.
c
      call rmfile(2, ofile(1:lofile))
      write(line, 2010) 'cp ', ifile(1:lifile), ' ', ofile(1:lofile)
      call strim(-1, line, lifile)
      call sysspawn(line(1:lifile), ' ', ' ', irc)
c
c     Appending other files.
c
      open(2, file = ofile(1:lofile),
     +        status = 'OLD', access = 'APPEND')
c
 1030 continue
      read(5, 2010, end = 1100) ifile
      call strim(-1, ifile, lifile)
      if (lifile .le. 0) goto 1030
      if (ifile(1:1) .eq. '#') goto 1030
c
      call opencrofile(' ', ifile(1:lifile), 0, 0, 0, ciochann, irc)
      if (irc .gt. 1) goto 3030
c
 1040 continue
      newok = ciorebuffer(ciochann, 0, irc)
      if (irc .ne. 0) goto 1050
      write(2, 2010) cio2record1(ciochann)(1:cio2buflen(ciochann))
      goto 1040
 1050 continue
      call cioclose1(ciochann)
c
      goto 1030
 1100 continue
      close(2)
c
      stop
c
 3010 continue
      print 2010, 'No output file defined'
      stop
c
 3020 continue
      print 2010, 'No input file(s) defined'
      stop
c
 3030 continue
      print 2010, 'Error processing file ',
     +            ifile(1:lifile)
c
      end
c     --- End of program mergeciofiles.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'mergeciofiles.f'
