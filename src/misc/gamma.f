c
c     FILE: gamma.f                         Creation date: 25/OCT/2004.
c                                       LAST MODIFICATION: 26/OCT/2004.
c
c     A routine to evaluate the Gamma function for real x
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function Gamma (x)
c
c     Gamma function for real x.
c
c     S. J. Sciutto, La Plata 1993, 2004.
c
      implicit none
c
      double precision  Gamma
      double precision  x
c
c     Coefficients for the series expansion of 1/Gamma.
c
      double precision  cinv(2:26)
     .           / 0.5772156649015329d0, -0.6558780715202538d0,
     .            -0.0420026350340952d0,  0.1665386113822915d0,
     .            -0.0421977345555443d0, -0.0096219715278770d0,
     .             0.0072189432466630d0, -0.0011651675918591d0,
     .            -0.0002152416741149d0,  0.0001280502823882d0,
     .            -0.0000201348547807d0, -0.0000012504934821d0,
     .             0.0000011330272320d0, -0.0000002056338417d0,
     .             0.0000000061160950d0,  0.0000000050020075d0,
     .            -0.0000000011812746d0,  0.0000000001043427d0,
     .             0.0000000000077823d0, -0.0000000000036968d0,
     .             0.0000000000005100d0, -0.0000000000000206d0,
     .            -0.0000000000000054d0,  0.0000000000000014d0,
     .             0.0000000000000001d0                          /
c
      integer           m, i
      double precision  y, prec, ssum
c
c     FIRST EXECUTABLE STATEMENT.
c
      m = abs(x)
      if (x .ge. 0) then
        if (m .eq. x) m = m - 1
        prec = 1
        do i = 1, m
          prec = prec * (x - i)
        enddo
        y = x - m
      else
        prec = 1 / x
        do i = 1, m
          prec = prec / (x + i)
        enddo
        y = x + (m + 1)
      endif
c
c     Evaluating 1/Gamma(y)
c
      ssum = cinv(26)
      do i = 25, 2, -1
        ssum = cinv(i) + y * ssum
      enddo
      ssum = y * (1 + y * ssum)
c
      Gamma = prec / ssum
      return
      end
c     --- End of routine Gamma
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'gamma.f'

c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
