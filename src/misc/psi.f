c
c     FILE: psi.f                         Creation date: 26/OCT/2004.
c                                       LAST MODIFICATION: 26/OCT/2004.
c
c     A routine to evaluate the Psi function (derivative of the
c     logarithm of the Gamma function) for real x
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      function Psi (x)
c
c     Derivative of the logarithm of the Gamma function for real x.
c
c     S. J. Sciutto, La Plata 1994, 2004.
c
      implicit none
c
      double precision  Psi
      double precision  x
c
      integer           m, i
      double precision  y, prec, ssum
c
c     Values of Riemman's zeta function
c
      double precision  rzeta(2:55)
c
      data rzeta(02) /  1.64493406684822643647d0 / 
      data rzeta(03) / -1.20205690315959428540d0 /
      data rzeta(04) /  1.08232323371113819152d0 /
      data rzeta(05) / -1.03692775514336992633d0 /
      data rzeta(06) /  1.01734306198444913971d0 /
      data rzeta(07) / -1.00834927738192282684d0 /
      data rzeta(08) /  1.00407735619794433938d0 /
      data rzeta(09) / -1.00200839282608221442d0 /
      data rzeta(10) /  1.00099457512781808534d0 /
      data rzeta(11) / -1.00049418860411946456d0 /
      data rzeta(12) /  1.00024608655330804830d0 /
      data rzeta(13) / -1.00012271334757848915d0 /
      data rzeta(14) /  1.00006124813505870483d0 /
      data rzeta(15) / -1.00003058823630702049d0 /
      data rzeta(16) /  1.00001528225940865187d0 /
      data rzeta(17) / -1.00000763719763789976d0 /
      data rzeta(18) /  1.00000381729326499984d0 /
      data rzeta(19) / -1.00000190821271655394d0 /
      data rzeta(20) /  1.00000095396203387280d0 /
      data rzeta(21) / -1.00000047693298678781d0 /
      data rzeta(22) /  1.00000023845050272773d0 /
      data rzeta(23) / -1.00000011921992596531d0 /
      data rzeta(24) /  1.00000005960818905126d0 /
      data rzeta(25) / -1.00000002980350351465d0 /
      data rzeta(26) /  1.00000001490155482837d0 /
      data rzeta(27) / -1.00000000745071178984d0 /
      data rzeta(28) /  1.00000000372533402479d0 /
      data rzeta(29) / -1.00000000186265972351d0 /
      data rzeta(30) /  1.00000000093132743242d0 /
      data rzeta(31) / -1.00000000046566290650d0 /
      data rzeta(32) /  1.00000000023283118337d0 /
      data rzeta(33) / -1.00000000011641550173d0 /
      data rzeta(34) /  1.00000000005820772088d0 /
      data rzeta(35) / -1.00000000002910385044d0 /
      data rzeta(36) /  1.00000000001455192189d0 /
      data rzeta(37) / -1.00000000000727595984d0 /
      data rzeta(38) /  1.00000000000363797955d0 /
      data rzeta(39) / -1.00000000000181898965d0 /
      data rzeta(40) /  1.00000000000090949478d0 /
      data rzeta(41) / -1.00000000000045474738d0 /
      data rzeta(42) /  1.00000000000022737368d0 /
      data rzeta(43) / -1.00000000000011368684d0 /
      data rzeta(44) /  1.00000000000005684342d0 /
      data rzeta(45) / -1.00000000000002842171d0 /
      data rzeta(46) /  1.00000000000001421085d0 /
      data rzeta(47) / -1.00000000000000710543d0 /
      data rzeta(48) /  1.00000000000000355271d0 /
      data rzeta(49) / -1.00000000000000177636d0 /
      data rzeta(50) /  1.00000000000000088818d0 /
      data rzeta(51) / -1.00000000000000044409d0 /
      data rzeta(52) /  1.00000000000000022204d0 /
      data rzeta(53) / -1.00000000000000011102d0 /
      data rzeta(54) /  1.00000000000000005551d0 /
      data rzeta(55) / -1.00000000000000002776d0 /
c
c     Euler's constant
c
      double precision  Eulerneg
      data              Eulerneg  / -0.577215664901532860607d0 /
c
c     FIRST EXECUTABLE STATEMENT.
c
      y = abs(x - 0.5d0)
      m = y
      if (x .ge. 0.5d0) then
        prec = 0
        do i = 1, m
          prec = prec + 1.d0 / (x - i)
        enddo
        y = x - (m + 1)
      else
        if (m .eq. y) m = m - 1
        prec = -1.d0 / x
        do i = 1, m
          prec = prec - 1.d0 / (x + i)
        enddo
        y = x + m
      endif
c
c     Evaluating the series.
c
      ssum = rzeta(55)
      do i = 54, 2, -1
        ssum = rzeta(i) + y * ssum
      enddo
c
      Psi = prec + y * ssum + Eulerneg
      return
      end
c     --- End of routine Gamma
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'psi.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay49299427284ay 0 Tue Dec 12 16:29:49 ART 2006
