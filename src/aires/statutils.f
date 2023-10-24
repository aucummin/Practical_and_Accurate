c
c     FILE: statutils.f                     Creation date: 09/JAN/1997.
c                                       LAST MODIFICATION: 05/APR/2002.
c
c     This file contains several routines to perform statistical
c     calculations.
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine statupdate(ndata, ld1, stdata)
c
c     Updating a statistical counter.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001.
c
c
c     Arguments:
c     =========
c
c     ndata........... (input, integer) The number of data items.
c     ld1............. (input, integer) Leading dimension of array
c                      stdata.
c     stdata.......... (input-output, double precision,
c                      array(ld1, ndata)) The statistical data array.
c                      Elements (1, *) carry the running sums of the
c                      corresponding data item, element (2, *) the
c                      current data items, to use in the update,
c                      elements (3, *) carry the running sum of squares
c                      and elements (4, *) and (5, *) the minimum and
c                      maximum values, respectively. Elements (2, *)
c                      are set to zero to allow count restart.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ndata, ld1
      double precision  stdata(ld1, ndata)
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  tmp1
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, ndata
        tmp1 = stdata(2, i)
        stdata(1, i) = stdata(1, i) + tmp1
        stdata(3, i) = stdata(3, i) + tmp1 * tmp1
        if (tmp1 .lt. stdata(4, i)) stdata(4, i) = tmp1
        if (tmp1 .gt. stdata(5, i)) stdata(5, i) = tmp1
        stdata(2, i) = 0
      enddo
      return
c
      end
c     --- End of routine statupdate
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine stmomts(stsize, ndata, ld1, stdata)
c
c     Evaluations of means, standard deviations and RMS errors.
c
c     Written by: S. J. Sciutto, La Plata 1996, 1997, 2001.
c
c
c     Arguments:
c     =========
c
c     stsize.......... (input, integer) The number of data samples.
c                      If it is less than 1, no action is taken.
c                      If it is equal to one, RMS errors are set
c                      equal to zero.
c     ndata........... (input, integer) The number of data items.
c     ld1............. (input, integer) Leading dimension of array
c                      stdata.
c     stdata.......... (input-output, double precision,
c                      array(ld1, ndata)) Statistical data array.
c                      As input contains the following data:
c                           stdata(1, *) = sums of x
c                           stdata(3, *) = sums of x^2
c                      As output:
c                           stdata(1, *) = <x>
c                           stdata(2, *) = RMS error of <x>.
c                           stdata(3, *) = Estimator of sigma(x).
c                      Other array elements are not used.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           stsize, ndata, ld1
      double precision  stdata(ld1, ndata)
c
c     Declaration of internal variables and arrays.
c
      integer           i
      double precision  x, xx, f1, f2, f3
c
c     FIRST EXECUTABLE STATEMENT
c
c     Switching accordingly with the number of samples.
c
      if (stsize .gt. 1) then
c
c       Processing the data points normally.
c
        f1 = 1.d0 / stsize
        f2 = dfloat(stsize) / (stsize - 1)
        f3 = sqrt(f1)
c
        do i = 1, ndata
          x  = f1 * stdata(1, i)
          xx = sqrt(f2 * abs(f1 * stdata(3, i) - x * x))
          stdata(1, i) = x
          stdata(2, i) = f3 * xx
          stdata(3, i) = xx
        enddo
c
      else if (stsize .gt. 0) then
c
c       Just one point. Returning zero standard deviations.
c
        do i = 1, ndata
          stdata(2, i) = 0
          stdata(3, i) = 0
        enddo
c
      endif
      return
c
      end
c     --- End of routine stmomts
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
      subroutine statzero(ndata, ld1, stdata)
c
c     Initializing statistical counters.
c
c     Written by: S. J. Sciutto, La Plata 2001, 2002.
c
c
c     Arguments:
c     =========
c
c     ndata........... (input, integer) The number of data items.
c     ld1............. (input, integer) Leading dimension of array
c                      stdata.
c     stdata.......... (output, double precision, array(ld1, ndata))
c                      The statistical data array. Elements (1, *)
c                      carry the running sums of the corresponding data
c                      item, element (2, *) the current data items, to
c                      use in the update, elements (3, *) carry the
c                      running sum of squares and elements (4, *) and
c                      (5, *) the minimum and maximum values,
c                      respectively.
c
c
      implicit none
c
c     Declaration of arguments.
c
      integer           ndata, ld1
      double precision  stdata(ld1, ndata)
c
c     Declaration of internal variables and arrays.
c
      integer           i
c
c     FIRST EXECUTABLE STATEMENT
c
      do i = 1, ndata
        stdata(1, i) =  0.00d00
        stdata(2, i) =  0.00d00
        stdata(3, i) =  0.00d00
        stdata(4, i) =  5.33d35
        stdata(5, i) = -5.33d35
      enddo
      return
c
      end
c     --- End of routine statzero
c
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c<---><---><---><---><---><---><--->*<---><---><---><---><---><---><--->
c
c     End of file 'statutils.f'
c     This source file is part of AIRES 2.8.4a distribution.
c
c     Ay48299427284ay 0 Tue Dec 12 16:29:48 ART 2006
