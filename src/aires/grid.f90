!program main
module grid
    use iso_c_binding
    implicit none

    interface
        subroutine add_one() bind(C, name="add_one")
            use iso_c_binding
        end subroutine add_one
        
        subroutine calculate_radio() bind(C, name="calculate_radio")
            use iso_c_binding
        end subroutine calculate_radio

      subroutine update_vector(PID,X1,Y1,Z1,T1,X2,Y2,Z2,T2,E1,E2,weight,ANTX,ANTY,ANTZ) bind(C, name="update_vector")
          use iso_c_binding
          use, intrinsic :: iso_fortran_env
          implicit none
          integer, value :: PID
          double precision, value :: X1,Y1,Z1,T1,X2,Y2,Z2,T2,E1,E2,weight,ANTX,ANTY,ANTZ
      end subroutine update_vector

        
        subroutine grid_statistics() bind(C, name="grid_statistics")
            use iso_c_binding
            use, intrinsic :: iso_fortran_env
        end subroutine grid_statistics
    end interface

end module grid
