!program main
module grid
    use iso_c_binding
    implicit none

    interface
        function get_vector_ptr() bind(C, name="get_vector_ptr")
            use iso_c_binding
            type(c_ptr) :: get_vector_ptr
        end function get_vector_ptr

        subroutine update_vector(ptr, PID, X1, Y1, Z1, T1, X2, Y2, Z2, T2, E1, E2, weight) bind(C, name="update_vector")
            use iso_c_binding
            use, intrinsic :: iso_fortran_env
            integer, value :: PID
            double precision, value :: X1, Y1, Z1, T1, X2, Y2, Z2, T2, E1, E2, weight
            type(c_ptr), value :: ptr
        end subroutine update_vector
        
        subroutine grid_statistics(ptr) bind(C, name="grid_statistics")
            use iso_c_binding
            use, intrinsic :: iso_fortran_env
            type(c_ptr), value :: ptr
        end subroutine grid_statistics
    end interface

!    type(c_ptr) :: vec_c_ptr

!    vec_c_ptr = get_vector_ptr()
                                                      
!    call update_vector(vec_c_ptr,2, 86559.992293896983d0, 4.6934191051443369d-6, &
!                       18494.739326709587d0, -92115.219784851506d0, 86428.796248203944d0, &
!                       8.8004424864505637d-6, 18542.490776703784d0, -91975.603871066443d0, &
!                       1040204441.9762425d0, 1040204441.5635517d0, 1.0000000000000000d0)

!    call grid_statistics(vec_c_ptr)
end module grid
!end program
