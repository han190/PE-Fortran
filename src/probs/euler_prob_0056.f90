submodule(euler_interface_m) euler_problem_0056
    use euler_mi_m
    implicit none 

contains

    module character(len=20) function euler0056()
        write (euler0056, "(i20)") ans()
    end function euler0056

    integer function ans()
        type(very_long_int_t) :: int_arr(10), mul(10)
        integer :: sum_arr(10, 10), i, j

        do i = 1, 10
            mul(i) = 89 + i
            int_arr(i) = mul(i) ** 89
        end do 
        
        do j = 1, 10
            do i = 1, 10 
                int_arr(i) = mul(i) * int_arr(i)
                sum_arr(i, j) = sum( int_arr(i)%arr )
            end do 
        end do 

        ans = maxval(sum_arr)
    end function ans

end submodule euler_problem_0056