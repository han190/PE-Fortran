submodule(euler_interface_m) euler_problem_0057
    use euler_mi_m
    implicit none 

contains 

    module character(len=20) function euler0057()
        write (euler0057, "(i20)") ans(1000)
    end function euler0057

    integer function ans(u_bound)
        integer, intent(in) :: u_bound
        integer :: i, k 
        type(very_long_int_t) :: a, b, c

        i = 1; k = 0
        a = '1'; b = '2'

        do 
            if (i == u_bound) exit 
            
            a = a + b * to_long('2')
            call swap_long(a, b)
            c = a + b 

            if ( size(c%arr) > size(b%arr) ) then 
                k = k + 1 
            end if 

            i = i + 1 
        end do 
        ans = k 
    end function ans 

    subroutine swap_long(a, b)
        type(very_long_int_t), intent(inout) :: a, b 
        type(very_long_int_t) :: tmp 

        call move_alloc(a%arr, tmp%arr)
        call move_alloc(b%arr, a%arr)
        call move_alloc(tmp%arr, b%arr)
    end subroutine swap_long

end submodule euler_problem_0057
