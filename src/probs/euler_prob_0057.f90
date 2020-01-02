submodule(euler_interface_m) euler_problem_0057
    use euler_mp_m
    implicit none 

contains 

    module character(len=20) function euler0057()
        write (euler0057, "(i20)") ans(1000)
    end function euler0057

    integer function ans(u_bound)
        integer, intent(in) :: u_bound
        integer :: i, k 
        type(long_int_t) :: a, b, c, d 

        i = 1; k = 0
        a = '1'; b = '2'

        do 
            if (i == u_bound) exit 
            call plus_two(a, b)
            call one_over(a, b)
            call plus_one(a, b, c, d)

            associate(                                                         &
                x => size(c%arr),                                              &
                y => size(d%arr)                                               &
            )
                if (x > y) then 
                    k = k + 1 
                end if 
            end associate

            i = i + 1 
        end do 
        ans = k 
    end function ans 

    subroutine plus_two(a, b)
        type(long_int_t), intent(inout) :: a, b 
        type(long_int_t) :: two 

        two = '2'
        a = a + b * two 
    end subroutine plus_two

    subroutine plus_one(a, b, c, d)
        type(long_int_t), intent(in) :: a, b 
        type(long_int_t), intent(out) :: c, d 

        c = a + b 
        d = b 
    end subroutine plus_one

    subroutine one_over(a, b)
        type(long_int_t), intent(inout) :: a, b 
        type(long_int_t) :: tmp 

        tmp%arr = a%arr
        a%arr = b%arr 
        b%arr = tmp%arr
    end subroutine one_over

end submodule euler_problem_0057
        

