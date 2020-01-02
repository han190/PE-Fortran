submodule(euler_interface_m) euler_prob_0004_m
    implicit none 

contains 

    module character(len=20) function euler0004() 
        write (euler0004, "(i20)") ans(999)
    end function euler0004 

    integer function ans(n)
        integer, intent(in) :: n 
        integer :: a, b, p_max 

        p_max = 0; a = 100
        do while ( a <= n )
            b = 100
            do while ( b <= n )
                if ( is_palindromic(a * b)                                     &
                    .and. a * b > p_max                                        &
                ) then
                    p_max = a * b
                end if
                b = b + 1
            end do
            a = a + 1
        end do
        ans = p_max 
    end function ans 
end submodule euler_prob_0004_m 
