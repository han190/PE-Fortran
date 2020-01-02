submodule(euler_interface_m) euler_prob_0009_m
    implicit none 

contains 

    module character(len=20) function euler0009()
        write (euler0009, "(i20)") ans(1000)
    end function euler0009

    integer function ans(n)
        integer, intent(in) :: n 
        integer :: a, b, c 

        ans = 0
        outer: do a = 1, n 
            inner: do b = a + 1, n  
                c = n - a - b 
                if ( a**2 + b**2 == c**2 ) then 
                    ans = a * b * c 
                    return 
                end if 
            end do inner
        end do outer
    end function ans

end submodule euler_prob_0009_m