submodule(euler_interface_m) euler_prob_0002_m
    implicit none 

contains 

    module character(len=20) function euler0002()
        write (euler0002, "(i20)") ans(4000000)
    end function euler0002 

    integer function ans(n)
        integer, intent(in) :: n
        integer :: i, i_sum 
        
        i = 0; i_sum = 0 
        do while ( fibonacci(i) <= n )
            i_sum = i_sum + fibonacci(i)
            i = i + 3 
        end do 
        ans = i_sum
    end function ans 

end submodule euler_prob_0002_m
