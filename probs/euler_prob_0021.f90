submodule(euler_interface_m) euler_prob_0021_m
    implicit none 

contains 

    module character(len=20) function euler0021()
        write (euler0021, "(i20)") ans(10000)
    end function euler0021 

    integer function ans(n) 
        integer, intent(in) :: n
        procedure(sum_of_prop_div), pointer :: f
        integer :: p, t 

        t = 0 
        f => sum_of_prop_div
        do p = 1, n
            if ( f( f(p) ) == p .and. f(p) /= p ) then 
                 t = t + p
            end if 
        end do 
        ans = t 
    end function ans 

    integer function sum_of_prop_div(n)
        integer, intent(in) :: n 
        integer :: i, sum 

        sum = 1 
        do i = 2, int( sqrt( real(n, sp) ) )
            if ( mod(n, i) == 0 ) then 
                sum = sum + i + n / i 
            end if  
        end do 
        sum_of_prop_div = sum
    end function sum_of_prop_div
end submodule euler_prob_0021_m