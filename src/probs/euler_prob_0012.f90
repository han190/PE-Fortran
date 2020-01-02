submodule(euler_interface_m) euler_prob_0012_m 
    implicit none 

contains 

    module character(len=20) function euler0012() 
        write (euler0012, "(i20)") ans(500)
    end function euler0012

    integer function ans(n)
        integer, intent(in) :: n 
        integer :: j, k  

        j = 0; k = 0 
        do 
            j = j + 1 
            k = k + j 
            if ( nums_of_divs(k) > n ) exit 
        end do 
        ans = k 
    end function ans 

    integer function nums_of_divs(n)
        integer, intent(in) :: n  
        integer :: m, i, tmp 

        m = int( sqrt( real(n, sp) ) )
        tmp = 0 

        do i = 1, m - 1 
            if ( mod(n, i) == 0 ) then 
                tmp = tmp + 1 
            end if 
        end do 
        tmp = tmp * 2 

        if ( mod(n, m) == 0 ) then 
            tmp = tmp + 1 
        end if 

        nums_of_divs = tmp 
    end function nums_of_divs
end submodule euler_prob_0012_m