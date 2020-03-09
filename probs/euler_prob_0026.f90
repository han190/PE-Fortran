submodule(euler_interface_m) euler_prob_0026_m
    implicit none 

contains 

    module character(len=20) function euler0026()
        write (euler0026, "(i20)") ans(1000_int64)
    end function euler0026

    integer(int64) function ans(n)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none 
        integer(int64), intent(in) :: n
        integer(int64) :: tmp, k, ki, i 
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(n, is_prime)
        tmp = 0_int64; k = 1_int64
        do i = 7_int64, n
            if ( is_prime(i) ) then
                tmp = multiplicative_order(i, 10_int64)
                if (tmp > k) then
                    k = tmp
                    ki = i
                end if
            end if
        end do
        ans = ki 
    end function ans 

    integer(int64) function multiplicative_order(n, m)
        integer(int64), intent(in) :: n, m
        integer(int64) :: res, k, tmp

        k = 1_int64; tmp = 1_int64
        do
            res = mod(k * m, n)
            if (res /= 1_int64) then
                k = res
                tmp = tmp + 1_int64
            else
                exit
            end if
        end do
        multiplicative_order = tmp
    end function multiplicative_order
    
end submodule euler_prob_0026_m